library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(MLmetrics)
library(RcppRoll)
library(randomForest)

# Read in the world cup CSV data
rawdata = read_csv("wc_datathon_dataset.csv")

rawdata<-ATP_matches

# Convert the match date to an R date
# -- and add in a unique match key for later
rawdata = rawdata %>% 
  mutate(
    date = dmy(Tournament_Date),
    player_1 = tolower(Winner), player_2 = tolower(Loser),
    match_key = to_snake_case(paste(format(date, "%Y %m %d"), player_1, player_2))
  )

# Read in the WC 2018 Fixture
raw_2018 = men_dummy%>%
  mutate(
    date = dmy(date),
    match_key = to_snake_case(paste(format(date, "%Y %m %d"), team_1, team_2)),
    is_team_1_home = ifelse(team_1 == "russia", TRUE, FALSE),
    is_team_2_home = ifelse(team_2 == "russia", TRUE, FALSE),
    is_neutral = !(is_team_1_home | is_team_2_home)
  )

# Add in the fixture to the training data for feature calculations
rawdata = rawdata %>% 
  union_all(
    raw_wc_2018 %>%
      mutate(
        tournament = "World Cup 2018",
        team_1_goals = 0, team_2_goals = 0
      ) %>%
      select(-prob_team_1_win, -prob_team_1_draw, -prob_team_1_lose, -match_id)
  )

# +++++++++++++++++++
# Restructuring Data
# +++++++++++++++++++

# ML solutions are a bit awkward for sport problems because the data is often on a single row
# To simplify our calculations we'll split out the data into 2 rows before collapsing it later for predictions

playa_1_df = rawdata %>%
  mutate(side = "player_1") %>%
  select(
    match_key, side, tournament, date,
    "team" = team_1, "opponent" = team_2, 
    "goals" = team_1_goals, "opp_goals" = team_2_goals, 
    "home_game" = is_team_1_home, is_neutral
  )

team_2_df = rawdata %>%
  mutate(side = "team_2") %>%
  select(
    match_key, side, tournament, date,
    "team" = team_2, "opponent" = team_1, 
    "goals" = team_2_goals, "opp_goals" = team_1_goals, 
    "home_game" = is_team_2_home, is_neutral
  )

expanded_df = team_1_df %>% union_all(team_2_df)

# +++++++++++++++++++
# Calculate Features
# +++++++++++++++++++

# We'll use this expanded dataframe to perform the feature calculations
# including looking back at previous games and performing rolling averages for example

expanded_df = expanded_df %>%
  mutate(
    margin = goals - opp_goals,
    home_game = ifelse(home_game, 1, 0),
    is_neutral = ifelse(is_neutral, 1, 0),
    target = as.factor(
      case_when(
        margin > 0 ~ "Win",
        margin < 0 ~ "Lose",
        TRUE ~ "Draw"
      )
    )
  ) %>%
  group_by(team) %>%
  mutate(
    last_margin = lag(margin, 1, default = 0),
    last_5_margin_mean = roll_mean(margin, 5, align = "right", fill = NA),
    max_score_10 = roll_max(goals, 10, align = "right", fill = NA),
    max_allowed_10 = roll_max(opp_goals, 10, align = "right", fill = NA)
  ) %>%
  ungroup() %>%
  drop_na() 

# ++++++++++++++++++++++++++
# Assemble features together
# ++++++++++++++++++++++++++

# Now that we've performed the calculations we'll flatten out the data again
# by joining the team 1 and team 2 features back together into a single row

# Outcome
outcome_df = expanded_df %>%
  filter(
    side == "team_1",
    tournament != "World Cup 2018"
  ) %>%
  select(match_key, date, tournament, margin, target, home_game, is_neutral)

# Need team 1 feature matrix
team_1_features = expanded_df %>%
  filter(
    side == "team_1",
    tournament != "World Cup 2018"
  ) %>%
  select(match_key, team, last_margin:max_allowed_10) %>%
  rename_at(vars(-match_key), function(x){paste0("h_", x)})

# Need team 2 features
team_2_features = expanded_df %>%
  filter(
    side == "team_2",
    tournament != "World Cup 2018"
  ) %>%
  select(match_key, team, last_margin:max_allowed_10) %>%
  rename_at(vars(-match_key), function(x){paste0("a_", x)})

# Join together into feature matrix
feature_matrix = outcome_df %>%
  inner_join(team_1_features)%>%
  inner_join(team_2_features)

# ++++++++++++++++++++++
# Split Training / Test
# ++++++++++++++++++++++

# We're going to use all matches (qualifiers / friendlies etc) AFTER the 2010 world cup to predict the 2014 world cup matches
training = feature_matrix %>%
  # Select only features and target
  select(
    target,
    is_neutral, home_game,
    # Select feature columns that start with an a or h
    matches("^h|^a")
  )

# ++++++++++++
# Train Model
# +++++++++++

# For reproducibility
set.seed(123)

# Train Model
rf.fit = randomForest(target ~ . , data = training)

# ++++++++++++++++++
# Assemble Test Data
# ++++++++++++++++++

# We need a little bit more work to produce the test set
# -- Given we won't be updating the predictions after every pool game we'll use
# -- the same feature set for a team in each of their pool games
# -- that will be based on their last set of games before the 2018 world cup

# Most recent feature record is the first feature record of the world cup 2018
most_recent_feats = expanded_df %>%
  filter(tournament == "World Cup 2018") %>%
  group_by(team) %>%
  arrange(date) %>%
  filter(row_number() == 1) %>%
  select(-match_key)

# Home features
home_features = 
  raw_wc_2018 %>%
  select(match_key, "team" = team_1) %>%
  inner_join(most_recent_feats, by = "team") %>%
  select(match_key, team, last_margin:max_allowed_10) %>%
  rename_at(vars(-match_key), function(x){paste0("h_", x)})

# Away features
away_features = 
  raw_wc_2018 %>%
  select(match_key, "team" = team_2) %>%
  inner_join(most_recent_feats, by = "team") %>%
  select(match_key, team, last_margin:max_allowed_10) %>%
  rename_at(vars(-match_key), function(x){paste0("a_", x)})

# Fold datasets together
pred_set = raw_wc_2018 %>%
  inner_join(home_features)%>%
  inner_join(away_features) %>%
  select(-h_team, -a_team) %>%
  mutate(
    home_game = ifelse(is_team_1_home, 1, 0),
    is_neutral = ifelse(is_neutral, 1, 0)
  )

# ++++++++++++++++
# Make Predictions
# ++++++++++++++++

# Make predictions on prediction set
rf.pred = predict(rf.fit, pred_set, type = "prob")

# ++++++++++++++++
# Create submission
# ++++++++++++++++

submission = raw_wc_2018 %>%
  select(date, match_id, team_1, team_2) %>%
  # Predicted Probabilities
  mutate(
    prob_team_1_win = rf.pred[,3],
    prob_team_1_draw = rf.pred[,2],
    prob_team_1_lose = rf.pred[,1] 
  ) 

# Write submission file
write_csv(submission, "betfair_datascientists_ML-DiMaria.csv")
