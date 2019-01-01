# +++++++
# Model: MakelELO
# Date: 23 / 5 / 2018
# +++++++

library(readr)
library(dplyr)
library(elo)
library(lubridate)

# Read in the world cup CSV data
training = read_csv("ATP_matches.csv")
training$date = dmy(training$Tournament_Date)

# Lowercase team names
training$playerA = tolower(training$Winner)
training$playerB = tolower(training$Loser)

# Read in submission file
wc_2018 = read_csv("aus_open_match_odds_(1).csv")

# Fix the ELO k factor - here you can try different values to see if improves the model performance
k_fac = training$K

# Run ELO
elo_run = elo.run(
  score(Winner_TotalPoints_Won, Loser_TotalPoints_Won) ~ playerA + playerB,
  data = training,
  k = k_fac
)

elo.update(wins.A, Winner, Loser,data=ATP_matches)

aus_open$playerA = tolower(aus_open$player_1)
aus_open$playerB = tolower(aus_open$player_2)


newdat <- data.frame(
  playerA = "novak djokovic",
  playerB = "marin cilic"
)

# Run predictions on 2018 world cup: the predict function, in this case, just needs the home and away team names for the tournament
tennis_probs = predict(elo_run, newdata = newdat)
                         #aus_open%>% select(playerA, playerB))


# To our WC 2018 dataset let's add in our predicted win probabilities and fold in the expected draw rates from our table above
wc_2018 = wc_2018 %>%
  select(-prob_team_1_draw) %>%
  mutate(
    prob_team_1_win = wc_2018_home_probabilities,
    prob_team_1_lose = 1 - prob_team_1_win,
    prob_bucket = round(20 * abs((prob_team_1_win - prob_team_1_lose))) / 20
  ) %>%
  left_join(draw_rates) %>%
  mutate(
    prob_team_1_win = prob_team_1_win - 0.5 * draw_prob,
    prob_team_1_lose = prob_team_1_lose - 0.5 * draw_prob
  ) %>%
  select(date, match_id, team_1, team_2, prob_team_1_win, "prob_team_1_draw" = draw_prob, prob_team_1_lose, -prob_bucket)

# Write submission to file
write_csv(wc_2018, "betfair_datascientists_makelelo.csv")
