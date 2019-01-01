library(ggplot2)
library(ggthemes)
library(dplyr)
library(htmlTable)
library(elo)
library(tidyverse)


elo.A <- c(1500, 1500)
elo.B <- c(1500, 1500)
elo.prob(elo.A, elo.B)

ATP_matches$wins.A <- ATP_matches$Winner_Games_Won>ATP_matches$Loser_Games_Won
K<-ATP_matches$K
score<-elo.run(wins.A ~ Winner + Loser, data = ATP_matches, k = K)

elo.update(wins.A, Winner, Loser,data=ATP_matches)

summary(score)
tail(as.matrix(score))

newdat <- data.frame(
  Winner = "Roger Federer",
  Loser = "Yen-Hsun Lu"
)
predict(score, newdata = newdat)


