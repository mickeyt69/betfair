library(ggplot2)
library(ggthemes)
library(dplyr)
library(htmlTable)
library(elo)
library(tidyverse)


elo.A <- c(1500, 1500)
elo.B <- c(1500, 1500)
elo.prob(elo.A, elo.B)

ATP_matches$wins.A <- if_else(ATP_matches$Winner_Sets_Won>ATP_matches$Loser_Sets_Won,1,0)
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


