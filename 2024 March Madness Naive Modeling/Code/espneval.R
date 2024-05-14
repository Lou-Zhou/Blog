library(hoopR)
library(dplyr)
library(plyr)
library(ggplot2)
library(png)
library(sportyR)
library(ash)
library(rms)
library(stringr)
library(stringdist)
library(tidyverse)
library(wehoop)
games <- load_mbb_team_box(seasons = most_recent_mbb_season())
unique(games$team_home_away)
tour <- games |> dplyr::filter(season_type == 3, game_id <= 401638645 & game_id >= 401638583)
tour <- tour |> dplyr::filter(!duplicated(game_id))
#get all tournament games
for (game_id in seq(401638584, 401638645,1)){
  print(game_id)
  try({
    new_bet <- espn_mbb_wp(game_id)[1,]
    print(new_bet)
    bet <- rbind(bet, new_bet)})
}#getting ESPN predictions
bet_n <- bet |> dplyr::select(game_id,home_win_percentage, away_win_percentage)

new_tour <-tour |> dplyr::select(game_id, team_id, opponent_team_id, team_home_away, team_score, opponent_team_score)
new_tour <- new_tour |> dplyr::mutate(
  og_win = ifelse(team_score > opponent_team_score, 1, 0),
  game_id = as.character(game_id)#cleaning
)

bet_t<-as.data.frame(bet)
new_tour
comb <- merge(bet_t, new_tour, by = c("game_id"))
comb <- comb |> dplyr::select(game_id, away_win_percentage, og_win)#combining actual with ESPN prediction
library(DescTools)#getting Brier Score
BrierScore(comb$og_win, comb$away_win_percentage)#0.1840701
