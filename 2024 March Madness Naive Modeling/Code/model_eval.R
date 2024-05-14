library(dplyr)
w_scores = read.csv("2024MarchMadness/csvs/w_scores.csv")
m_scores = read.csv("2024MarchMadness/csvs/scores.csv")

m_scores <- m_scores |> dplyr::mutate(
  ID = ifelse(TeamID < opponent_TeamID, paste("2023_",TeamID,"_",opponent_TeamID, sep = ""), paste("2023_",opponent_TeamID,"_",TeamID, sep = "")),
  og_is_higher = ifelse(TeamID > opponent_TeamID, TRUE, FALSE),
  Test_Pred = case_when(
    team_score > opponent_team_score & !og_is_higher ~ 1,
    team_score < opponent_team_score & !og_is_higher ~ 0,
    team_score > opponent_team_score & og_is_higher ~ 0,
    team_score < opponent_team_score & og_is_higher ~ 1,#cleaning
  )
)
w_scores <- w_scores |> dplyr::mutate(
    ID = ifelse(OG_TeamID < opponent_TeamID, paste("2023_",OG_TeamID,"_",opponent_TeamID, sep = ""), paste("2023_",opponent_TeamID,"_",OG_TeamID, sep = "")),
    og_is_higher = ifelse(OG_TeamID > opponent_TeamID, TRUE, FALSE),
    Test_Pred = case_when(
      team_score > opponent_team_score & !og_is_higher ~ 1,#cleaning
      team_score < opponent_team_score & !og_is_higher ~ 0,
      team_score > opponent_team_score & og_is_higher ~ 0,
      team_score < opponent_team_score & og_is_higher ~ 1,
    ))
m_scores <- m_scores |> dplyr::select(ID, Test_Pred)
w_scores <- w_scores |> dplyr::select(ID, Test_Pred) #getting predictions

m_sub = read.csv("2024MarchMadness/MSubmission_New.csv")
w_sub = read.csv("2024MarchMadness/csvs/WSubmission.csv")
all_scores = rbind(m_scores, w_scores) 
sub = rbind(m_sub, w_sub)

mens_combined = merge(m_sub, m_scores, on = c("ID"))
combined = merge(sub, all_scores, on =c("ID"))#combining mens and womens
womens_combined = merge(w_sub, w_scores, on = c("ID"))

library(DescTools) #getting Brier Scores
BrierScore(mens_combined$Test_Pred, mens_combined$Pred)#0.1895659
BrierScore(womens_combined$Test_Pred, womens_combined$Pred) # 0.1212572
BrierScore(combined$Test_Pred, combined$Pred)#0.1547015
