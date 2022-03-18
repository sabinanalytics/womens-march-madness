#LOADING EXTERNAL PACKAGES AND FUNCTIONS USED IN THIS WORK
# ThisProgramDirectory <- "H:/Kaggle/Women's 2018"
library(espnanalytics)
library(tidyverse)
library(splines)
library(readr)
library(readxl)
library(lubridate)
library(Matrix)
library(elo)
library(randomForest)
options(dplyr.width = Inf)
options(stringsAsFactors = F)
options("scipen"=10, "digits"=4)
#Expands the internal memory limit
memory.limit(size = 8000000)

currentSeason <- 2022

source("MCBBMetrics_functions.R")
source("NCAA Tournament Functions.R")


cities <- read_csv("WCities.csv")
gameCities <- read_csv("WGameCities.csv")
tourneyCompactResults <- read_csv("WNCAATourneyCompactResults.csv")
tourneyDetailedResults <- read_csv("WNCAATourneyDetailedResults.csv")
tourneySeeds <- read_csv("WNCAATourneySeeds.csv")
tourneySlots <- read_csv("WNCAATourneySlots.csv")
gameCompactResults <- read_csv("WRegularSeasonCompactResults.csv")
gameDetailedResults <- read_csv("WRegularSeasonDetailedResults.csv")
seasons <- read_csv("WSeasons.csv")
teams <- read_csv("WTeams.csv")
teamSpellings <- read_csv("WTeamSpellings.csv")

maxDaysRest <- 14
avgDaysRest <- 4
maxDiffDaysRest <- 12

#  Elo ###
#Date Numbers for elo
gameDate.index <- gameCompactResults %>% 
  select(Season, DayNum) %>% unique() %>%
  arrange(Season, DayNum) %>%
  mutate(DATENM = row_number(Season))

gameResults.elo <- left_join(gameCompactResults, gameDate.index, by = c("Season", "DayNum")) %>%
  mutate(W = if_else(WScore > LScore, 1, 0),
         HomeID = ifelse(WLoc == "H", WTeamID, LTeamID),
         AwayID = ifelse(WLoc == "A", WTeamID, LTeamID),
         HomeScore = ifelse(WLoc == "H", WScore, LScore),
         AwayScore = ifelse(WLoc == "H", LScore, WScore)) %>%
  left_join(., teams, by = c("HomeID" = "TeamID")) %>% rename(HomeTeam = TeamName) %>%
  left_join(., teams, by = c("AwayID" = "TeamID")) %>% rename(AwayTeam = TeamName) 

elo.results <- elo.run(select(gameResults.elo, DATENM, HomeTeam, AwayTeam, W), history = TRUE, sort = TRUE, init = 1500)

# hfa_elo <- 70
# elo.results <- elo.run(score(HomeScore, away_score) ~ 
#                            adjust(home_team_full, hfa_elo) + away_team_full, 
#                          data = mcbb_scores, 
#                          k = 20)

temp <- elo.run(score(HomeScore, AwayScore) ~ 
                  HomeTeam + AwayTeam + 
                  k(20*log(abs(HomeScore - AwayScore) + 1)) + 
                  regress(Season, 1500, 0.2), 
                data = gameResults.elo, history = TRUE)

gameResults.elo$HomeElo <- temp$elos[,7]
gameResults.elo$AwayElo <- temp$elos[,8]


# Get Possessions for each game
tempGameResults <- gameDetailedResults %>% 
  mutate(WPoss = WFGA - WOR + WTO + 0.475*WFTA, 
         LPoss = LFGA - LOR + LTO + 0.475*LFTA, 
         Poss = (WPoss + LPoss) / 2)

tempGameResults.temp1 <- tempGameResults.temp2 <- tempGameResults
colnames(tempGameResults.temp1) <- colnames(tempGameResults.temp1) %>% gsub("W", "TM_",.) 
colnames(tempGameResults.temp1) <- colnames(tempGameResults.temp1) %>% gsub("L", "OPP_",.) 
colnames(tempGameResults.temp2) <- colnames(tempGameResults.temp2) %>% gsub("L", "TM_",.) 
colnames(tempGameResults.temp2) <- colnames(tempGameResults.temp2) %>% gsub("W", "OPP_",.) 

tempGameResults <- bind_rows(tempGameResults.temp1, tempGameResults.temp2) %>% 
  arrange(Season, DayNum, TM_TeamID) %>% 
  mutate(site = ifelse(is.na(TM_OPP_oc), OPP_TM_oc, TM_OPP_oc),
         netEff = (TM_Score - OPP_Score) / Poss) %>%
  left_join(., gameDate.index, by = c("Season", "DayNum")) %>%
  select(-ends_with("_oc"), -TM_Poss, -OPP_Poss)

### days rest
tempGameResults <- tempGameResults %>% group_by(Season, TM_TeamID) %>% 
  arrange(DATENM, TM_TeamID) %>%
  mutate(DATENM = as.numeric(DATENM),
         TM_DaysRest = DATENM - lag(DATENM, default = as.numeric(maxDaysRest)) ,
         TM_DaysRest = if_else(TM_DaysRest >maxDaysRest, as.numeric(maxDaysRest), TM_DaysRest)) %>%
  group_by(Season, OPP_TeamID) %>%   
  arrange(DATENM, OPP_TeamID) %>%
  mutate(OPP_DaysRest = DATENM - lag(DATENM, default = as.numeric(maxDaysRest)),
         OPP_DaysRest = ifelse(OPP_DaysRest >maxDaysRest, as.numeric(maxDaysRest), OPP_DaysRest)) %>% 
  ungroup() %>%
  mutate(Diff.DaysRest = (TM_DaysRest - OPP_DaysRest),
         Diff.DaysRest = ifelse(abs(Diff.DaysRest) > maxDiffDaysRest, sign(as.numeric(maxDaysRest))*as.numeric(maxDaysRest), Diff.DaysRest),
         TM_SITE.num = if_else(site == "H", 1, 
                               if_else(site == "A", -1, 0))) %>%
  ungroup()

avgGamePoss <- mean(tempGameResults$Poss)

allTeamsSeasons <- group_by(tempGameResults, Season, TM_TeamID) %>%
  filter(row_number(TM_TeamID) == 1) %>% ungroup() %>%
  select(Season, TM_TeamID) %>% 
  mutate(BPI = NA_real_)

# all.ADJ_TEAM_SEASON_ID <- unique(MCBBGamesData$TM_SEASON_ID)

bpi.seasons <- select(tempGameResults, Season) %>% unique() %>% unlist()
for(i in 1:length(bpi.seasons)){
  mod.data <- filter(tempGameResults, Season == bpi.seasons[i])
  teamsThisSeason <- filter(allTeamsSeasons, Season == bpi.seasons[i]) %>%
    select(Season, TM_TeamID)
  n.teams <- length(unique(mod.data$TM_TeamID))
  
  X.team.opp <- sparseMatrix(i = c(1:nrow(mod.data), 1:nrow(mod.data)), 
                             j = c(match(mod.data$TM_TeamID, teamsThisSeason$TM_TeamID), match(mod.data$OPP_TeamID, teamsThisSeason$TM_TeamID)),
                             x = rep(c(1,-1), each = nrow(mod.data)) )
  X.team.opp <- cbind(X.team.opp, mod.data$TM_SITE.num, ns(mod.data$Diff.DaysRest, df = 3))
  XtX.team.opp <- t(X.team.opp) %*% X.team.opp
  Xty <- t(X.team.opp) %*% mod.data$netEff
  
  prior.mean = rep(0,ncol(X.team.opp))
  prior_COV = diag(rep(99999,ncol(X.team.opp)))
  prior_COV_inv = inv(prior_COV)
  posterior_COV_inv = prior_COV_inv+XtX.team.opp
  posterior_COV = inv(posterior_COV_inv)
  
  posterior_beta=posterior_COV%*%(prior_COV_inv%*%prior.mean+Xty)
  team.bpi <- posterior_beta[1:n.teams]  - median(posterior_beta[1:n.teams])
  
  teamsThisSeason <- teamsThisSeason %>% 
    mutate(temp.BPI = team.bpi*avgGamePoss)
  allTeamsSeasons <- allTeamsSeasons %>% 
    left_join(., teamsThisSeason, by = c("Season", "TM_TeamID")) %>%
    mutate(BPI = ifelse(!is.na(temp.BPI), temp.BPI, BPI)) %>% 
    select(Season, TM_TeamID, BPI)
  cat(bpi.seasons[i], " finished \r")
}

allSeasonBPI <- allTeamsSeasons %>% 
  left_join(., teams, by = c("TM_TeamID" = "TeamID"))

# Get all end of season ELO ratings

team.elo.endSeason <- gameResults.elo %>% 
  select(Season, DayNum, HomeID, HomeElo, AwayID, AwayElo) %>% 
  rename(TM_TeamID = HomeID, TM_ELO = HomeElo, OPP_TeamID = AwayID, OPP_ELO = AwayElo)
opp.elo.endSeason <- gameResults.elo %>% 
  select(Season, DayNum, HomeID, HomeElo, AwayID, AwayElo) %>% 
  rename(TM_TeamID = AwayID, TM_ELO = AwayElo, OPP_TeamID = HomeID, OPP_ELO = HomeElo)

allSeasonELO <- bind_rows(team.elo.endSeason, opp.elo.endSeason) %>% 
  arrange(Season, DayNum, TM_TeamID) %>%
  group_by(Season, TM_TeamID) %>%
  filter(DayNum == max(DayNum)) %>% 
  filter(row_number(TM_TeamID) == 1) %>%
  ungroup() %>%
  select(Season, TM_TeamID, TM_ELO) %>%
  left_join(., teams, by = c("TM_TeamID" = "TeamID"))


## Adjusted Box-Score Stats for final model

### Loop for Each Stat
gameStats <- tempGameResults %>% 
  select(Season, TM_TeamID, OPP_TeamID, DayNum, 
         TM_Score, TM_FGM, TM_FGA, TM_FGM3, TM_FGA3, TM_FTM, TM_FTA, TM_OR, TM_DR, TM_Ast, TM_TO, TM_Stl, 
         TM_Blk, TM_PF, 
         OPP_Score, OPP_FGM, OPP_FGA, OPP_FGM3, OPP_FGA3, OPP_FTM, OPP_FTA, OPP_OR, OPP_DR, OPP_Ast, OPP_TO, OPP_Stl, 
         OPP_Blk, OPP_PF) %>%
  mutate(TM_FG2PCT = (TM_FGM - TM_FGM3)/(TM_FGA - TM_FGA3) %>% coalesce(.,as.integer(0)),
         TM_FG3PCT = TM_FGM3/TM_FGA3 %>% coalesce(.,as.integer(0)),
         TM_FTPCT = if_else(TM_FTA == 0, 0, TM_FTM/TM_FTA ),
         OPP_FG2PCT = (OPP_FGM - OPP_FGM3)/(OPP_FGA - OPP_FGA3) %>% coalesce(.,as.integer(0)),
         OPP_FG3PCT = OPP_FGM3/OPP_FGA3 %>% coalesce(.,as.integer(0)),
         OPP_FTPCT = if_else(OPP_FTA == 0, 0, OPP_FTM/OPP_FTA ) )
### get rid of ID columns and save off tibble to be rewritten w/ adjusted stats
gameStatsOnly <-  gameStats %>% select(-ends_with("ID"), -Season, -DayNum) 
stats.name.list <- gameStatsOnly %>% colnames

gameStatsAdjusted <- gameStats %>% select(Season, DayNum, ends_with("ID")) 

allTeamsSeasons.adjBoxAvg <- NULL

for( i in 1:length(bpi.seasons)){
  season.ind <- which(tempGameResults$Season == bpi.seasons[i])
  mod.data <- filter(tempGameResults, Season == bpi.seasons[i])
  teamsThisSeason <- filter(allTeamsSeasons, Season == bpi.seasons[i]) %>%
    select(Season, TM_TeamID)
  n.teams <- length(unique(mod.data$TM_TeamID))
  
  gameStatsOnlyThisSeason <- gameStatsOnly[season.ind,]
  gameStatsAdjustedThisSeason <- gameStatsAdjusted[season.ind,]
  
  X.team.opp <- sparseMatrix(i = c(1:nrow(mod.data), 1:nrow(mod.data)), 
                             j = c(match(mod.data$TM_TeamID, teamsThisSeason$TM_TeamID), match(mod.data$OPP_TeamID, teamsThisSeason$TM_TeamID)),
                             x = rep(c(1,-1), each = nrow(mod.data)) )
  X.team.opp <- cbind(X.team.opp, mod.data$TM_SITE.num, ns(mod.data$Diff.DaysRest, df = 3))
  XtX.team.opp <- t(X.team.opp) %*% X.team.opp
  
  prior.mean = rep(0,ncol(X.team.opp))
  prior_COV = diag(rep(99999,ncol(X.team.opp)))
  prior_COV_inv = inv(prior_COV)
  posterior_COV_inv = prior_COV_inv+XtX.team.opp
  posterior_COV = inv(posterior_COV_inv)
  
  for(j in 1:ncol(gameStatsOnlyThisSeason)){
    y <- unlist(gameStatsOnlyThisSeason[,j] / tempGameResults$Poss )
    Xty <- t(X.team.opp)%*%y
    
    posterior_beta=posterior_COV%*%(prior_COV_inv%*%prior.mean+Xty)
    
    pred.y <- X.team.opp %*% posterior_beta
    adjustedStat <- pred.y * avgGamePoss### put in for average possession game
    gameStatsAdjustedThisSeason <- gameStatsAdjustedThisSeason %>% mutate(col.name = as.numeric(unlist(adjustedStat)))
    colnames(gameStatsAdjustedThisSeason)[(j + 4)] <- paste0(stats.name.list[j], "_ADJ")
  }
  
  avg.gameStatsAdjustedThisSeason <- gameStatsAdjustedThisSeason %>% group_by(Season, TM_TeamID) %>% 
    summarize_at(vars(ends_with("_ADJ")), funs(mean(., na.rm = T))) %>% ungroup()
  
  allTeamsSeasons.adjBoxAvg <- bind_rows(allTeamsSeasons.adjBoxAvg, avg.gameStatsAdjustedThisSeason)
  
  cat("Adjustment for Season", bpi.seasons[i], " finished. ", i, "/", length(bpi.seasons), " complete \r")
}



allSeasonBox.Adj <- allTeamsSeasons.adjBoxAvg %>% 
  select(Season, starts_with("TM_")) %>% 
  mutate(TM_FG3PCT_ADJ = TM_FGM3_ADJ / TM_FGA3_ADJ)
allSeasonElo
allSeasonBPI

# rename(OPP_Score = TM_Score, OPP_FGM = TM_FGM, OPP_FGA = TM_FGA, OPP_FGM3 = TM_FGM3, OPP_FGA3 = TM_FGA3, OPP_FTM = TM_FTM, OPP_FTA = TM_FTA, 
#        OPP_OR = TM_OR, OPP_DR = TM_DR, OPP_Ast = TM_Ast, OPP_TO = TM_TO, OPP_Stl = TM_Stl, OPP_Blk = TM_Blk, OPP_PF = TM_PF)


tourneyResults.persp1 <- tourneyCompactResults %>% 
  select(Season, WTeamID, LTeamID, WScore, LScore, WLoc) %>%
  mutate(WSite = ifelse(WLoc == "H",1, ifelse(WLoc == "A", -1, 0))) %>%
  rename(TM_TeamID = WTeamID, OPP_TeamID = LTeamID, TM_Score = WScore, OPP_Score = LScore, TM_SITE = WSite)

tourneyResults.persp2 <- tourneyCompactResults %>% 
  select(Season, WTeamID, LTeamID, WScore, LScore, WLoc) %>%
  mutate(LSite = ifelse(WLoc == "H",-1, ifelse(WLoc == "A", 1, 0))) %>%
  rename(TM_TeamID = LTeamID, OPP_TeamID = WTeamID, TM_Score = LScore, OPP_Score = WScore, TM_SITE = LSite)


tourneyResults <- bind_rows(tourneyResults.persp1, tourneyResults.persp2) %>% 
  arrange(Season, TM_TeamID, OPP_TeamID) %>% select(-WLoc)


teamPace <- tempGameResults %>% group_by(Season, TM_TeamID) %>% 
  summarize(TM_avgPace = mean(40*Poss / (40 + 5*NumOT))) %>% ungroup() 

### get all variables in the data frame to model 
tourneyGame.variables <- tourneyResults %>% 
  left_join(., allSeasonELO, by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>%
  rename(OPP_ELO = TM_ELO, OppName = TeamName) %>% 
  left_join(., allSeasonELO, by = c("Season", "TM_TeamID")) %>% 
  left_join(., select(allSeasonBPI, -TeamName), by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>% 
  rename(OPP_BPI = BPI) %>% 
  left_join(., select(allSeasonBPI, -TeamName), by = c("Season", "TM_TeamID")) %>% 
  rename(TM_BPI = BPI) %>% 
  left_join(., allSeasonBox.Adj, by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>% 
  rename(OPP_Score_ADJ = TM_Score_ADJ, OPP_FGM_ADJ = TM_FGM_ADJ, OPP_FGA_ADJ = TM_FGA_ADJ, OPP_FGM3_ADJ = TM_FGM3_ADJ, 
         OPP_FG3PCT_ADJ = TM_FG3PCT_ADJ, 
         OPP_FGA3_ADJ = TM_FGA3_ADJ, OPP_FTM_ADJ = TM_FTM_ADJ, OPP_FTA_ADJ = TM_FTA_ADJ, OPP_FG2PCT_ADJ = TM_FG2PCT_ADJ, OPP_FTPCT_ADJ = TM_FTPCT_ADJ,
         OPP_OR_ADJ = TM_OR_ADJ, OPP_DR_ADJ = TM_DR_ADJ, OPP_Ast_ADJ = TM_Ast_ADJ, OPP_TO_ADJ = TM_TO_ADJ, OPP_Stl_ADJ = TM_Stl_ADJ, OPP_Blk_ADJ = TM_Blk_ADJ, OPP_PF_ADJ = TM_PF_ADJ) %>% 
  left_join(., allSeasonBox.Adj, by = c("Season", "TM_TeamID")) %>% 
  left_join(., teamPace, by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>% 
  rename(OPP_avgPace = TM_avgPace) %>% 
  left_join(., teamPace, by = c("Season", "TM_TeamID")) %>% 
  left_join(., tourneySeeds, by = c("OPP_TeamID" = "TeamID", "Season")) %>%
  mutate(OPP_Seed = as.numeric(substr(Seed, 2,3))) %>% select(-Seed) %>%
  left_join(., tourneySeeds, by = c("TM_TeamID" = "TeamID", "Season")) %>% 
  mutate(TM_Seed = as.numeric(substr(Seed, 2,3))) %>% select(-Seed) %>% 
  mutate(TM_BPI_DIFF = TM_BPI - OPP_BPI, 
         TM_ELO_DIFF = TM_ELO - OPP_ELO,
         TM_SEED_DIFF = TM_Seed - OPP_Seed)



gamePred.modData <- tourneyGame.variables %>%
  mutate(TM_WL = ifelse(TM_Score > OPP_Score, 1, 0)) %>%
  filter(Season >= 2010) %>%
  select(-Season, - TM_TeamID, -OPP_TeamID, -TM_Score, -OPP_Score, - OppName, -TeamName,
         -OPP_BPI, -OPP_ELO) 


# set.seed(5)
# rf.Mod <- randomForest(TM_WL ~ ., data = gamePred.modData)
# varImpPlot(rf.Mod)

gamePred.modData$pred <- rf.Mod$predicted

gamePred.modData %>% mutate(pred.bin = cut_interval(pred, 10)) %>% group_by(pred.bin) %>% 
  summarize(actual.percent= mean(TM_WL), bin.size = n())

#use linear model instead
glm.Mod <- glm(TM_WL ~ -1 + . + 
                 TM_avgPace*TM_FG3PCT_ADJ*TM_BPI_DIFF + 
                 OPP_avgPace*OPP_FG3PCT_ADJ*TM_BPI_DIFF, 
               data = gamePred.modData %>% 
                 select(TM_WL,
                        
                        TM_BPI_DIFF, TM_ELO_DIFF, 
                        TM_SEED_DIFF, 
                        OPP_FTM_ADJ, 
                        OPP_Stl_ADJ, OPP_Blk_ADJ, 
                        OPP_PF_ADJ, OPP_FG2PCT_ADJ, OPP_FG3PCT_ADJ, 
                        
                        TM_FTM_ADJ, 
                        TM_Stl_ADJ, TM_Blk_ADJ, 
                        TM_PF_ADJ, TM_FG2PCT_ADJ, TM_FG3PCT_ADJ,
                        
                        TM_avgPace, OPP_avgPace
                        
                 ), 
               family = binomial(link ="probit"))
summary(glm.Mod)
BIC(glm.Mod)


### All Possible Matchups for current year's tournament

teamsInTourney <- filter(tourneySeeds, Season == currentSeason) %>% 
  select(TeamID) %>% unlist()
teamsInTourneySeeds <- filter(tourneySeeds, Season == currentSeason) %>%
  select(TeamID, Seed)

allPossibleMatchups <- expand.grid(teamsInTourney, teamsInTourney) %>% tbl_df() %>%
  rename(TM_TeamID = Var1, OPP_TeamID = Var2) %>% 
  left_join(., teamsInTourneySeeds, by = c("OPP_TeamID" = "TeamID")) %>%
  rename(OPP_Seed = Seed) %>%
  left_join(., teamsInTourneySeeds, by = c("TM_TeamID" = "TeamID")) %>%
  rename(TM_Seed = Seed) %>% 
  filter(TM_TeamID != OPP_TeamID) %>% 
  mutate(Season = currentSeason, 
         TM_Region = substr(TM_Seed, 1,1), 
         OPP_Region = substr(OPP_Seed, 1,1),
         TM_Seed = as.numeric(substr(TM_Seed, 2,3)), 
         OPP_Seed = as.numeric(substr(OPP_Seed, 2,3)),
         TM_SITE = ifelse(TM_Region != OPP_Region, 0,
                          ifelse(TM_Seed <= 4 & OPP_Seed >=5, 1,
                                 ifelse(TM_Seed >=5 & OPP_Seed <=4, -1, 0))) ) %>% 
  left_join(., allSeasonELO, by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>%
  rename(OPP_ELO = TM_ELO, OppName = TeamName) %>% 
  left_join(., allSeasonELO, by = c("Season", "TM_TeamID")) %>% 
  left_join(., select(allSeasonBPI, -TeamName), by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>% 
  rename(OPP_BPI = BPI) %>% 
  left_join(., select(allSeasonBPI, -TeamName), by = c("Season", "TM_TeamID")) %>% 
  rename(TM_BPI = BPI) %>% 
  left_join(., allSeasonBox.Adj, by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>% 
  rename(OPP_Score_ADJ = TM_Score_ADJ, OPP_FGM_ADJ = TM_FGM_ADJ, OPP_FGA_ADJ = TM_FGA_ADJ, OPP_FGM3_ADJ = TM_FGM3_ADJ, 
         OPP_FG3PCT_ADJ = TM_FG3PCT_ADJ,
         OPP_FGA3_ADJ = TM_FGA3_ADJ, OPP_FTM_ADJ = TM_FTM_ADJ, OPP_FTA_ADJ = TM_FTA_ADJ, OPP_FG2PCT_ADJ = TM_FG2PCT_ADJ, OPP_FTPCT_ADJ = TM_FTPCT_ADJ,
         OPP_OR_ADJ = TM_OR_ADJ, OPP_DR_ADJ = TM_DR_ADJ, OPP_Ast_ADJ = TM_Ast_ADJ, OPP_TO_ADJ = TM_TO_ADJ, OPP_Stl_ADJ = TM_Stl_ADJ, OPP_Blk_ADJ = TM_Blk_ADJ, OPP_PF_ADJ = TM_PF_ADJ) %>% 
  left_join(., allSeasonBox.Adj, by = c("Season", "TM_TeamID")) %>% 
  left_join(., teamPace, by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>% 
  rename(OPP_avgPace = TM_avgPace) %>% 
  left_join(., teamPace, by = c("Season", "TM_TeamID")) %>% 
  mutate(TM_BPI_DIFF = TM_BPI - OPP_BPI, 
         TM_ELO_DIFF = TM_ELO - OPP_ELO,
         TM_SEED_DIFF = TM_Seed - OPP_Seed)


# allPossibleMatchups$pred <- predict(rf.Mod, newdata = allPossibleMatchups)
allPossibleMatchups$pred <- predict(glm.Mod, newdata = allPossibleMatchups, type = 'response')

allPossibleMatchups <- allPossibleMatchups %>% 
  mutate(tempEventID = paste0(Season, pmax(TM_TeamID, OPP_TeamID), pmin(TM_TeamID, OPP_TeamID)) ) %>% 
  group_by(Season, tempEventID) %>%
  mutate(pred.adj = pred / sum(pred))


###### In each scenario, give 100% probability to either the most likely or 2nd most likely champion
### Scenario 1 -- UConn gets 100%
allPossibleMatchups %>% ungroup() %>%
  mutate(final.winpct = ifelse(TM_TeamID == 3163, 1, 
                               ifelse(OPP_TeamID == 3163, 0, pred.adj))) %>% 
  mutate(ID = paste(Season, TM_TeamID, OPP_TeamID, sep = "_"), 
         Pred = final.winpct) %>% 
  filter(TM_TeamID < OPP_TeamID) %>% 
  select(ID, Pred) %>% 
  write_csv(., paste0(currentSeason, "_Paul_1.csv"))
### Scenario 2 -- South Carolina gets 100 % everywhere
allPossibleMatchups %>% ungroup() %>%
  mutate(final.winpct = ifelse(TM_TeamID == 3376, 1, 
                               ifelse(OPP_TeamID == 3376, 0, pred.adj))) %>% 
  mutate(ID = paste(Season, TM_TeamID, OPP_TeamID, sep = "_"), 
         Pred = final.winpct) %>% 
  filter(TM_TeamID < OPP_TeamID) %>% 
  select(ID, Pred) %>% 
  write_csv(., paste0(currentSeason, "_Paul_2.csv"))