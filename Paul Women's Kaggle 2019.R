#LOADING EXTERNAL PACKAGES AND FUNCTIONS USED IN THIS WORK
# RFilesDirectory <- "\\\\etsig01\\SIG\\R Files"
ThisProgramDirectory <- "H:/Kaggle/Women's 2019"
ThisProgramDirectory <- "C:/Users/sabinr/Dropbox/Kaggle/Women's 2019"
# library(ROracle)
# library(dtplyr)
library(data.table)
library(tidyverse)
library(reshape2)
library(splines)
library(readr)
library(readxl)
library(lubridate)
library(Matrix)
library(elo)
library(randomForest)
library(rstan)
library(gbm)
options(dplyr.width = Inf)
options(stringsAsFactors = F)
options("scipen"=10, "digits"=4)

rfunctionspathWork <- "//etsig01/SIG/R Files/GenericSavedRFunctions.R"
rfunctionspathHome <- "/Users/psabin/Documents/Virginia Tech Files/Research/GenericSavedRFunctions.R"
if(file.exists(rfunctionspathWork)){source(rfunctionspathWork)}
if(file.exists(rfunctionspathHome)){source(rfunctionspathHome)}

setwd(ThisProgramDirectory)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm <- stan_model("ncaaw.stan")
nsamples <- 1000## for week by week rating

currentSeason <- 2019

# setwd(RFilesDirectory)
# source("PoibinModifiedFunctions.R")
# source("GenericSavedRFunctions.R")
# source("OracleConnectionFunctions.R")
# 
# source("\\\\etsig01\\SIG\\Men's CBB\\MCBB Metrics\\MCBB Metrics R Scripts\\MCBBMetrics_functions.R")
# source("\\\\etsig01\\SIG\\Men's CBB\\MCBB Metrics\\MCBB Metrics R Scripts\\NCAA Tournament Functions.R")
# #### map to SDR Team ID's
# con <- OpenOracleConnection("SDRPRD")
# SDRTeamNames <- QueryOracleTable(con,
#                                  query_text = "select team_id, team_name as team from
#                                  sdroltp.team_name names
#                                  where names.requestor_id = 0
#                                  and team_id IN (select distinct team_id from WCBB_ELO_TEAM_RATINGS)") %>% as_tibble


#Expands the internal memory limit
memory.limit(size = 8000000)
setwd(ThisProgramDirectory)

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

tourneyDayNumCutoff <- tourneyCompactResults %>% group_by(Season) %>% 
  summarize(tourneyStartDayNum = min(DayNum))



#  Elo ###
#Date Numbers for elo
gameDate.index <- gameCompactResults %>% 
  select(Season, DayNum) %>% unique() %>%
  arrange(Season, DayNum) %>%
  group_by(Season) %>% 
  mutate(DATENM = row_number(Season)) %>% ungroup

gameResults.elo <- left_join(gameCompactResults, gameDate.index, by = c("Season", "DayNum")) %>%
  mutate(W = if_else(WScore > LScore, 1, 0),
         HomeID = ifelse(WLoc == "H", WTeamID, LTeamID),
         AwayID = ifelse(WLoc == "A", WTeamID, LTeamID),
         HomeScore = ifelse(WLoc == "H", WScore, LScore),
         AwayScore = ifelse(WLoc == "H", LScore, WScore)) %>%
  left_join(., teams, by = c("HomeID" = "TeamID")) %>% rename(HomeTeam = TeamName) %>%
  left_join(., teams, by = c("AwayID" = "TeamID")) %>% rename(AwayTeam = TeamName) %>% 
  arrange(Season, DATENM) 

home.field <- 30
temp <- elo.run(score(HomeScore, AwayScore) ~ adjust(HomeTeam, home.field) +
                  AwayTeam + 
                  k(20*log(abs(HomeScore - AwayScore) + 1)) + 
          regress(Season, 1500, 0.2), 
        data = gameResults.elo, history = TRUE)

gameResults.elo$HomeElo <- temp$elos[,6]
gameResults.elo$AwayElo <- temp$elos[,7]


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

##temp1 has TM as the winning team and temp2 has TM as the losing team
tempGameResults.temp1 <- tempGameResults.temp1 %>% 
  mutate(site = case_when(TM_OPP_oc == "H" ~ 1, 
                          TM_OPP_oc == "A" ~ -1, 
                          TRUE ~ 0))
tempGameResults.temp2 <- tempGameResults.temp2 %>% 
  mutate(site = case_when(OPP_TM_oc == "H" ~ -1, 
                          OPP_TM_oc == "A" ~ 1, 
                          TRUE ~ 0))

tempGameResults <- bind_rows(tempGameResults.temp1, tempGameResults.temp2) %>% 
  arrange(Season, DayNum, TM_TeamID) %>% 
  mutate(netEff = (TM_Score - OPP_Score) / Poss) %>%
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
  mutate(BPI = NA_real_, tempo = NA_real_)
  
# all.ADJ_TEAM_SEASON_ID <- unique(MCBBGamesData$TM_SEASON_ID)
  
bpi.seasons <- select(tempGameResults, Season) %>% arrange(Season) %>% unique %>% unlist
set.seed(5)
N.iter <- 400
burnin <- 100
a = 1
b = 1/2
post.var <- matrix(1, nrow = (N.iter + burnin), ncol = length(bpi.seasons))
post.var[,1] <- 1/rgamma((N.iter+burnin), shape = a, rate = b)
for(i in 1:length(bpi.seasons)){
  mod.data <- filter(tempGameResults, Season == bpi.seasons[i])
  teamsThisSeason <- filter(allTeamsSeasons, Season == bpi.seasons[i]) %>%
    select(Season, TM_TeamID)
  n.teams <- length(unique(mod.data$TM_TeamID))
  y = mod.data$netEff
  N = length(mod.data$netEff)
  X.team.opp <- sparseMatrix(i = c(1:nrow(mod.data), 1:nrow(mod.data)), 
                             j = c(match(mod.data$TM_TeamID, teamsThisSeason$TM_TeamID), match(mod.data$OPP_TeamID, teamsThisSeason$TM_TeamID)),
                             x = rep(c(1,-1), each = nrow(mod.data)) )
  X.team.opp.raw <- abs(X.team.opp)
  X.team.opp <- cbind(X.team.opp, mod.data$TM_SITE.num, ns(mod.data$Diff.DaysRest, df = 3))
  XtX.team.opp <- t(X.team.opp) %*% X.team.opp
  Xty <- t(X.team.opp) %*% y
  
  prior.mean.beta = rep(0,ncol(X.team.opp))
  prior.prec.beta = 1 / (3^2)
  post.beta <- matrix(0, nrow = (N.iter + burnin), ncol = ncol(X.team.opp))
  
  for(iter in 2:(N.iter + burnin)){
    lambda = post.var[(iter - 1),i]
    post.beta.prec = lambda*XtX.team.opp + prior.prec.beta*diag(ncol(X.team.opp))
    post.beta.var = inv(post.beta.prec)
    post.beta.mean = post.beta.var%*%(lambda*Xty + prior.prec.beta*prior.mean.beta) %>% as.vector
    post.beta[iter, ] <- rmultnorm(1, mu = post.beta.mean, sigma = post.beta.var)[1,]
    post.beta[iter, 1:n.teams] <- post.beta[iter, 1:n.teams] - median(post.beta[iter, 1:n.teams])
    
    ss.temp = sum((y - X.team.opp%*%post.beta[iter,])^2)
    lambda.new = rgamma(1, shape = a + N/2, rate = b + 0.5*ss.temp)
    post.var[iter,i] = lambda.new
    cat("MCMC Iter: ", iter, "\r")
  }
  posterior_beta = apply(post.beta[-c(1:burnin),], 2, mean)
  team.bpi <- posterior_beta[1:n.teams]  - median(posterior_beta[1:n.teams])
  
  ### Possession Estimation
  team.poss.estimate = inv(t(X.team.opp.raw)%*%X.team.opp.raw + 0.1*diag(n.teams))%*%t(X.team.opp.raw)%*%mod.data$Poss
  
  teamsThisSeason <- teamsThisSeason %>% 
    mutate(temp.BPI = team.bpi*avgGamePoss, 
           temp.tempo = as.vector(team.poss.estimate)*2)
  allTeamsSeasons <- allTeamsSeasons %>% 
    left_join(., teamsThisSeason, by = c("Season", "TM_TeamID")) %>%
    mutate(BPI = ifelse(!is.na(temp.BPI), temp.BPI, BPI),
           tempo = ifelse(!is.na(temp.tempo), temp.tempo, tempo)) %>% 
    select(Season, TM_TeamID, BPI, tempo)
  cat(bpi.seasons[i], " finished \r")
}

allSeasonBPI <- allTeamsSeasons %>% 
  left_join(., teams, by = c("TM_TeamID" = "TeamID"))
# saveRDS(allSeasonBPI, "allSeasonBPI.rds")
# allSeasonBPI <- readRDS("allSeasonBPI.rds")

### get paces for each team too saved off

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


########## State-Space Model -- How Good Were Teams Across Time & RIGHT Before Tournament? 
### calculate weeks in each season
games.week = 2
tempGameResults <- tempGameResults %>% 
  arrange(Season, DayNum, TM_TeamID) %>% 
  group_by(Season, TM_TeamID) %>% 
  mutate(tmGameNo = row_number(TM_TeamID),
         week = ceiling(tmGameNo / games.week) ) %>% 
  ungroup

avg.poss = mean(tempGameResults$Poss )
time.varying.bpi <- NULL
for(i in 1:length(bpi.seasons)){
  mod.data <- filter(tempGameResults, Season == bpi.seasons[i])
  
  ### team integers
  team1 <- mod.data %>% select(TM_TeamID) %>% arrange %>% unique %>% unlist
  team2 <- mod.data %>% select(OPP_TeamID) %>% arrange %>% unique %>% unlist 
  unique.teams <- c(team1, team2) %>% unique
  teams.map <- tibble(teamid = unique.teams, team.int = 1:length(unique.teams)) %>% 
    left_join(filter(allSeasonBPI, Season == bpi.seasons[i]) %>% select(TM_TeamID, BPI), by = c("teamid" = "TM_TeamID"))
  
  ## only keep home games or lower team id of neutral games -- and each team's week no.
  mod.data1 <- mod.data %>% 
    mutate(EVENTID = paste0(DayNum, pmin(TM_TeamID, OPP_TeamID), pmax(TM_TeamID, OPP_TeamID)) ) %>% 
    filter(TM_SITE.num == 1 | (TM_SITE.num == 0 & TM_TeamID < OPP_TeamID)) %>% 
    select(DayNum, TM_TeamID, TM_Score, Poss,  site,  netEff, EVENTID, tmGameNo, week ) %>% 
    rename(HDayNum = DayNum, HTeamID = TM_TeamID, HScore = TM_Score, 
           HPoss = Poss, Hsite = site, HnetEff = netEff, HGameNo = tmGameNo, Hweek = week)
  
  mod.data2 <- mod.data %>% 
    mutate(EVENTID = paste0(DayNum, pmin(TM_TeamID, OPP_TeamID), pmax(TM_TeamID, OPP_TeamID)) ) %>% 
    filter(TM_SITE.num == -1 | (TM_SITE.num == 0 & TM_TeamID > OPP_TeamID)) %>% 
    select(DayNum, TM_TeamID, TM_Score, Poss,  site,  netEff, EVENTID, tmGameNo, week ) %>% 
    rename(ADayNum = DayNum, ATeamID = TM_TeamID, AScore = TM_Score, 
           APoss = Poss, Asite = site, AnetEff = netEff, AGameNo = tmGameNo, Aweek = week)
  
  mod.data <- left_join(mod.data1, mod.data2, by = "EVENTID") %>% 
    left_join(teams.map, by = c("HTeamID" = "teamid")) %>% rename(Hteam.int = team.int, HBPI = BPI) %>% 
    left_join(teams.map, by  = c("ATeamID" = "teamid")) %>% rename(Ateam.int = team.int, ABPI = BPI) %>% 
    mutate(HScore.Adj = HScore*(avg.poss/HPoss),
           AScore.Adj = AScore*(avg.poss/APoss))
  
  
  n.teams <- nrow(teams.map)
  n.weeks <- max(c(mod.data$Hweek, mod.data$Aweek))
  
  y = mod.data$HScore.Adj - mod.data$AScore.Adj
  n.games = nrow(mod.data)


  
  
  stanDat <- list(nteams = n.teams,
                  ngames = n.games, # number of games 
                  nweeks = n.weeks, # number of weeks 
                  home_week = mod.data$Hweek, # week number for the home team
                  away_week = mod.data$Aweek, # week number for the away team
                  home_team = mod.data$Hteam.int, #home team ID (1, ..., 20)
                  away_team = mod.data$Ateam.int, #away team ID (1, ..., 20)
                  score_diff = mod.data$HnetEff,#change this to net efficiency
                  prev_perf = teams.map$BPI)#prior
  
  fit <- sampling(sm, chains = 4, iter = (nsamples/2), data = stanDat)
  # saveRDS(fit, paste("FITS/fit_", w, ".rds", sep="")) ## save off at week w of the season
  sims <- extract(fit)
  temp.time.varying.bpi <- tibble(Season = integer(),
                                  week = integer(),
                                  teamid = integer(), 
                                  rating = numeric(), 
                                  rating.05 = numeric(), 
                                  rating.95 = numeric())
  for(w in 1:n.weeks){
    temp.time.varying.bpi <- tibble(Season = bpi.seasons[i],
                                    week = w, 
                                    teamid = teams.map$teamid,
                                    rating = apply(sims$a[-c(1:(nsamples/2)),w,]  , 2, mean)*(100/avg.poss),
                                    rating.05 = apply(sims$a[-c(1:(nsamples/2)),w,]  , 2, function(x)quantile(x, 0.05))*(100/avg.poss),
                                    rating.95 = apply(sims$a[-c(1:(nsamples/2)),w,]  , 2, function(x)quantile(x, 0.95))*(100/avg.poss)) %>% 
      bind_rows(temp.time.varying.bpi, .)
    
  }
  time.varying.bpi <- temp.time.varying.bpi %>% 
    bind_rows(time.varying.bpi, .)
  
  cat("Finished State-Space Season: ", bpi.seasons[i], '\n')
  
}
# saveRDS(time.varying.bpi, "time_varying.rds")
# time.varying.bpi <- readRDS("time_varying.rds")

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
                    select(Season, starts_with("TM_"))
allSeasonELO
allSeasonBPI
time.varying.bpi.end <- time.varying.bpi %>% 
  group_by(Season) %>% 
  filter(week == max(week)) %>% ungroup
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

### better team pace calculated as part of allSeasonBPI
# teamPace <- tempGameResults %>% group_by(Season, TM_TeamID) %>% 
#   summarize(TM_avgPace = mean(40*Poss / (40 + 5*NumOT))) %>% ungroup() 

### get all variables in the data frame to model 
tourneyGame.variables <- tourneyResults %>% 
  left_join(., allSeasonELO, by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>%
  rename(OPP_ELO = TM_ELO, OppName = TeamName) %>% 
  left_join(., allSeasonELO, by = c("Season", "TM_TeamID")) %>% 
  left_join(., select(allSeasonBPI, -TeamName), by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>% 
  rename(OPP_BPI = BPI, OPP_avgPace = tempo) %>% 
  left_join(., select(allSeasonBPI, -TeamName), by = c("Season", "TM_TeamID")) %>% 
  rename(TM_BPI = BPI, TM_avgPace = tempo) %>% 
  left_join(., select(time.varying.bpi.end, -week), by = c("Season", "OPP_TeamID" = "teamid")) %>% 
  rename(OPP_time.rating = rating, OPP_time.rating.05 = rating.05, OPP_time.rating.95 = rating.95) %>% 
  left_join(., select(time.varying.bpi.end, -week), by = c("Season", "TM_TeamID" = "teamid")) %>% 
  rename(TM_time.rating = rating, TM_time.rating.05 = rating.05, TM_time.rating.95 = rating.95) %>% 
  left_join(., select(allSeasonBox.Adj, -TM_FG3PCT_ADJ), by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>% 
  rename(OPP_Score_ADJ = TM_Score_ADJ, OPP_FGM_ADJ = TM_FGM_ADJ, OPP_FGA_ADJ = TM_FGA_ADJ, OPP_FGM3_ADJ = TM_FGM3_ADJ, 
         OPP_FGA3_ADJ = TM_FGA3_ADJ, OPP_FTM_ADJ = TM_FTM_ADJ, OPP_FTA_ADJ = TM_FTA_ADJ, OPP_FG2PCT_ADJ = TM_FG2PCT_ADJ, OPP_FTPCT_ADJ = TM_FTPCT_ADJ,
         OPP_OR_ADJ = TM_OR_ADJ, OPP_DR_ADJ = TM_DR_ADJ, OPP_Ast_ADJ = TM_Ast_ADJ, OPP_TO_ADJ = TM_TO_ADJ, OPP_Stl_ADJ = TM_Stl_ADJ, OPP_Blk_ADJ = TM_Blk_ADJ, OPP_PF_ADJ = TM_PF_ADJ) %>% 
  left_join(., select(allSeasonBox.Adj, -TM_FG3PCT_ADJ), by = c("Season", "TM_TeamID")) %>% 
  left_join(., tourneySeeds, by = c("OPP_TeamID" = "TeamID", "Season")) %>%
  mutate(OPP_Seed = as.numeric(substr(Seed, 2,3))) %>% select(-Seed) %>%
  left_join(., tourneySeeds, by = c("TM_TeamID" = "TeamID", "Season")) %>% 
  mutate(TM_Seed = as.numeric(substr(Seed, 2,3))) %>% select(-Seed)



gamePred.modData <- tourneyGame.variables %>%
  mutate(TM_WL = ifelse(TM_Score > OPP_Score, 1, 0)) %>%
  filter(Season >= 2010) %>%
  select(-Season, - TM_TeamID, -OPP_TeamID, -TM_Score, -OPP_Score, - OppName, -TeamName) 
 
########### Random Forest
set.seed(5)
rf.Mod <- randomForest(TM_WL ~ ., data = gamePred.modData)
varImpPlot(rf.Mod)
plot(rf.Mod$mse)
gamePred.modData$pred <- rf.Mod$predicted

gamePred.modData %>% mutate(pred.bin = cut_interval(pred, 10)) %>% group_by(pred.bin) %>% 
  summarize(actual.percent = mean(TM_WL), bin.size = n())

### Gradient Boosting
set.seed(5)
gbm.Mod <- gbm(TM_WL ~ ., n.trees = 150,
                    data = gamePred.modData, distribution = "bernoulli", 
                    cv.folds = 4, #
                    interaction.depth = 3, shrinkage = 0.05, n.cores = 5)
summary(gbm.Mod)
gbm.Mod$cv.error
## cv log loss for random forest and gradient model
logloss.tbl <- NULL
for(i in 1:length(bpi.seasons)){
  train.data <- tourneyGame.variables %>%
    mutate(TM_WL = ifelse(TM_Score > OPP_Score, 1, 0)) %>%
    filter(Season >=2010) %>% 
    filter(Season != bpi.seasons[i]) %>%
    select(-Season, -TM_TeamID, -OPP_TeamID, -TM_Score, -OPP_Score, - OppName, -TeamName) 
  test.data <- tourneyGame.variables %>%
    mutate(TM_WL = ifelse(TM_Score > OPP_Score, 1, 0)) %>%
    filter(Season >=2010) %>% 
    filter(Season == bpi.seasons[i]) %>%
    select(-Season, -TM_TeamID, -OPP_TeamID, -TM_Score, -OPP_Score, - OppName, -TeamName) 
  set.seed(5)
  train.rf <- randomForest(TM_WL ~ ., data = train.data, ntrees = 350)
  train.gbm <- gbm(TM_WL ~ ., n.trees = 350,
                   data = train.data, distribution = "bernoulli", 
                   cv.folds = 4, interaction.depth = 3, shrinkage = 0.03, n.cores = 5)
  rf.pred = predict(train.rf, newdata = test.data)
  gbm.pred = predict(train.gbm, newdata = test.data, type = "response")
  
  logloss.tbl <- test.data %>% 
    mutate(rf = rf.pred, 
           gbm = gbm.pred) %>% 
    mutate(rf = case_when(rf == 1 ~ 0.999, 
                          rf == 0 ~ 0.001,
                          TRUE ~ rf), 
           gbm = case_when(gbm == 1 ~ 0.999, 
                           gbm == 0 ~ 0.001, 
                           TRUE ~ gbm)) %>% 
    summarize(rf.logloss = - mean(TM_WL*log(rf) + (1-TM_WL)*log(1-rf)),
              gbm.logloss = - mean(TM_WL*log(gbm) + (1-TM_WL)*log(1-gbm)),
              games = n()) %>% 
    mutate(test.season = bpi.seasons[i]) %>% 
    bind_rows(logloss.tbl, .)
  cat('cv season: ', bpi.seasons[i], ' complete \n')
  
}
logloss.tbl %>% summarize_at(vars(contains("logloss")), funs(mean))
### stick with the random forest


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
  rename(OPP_BPI = BPI, OPP_avgPace = tempo) %>% 
  left_join(., select(allSeasonBPI, -TeamName), by = c("Season", "TM_TeamID")) %>% 
  rename(TM_BPI = BPI, TM_avgPace = tempo) %>% 
  left_join(., select(time.varying.bpi.end, -week), by = c("Season", "OPP_TeamID" = "teamid")) %>% 
  rename(OPP_time.rating = rating, OPP_time.rating.05 = rating.05, OPP_time.rating.95 = rating.95) %>% 
  left_join(., select(time.varying.bpi.end, -week), by = c("Season", "TM_TeamID" = "teamid")) %>% 
  rename(TM_time.rating = rating, TM_time.rating.05 = rating.05, TM_time.rating.95 = rating.95) %>% 
  left_join(., select(allSeasonBox.Adj, -TM_FG3PCT_ADJ), by = c("Season", "OPP_TeamID" = "TM_TeamID")) %>% 
  rename(OPP_Score_ADJ = TM_Score_ADJ, OPP_FGM_ADJ = TM_FGM_ADJ, OPP_FGA_ADJ = TM_FGA_ADJ, OPP_FGM3_ADJ = TM_FGM3_ADJ, 
         OPP_FGA3_ADJ = TM_FGA3_ADJ, OPP_FTM_ADJ = TM_FTM_ADJ, OPP_FTA_ADJ = TM_FTA_ADJ, OPP_FG2PCT_ADJ = TM_FG2PCT_ADJ, OPP_FTPCT_ADJ = TM_FTPCT_ADJ,
         OPP_OR_ADJ = TM_OR_ADJ, OPP_DR_ADJ = TM_DR_ADJ, OPP_Ast_ADJ = TM_Ast_ADJ, OPP_TO_ADJ = TM_TO_ADJ, OPP_Stl_ADJ = TM_Stl_ADJ, OPP_Blk_ADJ = TM_Blk_ADJ, OPP_PF_ADJ = TM_PF_ADJ) %>% 
  left_join(., select(allSeasonBox.Adj, -TM_FG3PCT_ADJ), by = c("Season", "TM_TeamID"))


allPossibleMatchups$pred <- predict(rf.Mod, newdata = allPossibleMatchups)

allPossibleMatchups <- allPossibleMatchups %>% 
  mutate(tempEventID = paste0(Season, pmax(TM_TeamID, OPP_TeamID), pmin(TM_TeamID, OPP_TeamID)) ) %>% 
  group_by(Season, tempEventID) %>%
  mutate(pred.adj = pred / sum(pred))

# sdr.teamID.map <- SDRTeamNames %>%
#   mutate(TeamNameSpelling = tolower(TEAM)) %>%
#   left_join(teamSpellings, by = c("TeamNameSpelling")) %>%
#   filter(!is.na(TeamID)) %>% 
#   bind_rows(tibble(TEAM_ID = 6625, TEAM = "Miami", TeamNameSpelling = "miami - florida", TeamID = 3274))
# allPossibleMatchups %>% ungroup %>%
#   select(TM_TeamID, OPP_TeamID, TM_SITE, pred.adj) %>%
#   left_join(select(sdr.teamID.map, TEAM_ID, TeamID), by = c("OPP_TeamID" = "TeamID")) %>%
#   rename(OPP_ID = TEAM_ID) %>%
#   left_join(select(sdr.teamID.map, TEAM_ID, TeamID), by = c("TM_TeamID" = "TeamID")) %>%
#   select(TEAM_ID, OPP_ID, TM_SITE, pred.adj) %>%
#   write_csv("Womens All Possible Kaggle SDR ID.csv")

###### In each scenario, give 100% probability to 1 and 2 seeds in first round
# then either the most likely or 2nd most likely champion
### Scenario 1 -- Baylor gets 100%
allPossibleMatchups %>% ungroup() %>%
  mutate(final.winpct = ifelse(TM_TeamID == 3124, 1, 
                               ifelse(OPP_TeamID == 3124, 0, pred.adj))) %>% 
  mutate(ID = paste(Season, TM_TeamID, OPP_TeamID, sep = "_"), 
         Pred = final.winpct) %>% 
  filter(TM_TeamID < OPP_TeamID) %>% 
  select(ID, Pred) %>% 
  write_csv(., "2019_Paul_1.csv")
### Scenario 2 -- Notre Dame gets 100 % everywhere
allPossibleMatchups %>% ungroup() %>%
  mutate(final.winpct = ifelse(TM_TeamID == 3323, 1, 
                               ifelse(OPP_TeamID == 3323, 0, pred.adj))) %>% 
  mutate(ID = paste(Season, TM_TeamID, OPP_TeamID, sep = "_"), 
         Pred = final.winpct) %>% 
  filter(TM_TeamID < OPP_TeamID) %>% 
  select(ID, Pred) %>% 
  write_csv(., "2019_Paul_2.csv")