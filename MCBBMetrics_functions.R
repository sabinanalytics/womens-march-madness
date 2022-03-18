
### Elementwise addition and subtraction functions that replace NA's to 0's. 
`%+%` <- function(e1, e2) {e1[is.na(e1)] <- 0; e2[is.na(e2)] <- 0; return(e1 + e2)}
`%-%` <- function(e1, e2) {e1[is.na(e1)] <- 0; e2[is.na(e2)] <- 0; return(e1 - e2)}

##Inverse of a Matrix Using Choleksy Decomposition -- Faster than "solve()" -- Must be Positive Definite
#Last Updated: January 27, 2016
inv <- function(matrix){
  chol2inv(chol(matrix))
}

##### Converting 247 sports composite recruiting ranking to a 0-100 scale to use 
### Last Updated : October 19th, 2017
recruitCompositeCDF = function(recruitComposite){
  coef <- c(4.258851e-01,  1.495483e-02, -1.096363e-04,  1.644109e-07)
  cdf <- cbind(1, recruitComposite, recruitComposite^2, recruitComposite^3) %*% coef
  cdf <- ifelse(is.na(cdf), 0.4,
                ifelse(cdf>1,1,
                       ifelse(cdf <0.4, 0.4, cdf)) )
  return(cdf)
}

### Game Control -- Last Updated November 10, 2017 by Paul Sabin
game_control <- function(avgwp, persp = "top"){
  logit <- log(avgwp/ (1 - avgwp))
  if(persp == "top"){
    return(pnorm(logit,  0.817718038, 2.20252301)*100)
  }else{
    return(pnorm(logit,  -0.137145372, 2.18539167)*100)
  }
}
game_control <- Vectorize(game_control)

### Makes a Diagonal Matrix where the spots on the diagonal are matrices themselves. Not required to be square matrix
#(A O O)
#(0 B 0)
#(0 0 C) where A,B,C are matrices of any dimension and the 0's are matrices of appropriate dimensions
#Updated September 7th, 2016 
#by Paul Sabin
matrixDiagonal <- function(...){  
  matrixList<-list(...)
  #if(is.list(matrixList[[1]])) matrixList<-matrixList[[1]]
  
  dimension1<-sapply(matrixList,FUN=function(x) dim(x)[1])
  dimension2<-sapply(matrixList,FUN=function(x) dim(x)[2])
  finalDimension1<-sum(dimension1)
  finalDimension2<-sum(dimension2)
  finalMatrix<-matrix(0,nrow=finalDimension1,ncol=finalDimension2)
  index1 <- index2 <-1
  for(k in 1:length(dimension1)){
    finalMatrix[index1:(index1+dimension1[k]-1),index2:(index2+dimension2[k]-1)]<-matrixList[[k]]
    index1<-index1+dimension1[k]
    index2<-index2+dimension2[k]
  }
  finalMatrix
}

MCBB_MATCHUP_QUALITY <- function(TM_WINPROB, TM_BPI_OFF, TM_BPI_DEF,TM_BPI_RANK, 
                                 OPP_BPI_OFF, OPP_BPI_DEF, OPP_BPI_RANK,n_TeamsThisSeason){
  inv_winprob_diff <- 1/abs(TM_WINPROB - (1-TM_WINPROB))
  winprob_part <- pexp(inv_winprob_diff, 1/2)
  ### mean BPI_OFF of any 2 teams:0, sd: 3.303188 (DEF_BPI b/w two teams:2.931699)
  #team BPI rankings Part
  bpiRank_part <- 1-pexp((TM_BPI_RANK + OPP_BPI_RANK)/2, 2/n_TeamsThisSeason)
  
  off_part <- pnorm((TM_BPI_OFF + OPP_BPI_OFF)/2,0, 3.303)
  def_part <- pnorm((TM_BPI_DEF + OPP_BPI_DEF)/2, 0 , 2.931699)
  
  matchup_quality <- 100*(0.3*winprob_part + 0.3*bpiRank_part + 0.25*off_part + 0.15*def_part)
  return(matchup_quality)
  
}


#### This function takes the team's pace information for prior years and produces a Estimated Prior Pace Effect
## For each team. (Pace effect is how far off on average your team is from the season average for pace)
PriorPace <- function(Data){
  value <- -0.162860545 + 0.068358106*Data$COACH_PACE_EFFECT +  0.617272717*Data$TEAM_PACE_EFFECT_LAST_SEASON + 
    0.005205766*Data$COACH_PACE_EFFECT_VAR +  -0.001919262*Data$TEAM_PACE_EFFECT_VAR_LAST_SEASON + 
    0.010431383*Data$COACH_PACE_EFFECT*Data$COACH_PACE_EFFECT_VAR  + 
    -0.009951882*Data$TEAM_PACE_EFFECT_LAST_SEASON*Data$TEAM_PACE_EFFECT_VAR_LAST_SEASON
  return(coalesce(value,0))
}

predPace <- function(SeasonPaceMeanCoef, TeamCoeffAndMapping, PaceGameVariablesCoefficients, TEAM_ID, OPP_ID,
                     DIFF_DAYS_REST, DIFF_DIST_FROM_HOME, DIFF_HOMECOURT, returnVar = F){
  
  #### Game Level Factors that go into Model
  Z.DaysRest <- predict(Diff.DaysRest.basis,DIFF_DAYS_REST)
  Z.DistFromHome <- predict(Diff.DistFromHome.basis,DIFF_DIST_FROM_HOME)
  Z <- cbind(Z.DaysRest*DIFF_HOMECOURT, Z.DistFromHome)  %>% 
       as.matrix()
  Zstar <- t(t(Z) - apply(Z,2,mean)) ### subtract column averages from each value of Z
  
  TEAM_PACE_EFFECTS <- TeamCoeffAndMapping$TEAM_PACE_EFFECTS[match(TEAM_ID, TeamCoeffAndMapping$ADJ_TEAM_ID)]
  OPP_PACE_EFFECTS <- TeamCoeffAndMapping$TEAM_PACE_EFFECTS[match(OPP_ID, TeamCoeffAndMapping$ADJ_TEAM_ID)]
  
  predPace <- SeasonPaceMeanCoef + TEAM_PACE_EFFECTS + OPP_PACE_EFFECTS + Zstar%*%PaceGameVariablesCoefficients
}



bpi_pts <- function(T.,TtT, Tty, PreseasonOffense, PreseasonDefense, PriorWeightDenominator = 10){
  
  n_teams <- dim(PreseasonOffense)[1]
  m_gamma <- c(3.1809583784602, 2.9939629626618, 3.2015886176166, 0.0760416896543, 0.0076999156509,
               0.0443733564604, -0.0002334295195, 43.1733381752075, 135.9029447164942,
               17.9070694356501, -48.4079884178492, -123.2449054279088, -16.1836903195684, 66.2066796796366)
  n_gameCoefficients <- length(m_gamma)
  
  
  # ### Use Non-linear effects for Recruiting Rankings
  maxRecruitingRtg <- 100
  minRecruitingRtg <- 70
  
  Recruiting.basis <- ns(seq(minRecruitingRtg, maxRecruitingRtg, length = 1000), knots = c(94),
                         Boundary.knots = c(minRecruitingRtg, maxRecruitingRtg))
  
  RecruitingPredictors.Off <- predict(Recruiting.basis, PreseasonOffense$AVG_ALL_RECRUITS_GRADE)
  RecruitingPredictors.Def <- predict(Recruiting.basis, PreseasonDefense$AVG_ALL_RECRUITS_GRADE)
  
  PreseasonOffense <- mutate(PreseasonOffense,
                             RecruitingOffense1 = RecruitingPredictors.Off[,1], RecruitingOffense2 = RecruitingPredictors.Off[,2])
  PreseasonDefense <- mutate(PreseasonDefense,
                             RecruitingDefense1 = RecruitingPredictors.Def[,1], RecruitingDefense2 = RecruitingPredictors.Def[,2])
  
  ### Prior Parameter Matrices
  W.Off <- model.matrix( ~ PLAYER_OFF_RTG + COACH_AVG_OFFRTG + PRIOR_YEAR_WEIGHT +
                           COMPOSITE_5_STARS + recruitComposite.cdf + PRIOR_BPI_OFF + 
                           PLAYER_OFF_RTG:COACH_AVG_OFFRTG +
                           PLAYER_OFF_RTG:PRIOR_YEAR_WEIGHT - 1, data = PreseasonOffense)
  W.Def <- model.matrix(~ PRIOR_YEAR_WEIGHT + PLAYER_DEF_RTG + RETURNING_COACH + PRIOR_BPI_DEF + 
                          COACH_AVG_DEFRTG + RecruitingDefense1 + PLAYER_DEF_RTG:RETURNING_COACH +
                          RETURNING_COACH:COACH_AVG_DEFRTG + RETURNING_COACH:RecruitingDefense1 -
                          1, data = PreseasonDefense)
  # W.Off <- model.matrix( ~ PLAYER_OFF_RTG + COACH_AVG_OFFRTG + PRIOR_YEAR_WEIGHT +
  #                          COMPOSITE_5_STARS + PLAYER_OFF_RTG:COACH_AVG_OFFRTG +
  #                          PLAYER_OFF_RTG:PRIOR_YEAR_WEIGHT - 1, data = PreseasonOffense)
  # W.Def <- model.matrix(~ PRIOR_YEAR_WEIGHT + PLAYER_DEF_RTG + RETURNING_COACH +
  #                         COACH_AVG_DEFRTG + RecruitingDefense1 + PLAYER_DEF_RTG:RETURNING_COACH +
  #                         RETURNING_COACH:COACH_AVG_DEFRTG + RETURNING_COACH:RecruitingDefense1 -
  #                         1, data = PreseasonDefense)
  W <- matrixDiagonal(W.Off, W.Def, diag(n_gameCoefficients))
  
  #Prior mean model coefficients
  alpha.hat.offense <- c(-0.12228361304, -0.14368339436, -16.75604006399, 0.81460298491, 0.86576670458, 0.12993917356,
                         0.00239542524, 0.21656599573)
  alpha.hat.defense <- c(3.2722227520, -0.1199914416, 19.7346671049, 0.1507978423, 0.0916448616, 1.2762685778, 
                         -0.0514498622, -0.1751398344, -0.2905031833 )
  # alpha.hat.offense <- c(-0.15511663,  -0.17010982,-19.99337876, 1.01051782, 0.00311789, 0.24646080)
  # alpha.hat.defense <- c(3.48330257577,  -0.17737229181, 28.37434038146, 0.14761256613, 1.98615565770, 
  #                        -0.06083534011,  -0.26608768803, -1.04225630565)
  alpha.hat <- c(alpha.hat.offense, alpha.hat.defense, m_gamma)
  
  #Prior Precision Model Coefficients
  off_prior_var_coef <- c(0.203972, -0.016558, 0.002164)
  def_prior_var_coef <- c(1.276578, -0.835656,  0.165364, -0.008261, -0.003608, 0.008893, -0.002239)
  
  ##Precision and Variance Terms
  #this predicts end of season posterior variance. Prior Variance is (1/30) this prediction (about 30 games for most teams)
  prior_var_scale <- PriorWeightDenominator
  W.Off.var.mat <- model.matrix( ~ RETURNING_COACH + COACH_SD_OFFRTG, data = PreseasonOffense)
  W.Def.var.mat <- model.matrix( ~ RETURNING_COACH + PRIOR_YEAR_WEIGHT + 
                                   PLAYER_DEF_RTG + AVG_ALL_RECRUITS_GRADE + RETURNING_COACH:PLAYER_DEF_RTG + 
                                   PRIOR_YEAR_WEIGHT:PLAYER_DEF_RTG, data = PreseasonDefense ) 
  lambda.O <- 1/(exp(W.Off.var.mat%*%off_prior_var_coef) * prior_var_scale)
  lambda.D <- 1/(exp(W.Def.var.mat%*%def_prior_var_coef) * prior_var_scale)
  
  lambda.gamma <- 0.0004168427955
  lambda.model <- 0.0247895172752
  Lambda <- c(lambda.O, lambda.D, rep(lambda.gamma, n_gameCoefficients))* 
    diag(2*n_teams + n_gameCoefficients)
  
  
  var.u <- inv(lambda.model*TtT + Lambda)
  exp.u <- var.u%*%(lambda.model*Tty + Lambda%*%W%*%alpha.hat)
  
  
  offense_team_raw <-  exp.u[1:n_TeamsThisSeason]
  defense_team_raw <- exp.u[(n_TeamsThisSeason+1):(2*n_TeamsThisSeason)]
  Game_Coef <- exp.u[(2*n_TeamsThisSeason+1):(2*n_TeamsThisSeason + ncol_z)]
  
  BPI_Off <- offense_team_raw - mean(offense_team_raw)
  BPI_Def <- defense_team_raw - mean(defense_team_raw)
  BPI_Tot <- BPI_Off + BPI_Def
  
  diag.var.u <- diag(var.u)
  BPI_Off_var <- diag.var.u[1:n_TeamsThisSeason] /prior_var_scale
  BPI_Def_var <- diag.var.u[(n_TeamsThisSeason+1):(2*n_TeamsThisSeason)]/prior_var_scale
  Game_Coef_var <- diag.var.u[(2*n_TeamsThisSeason+1):(2*n_TeamsThisSeason + ncol_z)]
  return(list(BPI_Off = BPI_Off, BPI_Def = BPI_Def, BPI_Tot = BPI_Tot,offense_team_raw = offense_team_raw, 
              defense_team_raw = defense_team_raw, Game_Coef = Game_Coef,
              BPI_Off_var = BPI_Off_var, BPI_Def_var = BPI_Def_var, Game_Coef_var = Game_Coef_var,
              Coef_Var_Matrix = var.u))
}


############ BPI Calculations given prior parameters and weight paramter for the priors
bpi_preseason <- function(PreseasonOffense, PreseasonDefense, PriorWeightDenominator = 10){
  
  
  ###################################################################################################
  n_teams <- dim(PreseasonOffense)[1]
  m_gamma <- c(3.1809583784602, 2.9939629626618, 3.2015886176166, 0.0760416896543, 0.0076999156509,
               0.0443733564604, -0.0002334295195, 43.1733381752075, 135.9029447164942,
               17.9070694356501, -48.4079884178492, -123.2449054279088, -16.1836903195684, 66.2066796796366)
  n_gameCoefficients <- length(m_gamma)
  
  
  # ### Use Non-linear effects for Recruiting Rankings
  maxRecruitingRtg <- 100
  minRecruitingRtg <- 70
  
  Recruiting.basis <- ns(seq(minRecruitingRtg, maxRecruitingRtg, length = 1000), knots = c(94),
                         Boundary.knots = c(minRecruitingRtg, maxRecruitingRtg))
  
  RecruitingPredictors.Off <- predict(Recruiting.basis, PreseasonOffense$AVG_ALL_RECRUITS_GRADE)
  RecruitingPredictors.Def <- predict(Recruiting.basis, PreseasonDefense$AVG_ALL_RECRUITS_GRADE)
  
  PreseasonOffense <- mutate(PreseasonOffense,
                             RecruitingOffense1 = RecruitingPredictors.Off[,1], RecruitingOffense2 = RecruitingPredictors.Off[,2])
  PreseasonDefense <- mutate(PreseasonDefense,
                             RecruitingDefense1 = RecruitingPredictors.Def[,1], RecruitingDefense2 = RecruitingPredictors.Def[,2])
  
  ### Prior Parameter Matrices
  W.Off <- model.matrix( ~ PLAYER_OFF_RTG + COACH_AVG_OFFRTG + PRIOR_YEAR_WEIGHT +
                           COMPOSITE_5_STARS + recruitComposite.cdf + PRIOR_BPI_OFF + 
                           PLAYER_OFF_RTG:COACH_AVG_OFFRTG +
                           PLAYER_OFF_RTG:PRIOR_YEAR_WEIGHT - 1, data = PreseasonOffense)
  W.Def <- model.matrix(~ PRIOR_YEAR_WEIGHT + PLAYER_DEF_RTG + RETURNING_COACH + PRIOR_BPI_DEF + 
                          COACH_AVG_DEFRTG + RecruitingDefense1 + PLAYER_DEF_RTG:RETURNING_COACH +
                          RETURNING_COACH:COACH_AVG_DEFRTG + RETURNING_COACH:RecruitingDefense1 -
                          1, data = PreseasonDefense)
  # W.Off <- model.matrix( ~ PLAYER_OFF_RTG + COACH_AVG_OFFRTG + PRIOR_YEAR_WEIGHT +
  #                          COMPOSITE_5_STARS + PLAYER_OFF_RTG:COACH_AVG_OFFRTG +
  #                          PLAYER_OFF_RTG:PRIOR_YEAR_WEIGHT - 1, data = PreseasonOffense)
  # W.Def <- model.matrix(~ PRIOR_YEAR_WEIGHT + PLAYER_DEF_RTG + RETURNING_COACH +
  #                         COACH_AVG_DEFRTG + RecruitingDefense1 + PLAYER_DEF_RTG:RETURNING_COACH +
  #                         RETURNING_COACH:COACH_AVG_DEFRTG + RETURNING_COACH:RecruitingDefense1 -
  #                         1, data = PreseasonDefense)
  W <- matrixDiagonal(W.Off, W.Def, diag(n_gameCoefficients))
  
  #Prior mean model coefficients
  alpha.hat.offense <- c(-0.12228361304, -0.14368339436, -16.75604006399, 0.81460298491, 0.86576670458, 0.12993917356,
                         0.00239542524, 0.21656599573)
  alpha.hat.defense <- c(3.2722227520, -0.1199914416, 19.7346671049, 0.1507978423, 0.0916448616, 1.2762685778, 
                         -0.0514498622, -0.1751398344, -0.2905031833 )
  # alpha.hat.offense <- c(-0.15511663,  -0.17010982,-19.99337876, 1.01051782, 0.00311789, 0.24646080)
  # alpha.hat.defense <- c(3.48330257577,  -0.17737229181, 28.37434038146, 0.14761256613, 1.98615565770, 
  #                        -0.06083534011,  -0.26608768803, -1.04225630565)
  alpha.hat <- c(alpha.hat.offense, alpha.hat.defense, m_gamma)
  #Prior Precision Model Coefficients
  off_prior_var_coef <- c(0.203972, -0.016558, 0.002164)
  def_prior_var_coef <- c(1.276578, -0.835656,  0.165364, -0.008261, -0.003608, 0.008893, -0.002239)
  
  ##Precision and Variance Terms
  #this predicts end of season posterior variance. Prior Variance is (1/30) this prediction (about 30 games for most teams)
  prior_var_scale <- PriorWeightDenominator
  W.Off.var.mat <- model.matrix( ~ RETURNING_COACH + COACH_SD_OFFRTG, data = PreseasonOffense)
  W.Def.var.mat <- model.matrix( ~ RETURNING_COACH + PRIOR_YEAR_WEIGHT + 
                                   PLAYER_DEF_RTG + AVG_ALL_RECRUITS_GRADE + RETURNING_COACH:PLAYER_DEF_RTG + 
                                   PRIOR_YEAR_WEIGHT:PLAYER_DEF_RTG, data = PreseasonDefense ) 
  lambda.O <- 1/(exp(W.Off.var.mat%*%off_prior_var_coef) * prior_var_scale)
  lambda.D <- 1/(exp(W.Def.var.mat%*%def_prior_var_coef) * prior_var_scale)
  
  lambda.gamma <- 0.0004168427955
  lambda.model <- 0.0247895172752
  Lambda <- c(lambda.O, lambda.D, rep(lambda.gamma, n_gameCoefficients))* 
    diag(2*n_teams + n_gameCoefficients)
  
  
  var.u <- inv(Lambda)
  exp.u <- W%*%alpha.hat
  
  
  offense_team_raw <-  exp.u[1:n_TeamsThisSeason]
  defense_team_raw <- exp.u[(n_TeamsThisSeason+1):(2*n_TeamsThisSeason)]
  Game_Coef <- exp.u[(2*n_TeamsThisSeason)+1:(2*n_TeamsThisSeason + length(m_gamma))]
  
  BPI_Off <- offense_team_raw - mean(offense_team_raw)
  BPI_Def <- defense_team_raw - mean(defense_team_raw)
  BPI_Tot <- BPI_Off + BPI_Def
  
  diag.var.u <- diag(var.u)
  BPI_Off_var <- diag.var.u[1:n_TeamsThisSeason]/prior_var_scale
  BPI_Def_var <- diag.var.u[(n_TeamsThisSeason+1):(2*n_TeamsThisSeason)]/prior_var_scale
  Game_Coef_var <- diag.var.u[(2*n_TeamsThisSeason+1):(2*n_TeamsThisSeason + length(m_gamma))]
  return(list(BPI_Off = BPI_Off, BPI_Def = BPI_Def, BPI_Tot = BPI_Tot,offense_team_raw = offense_team_raw, 
              defense_team_raw = defense_team_raw, Game_Coef = Game_Coef,
              BPI_Off_var = BPI_Off_var, BPI_Def_var = BPI_Def_var, Game_Coef_var = Game_Coef_var,
              Coef_Var_Matrix = var.u ))
}


# bpi_pts <- function(T.,TtT, Tty, PreseasonOffense, PreseasonDefense, PriorWeightDenominator = 10){
#   
#   n_teams <- dim(PreseasonOffense)[1]
#   m_gamma <- c(3.41691, 16.35033, 2.50442, -2.75857, -7.31174,0.84895, -0.00017, 36.01116, 116.30406,
#                15.18773, -43.00823, -111.69943, -16.49531, 63.77014)
#   n_gameCoefficients <- length(m_gamma)
#   
#   
#   # ### Use Non-linear effects for Recruiting Rankings
#   maxRecruitingRtg <- 100
#   minRecruitingRtg <- 70
#   
#   Recruiting.basis <- ns(seq(minRecruitingRtg, maxRecruitingRtg, length = 1000), knots = c(94),
#                          Boundary.knots = c(minRecruitingRtg, maxRecruitingRtg))
#  
#   RecruitingPredictors.Off <- predict(Recruiting.basis, PreseasonOffense$AVG_ALL_RECRUITS_GRADE)
#   RecruitingPredictors.Def <- predict(Recruiting.basis, PreseasonDefense$AVG_ALL_RECRUITS_GRADE)
#   
#   PreseasonOffense <- mutate(PreseasonOffense,
#                              RecruitingOffense1 = RecruitingPredictors.Off[,1], RecruitingOffense2 = RecruitingPredictors.Off[,2])
#   PreseasonDefense <- mutate(PreseasonDefense,
#                              RecruitingDefense1 = RecruitingPredictors.Def[,1], RecruitingDefense2 = RecruitingPredictors.Def[,2])
#   
#   ### Prior Parameter Matrices
#   W.Off <- model.matrix( ~ PLAYER_OFF_RTG + COACH_AVG_OFFRTG + PRIOR_YEAR_WEIGHT +
#                            COMPOSITE_5_STARS + PLAYER_OFF_RTG:COACH_AVG_OFFRTG +
#                            PLAYER_OFF_RTG:PRIOR_YEAR_WEIGHT - 1, data = PreseasonOffense)
#   W.Def <- model.matrix(~ PRIOR_YEAR_WEIGHT + PLAYER_DEF_RTG + RETURNING_COACH +
#                           COACH_AVG_DEFRTG + RecruitingDefense1 + PLAYER_DEF_RTG:RETURNING_COACH +
#                           RETURNING_COACH:COACH_AVG_DEFRTG + RETURNING_COACH:RecruitingDefense1 -
#                           1, data = PreseasonDefense)
#   W <- matrixDiagonal(W.Off, W.Def, diag(n_gameCoefficients))
#   
#   #Prior mean model coefficients
#   alpha.hat.offense <- c(-0.16002880979,  -0.16546230999,-20.37518985312, 1.03609409717, 0.00310774723, 0.25505183281)
#   alpha.hat.defense <- c(3.1551651607,  -0.2092674845, 30.0494004964, 0.1822793432, 1.5760643696, -0.0530757048,  -0.2950450974, -0.8515914234)
#   alpha.hat <- c(alpha.hat.offense, alpha.hat.defense, m_gamma)
#   
#   #Prior Precision Model Coefficients
#   off_prior_var_coef <- c(0.203972, -0.016558, 0.002164)
#   def_prior_var_coef <- c(1.276578, -0.835656,  0.165364, -0.008261, -0.003608, 0.008893, -0.002239)
#   
#   ##Precision and Variance Terms
#   #this predicts end of season posterior variance. Prior Variance is (1/30) this prediction (about 30 games for most teams)
#   prior_var_scale <- PriorWeightDenominator
#   W.Off.var.mat <- model.matrix( ~ RETURNING_COACH + COACH_SD_OFFRTG, data = PreseasonOffense)
#   W.Def.var.mat <- model.matrix( ~ RETURNING_COACH + PRIOR_YEAR_WEIGHT + 
#                                    PLAYER_DEF_RTG + AVG_ALL_RECRUITS_GRADE + RETURNING_COACH:PLAYER_DEF_RTG + 
#                                    PRIOR_YEAR_WEIGHT:PLAYER_DEF_RTG, data = PreseasonDefense ) 
#   lambda.O <- 1/(exp(W.Off.var.mat%*%off_prior_var_coef) * prior_var_scale)
#   lambda.D <- 1/(exp(W.Def.var.mat%*%def_prior_var_coef) * prior_var_scale)
#   
#   lambda.gamma <- 0.000528739
#   lambda.model <- 0.02031624
#   Lambda <- c(lambda.O, lambda.D, rep(lambda.gamma, n_gameCoefficients))* 
#     diag(2*n_teams + n_gameCoefficients)
#   
#   
#   var.u <- inv(lambda.model*TtT + Lambda)
#   exp.u <- var.u%*%(lambda.model*Tty + Lambda%*%W%*%alpha.hat)
#   
#   
#   offense_team_raw <-  exp.u[1:n_TeamsThisSeason]
#   defense_team_raw <- exp.u[(n_TeamsThisSeason+1):(2*n_TeamsThisSeason)]
#   Game_Coef <- exp.u[(2*n_TeamsThisSeason+1):(2*n_TeamsThisSeason + ncol_z)]
#   
#   BPI_Off <- offense_team_raw - mean(offense_team_raw)
#   BPI_Def <- defense_team_raw - mean(defense_team_raw)
#   BPI_Tot <- BPI_Off + BPI_Def
#   
#   diag.var.u <- diag(var.u)
#   BPI_Off_var <- diag.var.u[1:n_TeamsThisSeason] /prior_var_scale
#   BPI_Def_var <- diag.var.u[(n_TeamsThisSeason+1):(2*n_TeamsThisSeason)]/prior_var_scale
#   Game_Coef_var <- diag.var.u[(2*n_TeamsThisSeason+1):(2*n_TeamsThisSeason + ncol_z)]
#   return(list(BPI_Off = BPI_Off, BPI_Def = BPI_Def, BPI_Tot = BPI_Tot,offense_team_raw = offense_team_raw, 
#               defense_team_raw = defense_team_raw, Game_Coef = Game_Coef,
#               BPI_Off_var = BPI_Off_var, BPI_Def_var = BPI_Def_var, Game_Coef_var = Game_Coef_var,
#               Coef_Var_Matrix = var.u))
# }


############ BPI Calculations given prior parameters and weight paramter for the priors
# bpi_preseason <- function(PreseasonOffense, PreseasonDefense, PriorWeightDenominator = 10){
#   
#   
#   ###################################################################################################
#   n_teams <- dim(PreseasonOffense)[1]
#   m_gamma <- c(3.41691, 16.35033, 2.50442, -2.75857, -7.31174,0.84895, -0.00017, 36.01116, 116.30406,
#                15.18773, -43.00823, -111.69943, -16.49531, 63.77014)
#   n_gameCoefficients <- length(m_gamma)
#   
#   
#   # ### Use Non-linear effects for Recruiting Rankings
#   maxRecruitingRtg <- 100
#   minRecruitingRtg <- 70
#   
#   Recruiting.basis <- ns(seq(minRecruitingRtg, maxRecruitingRtg, length = 1000), knots = c(94),
#                          Boundary.knots = c(minRecruitingRtg, maxRecruitingRtg))
#   
#   RecruitingPredictors.Off <- predict(Recruiting.basis, PreseasonOffense$AVG_ALL_RECRUITS_GRADE)
#   RecruitingPredictors.Def <- predict(Recruiting.basis, PreseasonDefense$AVG_ALL_RECRUITS_GRADE)
#   
#   PreseasonOffense <- mutate(PreseasonOffense,
#                              RecruitingOffense1 = RecruitingPredictors.Off[,1], RecruitingOffense2 = RecruitingPredictors.Off[,2])
#   PreseasonDefense <- mutate(PreseasonDefense,
#                              RecruitingDefense1 = RecruitingPredictors.Def[,1], RecruitingDefense2 = RecruitingPredictors.Def[,2])
#   
#   ### Prior Parameter Matrices
#   W.Off <- model.matrix( ~ PLAYER_OFF_RTG + COACH_AVG_OFFRTG + PRIOR_YEAR_WEIGHT +
#                            COMPOSITE_5_STARS + PLAYER_OFF_RTG:COACH_AVG_OFFRTG +
#                            PLAYER_OFF_RTG:PRIOR_YEAR_WEIGHT - 1, data = PreseasonOffense)
#   W.Def <- model.matrix(~ PRIOR_YEAR_WEIGHT + PLAYER_DEF_RTG + RETURNING_COACH +
#                           COACH_AVG_DEFRTG + RecruitingDefense1 + PLAYER_DEF_RTG:RETURNING_COACH +
#                           RETURNING_COACH:COACH_AVG_DEFRTG + RETURNING_COACH:RecruitingDefense1 -
#                           1, data = PreseasonDefense)
#   W <- matrixDiagonal(W.Off, W.Def, diag(n_gameCoefficients))
#   
#   #Prior mean model coefficients
#   alpha.hat.offense <- c(-0.16002880979,  -0.16546230999,-20.37518985312, 1.03609409717, 0.00310774723, 0.25505183281)
#   alpha.hat.defense <- c(3.1551651607,  -0.2092674845, 30.0494004964, 0.1822793432, 1.5760643696, -0.0530757048,  -0.2950450974, -0.8515914234)
#   alpha.hat <- c(alpha.hat.offense, alpha.hat.defense, m_gamma)
#   #Prior Precision Model Coefficients
#   off_prior_var_coef <- c(0.203972, -0.016558, 0.002164)
#   def_prior_var_coef <- c(1.276578, -0.835656,  0.165364, -0.008261, -0.003608, 0.008893, -0.002239)
#   
#   ##Precision and Variance Terms
#   #this predicts end of season posterior variance. Prior Variance is (1/30) this prediction (about 30 games for most teams)
#   prior_var_scale <- PriorWeightDenominator
#   W.Off.var.mat <- model.matrix( ~ RETURNING_COACH + COACH_SD_OFFRTG, data = PreseasonOffense)
#   W.Def.var.mat <- model.matrix( ~ RETURNING_COACH + PRIOR_YEAR_WEIGHT + 
#                                    PLAYER_DEF_RTG + AVG_ALL_RECRUITS_GRADE + RETURNING_COACH:PLAYER_DEF_RTG + 
#                                    PRIOR_YEAR_WEIGHT:PLAYER_DEF_RTG, data = PreseasonDefense ) 
#   lambda.O <- 1/(exp(W.Off.var.mat%*%off_prior_var_coef) * prior_var_scale)
#   lambda.D <- 1/(exp(W.Def.var.mat%*%def_prior_var_coef) * prior_var_scale)
#   
#   lambda.gamma <- 0.000528739
#   lambda.model <- 0.02031624
#   Lambda <- c(lambda.O, lambda.D, rep(lambda.gamma, n_gameCoefficients))* 
#     diag(2*n_teams + n_gameCoefficients)
#   
#   
#   var.u <- inv(Lambda)
#   exp.u <- W%*%alpha.hat
#   
#   
#   offense_team_raw <-  exp.u[1:n_TeamsThisSeason]
#   defense_team_raw <- exp.u[(n_TeamsThisSeason+1):(2*n_TeamsThisSeason)]
#   Game_Coef <- exp.u[(2*n_TeamsThisSeason)+1:(2*n_TeamsThisSeason + length(m_gamma))]
#   
#   BPI_Off <- offense_team_raw - mean(offense_team_raw)
#   BPI_Def <- defense_team_raw - mean(defense_team_raw)
#   BPI_Tot <- BPI_Off + BPI_Def
#   
#   diag.var.u <- diag(var.u)
#   BPI_Off_var <- diag.var.u[1:n_TeamsThisSeason]/prior_var_scale
#   BPI_Def_var <- diag.var.u[(n_TeamsThisSeason+1):(2*n_TeamsThisSeason)]/prior_var_scale
#   Game_Coef_var <- diag.var.u[(2*n_TeamsThisSeason+1):(2*n_TeamsThisSeason + length(m_gamma))]
#   return(list(BPI_Off = BPI_Off, BPI_Def = BPI_Def, BPI_Tot = BPI_Tot,offense_team_raw = offense_team_raw, 
#               defense_team_raw = defense_team_raw, Game_Coef = Game_Coef,
#               BPI_Off_var = BPI_Off_var, BPI_Def_var = BPI_Def_var, Game_Coef_var = Game_Coef_var,
#               Coef_Var_Matrix = var.u ))
# }
# 


MCBBGameAdjWinPct <-  function(tm_winpct, exp_winpct)
{
  #Takes team's win pct and expected win pct (usually from reference team's perspective)
  #provides adjusted win percentage
  #Last Updated: January 25, 2016
  adj_winpct <- (tm_winpct - exp_winpct)/2 + 0.5
  
  return(adj_winpct)
}



MCBBSeasonProbPercentile <- function(prob_stat_or_better, statistic = "WINPCT", perspective = "AVGDITM", 
                                     distribution_basis = "SELECTION SUNDAY")
{
  #Takes probability of team's season W-L or better, perspective, and basis for distribution,
  #provides estimated percentile based on historical DI distribution of those probabilities
  #Season reference distributions built by Alok; weekly reference Beta distributions don't exist yet
  #Last Updated: February 3, 2016
  
  #Start with default of these parameters being NAs, they get filled in if appropriate parameters are passed in below
  REF_BETADIST_ALPHA <- NA
  REF_BETADIST_BETA <- NA     
  
  if(toupper(distribution_basis) %in% c("SELECTIONSUNDAY", "SELECTION SUNDAY"))
  {     
    if(toupper(perspective) %in% c("AVG", "AVGDI", "AVGDITM"))
    {    
      if(toupper(statistic) %in% c("WINPCT", "WL"))
      {
        REF_BETADIST_ALPHA <- 0.1753849
        REF_BETADIST_BETA <- 0.1542144
      } else
      {
        REF_BETADIST_ALPHA <- NA
        REF_BETADIST_BETA <- NA
      }
    } else if (toupper(perspective) %in% c("TOP", "AVGTOP", "AVGTOPTM"))
    {
      if(toupper(statistic) %in% c("WINPCT", "WL"))
      {
        REF_BETADIST_ALPHA <- 0.3961548
        REF_BETADIST_BETA <- 0.054473
      } else
      {
        REF_BETADIST_ALPHA <- NA
        REF_BETADIST_BETA <- NA
      } 
    }
    
    prob_pct <- pbeta(prob_stat_or_better, REF_BETADIST_ALPHA, REF_BETADIST_BETA, lower.tail = FALSE)
    
  } else if(toupper(distribution_basis) %in% c("WEEKLY"))     
  {  #Functions to be created if weekly scaling becomes necessary, NA for now
    prob_pct <- NA
  } 
  
  return(prob_pct)
}

#### This is the Non-linear spline function for Difference in Altitude from Home
#between teams. 
#Updated September 6, 2016
AltitudeTransformation <- function(x,AltitudeKnot = 3500){
  return(ifelse(is.na(x),0,ifelse(abs(x) > AltitudeKnot, sign(x)*(abs(x)-AltitudeKnot),0)))
}


TEAM_PREGAME_WIN_PROB <- function(TEAM_BPI, OPP_BPI, TEAM_VAR_OFF, TEAM_VAR_DEF, OPP_VAR_OFF, OPP_VAR_DEF,
                                  TEAM_DIFF_DIST_FROM_HOME,
                                  TEAM_DIFF_DAYS_REST, TEAM_DIFF_ALTITUDE_FROM_HOME,
                                  TEAM_SITE,
                                  HOMECOURT_ADJUSTMENT = 1){
  #TEAM_BPI and #OPP_BPI are the TOTAL BPI for each team
  TEAM_DIFF_BPI <- TEAM_BPI - OPP_BPI
  
  ###How Much Do we Know about each team? TEAM_VAR_DIFF is the difference in total team variances
  TEAM_VAR_DIFF = (TEAM_VAR_OFF + TEAM_VAR_DEF) - (OPP_VAR_OFF + OPP_VAR_DEF)
  
  #TEAM_DIFF_DIST_FROM_HOME is the difference between how far the game site is
  ### for TEAM and how far it is for OPP i.e. (miles from venue to TEAM_HOME) - (Miles from venue to OPP_HOME)
  maxDistFromHome <- 2600 #Distance that we have a lot of data on since 2006 (2600 miles - About Honolulu to LA or LA to Maine)
  TEAM_DIFF_DIST_FROM_HOME <- coalesce(TEAM_DIFF_DIST_FROM_HOME,0) #if NA replace with 0
  Diff.DistFromHome.basis <- ns(TEAM_DIFF_DIST_FROM_HOME, knots = c(-1500,1500), 
                                Boundary.knots = c(-maxDistFromHome,maxDistFromHome))
  number_DistFromHome_columns <- ncol(Diff.DistFromHome.basis)
  
  ##TEAM_DIFF_DAYS_REST is how many more days rest TEAM has from OPP (certain maximum apply)  
  maxDiffDaysRest <- 12
  #IF Difference Days Rest is missing give it 0 (i.e. no Advantage)
  TEAM_DIFF_DAYS_REST <- coalesce(TEAM_DIFF_DAYS_REST, 0)
  TEAM_DIFF_DAYS_REST <- if_else(abs(TEAM_DIFF_DAYS_REST)> maxDiffDaysRest,
                                 sign(TEAM_DIFF_DAYS_REST)*maxDiffDaysRest, TEAM_DIFF_DAYS_REST)
  
  Diff.DaysRest.basis <- ns(TEAM_DIFF_DAYS_REST,knots = c(-5,5), Boundary.knots = c(-maxDiffDaysRest,maxDiffDaysRest))
  number_DaysRest_columns <- ncol(Diff.DaysRest.basis)
  
  ## TEAM_DIFF_ALTITUDE_FROM_HOME is (Venue Alt(ft.) - TEAM home Altitude) - (Venue Alt(ft.) - OPP home Altitude)
  maxAltitudeDiff <- 6000 #Maximum altitude allowed for a difference between home venues.
  Altitude.Basis <- AltitudeTransformation(TEAM_DIFF_ALTITUDE_FROM_HOME, AltitudeKnot = 3500)
  number_Altitude_columns <- 1
  
  ## TEAM_SITE is where game is being played ("Home", "Road", "Neutral") 
  TEAM_DIFF_SITE <- if_else(TEAM_SITE == "Home",1,if_else(TEAM_SITE == "Road",-1,0))*HOMECOURT_ADJUSTMENT
  
  ### Create Model Matrix
  
  # Game Variables
  Z.DaysRest <- predict(Diff.DaysRest.basis,TEAM_DIFF_DAYS_REST)
  Z.DistFromHome <- predict(Diff.DistFromHome.basis,TEAM_DIFF_DIST_FROM_HOME)
  Z <-         cbind(Z.DaysRest, Z.DaysRest*TEAM_DIFF_SITE, Altitude.Basis,
                     Z.DistFromHome, Z.DistFromHome*TEAM_DIFF_SITE,
                     TEAM_DIFF_SITE)  %>% as.matrix()
  X <- cbind(TEAM_DIFF_BPI, TEAM_VAR_DIFF, Z) #Full Model Matrix
  # Coef<- c( 0.1977279749588, -0.3174984910594,
  #          -0.1114994392003, -0.5133081535031,
  #          -0.3119467246617, -0.3308030922867,
  #          -0.5140321480908,  0.2408280115407,
  #          -0.0000774345085, -0.4046127616258,
  #           0.5243419101255, -0.3838686623742,
  #          -0.2213646905194, -0.6198340573284,
  #          -0.0989565554007,  1.2873003617502) #deprecated
    
  Coef<- c( 0.19681190011711, -0.20769136302880,
            -0.11099718621118, -0.48584295174359,
            -0.26615121513273, -0.30145197656974,
            -0.45240023555650,  0.22835864133998,
            -0.00001887365716, -0.37886355356881,
            0.65962744471587, -0.14449790987618,
            -0.34641080269565, -0.53392736136906,
            0.17188888833772,  1.31403756020142)
  probabilities <- as.numeric(1 / (1 + exp(-X%*%Coef)))

  return(probabilities)
  
}

### Takes a team win percent and calculates a net efficiency per possession -- inherently it includes all those other variables that the WINPCT prediction does
TEAM_PREGAME_NET_EFF_PRED <- function(TEAM_WINPCT, RETURN_VARIANCE = FALSE){
  logodds_winpct <- log(TEAM_WINPCT/(1-TEAM_WINPCT))
  
  Coef<- 0.0917711473
  model.sigma2 <- 0.02362 ### the estimated variance for the model
  
  net_eff_per_poss <- logodds_winpct*Coef
  
  if(RETURN_VARIANCE == T){
    prediction_variance <- model.sigma2 #don't have to define identity matrix to make the proper prediction interval, just add 1 to all of 'hat' matrix
    return(prediction_variance)
  }else{
    return(net_eff_per_poss)  
  }
  
}



#########Function that will Round 0.5 up instead of to the nearest even number (R rounds to nearest even number)
### Last Updated 11/03/2016 by Paul Sabin
cround = function(x,n = 0){
  vorz = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*vorz
}


####Adjusting Final Points based on Play by Play Data
PTS_ADJ_BY_PLAYBYPLAY <- function(TEAM_SITE, AVG_SCORE_SPREAD, GAME_POSS, PTS){
  maxAdjustment <- 10
  Intercept <- 1.0179486
  Neutral <- -0.0067028
  Road <- -0.0172478
  Beta <- 0.7469101
  y <- GAME_POSS*(Intercept + Beta*(AVG_SCORE_SPREAD/GAME_POSS) + (TEAM_SITE == "Neutral")*Neutral + (TEAM_SITE == "Road")*Road )
  y[is.na(y)] <- PTS[is.na(y)]
  y[abs(y-PTS)>10] <- (sign(y-PTS)*10 + PTS)[abs(y-PTS)>10]
  
  return(y)
}


################
#Returns all possible matchups for conference tournament when given a list of team id's
## Last Updated Februrary 10th, 2017

#e.g, conference = "Big 12"
allPossibleMatchupsConfTourney <- function(conference, BPIOutput = BPIOutput, Venues = VenuesTable){
  
  
  if(!(conference %in% confTournamentFormatSaved)){stop("conference is not found in approved list (confTournamentFormatSaved")}
  
  teamConfTourney <- select(HomeVenues,HOME_VENUE_ID, SEASON_ID, TEAM_ID, HOME_ALTITUDE,HOME_LATITUDE, HOME_LONGITUDE) %>%
    filter(SEASON_ID == CurrentSeason$SEASON_ID) %>% 
    left_join(conferenceTourneyTeams[[tempConf]], ., by = c("TEAM_ID")) %>% #addin home arena info for 
    rename(TEAM_HOME_VENUE_ID = HOME_VENUE_ID, TEAM_HOME_ALTITUDE = HOME_ALTITUDE, 
           TEAM_HOME_LAT = HOME_LATITUDE, TEAM_HOME_LON = HOME_LONGITUDE) %>%
    mutate(TOURNEY_VENUE_ID = conferenceTourney_VENUE_ID[[conference]]) %>%
    left_join(., select(Venues, VENUE_ID, LATITUDE, LONGITUDE, ALTITUDE), by = c("TOURNEY_VENUE_ID" = "VENUE_ID")) %>%
    rename(TOURNEY_LATITUDE = LATITUDE, TOURNEY_LONGITUDE = LONGITUDE, TOURNEY_ALTITUDE = ALTITUDE) %>%
    mutate(TEAM_DiffAltitude = TOURNEY_ALTITUDE -TEAM_HOME_ALTITUDE %>% coalesce(.,0),
           TEAM_DistFromHome =  MilesBetweenTwoLatLongHF(TEAM_HOME_LAT, TEAM_HOME_LON, TOURNEY_LATITUDE, TOURNEY_LONGITUDE) %>%
             ifelse(. > maxDistFromHome, maxDistFromHome, .) %>% coalesce(.,0) 
    )  %>%
    left_join(.,select(BPIOutput, TEAM_ID, BPI, VARIATION_OFF, VARIATION_DEF), by = "TEAM_ID") %>%
    select(TEAM_ID, TEAM_DiffAltitude, TEAM_DistFromHome, BPI, VARIATION_DEF, VARIATION_OFF)
  
  confTourney_allPossibleMatchups <-expand.grid(teamConfTourney$TEAM_ID, teamConfTourney$TEAM_ID) %>% tbl_df() %>%
    rename(TEAM_ID = Var1, OPP_ID = Var2) %>%
    filter(TEAM_ID != OPP_ID) %>%
    left_join(., teamConfTourney, by = c("OPP_ID" = "TEAM_ID")) %>%
    rename(OPP_DiffAltitude = TEAM_DiffAltitude, OPP_DistFromHome = TEAM_DistFromHome,
           TEAM_BPI = BPI, TEAM_VARIATION_OFF = VARIATION_OFF, TEAM_VARIATION_DEF = VARIATION_DEF) %>%
    left_join(., teamConfTourney, by= c("TEAM_ID")) %>%
    rename(OPP_BPI = TEAM_BPI, OPP_VARIATION_OFF = TEAM_VARIATION_OFF, OPP_VARIATION_DEF =TEAM_VARIATION_DEF,
           TEAM_BPI = BPI, TEAM_VARIATION_OFF = VARIATION_OFF, TEAM_VARIATION_DEF = VARIATION_DEF) %>%
    mutate(Diff.DistFromHome = TEAM_DistFromHome - OPP_DistFromHome, 
           Diff.AltitudeFromHome = (TEAM_DiffAltitude - OPP_DiffAltitude),
           Diff.DistFromHome = coalesce(Diff.DistFromHome, 0),
           Diff.DaysRest = 0, 
           TEAM_WIN_PROB_raw = TEAM_PREGAME_WIN_PROB(TEAM_BPI = TEAM_BPI, OPP_BPI, 
                                                     TEAM_VAR_OFF = TEAM_VARIATION_OFF, TEAM_VAR_DEF = TEAM_VARIATION_DEF, 
                                                     OPP_VARIATION_OFF, OPP_VARIATION_DEF,
                                                     TEAM_DIFF_DIST_FROM_HOME = Diff.DistFromHome,
                                                     TEAM_DIFF_DAYS_REST = Diff.DaysRest , 
                                                     TEAM_DIFF_ALTITUDE_FROM_HOME = Diff.AltitudeFromHome,
                                                     TEAM_SITE = CONF_TOURNEY_SITE[[conference]]),
           tempEVENT_ID = paste(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID), sep = "-"),
           AVGTOPTM_EXP_WINPCT = TEAM_PREGAME_WIN_PROB(TEAM_BPI = AvgTop25TmBPI, OPP_BPI, 
                                                       TEAM_VAR_OFF = 1, TEAM_VAR_DEF = 1, 
                                                       OPP_VARIATION_OFF , OPP_VARIATION_DEF,
                                                       TEAM_DIFF_DIST_FROM_HOME = Diff.DistFromHome,
                                                       TEAM_DIFF_DAYS_REST = Diff.DaysRest , 
                                                       TEAM_DIFF_ALTITUDE_FROM_HOME = Diff.AltitudeFromHome,
                                                       TEAM_SITE = CONF_TOURNEY_SITE[[conference]])) 
  
  ### correct for computational errors in game predictions to make them match between the same team.
  ### Adjust for Computation Error and force Win Probs to add to 100
  WinPROBAdj <- group_by(confTourney_allPossibleMatchups, tempEVENT_ID) %>% summarize(WinPROB_Adjustment = sum(TEAM_WIN_PROB_raw)) %>% ungroup()
  
  confTourney_allPossibleMatchups <- left_join(confTourney_allPossibleMatchups, WinPROBAdj, by = "tempEVENT_ID")   %>%
    mutate(TEAM_PRED_WIN_PCT = TEAM_WIN_PROB_raw/WinPROB_Adjustment,
           OPP_PRED_WINPCT = 1 - TEAM_PRED_WIN_PCT) %>%
    select(TEAM_ID, OPP_ID, TEAM_PRED_WIN_PCT, OPP_PRED_WINPCT, AVGTOPTM_EXP_WINPCT)

  
  return(confTourney_allPossibleMatchups)
}

allPossibleMatchupsConfTourneyWithSeeds <- function(teamSeeds, confTourney_allPossibleMatchups){
        ### Seeds - This step is only done in the simulation itself
        ConfTourney_allPossibleWithSeeds <- left_join(confTourney_allPossibleMatchups, teamSeeds, by = c("OPP_ID" = "TEAM_ID")) %>%
          rename(OPP_SEED = SEED) %>%
          left_join(.,teamSeeds, by = c("TEAM_ID")) %>%
          rename(TEAM_SEED = SEED)
        return(ConfTourney_allPossibleWithSeeds)
}


# bracket = currentBracket
# allPossibleMatchups_withSEEDS = ConfTourney_allPossibleWithSeeds

######################################## Simulate a Conference Tournament based on all possible matchups with seeds and simple bracket structure (see: "Conference Tournament Formats.xlsx")
#last updated Feburary 10th, 2017 by Paul Sbain
confTournamentSimulator <- function(bracket, allPossibleMatchups_withSEEDS){
  simGames <- NULL
  if(nrow(filter(bracket, is.na(R064), R128>-1)) ==0){
    roundOf128 <- bracket 
  }else{
    roundOf128Games <- filter(bracket, is.na(R064), R128>-1) %>%
                        mutate(temp_game_id = floor((as.numeric(rownames(.) )+ 1)/2) %>% as.character() ) %>%
                        group_by(temp_game_id) %>%
                        mutate(TEAM_SEED_min = min(R128),
                               TEAM_SEED_max = max(R128),
                               TEAM_SEED = R128,
                               OPP_SEED = ifelse(TEAM_SEED_min == R128, TEAM_SEED_max, TEAM_SEED_min)) %>%
                        ungroup() %>%
                        select(-starts_with("TEAM_SEED_m") ) %>%
                        left_join(., allPossibleMatchups_withSEEDS, by = c("TEAM_SEED", "OPP_SEED")) %>%
                        arrange(temp_game_id, TEAM_ID) %>%
                        mutate(SimRound = ifelse(TEAM_ID < OPP_ID, NA_integer_, ifelse(runif(1) <=TEAM_PRED_WIN_PCT,1,0) ),
                               SimRound = ifelse(is.na(SimRound), 1- lead(SimRound),SimRound)) 
    roundOf128 <- select(roundOf128Games, R128, SimRound) %>%
                  left_join(bracket, ., by =c("SEED" = "R128") ) %>%
                  mutate(R064 = ifelse(!is.na(SimRound) & SimRound == 1, R128, 
                                       ifelse(!is.na(SimRound) & SimRound == 0,-1,
                                              ifelse(R128 == -1, -1,R064) )   )) %>%
                  select(-SimRound)
    simGames <- select(roundOf128Games, TEAM_ID, OPP_ID, TEAM_PRED_WIN_PCT, AVGTOPTM_EXP_WINPCT, SimRound) %>% rbind(simGames, .)
  }
  
  if(nrow(filter(roundOf128, is.na(R032), R064>-1)) ==0){
    roundOf64 <- roundOf128 
  }else{
    roundOf64Games <- filter(roundOf128, is.na(R032), R064>-1) %>%
                      mutate(temp_game_id = floor((as.numeric(rownames(.) )+ 1)/2) %>% as.character() ) %>%
                      group_by(temp_game_id) %>%
                      mutate(TEAM_SEED_min = min(R064),
                             TEAM_SEED_max = max(R064),
                             TEAM_SEED = R064,
                             OPP_SEED = ifelse(TEAM_SEED_min == R064, TEAM_SEED_max, TEAM_SEED_min)) %>%
                      ungroup() %>%
                      select(-starts_with("TEAM_SEED_m") ) %>%
                      left_join(., allPossibleMatchups_withSEEDS, by = c("TEAM_SEED", "OPP_SEED")) %>%
                      arrange(temp_game_id, TEAM_ID) %>%
                      mutate(SimRound = ifelse(TEAM_ID < OPP_ID, NA_integer_, ifelse(runif(1) <=TEAM_PRED_WIN_PCT,1,0) ),
                             SimRound = ifelse(is.na(SimRound), 1- lead(SimRound),SimRound)) 
    roundOf64 <- select(roundOf64Games, R064, SimRound) %>%
                    left_join(roundOf128, ., by =c("SEED" = "R064") ) %>%
                    mutate(R032 = ifelse(!is.na(SimRound) & SimRound == 1, R064, 
                                         ifelse(!is.na(SimRound) & SimRound == 0,-1,
                                                ifelse(R064 == -1, -1,R032) )   )) %>%
                    select(-SimRound)
    simGames <- select(roundOf64Games, TEAM_ID, OPP_ID, TEAM_PRED_WIN_PCT, AVGTOPTM_EXP_WINPCT, SimRound) %>% rbind(simGames, .)
  }
  
  if(nrow(filter(roundOf64, is.na(R016), R032>-1)) ==0){
    roundOf32 <- roundOf64 
  }else{
    roundOf32Games <- filter(roundOf64, is.na(R016), R032>-1) %>%
                      mutate(temp_game_id = floor((as.numeric(rownames(.) )+ 1)/2) %>% as.character() ) %>%
                      group_by(temp_game_id) %>%
                      mutate(TEAM_SEED_min = min(R032),
                             TEAM_SEED_max = max(R032),
                             TEAM_SEED = R032,
                             OPP_SEED = ifelse(TEAM_SEED_min == R032, TEAM_SEED_max, TEAM_SEED_min)) %>%
                      ungroup() %>%
                      select(-starts_with("TEAM_SEED_m") ) %>%
                      left_join(., allPossibleMatchups_withSEEDS, by = c("TEAM_SEED", "OPP_SEED")) %>%
                      arrange(temp_game_id, TEAM_ID) %>%
                      mutate(SimRound = ifelse(TEAM_ID < OPP_ID, NA_integer_, ifelse(runif(1) <=TEAM_PRED_WIN_PCT,1,0) ),
                             SimRound = ifelse(is.na(SimRound), 1- lead(SimRound),SimRound)) 
    roundOf32 <- select(roundOf32Games, R032, SimRound) %>%
                  left_join(roundOf64, ., by =c("SEED" = "R032") ) %>%
                  mutate(R016 = ifelse(!is.na(SimRound) & SimRound == 1, R032, 
                                       ifelse(!is.na(SimRound) & SimRound == 0,-1,
                                              ifelse(R032 == -1, -1,R016) )   )) %>%
                  select(-SimRound)
    simGames <- select(roundOf32Games, TEAM_ID, OPP_ID, TEAM_PRED_WIN_PCT, AVGTOPTM_EXP_WINPCT, SimRound) %>% rbind(simGames, .)
  }
  if(nrow(filter(roundOf32, is.na(R008), R016>-1)) ==0){
    roundOf16 <- roundOf32 
  }else{
    roundOf16Games <- filter(roundOf32, is.na(R008), R016>-1) %>%
                      mutate(temp_game_id = floor((as.numeric(rownames(.) )+ 1)/2) %>% as.character() ) %>%
                      group_by(temp_game_id) %>%
                      mutate(TEAM_SEED_min = min(R016),
                             TEAM_SEED_max = max(R016),
                             TEAM_SEED = R016,
                             OPP_SEED = ifelse(TEAM_SEED_min == R016, TEAM_SEED_max, TEAM_SEED_min)) %>%
                      ungroup() %>%
                      select(-starts_with("TEAM_SEED_m") ) %>%
                      left_join(., allPossibleMatchups_withSEEDS, by = c("TEAM_SEED", "OPP_SEED")) %>%
                      arrange(temp_game_id, TEAM_ID) %>%
                      mutate(SimRound = ifelse(TEAM_ID < OPP_ID, NA_integer_, ifelse(runif(1) <=TEAM_PRED_WIN_PCT,1,0) ),
                             SimRound = ifelse(is.na(SimRound), 1- lead(SimRound),SimRound)) 
      roundOf16 <- select(roundOf16Games, R016, SimRound) %>%
                  left_join(roundOf32, ., by =c("SEED" = "R016") ) %>%
                  mutate(R008 = ifelse(!is.na(SimRound) & SimRound == 1, R016, 
                                       ifelse(!is.na(SimRound) & SimRound == 0,-1,
                                              ifelse(R016 == -1, -1,R008) )   )) %>%
                  select(-SimRound)
      
      simGames <- select(roundOf16Games, TEAM_ID, OPP_ID, TEAM_PRED_WIN_PCT, AVGTOPTM_EXP_WINPCT, SimRound) %>% rbind(simGames, .)
      
  }
  
  if(nrow(filter(roundOf16, is.na(R004), R008>-1)) ==0){
    roundOf08 <- roundOf16 
  }else{
    roundOf08Games <- filter(roundOf16, is.na(R004), R008>-1) %>%
                      mutate(temp_game_id = floor((as.numeric(rownames(.) )+ 1)/2) %>% as.character() ) %>%
                      group_by(temp_game_id) %>%
                      mutate(TEAM_SEED_min = min(R008),
                             TEAM_SEED_max = max(R008),
                             TEAM_SEED = R008,
                             OPP_SEED = ifelse(TEAM_SEED_min == R008, TEAM_SEED_max, TEAM_SEED_min)) %>%
                      ungroup() %>%
                      select(-starts_with("TEAM_SEED_m") ) %>%
                      left_join(., allPossibleMatchups_withSEEDS, by = c("TEAM_SEED", "OPP_SEED")) %>%
                      arrange(temp_game_id, TEAM_ID) %>%
                      mutate(SimRound = ifelse(TEAM_ID < OPP_ID, NA_integer_, ifelse(runif(1) <=TEAM_PRED_WIN_PCT,1,0) ),
                             SimRound = ifelse(is.na(SimRound), 1- lead(SimRound),SimRound))
      roundOf08 <- select(roundOf08Games, R008, SimRound) %>%
                    left_join(roundOf16, ., by =c("SEED" = "R008") ) %>%
                    mutate(R004 = ifelse(!is.na(SimRound) & SimRound == 1, R008, 
                                         ifelse(!is.na(SimRound) & SimRound == 0,-1,
                                                ifelse(R008 == -1, -1,R004) )   )) %>%
                    select(-SimRound)
      simGames <- select(roundOf08Games, TEAM_ID, OPP_ID, TEAM_PRED_WIN_PCT, AVGTOPTM_EXP_WINPCT, SimRound) %>% rbind(simGames, .)
      
  }
  if(nrow(filter(roundOf08, is.na(R002), R004>-1)) ==0){
    roundOf04 <- roundOf08
  }else{
    roundOf04Games <- filter(roundOf08, is.na(R002), R004>-1) %>%
                      mutate(temp_game_id = floor((as.numeric(rownames(.) )+ 1)/2) %>% as.character() ) %>%
                      group_by(temp_game_id) %>%
                      mutate(TEAM_SEED_min = min(R004),
                             TEAM_SEED_max = max(R004),
                             TEAM_SEED = R004,
                             OPP_SEED = ifelse(TEAM_SEED_min == R004, TEAM_SEED_max, TEAM_SEED_min)) %>%
                      ungroup() %>%
                      select(-starts_with("TEAM_SEED_m") ) %>%
                      left_join(., allPossibleMatchups_withSEEDS, by = c("TEAM_SEED", "OPP_SEED")) %>%
                      arrange(temp_game_id, TEAM_ID) %>%
                      mutate(SimRound = ifelse(TEAM_ID < OPP_ID, NA_integer_, ifelse(runif(1) <=TEAM_PRED_WIN_PCT,1,0) ),
                             SimRound = ifelse(is.na(SimRound), 1- lead(SimRound),SimRound))
    roundOf04 <-  select(roundOf04Games, R004, SimRound) %>%
                  left_join(roundOf08, ., by =c("SEED" = "R004") ) %>%
                  mutate(R002 = ifelse(!is.na(SimRound) & SimRound == 1, R004, 
                                       ifelse(!is.na(SimRound) & SimRound == 0,-1,
                                              ifelse(R004 == -1, -1,R002) )   )) %>%
                  select(-SimRound)
    simGames <- select(roundOf04Games, TEAM_ID, OPP_ID, TEAM_PRED_WIN_PCT, AVGTOPTM_EXP_WINPCT, SimRound) %>% rbind(simGames, .)
    
  }
  
  if(nrow(filter(roundOf04, is.na(R001), R002>-1)) ==0){
    roundOf02 <- roundOf04
  }else{
    roundOf02Games <- filter(roundOf04, R002>-1) %>%
                      mutate(TEAM_SEED_min = min(R002),
                             TEAM_SEED_max = max(R002),
                             TEAM_SEED = R002,
                             OPP_SEED = ifelse(TEAM_SEED_min == R002, TEAM_SEED_max, TEAM_SEED_min)) %>%
                      ungroup() %>%
                      select(-starts_with("TEAM_SEED_m")) %>%
                      left_join(., allPossibleMatchups_withSEEDS, by = c("TEAM_SEED", "OPP_SEED")) %>%
                      arrange(TEAM_ID) %>%
                      mutate(SimRound = ifelse(TEAM_ID < OPP_ID, NA_integer_, ifelse(runif(1) <=TEAM_PRED_WIN_PCT,1,0) ),
                             SimRound = ifelse(is.na(SimRound), 1- lead(SimRound),SimRound)) 
      roundOf02 <- select(roundOf02Games, R002, SimRound) %>%
                    left_join(roundOf04, ., by =c("SEED" = "R002") ) %>%
                    mutate(R001 = ifelse(!is.na(SimRound) & SimRound == 1, R002, 
                                         ifelse(!is.na(SimRound) & SimRound == 0,-1,
                                                ifelse(R002 == -1, -1,R001) )   )) %>%
                    select(-SimRound)
      simGames <- select(roundOf02Games, TEAM_ID, OPP_ID, TEAM_PRED_WIN_PCT, AVGTOPTM_EXP_WINPCT, SimRound) %>% rbind(simGames,  .)
      
  }
  #Clean up Simulated Results
  simGames <- rename(simGames, ADJ_TEAM_ID = TEAM_ID, ADJ_OPP_ID = OPP_ID, simWin = SimRound, TEAM_WINPCT_CURRENTBPI = TEAM_PRED_WIN_PCT)
  
  ####Clean up simulation piece
  roundOf02[roundOf02 == -1] <- 0
  simConfTourney <- mutate_at(roundOf02, vars(starts_with("R")), funs(ifelse(.>0,1,0))) %>%
                    left_join(., unique(select(allPossibleMatchups_withSEEDS, TEAM_SEED)), by = c("SEED" = "TEAM_SEED"))
    
  return(list(simGames = simGames, simConfTourney = simConfTourney))
}


############ Function that calculates current RPI based on team game logs for each Sim
### Last updated June 24nd, 2017
RPI <- function(TEAM, OPP, TEAM_SITE, TEAM_RESULT, TEAM_DIV, OPP_DIV, Sim){
  # TEAM <- FinishedGamesThisSeason$TEAM
  # OPP <- FinishedGamesThisSeason$OPP
  # TEAM_SITE<- FinishedGamesThisSeason$TEAM_SITE
  # TEAM_RESULT <- FinishedGamesThisSeason$TEAM_RESULT
  # TEAM_DIV <- FinishedGamesThisSeason$TEAM_DIV
  # OPP_DIV <- FinishedGamesThisSeason$OPP_DIV
  # Sim <- 1
  
  divIOnly <- cbind(TEAM, OPP, TEAM_SITE, TEAM_RESULT, TEAM_DIV, OPP_DIV, Sim) %>% tbl_df() %>%
    filter(., TEAM_DIV== "Division I", OPP_DIV == "Division I") %>%
    select(Sim, TEAM, OPP, TEAM_SITE, TEAM_RESULT )
  
  WP <- mutate(divIOnly, GM_PTS = ifelse(TEAM_SITE == "Neutral",1.0, 
                                         ifelse((TEAM_SITE == "Home" & TEAM_RESULT == "W") | (TEAM_SITE == "Road" & TEAM_RESULT == "L"),0.6,1.4)),
               WIN_PTS = ifelse(TEAM_RESULT == "W", GM_PTS, 0)) %>%
    group_by(Sim, TEAM) %>%
    summarize(WP = sum(WIN_PTS) / sum(GM_PTS)) %>%
    arrange(TEAM) %>% ungroup()
  
  
  temp <- mutate(divIOnly, WIN_PTS = ifelse(TEAM_RESULT == "W", 1, 0)) %>%
    group_by(Sim, TEAM,OPP) %>%
    summarize(GAMES = n() , Wins = sum(WIN_PTS)) %>%
    ungroup() %>%
    arrange(TEAM)
  
  
  recordExceptAgainstOPP <- group_by(temp, Sim, TEAM) %>%
    mutate(gamesExcept = sum(GAMES) - GAMES,
           winsExcept = sum(Wins) - Wins) %>% ungroup() %>%
    select(-GAMES, -Wins)
  
  OWP <- left_join(temp, recordExceptAgainstOPP, by = c("TEAM" = "OPP", "OPP" = "TEAM", "Sim")) %>%
    rename(oppGamesExceptTeam = gamesExcept, oppWinsExceptTeam = winsExcept) %>%
    mutate(OWP_individual = oppWinsExceptTeam/oppGamesExceptTeam) %>%  
    group_by(Sim, TEAM) %>%
    summarize(OWP = weighted.mean(x = OWP_individual, w = GAMES)) %>% ungroup()
  
  OOWP <- left_join(temp, OWP, by = c("OPP" = "TEAM", "Sim") ) %>%
    rename(OPP_OWP = OWP) %>%
    group_by(Sim, TEAM) %>%
    summarize(OOWP = weighted.mean(x = OPP_OWP, w = GAMES)) %>% ungroup()
  
  left_join(WP, OWP, by = c("Sim", "TEAM")) %>% 
    left_join(., OOWP, by = c("Sim", "TEAM")) %>%
    mutate(RPI = 0.25*WP + 0.50*OWP + 0.25*OOWP) %>%
    group_by(Sim) %>% 
    mutate(RPI_RANK = rank(desc(RPI))) %>% ungroup() %>% 
    select(Sim, TEAM, RPI, RPI_RANK) %>%
    arrange(Sim, RPI_RANK) %>%
    return(.)
  
}
