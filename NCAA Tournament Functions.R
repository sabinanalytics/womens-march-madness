##########Functions Needed to Simulate out NCAA Tournament
MatchupRound <- function(R128_Slot1, R128_Slot2, name = T){
  Rounds <- select(TourneyStructure, R128_SLOT, R064_SLOT, R032_SLOT, R016_SLOT, R008_SLOT, R004_SLOT, R002_SLOT) %>% 
            arrange(R128_SLOT)
  
  if(length(R128_Slot1) != length(R128_Slot2)){stop("Input Vectors are not the same length")}
  n <- length(R128_Slot1)
  
  temp <- cbind(R128_Slot1, R128_Slot2) %>% tbl_df() %>%
          left_join(., Rounds, by =c("R128_Slot2" = "R128_SLOT")) %>%
          rename(R064_2 = R064_SLOT, R032_2 = R032_SLOT, R016_2 = R016_SLOT, R008_2 = R008_SLOT, R004_2 = R004_SLOT, R002_2 = R002_SLOT) %>%
          left_join(., Rounds, by =c("R128_Slot1" = "R128_SLOT")) %>%
          rename(R064_1 = R064_SLOT, R032_1 = R032_SLOT, R016_1 = R016_SLOT, R008_1 = R008_SLOT, R004_1 = R004_SLOT, R002_1 = R002_SLOT) %>%
          mutate(R064_match = if_else(R064_1 == R064_2, TRUE, FALSE),
                  R032_match = if_else(R032_1 == R032_2, TRUE, FALSE),
                  R016_match = if_else(R016_1 == R016_2, TRUE, FALSE),
                  R008_match = if_else(R008_1 == R008_2, TRUE, FALSE),
                  R004_match = if_else(R004_1 == R004_2, TRUE, FALSE),
                  R002_match = if_else(R002_1 == R002_2, TRUE, FALSE),
                  matchupRound = if_else(R064_match == TRUE, "R128_SLOT", 
                                          if_else(R032_match == TRUE, "R064_SLOT", 
                                                  if_else(R016_match == TRUE, "R032_SLOT",
                                                          if_else(R008_match == TRUE, "R016_SLOT",
                                                                  if_else(R004_match == TRUE, "R008_SLOT",
                                                                          if_else(R002_match == TRUE, "R004_SLOT", "R002_SLOT")))))),
                 ROUND_CODE = substr(matchupRound,1,4)) %>%
                 left_join(.,select(TourneyRounds_fromQuery, ROUND_CODE, ROUND_NAME), by = "ROUND_CODE")
  if(name == T){
    return(temp$matchupRound)
  }else{
    return(temp$ROUND_NAME)
  }
}
# 
# #Give it the Slots in the 128 Team Bracket and it will Give you either the Round Name the teams meet (NAME = T) or the R128 moniker
# MatchupRound <- function(R128_Slot1, R128_Slot2, name = T){
#   Rounds <- select(TourneyStructure, R128_SLOT, R064_SLOT, R032_SLOT, R016_SLOT, R008_SLOT, R004_SLOT, R002_SLOT)
#   
#   if(length(R128_Slot1) != length(R128_Slot2)){stop("Input Vectors are not the same length")}
#   n <- length(R128_Slot1)
#   
#   output.vec <- character(n)
#   
#   for(i in 1:n){
#     Path1 <- filter(Rounds, R128_SLOT == R128_Slot1[i])[1,]
#     Path2 <- filter(Rounds, R128_SLOT == R128_Slot2[i])[1,]
#     
#     #The Round they meet in is the last Round where the Path's are NOT equal
#     ROUND <- colnames(Rounds)[Path1 != Path2] %>% tail(1)
#     
#     if(name ==T){
#       output.vec[i] <- ROUND
#     }else{output.vec[i] <- TourneyRounds_fromQuery$ROUND_NAME[TourneyRounds_fromQuery$ROUND_CODE == unlist(strsplit(ROUND, split="_"))[1]] %>% as.character()}
#   }
#   return(output.vec)  
# }
# 



TeamTourneyPath <- function(TM_ID, TEAM_SLOT){
  #Find their route (Game Dates and Locations) to the Championshp
  Path <- select(TourneyStructure, R128_SLOT, R064_SLOT, R032_SLOT, R016_SLOT, R008_SLOT, R004_SLOT, R002_SLOT) %>% 
    filter(R128_SLOT == TEAM_SLOT) %>% unlist()
  TeamPath <- filter(Tourney_Dates_Locations,SEASON_ID %in% TourneySeasons$SEASON_ID, ROUND_SLOT %in% Path) %>% arrange(DATE)
  
  #Are they in the First 4?
  First4 <- filter(TourneySlots, (lead(R128_SLOT) - R128_SLOT) <2)
  if(TM_ID %in% First4$TEAM_ID){
    FirstGame <- filter(Tourney_Dates_Locations, SEASON_ID %in% TourneySeasons$SEASON_ID, SLOT == TEAM_SLOT, ROUND == "R128_SLOT")
    return(rbind(FirstGame,TeamPath))
  }else( return(TeamPath))
  
}

# 
# TM_ID <- allPossibleMatchups_inputs$ADJ_TEAM_ID
# TEAM_SLOT <- TEAM_R128_SLOT
# Round_Name <- Round_Name
TourneyDaysRest <- function(TM_ID, TEAM_SLOT, Round_Name){
  
  if(length(TM_ID) != length(TEAM_SLOT) | length(TM_ID) != length(Round_Name)){stop("Input Vectors are not the same length")}
  n <- length(TM_ID)

  
  DaysRest <- numeric(n)
  
  for( i in 1:n){
    #Generate Teams Tournament Path 
    Path <- TeamTourneyPath(TM_ID[i], TEAM_SLOT[i]) %>% arrange(DATE)
    TeamFirstGame = FirstGameTimes %>% filter(TEAM_ID == TM_ID[i])
    #See if it is the First game of the tournament for that team
    if(Path$ROUND[1] == Round_Name[i]){
      #We Need to Get the Last Games For Each Team (before the Tournament) if it is the first game of the tournament
      MostRecentGame <- select(TeamGameLogs_fromQuery, SEASON_ID, SCHEDULE_ID, GAME_DATE, NCAA_TOURNAMENT,STADIUM_ID, TEAM_ID, TEAM)  %>% 
        filter(TEAM_ID == TM_ID[i], SEASON_ID %in% TourneySeasons$SEASON_ID, GAME_DATE < TeamFirstGame$GAME_DATE) %>% group_by(TEAM_ID) %>%
        filter(GAME_DATE == max(GAME_DATE), row_number(TEAM_ID) == 1)
      
      temp.daysrest <- as.numeric( min(as.Date(Path$DATE) ) - as.Date(MostRecentGame$GAME_DATE) )
      DaysRest[i] <- ifelse(temp.daysrest > maxDaysRest, maxDaysRest, temp.daysrest)
    }else{
      temp.daysrest <- as.numeric(  as.Date(Path$DATE[Path$ROUND == Round_Name[i]]) - as.Date(Path$DATE[lead(Path$ROUND == Round_Name[i])][1])  )
      DaysRest[i] <- ifelse(temp.daysrest > maxDaysRest, maxDaysRest, temp.daysrest)
    }
  }
  return(DaysRest)
} 

################Distance From Home##########################################################
##########Join With the Venues Data###########
##This requires a Dataset, Calls TeamTourneyPath() in a loop then does a big join using dplyr
#outputs same dataset with the lat, lon, altitude of both teams home courts and game location
TourneyDistAltfromHome <- function(Data, CurrentSeasonID = CurrentSeason$SEASON_ID){
  
  ###Save the variables that are used 
  TM_ID <- Data$ADJ_TEAM_ID
  OPP_ID <- Data$ADJ_OPP_ID
  TEAM_SLOT <- Data$TEAM_R128_SLOT
  Round_Name <- Data$Round_Name
  
  if(length(TM_ID) != length(TEAM_SLOT) | length(TM_ID) != length(Round_Name)){stop("Input Vectors are not the same length")}
  n <- length(TM_ID)
  DistFromHome <- numeric(n)
  
  
  Path <- TeamTourneyPath(TM_ID[1], TEAM_SLOT[1]) %>% arrange(DATE) %>% filter(ROUND == Round_Name[1])
  if(n>1){
    for( i in 2:n){
      TM <- TM_ID[i]
      OPP <- OPP_ID[i]  
      #Generate Teams Tournament Path 
      Path <- TeamTourneyPath(TM_ID[i], TEAM_SLOT[i]) %>% arrange(DATE) %>% filter(ROUND == Round_Name[i]) %>% rbind(Path, . )
    }  
    
  }
  Matchups <- cbind(TM_ID,OPP_ID, CurrentSeasonID, Path)

  DistFromHome <- left_join(Matchups, select(VenuesTable, ALTITUDE, LATITUDE, LONGITUDE, VENUE_ID), by = c("VENUE_ID")) %>% 
    left_join(.,select(HomeVenues, TEAM_ID,SEASON_ID, HOME_ALTITUDE,HOME_LAT, HOME_LON), by = c("TM_ID" = "TEAM_ID", "CurrentSeasonID" = "SEASON_ID")) %>% 
    rename(TEAM_HOME_ALTITUDE = HOME_ALTITUDE, TEAM_HOME_LAT = HOME_LAT, TEAM_HOME_LON = HOME_LON) %>%
    left_join(.,select(HomeVenues, TEAM_ID, SEASON_ID, HOME_ALTITUDE,HOME_LAT, HOME_LON), by = c("OPP_ID" = "TEAM_ID", "CurrentSeasonID" = "SEASON_ID")) %>%
    rename(OPP_HOME_ALTITUDE = HOME_ALTITUDE, OPP_HOME_LAT = HOME_LAT, OPP_HOME_LON = HOME_LON) %>% 
    right_join(.,Data, by = c("TM_ID" = "ADJ_TEAM_ID", "OPP_ID" = "ADJ_OPP_ID", "SEASON_ID")) %>% rename(ADJ_TEAM_ID = TM_ID, ADJ_OPP_ID = OPP_ID) %>%
    tbl_df()
  
  
  return(DistFromHome)
}


###NCAA Tournament Simulator
################################################################################################################################################

### function that returns the next round probabilities for each team given all possible matchups file and the prob for all teams to reach current round
roundProb <- function(allPossible, probAdvance, currentRound = "Round of 64"){
  new.probAdvance <- allPossible %>% filter(currentRound == Round) %>% 
    left_join(., select(probAdvance, TEAM_ID, ADVANCE_PROB), by = c("OPP_ID" = "TEAM_ID")) %>%
    rename(OPP_ADVANCE_PROB = ADVANCE_PROB) %>% 
    left_join(., select(probAdvance, TEAM_ID, ADVANCE_PROB), by = c("TEAM_ID")) %>%
    rename(TM_ADVANCE_PROB = ADVANCE_PROB) %>% 
    group_by(TEAM_ID) %>% 
    summarize(ADVANCE_PROB = mean(TM_ADVANCE_PROB)*sum(TEAM_PRED_WINPCT*OPP_ADVANCE_PROB) ) %>% ungroup()
  return(new.probAdvance)
}



#Simulates the NCAA Tournament 1 time based on:
###### The slots of the tournament 1-128 Set up where 1 = top left part of bracket #1 seed
###### then down the left hand side to bottom left
###### then top right down to bottom right, last should be slot 127 (if not in First Four and 128 if in First Four)
###### Even no'd slots are only for 4 of the First Four Teams.

### Tournament Slots Table Needs the following columns:
#SEASON_ID, TEAM_ID, R128_SLOT, BRACKET_SECTION, R064_SEED, REGION_NAME
#R064_SLOT, R032_SLOT, R016_SLOT, R008_SLOT, R004_SLOT, R002_SLOT, R001_SLOT
#Written By: Paul Sabin
#Last Updated: 12/21/2016


NCAATournamentSimulator <- function(TourneySlots, AllPossibleMatchups){
  
  TourneySlots <- arrange(TourneySlots, Sim, R128_SLOT)
  
  #Determine the Slots of the "First 4" Participants. Even slots are by default First Four participants, plus the team listed immediately before them
  FirstFourTeamSlots <-  TourneySlots[((TourneySlots$R128_SLOT %% 2) ==0) + ((lead(TourneySlots$R128_SLOT,default = 129) %% 2) ==0)==1 ,]
  FirstFourMelted <-  reshape2::melt(data = FirstFourTeamSlots, id.vars = c("SEASON_ID", "R128_SLOT", "BRACKET_SECTION", "R064_SEED", "REGION_NAME", 
                                                                  "REGIONAL_VENUE_ID",  "TEAM_ID", "Status", "Sim", "EliminationRound"),
                           variable.name = "Round", value.name = "Round_Slot") %>% tbl_df() %>%
    mutate(Round = substr(Round,1,4)) %>% group_by(TEAM_ID, Sim) %>%
    filter(row_number(TEAM_ID) ==1) %>% ungroup() %>%
    mutate(Round = "R128", Round_Slot = paste(Round,R128_SLOT, sep='-'))
  
  TourneySlotsMelted <- reshape2::melt(data = TourneySlots, id.vars = c("SEASON_ID", "R128_SLOT", "BRACKET_SECTION", "R064_SEED", "REGION_NAME", 
                                                              "REGIONAL_VENUE_ID",  "TEAM_ID", "Status","Sim", "EliminationRound"),
                             variable.name = "Round", value.name = "Round_Slot") %>% tbl_df() %>%
    mutate(Round = substr(Round,1,4)) %>%
    rbind(.,FirstFourMelted) %>%
    arrange(R128_SLOT, TEAM_ID, desc(Round), desc(Round_Slot)) %>%
    left_join(.,TourneyGameResults, by = c("TEAM_ID" = "TM_ID", "Round"="ROUND_CODE", "SEASON_ID")) %>%
    group_by(SEASON_ID, TEAM_ID, Sim) %>%
    mutate(inThisRound = if_else(lag(TM_RESULT, default = "W") == "W", 1,0),
           inThisRoundSimulated = if_else(lag(TM_RESULT, default = "W") == "W", 1,0),
           simulateThisRound = if_else(Status == 'in' & is.na(inThisRound),'Y','N'),
           inThisRound = coalesce(inThisRound,0) ) %>%
    ungroup() %>% 
    tbl_df() %>%
    select(SEASON_ID, R128_SLOT, BRACKET_SECTION, R064_SEED, REGION_NAME, TEAM_ID, Status, Sim,
           EliminationRound, Round, Round_Slot, inThisRound, inThisRoundSimulated, simulateThisRound) %>%
    arrange(Sim, R128_SLOT)
  
  
  
  
  ############################################# Simulate First Four
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R064", Status == "in") %>%
    group_by(Round_Slot, Sim) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID)) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted -- Repeat for Sweet 16 Round
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin, Sim) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot", "Sim")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin, Sim ), by = c("TEAM_ID", "Round_Slot", "Sim")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID, Sim) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & simulateThisRound == "Y" & is.na(inThisRoundSimulated),0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin) %>%
    arrange(Sim, R128_SLOT)
  
  
  ###### Simulate Round of 64
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R032", simEliminated == "N") %>%
    group_by(Round_Slot, Sim) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID)) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin, Sim) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot", "Sim")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin, Sim ), by = c("TEAM_ID", "Round_Slot", "Sim")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID, Sim) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & simulateThisRound == "Y" & is.na(inThisRoundSimulated),0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin) %>%
    arrange(Sim, R128_SLOT)
  
  
  ###### Simulate Round of 32
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R016", simEliminated == "N") %>%
    group_by(Round_Slot, Sim) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID)) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin, Sim) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot", "Sim")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin, Sim ), by = c("TEAM_ID", "Round_Slot", "Sim")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID, Sim) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & simulateThisRound == "Y" & is.na(inThisRoundSimulated),0, 
                                         inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin) %>%
    arrange(Sim, R128_SLOT)
  
  ###### Simulate Sweet 16
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R008", simEliminated == "N") %>%
    group_by(Round_Slot, Sim) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID)) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin, Sim) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot", "Sim")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin, Sim ), by = c("TEAM_ID", "Round_Slot", "Sim")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID, Sim) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & simulateThisRound == "Y" & is.na(inThisRoundSimulated),0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin) %>%
    arrange(Sim, R128_SLOT)
  
  ###### Simulate Elite 8
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R004", simEliminated == "N") %>%
    group_by(Round_Slot, Sim) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID)) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin, Sim) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot", "Sim")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin, Sim ), by = c("TEAM_ID", "Round_Slot", "Sim")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID, Sim) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & simulateThisRound == "Y" & is.na(inThisRoundSimulated),0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin) %>%
    arrange(Sim, R128_SLOT)
  
  ###### Simulate Final Four
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R002", simEliminated == "N") %>%
    group_by(Round_Slot, Sim) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID)) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin, Sim) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot", "Sim")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin, Sim ), by = c("TEAM_ID", "Round_Slot", "Sim")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID, Sim) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & simulateThisRound == "Y" & is.na(inThisRoundSimulated),0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin) %>%
    arrange(Sim, R128_SLOT)
  
  
  ###### Simulate Championship Game
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R001", simEliminated == "N") %>%
    group_by(Round_Slot, Sim) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID)) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin, Sim) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot", "Sim")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin, Sim ), by = c("TEAM_ID", "Round_Slot", "Sim")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID, Sim) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & simulateThisRound == "Y" & is.na(inThisRoundSimulated),0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin) %>%
    arrange(Sim, R128_SLOT)
  
  
  
  
  
  SimTournamentResult <- reshape2::dcast(SEASON_ID + TEAM_ID + Sim # R128_SLOT + BRACKET_SECTION + R064_SEED + REGION_NAME + Status + EliminationRound + simEliminated
                               ~ Round, value.var = 'inThisRoundSimulated', data = TourneySlotsMelted) %>%
                         arrange(SEASON_ID, Sim, TEAM_ID)
  return(SimTournamentResult)
}

############This Tournament Simulator is for pre-bracket release and does not care about game location and days rest
## Updated 11/6/2019 by Paul Sabin

genericNCAATournamentSimulator <- function(TourneySlots, AllPossibleMatchups){
  
  #Determine the Slots of the "First 4" Participants. Even slots are by default First Four participants, plus the team listed immediately before them
  FirstFourTeamSlots <-  TourneySlots[((TourneySlots$R128_SLOT %% 2) ==0) + ((lead(TourneySlots$R128_SLOT,default = 129) %% 2) ==0)==1 ,]
  FirstFourMelted <-  FirstFourTeamSlots %>% 
    pivot_longer(cols = c("R064_SLOT", "R032_SLOT", "R016_SLOT", "R008_SLOT", "R004_SLOT", "R002_SLOT", "R001_SLOT"), names_to = "Round", values_to = "Round_Slot") %>%
    mutate(Round = substr(Round,1,4)) %>% group_by(TEAM_ID) %>%
    filter(row_number(TEAM_ID) ==1) %>% ungroup() %>%
    mutate(Round = "R128", Round_Slot = paste(Round,R128_SLOT, sep='-'))
  
  TourneySlotsMelted <-TourneySlots %>% 
    pivot_longer(cols = c("R064_SLOT", "R032_SLOT", "R016_SLOT", "R008_SLOT", "R004_SLOT", "R002_SLOT", "R001_SLOT"), names_to = "Round", values_to = "Round_Slot") %>%
    mutate(Round = substr(Round,1,4)) %>%
    rbind(.,FirstFourMelted) %>%
    arrange(R128_SLOT, TEAM_ID, desc(Round), desc(Round_Slot)) %>%
    mutate(TM_RESULT = NA_character_) %>%
    group_by(SEASON_ID, TEAM_ID) %>%
    mutate(inThisRound = if_else(lag(TM_RESULT, default = "W") == "W", 1,0),
           inThisRoundSimulated = if_else(lag(TM_RESULT, default = "W") == "W", 1,0),
           simulateThisRound = 'Y',
           inThisRound = coalesce(inThisRound,0) ) %>%
    ungroup() %>% 
    tbl_df() %>%
    select(SEASON_ID, R128_SLOT, BRACKET_SECTION, R064_SEED, TEAM_ID,  
           Round, Round_Slot, inThisRound, inThisRoundSimulated, simulateThisRound)
  
  
  
  
  ############################################# Simulate First Four
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R064") %>%
    group_by(Round_Slot) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = as.numeric(paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID))) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted -- Repeat for Sweet 16 Round
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin), by = c("TEAM_ID", "Round_Slot")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & simulateThisRound == "Y",0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin)
  
  
  ###### Simulate Round of 64
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R032", simEliminated == "N") %>%
    group_by(Round_Slot) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = as.numeric(paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID))) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin), by = c("TEAM_ID", "Round_Slot")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & is.na(inThisRoundSimulated),
                                         0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin)
  
  
  ###### Simulate Round of 32
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R016", simEliminated == "N") %>%
    group_by(Round_Slot) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = as.numeric(paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID))) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin), by = c("TEAM_ID", "Round_Slot")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & is.na(inThisRoundSimulated),
                                         0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin)
  
  ###### Simulate Sweet 16
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R008", simEliminated == "N") %>%
    group_by(Round_Slot) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = as.numeric(paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID))) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin), by = c("TEAM_ID", "Round_Slot")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & is.na(inThisRoundSimulated),
                                         0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin)
  
  ###### Simulate Elite 8
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R004", simEliminated == "N") %>%
    group_by(Round_Slot) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = as.numeric(paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID))) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin), by = c("TEAM_ID", "Round_Slot")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & is.na(inThisRoundSimulated),
                                         0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin)
  
  ###### Simulate Final Four
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R002", simEliminated == "N") %>%
    group_by(Round_Slot) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = as.numeric(paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID))) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin), by = c("TEAM_ID", "Round_Slot")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & is.na(inThisRoundSimulated),
                                         0, inThisRoundSimulated) )%>%
    select(-simOppWin, -simTeamWin)
  
  
  ###### Simulate Championship Game
  #grab games in sim
  temp <- filter(TourneySlotsMelted, Round == "R001", simEliminated == "N") %>%
    group_by(Round_Slot) %>%
    mutate(OPP_ID = max(TEAM_ID)) %>%
    filter(TEAM_ID != OPP_ID) %>% ungroup() %>%
    mutate(EVENT_IDplaceholder = as.numeric(paste0(pmin(TEAM_ID, OPP_ID), pmax(TEAM_ID, OPP_ID))) ) %>%
    left_join(., select(AllPossibleMatchups, ADJ_TEAM_ID, EVENT_IDplaceholder, TEAM_PRED_WINPCT), 
              by = c("EVENT_IDplaceholder", "TEAM_ID" = "ADJ_TEAM_ID")) %>%
    mutate(simTeamWin = if_else(runif(n()) <= TEAM_PRED_WINPCT,1,0 )) #grab %'s from lookup table and use them to simulate winners of each game
  
  #save winners in TourneySlotsMelted 
  TourneySlotsMelted <- left_join(TourneySlotsMelted,select(temp,OPP_ID,Round_Slot, simTeamWin) %>% rename(simOppWin = simTeamWin), 
                                  by = c("TEAM_ID" = "OPP_ID", "Round_Slot")) %>%
    left_join(., select(temp,TEAM_ID,Round_Slot, simTeamWin), by = c("TEAM_ID", "Round_Slot")) %>%
    mutate(inThisRoundSimulated = if_else(!is.na(simOppWin), 1-simOppWin, inThisRoundSimulated),
           inThisRoundSimulated = if_else(!is.na(simTeamWin), simTeamWin, inThisRoundSimulated)) %>%
    group_by(TEAM_ID) %>%
    mutate(simEliminated = if_else(min(inThisRoundSimulated, na.rm = T) ==0, "Y", "N") ) %>%
    ungroup() %>%
    mutate(inThisRoundSimulated = ifelse(simEliminated == "Y" & is.na(inThisRoundSimulated),
                                         0, inThisRoundSimulated) ) %>%
    select(-simOppWin, -simTeamWin)
  
  
  
  
  
  # SimTournamentResult <- reshape2::dcast(SEASON_ID + TEAM_ID ~ Round, value.var = 'inThisRoundSimulated', data = TourneySlotsMelted)# R128_SLOT + BRACKET_SECTION + R064_SEED + REGION_NAME + Status + EliminationRound + simEliminated
  SimTournamentResult <- pivot_wider(TourneySlotsMelted, id_cols = c("SEASON_ID", "TEAM_ID"), names_from = Round, values_from = inThisRoundSimulated)
  return(SimTournamentResult)
}



