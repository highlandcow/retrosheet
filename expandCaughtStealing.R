### Expand Retrosheet data to describe caught stealing events

expandCaughtStealing <- function(eventData) {
  i = 1
  sb_base_attempted <- vector()
  caught_attempt2B <- vector()
  caught_attempt3B <- vector()
  caught_attemptH <- vector()
  events <- eventData

  while ( i <= nrow(events)) {
    if ( grepl("CS2", events[i,]$EVENT_TX) ) {
      # Runner on 1st was thrown out attempting to steal 2nd
      sb_base_attempted[i] <- "2B"
      caught_attempt2B[i] <- as.character(events[i,]$BASE1_RUN_ID)
      caught_attempt3B[i] <- "NA"
      caught_attemptH[i] <- "NA"
    } 
    if ( grepl("CS3", events[i,]$EVENT_TX) ) {
      # Runner on 2nd was thrown out attempting to steal 3rd
      sb_base_attempted[i] <- "3B"
      caught_attempt2B[i] <- "NA"
      caught_attempt3B[i] <- as.character(events[i,]$BASE2_RUN_ID)
      caught_attemptH[i] <- "NA"
    } 
    if ( grepl("CSH", events[i,]$EVENT_TX) ) {
      # Runner on 3rd was thrown out attempting to steal home
      sb_base_attempted[i] <- "H"
      caught_attempt2B[i] <- "NA"
      caught_attempt3B[i] <- "NA"
      caught_attemptH[i] <- as.character(events[i,]$BASE3_RUN_ID)
    } 
    if ( ( grepl("CS2", events[i,]$EVENT_TX) | 
           grepl("CS3", events[i,]$EVENT_TX) | 
           grepl("CSH", events[i,]$EVENT_TX) ) == FALSE ) {
      # No steal attempt
      sb_base_attempted[i] <- "NA"
      caught_attempt2B[i] <- "NA"
      caught_attempt3B[i] <- "NA"
      caught_attemptH[i] <- "NA"
    }
    i <- i+1
  }
  steals <- data.frame(SB_BASE_ATTEMPTED=sb_base_attempted,
                       CS_ATTEMPT_2B=caught_attempt2B, 
                       CS_ATTEMPT_3B=caught_attempt3B, 
                       CS_ATTEMPT_H=caught_attemptH)
  return(steals)
}


### 


sumCS <- function(caughtStealsData, playerID) {
  # Returns the total number of caught stealing by a player
  return(nrow(caughtStealsData[caughtStealsData$CS_ATTEMPT_2B == playerID, ]) +
           nrow(caughtStealsData[caughtStealsData$CS_ATTEMPT_3B == playerID, ])+
           nrow(caughtStealsData[caughtStealsData$CS_ATTEMPT_H == playerID, ]))
}

summarizeCS <- function(csData, playerID) {
  # Returns a summary of caught stealing events for a single player
  cs2B <- nrow(csData[csData$CS_ATTEMPT_2B == playerID, ])
  cs3B <- nrow(csData[csData$CS_ATTEMPT_3B == playerID, ])
  csH <- nrow(csData[csData$CS_ATTEMPT_H == playerID, ])
  totalCS <- sum(cs2B, cs3B, csH)
  caughtStealing <- data.frame(PLAYER_ID=playerID, CS_ATTEMPT_2B=cs2B, 
                               CS_ATTEMPT_3B=cs3B, CS_ATTEMPT_H=csH, TOTAL_CS=totalCS,
                               stringsAsFactors = FALSE)
  return(caughtStealing)
}


summarizeCS.all <- function(csData, players) {
  # Returns a summary of CS events for a vector of players
  i = 1
  playerID <- vector()
  cs2B <- vector()
  cs3B <- vector()
  csH <- vector()
  totalCS <- vector()
  allCS <- data.frame(PLAYER_ID=playerID, 
                      CS_ATTEMPT_2B=cs2B,
                      CS_ATTEMPT_3B=cs3B, 
                      CS_ATTEMPT_H=csH, 
                      TOTAL_CS=totalCS, 
                        stringsAsFactors = FALSE)
  
  while ( i <= length(players) ) {
    allCS[i,] <- summarizeCS(csData, as.character(players[i]))
    i <- i + 1
  }
  
  return(allCS)
}

getCaughtStealers <- function(csData) {
  # Returns vector of players who were caught stealing at least one abse
  allCaughtStealers <- c(as.character(csData[csData$CS_ATTEMPT_2B != 'NA', "CS_ATTEMPT_2B"]),
                   as.character(csData[csData$CS_ATTEMPT_3B != 'NA', "CS_ATTEMPT_3B"]),
                   as.character(csData[csData$CS_ATTEMPT_H != 'NA', "CS_ATTEMPT_H"])
                   )
  return(unique(allCaughtStealers))
}
