# This function expands retrosheet event data to more easily analyze stolen bases. 
# It takes retrosheet event data and returns a data frame defining which bases were 
# stolen during a play and the retrosheet ID of any player who stole a base. 

expandStolenBases <- function(eventData) {
  i = 1
  base_stolen <- vector()
  steal_2B <- vector()
  steal_3B <- vector()
  steal_H <- vector()
  events <- eventData
  
  isDoubleSteal <- function() {
    return(
      (grepl("SB2", events[i,]$EVENT_TX) && grepl("SB3", events[i,]$EVENT_TX)) | 
        (grepl("SB2", events[i,]$EVENT_TX) && grepl("SBH", events[i,]$EVENT_TX)) | 
        (grepl("SB3", events[i,]$EVENT_TX) && grepl("SBH", events[i,]$EVENT_TX)) 
    )
  }
  
  while ( i <= nrow(events)) {
    if ( grepl("SB2", events[i,]$EVENT_TX) && grepl("SB3", events[i,]$EVENT_TX) ) {
      # Second and third were stolen
      base_stolen[i] <- "2B+3B"
      steal_2B[i] <- as.character(events[i, ]$BASE1_RUN_ID)
      steal_3B[i] <- as.character(events[i, ]$BASE2_RUN_ID)
      steal_H[i] <- "NA"
    }
    if ( grepl("SB3", events[i,]$EVENT_TX) && grepl("SBH", events[i,]$EVENT_TX) ) {
      # Third and home were stolen
      base_stolen[i] <- "3B+H"
      steal_2B[i] <- "NA"
      steal_3B[i] <- as.character(events[i, ]$BASE2_RUN_ID)
      steal_H[i] <- as.character(events[i, ]$BASE3_RUN_ID)
    }
    if ( grepl("SB2", events[i,]$EVENT_TX) && grepl("SBH", events[i,]$EVENT_TX) ) {
      # Second and home were stolen
      base_stolen[i] <- "2B+H"
      steal_2B[i] <- as.character(events[i, ]$BASE1_RUN_ID)
      steal_3B[i] <- "NA"
      steal_H[i] <- as.character(events[i, ]$BASE3_RUN_ID)
    }
    if ( grepl("SB2", events[i,]$EVENT_TX)
         && (isDoubleSteal() == FALSE)) {
      base_stolen[i] <- "2B"
      steal_2B[i] <- as.character(events[i, ]$BASE1_RUN_ID)
      steal_3B[i] <- "NA"
      steal_H[i] <- "NA"
    } 
    if ( grepl("SB3", events[i,]$EVENT_TX)
         && (isDoubleSteal() == FALSE)) {
      base_stolen[i] <- "3B"
      steal_2B[i] <- "NA"
      steal_3B[i] <- as.character(events[i,]$BASE2_RUN_ID)
      steal_H[i] <- "NA"
    } 
    if ( grepl("SBH", events[i,]$EVENT_TX)
         && (isDoubleSteal() == FALSE)) {
      base_stolen[i] <- "H"
      steal_2B[i] <- "NA"
      steal_3B[i] <- "NA"
      steal_H[i] <- as.character(events[i,]$BASE3_RUN_ID)
    } 
    if ( ( grepl("SB2", events[i,]$EVENT_TX) | 
           grepl("SB3", events[i,]$EVENT_TX) | 
           grepl("SBH", events[i,]$EVENT_TX) ) == FALSE ) {
      # No base was stolen
      base_stolen[i] <- "NONE"
      steal_2B[i] <- "NA"
      steal_3B[i] <- "NA"
      steal_H[i] <- "NA"
    }
    i <- i+1
  }
  steals <- data.frame(BASE_STOLEN=base_stolen,
                       STOLE_2B_ID=steal_2B, 
                       STOLE_3B_ID=steal_3B, 
                       STOLE_H_ID=steal_H)
  return(steals)
}


sumSteals <- function(stealsData, playerID) {
  # Returns the total number of steals by a player
  return(nrow(stealsData[stealsData$STOLE_2B_ID == playerID, ]) +
           nrow(stealsData[stealsData$STOLE_3B_ID == playerID, ]))
}

sumStealsDetailed <- function(stealsData, playerID) {
  # Returns data frame showing number of steals of each base and the total number of steals for a given player
  playerSteals <- (subset(stealData, stealData$STOLE_2B_ID == playerID | stealData$STOLE_3B_ID == playerID ,))
  stole2B <- nrow(stealsData[stealsData$STOLE_2B_ID == playerID, ])
  stole3B <- nrow(stealsData[stealsData$STOLE_3B_ID == playerID, ])
  stoleH <- nrow(stealsData[stealsData$STOLE_H_ID == playerID, ])
  totalSteals <- sum(stole2B, stole3B, stoleH)
  steals <- data.frame(PLAYER_ID=playerID, STOLE_2B=stole2B, STOLE_3B=stole3B, STOLE_H=stoleH, TOTAL_STEALS=totalSteals)
  return(steals)
}

getBaseStealers <- function(stealsData) {
  # Returns vector of players who stole at least one base
  allStealers <- c(as.character(stealsData[stealsData$STOLE_2B_ID != 'NA', "STOLE_2B_ID"]),
                   as.character(stealsData[stealsData$STOLE_3B_ID != 'NA', "STOLE_3B_ID"]))
  return(unique(allStealers))
}

sumStealsDetailedAll <- function(stealsData, players) {
  # Summarize stolen bases (count of steals of 2nd, 3rd, home; total steals) for a set of players. 
  i = 1
  playerID <- vector()
  stole2B <- vector()
  stole3B <- vector()
  stoleH <- vector()
  total_steals <- vector()
  allThefts <- data.frame(PLAYER_ID=playerID, STOLE_2B=stole2B, 
                          STOLE_3B=stole3B, STOLE_H=stoleH, 
                          TOTAL_STEALS=total_steals, stringsAsFactors = FALSE)

  while ( i < length(players) ) {
    allThefts[i,] <- sumStealsDetailed(stealData, as.character(players[i]))
    i <- i + 1
  }
  
  return(allThefts)
}
