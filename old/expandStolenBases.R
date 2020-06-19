# TODO: Vectorization much? :)

# expandStolenBases() uses Retrosheet play-by-play data to produce a data frame containing details
# for any stolen base occuring during an event. Specifically, expandStolenBases() parses the EVENT_TX 
# value for each event and determines if a stolen base occurred. If a stolen base occurred, the
# base stolen and the ID of the player who stole the base is captured. "NA" indicates a steal did not
# occur. 

# Call expandStolenBases() passing a data frame containing the Retrosheet play-by-play event data. 
# The function requires the variables EVENT_TX, BASE1_RUN_ID, BASE2_RUN_ID, and BASE3_RUN_ID to work.

steals1998 <- expandStolenBases(events1998)

# Example output:
head(steals1998[steals1998$BASE_STOLEN != 'NONE', ])

#     BASE_STOLEN STOLE_2B_ID STOLE_3B_ID STOLE_H_ID
# 79           2B    jeted001          NA         NA
# 132          2B    knobc001          NA         NA
# 177       2B+3B    vizqo001    loftk001         NA
# 186          2B    holld001          NA         NA
# 219          2B    erstd001          NA         NA
# 241          2B    vizqo001          NA         NA

# This can be merged into the Retrosheet play-by-play data to facilitate analysis of plays involving
# stolen bases. Several useful functions are included in this file to explore the data. 

# Example 1 - View plays in which second and home were stolen
events1998sb <- cbind(events1998, steals1998)
events1998sb[events1998sb$BASE_STOLEN == '2B+H', ]

# Example 2 - View plays featuring a "straight steal" of home
events1998sb[events1998sb$BASE_STOLEN == 'H', ]

# Example 3 - View plays in which Rickey Henderson stole a base
getPlayerSteals(steals1998, 'hendr001')

# Example 4 - Get the total number of steals by Rickey Henderson
sumSteals(steals1998, 'hendr001')

# Example 5 - Summarize Rickey Henderson's steals by base stolen
sumStealsDetailed(steals1998, 'hendr001')

# Output:
#   PLAYER_ID STOLE_2B STOLE_3B STOLE_H TOTAL_STEALS
# 1  hendr001       51       15       0           66

# Example 6 - Get a vector of playerIDs with stolen bases
stealers1998 <- getBaseStealers(steals1998)

# Example 7 - Summarize stolen bases for a vector of playerIDs
steals_summary1998 <- sumStealsDetailedAll(steals1998, stealers1998)
head(steals_summary1998[order(-steals_summary1998$TOTAL_STEALS), ])

# Output:
#     PLAYER_ID STOLE_2B STOLE_3B STOLE_H TOTAL_STEALS
# 23   hendr001       51       15       0           66
# 73   womat001       49        9       0           58
# 53   loftk001       38       16       0           54
# 16   stews002       45        6       0           51
# 228  biggc001       39       11       0           50
# 33   rodra001       30       15       1           46


# ~~~ #


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
      base_stolen[i] <- "NA"
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
           nrow(stealsData[stealsData$STOLE_3B_ID == playerID, ])+
           nrow(stealsData[stealsData$STOLE_H_ID == playerID, ]))
}

sumStealsDetailed <- function(stealsData, playerID) {
  # Returns data frame showing number of steals of each base and the total number of steals for a given player
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

  while ( i <= length(players) ) {
    allThefts[i,] <- sumStealsDetailed(stealData, as.character(players[i]))
    i <- i + 1
  }
  
  return(allThefts)
}

getPlayerSteals <- function(stealsData, playerID) {
  steals <- stealsData[stealsData$STOLE_2B_ID == playerID | 
                         stealsData$STOLE_3B_ID == playerID | 
                         stealsData$STOLE_H_ID == playerID, ]
  return(steals)
}
