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
        (grepl("SB2", events[i,]$EVENT_TX) && grepl("SB4", events[i,]$EVENT_TX)) | 
        (grepl("SB3", events[i,]$EVENT_TX) && grepl("SB4", events[i,]$EVENT_TX)) 
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
    if ( grepl("SB3", events[i,]$EVENT_TX) && grepl("SB4", events[i,]$EVENT_TX) ) {
      # Third and home were stolen
      base_stolen[i] <- "3B+H"
      steal_2B[i] <- "NA"
      steal_3B[i] <- as.character(events[i, ]$BASE2_RUN_ID)
      steal_H[i] <- as.character(events[i, ]$BASE3_RUN_ID)
    }
    if ( grepl("SB2", events[i,]$EVENT_TX) && grepl("SB4", events[i,]$EVENT_TX) ) {
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
    if ( grepl("SB4", events[i,]$EVENT_TX)
         && (isDoubleSteal() == FALSE)) {
      base_stolen[i] <- "H"
      steal_2B[i] <- "NA"
      steal_3B[i] <- "NA"
      steal_H[i] <- as.character(events[i,]$BASE3_RUN_ID)
    } 
    if ( ( grepl("SB2", events[i,]$EVENT_TX) | 
           grepl("SB3", events[i,]$EVENT_TX) | 
           grepl("SB4", events[i,]$EVENT_TX) ) == FALSE ) {
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

# Get the total stolen bases by a player
sumSteals <- function(stealsData, playerID) {
    return(nrow(stealsData[stealsData$STOLE_2B_ID == playerID, ]) +
    nrow(stealsData[stealsData$STOLE_3B_ID == playerID, ]))
}
