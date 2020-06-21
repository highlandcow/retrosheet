expandStolenBases <- function(eventData) {
  #' Returns data frame with variables STOLE_2B, STOLE_3B, STOLE_H
  events <- eventData[,"EVENT_TX"]
  stole_2B <- ifelse(grepl("SB2", events), as.character(eventData[,"BASE1_RUN_ID"]), '')
  stole_3B <- ifelse(grepl("SB3", events), as.character(eventData[,"BASE2_RUN_ID"]), '')
  stole_H <- ifelse(grepl("SBH", events), as.character(eventData[,"BASE3_RUN_ID"]), '')
  steals_expanded <- data.frame(STOLE_2B=stole_2B, STOLE_3B=stole_3B, STOLE_H=stole_H)
  return(steals_expanded)
}

expandCaughtStealing <- function(eventData) {
  #' Returns data frame with variables CAUGHT_2B, CAUGHT_3B, CAUGHT_H
  events <- eventData[,"EVENT_TX"]
  caught_2B <- ifelse(grepl("CS2", events), as.character(eventData[,"BASE1_RUN_ID"]), '')
  caught_3B <- ifelse(grepl("CS3", events), as.character(eventData[,"BASE2_RUN_ID"]), '')
  caught_H <- ifelse(grepl("CSH", events), as.character(eventData[,"BASE3_RUN_ID"]), '')
  cs_expanded <- data.frame(CAUGHT_2B=caught_2B, CAUGHT_3B=caught_3B, CAUGHT_H=caught_H)
  return(cs_expanded)
}

### 

sumPlayerSteals <- function(stealsData, retroID) {
  #' Returns total number of steals by a player
  return(length(stealsData[stole_2B==retroID,"STOLE_2B"])
         +length(stealsData[stole_3B==retroID,"STOLE_3B"])
         +length(stealsData[stole_H==retroID,"STOLE_H"]))
}

summarizePlayerSteals <- function(eventData, playerID) {
  #' Returns detailed info of SB and CS events by playerID
  
  player_sb <- eventData[, c("STOLE_2B", "STOLE_3B", "STOLE_H")] == playerID
  player_cs <- eventData[, c("CAUGHT_2B", "CAUGHT_3B", "CAUGHT_H")] == playerID
  
  stole_2b <- sum(player_sb[,"STOLE_2B"])
  stole_3b <- sum(player_sb[,"STOLE_3B"])
  stole_h <- sum(player_sb[,"STOLE_H"])
  stole_total <- sum(stole_2b, stole_3b, stole_h)
  
  caught_2b <- sum(player_cs[,"CAUGHT_2B"])
  caught_3b <- sum(player_cs[,"CAUGHT_3B"])
  caught_h <- sum(player_cs[,"CAUGHT_H"])
  caught_total <- sum(caught_2b, caught_3b, caught_h)

  attempts_2b <- sum(stole_2b, caught_2b)
  attempts_3b <- sum(stole_3b, caught_3b)
  attempts_h <- sum(stole_h, caught_h)
  attempts_total <- sum(attempts_2b, attempts_3b, attempts_h)
  
  steal_digest <- data.frame(TOTAL_STEALS=stole_total,STOLE_2B=stole_2b,
                             STOLE_3B=stole_3b, STOLE_H=stole_h)
  caught_digest <- data.frame(TOTAL_CAUGHT=caught_total, CAUGHT_2B=caught_2b, CAUGHT_3B=caught_3b, 
                              CAUGHT_H=caught_h)
  total_digest <- data.frame(TOTAL_ATTEMPTS=attempts_total,
                             ATTEMPTS_2B=attempts_2b, ATTEMPTS_3B=attempts_3b, 
                             ATTEMPTS_H=attempts_h)
  
  digest <- list(steal_digest, caught_digest, total_digest)
  names(digest) <- c("Stolen_Bases", "Caught_Stealing", "Total_Attempts")
  
  return(digest)
}
