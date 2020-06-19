expandStolenBases <- function(eventData) {
  # Returns data frame with variables STOLE_2B, STOLE_3B, STOLE_H
  events <- events1998[,"EVENT_TX"]
  stole_2B <- ifelse(grepl("SB2", events), as.character(events1998[,"BASE1_RUN_ID"]), '')
  stole_3B <- ifelse(grepl("SB3", events), as.character(events1998[,"BASE2_RUN_ID"]), '')
  stole_H <- ifelse(grepl("SBH", events), as.character(events1998[,"BASE3_RUN_ID"]), '')
  steals_expanded <- data.frame(STOLE_2B=stole_2B, STOLE_3B=stole_3B, STOLE_H=stole_H)
  return(steals_expanded)
}

sumPlayerSteals <- function(stealsData, retroID) {
  # Returns total steals by a player
  return(length(stealsData[stole_2B==retroID,"STOLE_2B"])
         +length(stealsData[stole_3B==retroID,"STOLE_3B"])
         +length(stealsData[stole_H==retroID,"STOLE_H"]))
}
