events <- events1998[,"EVENT_TX"]
stole_2B <- ifelse(grepl("SB2", events), as.character(events1998[,"BASE1_RUN_ID"]), '')
stole_3B <- ifelse(grepl("SB3", events), as.character(events1998[,"BASE2_RUN_ID"]), '')
stole_H <- ifelse(grepl("SBH", events), as.character(events1998[,"BASE3_RUN_ID"]), '')
steals_expanded <- data.frame(STOLE_2B=stole_2B, STOLE_3B=stole_3B, STOLE_H=stole_H)

num_SB_2B <- length(stole_2B[stole_2B != ''])
num_SB_3B <- length(stole_3B[stole_3B != ''])
num_SB_H <- length(stole_H[stole_H != ''])


sumPlayerSteals <- function(retroID) {
  return(length(steals_expanded[stole_2B==retroID,"STOLE_2B"])
         +length(steals_expanded[stole_3B==retroID,"STOLE_3B"])
         +length(steals_expanded[stole_H==retroID,"STOLE_H"]))
}
