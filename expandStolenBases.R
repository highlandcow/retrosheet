events <- events1998[,"EVENT_TX"]

SB_2B <- grepl("SB2", events)
SB_3B <- grepl("SB3", events)
SB_H <- grepl("SBH", events)

stole_2B <- ifelse(SB_2B, as.character(events1998[,"BASE1_RUN_ID"]), '')
stole_3B <- ifelse(SB_3B, as.character(events1998[,"BASE2_RUN_ID"]), '')
stole_h <- ifelse(SB_H, as.character(events1998[,"BASE3_RUN_ID"]), '')

steals_expanded <- data.frame(STOLE_2B=stole_2B, STOLE_3B=stole_3B, STOLE_H=stole_h)

num_SB_2B <- length(STOLE_2B[STOLE_2B != ''])
num_SB_3B <- length(STOLE_3B[STOLE_3B != ''])
num_SB_H <- length(STOLE_H[STOLE_H != ''])
