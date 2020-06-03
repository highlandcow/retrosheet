# Returns events with a given player on base
findPlayerOnBase <- function(annualData, playerID) {
  playerOnBase <- subset(annualData, BASE1_RUN_ID == playerID | BASE2_RUN_ID == playerID | BASE3_RUN_ID == playerID )
  return(playerOnBase)
}
