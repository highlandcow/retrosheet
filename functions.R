# Returns events with a given player on base
findPlayerOnBase <- function(eventData, playerID) {
  playerOn1 <- eventData[eventData$BASE1_RUN_ID == playerID,]
  playerOn2 <- eventData[eventData$BASE2_RUN_ID == playerID,]
  playerOn3 <- eventData[eventData$BASE3_RUN_ID == playerID,]
  playerOnBase <- rbind(playerOn1, playerOn2, playerOn3)
  return(playerOnBase)
}

# Collect a team's home and away games (don't use for aggregate team data...)
getTeamGames <- function(teamID, eventData) {
  eventData[substr(eventData$GAME_ID, 1, 3) == teamID  | eventData$AWAY_TEAM_ID == teamID, ]
}
