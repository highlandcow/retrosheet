# Returns events with a given player on base
findPlayerOnBase <- function(annualData, playerID) {
  playerOnBase <- subset(annualData, BASE1_RUN_ID == playerID | BASE2_RUN_ID == playerID | BASE3_RUN_ID == playerID )
  return(playerOnBase)
}

# Collect a team's home and away games (don't use for aggregate team data...)
getTeamGames <- function(teamID, eventData) {
  eventData[substr(eventData$GAME_ID, 1, 3) == teamID  | eventData$AWAY_TEAM_ID == teamID, ]
}
