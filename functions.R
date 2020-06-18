
# BASE RUNNING
# Returns events with a given player on base
findPlayerOnBase <- function(eventData, playerID) {
  playerOn1 <- eventData[eventData$BASE1_RUN_ID == playerID,]
  playerOn2 <- eventData[eventData$BASE2_RUN_ID == playerID,]
  playerOn3 <- eventData[eventData$BASE3_RUN_ID == playerID,]
  playerOnBase <- rbind(playerOn1, playerOn2, playerOn3)
  return(playerOnBase)
}

getUniqueRunners <- function(eventData) {
  runners <- as.character(eventData$BASE1_RUN_ID)
  runners <- as.character(eventData$BASE2_RUN_ID)
  runners <- as.character(eventData$BASE3_RUN_ID)
  return(unique(runners[runners != '']))
}


# Expand Game Info

expandGameInfo <- function (eventData) {
  # Returns data frame with HOME_TEAM, DATE, and GAME_NUM as variables. 
  home_teams <- substring(eventData$GAME_ID,0,3)
  date <- as.Date(substring(eventData$GAME_ID,4,11),"%Y%m%d")
  game_num <- substring(eventData$GAME_ID,12)
  
  gameInfo <- data.frame(
    HOME_TEAM_ID = home_teams,
    DATE = date,
    GAME_NUMBER = game_num
  )
  
  return(gameInfo)
}



# Collect a team's home and away games (don't use for aggregate team data...)
getTeamGames <- function(teamID, eventData) {
  eventData[substr(eventData$GAME_ID, 1, 3) == teamID  | eventData$AWAY_TEAM_ID == teamID, ]
}
