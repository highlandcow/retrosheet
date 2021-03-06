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

###

# Collect plays from a team's home and away games
getTeamGames <- function(teamID, eventData) {
  eventData[substr(eventData$GAME_ID, 1, 3) == teamID  | eventData$AWAY_TEAM_ID == teamID, ]
}
