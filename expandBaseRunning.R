expandNumRunners <- function(eventData) {
  #' Returns vector with numbers of runners on during a play
  events <- eventData[, c("BASE1_RUN_ID", "BASE2_RUN_ID", "BASE3_RUN_ID")]
  runnersOn <- (eventData$BASE1_RUN_ID != "") + (eventData$BASE2_RUN_ID != "") + (eventData$BASE3_RUN_ID != "")
  return(runnersOn)
}
