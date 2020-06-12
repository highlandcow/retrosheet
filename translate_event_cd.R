# Translates EVENT_CD values into plain English

event_cd.codes <- read.csv("path-to-event_cd-code-book.csv")

translate.event_cd <- function(playerdf){
  c <- event_cd.codes
  p <- playerdf$EVENT_CD
  e <- character(0)
  i<-1
  while (i <= length(p)) {
    e[i] <- as.character(event_cd.codes[event_cd.codes$Code == p[i], ]$Primary.event)
    i <- i+1
  }
  return(e)
}

# Example: events2019nya$EVENT_CD_TRANSLATED <- event_cd.codes(events2019nya)
