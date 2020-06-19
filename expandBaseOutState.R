expandBaseOutState <- function(eventData) {
  # Returns the starting base-out state in 1B2B3B_out format
  # Example: 000_0 reflects bases empty, 0 outs; 103_2 is 1st and 3rd with 2 outs
  
  status1B <- ifelse(eventData[,"BASE1_RUN_ID"] =="", 0, 1)
  status2B <- ifelse(eventData[,"BASE2_RUN_ID"] =="", 0, 2)
  status3B <- ifelse(eventData[,"BASE3_RUN_ID"] =="", 0, 3)
  outs <- eventData[,"OUTS_CT"]
  base_out_state <- paste(status1B,status2B,status3B,'_',outs,sep="")
  return(base_out_state)
}
