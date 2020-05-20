rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  df  <- as.data.frame(cbind(data[, 2],   # hospital
                              data[, 7],   # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% df[, "state"]) {stop('invalid state')} 
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){stop('invalid outcome')}
  
  else if (is.numeric(rank)) {
    rowextract <- which(df[, "state"] == state) #Get the Row Number
    dataextract<-df[rowextract,]                #Get the Data from each Row Numbers
    dataextract[, eval(outcome)] <- as.numeric(dataextract[, eval(outcome)]) #Extract specific Data based on outcome
    dataextract <- dataextract[order(dataextract[, eval(outcome)], dataextract[, "hospital"]), ]
    output <- dataextract[, "hospital"][rank]
  } 
  
  else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      rowextract <- which(df[, "state"] == state)
      dataextract <- df[rowextract, ]    
      dataextract[, eval(outcome)] <- as.numeric(dataextract[, eval(outcome)])
      dataextract <- dataextract[order(dataextract[, eval(outcome)], dataextract[, "hospital"], decreasing = TRUE), ]
      output <- dataextract[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(output)
}