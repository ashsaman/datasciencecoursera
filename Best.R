best<-function(state,outcome){
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character", header = TRUE)
  df<-as.data.frame(cbind(data[,2],   # hospital
                          data[,7],   # state
                          data[,11],  # heart attack
                          data[,17],  # heart failure
                          data[,23]), # pneumonia
                          stringsAsFactors = FALSE)
  colnames(df)<- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% df[,"state"]){stop("invalid state")}
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {stop("invalid outcome")}
  
  else {
    rowextract<-which(df[,"state"]==state) #Get the Row Numbers
    dataextract<-df[rowextract,]  #Get the Data from each Row Numbers
    outcomeextract<-as.numeric(dataextract[,eval(outcome)]) #Extract specific Data based on outcome
    min_val<-min(outcomeextract,na.rm = TRUE) #Evaluate the minimum value from specific data
    result<-dataextract[,"hospital"][which(outcomeextract==min_val)] #Evaluate the hospital name for minimum value
    output<-result[order(result)]
  }
  
  return(output)
}