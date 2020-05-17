complete<-function(directory, id=1:332){
  all_files<-list.files(path=directory,full.names = TRUE)
  selected_data<-data.frame()
  complete_cases <- data.frame()
  nobs <- data.frame();
  
  for(i in id){
    selectedData <- (read.csv(all_files[i],header=TRUE))
    nobs <- sum(complete.cases(selectedData))
    complete_cases <- rbind(complete_cases, data.frame(i,nobs))
  }
  complete_cases
}

