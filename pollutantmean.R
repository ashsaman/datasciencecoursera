pollutantmean<-function(directory, pollutant , id=1:332){
  all_files<-list.files(path=directory,full.names = TRUE)
  selected_data<-data.frame()
  for(i in id){
    selected_data<-rbind(selected_data,read.csv(all_files[i]))
  }
    
  if(pollutant=='sulfate'){
    mean(selected_data$sulfate,na.rm = TRUE)
    }  
    
  else if(pollutant=='nitrate') {
    mean(selected_data$nitrate,na.rm = TRUE)
    } 
      
  }
  
  
