pollutantmean <- function(directory, pollutant, id){
  average_pol = 0
  for(i in seq_along(id)){
    filename <- if(id[i]<10){
      paste(directory,'/', paste(0,0,id[i],sep=''), '.csv', sep='')
    }
    else if (id[i]>=10 & id[i]<100){
      paste(directory,'/', paste(0,id[i],sep=''), '.csv', sep='')
    }
    else{
      paste(directory,'/', paste(id[i],sep=''), '.csv', sep='')
    }
    data_ = read.csv(filename)
    average_pol <- average_pol + mean(data_[[pollutant]], na.rm=TRUE) 
  }
average_pol
}