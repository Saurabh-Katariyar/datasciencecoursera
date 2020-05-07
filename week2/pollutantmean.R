pollutantmean <- function(directory, pollutant, id=1:332){
  average_pol <- 0
  entries <- 0
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
    entries <- entries + colSums(!is.na(data_))
    average_pol <- average_pol + sum(data_[[pollutant]], na.rm=TRUE) 
  }
average_pol/entries[[pollutant]]
}