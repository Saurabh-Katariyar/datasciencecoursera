complete <- function(directory, id=1:332){
  dataframe <- data.frame(id = integer(length(id)),
                          nobs = integer(length(id)))
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
    data = read.csv(filename)
    dataframe$id[i] <- id[i]
    dataframe$nobs[i] <- sum(complete.cases(data))
  }
dataframe
}