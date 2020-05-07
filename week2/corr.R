corr <- function(directory, threshold=0){
  cor_vect <- c()
  for(i in 1:332){
    filename <- if(i<10){
      paste(directory,'/', paste(0,0,i,sep=''), '.csv', sep='')
    }
    else if (i>=10 & i<100){
      paste(directory,'/', paste(0,i,sep=''), '.csv', sep='')
    }
    else{
      paste(directory,'/', paste(i,sep=''), '.csv', sep='')
    }
    data = read.csv(filename)
    if(sum(complete.cases(data))>threshold){
      cor_vect <- c(cor_vect, cor(data$sulfate, data$nitrate, use='complete.obs'))
    }
  }
  cor_vect
}