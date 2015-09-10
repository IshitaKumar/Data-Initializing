corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  
  co=c()
  for (i in seq_len(length(list.files(directory))))
  {
    airData=read.csv(paste(directory,"/",formatC(i, width=3, flag="0"),".csv",sep=""),colClasses = c("Date","numeric","numeric","numeric"))
    validObs=sum(complete.cases(airData))
    if(validObs>threshold)
    {
      co=append(co,cor(airData[["sulfate"]],airData[["nitrate"]],use="complete.obs"))
    }
  }
  
  co
}