complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  files=c()
  obs=c()
  for (i in id)
  {
    airData=read.csv(paste(directory,"/",formatC(i, width=3, flag="0"),".csv",sep=""))
    files=append(files,i)
    validObs=sum(complete.cases(airData))
    obs=append(obs,validObs)
  }
  data.frame(id=files,nobs=obs)
}