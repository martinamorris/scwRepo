summarize <- function(datafile) {
  mydir <- "../"  # data dirctory
  rawdata <- read.csv(paste(mydir, datafile, sep=""), header=T, na.strings="M") #read it in
  aaa <- rawdata[,c("Tmax","Tmin", "Tavg", "Depart")] #subset
  
  par(mfrow=c(1,2))
  plot(aaa$Tmin, aaa$Tmax, pch="@", col="red",
       main="Dec 2015 Daily Temps", xlab="Min", ylab="Max")
  abline(lm(aaa$Tmax ~ aaa$Tmin))
  
  plot(aaa$Depart, col="blue",
       main="Departure from Average", xlab="Date", ylab="Degrees")
  abline(h=0, col="red")
  
  meanvec <- colMeans(aaa, na.rm=TRUE)
  print(round(meanvec,2))
  
  return(meanvec)
}

