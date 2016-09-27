
dir<-"C:/Users/anderstn/Desktop/RStudio/MangoApp/DM" #change this to your directory

temp <- list.files(pattern="*.csv", path = dir, full.names = TRUE)
importDM<-do.call("rbind", lapply(temp, read.csv, header = TRUE))
rawDM <- as.data.frame(importDM)

rawDM$Created.Date <- as.Date(rawDM$Created.Date...Time, format="%d/%m/%Y")
rawDM$Week <- strftime(rawDM$Created.Date,format="%W")
rawDM$True.Date <- rawDM$Created.Date
rawDM$Created.Date <- rawDM$Created.Date - 1

#This is amended DM script, delete if user does not wish to use
#and change filly$DryMatter=rawDM$Prediction.1.Value
maxDate<-Sys.Date() #calculates most recent date
rawDM$amendDate<-rawDM$Created.Date-maxDate # calculates the days difference between scan date and maxDate
rawDM$incrementDM<-as.numeric((rawDM$amendDate)) #this is assuming 0.1DM is daily increase
#would want to have 0.1 switched to a user selection from interface
rawDM$DMamendV<-sqrt(rawDM$incrementDM^2)# removes the negation
rawDM$DMamendV <-rawDM$DMamendV
rawDM$DMamended<-rawDM$Prediction.1.Value
#if the DM amend is 50
#this means 50 days, it will not be amended and will be classified as old data
# prodates the DM data to an estimated value based on maxDate
#for ex: maxDate = 23/11/2015    20/11/2015 DM= 15 -> DM= 15.3

filly <- data.frame(
  Date=rawDM$Created.Date,
  DryMatter=rawDM$DMamended,
  Lat=rawDM$Latitude,
  Long=rawDM$Longitude,
  Week=rawDM$Week,
  Amend=rawDM$DMamendV
)

CI <- .90
dmStrd <- 18

occur <- length(unique(filly$Amend))

if (occur > 2) {splitDate <- split(filly, list(filly$Amend))
regPlots <- as.numeric(vapply(splitDate, function(x) mean(x$DryMatter, na.rm=TRUE), 0))
datePlots <- unique(filly$Amend)
regress <- lm(regPlots~datePlots)
slope <-as.numeric(regress$coefficients[2])
interPlot <-as.numeric(regress$coefficients[1])
filly$ciAmend <- (dmStrd-filly$DryMatter)/slope
}else{
  splitDate <- split(filly, list(filly$Amend))
  regPlots <- as.numeric(vapply(splitDate, function(x) mean(x$DryMatter, na.rm=TRUE), 0))
  datePlots <- unique(filly$Amend)
  regress <- lm(regPlots~datePlots)
  slope <- 0.1 # here we would use inc within the server side
  filly$ciAmend <- (dmStrd-filly$DryMatter)/slope
}
  
recentDM <- filly[which(filly$Amend==min(filly$Amend)),]

recentDM<- recentDM[with(recentDM, order(recentDM$DryMatter)), ]
quantile(recentDM$DryMatter, CI)

recentDM <- recentDM[recentDM$DryMatter < quantile(recentDM$DryMatter, CI), ]
recentDM$ciAmend <- recentDM$ciAmend - recentDM$Amend
highAmend <- round(max(recentDM$ciAmend))

pickDateProj<- Sys.Date()+highAmend


plot(datePlots, regPlots,xlab = "Days", ylab = "Average DM%", col="red")
abline(lm(regPlots ~ datePlots))

print(paste0("The average daily DM increase was: ", round(slope, digits = 2)))
print(paste0("The projected pick date for a DM minimum of ", dmStrd, " all above ", CI, "% is : ", pickDateProj))
