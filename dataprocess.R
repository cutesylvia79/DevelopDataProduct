require(data.table)
library(dplyr)
library(plyr)
library(DT)


data <- fread("./Data/activity.csv", sep=",", header="auto", na.strings="NA",
      stringsAsFactors=FALSE,colClasses = c("integer","character","integer"))
week.date <- data.frame("week.day"=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), "DayType"=c("weekdays","weekdays","weekdays","weekdays","weekdays","weekend","weekend"))

##week.day <- 
##data <-read.csv("./Data/activity.csv", header= TRUE, sep=",", 
##                colClasses = c("integer","character","integer"))
## format the date into yyyy-mm-dd
data$date <-as.Date(data$date,"%Y-%m-%d")
data$week.day <-weekdays(data$date)

new.data <- merge(data,week.date, by=c("week.day"))
data <- new.data

impute.value <- function(steps,interval,IA.Data)
{
  imputev <-NA
  if (!is.na(steps))
    imputev <- c(steps)
  else
    imputev <- IA.Data[IA.Data$interval==interval,"steps"]
  return(imputev)
}

PrepData <- function(dt, StartDate, EndDate, NAProcessType)
{
  if (NAProcessType=="NoNA"){
    dt1.final<-dt[!is.na(dt$steps), ]
  }
  
  if (NAProcessType=="ImputeNAMean"){
    ##IA.Data1 <- ddply(dt,"interval", summarise, steps=avg  <- mean(steps,na.rm=TRUE))
    mean.step = mean(dt$steps, na.rm = TRUE)
    
    
    dt[is.na(dt$steps),"steps"] <- mean.step
    dt1.final <-dt
    ##dt1.final$steps <-mapply(impute.value,dt1.final$steps, dt1.final$interval, IA.Data1)
    
    
    
  }
  
  dt.Final2 <- dt1.final[dt1.final$date >= as.Date( StartDate),]
  dt.Final2 <- dt.Final2[dt.Final2$date <= as.Date(EndDate),]
  
  dtFinal <- dt.Final2
  return(dtFinal)
}




PrepDataset <- function(dt) {

  ##dt <- groupByYearPiece(dt, minYear, maxYear, minPiece,
  ##                       maxPiece, themes) 
  
  result <- datatable(dt, options = list(iDisplayLength = 50))
  return(result)
  # The following does not work
  #     fn$sqldf("SELECT * FROM data 
  #          WHERE year >= $minYear and year <= $maxYear
  #          and theme in $themes")
  
  #return(data.table(result))
}

PrepHistogram <- function(dt) {
  
  d1 <- ddply(dt,"date", summarise, steps=sum(steps))
  ##dlyActivity <- ddply(data, "date", summarise, steps=sum(steps))
  graph1<- ggplot(data=d1, aes(d1$steps)) + geom_histogram()
  ##print graph1
  ##gvisHistogram(dlyActivity$steps, options=list(
  ##  title= "Average Daily Activity Pattern",
  ##  histogram="{ hideBucketItems:true, bucketSize:10}",
  ##  hAxis="{ title:'Steps per day'}",
  ##  yAxis="{ title:'Frequency'}"
  ##))
  ##return(graph1)
  return(graph1)
}


PrepWeekData <- function(dt) {
  
  week.activity <-ddply(dt, c("interval", "DayType"), 
                        summarise, steps = mean(steps, na.rm=TRUE))
  return(week.activity)
}




