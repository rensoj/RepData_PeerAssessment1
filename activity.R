activity <- function(){

#############################################################     
     library(dplyr)
     library(ggplot2)        
     if (!exists("FullData",envir=.GlobalEnv)){
          if(!file.exists("./activity.csv")){
               unzip("./activity.zip")
          }
          FullData <- as_tibble(read.csv("activity.csv",stringsAsFactors=FALSE))
          assign("FullData",FullData,envir=.GlobalEnv)
     }
     
     FullData <- tibble(FullData,PosDate=as.POSIXct(FullData$date,format="%Y-%m-%d"))
############################################################
     
############################################################ 
     DaySums <- FullData %>% group_by(PosDate) %>% summarize(TotalSteps=sum(steps,na.rm=TRUE))
     hist(DaySums$TotalSteps,breaks=15,density=20,xlab="Daily Steps",main="Distribution of Daily Step Totals")
     print("The mean number of daily steps is:")
     print(mean(DaySums$TotalSteps))
     print("The median number of daily steps is:")
     print(median(DaySums$TotalSteps))
###########################################################
     
###########################################################
     MinMeans <- FullData %>% group_by(interval) %>% summarize(mean=mean(steps,na.rm=TRUE))
     with(MinMeans,plot(interval,mean,type="l",ylab="Mean Steps per 5 min Interval",
                        xlab="Time (24hr)",main="Activity Level"))
     print("The maximum average step count occurs at:")
     print(MinMeans$interval[which.max(MinMeans$mean)])
##########################################################

##########################################################
     IC <- !complete.cases(FullData)
     print("The number of rows with missing values is:")
     print(sum(IC))
     CompData <- FullData
     CompData$steps[IC] <- MinMeans$mean[match(FullData$interval[IC],MinMeans$interval)]
     CompDaySums <- CompData %>% group_by(PosDate) %>% summarize(TotalSteps=sum(steps,na.rm=TRUE))
     hist(CompDaySums$TotalSteps,breaks=15,density=20,xlab="Daily Steps",
          main="Distribution of Completed Daily Step Totals")
     print("The mean number of daily steps is:")
     print(mean(CompDaySums$TotalSteps))
     print("The median number of daily steps is:")
     print(median(CompDaySums$TotalSteps))
#########################################################
     
#########################################################
     D <- factor(1*(weekdays(CompData$PosDate) %in% c("Saturday","Sunday")),labels=c("weekday","weekend"))
     CompData <- tibble(CompData,DayType=D)
     View(CompData)
     WeekMinMeans <- CompData %>% group_by(interval,DayType) %>% summarize(mean=mean(steps))
     par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(3.2,3.2,0.5,0.5))
     with(filter(WeekMinMeans,DayType == "weekday"),plot(interval,mean,type="l",xaxt="n"))
     mtext("Weekdays", side = 3, line = -1, adj = 0.03)
     with(filter(WeekMinMeans,DayType == "weekend"),plot(interval,mean,type="l"))
     mtext("Weekends", side = 3, line = -1, adj = 0.03)
     mtext("Time (24 hr)", side = 1, outer = TRUE,line=2.2) 
     mtext("Average Steps", side = 2, outer = TRUE,line=2.2) 
##########################################################
     
}