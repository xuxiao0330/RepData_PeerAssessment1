# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

unzip("activity.zip")
data <- read.csv("activity.csv")
data$date=as.Date(data$date,'%Y-%m-%d')

## What is mean total number of steps taken per day?
## making histogram
totalsteps <- aggregate(data$steps,list(data$date),FUN = "sum", na.rm = TRUE)
png("plot1.png")
hist(totalsteps$x,main="Histogram of Total Steps by Day",ylab='Days', xlab="Steps")
dev.off()
## calculating mean and median
mean(totalsteps$x)
median(totalsteps$x)

## What is the average daily activity pattern?
##Making a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avg5mins = aggregate(data$steps, list(data$interval),FUN = "mean",na.rm = TRUE)
png("plot2.png")
plot(avg5mins$x,avg5mins$interveral, type ='l',main= "Activity Daily Pattern", xlab="interval", ylab = "Steps")
dev.off()
##Showing the 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

avg5mins$Group.1[which.max(avg5mins$x)]

## Imputing missing values
##Calculate and report the total number of missing values in the dataset
sum(is.na(data$steps))
##Filling missing value by 5-min interval
filling <- merge(data,avg5mins, by.x="interval", by.y="Group.1")
filling$steps[is.na(filling$steps)]<-filling$x

##Creating new dataset
datanew <- cbind(filling$steps, data[2:3])
colnames(datanew) <- c('steps','date','interval')

##Making Histograms of Total Number of steps, Mean, Median
totalnewsteps <- aggregate(datanew$steps,list(datanew$date),FUN = "sum")
png("png3.png")
hist(totalnewsteps$x,main="Histogram of Total New Steps by Day",ylab='Days', xlab="Steps")
dev.off()
mean(totalnewsteps$x)
median(totalnewsteps$x)
##Conclusion:
##Using 5-min Average, the mean and median are smaller than previous.

## Are there differences in activity patterns between weekdays and weekends?

##Creating New Vector Weekday or Weekend

datanew$date=as.Date(datanew$date,'%Y-%m-%d')
datanew$days <- (weekdays(datanew$date)=="Saturday"|weekdays(datanew$date)=="Sunday")
datanew$days1 <- ifelse(datanew$days == "TRUE", "Weekends","Weekdays")

# plotting using lattice xyplot
avg5minsnew <- aggregate(steps ~ interval+days1, data=datanew, FUN="mean", na.rm=TRUE)
library(lattice)
png("png4.png")
xyplot(steps ~ interval | days1, data=avg5minsnew, type = "l", layout = c(1, 2), xlab="Interval",ylab="Number of steps")
dev.off()
