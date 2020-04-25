---
title: "Reproducible Research"
author: "Shaopeng Li"
date: "4/25/2020"
output: html_document
---



# Loading and preprocessing the data
1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
unzip('activity.zip')
tempset <- read.csv('activity.csv')
tempset$date <- as.Date(tempset$date,'%Y-%m-%d')
```

# What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day


```r
totalstep <- aggregate(tempset$steps,by=list(tempset$date),sum)
library(ggplot2)
ggplot(totalstep, aes(x=Group.1, y=x))+labs(x="Date",y="Steps")+
  geom_bar(stat = "identity")+
  labs(title = "Total number of steps each day")+theme(plot.title = element_text(hjust = 0.5))
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![plot of chunk step2](figure/step2-1.png)

```r
options(scipen=1, digits=2)
stepmean <- mean(totalstep$x,na.rm = TRUE)
stepmedian <- median(totalstep$x,na.rm = TRUE)
```
The mean of the total steps each day is 10766.19.  
The median of the total steps each day is 10765. 

# What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
meanstep <- aggregate(tempset$steps,by=list(tempset$interval),mean,na.rm=TRUE)
colnames(meanstep) <- c("interval","steps")
plot(meanstep$interval,meanstep$steps,type="l",xlab = "Interval",ylab = "Avg. steps per day")
```

![plot of chunk step3](figure/step3-1.png)

```r
maxstep <- meanstep[which(meanstep$steps==max(meanstep$steps)),1]
```
The 835 (5-minute) has the maximum number of steps, on avegrage across all the days in the dataset.

# Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(tempset))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
fillinnull <- function(dataset,meanvalue){
for (x in 1:length(dataset$steps)) {
  if (is.na(dataset$steps[x])){
  dataset[x,1] <- meanvalue[which(meanvalue$interval==dataset[x,3]),2]
  }
}
  dataset
}
newdateset<- fillinnull(tempset,meanstep)
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalstepNew <- aggregate(newdateset$steps,by=list(newdateset$date),sum)
library(ggplot2)
ggplot(totalstepNew, aes(x=Group.1, y=x))+labs(x="Date",y="Steps")+
  geom_bar(stat = "identity")+
  labs(title = "Total number of steps each day")+theme(plot.title = element_text(hjust = 0.5))
```

![plot of chunk step4.3](figure/step4.3-1.png)

```r
options(scipen=1, digits=2)
stepmeanNew <- mean(totalstepNew$x)
stepmedianNew <- median(totalstepNew$x)
```
Type of Estimate | Mean_Steps | Median_Steps
--- | --- | ---
First Part (with na) | 10766.19 | 10765
Second Part (fillin in na with mean) |  10766.19  | 10766.19
 
# Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
newdateset$weekdays <- weekdays(newdateset$date)
newdateset[grepl(newdateset$weekdays ,pattern = "Monday|Tuesday|Wednesday|Thursday|Friday"),"weekday or weekend"] <-"weekday"
newdateset[grepl(newdateset$weekdays ,pattern = "Saturday|Sunday"),"weekday or weekend"] <-"weekend"
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
meanstep5 <- aggregate(newdateset$steps,by=list(newdateset$interval,newdateset$`weekday or weekend`),mean,na.rm=TRUE)
colnames(meanstep5) <- c("interval","weekday or weekend","steps")

ggplot(meanstep5 , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weekday/Weekend", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2) +theme(plot.title = element_text(hjust = 0.5))
```

![plot of chunk step5.2](figure/step5.2-1.png)
