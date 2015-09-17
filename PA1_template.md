---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
setwd("C:\\MyFolder\\DataScience\\MyRWork\\5ReproducibleResearch\\Week2\\Project")

# #Download the zip file from the internet
zipfile <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(zipfile,destfile = "personalActivity.zip",mode="wb")
# 
# #Unzip the file
unzip("personalActivity.zip")

#Load the data
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
#Ignore the NAs...
activityWithoutNA <- activity[complete.cases(activity),]
answerSum <- as.data.frame(tapply(activityWithoutNA$steps, activityWithoutNA$date, sum))
colnames(answerSum)[1] <- "steps"
answerSum[is.na(answerSum)] <- 0

#Histogram of total number of steps taken per day
hist(answerSum$steps
     , main="Total number of steps taken per day"
     , xlab="Steps Taken"
     )
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
#mean, median calculation
meanSum <- mean(answerSum$steps)
medianSum <- median(answerSum$steps)
```

### Total number of steps taken per day:
* Mean Value: 9354.2295082.
* Median Value: 1.0395 &times; 10<sup>4</sup>.

## What is the average daily activity pattern?


```r
#Find the mean
answerMean <- as.data.frame(tapply(activityWithoutNA$steps, activityWithoutNA$interval, mean))
colnames(answerMean)[1] <- "steps"
answerMean[is.na(answerMean)] <- 0

#Histogram of average number of steps taken per day
plot(x=answerMean$steps
     , type="l"
     , main="Daily Activity Pattern"
     , xlab="Interval"
     , ylab="Total Steps"
     )
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
#To find the 5-minute interval containing the maximum number of steps
maxMean <- max(answerMean$steps)
answerMean$interval <- rownames(answerMean)
highInterval <- answerMean[answerMean$steps == maxMean,2]
```

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
* 5-minute interval : 835

## Inputing missing values


```r
# 1.  Calculate and report the total number of missing values in the 
# dataset (i.e. the total number of rows with NAs)

totalMissingValues <- sum(is.na(activity$steps)) 

#2.  Devise a strategy for filling in all of the missing values in the 
# dataset. The strategy does not need to be sophisticated. 
#  For example, you could use the mean/median for that day, or the mean 
#  for that 5-minute interval, etc.

#We are using the mean for that 5-minute interval...
newDataSet <- merge(activity,answerMean,by.x="interval",by.y="row.names")
```

```
## Warning in merge.data.frame(activity, answerMean, by.x = "interval", by.y =
## "row.names"): column name 'interval' is duplicated in the result
```

```r
newDataSet$steps.x <- 
  ifelse((newDataSet$steps.x == 0 | is.na(newDataSet$steps.x))
         , newDataSet$steps.y, newDataSet$steps.x)

sumOfMean <- as.data.frame(tapply(newDataSet$steps.x, newDataSet$date, sum))
colnames(sumOfMean)[1] <- "steps"

#Histogram of total number of steps taken per day
hist(sumOfMean$steps
     , main="Total number of steps per day"
     , xlab="Steps"
     , ylab="Frequency")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
#mean and median values
newMean <- mean(sumOfMean$steps)
newMedian <- median(sumOfMean$steps)
```

The total number of missing values in the original dataset is 2304

The new Mean and Median values can be found below:
* Mean : 1.5875987 &times; 10<sup>4</sup>
* Median: 1.5837736 &times; 10<sup>4</sup>

As you can see, the new values, obtained after subsituting the "mean for the five
  minute interval" for missing values in the original dataset are  higher than the
  ones calculated by ignoring missing values from the dataset.


## Are there differences in activity patterns between weekdays and weekends?


```r
# 1.  Create a new factor variable in the dataset with two levels - 
# "weekday" and "weekend" indicating whether a given date is a weekday 
# or weekend day.

newDataSet$dayCategory <- 
  factor(ifelse((weekdays(as.Date(newDataSet$date)) == "Sunday" 
                   | weekdays(as.Date(newDataSet$date)) == "Saturday")
          ,"weekend", "weekday"))


# 2.  Make a panel plot containing a time series plot (i.e. type = "l") 
# of the 5-minute interval (x-axis) and the average number of steps 
# taken, averaged across all weekday days or weekend days (y-axis). 

resAgg <- aggregate(steps.x ~ dayCategory + interval, data = newDataSet, mean)
colnames(resAgg)[3] <- "Steps"

library(lattice)
xyplot(  Steps ~ interval | dayCategory,
         data = resAgg,
         type="l",
         y.lab=list(label="Steps"),
         layout=c(1,2)
      )
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
 
