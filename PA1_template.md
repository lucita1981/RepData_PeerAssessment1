---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
if(!file.exists("activity.csv")){
    unzip("activity.zip")
}
Data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
Total <- tapply(Data$steps, Data$date, FUN=sum, na.rm=TRUE)
```

##### 1. Make a histogram of the total number of steps taken each day

```r
hist(Total, col="red", main="Total Number of Steps per Day", xlab="Steps")
```

![](PA1_template_files/figure-html/hist1-1.png)<!-- -->

##### 2. Calculate and report the mean and median total number of steps taken per day

```r
mean(Total)
```

```
## [1] 9354.23
```

```r
median(Total)
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
Interval <- aggregate(steps ~ interval, Data, mean)
```

##### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
plot(Interval$interval,Interval$steps, type="l", xlab="5-minute interval", ylab=" Average number of steps", main="Average Number of Steps per Day by Interval", col="blue")
```

![](PA1_template_files/figure-html/plot2-1.png)<!-- -->

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
Interval[which.max(Interval$steps), 1]
```

```
## [1] 835
```

## Imputing missing values

##### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
MissingValues <- is.na(Data$steps)
sum(MissingValues)
```

```
## [1] 2304
```

##### 2. Devise a strategy for filling in all of the missing values in the dataset. & Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
library(Hmisc)
```

```r
newData <- Data
newData$steps <- impute(Data$steps, fun=mean)
```

##### 4. Make a histogram of the total number of steps taken each day


```r
newTotal <- tapply(newData$steps, newData$date, FUN=sum)
hist(newTotal, main = "Total Number of Steps per Day (Imputed)", col="orange", xlab="Steps")
```

![](PA1_template_files/figure-html/hist3-1.png)<!-- -->

##### 4. (Cont.) Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 


```r
mean(newTotal)
```

```
## [1] 10766.19
```

```r
median(newTotal)
```

```
## [1] 10766.19
```

##### 4. (Cont.) What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sum(newData$steps)-sum(Data$steps,na.rm = TRUE)
```

```
## [1] 86129.51
```

## Are there differences in activity patterns between weekdays and weekends?

##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
newData$TypeDay <-  ifelse(as.POSIXlt(newData$date)$wday %in% c(0,6), "weekend", "weekday")
```

##### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(ggplot2)
```

```r
averageperTypeDay <- aggregate(steps ~ interval + TypeDay, data=newData, mean)
ggplot(averageperTypeDay, aes(interval, steps, col=TypeDay)) + 
    geom_line() + 
    facet_grid(TypeDay ~ .) +
    labs(title="Average Step per Day Type", xlab = "Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/plot4-1.png)<!-- -->
