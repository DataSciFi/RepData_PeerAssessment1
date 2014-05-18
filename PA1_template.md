# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
setwd("/home/antony/Dropbox/Courses/1405_ReproducibleResearch/project01/RepData_PeerAssessment1")
activity <- read.csv("activity.csv", header = TRUE)
summary(activity)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```


## What is mean total number of steps taken per day?

Prepare daily steps number subset

```r
steps.daily <- sapply(unique(activity$date), function(x) sum(activity$steps[activity$date == 
    x], na.rm = TRUE))
```

Total number of steps taken daily histogramm

```r
hist(steps.daily, main = "Total number of steps taken daily", xlab = "Steps number")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

The mean and the median of the total number of steps taken daily

```r
mean(steps.daily, na.rm = TRUE)
```

```
## [1] 9354
```

```r
median(steps.daily, na.rm = TRUE)
```

```
## [1] 10395
```



## What is the average daily activity pattern?
Prepare interval data subset

```r
intervals <- unique(activity$interval)
steps.by.interval <- sapply(intervals, function(x) mean(activity$steps[activity$interval == 
    x], na.rm = TRUE))
```


A time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(intervals, steps.by.interval, type = "l", main = "Average steps number by 5-minute interval", 
    xlab = "Interval ID", ylab = "Average steps number")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervals[which.max(steps.by.interval)]
```

```
## [1] 835
```


## Imputing missing values

The total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```


The strategy for filling in all of the missing values in the dataset is to use the mean that 5-minute interval. New data set __activity.complete__

```r
activity.complete <- activity
interval.means <- data.frame(interval = intervals, stepmean = steps.by.interval)
narows <- attr(activity.complete[!complete.cases(activity.complete), ], "row.names")
for (idRow in narows) {
    activity.complete[idRow, "steps"] <- interval.means[interval.means$interval == 
        activity.complete[idRow, "interval"], "stepmean"]
}
```



A histogram of the total number of steps taken each day, the mean and median. These values do differ from the estimates from the first part of the assignment. After imputing missing data the distribution of estimates is looking more normal.

```r
steps.daily.complete <- sapply(unique(activity.complete$date), function(x) sum(activity.complete$steps[activity.complete$date == 
    x]))
hist(steps.daily.complete, main = "Total number of steps taken daily", xlab = "Steps number")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
mean(steps.daily.complete)
```

```
## [1] 10766
```

```r
median(steps.daily.complete)
```

```
## [1] 10766
```



## Are there differences in activity patterns between weekdays and weekends?
Let's create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. The updated data frame is __activity.complete.daytype__

```r
mapdays <- data.frame(dname = c("Monday", "Tuesday", "Wednesday", "Thursday", 
    "Friday", "Saturday", "Sunday"), dtype = append(rep("weekday", 5), rep("weekend", 
    2)))
daytype <- sapply(weekdays(as.POSIXlt(activity.complete$date, tz = "GMT")), 
    function(x) mapdays$dtype[mapdays$dname == x])
activity.complete.daytype <- cbind(activity.complete, daytype)
```


A panel plot below contains a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
steps.by.interval.weekends <- sapply(intervals, function(x) mean(activity.complete.daytype$steps[activity.complete.daytype$interval == 
    x & activity.complete.daytype$daytype == "weekend"]))
steps.by.interval.weekdays <- sapply(intervals, function(x) mean(activity.complete.daytype$steps[activity.complete.daytype$interval == 
    x & activity.complete.daytype$daytype == "weekday"]))
library(lattice)
par(mfrow = c(2, 1))
plot(intervals, steps.by.interval.weekends, type = "l", main = "Average steps by 5-minute interval on weekends", 
    xlab = "Interval ID", ylab = "Average steps number")
plot(intervals, steps.by.interval.weekdays, type = "l", main = "Average steps by 5-minute interval on weekdays", 
    xlab = "Interval ID", ylab = "Average steps number")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 



