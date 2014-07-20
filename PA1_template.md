# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Read in activity.csv from the working directory, and define the class of each column


```r
classes <- c(steps = "integer", date = "Date", interval = "integer")
# read in data frame with 17568 observations
activity <- read.csv("activity.csv", colClasses = classes)
```



## What is mean total number of steps taken per day?


```r
library(reshape2)
activitymelt <- melt(activity, id = c("date", "interval"), na.rm = TRUE)
# sumbyday is the sum total of steps taken per day
sumbyday <- dcast(activitymelt, date ~ variable, sum)
# meanbyday is the mean number of steps taken each day
meanbyday <- dcast(activitymelt, date ~ variable, mean)
```



```r
hist(sumbyday$steps/1000, xlab = "Total number of steps by day in thousands", 
    main = NULL, breaks = 15)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 



```r

stepstats <- c(mean = mean(sumbyday$steps), median = median(sumbyday$steps))
stepstats
```

```
##   mean median 
##  10766  10765
```




## What is the average daily activity pattern?

```r
averageoverdays <- dcast(activitymelt, interval ~ variable, mean)

with(averageoverdays, plot(interval, steps, type = "l"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r

averageoverdays[averageoverdays$steps %in% max(averageoverdays$steps), ]
```

```
##     interval steps
## 104      835 206.2
```

The time between 8:35 and 8:40 contains the maximum average of 206 steps.

## Imputing missing values
I am going to average the number of steps from that interval of the surrounding days
There are 8 days of missing data 288 observation/day *8 = 2304
The dates are 
- 2012-10-01
- 2012-10-08
- 2012-11-01
- 2012-11-04
- 2012-11-09
- 2012-11-10
- 2012-11-14
- 2012-11-30


```r
dates <- is.na(activity$steps)
avgdates <- activity$date[!dates]
dates <- activity$date[dates]
dates <- unique(dates)
avgdates <- unique(avgdates)
intervals <- unique(activity$interval)

useavgdate <- activity$date %in% avgdates
useactivity <- activity[useavgdate, ]
activityavg <- melt(useactivity, id = c("date", "interval"), na.rm = TRUE)
averagetouse <- dcast(activityavg, interval ~ variable, mean)

update <- activity$date %in% dates

# activity$interval %in% intervals activity$steps == averagetouse$steps for
# (i in seq_along(intervals)){



for (i in seq_along(dates)) {
    # print(dates[i])
    count = 0
    if ((dates[i] - 1) %in% activity$date) {
        # print ((dates[i]-1) %in% activity$date)
        count = count - 1
        # if (activity$steps[activity$date==dates[i]-1]==NA){count = -10}
    }
    if ((dates[i] + 1) %in% activity$date) {
        # print ((dates[i]+1) %in% activity$date)
        count = count + 1
        if (NA %in% activity$steps[activity$date == dates[i] + 1]) {
            count = +20
        }
    }
    # print (count)
    rolling = 1
    for (j in seq_along(intervals)) {
        
        if (count == -1) {
            activity$steps[activity$date == dates[i] & activity$interval == 
                intervals[j]] <- activity$steps[activity$date == dates[i] - 
                1 & activity$interval == intervals[j]]
        }
        if (count == 1) {
            activity$steps[activity$date == dates[i] & activity$interval == 
                intervals[j]] <- activity$steps[activity$date == dates[i] + 
                1 & activity$interval == intervals[j]]
        }
        if (count == 0) {
            
            activity$steps[activity$date == dates[i] & activity$interval == 
                intervals[j]] <- mean(activity$steps[activity$date == dates[i] + 
                1 & activity$interval == intervals[j]], activity$steps[activity$date == 
                dates[i] - 1 & activity$interval == intervals[j]])
            # rolling =rolling+1 print (rolling)
        }
        if (count == 20) {
            
            activity$steps[activity$date == dates[i] & activity$interval == 
                intervals[j]] <- mean(activity$steps[activity$date == dates[i] + 
                2 & activity$interval == intervals[j]], activity$steps[activity$date == 
                dates[i] - 1 & activity$interval == intervals[j]])
            rolling = rolling + 1
            # print (rolling)
        }
    }
}

```



## Are there differences in activity patterns between weekdays and weekends?

```r
library("ggplot2")
activity$weekday <- weekdays(activity$date)
activity$weekday <- as.factor(activity$weekday)
weekend <- grep("S.+", activity$weekday)
# weekend
activitymelt <- melt(activity, id = c("date", "interval", "weekday"), na.rm = TRUE)
# sumbyday is the sum total of steps taken per day
sumbyday <- dcast(activitymelt, date ~ variable, sum)


qplot(interval, steps, data = activity, facets = . ~ weekday, rm.na = TRUE, 
    geom = "line")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-71.png) 

```r
qplot(interval, steps, data = averageoverdays, geom = c("point", "line"))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-72.png) 

```r
par(mfrow = c(2, 1))
with(averageoverdays, plot(interval, steps, type = "l"))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-73.png) 

Any reference to a commit’s SHA-1 hash will be automatically converted into a link to that commit on GitHub.