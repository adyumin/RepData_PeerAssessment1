# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
rawdata <- read.csv("activity.csv", na.strings = "NA")
```



## What is mean total number of steps taken per day?


```r
stepsbyday <- aggregate(rawdata$steps, by = list(date = rawdata$date), FUN = "sum")
plot(strptime(stepsbyday$date, "%Y-%m-%d"), stepsbyday$x, type = "hist", xlab = "Date", 
    ylab = "Number of Steps")
```

```
## Warning: plot type 'hist' will be truncated to first character
```

```r
title("Total number of steps by day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
meanbyday <- mean(stepsbyday$x, na.rm = TRUE)
medianbyday <- median(stepsbyday$x, na.rm = TRUE)
```


Mean steps count per day: 1.0766 &times; 10<sup>4</sup>  
Median steps count per day: 10765

## What is the average daily activity pattern?



```r
fdata <- subset(rawdata, !is.na(steps), select = c(steps, date, interval))
stepsbyinterval <- aggregate(fdata$steps, by = list(interval = fdata$interval), 
    FUN = "mean")
plot(stepsbyinterval$interval, stepsbyinterval$x, type = "l", xlab = "interval", 
    ylab = "Number of Steps")
title("Average number of steps by 5-minutes interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
maxinteravl <- stepsbyinterval$interval[max(stepsbyinterval$x) == stepsbyinterval$x]
```


Interval with maximum number of steps: 835


```r
nadata <- subset(rawdata, is.na(steps), select = c(steps, date, interval))
misval <- dim(nadata)[1]
maxinteravl <- stepsbyinterval$interval[max(stepsbyinterval$x) == stepsbyinterval$x]
```



## Imputing missing values




```r
nadata <- subset(rawdata, is.na(steps), select = c(date, interval))
misval <- dim(nadata)[1]
```


Number of missing values is 2304

Now we replcaing missing values by mean of 5-minute intervals calculated withowt missing data


```r
tmp <- merge(stepsbyinterval, nadata, by = "interval")
updata <- data.frame(steps = tmp$x, date = tmp$date, interval = tmp$interval)
newdata <- rbind(updata, fdata)
newstepsbyday <- aggregate(newdata$steps, by = list(date = newdata$date), FUN = "sum")
plot(strptime(newstepsbyday$date, "%Y-%m-%d"), newstepsbyday$x, type = "hist", 
    xlab = "Date", ylab = "Number of Steps")
```

```
## Warning: plot type 'hist' will be truncated to first character
```

```r
title("Total number of steps by day with recovered missing data")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
newmeanbyday <- mean(newstepsbyday$x)
newmedianbyday <- median(newstepsbyday$x)
```


Mean steps count per day after recover missing values: 1.0766 &times; 10<sup>4</sup> (difference is 0)  
Median steps count per day after recover missing values: 1.0766 &times; 10<sup>4</sup> (difference is 1.1887)


## Are there differences in activity patterns between weekdays and weekends?


```r
wdata <- data.frame(steps = newdata$steps, date = newdata$date, interval = newdata$interval, 
    weekend = if (as.POSIXlt(strptime(newdata$date, "%Y-%m-%d"))$wday == 0) factor("weekend", 
        levels = c("weekend", "weekday")) else factor("weekday", levels = c("weekend", 
        "weekday")))
```

```
## Warning: the condition has length > 1 and only the first element will be
## used
```
