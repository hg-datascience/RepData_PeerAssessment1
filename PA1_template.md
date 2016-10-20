# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

### Loading and preprocessing the data


```r
data <- read.csv("activity.csv", colClasses = c("numeric", 
                                                "Date", 
                                                "numeric"))
```

### Helper filter


```r
filterNaSteps <- is.na(data$steps)
```

## What is mean total number of steps taken per day?

### Total number of steps taken per day


```r
daySteps <- aggregate(data$steps[!filterNaSteps], 
                      by = list(date = data$date[!filterNaSteps]), 
                      FUN = sum)
```

### Histogram of the total number of steps taken each day

```r
hist(
  daySteps$x,
  main = "Total number of steps taken each day",
  xlab = "Steps"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Mean and median of the total number of steps taken per day

```r
dayStepsMean <- mean(daySteps$x, na.rm = TRUE)
dayStepsMean
```

```
## [1] 10766.19
```


```r
dayStepsMedian <- median(daySteps$x, na.rm = TRUE)
dayStepsMedian
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days 


```r
stepsPerInterval <- aggregate(data$steps[!filterNaSteps],
                              by = list(interval = data$interval[!filterNaSteps]),
                              FUN = mean)
                              
plot(x = stepsPerInterval$interval,
     y = stepsPerInterval$x,
     type = "l",
     main = "Aaverage daily activity pattern?",
     xlab = "Interval",
     ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsPerInterval$interval[which.max(stepsPerInterval$x)]
```

```
## [1] 835
```

## Imputing missing values

### Total number of missing values in the dataset


```r
sum(filterNaSteps)
```

```
## [1] 2304
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in (devised strategy: mean for that 5-minute interval)


```r
library(plyr)

dataWithIntervalMean <- arrange(merge(data,  
                                      stepsPerInterval,
                                      by = "interval"),
                                date,
                                interval)
dataNonNA <- data 

dataNonNA$steps[filterNaSteps] <- dataWithIntervalMean$x[filterNaSteps]
```

### Histogram of the total number of steps taken each day


```r
dayStepsNonNA <- aggregate(dataNonNA$steps, 
                           by = list(dataNonNA = dataNonNA$date), 
                           FUN = sum)

hist(
  dayStepsNonNA$x,
  main = "Total number of steps taken each day",
  xlab = "Steps"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

### Mean and median total number of steps taken per day


```r
dayStepsNonNAMean <- mean(dayStepsNonNA$x)
dayStepsNonNAMean
```

```
## [1] 10766.19
```


```r
dayStepsNonNAMedian <- median(dayStepsNonNA$x)
dayStepsNonNAMedian
```

```
## [1] 10766.19
```

### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


Before imputing missing data the mean was 10766.19 and the median was 10765.00.

After imputing missing the mean was 10766.19 and the median was 10766.19.

There is not impact of imputing missing data on the mean value, but the median changes moving towards the mean value.


## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
isWeekend <- function(date) {
  isWeekend <- as.POSIXlt(date)$wday %in% c(0, 6)
  isWeekend
}

dataNonNA <- cbind(dataNonNA, day = apply(data, 1, 
                                          function(r) {
                                            if(isWeekend(r['date']))
                                              "weekend"
                                            else
                                              "weekday"
                                          }))

dayStepsWeekdays <- aggregate(dataNonNA$steps, 
                              by = list(interval = dataNonNA$interval, 
                                        day = dataNonNA$day),
                              FUN = mean)
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(lattice)
xyplot(x ~ interval | day, data = dayStepsWeekdays, type = "l", layout = c(1, 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

