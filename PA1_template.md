# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", header = TRUE)
dataNotNA <- data[!is.na(data$steps), ]
dataNA <- data[is.na(data$steps), ]
```


## What is mean total number of steps taken per day?

```r
dataSumByDate <- aggregate(x = dataNotNA$steps, by = list(Date = dataNotNA$date), 
    FUN = sum)
names(dataSumByDate) <- c("date", "steps")

hist(dataSumByDate$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(dataSumByDate$steps)
```

```
## [1] 10766
```

```r
median(dataSumByDate$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
dataAvgByInterval <- aggregate(x = dataNotNA$steps, by = list(Interval = dataNotNA$interval), 
    FUN = mean)
names(dataAvgByInterval) <- c("interval", "steps")

plot(dataAvgByInterval$interval, dataAvgByInterval$steps, type = "l", main = "Average Daily Pattern", 
    xlab = "Time", ylab = "# of Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r

dataAvgByIntervalSorted <- dataAvgByInterval[order(dataAvgByInterval$steps, 
    dataAvgByInterval$interval), ]

MaximumInterval <- tail(dataAvgByIntervalSorted, n = 1)[[1]]
```

## Imputing missing values

```r

dataNA <- data[is.na(data$steps), ]

TotalNumberOfMissingValues <- nrow(dataNA)
```

#Strategy will be based on using the average 

```r
dataNA$steps <- 1
tail(dataNA)
```

```
##       steps       date interval
## 17563     1 2012-11-30     2330
## 17564     1 2012-11-30     2335
## 17565     1 2012-11-30     2340
## 17566     1 2012-11-30     2345
## 17567     1 2012-11-30     2350
## 17568     1 2012-11-30     2355
```

```r
for (i in 1:nrow(dataNA)) {
    dataNA[i, 1] = dataAvgByInterval[dataAvgByInterval$interval == dataNA[i, 
        3], 2]
}

dataFilledIn <- rbind(dataNotNA, dataNA)

dataSumByDateFI <- aggregate(x = dataFilledIn$steps, by = list(Date = dataFilledIn$date), 
    FUN = sum)
names(dataSumByDateFI) <- c("date", "steps")

hist(dataSumByDateFI$steps)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
mean(dataSumByDateFI$steps)
```

```
## [1] 10766
```

```r
median(dataSumByDateFI$steps)
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?

```r

dataFilledIn$DayOfWeek <- "TBD"
dataFilledIn$IsWeekend <- "weekday"
for (i in 1:nrow(dataFilledIn)) {
    dayofweek <- weekdays(strptime(dataFilledIn[i, 2], "%Y-%m-%d"))
    dataFilledIn[i, 4] <- dayofweek
    if (dayofweek %in% c("Saturday", "Sunday")) {
        dataFilledIn[i, 5] <- "weekend"
    }
}

weekends <- dataFilledIn[dataFilledIn$IsWeekend == "weekend", ]
weekdays <- dataFilledIn[dataFilledIn$IsWeekend == "weekday", ]

```

