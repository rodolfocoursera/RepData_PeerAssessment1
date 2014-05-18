# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
### The script is executed on the same directory of activity.zip is stored

```r
unzip(zipfile = "activity.zip")

activity <- read.csv(file = "activity.csv")
dim(activity)
```

```
## [1] 17568     3
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
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


### Creates a new dataframe excluding NA values for the variable "steps"

```r
activity.notNA <- activity[!is.na(activity$steps), ]
dim(activity.notNA)
```

```
## [1] 15264     3
```

```r
summary(activity.notNA)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-02:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-03:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-04:  288   Median :1178  
##  Mean   : 37.4   2012-10-05:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-06:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-07:  288   Max.   :2355  
##                  (Other)   :13536
```


## What is mean total number of steps taken per day?
### Totalize the number of steps per day

```r
require(data.table)
```

```
## Loading required package: data.table
```

```r
steps.dt <- data.table(activity.notNA, key = "date")
total.steps <- steps.dt[, list(steps = sum(steps)), by = date]
```


### Make a histogram of the total number of steps taken each day

```r
hist(total.steps$steps, main = "Total number of steps taken each day", xlab = "Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


### Calculates and prints mean and median of the total number of steps taken per day

```r
print(paste("Steps. Mean:", mean(total.steps$steps)))
```

```
## [1] "Steps. Mean: 10766.1886792453"
```

```r
print(paste("Steps. Median:", median(total.steps$steps)))
```

```
## [1] "Steps. Median: 10765"
```


## What is the average daily activity pattern?
### Averages the number of steps per 5-minute interval

```r
steps.dt.av <- data.table(activity.notNA, key = "interval")
average.steps <- steps.dt.av[, list(steps = mean(steps)), by = interval]
```


### Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
plot(x = average.steps$interval, y = average.steps$steps, type = "l", , xlab = "Steps. Average", 
    ylab = "Interval", main = "Average number of steps taken each 5-minute interval")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
average.steps[average.steps$steps == max(average.steps$steps), ]
```

```
##    interval steps
## 1:      835 206.2
```


## Imputing missing values
### Calculates and reports the total number of missing values in the dataset

```r
print(paste("Number of NAs in the dataset:", sum(is.na(activity))))
```

```
## [1] "Number of NAs in the dataset: 2304"
```


### Filling in all of the missing values in the dataset with the mean for that 5-minute interval

```r
# Saving index of NAs
nas.index <- activity[which(is.na(activity$steps)), ]

# Looping over NAs and getting the mean value
steps.meanValues <- by(activity[is.na(activity$steps), ], 1:nrow(nas.index), 
    simplify = T, function(row) average.steps[average.steps$interval == row$interval, 
        steps])
```


### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# Cloning original dataframe
activityNew <- activity

# Setting NA steps with the mean
activityNew[which(is.na(activityNew$steps)), 1] <- steps.meanValues

print(paste("Number of NAs in the new dataset:", sum(is.na(activityNew))))
```

```
## [1] "Number of NAs in the new dataset: 0"
```


### Make a histogram of the total number of steps taken each day 

```r
steps.dt.new <- data.table(activityNew, key = "date")
total.steps.new <- steps.dt.new[, list(steps = sum(steps)), by = date]
hist(total.steps.new$steps, main = "Total number of steps taken each day", xlab = "Steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


### Calculate and report the mean and median total number of steps taken per day.

```r
print(paste("Steps. Mean:", mean(total.steps.new$steps)))
```

```
## [1] "Steps. Mean: 10766.1886792453"
```

```r
print(paste("Steps. Median:", median(total.steps.new$steps)))
```

```
## [1] "Steps. Median: 10766.1886792453"
```


### Do these values differ from the estimates from the first part of the assignment? 

```r
print(paste("Difference in the mean:", mean(total.steps.new$steps) - mean(total.steps$steps)))
```

```
## [1] "Difference in the mean: 0"
```

```r
print(paste("Difference in the median:", median(total.steps.new$steps) - median(total.steps$steps)))
```

```
## [1] "Difference in the median: 1.1886792452824"
```


### What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
sum.difference <- sum(total.steps.new$steps) - sum(total.steps$steps)
print(paste("Difference in the sum of steps:", sum.difference))
```

```
## [1] "Difference in the sum of steps: 86129.5094339623"
```

```r
print(paste("% impact:", sum.difference/dim(activity)[1]))
```

```
## [1] "% impact: 4.90263601058528"
```


### Are there differences in activity patterns between weekdays and weekends?
#### Creating a new factor variable in the dataset with two levels – “weekday” and “weekend”

```r
# In Spanish: domingo=sunday, sábado=saturday
week.factor <- weekdays(as.Date(activityNew$date), abbreviate = T) == "dom" | 
    weekdays(as.Date(activityNew$date), abbreviate = T) == "sáb"
week.factor <- factor(week.factor, labels = c("weekday", "weekend"))

activityNew <- cbind(activityNew, week.factor)
head(activityNew)
```

```
##     steps       date interval week.factor
## 1 1.71698 2012-10-01        0     weekday
## 2 0.33962 2012-10-01        5     weekday
## 3 0.13208 2012-10-01       10     weekday
## 4 0.15094 2012-10-01       15     weekday
## 5 0.07547 2012-10-01       20     weekday
## 6 2.09434 2012-10-01       25     weekday
```


#### Averaged across all weekday days or weekend days for the steps variable

```r
average.steps.weekday <- aggregate(data = activityNew[week.factor == "weekday", 
    ], steps ~ interval, FUN = "mean")
average.steps.weekend <- aggregate(data = activityNew[week.factor == "weekend", 
    ], steps ~ interval, FUN = "mean")
```


#### Panel plot containing a time series plot comparing average of steps 

```r
par(mfrow = c(2, 1))
plot(x = average.steps.weekday$interval, y = average.steps.weekday$steps, type = "l", 
    , ylab = "Steps", xlab = "Interval", main = "Average of steps. Weekdays")
plot(x = average.steps.weekend$interval, y = average.steps.weekend$steps, type = "l", 
    , ylab = "Steps", xlab = "Interval", main = "Average of steps. Weekends")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18.png) 



