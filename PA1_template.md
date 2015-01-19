# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

### The process is as follows:

* Set working directory

* Unzip activity.zip


```r
getwd()
```

```
## [1] "/Users/cemalperozen/Documents/Repos/Coursera/RepData_PeerAssessment1"
```

```r
setwd("~/Documents/Repos/Coursera/RepData_PeerAssessment1/")
list.files()
```

```
## [1] "activity.csv"       "activity.zip"       "doc"               
## [4] "instructions_fig"   "PA1_template_files" "PA1_template.html" 
## [7] "PA1_template.md"    "PA1_template.Rmd"   "README.md"
```

```r
unzip("./activity.zip", exdir = "./")
list.files()
```

```
## [1] "activity.csv"       "activity.zip"       "doc"               
## [4] "instructions_fig"   "PA1_template_files" "PA1_template.html" 
## [7] "PA1_template.md"    "PA1_template.Rmd"   "README.md"
```


* Read csv file


```r
data <- read.csv(file = "./activity.csv", header = TRUE)
head(data)
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
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?


```r
library(plyr)
dailySteps <- ddply(data, c("date"), summarise, totSteps = sum(steps, na.rm = TRUE))
head(dailySteps)
```

```
##         date totSteps
## 1 2012-10-01        0
## 2 2012-10-02      126
## 3 2012-10-03    11352
## 4 2012-10-04    12116
## 5 2012-10-05    13294
## 6 2012-10-06    15420
```

```r
hist(x = dailySteps$totSteps, xlab = "Total Steps per Day", main = "Historgram of Daily Total Steps")
```

![](./PA1_template_files/figure-html/MTSPD-1.png) 

```r
meanSteps <- format(x = mean(dailySteps$totSteps), digits = 6)
medianSteps <- median(dailySteps$totSteps)
```

The mean of the total daily steps is 9354.23 and the median is 10395.

## What is the average daily activity pattern?


```r
actPattern <- ddply(data, c("interval"), summarise, avgSteps = mean(steps, na.rm = TRUE))
head(actPattern)
```

```
##   interval  avgSteps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
maxActivty <- actPattern[actPattern$avgSteps == max(actPattern$avgSteps), ]
plot(actPattern$interval, actPattern$avgSteps, 
     xlab = "Time of Day (h)", 
     ylab = "Avg # of Steps Taken Across All Days", 
     type = "l",
     axes = FALSE)
axis(side = 1, at = seq(from = 0, to = 2400, by = 200), labels = formatC(seq(0, 24, 2)))
axis(side = 2)
box()
```

![](./PA1_template_files/figure-html/ADAP-1.png) 

Maximum average activity happened in interval 835 and it was 206.1698113.

## Imputing missing values


```r
???????????????????????
numMissing <- length(data[is.na(data$steps),]$steps)
```

```
## Contacting Delphi...the oracle is unavailable.
## We apologize for any inconvenience.
```

```r
dailyAvgSteps <- ddply(data, c("date"), mutate, avgSteps = ave(steps, na.rm = TRUE, FUN=mean))
dailyAvgSteps[is.na(dailyAvgSteps$avgSteps),]$avgSteps <- 0.0
#head(dailyAvgSteps,500)
dataNew <- data
dataNew$steps <- as.numeric(dataNew$steps)
for (i in 1:length(dataNew$steps)) {
    if (is.na(dataNew[i,]$steps)) {
        dataNew[i,]$steps <- as.numeric(dailyAvgSteps[i,]$avgSteps)
    }
}

#head(data,500)

newdailySteps <- ddply(dataNew, c("date"), summarise, totSteps = sum(steps))
head(newdailySteps)
```

```
##         date totSteps
## 1 2012-10-01        0
## 2 2012-10-02      126
## 3 2012-10-03    11352
## 4 2012-10-04    12116
## 5 2012-10-05    13294
## 6 2012-10-06    15420
```

```r
hist(x = newdailySteps$totSteps, xlab = "Total Steps per Day", main = "Historgram of Daily Total Steps")
```

![](./PA1_template_files/figure-html/missingValue-1.png) 

```r
newmeanSteps <- format(x = mean(newdailySteps$totSteps), digits = 6)
newmedianSteps <- median(newdailySteps$totSteps)
```

## Are there differences in activity patterns between weekdays and weekends?

```r
weekdata <- ddply(data, c("date"), mutate,
            week = ifelse((weekdays(as.POSIXlt(date)) == "Saturday" | weekdays(as.POSIXlt(date)) == "Sunday"), "weekend", "weekday"))

weekdata$week <- as.factor(weekdata$week)

weekPattern <- ddply(weekdata, c("interval", "week"), summarise, avgSteps = mean(steps, na.rm = TRUE))
#head(weekPattern,1000)
library(lattice)
xyplot(weekPattern$avgSteps ~ weekPattern$interval | weekPattern$week, 
        layout = c(1,2), xlab = "Interval", ylab = "Number of steps", type = "l")
```

![](./PA1_template_files/figure-html/weekend-1.png) 

