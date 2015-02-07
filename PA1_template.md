# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

### The process is as follows:

* Set working directory with setwd() command

* Unzip activity.zip into the same folder using unzip() command


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


* Now we have the CSV file in the folder, we can read it with its headers with read.csv().

* Content of the CSV file is in a data frame called "data". Let's look at the initial rows of data using head() command and call str() function to get and idea of the columns and their type


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


## Q1: What is mean total number of steps taken per day?

* Let's first generate a new data frame after removing the NA values using na.omit() function

* Plyr package is very handy when we are interested in data reduction

* I can get the total number of steps taken per day with ddply() function. Here I apply the summarize function to sum up all the steps at each day. While doing so, I also create a new variable called totSteps. Finally I assign the result to a new data frame called dailySteps.

* Plot the histogram of the total steps taken each day with hist() command. Add x axis text and title

* The last step of this part is to calculate the mean and median of the data. I assign the results to variables so that I can use them in the text below. 



```r
data2 <- na.omit(data)
library(plyr)
dailySteps <- ddply(data2, c("date"), summarise, totSteps = sum(steps, na.rm = TRUE))
head(dailySteps)
```

```
##         date totSteps
## 1 2012-10-02      126
## 2 2012-10-03    11352
## 3 2012-10-04    12116
## 4 2012-10-05    13294
## 5 2012-10-06    15420
## 6 2012-10-07    11015
```

```r
hist(x = dailySteps$totSteps, xlab = "Total Steps per Day", main = "Historgram of Daily Total Steps")
```

![](./PA1_template_files/figure-html/MTSPD-1.png) 

```r
meanSteps <- format(x = mean(dailySteps$totSteps), digits = 6)
medianSteps <- median(dailySteps$totSteps)
```

The mean of the total daily steps is 10766.2 and the median is 10765.

## Q2: What is the average daily activity pattern?

* Now we need to avarage the steps taken across all days in the dat for each interval. We can use ddply again. Now we apply summarise on interval column and generate a new variable called avgSteps with mean steps information. We assign the result to actPattern data frame.

* we assign the maxActivity result to maxActivity variable. 

* Plot interval vs avgSteps with type "l". Convert the x-axis to 24Hr format

* Finally below the code chunk we print the maxActivity interval and the avg steps in that interval. 


```r
actPattern <- ddply(data2, c("interval"), summarise, avgSteps = mean(steps, na.rm = TRUE))
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

## Q3: Imputing missing values

* We can check the missing values with is.na(). If I use is.na() to get the subset of a dataframe, I can then get the size/length of this resulting data frame to get te number of missing values 

* I decided to use the averages calculated for each interval for the missing values. Created a new data frame called dataNew from original data frame "data". Looped through each row and replace the rows that has NA step count with the avarage taken from the corresponsing interval in actPattern data set.

* Now I can calculate the daily total steps, plot histogram and compute the mean&median again as described in Q1.


```r
numMissing <- length(data[is.na(data$steps),]$steps)
#dailyAvgSteps <- ddply(data2, c("interval"), mutate, avgSteps = ave(steps, na.rm = FALSE, FUN=mean))
#dailyAvgSteps[is.na(dailyAvgSteps$avgSteps),]$avgSteps <- 0.0
dataNew <- data
dataNew$steps <- as.numeric(dataNew$steps)
for (i in 1:length(dataNew$steps)) {
    if (is.na(dataNew[i,]$steps)) {
        dataNew[i,]$steps <- actPattern[actPattern$interval == dataNew[i,]$interval,]$avgSteps
    }
}

newdailySteps <- ddply(dataNew, c("date"), summarise, totSteps = sum(steps))
head(newdailySteps)
```

```
##         date totSteps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
hist(x = newdailySteps$totSteps, xlab = "Total Steps per Day", main = "Historgram of Daily Total Steps")
```

![](./PA1_template_files/figure-html/missingValue-1.png) 

```r
newmeanSteps <- format(x = mean(newdailySteps$totSteps), digits = 6)
newmedianSteps <- format(x = median(newdailySteps$totSteps), digits = 6)
```

The total number of missing values in the dataset is 2304. The new mean of the total daily steps is 10766.2 and the new median is 10766.2. The mean did not change because we used the mean value but median is now equal to mean number. 

## Q4: Are there differences in activity patterns between weekdays and weekends?

* I applied ddply and created a new variable called week which would take weekend or weekday values based on the ifelse check on each value of the date row. We need to convert the date to POSIXlt and then we can use weekdays function to check if the date correspond to Staurday or Sunday.

* Then we can convert the column to Factor and  calculate the mean steps.

* Next phase is to use lattice plot and plot activity during weekdays and weekends.


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

