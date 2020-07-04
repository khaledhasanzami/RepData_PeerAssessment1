---
title: Reproducible Research Peer Assessment 1 -Analysis of Personal Movement Using
  Activity Monitoring Devices
author: "Md Khaled Hasan Zami"
date: "July 1, 2020"
output: 
  html_document: 
    keep_md: true 
        

---



#A. Loading and preprocessing the data
Here, we will perform two types of works. 
 
 1. We will load the data from the dataset [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
 
 2. We will perform some pre-processing of the dataset for the suitability of our analysis.
 

##1. Loading the data

Let's create a directory to to store the dataset. We named it as **data**. We will be downloading and saving all the files in it.

```r
if (!file.exists("data")) {
        dir.create("data")
}
```


Let's download the dataset.

Let's assign the dataset url to a variable. Then, download the file from the url and save it to directory named **data**. 

> method = "curl" $\rightarrow$ for mac users

> method = "wininet" $\rightarrow$ for windows users


```r
dataset_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 
download.file(dataset_url, destfile="./data/Activity_monitoring_data.zip", method = "wininet")
```

Let's unzip the downloaded zip file.

```r
unzip("./data/Activity_monitoring_data.zip")
```

Let's see what file was in the zipped file. We will find a **.csv** file.

```r
list.files("./data",  all.files=T)
```

```
## [1] "."                            ".."                          
## [3] "Activity_monitoring_data.zip"
```

We have our **.csv** file. We need to read the file using `read.csv()`.

```r
activity <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
```

**The dataset is loaded successfully**

##2. Preprocessing the data

We have read our dataset. Now, it is time to look inside. Let's `str()` the loaded dataset.

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
As we can see that we have 3 types of varibales. But the date column is in **factor** which was supposed to be in **date** format. So, let;s convert it into **date** format.

```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```
Now we have converted the date column in **Date** class. 

# B. What is mean total number of steps taken per day?

For this part of the assignment, we will be performing three things:
 
 1. We will be calculating the total number of steps taken per day
 
 2. We will be making a histogram of the total number of steps taken per day.
 
 3. We will be calculating the **mean** and **meadian** of the total number of steps taken.

##1. The total number of steps taken per day

Now, let's create a new data frame named **steps_per_day** and save the total number of steps taken each day.

```r
steps_per_day <- aggregate(steps ~ date, activity, sum)
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

##2. Histogram of the total number of steps taken each day

As the data has been saved to a new dataframe, let's make the histogram of the total number of steps taken per day.

```r
hist(steps_per_day$steps, main = paste("The total number of steps taken per day"),  breaks = 20, col="red", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##3. Mean and meadian of the total number of steps taken

R has a generic function called `mean()` to calculate the mean.

```r
rmean <- mean(steps_per_day$steps)
rmean
```

```
## [1] 10766.19
```
**So, the mean of the total number of steps taken is 10766.19**

Additionally, R has a generic function called `median()` to calculate the median.

```r
rmedian <- median(steps_per_day$steps)
rmedian
```

```
## [1] 10765
```
**So, the median of the total number of steps taken is 10765**

#C. What is the average daily activity pattern?

For this part of the assignment, we will be performing two things:

 1. We will be making a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
 2. We will be finding on Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

##1. Making a time series plot

First of all, we created a data frame named **steps_by_interval** by averaging the steps taken on each time interval.
Then, we ploted having the the intervals in x-axis and average number of steps in y-axis. The plot was titles using the `main` argument and colored using the `col` argument.

```r
steps_by_interval<- aggregate(steps ~ interval, activity, mean)
plot(steps_by_interval$interval, steps_by_interval$steps, 
      type = "l", 
      main = 'Average number of steps taken in 5-minute interval, averaged across all days',
      xlab='Intervals',
      ylab='Average number of steps',
      col = "#00AFBB")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

##2. 5-minute interval that contains the maximum number of steps

We calculated the maximum number of steps taken in the interval using the `which.max` function then printed out the on which interval it was maximum.


```r
intervale_max_steps<-steps_by_interval[which.max(steps_by_interval$steps),]$interval
intervale_max_steps
```

```
## [1] 835
```

#D. Inputing missing values

For this part of the assignment, we will be performing four things:

 1. We will be calculating and reporting the total number of missing values in the dataset 
 2. We will be using Mean values for fullfiling missing values
 3. We will be creating a new dataset including the imputed missing values
 4. We will be making a histogram of the total number of steps taken each day and calculating and reporting the mean and median total number of steps taken per day

##1. calculation and report of the total number of missing values in the dataset


```r
 totalNA<- sum(!complete.cases(activity))
 totalNA
```

```
## [1] 2304
```
**Total Number of Missing values are 2304**

##2. Fullfiling missing values


```r
StepsAverage <- aggregate(steps ~ interval, data = activity, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(activity)) {
    obs <- activity[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
```
##3. Creating a new dataset including the imputed missing values

```r
new_activity <- activity
new_activity$steps <- fillNA
```
##4. Making a histogram of the total number of steps taken each day and calculating and reporting the mean and median total number of steps taken per day

```r
StepsTotalUnion <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotalUnion$steps, main = "Total Steps Each Day", col="#00AFBB", xlab="Number of Steps")
#Create Histogram to show difference. 
hist(steps_per_day$steps, main = "Total Steps Each Day", col="maroon", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("#00AFBB", "maroon"), lwd=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
###Calculating Mean

```r
rmeantotal <- mean(StepsTotalUnion$steps)
rmeantotal
```

```
## [1] 10766.19
```
###Calculating Median

```r
rmediantotal <- median(StepsTotalUnion$steps)
rmediantotal
```

```
## [1] 10766.19
```
###Do these values differ from the estimates from the first part of the assignment?

```r
rmediandiff <- rmediantotal - rmedian
rmediandiff
```

```
## [1] 1.188679
```

```r
rmeandiff <- rmeantotal - rmean
rmeandiff
```

```
## [1] 0
```

**The mean(Mean Var: 0) is the same however the median does have a small variance(Median Var:1.1886792). between the total which includes the missing values to the base**

###What is the impact of imputing missing data on the estimates of the total daily number of steps?
On observation the impact of the missing data has the biggest effect on the 10000 - 150000 step interval and changes frequency from 27.5 to 35 a variance of 7.5


#E. Are there differences in activity patterns between weekdays and weekends?
For this part of the assignment, we will be performing two things:

 1. We will be creating a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day
 2. We will be making a panel plot containing a time series plot
 
##1. Creating a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
new_activity$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date)),weekdays), "Weekday", "Weekend"))
StepsTotalUnion <- aggregate(steps ~ interval + dow, new_activity, mean)
```

##2. A panel plot containing a time series plot

```r
library(lattice)
xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-23-1.png)<!-- -->


