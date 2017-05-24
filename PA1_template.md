# Reproducible Research: Peer Assessment 1
Pedro A. Alonso Baigorri  
22/05/2017  
## Introduction
This document describes the analysis done for the 1st practice of the Reproducible Research Course, part of the Data Science specialization in course.

The objectives of the exercice are described in the README.md file on this repository.


## Setting global options


## Loading and preprocessing the data

```r
setwd('.')
unzip("activity.zip")
steps_dataset <- read.csv("activity.csv")
steps_dataset$dateformat <- as.Date(steps_dataset$date, "%Y-%m-%d")
head(steps_dataset)
```

```
##   steps       date interval dateformat
## 1    NA 2012-10-01        0 2012-10-01
## 2    NA 2012-10-01        5 2012-10-01
## 3    NA 2012-10-01       10 2012-10-01
## 4    NA 2012-10-01       15 2012-10-01
## 5    NA 2012-10-01       20 2012-10-01
## 6    NA 2012-10-01       25 2012-10-01
```

## Histogram of the total number of steps taken each day
The purpose of the histogram is to know what's the frecuency (number of days) where an specific value of the total steps has occured during all the period of analysis. In summary I will represent a barplot where the y-axis is the frequency (number of days) and the x-axis the different values of total  number of steps.

To do this I will calculate first the total number of the steps by day and then plot an histogram using the hist() function.


```r
steps_by_day <- aggregate(steps ~ date, data = steps_dataset, FUN = sum )
head(steps_by_day)
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

```r
hist(steps_by_day$steps, breaks=30, col=c("red"), xlab = "Number of steps", main =" Histogram of the total number of steps taken each day")
```

![](figure/unnamed-chunk-2-1.png)<!-- -->

## What is the mean and median of  total number of steps taken per day?

Using the agreggated data calculated before, the mean and the median can be calculated as follows:


```r
mean_steps <- mean(steps_by_day$steps)
median_steps <- median(steps_by_day$steps)
```

This provides the following results:  
  
- Mean = 1.0766189\times 10^{4}  
- Median = 10765


## What is the average daily activity pattern?
To analyze the average daily activity pattern first of all I will make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_by_interval <- aggregate(steps ~ interval, data = steps_dataset, FUN = mean )
head(steps_by_interval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
plot(steps_by_interval$interval, steps_by_interval$steps,  ylab = "Average of steps", xlab = "interval", type = "l", main = "Daily activity pattern")
```

![](figure/unnamed-chunk-4-1.png)<!-- -->

Now I'll calculate the maximum value and  interval when this maximum number happens

```r
max_steps <- max(steps_by_interval$steps)

id_max_interval <- which(steps_by_interval$steps == max_steps)
max_interval <- steps_by_interval[max(id_max_interval), "interval"]

#another way to calculate the max_interval
max_interval <- steps_by_interval[steps_by_interval$steps==max_steps,"interval"]
```

The max avg steps are: 206.1698113.  
The interval when the max avg step happens is: 835


## Imputing missing values
Now I wll analyze the impact of missing values in the dataset. To do this first of all I'll calculate the total number of NA's in the dataset and how they are distributed.


```r
# calculation of number of intervals with NA's per day
nas <- steps_dataset[is.na(steps_dataset$steps),]
nas$count <- 0
head(nas)
```

```
##   steps       date interval dateformat count
## 1    NA 2012-10-01        0 2012-10-01     0
## 2    NA 2012-10-01        5 2012-10-01     0
## 3    NA 2012-10-01       10 2012-10-01     0
## 4    NA 2012-10-01       15 2012-10-01     0
## 5    NA 2012-10-01       20 2012-10-01     0
## 6    NA 2012-10-01       25 2012-10-01     0
```

```r
nas_by_day <- aggregate(count ~ date, data = nas, FUN = length )
print(nas_by_day)
```

```
##         date count
## 1 2012-10-01   288
## 2 2012-10-08   288
## 3 2012-11-01   288
## 4 2012-11-04   288
## 5 2012-11-09   288
## 6 2012-11-10   288
## 7 2012-11-14   288
## 8 2012-11-30   288
```

```r
# calculation of number of total intervals with or without NA's per day
steps_dataset$count <- 0
intervals_by_day <- aggregate(count ~ date, data = steps_dataset, FUN = length )
head(intervals_by_day)
```

```
##         date count
## 1 2012-10-01   288
## 2 2012-10-02   288
## 3 2012-10-03   288
## 4 2012-10-04   288
## 5 2012-10-05   288
## 6 2012-10-06   288
```

We can see that there is always the same value NAs for each day: 288, and it's the same of the total number of intervals per day.
288.  This demonstrates that there are not single missing values inside the days, only full days with all the intervals missing.

So, to imput missing values to the missing days , I can assign for each missing 
day the average of the rest of the days per each interval.


```r
steps_dataset_clean <- steps_dataset
steps_dataset_clean[is.na(steps_dataset$steps), "steps"] <- as.integer(steps_by_interval$steps)
head(steps_dataset_clean)
```

```
##   steps       date interval dateformat count
## 1     1 2012-10-01        0 2012-10-01     0
## 2     0 2012-10-01        5 2012-10-01     0
## 3     0 2012-10-01       10 2012-10-01     0
## 4     0 2012-10-01       15 2012-10-01     0
## 5     0 2012-10-01       20 2012-10-01     0
## 6     2 2012-10-01       25 2012-10-01     0
```

Now calculate the histogram, mean and median with the new data: 

```r
steps_by_day <- aggregate(steps ~ date, data = steps_dataset_clean, FUN = sum )
head(steps_by_day)
```

```
##         date steps
## 1 2012-10-01 10641
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
hist(steps_by_day$steps, breaks=30, col=c("red"), xlab = "Number of steps", 
     main =" Histogram of the total number of steps taken each day (imputting missing values)")
```

![](figure/unnamed-chunk-8-1.png)<!-- -->

```r
mean_steps_clean <- mean(steps_by_day$steps)
median_steps_clean <- median(steps_by_day$steps)
```

This provides the following results:  

(With imputation)
  
- Mean = 1.074977\times 10^{4}  
- Median = 10641

(Without imputation)
  
- Mean = 1.0766189\times 10^{4}  
- Median = 10765


## Are there differences in activity patterns between weekdays and weekends?

First, I'll create a new factor variable in the dataset with two levels - 
"weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
steps_dataset_clean$wday <- weekdays(steps_dataset_clean$dateformat)
steps_dataset_clean$weekend <- ifelse((steps_dataset_clean$wday == "sábado") | 
                                          (steps_dataset_clean$wday == "domingo"), "WEEKEND", "WORKDAY")
```

Then I'll make a panel plot containing a time series plot (i.e. type = "l") of 
the 5-minute interval (x-axis) and the average number of steps taken, averaged 
across all weekday days or weekend days (y-axis)


```r
steps_by_interval_weekend<- aggregate(steps ~ interval + weekend, 
                                    data = steps_dataset_clean, FUN = mean )
head(steps_by_interval_weekend)
```

```
##   interval weekend steps
## 1        0 WEEKEND 0.125
## 2        5 WEEKEND 0.000
## 3       10 WEEKEND 0.000
## 4       15 WEEKEND 0.000
## 5       20 WEEKEND 0.000
## 6       25 WEEKEND 3.500
```

```r
table(steps_by_interval_weekend$weekend, steps_by_interval_weekend$interval)
```

```
##          
##           0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 125 130
##   WEEKEND 1 1  1  1  1  1  1  1  1  1  1  1   1   1   1   1   1   1   1
##   WORKDAY 1 1  1  1  1  1  1  1  1  1  1  1   1   1   1   1   1   1   1
##          
##           135 140 145 150 155 200 205 210 215 220 225 230 235 240 245 250
##   WEEKEND   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##   WORKDAY   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##          
##           255 300 305 310 315 320 325 330 335 340 345 350 355 400 405 410
##   WEEKEND   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##   WORKDAY   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##          
##           415 420 425 430 435 440 445 450 455 500 505 510 515 520 525 530
##   WEEKEND   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##   WORKDAY   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##          
##           535 540 545 550 555 600 605 610 615 620 625 630 635 640 645 650
##   WEEKEND   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##   WORKDAY   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##          
##           655 700 705 710 715 720 725 730 735 740 745 750 755 800 805 810
##   WEEKEND   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##   WORKDAY   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##          
##           815 820 825 830 835 840 845 850 855 900 905 910 915 920 925 930
##   WEEKEND   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##   WORKDAY   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
##          
##           935 940 945 950 955 1000 1005 1010 1015 1020 1025 1030 1035 1040
##   WEEKEND   1   1   1   1   1    1    1    1    1    1    1    1    1    1
##   WORKDAY   1   1   1   1   1    1    1    1    1    1    1    1    1    1
##          
##           1045 1050 1055 1100 1105 1110 1115 1120 1125 1130 1135 1140 1145
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           1150 1155 1200 1205 1210 1215 1220 1225 1230 1235 1240 1245 1250
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           1255 1300 1305 1310 1315 1320 1325 1330 1335 1340 1345 1350 1355
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           1400 1405 1410 1415 1420 1425 1430 1435 1440 1445 1450 1455 1500
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           1505 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 1600 1605
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           1610 1615 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           1715 1720 1725 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           1820 1825 1830 1835 1840 1845 1850 1855 1900 1905 1910 1915 1920
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           1925 1930 1935 1940 1945 1950 1955 2000 2005 2010 2015 2020 2025
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           2030 2035 2040 2045 2050 2055 2100 2105 2110 2115 2120 2125 2130
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           2135 2140 2145 2150 2155 2200 2205 2210 2215 2220 2225 2230 2235
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           2240 2245 2250 2255 2300 2305 2310 2315 2320 2325 2330 2335 2340
##   WEEKEND    1    1    1    1    1    1    1    1    1    1    1    1    1
##   WORKDAY    1    1    1    1    1    1    1    1    1    1    1    1    1
##          
##           2345 2350 2355
##   WEEKEND    1    1    1
##   WORKDAY    1    1    1
```

```r
plot(steps_by_interval_weekend$interval, steps_by_interval_weekend$steps,  
     ylab = "Average of steps", xlab = "interval", type = "l", main = "Daily activity pattern")
```

![](figure/unnamed-chunk-10-1.png)<!-- -->
