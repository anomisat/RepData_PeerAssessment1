Reproducible Research Course Project 1
========================================

This assignment analyzes data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


## Loading and preprocessing the data


```r
setwd("C:/Projects/Training/R/Reproducible Research/Scripts")
library(dplyr)
library(ggplot2)
library(lattice)
```


```r
activity <- read.csv("activity.csv")
activity$date_2 <- as.Date(activity$date, "%Y-%m-%d")
```

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


## What is mean total number of steps taken per day?


```r
table_1 <- group_by(activity, date_2)
table_2 <- summarise(table_1, Total_steps=sum(steps, na.rm = T))
g <- ggplot(table_2,aes(x=date_2,y=Total_steps))
g + geom_bar(stat="identity")+xlab("Day")+ylab("Steps")+ggtitle("Total steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
table_3 <- summarise(table_1, Avg_steps=mean(steps, na.rm = T), Median_steps=median(steps, na.rm = T))
print(table_3)
```

```
## # A tibble: 61 × 3
##        date_2 Avg_steps Median_steps
##        <date>     <dbl>        <dbl>
## 1  2012-10-01       NaN           NA
## 2  2012-10-02   0.43750            0
## 3  2012-10-03  39.41667            0
## 4  2012-10-04  42.06944            0
## 5  2012-10-05  46.15972            0
## 6  2012-10-06  53.54167            0
## 7  2012-10-07  38.24653            0
## 8  2012-10-08       NaN           NA
## 9  2012-10-09  44.48264            0
## 10 2012-10-10  34.37500            0
## # ... with 51 more rows
```

## What is the average daily activity pattern?


```r
means <- tapply(activity$steps, as.factor(activity$interval), mean, na.rm = T)
int_f <- as.factor(activity$interval)
plot(levels(int_f), means, type="l", xlab = "Interval", ylab = "Avg steps", main = "Avg Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
which.max(means)
```

```
## 835 
## 104
```

The daily activities begin after the 500th interval with a peak in the 835th interval.

## Imputing missing values

When there are missing values for the intervals they were imputed with the average value for that interval for all observations.


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

The total number of missing values is 2304.


```r
means_all <- rep(means, 61)
activity$Mean_by_int <- means_all
activity$steps_2[is.na(activity$steps)] <- activity$Mean_by_int[is.na(activity$steps)]
activity$steps_2[!is.na(activity$steps)] <- activity$steps[!is.na(activity$steps)]
activity_2 <- activity[,-1]
```

After the imputation, the mean and median of the steps change.


```r
activity_2$date_2 <- as.Date(activity_2$date, "%Y-%m-%d")
table_4 <- group_by(activity_2, date_2)
table_5 <- summarise(table_4, Total_steps=sum(steps_2, na.rm = T))

g <- ggplot(table_5,aes(x=date_2,y=Total_steps))
g + geom_bar(stat="identity")+xlab("Day")+ylab("Steps")+ggtitle("Total steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
table_6 <- summarise(table_4, Avg_steps=mean(steps_2, na.rm = T), Median_steps=median(steps_2, na.rm = T))
table_6
```

```
## # A tibble: 61 × 3
##        date_2 Avg_steps Median_steps
##        <date>     <dbl>        <dbl>
## 1  2012-10-01  37.38260     34.11321
## 2  2012-10-02   0.43750      0.00000
## 3  2012-10-03  39.41667      0.00000
## 4  2012-10-04  42.06944      0.00000
## 5  2012-10-05  46.15972      0.00000
## 6  2012-10-06  53.54167      0.00000
## 7  2012-10-07  38.24653      0.00000
## 8  2012-10-08  37.38260     34.11321
## 9  2012-10-09  44.48264      0.00000
## 10 2012-10-10  34.37500      0.00000
## # ... with 51 more rows
```

```r
table_5 <- rename(table_5, Total_steps_imp=Total_steps)
table_7 <- cbind(table_2, table_5)

m <-  ggplot(table_7, aes(date_2)) + 
  geom_bar(aes(y = Total_steps_imp, fill = "Total_steps_imp"), stat = "identity") +
  geom_bar(aes(y = Total_steps, fill = "Total_steps"), stat = "identity") + 
  xlab("Day")+ylab("Steps")+ggtitle("Total steps per day")
m
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-2.png)

On the graph we can see the total number of steps before and after the imputation. When the steps were missing before the imputation, now we see a value.

## Are there differences in activity patterns between weekdays and weekends?


```r
activity_2$Weekday <- weekdays(activity_2$date_2)
activity_2$day_flag[activity_2$Weekday %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")] <- "Weekday"
activity_2$day_flag[activity_2$Weekday %in% c("Saturday","Sunday")] <- "Weekend"
activity_2$day_flag <- as.factor(activity_2$day_flag)

grouped_data <- aggregate(steps_2 ~ interval + day_flag, activity_2, mean)
xyplot(steps_2 ~ interval | day_flag, data = grouped_data, xlab="Interval", ylab="Steps", main = "Avarage Steps per Interval by Weekday", layout=c(1,2), type="l")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

During the weekend, there is higher activity in general, although in the weekdays, the maximum number of steps is higher than in the weekend.












