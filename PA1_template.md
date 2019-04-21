``` r
library(plyr)
```

    ## Warning: package 'plyr' was built under R version 3.5.2

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.5.2

Loading and Processing the data
===============================

### loading the Data

``` r
setwd("~/repdata_data_activity")
activity <- read.csv("activity.csv")
```

### cleaning the Data

``` r
activity$day <- weekdays(as.Date(activity$date))
clean<-activity[!is.na(activity$steps),]
```

Mean total number of steps taken per day
========================================

``` r
sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum )
colnames(sumTable)<- c("Date", "Steps")
```

### total number of steps taken per day

``` r
sumTable
```

    ##          Date Steps
    ## 1  2012-10-02   126
    ## 2  2012-10-03 11352
    ## 3  2012-10-04 12116
    ## 4  2012-10-05 13294
    ## 5  2012-10-06 15420
    ## 6  2012-10-07 11015
    ## 7  2012-10-09 12811
    ## 8  2012-10-10  9900
    ## 9  2012-10-11 10304
    ## 10 2012-10-12 17382
    ## 11 2012-10-13 12426
    ## 12 2012-10-14 15098
    ## 13 2012-10-15 10139
    ## 14 2012-10-16 15084
    ## 15 2012-10-17 13452
    ## 16 2012-10-18 10056
    ## 17 2012-10-19 11829
    ## 18 2012-10-20 10395
    ## 19 2012-10-21  8821
    ## 20 2012-10-22 13460
    ## 21 2012-10-23  8918
    ## 22 2012-10-24  8355
    ## 23 2012-10-25  2492
    ## 24 2012-10-26  6778
    ## 25 2012-10-27 10119
    ## 26 2012-10-28 11458
    ## 27 2012-10-29  5018
    ## 28 2012-10-30  9819
    ## 29 2012-10-31 15414
    ## 30 2012-11-02 10600
    ## 31 2012-11-03 10571
    ## 32 2012-11-05 10439
    ## 33 2012-11-06  8334
    ## 34 2012-11-07 12883
    ## 35 2012-11-08  3219
    ## 36 2012-11-11 12608
    ## 37 2012-11-12 10765
    ## 38 2012-11-13  7336
    ## 39 2012-11-15    41
    ## 40 2012-11-16  5441
    ## 41 2012-11-17 14339
    ## 42 2012-11-18 15110
    ## 43 2012-11-19  8841
    ## 44 2012-11-20  4472
    ## 45 2012-11-21 12787
    ## 46 2012-11-22 20427
    ## 47 2012-11-23 21194
    ## 48 2012-11-24 14478
    ## 49 2012-11-25 11834
    ## 50 2012-11-26 11162
    ## 51 2012-11-27 13646
    ## 52 2012-11-28 10183
    ## 53 2012-11-29  7047

### histogram of the total number of steps taken each day

``` r
hist(sumTable$Steps, xlab="Steps", main = "Total Steps per Day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

### mean and median of the total number of steps taken per day

``` r
as.integer(mean(sumTable$Steps))
```

    ## [1] 10766

``` r
as.integer(median(sumTable$Steps))
```

    ## [1] 10765

Average daily activity pattern
==============================

### time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
# Average number of steps of every interval 
intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))
p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)

### interval that has maximum nuber of steps on average across all the days

``` r
maxSteps <- max(intervalTable$Avg)
intervalTable[intervalTable$Avg==maxSteps,1]
```

    ## [1] 835

Imputing missing values
=======================

### total number of missing values in the dataset

``` r
nrow(activity[is.na(activity$steps),])
```

    ## [1] 2304

### filling the missiing value

``` r
avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))
nadata<- activity[is.na(activity$steps),]
newdata<-merge(nadata, avgTable, by=c("interval","day"))
```

### creating the new dataset same as original

``` r
newdata2<- newdata[,c(5,4,1,2)]
colnames(newdata2)<- c("steps", "date", "interval", "day")
mergeData <- rbind(clean, newdata2)
```

### histogram of the total number of steps taken each day

``` r
sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum )
colnames(sumTable2)<- c("Date", "Steps")
hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=TRUE)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-13-1.png)

### mean and median of new dataset in which na's are taken care

``` r
mean(sumTable2$Steps)
```

    ## [1] 10821.21

``` r
median(sumTable2$Steps)
```

    ## [1] 11015

Differences in activity patterns between weekdays and weekends
==============================================================

``` r
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))
p <- ggplot(intervalTable2, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval Based on day type")+facet_grid(rows = vars(DayCategory))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-15-1.png)
