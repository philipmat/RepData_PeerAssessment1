# Reproducible Research: Peer Assessment 1
Philip Mateescu  


### Preamble; Codebook

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)
* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format
* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

We will be using the *dplyr* library for data processing, and `ggplot2` for drawing:

```r
library(dplyr)
library(ggplot2)
```


## Loading and preprocessing the data
Fortunately, this repository already contains the **activity.zip** file mentioned above.
However, let's guard against the file being missing for whatever reason:


```r
if(!file.exists("activity.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile="activity.zip")
}
```

We don't need to extract the zip - the `unz` file can do this for us:


```r
step.data <- read.csv(unz('activity.zip', filename = 'activity.csv'), colClasses = c('integer', 'Date', 'integer'))
```

For convenience, we'll also load it into a *dplyr* data frame tbl:

```r
step.data <- tbl_df(step.data)
step.data
```

```
## Source: local data frame [17,568 x 3]
## 
##    steps       date interval
##    (int)     (date)    (int)
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
## ..   ...        ...      ...
```

## What is mean total number of steps taken per day?


```r
mean.steps.per.day <- mean(step.data$steps, na.rm = TRUE)
median.steps.per.day <- median(step.data$steps, na.rm = TRUE)

days.with.steps <- step.data  %>% 
    filter(!is.na(steps)) %>% 
    arrange(date) %>%
    mutate(month.name=months(date), weekday=weekdays(date))
print(days.with.steps)    
```

```
## Source: local data frame [15,264 x 5]
## 
##    steps       date interval month.name weekday
##    (int)     (date)    (int)      (chr)   (chr)
## 1      0 2012-10-02        0    October Tuesday
## 2      0 2012-10-02        5    October Tuesday
## 3      0 2012-10-02       10    October Tuesday
## 4      0 2012-10-02       15    October Tuesday
## 5      0 2012-10-02       20    October Tuesday
## 6      0 2012-10-02       25    October Tuesday
## 7      0 2012-10-02       30    October Tuesday
## 8      0 2012-10-02       35    October Tuesday
## 9      0 2012-10-02       40    October Tuesday
## 10     0 2012-10-02       45    October Tuesday
## ..   ...        ...      ...        ...     ...
```

```r
totalsteps.per.day <- days.with.steps  %>% 
    group_by(date) %>%
    summarize(steps.total=sum(steps))

totalsteps.per.day %>% arrange(steps.total) %>% print
```

```
## Source: local data frame [53 x 2]
## 
##          date steps.total
##        (date)       (int)
## 1  2012-11-15          41
## 2  2012-10-02         126
## 3  2012-10-25        2492
## 4  2012-11-08        3219
## 5  2012-11-20        4472
## 6  2012-10-29        5018
## 7  2012-11-16        5441
## 8  2012-10-26        6778
## 9  2012-11-29        7047
## 10 2012-11-13        7336
## ..        ...         ...
```

```r
sum.steps.total <- sum(totalsteps.per.day$steps.total)
mean.steps.total <- mean(totalsteps.per.day$steps.total)
median.steps.total <- median(totalsteps.per.day$steps.total)
```

Took a **total** of 570,608 steps,
with an **average** of 10,766 steps 
and a **median** value of 10,765 steps.


```r
ggplot(days.with.steps, aes(x = date, y = steps, fill = weekday)) + 
    geom_histogram(stat = 'identity') +
    geom_hline(aes(yintercept=mean.steps.total), color = 'red', linetype='dashed') +
    geom_hline(aes(yintercept=median.steps.total), color = 'black', linetype='dotted') +
    labs(title = 'Total number of steps per day', x = 'Date', y = 'Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 




## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
