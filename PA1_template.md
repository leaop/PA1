---
title: "Activity Monitoring Data Analysis"
author: "Leão Pereira"
date: "2025-02-25"
output: html_document
---



## Loading and Preprocessing the Data


```r
library(ggplot2)

# Load the data
data <- read.csv("activity.csv", stringsAsFactors = FALSE)

data$date <- as.Date(data$date)
```

## What is the mean total number of steps taken per day?


```r
# Calculate total steps per day
steps_per_day <- aggregate(steps ~ date, data, sum, na.rm = TRUE)

# Histogram of total steps per day
hist(steps_per_day$steps, breaks = 20, col = "blue", main = "Total Steps per Day", xlab = "Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
# Mean and median
mean_steps <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
# Average steps per 5-minute interval
steps_per_interval <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)

# Time series plot
ggplot(steps_per_interval, aes(x = interval, y = steps)) +
  geom_line(color = "blue") +
  labs(title = "Average Daily Activity Pattern", x = "5-minute Interval", y = "Average Steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
# Interval with max steps
max_interval <- steps_per_interval[which.max(steps_per_interval$steps), ]
max_interval
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing Missing Values


```r
# Count missing values
missing_values <- sum(is.na(data$steps))
missing_values
```

```
## [1] 2304
```

```r
# Fill missing values with the mean of corresponding interval
imputed_data <- data
imputed_data$steps[is.na(imputed_data$steps)] <- 
  ave(data$steps, data$interval, FUN = function(x) mean(x, na.rm = TRUE))[is.na(data$steps)]

# Histogram after imputing
steps_per_day_imputed <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_per_day_imputed$steps, breaks = 20, col = "green", main = "Total Steps per Day (Imputed)", xlab = "Steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
# New mean and median
mean_steps_imputed <- mean(steps_per_day_imputed$steps)
median_steps_imputed <- median(steps_per_day_imputed$steps)
mean_steps_imputed
```

```
## [1] 10766.19
```

```r
median_steps_imputed
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?


```r
# Create factor variable for weekday/weekend
imputed_data$day_type <- ifelse(weekdays(imputed_data$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

# Average steps per interval by day type
steps_day_type <- aggregate(steps ~ interval + day_type, imputed_data, mean)

# Panel plot
ggplot(steps_day_type, aes(x = interval, y = steps, color = day_type)) +
  geom_line() +
  facet_wrap(~day_type, ncol = 1) +
  labs(title = "Activity Patterns: Weekdays vs Weekends", x = "5-minute Interval", y = "Average Steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)


