---
title: "Rep_data_PeerAssessment1"
author: "Cerys Williams"
date: "2025-12-16"
output: html_document
---
### Upload the dataset

I downloaded the data set to my working directory and then used read.csv to install the data set from there. 


``` r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 4.5.2
```

```
## ── Attaching core tidyverse packages ─────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.6.0
## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.2.0     
## ── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
activity <- read_csv("activity.csv")
```

```
## Rows: 17568 Columns: 3
## ── Column specification ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

### Set echo = TRUE as global default to ensure it is always used in each code chunk.  



``` r
knitr::opts_chunk$set(echo = TRUE)
```


### What is the total number of steps taken in one day?

Step 1: Calculate the total number of steps take in one day. 


``` r
daily_steps_total <- activity %>%
  group_by(date) %>%
  summarise(sum_total_steps = sum(steps))
```

Step 2: Make a histogram of the total number of steps each day. 


``` r
hist(daily_steps_total$sum_total_steps,
     main = "Total number of steps per day",
     xlab = "Number of Steps",
     ylab = "Frequency",
     )
```

![plot of chunk histogram total number of steps per day](figure/histogram total number of steps per day-1.png)

Step 3: Calculate and report the mean and median of the total number of steps taken per day.
        For this calculation dataframe I created in step 1 when I calculated the total number of steps in one day.   


``` r
daily_steps_mean_median <- daily_steps_total %>%
  group_by(date) %>%
  summarise(mean_total_steps = mean(sum_total_steps), median_total_steps = median(sum_total_steps))
```

### What is the average daily activity pattern?

Step 1: Create a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). Ignoring the N/A in this case does not produce a valid response.  


``` r
interval_mean <- activity %>%
  group_by(interval) %>%
  summarise(interval_mean = mean(steps, na.rm = TRUE))

ggplot(interval_mean, aes(x = interval, y = interval_mean)) +
  geom_line(color = "blue", lwd = 1) +
  labs(title = "Daily Activity Pattern",
       x = "Interval",
       y = "Mean number of steps per interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![plot of chunk interval mean](figure/interval mean-1.png)

Step 2: Calculate which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  Ignoring the N/A in this case does not produce a valid response.  The max number of steps has been arranged in decreasing order so that calling the head function would identify the interval with the most steps. A  a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) identifies the pattern. 


``` r
interval_max <- activity %>%
  group_by(interval) %>%
  summarise(max_steps = max(steps, na.rm = TRUE)) %>%
  arrange(desc(max_steps))

head(interval_max)
```

```
## # A tibble: 6 × 2
##   interval max_steps
##      <dbl>     <dbl>
## 1      615       806
## 2      900       802
## 3      550       794
## 4      720       789
## 5      835       786
## 6      925       785
```

``` r
ggplot(interval_max, aes(x = interval, y = max_steps)) +
  geom_line(color = "blue", lwd = 1) +
  labs(title = "Daily Activity Pattern",
       x = "Interval",
       y = "Max number of steps per interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![plot of chunk interval max](figure/interval max-1.png)

### Imputing missing values

Calculate the total number of NA in the activity data set. 


``` r
colSums(is.na(activity))
```

```
##    steps     date interval 
##     2304        0        0
```

A new dataset has been created where the  mean for that 5-minute interval in the day has been used to fill in all of the missing values.   The number of NA is is recalculated in the new dateset to make sure that none remain. 


``` r
activity_imputed <- activity %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps),
                        mean(steps, na.rm = TRUE),
                        steps))

colSums(is.na(activity_imputed))
```

```
##    steps     date interval 
##        0        0        0
```

### Are there differences in activity patterns between weekdays and weekends?

Used the weekdays() function to create a variable containing the 2 levels Weekday and weekend. 


``` r
activity_imputed <- activity_imputed %>%
  mutate(date, weekdays(date)) %>%
  mutate(weekdays = case_when(`weekdays(date)` %in% c("Saturday", "Sunday") ~ "Weekend",TRUE ~ "Weekday"))
```

Calculated the mean number of steps per interval, averaged across all weekday days or weekend days.  Use the data to create a panel plot  containing time series plots of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis) using ggplot2 and facet_wrap function. 


``` r
activity_imputed_averaged <- activity_imputed %>%
  group_by(interval, weekdays) %>%
  summarise(mean_steps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

``` r
ggplot(activity_imputed_averaged, aes(x = interval, y = mean_steps)) +
  geom_line(color = "blue", lwd = 1) +
  facet_wrap(~ weekdays) +   
  labs(title = "Weekday v Weekend Activity Pattern",
       x = "Interval",
       y = "Mean number of steps per interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![plot of chunk compare weekday and weekend](figure/compare weekday and weekend-1.png)

