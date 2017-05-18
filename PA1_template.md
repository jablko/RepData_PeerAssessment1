---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---



# Reproducible Research: Peer Assessment 1

This is my solution to the assignment!

## Loading and preprocessing the data


```r
# Read the data.
data <- read.csv("activity.csv")
# It contains consecutive rows (possibly missing values) for every day
# and every interval -- so reshape it into a matrix for convenience.
steps <- matrix(data$steps, 60 / 5 * 24)
# Parse the dates and times.
data$date <- as.Date(data$date)
data$interval <- strptime(formatC(data$interval, width = 4, flag = 0),
                          "%H%M")
# Set up some global values.
col <- c("#E41A1C", "#377EB8")
lty <- c(2, 3)
```

Code that sets up things for the sections that follow.

`data` is the assignment data. I parse the `date` column for the benefit
of `weekdays()` in the last section, below. Plotting the `interval`
column as is could cause problems because those values aren't regularly
spaced (there aren't 100 minutes in an hour) ... It might be fine if
they're plotted by index rather than by value, but converting them seems
right -- and it makes for nicely labeled axes.

Ideally I'd use a datatype for times -- the
[`hms`](https://cran.r-project.org/web/packages/hms/index.html) class --
but that package is a recent development and not widely distributed yet?
Oh well ... The standard `POSIXlt` class works fine -- we just have to
add `scale_x_datetime()` explicitly to get the right labels with ggplot2
is all.

Reshaping `steps` into a matrix helps with imputing missing values -- it
makes it simple to slice the data by interval or by day. It also means
we can use `colSums()` and `rowMeans()`, which is handy everywhere --
well, everywhere except in the final section (because of facetting).

Those last couple lines are just cosmetic values I use in two or more
places -- they're up here to avoid repetition.

## What is the mean total number of steps taken per day?


```r
# Columns == days
original <- colSums(steps)
hist(original, main = "Histogram of total steps taken per day",
     xlab = "Total steps taken per day")
mean_ <- mean(original, na.rm = TRUE)
median_ <- median(original, na.rm = TRUE)
abline(v = c(mean_, median_), col = col, lty = lty, lwd = 2)
legend("topright", c(paste("Mean:", format(mean_), "steps"),
                     paste("Median:", format(median_), "steps")),
       col = col, lty = lty, lwd = 2, bty = "n")
```

![plot of chunk unnamed-chunk-3](https://cdn.rawgit.com/jablko/RepData_PeerAssessment1/master/figure/unnamed-chunk-3-1.svg)

Here is a summary of the total number of steps taken per day -- missing
values ignored. The histogram is annotated with the mean and the median.

## What is the average daily activity pattern?


```r
# Rows == intervals
mean_ <- rowMeans(steps, na.rm = TRUE)
plot(data$interval[1:nrow(steps)], mean_, type = "l",
     main = "Average daily activity pattern",
     xlab = "Time of day", ylab = "Steps taken per 5 minute interval")
x <- data$interval[which.max(mean_)]
y <- max(mean_)
points(x, y)
text(x, y, paste("Maximum:", format(y), "steps at", format(x, "%H:%M")),
     pos = 4)
```

![plot of chunk unnamed-chunk-4](https://cdn.rawgit.com/jablko/RepData_PeerAssessment1/master/figure/unnamed-chunk-4-1.svg)

And this plot shows when in the day (on average) the activity took
place. It's annotated with the 5 minute interval in which the most steps
were taken.

I experimented with adding clues to illustrate the spread of the data --
using `smoothScatter()` or by plotting the standard deviation, for
example -- but it proved to be a lot of effort with the base plotting
system.

## Imputing missing values


```r
# How many values are missing?
missing_values <- sum(is.na(steps))
missing_days <- sum(apply(is.na(steps), 2, any))
# Fill them in by interpolating between the days before and after.
for (i in 1:nrow(steps)) {
  steps[i,] <- approxfun(seq_along(steps[i,]), steps[i,], rule = 2)(
      seq_along(steps[i,]))
}
imputed <- colSums(steps)
hist(imputed, main = "Histogram of total steps taken per day",
     xlab = "Total steps taken per day")
mean_ <- mean(imputed)
median_ <- median(imputed)
abline(v = c(mean_, median_), col = col, lty = lty, lwd = 2)
legend("topright", c(paste("Mean:", format(mean_), "steps"),
                     paste("Median:", format(median_), "steps")),
       col = col, lty = lty, lwd = 2, bty = "n")
```

![plot of chunk unnamed-chunk-5](https://cdn.rawgit.com/jablko/RepData_PeerAssessment1/master/figure/unnamed-chunk-5-1.svg)

There are 2304 missing values (8 missing
days). To fill them in, I first thought, interpolate between the
intervals before and after the missing values! In other words, assume
that activity levels are similar to nearby levels. But then I noticed
that the missing values are arranged in blocks comprising whole days --
and interpolating from midnight to midnight will likely result in all
zeros, which seems not right.

My second thought was to take the days before and after the missing
values and interpolate between the corresponding intervals of those. I
did this by reshaping the data into a matrix and calling `approxfun()`
on each row (rows == intervals, columns == days). `approxfun()` performs
linear interpolation by default.

This strategy didn't affect the mean or the median number of steps per
day by much. (The new mean and median are reported in the histogram
above.) Both decreased by less than 1% -- the mean was affected slightly
more than the median.


```r
# Compare the imputed data with the original.
boxplot(original, imputed, names = c("Original data", "Imputed data"),
        main = "Original vs. imputed data comparison",
        ylab = "Total steps taken per day")
```

![plot of chunk unnamed-chunk-6](https://cdn.rawgit.com/jablko/RepData_PeerAssessment1/master/figure/unnamed-chunk-6-1.svg)

Overall the shape of the imputed data shifted very slightly to the left
of the original data, in terms of total steps taken per day, as shown in
the plot above. I imagine it could have gone in either direction,
depending on whether the days before and after the missing days were
above or below the average numbers of steps.

This in mind I'm left wondering if I chose a good strategy -- especially
given the different weekday and weekend activity patterns illustrated in
the next section ... Without a detailed model for the data, maybe
filling missing values in with the overall average of corresponding
intervals is the fanciest strategy that still makes sense?

## Are there differences in activity patterns between weekdays and weekends?


```r
library(ggplot2)
data$steps <- as.vector(steps)
ggplot(data, aes(interval, steps)) +
    stat_summary(geom = "line", fun.data = mean_se) +
    labs(title = "Weekday vs. weekend activity comparison",
         x = "Time of day", y = "Steps taken per 5 minute interval") +
    scale_x_datetime(date_labels = "%H:%M") +
    facet_wrap(~ ifelse(weekdays(date) %in% c("Saturday", "Sunday"),
                        "Weekend", "Weekday"),
               ncol = 1)
```

![plot of chunk unnamed-chunk-7](https://cdn.rawgit.com/jablko/RepData_PeerAssessment1/master/figure/unnamed-chunk-7-1.svg)

This is a comparison of weekend activity to activity during the week.


```r
# Quantify some observations.
sum_ <- with(data, aggregate(
    steps,
    list(date = date,
         before_noon = interval < strptime("12:00", "%H:%M")),
    sum))
sum_ <- with(sum_, aggregate(
    x,
    list(weekend = weekdays(date) %in% c("Saturday", "Sunday"),
         before_noon = before_noon),
    mean))
```

On weekends there appears to be less activity before noon
(3904.365 steps on average
compared to 5196.404 on
weekdays) -- maybe because the person measured doesn't go to work or
school on the weekend? Conversely there appears to be more activity
after noon (8200.344 steps on
average compared to 4560.178
on weekdays) -- maybe because they work in an office during the week?
