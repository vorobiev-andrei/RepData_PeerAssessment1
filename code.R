unzip("activity.zip")
activity <- read.csv("activity.csv", na.strings = "NA")
library(dplyr)
activity <- tbl_df(activity)

activity_by_day <- activity %>% group_by(date) %>% summarize(steps = sum(steps, na.rm = TRUE))
hist(activity_by_day$steps)
barplot(activity_by_day$steps, main = "Number of steps taken for each day", xlab = "Date" , ylab = "Steps", names.arg = activity_by_day$date)
steps_mean <- mean(activity_by_day$steps)
steps_median <- median(activity_by_day$steps)

activity_by_int <- activity %>% group_by(interval) %>% summarize(steps = mean(steps, na.rm = TRUE))
with(activity_by_int, plot(interval, steps, type = "l", xlab = "5-minutes interval", ylab = "Average number of steps", main = "Average number of steps taken in 5-minute intervals"))

max_int <- filter(activity_by_int, steps == max(activity_by_int$steps))
max_int$interval[1]
max_int$steps[1]


count(filter(activity, is.na(steps)))

na_count <- sum(is.na(activity$steps))
na_count


activity_imp <- activity %>% group_by(interval) %>% mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

activity_by_day_imp <- activity_imp %>% group_by(date) %>% summarize(steps = sum(steps, na.rm = TRUE))
barplot(activity_by_day_imp$steps, main = "Number of steps taken for each day", xlab = "Date" , ylab = "Steps", names.arg = activity_by_day_imp$date)
steps_mean_imp <- mean(activity_by_day_imp$steps)
steps_median_imp <- median(activity_by_day_imp$steps)

