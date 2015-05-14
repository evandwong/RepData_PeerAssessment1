
## Loading data
options(scipen=9)
options(digits=2)
data <- read.csv("activity.csv")

## Steps by day
steps_by_day <- aggregate(.~date, data=data, FUN=sum)

hist(steps_by_day$steps, xlab="Total Steps Per Day", breaks=10)

mean_steps_by_day <- mean(steps_by_day$steps)
median_steps_by_day <- median(steps_by_day$steps)



## Daily Activity Pattern
steps_by_interval <- aggregate(.~interval, data=data, FUN=mean)

plot(steps_by_interval$steps, type="l")

maximum_daily_interval <- max(steps_by_interval$steps)



##Imputation
complete_records <- subset(data, !is.na(data$steps))

mean_complete <- mean(complete_records$steps)

impute_mean <- function(x) if (is.na(x)) mean_complete else x

data_imputed <- data

data_imputed$steps <- sapply(data_imputed$steps, impute_mean)

imputed_steps_by_day <- aggregate(.~date, data=data_imputed, FUN=sum)

hist(imputed_steps_by_day$steps, breaks=10)

imputed_mean_steps_by_day <- mean(imputed_steps_by_day$steps)

## Note that the median is now equal to the mean, and a non-integer
imputed_median_steps_by_day <- median(imputed_steps_by_day$steps)



## Weekday vs weekend
data_weekday <- data_imputed

data_weekday$date <- as.Date(data_weekday$date, format = "%Y-%m-%d")

weekender <- function(x){
   if (weekdays(x) == "Saturday" | weekdays(x) == "Sunday") "weekend"
   else "weekday"
}

data_weekday$weekend <- sapply(data_weekday$date, weekender)
data_weekday$weekend <- as.factor(data_weekday$weekend)

data_by_weekend <- aggregate(steps ~ interval + weekend, data=data_weekday, FUN=mean)


library(lattice)
xyplot(steps~interval | weekend, data=data_by_weekend, type="l")
