#load data
raw <- read.csv("activity.csv", header=TRUE, 
            colClasses=c("numeric", "Date", "numeric"))

#remove NAs
activity <- na.exclude(raw)

#What is the mean total number of steps taken per day?
agg_date <-aggregate(steps ~ date, data=activity, sum)

hist(agg_date$steps, 
     freq=TRUE,
     col="blue", 
     xlab="Number of Steps Taken per Day",
     ylab="Frequency",
     main="Total Number of Steps Taken per Day"
)

mean(agg_date$steps)
median(agg_date$steps)

#What is the average daily activity pattern?
agg_int <- aggregate(steps ~ interval, data=raw, mean)

plot(agg_int$interval, agg_int$steps, type="l",
     xlab="Interval",
     ylab="Average Number of Steps")


agg_int[round(agg_int$steps,4)==round(max(agg_int$steps),4), ]

#Imputing missing values
nrow(raw) - nrow(activity)

full <- raw
str(full)

raw$steps[5000]
full$steps[5000]


#assign mean for interval to missing value
for (n in 1:nrow(full)) {
        if (is.na(full$steps[n])) 
                {full$steps[n] <- agg_int$steps[agg_int$interval==full$interval[n]] }
}

agg_full <- aggregate(steps ~ date, data=full, sum)

hist(agg_full$steps, 
     freq=TRUE,
     col="blue", 
     xlab="Number of Steps Taken per Day",
     ylab="Frequency",
     main="Total Number of Steps Taken per Day",
     sub="(with Imputed Values for Missing Data)"
)

mean(agg_full$steps)
median(agg_full$steps)

#Are there differences in activity patterns between weekdays and weekends?
#add weekday 
full$weekday <- as.factor(weekdays(full$date))
str(full)

agg_weekend <- aggregate(steps ~ interval, data=subset(full, weekday=="Saturday" | weekday=="Sunday"), mean)
agg_weekday <- aggregate(steps ~ interval, data=subset(full, weekday!="Saturday" & weekday!="Sunday"), mean)

par(mfrow=c(2,1))

plot(agg_weekend$interval, agg_weekend$steps, type="l",
     xlab="Interval",
     ylab="Average Number of Steps",
     main="Weekend")
plot(agg_weekday$interval, agg_weekday$steps, type="l",
     xlab="Interval",
     ylab="Average Number of Steps",
     main="Weekday")

