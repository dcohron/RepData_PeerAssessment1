# Course Project #1
# Reproducible Research


# globallye suppress warnings
oldwarn<- getOption("warn")
options(warn = -1)

# install libraries/packages
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)

# read file into data frame for manipulation
file<- "activity.csv"
main<- read.csv(file)

# refactor raw date to type date
main<- mutate(main, date=as.Date(as.character(main$date)))

# group by days to get sum of steps per day
# ignore days with no steps recorded
stepsbyday<- group_by(main, date) %>% summarize_each(funs(sum))
na.omit(stepsbyday)

#calculate mean and median of number of steps per day
meansteps<-mean(stepsbyday$steps, na.rm=TRUE)
mediansteps<- median(stepsbyday$steps, na.rm=TRUE)

# plot histogram
hist(stepsbyday$steps,
     main = "Steps per Day",
     xlab = "Sum of steps taken per day",
     ylab = "Number of days at that number of daily steps",
     xlim = c(0, 25000),
     ylim = c(0, 30))

abline(v = meansteps, col = "blue", lwd=4)
abline(v = mediansteps, col = "red", lwd=2)

legend('topright', 
       c("mean", "median"), 
       col=c("blue", "red"), 
       lwd=3)

# group by 5 minute interval
# ignore intervals with no steps recorded
stepsbyinterval<- group_by(main, interval) %>% summarize_each(funs(mean(., na.rm=TRUE)))

# plot time series of 5 minute interval average steps
with(stepsbyinterval, plot(interval, steps, 
                           type = "l",
                           main = "Average Number of Steps Throughout the Day",
                           xlab = "5 Minute Interval (midnight to midnight)",
                           ylab = "Mean Number of Steps",
                           xlim = c(0, 2400),
                           ylim = c(0, 235)))

# calculate which 5 minute interval has highest average number of steps
maxintervalrow<- which.max(stepsbyinterval$steps)
maxinterval<- stepsbyinterval$interval[maxintervalrow]

# calculate the number of rows with missing (NA) data
numrowtotal<- nrow(main)
numrownona<- nrow(na.omit(main))
numrowwithna<- numrowtotal - numrownona

# impute missing values and replace with interval mean
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
mainimputed <- ddply(main, ~ interval, transform, steps = impute.mean(steps))

# group new data frame by days to get sum of steps per day
# all days/intervals have steps recorded
stepsbyday2<- group_by(mainimputed, date) %>% summarize_each(funs(sum))

# calculate mean and median of number of steps per day
meansteps2<-mean(stepsbyday2$steps)
mediansteps2<- median(stepsbyday2$steps)

# plot histogram on imputed data
hist(stepsbyday2$steps,
     main = "Steps per Day",
     xlab = "Sum of steps taken per day",
     ylab = "Number of days at that number of daily steps",
     xlim = c(0, 25000),
     ylim = c(0, 40))

abline(v = meansteps2, col = "blue", lwd=4)
abline(v = mediansteps2, col = "red", lwd=2)

legend('topright', 
       c("mean", "median"), 
       col=c("blue", "red"), 
       lwd=3)

# add columns showing day of the week
stepsbyday3<- mutate(mainimputed, dayofweek = weekdays(date, abbreviate=TRUE))

# add column with logical vector as to weekday/weekend
stepsbyday3<- mutate(stepsbyday3, weekend = (dayofweek == "Sun" | dayofweek == "Sat"))

# subset for weekday and weekend in new data frames
weekdaysteps<- subset(stepsbyday3, !weekend)
weekendsteps<- subset(stepsbyday3, weekend)

# convert logical vector to factors with labels
# Note: I did not need to do this for my analysis,
#       but it was a requirement of the project
stepsbyday3$weekend<-factor(stepsbyday3$weekend,levels=c(FALSE, TRUE), labels=c('Weekday', 'Weekend'))


#group by date
weekdaystepsum<- group_by(weekdaysteps, date) %>% summarize_each(funs(sum(steps)))
weekendstepsum<- group_by(weekendsteps, date) %>% summarize_each(funs(sum(steps)))

# calculate mean and median of number of steps per day
meanstepsweekday<-mean(weekdaystepsum$steps)
medianstepsweekend<- median(weekendstepsum$steps)

# group by 5 minute interval
# ignore intervals with no steps recorded
weekdaystepsbyinterval<- group_by(weekdaysteps, interval) %>% summarize_each(funs(mean(., na.rm=TRUE)))
weekendstepsbyinterval<- group_by(weekendsteps, interval) %>% summarize_each(funs(mean(., na.rm=TRUE)))

# plot time series of 5 minute interval average steps for weekday and weekends
par(mfrow = c(2,1), height = 500)
with(weekdaystepsbyinterval, plot(interval, steps, 
                                  type = "l",
                                  main = "Average Number of Steps on Average Weekday",
                                  xlab = "5 Minute Interval (midnight to midnight)",
                                  ylab = "Mean Number of Steps",
                                  xlim = c(0, 2400),
                                  ylim = c(0, 235)))

with(weekendstepsbyinterval, plot(interval, steps, 
                                  type = "l",
                                  main = "Average Number of Steps on Average Weekend Day",
                                  xlab = "5 Minute Interval (midnight to midnight)",
                                  ylab = "Mean Number of Steps",
                                  xlim = c(0, 2400),
                                  ylim = c(0, 235)))

# restore warnings
options(warn = oldwarn)
