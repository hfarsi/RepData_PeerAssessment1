RR_Project1 <- function() {
    
  library(ggplot2)
  library(plyr)
  library(gcookbook) 

  data <- read.csv2("repdata_data_activity/activity.csv", header=TRUE, sep=",", 
                  colClasses = "character")
  data$steps <- as.numeric(data$steps)
  data$interval <- as.numeric(data$interval)
  data1 <- data[!is.na(data$steps),]

  # What is mean total number of steps taken per day?
  # Make a histogram of the total number of steps taken each day
  # Calculate and report the mean and median total number of steps taken per day
  dailyTotal <- ddply(data1, "date", summarise, daily_total = sum(steps))
  png(file = "dailyTotal-1.png", width = 680, height = 480)
  p <- ggplot(dailyTotal, aes(x=date, y=daily_total)) +
    geom_bar(stat="identity", fill="blue", colr="black") +
    ggtitle("Total number of steps taken each day") +
    theme(axis.text.x = element_text(angle=90))
  print(p)
  dev.off()
  mean(dailyTotal$daily_total)
  median(arrange(dailyTotal,daily_total)$daily_total)

  # Extra work: wondering if there is a pattern in week days total steps
  # - Calculating mean of daily total for days of a week. This could be used
  #   to replave NAs for total steps table.
  dailyTotal$wkd <- as.factor(weekdays(as.POSIXlt(dailyTotal$date, format="%Y-%M-%d")))
  weekly <- ddply(dailyTotal, "wkd", summarise, wkd_mean = mean(daily_total))
  png(file = "dailyTotal-2.png", width = 480, height = 480)
  p <- ggplot(weekly, aes(x=wkd, y=wkd_mean)) +
    geom_bar(stat="identity", fill="blue", colr="black") + 
    xlab("Week Days") + ylab("Average of daily total") +
    ggtitle("Average of daily total number of steps by days of the week") 
  print(p)
  dev.off()

  # summary(dailyTotal$daily_total)[c("Median","Mean")] would have worked as well 
  # but for some reason knit is not displaying the correct value. 

  # Daily activity pattern:
  # Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
  # the average number of steps taken, averaged across all days (y-axis)

  intMean <- ddply(data1, "interval", summarise, Average_Interval = mean(steps))
  png(file = "intervalMean-1.png", width = 680, height = 480)
  p <- ggplot(intMean,aes(x=c(1:nrow(intMean)),y=Average_Interval)) + geom_line() + geom_point() +
    xlab("Intervals (288 per day)") + ylab("Average number of steps") +
    ggtitle("Average number of steps taken per interval, across all (61) days")
  print(p)
  dev.off()

  # Which 5-minute interval, on average across all the days in the dataset, contains 
  # the maximum number of steps?

  maxval <- intMean[intMean$Average_Interval==max(intMean$Average_Interval),]
  peak <- round(maxval[2],1)
  interval <- rownames(maxval)
  time <- round(as.numeric(interval)/12,0)
  peak; interval; time

  # Calculate and report the total number of missing values in the dataset 
  # (i.e. the total number of rows with NAs)
  # What is the size of missing data? scattered over which days of the week?

  miss_obs <- subset(data, is.na(data$steps), drop=F) 
  uniq_missing_date <- unique(miss_obs$date)
  wday <- as.factor(weekdays(as.POSIXlt(uniq_missing_date, format="%Y-%M-%d")))
  nrow(miss_obs)
  uniq_missing_date
  wday

  # Create the new data, replacing NA in steps with the corresponding calculated average 
  newdata <- merge(data,intMean,by="interval")
  for (i in 1:nrow(newdata)) 
    if (is.na(newdata$steps[i])) newdata$steps[i] <- newdata$Average_Interval[i]

  # New Data set: Plot total steps per day
  # Calculate and report mean and median of the daily total.
  dailyTotNA <- ddply(newdata, "date", summarise, daily_total_noNA = sum(steps))
  png(file = "dailyTotal-3.png", width = 680, height = 480)
  p <- ggplot(dailyTotNA, aes(x=date, y=daily_total_noNA)) + 
    geom_bar(stat="identity", fill="blue", colr="black") +
    theme(axis.text.x = element_text(angle=90)) +
    xlab("Date") + ylab("Daily Total Number of Steps") +
    ggtitle("New dataset: Total number of steps taken per day, across all (288) intervals")
  print(p)
  dev.off()
  mean(dailyTotNA$daily_total_noNA)
  median(arrange(dailyTotNA,daily_total_noNA)$daily_total_noNA)

  # New Data set: Plot average of steps for each interval, accross all days
  intMeanNA <- ddply(newdata, "interval", summarise, Average_noNA = mean(steps))
  png(file = "intervalMean-2.png", width = 680, height = 480)
  p <- ggplot(intMeanNA,aes(x=c(1:nrow(intMeanNA)),y=Average_noNA)) + geom_line() + geom_point() +
    xlab("Intervals (288 per day)") + ylab("Average number of steps") +
    ggtitle("New dataset: Average number of steps taken per interval, across all (61) days")
  print(p)
  dev.off()

  # Are there differences in activity patterns between weekdays and weekends?
  # Use the dataset with the filled-in missing values for this part.
  # Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
  # indicating whether a given date is a weekday or weekend day.
  # Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
  # interval (x-axis) and the average number of steps taken, averaged across all weekday 
  # days or weekend days (y-axis).
  newdata$wkd <- as.factor(weekdays(as.POSIXlt(newdata$date, format="%Y-%M-%d")))
  levels(newdata$wkd)[c(3:4)] <- "Weekend"
  levels(newdata$wkd)[c(1,2,4,5,6)] <- "Weekday"
  wkdayend <- ddply(newdata, c("wkd", "interval"), summarise, Int_mean = mean(steps), sd=sd(steps))
  wkdayend$INTERVAL <- rep(1:288,2)
  png(file = "weekdays-1.png", width = 680, height = 480)
  p <- ggplot(wkdayend,aes(x=INTERVAL, y=Int_mean, color=wkd)) + geom_line(size=1) + 
    geom_point(size=2, shape=21, fill="white") + facet_grid(wkd ~ .) + 
    xlab("Activity Interval (5 mins)") + ylab("Mean of total steps per interval") + 
    ggtitle("Activity Recorded Over Weekdays and Weekends") +
    theme(legend.position="none", strip.text=element_text(face="bold", size=rel(1)),
          strip.background=element_rect(fill="lightblue", colour="black", size=1))
  print(p)
  dev.off()
  # Remove noise due to inactive collection intervals between late evening and early morning.
  # Compare intensity of activities between weekday and weekends in a boxplot.
  highActive <- wkdayend[wkdayend$INTERVAL>70 & wkdayend$INTERVAL<260,]
  png(file = "weekdays-2.png", width = 480, height = 480)
  p <- ggplot(highActive, aes(x=wkd, y=Int_mean)) + geom_boxplot()
  print(p)
  dev.off()
}