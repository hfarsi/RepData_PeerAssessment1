library(ggplot2)
library(plyr)
library(gcookbook) 

data <- read.csv2("repdata_data_activity/activity.csv", header=TRUE, sep=",", 
                  colClasses = "character")
data$steps <- as.numeric(data$steps)
data$interval <- as.numeric(data$interval)
data1 <- data[!is.na(data$steps),]

dailyTotal <- ddply(data1, "date", summarise, daily_total = sum(steps))
ggplot(dailyTotal, aes(x=date, y=daily_total)) +
    geom_bar(stat="identity", fill="blue", colr="black") +
    ggtitle("Total number of steps taken each day") +
    theme(axis.text.x = element_text(angle=90))
    
summary(dailyTotal$daily_total)[c("Median","Mean")]

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis)

intMean <- ddply(data1, "interval", summarise, Average_Interval = mean(steps))
ggplot(intMean,aes(x=c(1:nrow(intMean)),y=Average_Interval)) + geom_line() + geom_point() +
    xlab("Intervals (288 per day)") + ylab("Average number of steps") +
    ggtitle("Average number of steps taken per interval, across all (61) days")

# Which 5-minute interval, on average across all the days in the dataset, contains 
# the maximum number of steps?
maxval <- intMean[intMean$Average_Interval==max(intMean$Average_Interval),]
round(maxval$Average_Interval,1); rownames(maxval)

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
sum(is.na(data))  # or nrow(data) - nrow(data1)
# Are there any NAs in date or interval columns? 
sum(is.na(data$date))+sum(is.na(data$interval))
# Are any missing interval in intMean as a result of removing NAs
length(unique(data$interval)) == length(intMean$interval)

# What is the size of missing data? scattered over how many and which days?
# Is any of these days have a record in the data se without NA?
miss <- subset(data, is.na(data$steps), drop=F); nrow(miss)
unique(miss$date)
unique(miss$date) %in% data1$date

# Create the new data, replacing NA in steps with the corresponding calculated average 
newdata <- merge(data,intMean,by="interval")
for (i in 1:nrow(newdata)) 
    if (is.na(newdata$steps[i])) newdata$steps[i] <- newdata$Average_Interval[i]
#Dnewdata <- data
#daily_mean <- round(mean(dailyTotal$daily_total),0)
#for (i in 1:nrow(Dnewdata)) 
#    if (is.na(Dnewdata$steps[i])) Dnewdata$steps[i] <- daily_mean "<<<<<< not good"

# New Data set: Plot total steps per day
dailyTotNA <- ddply(newdata, "date", summarise, daily_total_noNA = sum(steps))
ggplot(dailyTotNA, aes(x=date, y=daily_total_noNA)) + 
    geom_bar(stat="identity", fill="blue", colr="black") +
    theme(axis.text.x = element_text(angle=90)) +
    xlab("Date") + ylab("Daily Total Number of Steps") +
    ggtitle("New dataset: Total number of steps taken per day, across all (288) intervals")

# New Data set: Plot average of steps for each interval, accross all days
intMeanNA <- ddply(newdata, "interval", summarise, Average_noNA = mean(steps))
ggplot(intMeanNA,aes(x=c(1:nrow(intMeanNA)),y=Average_noNA)) + geom_line() + geom_point() +
    xlab("Intervals (288 per day)") + ylab("Average number of steps") +
    ggtitle("New dataset: Average number of steps taken per interval, across all (61) days")

#summary(intMean$Average_Interval)
#summary(intMeanNA$Average_noNA)
#summary(dailyTotal$daily_total)
#summary(dailyTotNA$daily_total_noNA)

Inewdata$wkd <- as.factor(weekdays(as.POSIXlt(Inewdata$date, format="%Y-%M-%d")))
levels(Inewdata$wkd)[c(3:4)] <- "Weekend"
levels(Inewdata$wkd)[c(1,2,4,5,6)] <- "Weekday"
wkdayend <- ddply(Inewdata, c("wkd", "interval"), summarise, Int_mean = mean(steps))
ggplot(wkdayend,aes(x=interval, y=Int_mean, color=wkd)) + geom_line(size=1) + 
    geom_point(size=2, shape=21, fill="white") + facet_grid(wkd ~ .) + 
    xlab("Activity Interval (5 mins)") + ylab("Mean of total steps per interval") + 
    ggtitle("Activity Recorded Over Weekdays and Weekends") +
    theme(legend.position="none", strip.text=element_text(face="bold", size=rel(1)),
          strip.background=element_rect(fill="lightblue", colour="black", size=1))

summary(wkdayend[wkdayend$wkd=="Weekday",3])
summary(wkdayend[wkdayend$wkd=="Weekend",3])