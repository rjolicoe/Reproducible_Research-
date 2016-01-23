#First load the activity dataset into your working directory. Once that is in then:
#Here we are using the loading the packages that we will utilize here during our analysis


library(ggplot2)



library(plyr)



library(knitr)
opts_chunk$set(echo = TRUE)


library(sqldf)


library(tcltk)

#Read the file in utilizing:
Activity<- read.csv('activity.csv', header = TRUE, sep = ",",
                    colClasses=c("numeric", "character", "numeric"))


#Lets go ahead and transform the data frame columns "date" into a date format and interval into a character format

Activity$datetime <- as.POSIXct(Activity$date, format = "%Y-%m-%d")
Activity$weekday <- weekdays(as.Date(Activity$date))




#From here lets utilize the sqldf package to find the sum of steps by days
Remove_NAS <- Activity[!is.na(Activity$steps),]
daily_steps <- sqldf("select date, sum(steps) as steps from Clean group by date")
colnames(daily_steps) <- c("date", "steps")


#Now we will construct the histogram of average steps by day
steps_plot <- function() {
steps <- ggplot(daily_steps, aes(steps))
steps <- steps+geom_histogram(binwidth=5000,fill="blue", alpha = 0.5)+labs(y="days observed", x="Number of steps per day", 
                                                                           title = "Number of steps taken per day")
plot(steps)
dev.copy(png, file="First_Histogram.png", width=480, height=480)
dev.off()
cat("First_Histogram.png",getwd())
}
steps_plot()
  
#Now that we have aggregated the steps by days and created daily_steps we can calculate the mean and the median
average_steps <- mean(daily_steps$steps)
#Utilizing the round function here to get this value as an integer value, I could use as integer but I use round more frequently
round(average_steps)  



median_steps <-  median(daily_steps$steps)

#Next lets create a function to find the number of steps taken per five minute interval early in the morning

intervals <- ddply(Activity, .(interval), summarize, Avg = mean(steps, na.rm=T))
Average_Steps_plot <- function(){
  avg_steps <- ggplot(intervals, aes(x=interval, y=Avg), xlab="Interval", ylab="Average number of steps")
  avg_steps + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
  dev.copy(png,file="Average_Steps_plot.png",width=480, height=480)
  dev.off()
  cat("Average_Steps_plot.png",getwd())
}
Average_Steps_plot()

#Next we want to determine the maximum number of steps per interval
Max <- max(intervals$Avg)
intervals[intervals$Avg==Max,1]
as.integer(Max) # This tells us which interval contains the max steps and what that average Max is in


#Now we need to impute for our missing step  where i show the true and false values
NAS <- sum(!complete.cases(Activity))

#Similar to the interval calculations, we need to take the average steps per weekday and intervals
imput <- transform(Activity, steps=ifelse(is.na(Activity$steps), intervals$Avg[match(Activity$interval,intervals$interval)],Activity$steps))


imput[as.character(imput$date)=="2012-10-01",1]<- 0

NAS2 <- sum(!complete.cases(steps_per_day))
#Here is a check to make sure all the nas have been removed from the data

#Again here i will utilize the sqldf function to aggreate the data
steps_per_day<- sqldf("select date, sum(steps) as steps from imput where steps >= 0 group by date")
imputed_steps <- function(){
  
hist(steps_per_day$steps, col="blue", breaks=5, main= "Total Steps per day", xlab="Steps Per Day")
dev.copy(png,file="imputed_steps.png", width=480, height=480)
dev.off()
cat("Imputed_steps.png", getwd())
}
imputed_steps()

mean_difference <- mean(steps_per_day$steps) - mean(daily_steps$steps)
median_difference <- median(steps_per_day$steps) - median(daily_steps$steps)

mean_difference
median_difference

summary(steps_per_day$steps)

#Here is where I put a binary indicator to detect if it is a weekday or weekend.  A Weekend is given a 1
#A weekday is given a zero

Day_indicator <- sqldf("select *, case
               when weekday like '%Saturday%' then 'Weekend'
               when weekday like '%Sunday%' then 'Weekend'
               else 'Weekday' end Indicator from imput")

table(Day_indicator$Indicator)

#Here I will load the lattice package to be able to graph the average steps by weekday and weekend
library(lattice)

Day_type_comparison <- ddply(Day_indicator,.(interval,Indicator), summarize, Avg=mean(steps))

#Now we will be able to plot these values to see the differences in weekends vs weekdays

Day_type_comparison_plot <- function(){
xyplot(Avg~interval|Indicator, data=Day_type_comparison, 
       type="l", layout=c(1,2), main="Average Number of steps across interval and day indicator type", 
       ylab = "Average Number of steps taken", xlab="interval")
 dev.copy(png,file="Day_type_comparison.png", width=480, height=480) 
 dev.off()
 cat("Day_type_comparison.png",getwd())
}
Day_type_comparison_plot()

#From the data we are able to see that yes there is a difference.  TYpically during the week most people with sedentary
#office jobs don't move around much.  During the weekend they tend to move around more

