---
title: "PA1_Template"
title2: "Reproducible Research Peer Assessment #1"
author: "Ryan Jolicoeur"
date: "January 23, 2016"
output: html_document
---


First load the activity dataset into your working directory and load the packages you will be working with:


```{r}
library(ggplot2)



library(plyr)



library(knitr)
opts_chunk$set(echo = TRUE, eval=TRUE)


library(sqldf)


library(tcltk)

Activity<- read.csv('activity.csv', header = TRUE, sep = ",",
                    colClasses=c("numeric", "character", "numeric"))


#Lets go ahead and transform the data frame columns "date" into a date format and interval into a character format

Activity$datetime <- as.POSIXct(Activity$date, format = "%Y-%m-%d")
Activity$weekday <- weekdays(as.Date(Activity$date))
```



From here lets utilize the sqldf package to find the sum of steps by days. I utilize sqldf package to aggregate the 
data together as well as to remove the missing values here:
```{r}
Remove_NAS <- Activity[!is.na(Activity$steps),]
daily_steps <- sqldf("select date, sum(steps) as steps from Remove_NAS group by date")
colnames(daily_steps) <- c("date", "steps")
```

Now we will construct the histogram of average steps by day:

```{r}
steps <- ggplot(daily_steps, aes(steps))
steps <- steps+geom_histogram(binwidth=5000,fill="blue", alpha = 0.5)+labs(y="days observed", x="Number of steps per day", 
 title = "Number of steps taken per day")
 
plot(steps)
```
  
Now that we have aggregated the steps by days and created daily_steps we can calculate the mean and the median:

```{r}
average_steps <- mean(daily_steps$steps)
round(average_steps)  

median_steps <-  median(daily_steps$steps)
median_steps
```

Next lets create a function to find the number of steps taken per five minute interval early in the morning:

```{r}
intervals <- ddply(Activity, .(interval), summarize, Avg = mean(steps, na.rm=T))
  avg_steps <- ggplot(intervals, aes(x=interval, y=Avg), xlab="Interval", ylab="Average number of steps")
  avg_steps + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```  

Next we want to determine the maximum number of steps per interval:

```{r}
Max <- max(intervals$Avg)
intervals[intervals$Avg==Max,1]
as.integer(Max) # This tells us which interval contains the max steps and what that average max is in
```

Now we need to impute for our missing step to remove all the NAS by imputing our data.
```{r}
NAS <- sum(!complete.cases(Activity))

#Similar to the interval calculations, we need to take the average steps per weekday and intervals
imput <- transform(Activity, steps=ifelse(is.na(Activity$steps), intervals$Avg[match(Activity$interval,intervals$interval)],Activity$steps))


imput[as.character(imput$date)=="2012-10-01",1]<- 0

NAS2 <- sum(!complete.cases(imput))
NAS2
```

```{r}
#Again here i will utilize the sqldf function to aggreate the data
steps_per_day<- sqldf("select date, sum(steps) as steps from imput where steps >= 0 group by date")

hist(steps_per_day$steps, col="blue", breaks=5, main= "Total Steps per day", xlab="Steps Per Day")
```

From here we need to be able to calculate the mean and median difference of steps taken from when the missings were
removed:

```{r}
mean_difference <- mean(steps_per_day$steps) - mean(daily_steps$steps)
median_difference <- median(steps_per_day$steps) - median(daily_steps$steps)

mean_difference
median_difference

summary(steps_per_day$steps)
```
This result was a little suprising the variation between the mean with and without the NAS included.  After running through the result a few times I didn't see any changes that I would make to the aggregation function once the NAS were removed from our "steps per day calculation".

Next is where I utilize the sqldf to reflect the case statement here.  If the days are coded as a Saturday or a Sunday then they are a weekend. If not then it is a weekday.  

```{r}
Day_indicator <- sqldf("select *, case
               when weekday like '%Saturday%' then 'Weekend'
               when weekday like '%Sunday%' then 'Weekend'
               else 'Weekday' end Indicator from imput")

table(Day_indicator$Indicator)
```

Here I will load the lattice package to be able to graph the average steps by weekday and weekend by panel graphing:
```{r}
library(lattice)

Day_type_comparison <- ddply(Day_indicator,.(interval,Indicator), summarize, Avg=mean(steps))
```
Now we will be able to plot these values to see the differences in weekends vs weekdays:

```{r}
xyplot(Avg~interval|Indicator, data=Day_type_comparison, 
       type="l", layout=c(1,2), main="Average Number of steps across interval and day indicator type", 
       ylab = "Average Number of steps taken", xlab="interval")
```

From the data we are able to see that yes there is a difference.  Typically during the week most people with sedentary
office jobs don't move around much.  During the weekend they tend to move around more frequently throughout the day.  

