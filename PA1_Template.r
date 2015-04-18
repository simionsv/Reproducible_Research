


library(dplyr)
library(ggplot2)

# Read csv file from the directory and also eliminate the NA values.
activity <- read.csv("C:/Users/Sergio Simioni/Desktop/Data_Science/activity.csv", sep=";")
head(activity)
dim(activity)
steps<- na.omit(activity)
head(steps)
dim(steps)


#Calculate the total number of steps taken per day
steps_per_day<- aggregate( steps$steps, by=list(steps$date), FUN = sum)
g<- ggplot(steps_per_day, aes(x))+labs(title="Total Number of Steps Taken per Day")+
  xlab("") + ylab("frequency")
p<- g + geom_histogram(binwidth=5000,fill="red") 
print(p)

#calculate the mean and the median of the total number of steps taken per day.
print(mean(steps_per_day$x))
print(median(steps_per_day$x))

#Calculate the average activity pattern
steps_by_interval<- aggregate( steps$steps, by=list(steps$interval), FUN = mean)
g<- ggplot(steps_by_interval, aes(Group.1, x))+labs(title="Average Number of Steps Taken Across All Days")+
  xlab("5 minutes Interval") + ylab("number of steps")
p<- g + geom_line() 
print(p)


#calculate the 5 minutes interval that contains the maximum number of steps.
max_steps<-filter(steps_by_interval, x == max(x))
print(max_steps[,1])



#replacing all NAs by the mean of the steps per day
steps_a<- mean(steps$steps )
activity_a<- activity
a<- nrow(activity_a)
for ( i in 1:a){
  if (is.na(activity_a[i,1])){
    activity_a[i,1] = steps_a
  }else{
    activity_a[i,1] = activity_a[i,1]
  }
}

#Calculate the total number of steps taken per day
steps_per_day_replacing_NAs<- aggregate( activity_a$steps, by=list(activity_a$date), FUN = sum)
head(steps_per_day)
head(steps_per_day_replacing_NAs)
g<- ggplot(steps_per_day_replacing_NAs, aes(x))+labs(title="Total Number of Steps Taken per Day Replacing NAs")+
        xlab("") + ylab("frequency")
p<- g + geom_histogram(binwidth=5000,fill="red") 
print(p)

#calculate the median and mean and compare the table with and without NAs.
print(mean(steps_per_day_replacing_NAs$x))
print(median(steps_per_day_replacing_NAs$x))



#calculating the difference between weekdays and weekdends
week<- activity_a
week$date<- as.Date(week$date, format="%d/%m/%Y")

week<- mutate(week, weekday = weekdays(date))

weekday <- week[which( week$weekday == "segunda-feira" | week$weekday =="terça-feira"
                       | week$weekday == "quarta-feira" | week$weekday =="quinta-feira"
                       | week$weekday == "sexta-feira"),]

weekend <- week[which( week$weekday == "sábado" | week$weekday =="domingo"),]

#aggregate the average of the steps by interval in weekday
steps_by_interval_weekday<- aggregate( weekday$steps, by=list(weekday$interval), FUN = mean)
steps_by_interval_weekday<- mutate(steps_by_interval_weekday, weekday = "weekday")


#aggregate the average of the steps by interval in weekend
steps_by_interval_weekend<- aggregate( weekend$steps, by=list(weekend$interval), FUN = mean)
steps_by_interval_weekend<- mutate(steps_by_interval_weekend, weekday = "weekend")

# combine the table from weekends and weekdays
general_steps<- rbind (steps_by_interval_weekday,steps_by_interval_weekend)


#plot using ggplot2 the average of the steps by interval in weekday
g<- ggplot(general_steps, aes(Group.1, x))+labs(title="Mean of the Steps by Interval split by Weekdays and Weekends")+
  xlab("5 minutes Interval") + ylab("number of steps")
weekday<- g + geom_line() +facet_grid(weekday~.)
print(weekday)




