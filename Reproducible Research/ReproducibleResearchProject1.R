library(readr)
library(dplyr)
data <- read.csv("/Users/asudeberber/Downloads/activity.csv", header = TRUE)
View(data)

#What is mean total number of steps taken per day?
mean(data$steps, is.na = TRUE)
data$date <- as.Date(data$date)
meantotalnumberofdailysteps <- data %>%
  group_by(date) %>%
  summarise(daily_total = sum(steps, na.rm = TRUE),
            daily_mean = mean(steps, na.rm = TRUE))
head(meantotalnumberofdailysteps)
view(meantotalnumberofdailysteps)

#hist of that
set.seed(123)
meantotalnumberofdailysteps <- rnorm(1000, mean = 50, sd = 10)
hist(meantotalnumberofdailysteps)

meanview <- mean(meantotalnumberofdailysteps)
mean-> #50.16128
medianview <- median(meantotalnumberofdailysteps$daily_total, na.rm = TRUE)

#What is the average daily activity pattern?
library(ggplot2)
avg_steps <- aggregate(steps ~ interval, data=data, FUN=mean, na.rm=TRUE)
ggplot(data=avg_steps, aes(x=interval, y=steps)) +
  geom_line(color="blue") +
  labs(x="5-minute interval", y="Average number of steps taken") +
  ggtitle("Average Number of Steps per 5-Minute Interval")   

#it can be seen that almost averagely in 900. mınute, number of steps taken is the highest. 



#Imputing missing values
total_missing <- sum(is.na(data))
cat("Total number of missing values:", total_missing, "\n")
#Result: Total number of missing values: 2304 


mean_non_na <- mean(data$steps, na.rm=TRUE)
# Create a new dataset with only NA values
na_data <- data[is.na(data$steps),]
view(na_data)
# Calculate the mean of the NA values
mean_na <- mean(na_data$steps, na.rm=TRUE)
# Print the results
cat("Mean of non-NA values:", mean_non_na, "\n")
cat("Mean of NA values:", mean_na, "\n")
## result: Mean of non-NA values: 37.3826


#Make a histogram of the total number of steps taken each day 
#and Calculate and report the mean and median total number of steps taken per day. 
mean_na <- rnorm(1000, mean = 20, sd = 5)
hist(mean_na, main="Total Number of Steps per Day with only NA values", xlab="interval")
#these values are differ from the first histogram. Here is the highest value for 20. interval time value
#So actually , this is invalid. And to reach the right conclusion, we need to remove NAs and make histogram again. ü



#Are there differences in activity patterns between weekdays and weekends?

# Create a new variable that identifies weekdays and weekends
data$day_type <- ifelse(weekdays(data$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
# Convert the day_type variable to a factor
data$day_type <- factor(data$day_type, levels=c("weekday", "weekend"))
head(data)

# Calculate the average number of steps per interval for weekdays and weekends
weekday_steps <- aggregate(steps ~ interval, data=data[data$day_type=="weekday",], FUN=mean)
weekend_steps <- aggregate(steps ~ interval, data=data[data$day_type=="weekend",], FUN=mean)

ggplot() +
  geom_line(data=weekday_steps, aes(x=interval, y=steps, day_type=="weekday"), color="blue") +
  geom_line(data=weekend_steps, aes(x=interval, y=steps, day_type=="weekend"), color="red") +
  facet_wrap(~ day_type, nrow=2) +
  labs(x="5-Minute Interval", y="Average Number of Steps", title="Average Number of Steps per Interval") +
  theme_bw()
