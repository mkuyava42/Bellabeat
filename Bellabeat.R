# Libraries utilized in data cleaning, organization and analysis
library(tidyverse)
library(dplyr)
library(here)
library(janitor)
library(lubridate)
library(skimr)
library(ggplot2)
library(gridExtra)

#Data set import
daily_activity <- read.csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
str(daily_activity)
daily_sleep <- read.csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
str(daily_sleep)
hourly_calories <- read.csv("Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
str(hourly_calories)
hourly_intensity <- read.csv("Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
str(hourly_intensity)
hourly_steps <- read.csv("Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
str(hourly_steps)
weight <- read.csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
str(weight)

#Checking all files for duplicates and missing values.
sum(duplicated(daily_activity))
sum(is.na(daily_activity))
sum(duplicated(daily_sleep))
sum(is.na(daily_sleep))
sum(duplicated(hourly_calories))
sum(is.na(hourly_calories))
sum(duplicated(hourly_intensity))
sum(is.na(hourly_intensity))
sum(duplicated(hourly_steps))
sum(is.na(hourly_steps))
sum(duplicated(weight))

#Remove duplicates from daily_sleep and recheck
daily_sleep <- daily_sleep[!duplicated(daily_sleep),]
sum(duplicated(daily_sleep))

#Setting date/time formats on all data to be uniform
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format = "%m/%d/%Y")
daily_activity <- daily_activity %>% rename(Date = ActivityDate)
daily_sleep$SleepDay <- as.Date(daily_sleep$SleepDay, format = "%m/%d/%Y")
daily_sleep <- daily_sleep  %>% rename(Date = SleepDay)
weight$Date <-  as.Date(weight$Date, format = "%m/%d/%Y")
hourly_intensity$ActivityHour <- mdy_hms(hourly_intensity$ActivityHour)
hourly_intensity$Hour <- hour(hourly_intensity$ActivityHour)
hourly_steps$ActivityHour <- mdy_hms(hourly_steps$ActivityHour)
hourly_steps$Hour <- hour(hourly_steps$ActivityHour)
hourly_calories$ActivityHour <- mdy_hms(hourly_calories$ActivityHour)
hourly_calories$Hour <- hour(hourly_calories$ActivityHour)

# Daily Activity and Daily Sleep
daily_all_merged <- merge(daily_activity, daily_sleep, by = c("Id", "Date"))

# Hourly Calories and Hourly Intensity
hourly_calories_intensity <- merge(hourly_calories, hourly_intensity, by = c("Id", "ActivityHour", "Hour"))
hourly_calories_steps <- merge(hourly_calories, hourly_steps, by = c("Id", "ActivityHour", "Hour"))

# Merged Hourly Calories)Intensity and Hourly Steps
hourly_all_merged <- merge(hourly_calories_intensity, hourly_calories_steps, by = c("Id", "ActivityHour", "Hour"))

#Distance
daily_all_merged %>%
  select(Steps_Total = TotalSteps,
         Distance_Total = TotalDistance,
         High_Activity = VeryActiveDistance,
         Mid_Activity = ModeratelyActiveDistance,
         Low_Activity = LightActiveDistance) %>%
  summary()

print(paste("Steps_Total:             ",round(sd(daily_all_merged$TotalSteps), digits = 4)))
print(paste("Distance_Total:          ",round(sd(daily_all_merged$TotalDistance), digits = 4)))
print(paste("High_Activity_Distance:  ",round(sd(daily_all_merged$VeryActiveDistance), digits = 4)))
print(paste("Mid_Activity_Distance:   ",round(sd(daily_all_merged$ModeratelyActiveDistance), digits = 4)))
print(paste("Low_Activity_Distance:   ",round(sd(daily_all_merged$LightActiveDistance), digits = 4)))

#Time
daily_all_merged %>%
  select(High_Active_Min = VeryActiveMinutes,
         Mid_Activive_Min = FairlyActiveMinutes,
         Low_Activive_Min = LightlyActiveMinutes,
         Sedentary_Min = SedentaryMinutes,
         Asleep_Min = TotalMinutesAsleep,
         InBed_Min = TotalTimeInBed,
         Calorie = Calories) %>%
  summary()

print(paste("High_Activity_Minutes:  ",round(sd(daily_all_merged$VeryActiveMinutes), digits = 4)))
print(paste("Mid_Activity_Minutes:   ",round(sd(daily_all_merged$FairlyActiveMinutes), digits = 4)))
print(paste("Low_Activity_Minutes:   ",round(sd(daily_all_merged$LightlyActiveMinutes), digits = 4)))
print(paste("Sedentary_Minutes:      ",round(sd(daily_all_merged$SedentaryMinutes), digits = 4)))
print(paste("Asleep_Minutes:         ",round(sd(daily_all_merged$TotalMinutesAsleep), digits = 4)))
print(paste("InBed_Minutes:          ",round(sd(daily_all_merged$TotalTimeInBed), digits = 4)))
print(paste("Calories Burned:        ",round(sd(daily_all_merged$Calories), digits = 4)))

#Values variables for activity levels DF
activity_low <- sum(daily_all_merged$TotalSteps <= 5000, na.rm = TRUE)
activity_mid <- sum(daily_all_merged$TotalSteps > 5000 & daily_all_merged$TotalSteps < 10000, na.rm = TRUE)
activity_high<- sum(daily_all_merged$TotalSteps >= 10000, na.rm = TRUE)
activity_levels <- data.frame(
  c(activity_low, activity_mid, activity_high),
  c(((activity_low)/(activity_low + activity_mid + activity_high)*100),
    ((activity_mid)/(activity_low + activity_mid + activity_high)*100),
    (activity_high)/(activity_low + activity_mid + activity_high)*100),
  c("Less than 5000", "More than 5000 and Less than 1000", "Equal or Greater than 10000" ))
rownames(activity_levels) <- c("Low Steps", "Mid Steps", "High Steps")
colnames(activity_levels) <- c("Count", "Percentage", "Steps")
activity_levels

#Correlation DF and Matrix with Standard deviation 
correlation <- daily_all_merged[c(3,4,11:14,15,17,18)]
correlation_maxtrix <- cor(correlation, use="complete.obs")

#Sleep correlation DF
correlation_sleep <- data.frame(correlation_maxtrix[,8])
colnames(correlation_sleep) <- "Correlation of Minutes Asleep"
correlation_sleep

##Graphical representation of the relationship between Sedentary Minutes and Asleep Minutes
daily_all_merged %>%
  group_by(SedentaryMinutes, TotalMinutesAsleep) %>% #Graphical representation of the relationship between
  ggplot(aes(x = SedentaryMinutes, y = TotalMinutesAsleep, color = TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth(color = "Orange") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "italic"),
        legend.position = c(.8, .8),
        legend.spacing.y = unit(2, "mm"),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))  +
  labs(title = 'Sleep & Sedentary Time',
       subtitle = "Minutes Alseep decrease as Sedentary Minutes increase",
       y = 'Minutes Asleep',
       x = 'Sedentary Minutes')

#Calorie  correlation DF
correlation_calories <- data.frame(correlation_maxtrix[,7])
colnames(correlation_calories) <- "Correlation of Calories"
correlation_calories

#Calorie graphs
calorie_distance_plot <- ggplot(data=daily_all_merged, aes(x=TotalDistance, y=Calories, colour=TotalDistance))+
  geom_point() + geom_smooth() +
  ggtitle("Calories & Distance") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

calorie_steps_plot <- ggplot(data=daily_all_merged, aes(x=TotalSteps, y=Calories, colour=TotalSteps))+
  geom_point() + geom_smooth() +
  ggtitle("Calories & Steps") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

grid.arrange(calorie_distance_plot, calorie_steps_plot, nrow=2)


#Hourly activity histogram
hourly_all_merged %>%
  group_by(Hour) %>%
  ggplot(data = hourly_all_merged, mapping = aes(x=Hour,y=AverageIntensity)) + geom_histogram(stat = "identity", fill="Blue")+
  geom_hline(aes(yintercept = 300), color="green" ) +
  geom_hline(aes(yintercept = 250)) +
  geom_hline(aes(yintercept = 200), color="yellow" ) +
  geom_hline(aes(yintercept = 150)) +
  geom_hline(aes(yintercept = 100), color="red" ) +
  geom_hline(aes(yintercept = 50)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Active Time of Day") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
active_days <- daily_all_merged %>%
  mutate(week_day = weekdays(Date), TotalSteps, TotalDistance)
active_days$week_day <- ordered(active_days$week_day, levels =
                                  c("Monday", "Tuesday", "Wednesday", "Thursday",
                                    "Friday", "Saturday", "Sunday"))

#Active Day graphs
active_day <- active_days  %>%
  group_by(week_day) %>%
  summarize(daily_steps = mean(TotalSteps), daily_distance = mean(TotalDistance))
active_day

daily_steps_plot = ggplot(data = active_day, mapping = aes(x=week_day,y=daily_steps)) + geom_col(fill="Blue" )+
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Daily Steps") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

daily_steps_distance = ggplot(data = active_day, mapping = aes(x=week_day,y=daily_distance)) + geom_col(fill="Blue" )+
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Daily Distance") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))

grid.arrange(daily_steps_plot, daily_steps_distance, nrow=1)

