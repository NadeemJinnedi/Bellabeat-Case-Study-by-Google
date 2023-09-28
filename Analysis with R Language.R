# Import required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)            
library(dplyr)              
library(skimr) 
library(sqldf)
library(janitor)
library(plotrix)

# Import Data
daily_activity <- read_csv("dailyActivity_merged.csv")
sleep_per_day <- read_csv("sleepDay_merged.csv")
weight_log_info <- read_csv("weightLogInfo_merged.csv")

# ----- Analysis of Data ----- 

# Dataset of daily activities
str(daily_activity)
skim(daily_activity)
head(daily_activity)
glimpse(daily_activity)

# Dataset of sleep per day
str(sleep_per_day)
skim(sleep_per_day)
head(sleep_per_day)
glimpse(sleep_per_day)

# Dataset of weight log info
str(weight_log_info)
skim(weight_log_info)
head(weight_log_info)
glimpse(weight_log_info)

# Data Preparation
daily_activity$Rec_Date <- as.Date(daily_activity$ActivityDate,"%m/%d/%y")
daily_activity$month <- format(daily_activity$Rec_Date,"%B")
daily_activity$day_of_week <- format(daily_activity$Rec_Date,"%A")

n_distinct(daily_activity$Id)

# Summary of data
daily_activity %>%  select(TotalSteps,TotalDistance,SedentaryMinutes,VeryActiveMinutes) %>% summary()
weight_log_info %>%  select(WeightKg,BMI) %>% summary()

# To find average sleeping time in minutes
Avg_minutes_asleep <- sqldf("SELECT SUM(TotalSleepRecords),SUM(TotalMinutesAsleep)/SUM(TotalSleepRecords) as avg_sleeptime
                            FROM sleep_per_day")
Avg_minutes_asleep

# To find average bed time in minutes
Avg_TimeInBed <- sqldf("SELECT SUM(TotalTimeInBed)/SUM(TotalSleepRecords) as avg_timeInBed
                       FROM sleep_per_day")
Avg_TimeInBed

n_distinct(sleep_per_day$Id)
n_distinct(weight_log_info$Id)

# ----- Data Visualizations -----

# No. of times users used tracker across week
daily_activity$day_of_week <- ordered(daily_activity$day_of_week,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

ggplot(data=daily_activity) + geom_bar(mapping = aes(x=day_of_week),fill="#008080") +
  labs(x="Day of week",y="Count",title="No. of times users used tracker across week") +
  theme(panel.background = element_rect(fill = "White"))

mean_steps <- mean(daily_activity$TotalSteps)
mean_calories <- mean(daily_activity$Calories)
mean_steps
mean_calories

# Calories burned for every step taken
ggplot(data=daily_activity) + geom_point(mapping=aes(x=TotalSteps, y=Calories, color=Calories), color="#5F9EA0") +
  geom_hline(mapping = aes(yintercept=mean_calories),color="yellow",lwd=1.0) +
  geom_vline(mapping = aes(xintercept=mean_steps),color="red",lwd=1.0) +
  geom_text(mapping = aes(x=10000,y=500,label="Average Steps",srt=-90)) +
  geom_text(mapping = aes(x=29000,y=2500,label="Average Calories")) +
  labs(x="Steps Taken",y="Calories Burned",title = "Calories burned for every step taken") +
  theme(panel.background = element_rect(fill = "ghost white"))

# Total Steps vs Sedentary Minutes
ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes, color = Calories)) + geom_point() +
  geom_smooth(method = "loess",color="green") + 
  labs(x="Total Steps",y="Sedentary Minutes",title="Total Steps vs Sedentary Minutes")

# Sleep Time vs Bed Time
ggplot(data=sleep_per_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point(color="#5F9EA0") + stat_smooth(method = lm, color = "Red") +
  labs(x="Total Minutes a sleep", y="Total Time in Bed", title = "Sleep Time vs Bed Time")+
  theme(panel.background = element_rect(fill = "ghost white"))

# Very Active Minutes vs Calories Burned
ggplot(data=daily_activity,aes(x = VeryActiveMinutes, y = Calories, color = Calories)) + geom_point(color="#5F9EA0") + 
  geom_smooth(method = "loess",color="red") +
  labs(x="Very Active Minutes",y="Calories",title = "Very Active Minutes vs Calories Burned") +
  theme(panel.background = element_rect(fill = "ghost white"))

# Calories vs. Sedentary Minutes
ggplot(data=daily_activity,aes(x=SedentaryMinutes,y=Calories,color=Calories)) + geom_point(color="#5F9EA0") + 
  geom_smooth(method="loess",color="red") + 
  labs(y="Calories", x="Sedentary Minutes", title="Calories vs. Sedentary Minutes")+
  theme(panel.background = element_rect(fill = "ghost white"))

# Calculating the sum of individual minute
activity_min <- sqldf("SELECT SUM(VeryActiveMinutes),SUM(FairlyActiveMinutes),
      SUM(LightlyActiveMinutes),SUM(SedentaryMinutes)
      FROM daily_activity")
activity_min

# Percentage of Activity in Minutes
x <- c(19895,12751,181244,931738)
piepercent <- round(100*x / sum(x), 1)
colors = c("#FF0000","#00FF00","#FFFF00","#800080")
pie3D(x,labels = paste0(piepercent,"%"),col=colors,main = "Percentage of Activity in Minutes")
legend("bottomright",c("Very Active Minutes","Fairly Active Minutes","Lightly Active Minutes","Sedentary Minutes"),cex=0.75,fill = colors)

# Calculating the sum of different distance values
activity_dist <- sqldf("SELECT SUM(ModeratelyActiveDistance),SUM(LightActiveDistance),
      SUM(VeryActiveDistance),SUM(SedentaryActiveDistance)
      FROM daily_activity")
activity_dist

# Percentage of Activity in Distance
y <- c(533.49,3140.37,1412.52)
piepercent <- round(100*y / sum(y), 1)
colors = c("red","green","yellow")
pie3D(y,labels = paste0(piepercent,"%"),col=colors,main = "# Percentage of Activity in Distance")
legend("bottomright",c("Moderately Active Distance","Lightly Active Distance","Very Active Distance"),cex=0.75,fill = colors)

# calculating the count of people with over weight
count_overweight <- sqldf("SELECT COUNT(DISTINCT(Id))
                          FROM weight_log_info
                          WHERE BMI > 24.9")
count_overweight

# Percentage of Over Weight vs Healthy Weight of People
z <- c(5,3)
piepercent <- round(100*z / sum(z),1)
colors = c("yellow","skyblue")
pie3D(z,labels=paste0(piepercent,"%"),explode=0.1,col=colors,radius=1,main="Percentage of People with
      Over Weight vs Healthy Weight")
legend("bottomright",c("Over Weight","Healthy Weight"),cex=1,fill=colors)










