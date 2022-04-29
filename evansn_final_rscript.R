library(tidyverse)
library(lubridate)
library(tidyr)
library(scales)

urlfile <- "https://github.com/NoahE53/IS4300-Final-Project/raw/main/Airplane_Crashes_and_Fatalities_Since_1908.csv"
Crash <- read_csv(url(urlfile)) #reads in data set

Crash <- Crash[,1:12] #removes summary from Crash data

Crash$Date <- as.Date(Crash$Date, "%m/%d/%y")
Crash$Date <- as.Date(ifelse(Crash$Date >= "2008-09-17", 
                                 format(Crash$Date, "19%y-%m-%d"), 
                                 format(Crash$Date))) 
index <- order(Crash$Date, decreasing = FALSE)
Crash <- Crash[index,]
#formats date and orders crash data by date

Crash$Operator <- as.factor(Crash$Operator)
Crash$Type <- as.factor(Crash$Type)
#formats data types

Crash <- mutate(Crash, Count = 1) #creates counter variable for number of crashes

head(Crash)

#######################

OpCrashes <- as.data.frame(Crash %>% group_by(Operator) %>% summarize(Crashes_Total = sum(Count)))
#counts number of crashes per operator
top_7 <- OpCrashes %>% top_n(7,Crashes_Total) %>% arrange(desc(Crashes_Total))
#gives top 7 operators with highest number of crashes

ggplot(top_7, aes(x=reorder(Operator, -Crashes_Total), y=Crashes_Total)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  labs(title="Ordered Bar Chart", 
       subtitle="Aircraft Operator Vs Num. of Crashes",
       x = "Operator", y = "Crashes")
#creates plot of crashes per operator

######################

Crash <- Crash %>% group_by(Year = lubridate::floor_date(Date, "year"))
Crash$Year <- as.factor(format(Crash$Date, "%Y"))

CrashYears <- as.data.frame(Crash %>% group_by(Year) %>% summarize(Crashes_Total = sum(Count)))
CrashYears$Year <- as.character(CrashYears$Year)
CrashYears$Year <- as.numeric(CrashYears$Year)
YearsFilled <- CrashYears %>% complete(Year = 1908:2007, 
           fill = list(Crashes_Total = 0)) %>% as.data.frame()
#creates data frame of crashes by year and fills in missing years with 0 crashes

ggplot(YearsFilled, aes(x=Year)) + 
  geom_line(aes(y=Crashes_Total)) + 
  labs(title="Time Series Chart", 
       subtitle="Crashes per Year", 
       y="Crashes")
#plots data frame

####################

TypeCrashes <- as.data.frame(Crash %>% group_by(Type) %>% summarize(Crashes_Total = sum(Count)))
#counts number of crashes per type
top_5 <- TypeCrashes %>% top_n(5,Crashes_Total) %>% arrange(desc(Crashes_Total))
#gives top 5 types with highest number of crashes

ggplot(top_5, aes(x=reorder(Type, Crashes_Total), y=Crashes_Total)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=Type, 
                   xend=Type, 
                   y=min(Crashes_Total), 
                   yend=max(Crashes_Total)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Type Vs Total Crashes",
       x="Type", y="Crashes") +  
  coord_flip()

#####################

DeathYears <- as.data.frame(Crash %>% group_by(Year) %>% summarize(Death_Total = sum(Fatalities)))
DeathYears$Year <- as.character(DeathYears$Year)
DeathYears$Year <- as.numeric(DeathYears$Year)
DYearsFilled <- DeathYears %>% complete(Year = 1908:2007, 
                                       fill = list(Death_Total = 0)) %>% as.data.frame()
#creates data frame of deaths by year and fills in missing years with 0 deaths

ggplot(DYearsFilled, aes(x=Year)) + 
  geom_line(aes(y=Death_Total)) + 
  labs(title="Time Series Chart", 
       subtitle="Deaths per Year", 
       y="Deaths")
#plots data frame


