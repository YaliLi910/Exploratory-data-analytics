library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)

setwd("~/MSBA/04 - Fall Semester/03 - Exploratory Data Analytics and Visualization - MSBA 6410/Homework/HW3")
cp16 <- read.csv("Central Perk Item Sales Summary 2016.csv", header=TRUE)
cp17 <- read.csv("Central Perk Item Sales Summary 2017.csv", header=TRUE)
cp18 <- read.csv("Central Perk Item Sales Summary 2018.csv", header=TRUE)
cpAll <- rbind(cp16, cp17, cp18)

# Make copy for density code
cpAllCleanDens <- cpAll

# Convert Dates and Times
cpAll$Date <- mdy(cpAll$Date)
cpAll$Time <- hms(cpAll$Time)

# Clean Data
cpAllClean = filter(cpAll, cpAll$Event.Type == "Payment")
cpAllClean$Gross.Sales = as.numeric(gsub("\\$", "", cpAllClean$Gross.Sales))
cpAllClean$Day = weekdays(cpAllClean$Date)
cpAllClean$Weekend = ifelse(cpAllClean$Day == "Saturday" | cpAllClean$Day == "Sunday", "Weekend", "Weekday")

# Separate Weekend and Weekday DFs
cpAllCleanWknd = cpAllClean %>% 
                  filter(cpAllClean$Weekend == "Weekend") %>%
                  group_by(Hour = hour(Time)) %>% 
                  summarise(Sales = sum(Gross.Sales))

cpAllCleanWk = cpAllClean %>% 
                  filter(cpAllClean$Weekend == "Weekday") %>%
                  group_by(Hour = hour(Time)) %>% 
                  summarise(Sales = sum(Gross.Sales))

# Demand by Hour (weekday and weekend)
cpAllClean %>% 
  group_by(Day.Type = Weekend, Hour = hour(Time)) %>% 
  summarise(Sales = sum(Gross.Sales)) %>%
    ggplot(aes(Hour, Sales, color = Day.Type)) + 
    geom_line() + 
    ylab("Sales ($)") + 
    scale_x_continuous(name="Time of Day", limits=c(6,19))

# Demand by Hour by product
cpAllClean %>% 
  group_by(Cat = Category, Hour = hour(Time)) %>% 
  summarise(Sales = sum(Gross.Sales)) %>%
  ggplot(aes(Hour, Sales, color = Cat)) + geom_line() + facet_wrap(.~Cat)

# Demand by Hour by day of week
cpAllClean %>% 
  group_by(Day = Day, Hour = hour(Time)) %>% 
  summarise(Sales = sum(Gross.Sales)) %>%
  ggplot(aes(Hour, Sales, color = Day)) + geom_line()



# Per Day Cleaning
cpAllCleanOrd = cpAllClean
cpAllCleanOrd$Day = factor(cpAllCleanOrd$Day, 
                           levels = c("Monday", 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

# dollars per day of week ###############################################################################################
cpAllCleanOrd %>% 
  group_by(Weekend = Weekend, Day = Day) %>% 
  summarise(Sales = sum(Gross.Sales)) %>%
  ggplot(aes(Day, Sales)) + geom_bar(stat = "identity",fill = 'navy',alpha = .5) +
  labs(y = "Revenue ($)", x = "Day",title = "Revenue by Day of Week") +
  scale_y_continuous(labels = comma)

p2 <- cpAllCleanOrd %>% 
  group_by(Weekend = Weekend, Day = Day) %>% 
  summarise(Sales = sum(Gross.Sales)) %>%
  ggplot(aes(Day, Sales)) + geom_bar(stat = "identity")


# Sales by month (Ice using n)
cpAllClean %>% 
  filter(cpAllClean$Item == "Ice") %>%
  group_by(Month = month(Date)) %>% 
  summarise(Sales = n()) %>%
  ggplot(aes(Month, Sales)) + geom_bar(stat = 'identity')

# Sales by month (Not Ice using n)
cpAllClean %>% 
  filter(cpAllClean$Item != "Ice") %>%
  group_by(Month = month(Date)) %>% 
  summarise(Sales = n()) %>%
  ggplot(aes(Month, Sales)) + geom_bar(stat = 'identity')

# Sales by Hour by month ALL
cpAllClean %>% 
#  filter(cpAllClean$Category == "Food") %>%
  group_by(Month = month(Date), Hour = hour(Time)) %>% 
  summarise(Sales = sum(Gross.Sales)) %>%
  ggplot(aes(Hour, Sales)) + geom_line() + facet_wrap(.~Month)

# Category percents  #############################################################################################
ggplot(cpAllClean, aes(Category)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "navy", alpha = .5) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Total Revenue (%)", x = "Product Category",title = "Product Category Revenue")

# Food percents ##################################################################################################
cpAllClean %>%
  filter(cpAllClean$Category == "Food") %>%
  ggplot(aes(Item)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = 'navy', alpha = .5) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Food Revenue (%)", x = "Item",title = "Food Item Revenue")


# Coffee Drinks Revenue
cpAllClean %>%
  filter(cpAllClean$Category == "Coffee") %>%
  ggplot(aes(Item)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = 'navy', alpha = .5) +
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Food Revenue (%)", x = "Item",title = "Coffee Drinks Revenue")

cpCoffee = cpAllClean %>% filter(cpAllClean$Category == "Coffee")
cpCoffee$Drip = ifelse(cpCoffee$Item == "Drip SM" | cpCoffee$Item == "Drip LG", "Drip Coffee", "Specialty Coffee" )
ggplot(cpCoffee, aes())


# item prices
prices = cpAllClean %>% 
          group_by(Category, Item = Item) %>% 
          summarise(Mean_Price = mean(Gross.Sales), SD = sd(Gross.Sales), Purchases = n()) %>%
          arrange(desc(Purchases))


# Density Section

# Clean Data
cpAllCleanDens = filter(cpAll, cpAll$Event.Type == "Payment")
cpAllCleanDens$Gross.Sales = as.numeric(gsub("\\$", "", cpAllCleanDens$Gross.Sales))
cpAllCleanDens$dt = paste(cpAllCleanDens$Date, " ", cpAllCleanDens$Time)
cpAllCleanDens$DateNew <- mdy(cpAllCleanDens$Date)
cpAllCleanDens$Day = weekdays(cpAllCleanDens$DateNew)
cpAllCleanDens$Weekend = ifelse(cpAllCleanDens$Day == "Saturday" | cpAllCleanDens$Day == "Sunday", "Weekend", "Weekday")
cpAllCleanDens$dt = paste(cpAllCleanDens$Date, " ", cpAllCleanDens$Time)
cpAllCleanDens$dt <- as.POSIXct(strptime(cpAllCleanDens$dt, "%m/%d/%y %H:%M:%S",  tz = "EST"))
cpAllCleanDens$dt <- hms::hms(second(cpAllCleanDens$dt), minute(cpAllCleanDens$dt), hour(cpAllCleanDens$dt))
cpAllCleanDens$dt <- as.POSIXct(cpAllCleanDens$dt)


# time of day
ggplot(cpAllCleanDens, aes(dt, color = Weekend)) + 
  geom_density(alpha =.35) + 
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%I")) +
  xlab('Time') + ylab("Transaction Density")

# density months
ggplot(cpAllCleanDens, aes(dt, color = Weekend)) + 
  geom_density(alpha =.35, fill = "white") + 
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%I")) +
  facet_wrap(.~month(DateNew))

# density items 
ggplot(cpAllCleanDens, aes(dt, color = Weekend)) + 
  geom_density(alpha =.35, fill = "white") + 
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%I")) +
  facet_wrap(.~Category)

# density top 6 categories ############################################################################################
cpAllCleanDens %>% filter(cpAllCleanDens$Category == "Coffee" |
                            cpAllCleanDens$Category == "Extras" |
                            cpAllCleanDens$Category == "Food" |
                            cpAllCleanDens$Category == "Non-Caffeinated Drinks" |
                            cpAllCleanDens$Category == "Tea" |
                            cpAllCleanDens$Category == "Beans") %>%
  ggplot(aes(dt, color = Weekend)) + 
  geom_density(alpha =.20, aes(fill = Weekend)) + 
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%I")) +
  facet_wrap(.~Category) + labs(x = 'Time', y = 'Transaction Density', title = 'Product Purchase Times')


# density top 6 food items #############################################################################################
cpAllCleanDens %>% filter(cpAllCleanDens$Category == "Food" &
                          (cpAllCleanDens$Item == "Alm Rasp" |
                          cpAllCleanDens$Item == "Almond Rasp" |
                          cpAllCleanDens$Item == "Croissant" |
                          cpAllCleanDens$Item == "Donut" |
                          cpAllCleanDens$Item == "Financier" |
                          cpAllCleanDens$Item == "Lenka Bar")) %>%
  ggplot(aes(dt, color = Weekend)) + 
  geom_density(alpha =.20, aes(fill = Weekend)) + 
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%I")) +
  facet_wrap(.~Item) + labs(x = 'Time', y = 'Transaction Density', title = 'Food Purchase Times')



grid.arrange(p1, p2, nrow = 2)



### TRANSACTION TABLE

#Density

cpTrans <- read.csv("transaction_vf.csv", header=TRUE)
cpTrans$dt = paste(cpTrans$Date, " ", cpTrans$Time)
cpTrans$DateNew <- mdy(cpTrans$Date)
cpTrans$Day = weekdays(cpTrans$DateNew)
cpTrans$Weekend = ifelse(cpTrans$Day == "Saturday" | cpTrans$Day == "Sunday", "Weekend", "Weekday")
cpTrans$dt = paste(cpTrans$Date, " ", cpTrans$Time)
cpTrans$dt <- as.POSIXct(strptime(cpTrans$dt, "%m/%d/%y %H:%M:%S",  tz = "EST"))
cpTrans$dt <- hms::hms(second(cpTrans$dt), minute(cpTrans$dt), hour(cpTrans$dt))
cpTrans$dt <- as.POSIXct(cpTrans$dt)

# time of day ###########################################################################################################
ggplot(cpTrans, aes(dt, fill = Weekend)) + 
  geom_density(alpha =.20) + 
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%I")) +
  xlab('Time') + labs(x = 'Time', y = 'Transaction Density', title = 'Central Perk Transaction Times')

# time of day Day of Week
ggplot(cpTrans, aes(dt)) + 
  geom_density(alpha =.20, fill = "white") + 
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%I")) +
  xlab('Time') + ylab("Transaction Density") + facet_wrap(.~Day)

# Standard

cpTrans2 <- read.csv("transaction_vf.csv", header=TRUE)
# Convert Dates and Times
cpTrans2$Date <- mdy(cpTrans2$Date)
cpTrans2$Time <- hms(cpTrans2$Time)
# Clean Data
cpTrans2$Day = weekdays(cpTrans2$Date)
cpTrans2$Weekend = ifelse(cpTrans2$Day == "Saturday" | cpTrans2$Day == "Sunday", "Weekend", "Weekday")
cpTrans2 = filter(cpTrans2, !is.na(cpTrans2$sales))
cpTrans2$Day = factor(cpTrans2$Day, 
                           levels = c("Monday", 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))



# Transaction $ by hour
cpTrans2 %>% 
  group_by(time = hour(Time)) %>% 
  summarise(Transactions = (sum(sales)/n())) %>%
  ggplot(aes(time, Transactions)) + geom_bar(stat = "identity") + xlim(c(7,19))

# Transaction item # by hour
cpTrans2 %>% 
  group_by(time = hour(Time)) %>% 
  summarise(Transactions = (sum(size)/n()  )) %>%
  ggplot(aes(time, Transactions)) + geom_bar(stat = "identity") +
  xlim(c(7,19))


cpTrans2 %>% 
  group_by(Time = hour(Time)) %>% 
  summarise(Items = (sum(size)/n()), Dollars = (sum(sales)/n() )) %>% 
  gather(key = "Measurement", value = "Value", -Time) %>%
  ggplot(aes(Time, Value, fill = Measurement)) + 
  #geom_line() + 
  xlim(c(7,19)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Items Per Transaction"))

# Transaction size in items by day # transaction size by $ by day #####

# items per transaction day alone
cpTrans2 %>% 
  group_by(Day) %>% 
  summarise(Items = (sum(size)/n())) %>% 
  ggplot(aes(Day, Items)) + geom_bar(stat = "identity")

# dollars per transaction day alone
cpTrans2 %>% 
  group_by(Day) %>% 
  summarise(Items = (sum(sales)/n())) %>% 
  ggplot(aes(Day, Items)) + geom_bar(stat = "identity")

cpTrans2 %>% 
        group_by(Day) %>% 
        summarise(Items = (sum(size)/n()), Dollars = (sum(sales)/n() )) %>% 
        gather(key = "Measurement", value = "Value", -Day) %>%
        ggplot(aes(Day, Value, fill = Measurement)) + geom_bar(stat = "identity", position = "dodge") +
        scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Items Per Transaction"))

 


