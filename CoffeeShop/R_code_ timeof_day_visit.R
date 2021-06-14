library(dplyr)
library(lubridate)
library(ggplot2)

setwd("~/Desktop/Fall term/EDA/HW 3/Central Perk")

shop<-read.csv("Central Perk Item Sales Summary 2016.csv",stringsAsFactors = F)
summary(shop)

shop<-filter(shop,shop$Customer.ID!='NA')

shop1<-shop %>%
  group_by(Customer.ID,Date,Time) %>%
  summarize(number_items = n()) 

shop1$time1<-hms(shop1$Time)

shop1$hr<-hour(shop1$time1)
shop2<-shop1 %>%
  group_by(hr = hours(hr)) %>%
  summarize(number_vists = n()) 

ggplot(shop2, aes(hr,number_vists)) + 
  geom_line() +
  xlab("Time of the day") + 
  ylab('number visits') + ggtitle('consumer visits time graph') +
  scale_x_continuous(breaks = 1:24) + theme_classic()

  #scale_x_continuous(breaks =c(round(seq(24) , limits=c(5,20)) + theme_classic()

# xlim("1", "24")#

