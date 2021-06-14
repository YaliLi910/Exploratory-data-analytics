#Getting the single dataset at customer level
shop<-read.csv("Central Perk Item Sales Summary 2016.csv",stringsAsFactors = F)
shop1<-read.csv("Central Perk Item Sales Summary 2017.csv",stringsAsFactors = F)
shop2<-read.csv("Central Perk Item Sales Summary 2018.csv",stringsAsFactors = F)

total <- rbind(shop,shop1,shop2)

#getting the dataset at transaction level

trans<-select(total,Date,Time,Category)

library(plyr)
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(trans,c("Date","Time"),
                         function(df1)paste(df1$Category,
                                            collapse = ","))

trans1<-select(total,Date,Time,Item)

#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData1 <- ddply(trans1,c("Date","Time"),
                          function(df1)paste(df1$Item,
                                             collapse = ","))

trans2<-select(total,Date,Time,Price.Point.Name)

#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData2 <- ddply(trans2,c("Date","Time"),
                          function(df1)paste(df1$Price.Point.Name,
                                             collapse = ","))

#Combining them in a single dataset

transaction_final<-inner_join(transactionData,transactionData1,by=c("Date","Time"))
transaction_final1<-inner_join(transaction_final,transactionData2,by=c("Date","Time"))

colnames(transaction_final1)<-c("Date","Time","Category","Item","Price_point_name")
write.csv(transaction_final1,"transaction_final")

total$Net.Sales1<-as.float(total$Net.Sales)
total_sales<-total%>%
  group_by(Date,Time) %>%
  summarize(sales = sum(int(Net.Sales)))

transaction_final<-read.csv("transaction_final.csv")
total1<-total%>%
  group_by(Date,Time) %>%
  summarize(size = n())

transaction_final1<-inner_join(transaction_final,total1,by=c("Date","Time"))

total$sales = as.numeric(gsub("\\$", "", total$Net.Sales))

total2<-total%>%
  group_by(Date,Time) %>%
  summarize(sales = sum(sales))

transaction_final2<-inner_join(transaction_final1,total2,by=c("Date","Time"))

write.csv(transaction_final2,"transaction_vf.csv")