# Load required packages
library(dplyr)
library(magrittr)
library(arules)
library(RSQLite)
library(manip)
library(assertive)
library(lubridate)
library(xml2)
library(XML)
library(tidyverse)
library(XML)
library(magrittr)
library(graphics)
library(gridExtra)
library(rlist)

# set working directory
setwd('~/Desktop/6410 - Exploratory/HW 1')

# Load the database
con <- src_sqlite("euro_soccer.sqlite")

country_tbl <- tbl(con, "country")
league_tbl <- tbl(con, "league")
match_tbl <- tbl(con, "match")
player_tbl <- tbl(con, "player")
player_atts_tbl <- tbl(con, "player_attributes")
team_tbl <- tbl(con, "team")
team_atts_tbl <- tbl(con, "team_attributes")

# Each of the followint tables are just dplyr connections to the database tables
# If or when I need to bring the table to local memory I need to run table <- collect(table)
# Bring table to local memory
country_tbl <- collect(country_tbl)
league_tbl <- collect(league_tbl)
match_tbl <- collect(match_tbl)
player_tbl <- collect(player_tbl)
player_atts_tbl <- collect(player_atts_tbl)
team_tbl <- collect(team_tbl)
team_atts_tbl <- collect(team_atts_tbl)

# ------------------------------------------------------------------------------------------------------------------------------

# Get the api of Roma
team_name <- 'Roma'
roma_api <- team_tbl$team_api_id[(team_tbl$team_long_name == team_name)]

# Get the desired columns of Roma
col <- c('id', 'match_api_id', 'date', 'stage', 'season', 'home_team_api_id', 'away_team_api_id', 'home_team_goal','away_team_goal')
roma_table <- match_tbl %>% select(col) %>% filter(home_team_api_id == roma_api | away_team_api_id == roma_api)

# Get the opponent team api id
roma_table$opponent <- roma_table$home_team_api_id + roma_table$away_team_api_id - 8686

# Whether Roma play "away" & "home" match
roma_table$home_away <- sapply(roma_table$home_team_api_id, function(x) if (x == 8686) x = 'home' else x = 'away') 

# Deal the date column and create a year column
roma_table$date <- ymd_hms(roma_table$date)
roma_table$year <- year(roma_table$date)

# Calculate the match score from Roma perspective (negative means "loss", zero means "draw", positive means "win")
score <- c()
for (i in (1:nrow(roma_table))) {
  if (roma_table$home_away[i] == 'home') score <- c(score, roma_table$home_team_goal[i] - roma_table$away_team_goal[i]) else score <- c(score, roma_table$away_team_goal[i] - roma_table$home_team_goal[i])
}
roma_table$score <- score

# Team attribute table preparation - for further merging
team_atts_tbl_selected <- team_atts_tbl %>% select_if(is.character)
team_atts_tbl_selected$team_api_id <- team_atts_tbl$team_api_id
team_atts_tbl_selected$date <- ymd_hms(team_atts_tbl$date) 
team_atts_tbl_selected$year <- year(team_atts_tbl_selected$date)

# ------------------------------------------------------------------------------------------------------------------------------

# When roma play as "home", add "Roma" & "Opponent" team attribute table
roma_home_match <- roma_table %>% filter(home_away == 'home')
df_match_1 <- merge(x=roma_home_match, y=team_atts_tbl_selected, by.x=c('home_team_api_id', 'year'), by.y=c('team_api_id', 'year'), all.x = TRUE)
# change the column name
attribute_name <- names(df_match_1)[14:26]
name <- c()
for (i in names(df_match_1)) {
  if (i %in% attribute_name) name <- c(name, paste(i,'roma', sep = '_')) else name <- c(name, i)
}
names(df_match_1) <- name


df_match_2 <- merge(x=roma_home_match, y=team_atts_tbl_selected, by.x=c('away_team_api_id', 'year'), by.y=c('team_api_id', 'year'), all.x = TRUE)
# change the column name
attribute_name <- names(df_match_2)[14:26]
name <- c()
for (i in names(df_match_2)) {
  if (i %in% attribute_name) name <- c(name, paste(i,'opponent', sep = '_')) else name <- c(name, i)
}
names(df_match_2) <- name

# Semi-final result 
df_match_1[27:39] <- df_match_2[14:26]

# ------------------------------------------------------------------------------------------------------------------------------

# When roma play as "away", add "Roma" & "Opponent" team attribute table
roma_away_match <- roma_table %>% filter(home_away == 'away')
df_match_3 <- merge(x=roma_away_match, y=team_atts_tbl_selected, by.x=c('away_team_api_id', 'year'), by.y=c('team_api_id', 'year'), all.x = TRUE)
# change the column name
attribute_name <- names(df_match_3)[14:26]
name <- c()
for (i in names(df_match_3)) {
  if (i %in% attribute_name) name <- c(name, paste(i,'roma', sep = '_')) else name <- c(name, i)
}
names(df_match_3) <- name

df_match_4 <- merge(x=roma_away_match, y=team_atts_tbl_selected, by.x=c('home_team_api_id', 'year'), by.y=c('team_api_id', 'year'), all.x = TRUE)
# change the column name
attribute_name <- names(df_match_4)[14:26]
name <- c()
for (i in names(df_match_4)) {
  if (i %in% attribute_name) name <- c(name, paste(i,'opponent', sep = '_')) else name <- c(name, i)
}
names(df_match_4) <- name

# Semi-final result 
df_match_3[27:39] <- df_match_4[14:26]

# ------------------------------------------------------------------------------------------------------------------------------

# Merge the "home" & "away" tables that I get previously
df <- merge(df_match_1, df_match_3, by.x=names(df_match_1), by.y=names(df_match_3), all = TRUE)

# Add opponent name
team_tbl_select <- team_tbl[c('team_api_id','team_long_name')]
df_new <- merge(df, team_tbl_select, by.x=c('away_team_api_id'), by.y=c('team_api_id'), all.x = TRUE)
names(df_new)[40] <- 'opponent_team'

# Drop the unrelated columns 
drop_column <- c('away_team_api_id','home_team_api_id','date.x','date.y_roma','date.y_opponent','home_team_goal','away_team_goal','date.y')
df_semi_final <-  df_new[,!(names(df_new) %in% drop_column)]

# ------------------------------------------------------------------------------------------------------------------------------

# Add the team formation from Aditya and get the final table
team_formation <- read.csv('team_formation.csv')
df_final <- merge(df_semi_final, team_formation, by.x=c('id'), by.y=c('id'), all = TRUE)

# Final table cleaning
drop_column_final <- c('X', 'opponent')
df_final <-  df_final[,!(names(df_final) %in% drop_column_final)]
df_final <- df_final[,c(3,1,2,4,5:7,32,8:31,33:62)]

# Export the result in csv file
write.csv(df_final, file = 'apriori_table.csv')
