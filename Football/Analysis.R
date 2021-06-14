library(readr)
library(dplyr)
library(tidyverse)

setwd("~/01 - Exploratory Data Analytics and Visualization - MSBA 6410")


# Historical Close Matches - Teams
    
    # Define close matches
    roma_matches = read_csv('apriori_table.csv', col_names = TRUE)
    roma_matches = roma_matches %>% select(score, opponent_team)
    roma_matches$close <- ifelse(roma_matches$score >= -1 & roma_matches$score <= 1 , 1, 0)
    
    # Group opponent teams and determine who close matches are played against
    opps = roma_matches %>% group_by(opponent_team) %>% 
                    summarise(total = n(),close = sum(close))
    opps = opps %>% mutate(close_percent = (close / total)) %>% 
                    filter(close_percent > .5 & total > 10)
    opps = opps %>% arrange(desc(close_percent))
    print(opps)

# League Table Line Graph
    lg_tbl = read_csv('lg_tbl.csv', col_names = TRUE)
    ggplot(lg_tbl, aes(Season, Rank, group = RankType, color = RankType)) +
      geom_line(aes(linetype=RankType)) +
      scale_linetype_manual(values=c("solid", "dashed", "solid")) +
      scale_color_manual(values=c('black','red', 'blue' )) +
      geom_point() +
      labs(x = 'Season', title = "Results With Hypothetical Close Match Outcomes")


# Bar graph of subset of games we are choosing
    roma_matches = read_csv('apriori_table.csv', col_names = TRUE)
    roma_matches = roma_matches %>% select(year, season, score)
    roma_matches$Result <- if_else(roma_matches$score == -1,'Close Loss',
                                    if_else(roma_matches$score == 1, 'Close Win',
                                    ifelse(roma_matches$score == 0, 'Draw', "Other Result")))
    
    ggplot(roma_matches, aes(season, fill = Result)) +
      geom_bar() +
      labs(x = 'Season', title = "Results of AS Roma Matches 2008-2016")
    
    
    

    
    
    
    
    
    
    
# Historical Close Matches - Gambling - Complete later
mch_tbl = read_csv('match_tbl.csv', col_names = TRUE)
mch_tbl_new = mch_tbl %>% select(home_team_goal,	away_team_goal, B365D) 
mch_tbl_new = mch_tbl_new %>% mutate(result = (abs(home_team_goal - away_team_goal)))
mch_tbl_new = mch_tbl_new %>% mutate(close = ifelse(result < 2, 1, 0))

ggplot(mch_tbl_new, aes(B365D, result))+
  geom_jitter()

plot(mch_tbl_new$B365D, mch_tbl_new$result)


hist(mch_tbl$B365D)
