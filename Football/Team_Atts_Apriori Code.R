# Load the dataset
df_pat <- read.csv('None_team_attributes.csv')

# Filter for the close game
df_pat <- df_pat %>% filter(score >= -1 & score <= 1)

# Convert numeric score to categorical class
df_pat$score <- ifelse(df_pat$score < 0, 'L',ifelse(df_pat$score == 0, 'D', 'W'))

# Team numeric attributes preparation
team_atts_tbl_selected_1 <- team_atts_tbl %>% select_if(negate(is.character))
team_atts_tbl_selected_1$team_api_id <- team_atts_tbl$team_api_id
team_atts_tbl_selected_1$date <- ymd_hms(team_atts_tbl$date) 
team_atts_tbl_selected_1$year <- year(team_atts_tbl_selected_1$date)
team_atts_tbl_selected_home <- team_atts_tbl_selected_1 %>% filter(team_api_id == 8686)

# Merge the datasets
df_pat_final <- merge(df_pat, team_atts_tbl_selected_home, by.x=c('year'), by.y=c('year'), all.x = TRUE)

df_pat_anova <- merge(df_pat_final, team_atts_tbl_selected_1, by.x=c('year','away_team_api_id'), by.y=c('year','team_api_id'), all.x = TRUE)

# First version - maintain the records of both teams
df_pat_anova <- df_pat_anova[,c(1,4,5,6,7,9,10,14:22,26:34)]

# Second version: show the difference of attributes
for (i in c(8:16)) {
  df_pat_anova[i] <- df_pat_anova[i] - df_pat_anova[i+9]
}
df_anova_diff <- df_pat_anova[,c(1:16)]

# Clean the column name
old <- names(df_anova_diff)
for (i in c(8:16)) {
  old[i] <- substr(old[i],1,nchar(old[i])-2)
}

names(df_anova_diff) <- old

# Create categories 
for (i in 8:16) {
  df_anova_diff[i] <- ifelse(df_anova_diff[i] > 20, 'Very Strong',
                             ifelse(df_anova_diff[i] > 10, 'Strong', 
                                    ifelse(df_anova_diff[i] > 0, 'Balance',
                                           ifelse(df_anova_diff[i] > -20, 'Weak', 'Very Weak'))))
}


# Filter data based on "Loss" & "Draw"
df_apri_DL <- df_anova_diff %>% filter(score %in% c('D','L'))

# Select the desired columns
df_apri_DL <- df_apri_DL[,c(5,6,8:16)]


# Start association rule
df_apri <- as(df_apri_DL, 'transactions')

# Basic analysis
inspect(df_apri)
itemInfo(df_apri)
itemFrequency(df_apri)
itemFrequencyPlot(df_apri)

# Note support is set very low, because even the most frequent item appears infrequently
rules <- apriori(df_apri, parameter=list(supp = 0.02, conf=1))

rules <- rules %>%
  subset(subset = (rhs %pin% "score=L") | rhs %pin% "score=D") %>%
  #subset(subset = (lhs %pin% 'buildUpPlayDribbling')) %>% 
  sort(by=c('lift'), decreasing = TRUE)

# Convert rules to dataframe
result <- DATAFRAME(rules)

# Expor the data as csv
write_csv(result, 'team_atts_association.csv')
