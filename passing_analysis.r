###World Cup 2022/2023 Football Passing Analysis###
rm(list=ls())
library(StatsBombR)
library(glmnet) #for fitting the lasso
library(stringr) #for selecting best lambda penalty factor 
library(caret)
library(dplyr)
library(ggplot2)
library(tibble) 
library(interactions)

#Loading Statsbomb datasets
#Get the preliminary datasets 
Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)

#Euro 2024 (Men's)
matches_euro24 <- Matches %>%
  filter(competition.competition_id == 55, season.season_id == 282)

#Extract unique match IDs for EURO 2024
match_ids_euro24 <- unique(matches_euro24$match_id)

#Print the unique match IDs
print(match_ids_euro24)

#Euro 2025 (Women's)
matches_euro25 <- Matches %>%
  filter(competition.competition_id == 53, season.season_id == 315)

#Extract unique match IDs for EURO 2025
match_ids_euro25 <- unique(matches_euro25$match_id)

#Print the unique match IDs
print(match_ids_euro25)

#####Preparing dataframes for logistic lasso####

#Empty list to store results
team_features_list_w <- list()

#Loop through each match in matches_euro25
for (i in 1:nrow(matches_euro25)) {
  events <- get.matchFree(matches_euro25[i, ])
  
  #Extract the match ID
  match_id <- as.character(matches_euro25[i, "match_id"])
  
  #Extract home and away teams from the matches_euro25 dataframe
  home_team <- matches_euro25[i, "home_team.home_team_name"]
  away_team <- matches_euro25[i, "away_team.away_team_name"]
  
  #Add a column to indicate if the team is 'home' or 'away'
  events <- events %>%
    mutate(
      team_role = case_when(
        team.name == home_team ~ "Home", 
        team.name == away_team ~ "Away"
      )
    )
  
  #Extract team-level features for each team (home and away)
  team_features <- events %>%
    group_by(team.name, team_role) %>%
    summarise(
      match_id = match_id,
      
      #Passing
      total_passes = sum(type.name == "Pass", na.rm = TRUE),
      avg_pass_length = mean(pass.length, na.rm = TRUE),
      pass_completion_rate = sum(is.na(pass.outcome.name) == FALSE & type.name == "Pass", na.rm = TRUE) / total_passes,
      progressive_passes = sum(pass.length > 30 & type.name == "Pass", na.rm = TRUE),
      crosses = sum(pass.cross == TRUE, na.rm = TRUE),
      
      #Carries
      total_carries = sum(type.name == "Carry", na.rm = TRUE),
      
      #Defensive
      pressures = sum(type.name == "Pressure", na.rm = TRUE),
      tackles = sum(type.name == "Duel" & duel.outcome.name == "Won", na.rm = TRUE),
      interceptions = sum(type.name == "Interception", na.rm = TRUE),
      clearances = sum(type.name == "Clearance", na.rm = TRUE),
      aerial_duels = sum(type.name == "Duel", na.rm = TRUE),
      
      #Shooting
      total_shots = sum(type.name == "Shot", na.rm = TRUE),
      total_goals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
      xG_per_shot = mean(shot.statsbomb_xg, na.rm = TRUE),
      shot_accuracy = ifelse(total_shots > 0, total_goals / total_shots, NA),
      set_piece_shots = sum(shot.type.name %in% c("Free Kick", "Corner"), na.rm = TRUE)
    ) %>%
    ungroup()
  
  #Add to the list
  team_features_list_w[[match_id]] <- team_features
}

#Combine all match data into a single dataframe
final_features_df_w <- bind_rows(team_features_list_w)

final_features_df_w <- final_features_df_w %>% distinct()
final_features_df_w$team_role <- NULL

##Repeat for men
#Empty list to store results from each match
team_features_list_m <- list()

#Loop through each match in matches_euro24
for (i in 1:nrow(matches_euro24)) {
  events <- get.matchFree(matches_euro24[i, ])
  
  #Extract the match ID
  match_id <- as.character(matches_euro24[i, "match_id"])
  
  #Extract home and away teams from the matches_euro24 dataframe
  home_team <- matches_euro24[i, "home_team.home_team_name"]
  away_team <- matches_euro24[i, "away_team.away_team_name"]
  
  #Add a column to indicate if the team is 'home' or 'away'
  events <- events %>%
    mutate(
      team_role = case_when(
        team.name == home_team ~ "Home", 
        team.name == away_team ~ "Away"
      )
    )
  
  #Extract team-level features for each team (home and away)
  team_features <- events %>%
    group_by(team.name, team_role) %>%
    summarise(
      match_id = match_id,
      
      #Passing
      total_passes = sum(type.name == "Pass", na.rm = TRUE),
      avg_pass_length = mean(pass.length, na.rm = TRUE),
      pass_completion_rate = sum(is.na(pass.outcome.name) == FALSE & type.name == "Pass", na.rm = TRUE) / total_passes,
      progressive_passes = sum(pass.length > 30 & type.name == "Pass", na.rm = TRUE),
      crosses = sum(pass.cross == TRUE, na.rm = TRUE),
      
      #Carries
      total_carries = sum(type.name == "Carry", na.rm = TRUE),
      
      #Defensive
      pressures = sum(type.name == "Pressure", na.rm = TRUE),
      tackles = sum(type.name == "Duel" & duel.outcome.name == "Won", na.rm = TRUE),
      interceptions = sum(type.name == "Interception", na.rm = TRUE),
      clearances = sum(type.name == "Clearance", na.rm = TRUE),
      aerial_duels = sum(type.name == "Duel", na.rm = TRUE),
      
      #Shooting
      total_shots = sum(type.name == "Shot", na.rm = TRUE),
      total_goals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
      xG_per_shot = mean(shot.statsbomb_xg, na.rm = TRUE),
      shot_accuracy = ifelse(total_shots > 0, total_goals / total_shots, NA),
      set_piece_shots = sum(shot.type.name %in% c("Free Kick", "Corner"), na.rm = TRUE)
    ) %>%
    ungroup()
  
  #Add to the list
  team_features_list_m[[match_id]] <- team_features
}

#Combine all match data into a single dataframe
final_features_df_m <- bind_rows(team_features_list_m)

final_features_df_m <- final_features_df_m %>% distinct()
final_features_df_m$team_role <- NULL

#Combining the men's and women's features 
final_features_df <- rbind(final_features_df_m, final_features_df_w)

#Add a column for gender: 1 = female, 0 = men
final_features_df$gender <- c(rep("0", 102), rep("1", 62))

#Making sure all character features are converted to factors 
final_features_df <- final_features_df %>%
  mutate_if(is.character, as.factor) 
##Running the LASSO model, where gender is the outcome 
set.seed(123)  #For reproducibility

#Drop non-feature columns
final_features_raw <- final_features_df %>%
  select(-match_id, -team.name)

#Remove any NA's
final_features_raw <- final_features_raw[complete.cases(final_features_raw), ]

#Split into training and testing set 
trainIndex <- createDataPartition(final_features_raw$gender, p = 0.8, list = FALSE)
trainData <- final_features_raw[trainIndex, ]
testData <- final_features_raw[-trainIndex, ]

#Convert to matrix
xtrain <- as.matrix(select(trainData, -gender))
xtest <- as.matrix(select(testData, -gender))

ytrain <- trainData$gender
ytrain <- as.numeric(ytrain)

ytest <- testData$gender
ytest <- as.numeric(ytest)

#Scale the features 
xtrain <- scale(xtrain)
xtest <- scale(xtest)

#Fit the lasso model with cross-validation on the training sets 
cv_lasso <- cv.glmnet(xtrain, ytrain, alpha = 1, family = "binomial")

#Choose optimal lambda value (the penalty parameter that determines which variables will be shrunk to zero)
best_lambda <- cv_lasso$lambda.1se

#Refit model using optimal lambda
lasso1 <- glmnet(xtrain, ytrain, alpha = 1, family = "binomial", lambda = best_lambda)

#Predict on test set
lasso_predictions <- predict(cv_lasso, s = best_lambda, newx = xtest)

#Examine coefficients
coef(lasso1, s = best_lambda)

#Interpret as odds ratios
exp(coef(lasso1, s = best_lambda))

##Graphing top features 
#Taking the coefficients from the model

coef_matrix <- as.matrix(coef(lasso1, s = best_lambda))
colnames(coef_matrix)
#####Running the LASSO model, where gender is the outcome####
set.seed(123)  #For reproducibility

#Drop non-feature columns
final_features_raw <- final_features_df %>%
  select(-match_id, -team.name)

#Remove any NA's
final_features_raw <- final_features_raw[complete.cases(final_features_raw), ]

#Split into training and testing set 
trainIndex <- createDataPartition(final_features_raw$gender, p = 0.8, list = FALSE)
trainData <- final_features_raw[trainIndex, ]
testData <- final_features_raw[-trainIndex, ]

#Convert to matrix
xtrain <- as.matrix(select(trainData, -gender))
xtest <- as.matrix(select(testData, -gender))

ytrain <- trainData$gender
ytrain <- as.numeric(ytrain)

ytest <- testData$gender
ytest <- as.numeric(ytest)

#Scale the features 
xtrain <- scale(xtrain)
xtest <- scale(xtest)

#Fit the lasso model with cross-validation on the training sets 
cv_lasso <- cv.glmnet(xtrain, ytrain, alpha = 1, family = "binomial")

#Choose optimal lambda value (the penalty parameter that determines which variables will be shrunk to zero)
best_lambda <- cv_lasso$lambda.1se

#Refit model using optimal lambda
lasso1 <- glmnet(xtrain, ytrain, alpha = 1, family = "binomial", lambda = best_lambda)

#Predict on test set
lasso_predictions <- predict(cv_lasso, s = best_lambda, newx = xtest)

#Examine coefficients
coef(lasso1, s = best_lambda)

#Interpret as odds ratios
exp(coef(lasso1, s = best_lambda))

##Graphing top features 
#Taking the coefficients from the model

coef_matrix <- as.matrix(coef(lasso1, s = best_lambda))
colnames(coef_matrix)

coef_df <- coef_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "feature") %>%
  rename('s' = 's1') %>%  #call colnames(coef_matrix)
  filter(feature != "(Intercept)", s != 0) %>%
  mutate(
    odds_ratio = exp(s),
    abs_coef = abs(s) #taking the absolute value 
  ) %>%
  arrange(desc(abs_coef))

#Plotting selected variables

#Taking the top 10 selected variables based on absolute coefficient
top_n <- 10
top_features <- coef_df %>%
  slice_max(order_by = abs_coef, n = top_n)

#Rename variables 
top_features <- top_features %>%
  mutate(feature = case_when(
    feature == "pass_completion_rate" ~ "Passing Rate", 
    feature == "pressures" ~ "Pressures", 
    feature == "avg_pass_length" ~ "Average Pass Length", 
    feature == "total_shots" ~ "Total Shots", 
    feature == "tackles" ~ "Tackles", 
    feature == "interceptions" ~ "Interceptions",
    feature == "crosses" ~ "Crosses",
    feature == "total_carries" ~ "Total Carries",
    feature == "shot_accuracy" ~ "Goal Conversion Rate",
    feature == "xG_per_shot" ~ "xG Per Shot",
    feature == "set_piece_shots" ~ "Set Piece Shots",
    feature == "aerial_duels" ~ "Aerial Duels",
    feature == "total_shots" ~ "Total Shots",
    feature == "clearances" ~ "Clearances",
    feature == "total_goals" ~ "Total Goals"))

#Plot the top features 
ggplot(top_features, aes(x = reorder(feature, abs_coef), y = abs_coef)) +
  geom_col(fill = "#6E8972", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top Variables Selected for Predicting Men's vs. Women's EURO 24/25 Match",
    x = "Variable",
    y = "LASSO Regression Coefficient"
  ) +
  theme_minimal()

#####Descriptive Stats and Plots#####

##Running loops to get information about pass type and pass length 
pass_length_list <- list()
#Loop through each match for EURO25
for (i in 1:nrow(matches_euro25)) {
  events <- get.matchFree(matches_euro25[i, ])
  
  passes <- events %>% filter(type.name == "Pass")
  
  passes <- passes %>%
    mutate(
      successful = ifelse(is.na(pass.outcome.name), 1, 0),
      pass_length = pass.length,
      pass_type = pass.type.name,
      pass_height = pass.height.name,
      under_pressure = ifelse(is.na(under_pressure), "No", "Yes")
    )
  
  #Calculate passing accuracy and average pass length per team
  match_pass_acc <- passes %>%
    group_by(team.name, match_id, pass_type, pass_height, under_pressure) %>%
    summarise(
      total_passes = n(),
      successful_passes = sum(successful),
      accuracy = successful_passes / total_passes,
      avg_pass_length = mean(pass_length, na.rm = TRUE),
      .groups = "drop"
    )
  
  pass_length_list[[i]] <- match_pass_acc
}

#Combine all match results into one data frame
pass_length_w <- bind_rows(pass_length_list)

pass_length_w <- pass_length_w %>%
  mutate(
    pass_type = replace_na(pass_type, 'Regular'))

##men
pass_length_list <- list()
#Loop through each match for EURO24
for (i in 1:nrow(matches_euro24)) {
  events <- get.matchFree(matches_euro24[i, ])
  
  passes <- events %>% filter(type.name == "Pass")
  
  passes <- passes %>%
    mutate(
      successful = ifelse(is.na(pass.outcome.name), 1, 0),
      pass_length = pass.length,
      pass_type = pass.type.name,
      pass_height = pass.height.name,
      under_pressure = ifelse(is.na(under_pressure), "No", "Yes")
    )
  
  #Calculate passing accuracy and average pass length per team
  match_pass_acc <- passes %>%
    group_by(team.name, match_id, pass_type, pass_height, under_pressure) %>%
    summarise(
      total_passes = n(),
      successful_passes = sum(successful),
      accuracy = successful_passes / total_passes,
      avg_pass_length = mean(pass_length, na.rm = TRUE),
      .groups = "drop"
    )
  
  pass_length_list[[i]] <- match_pass_acc
}

#Combine all match results into one data frame
pass_length_m <- bind_rows(pass_length_list)

pass_length_m <- pass_length_m %>%
  mutate(
    pass_type = replace_na(pass_type, 'Regular'))


#Combine with men's data and add gender label
pass_length_both <- rbind(pass_length_m, pass_length_w)
pass_length_both$gender <- c(rep("Men", nrow(pass_length_m)), rep("Women", nrow(pass_length_w)))
# pass_length_both <- pass_length_both %>%
#   select(-c(gender))

#Count the percentage of each type of pass
type_dist <- pass_length_both %>%
  group_by(pass_type) %>%
  summarise(count = sum(total_passes), .groups = "drop") %>%
  mutate(percentage = count/sum(count) * 100)

#Plot the pass type bar plot 
ggplot(type_dist, aes(x = pass_type, y = percentage)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#6E8972") +
  labs(
    title = "Pass Type Distribution",
    x = "Pass Type",
    y = "% of Passes ",
  ) +
  theme_minimal()

#####Create df with all stats######

##Create a function that extracts all the relevant stats for comparison for both men and women##
get_match_stats <- function(matches, gender) {
  
  results_list <- list()
  
  for (i in 1:nrow(matches)) {
    events <- get.matchFree(matches[i, ])
    match_id <- matches$match_id[i]
    
    #Passing related stats 
    passes <- events %>% 
      filter(type.name == "Pass" & is.na(pass.type.name)) %>% #filtering for only regular passes
      mutate(successful = ifelse(is.na(pass.outcome.name), 1, 0),
             pass_length = pass.length)
    
    pass_stats <- passes %>%
      group_by(team.name) %>% #this allows for the stats to be team-level instead of each individual event 
      summarise(
        total_passes = n(),
        successful_passes = sum(successful),
        accuracy = successful_passes / total_passes, #passing accuracy
        avg_pass_length = mean(pass_length, na.rm = TRUE), #average pass length
        .groups = "drop"
      )
    
    #Pressure per minute
    pressures <- events %>%
      group_by(team.name) %>%
      summarise(
        pressures_per_min = sum(!is.na(under_pressure)) / max(minute, na.rm = TRUE),
        .groups = "drop"
      )
    
    #Duels per minute 
    duels <- events %>%
      filter(!is.na(duel.type.id)) %>% #duel type id is not coded as 0/1, its 10/11 
      group_by(team.name) %>%
      summarise(
        duels = n(),
        duration = max(minute, na.rm = TRUE), 
        duels_per_min = duels / duration, 
        .groups = "drop"
      )
    
    #Shot related stats
    shots <- events %>% filter(type.name == "Shot")
    shot_stats <- shots %>%
      group_by(team.name) %>%
      summarise(
        total_shots = n(),
        total_xg = sum(shot.statsbomb_xg, na.rm = TRUE),
        avg_xg_per_shot = mean(shot.statsbomb_xg, na.rm = TRUE),
        .groups = "drop"
      )
    
    #Combine per team 
    match_stats <- left_join(pass_stats, shot_stats, by = "team.name") %>%
      left_join(., duels, by = "team.name") %>%
      left_join(., pressures, by = "team.name") %>%
      mutate(
        match_id = match_id,
        gender = gender #include gender column so that when the df's are combined they are already labelled 
      )
    
    results_list[[i]] <- match_stats
  }
  
  bind_rows(results_list)
}

combined_stats_m <- get_match_stats(matches_euro24, "Men")
combined_stats_w <- get_match_stats(matches_euro25, "Women")

#This df will contain team level stats per match
combined_stats <- rbind(combined_stats_m, combined_stats_w)

#Save combined_stats into a csv
###save the existing variables to file path of choice 
write.csv(combined_stats, "combined_stats_euro.csv")
#####Plot pressures Per Minute#####
ggplot(combined_stats, aes(x = gender, y = pressures_per_min, fill = gender)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  scale_fill_manual(values = c("Men" = "orange", "Women" = "steelblue")) +
  labs(title = "Average # of Pressures Per Min",
       x = "Gender",
       y = "Pressures Per Minute") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 14))

#Calculate statistical significance 
model2 <- glm(pressures_per_min ~ gender, data = combined_stats)
summary(model2)

t.test(pressures_per_min ~ gender, data = combined_stats)

#####Plot Pass length vs pressure rate by gender#####
ggplot(combined_stats, aes(x = pressures_per_min, y = avg_pass_length, colour = gender)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_manual(values = c("Men" = "orange", "Women" = "steelblue")) +
  labs(
    title = "Pass Length vs Pressure Rate by Gender",
    x = "Pressures per Minute",
    y = "Average Pass Length (m)",
    color = "Gender"
  ) +
  theme_minimal(base_size = 8)

model3 <- glm(avg_pass_length ~ gender * pressures_per_min, data = combined_stats)
summary(model3)
#####Calculating pass directions by gender####
#Describing horizonality and verticality of passes 
get_pass_angles <- function(matches, gender) {
  
  pass_angles <- list()
  
  for (i in 1:nrow(matches)) {
    events <- get.matchFree(matches[i, ])
    match_id <- matches$match_id[i]
    
    #Filter for regular play passes
    passes <- events %>% filter(type.name == "Pass" & (is.na(pass.type.name)))
    
    passes <- passes %>%
      mutate(
        under_pressure = ifelse(!is.na(under_pressure) & under_pressure == TRUE, 1, 0),
        pass_direction = case_when(
          pass.angle >= -pi/4 & pass.angle <= pi/4 ~ "Forward",
          ((pass.angle > pi/4 & pass.angle <= 3*pi/4) | (pass.angle < -pi/4 & pass.angle >= -3*pi/4)) ~ "Side",
          abs(pass.angle) > 3*pi/4 ~ "Backward",
          TRUE ~ "Diagonal"
        ),
        successful = ifelse(is.na(pass.outcome.name), 1, 0)
      )
    
    pass_accuracy <- passes %>%
      group_by(match_id, pass_direction, under_pressure) %>%
      summarise(
        total_passes = n(),
        successful_passes = sum(successful),
        accuracy = successful_passes / total_passes * 100,
        .groups = "drop"
      )
    
    pass_accuracy <- pass_accuracy %>%
      group_by(match_id) %>%
      mutate(
        percent_of_total = total_passes / sum(total_passes) * 100,
        gender = gender
      )
    
    #Store in list
    pass_angles[[i]] <- pass_accuracy
    
  }
  
  bind_rows(pass_angles)
}

pass_angles_m <- get_pass_angles(matches_euro24, "Men")
pass_angles_w <- get_pass_angles(matches_euro25, "Women")

pass_angles_combined <- rbind(pass_angles_m, pass_angles_w)
#####Plotting pass direction by gender#####
#First, need to sum the percentages per pass direction
pass_direction_totals <- pass_angles_combined %>%
  group_by(match_id, pass_direction, gender) %>%
  summarise(
    total_percent = sum(percent_of_total, na.rm = TRUE),
    .groups = "drop"
  )

#Then, need to summarise to get the average total # for that direction (avg across matches)
pass_plot_data <- pass_direction_totals %>%
  group_by(pass_direction, gender) %>%
  summarise(
    avg_percent = mean(total_percent, na.rm = TRUE),
    .groups = "drop"
  )

#Plot pass direction by gender
ggplot(pass_plot_data, aes(x = pass_direction, y = avg_percent, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Men" = "orange", "Women" = "steelblue")) +
  labs(
    x = "Pass Direction",
    y = "% of Total Passes",
    fill = "Gender"
  ) +
  coord_flip() +
  theme_minimal(base_size = 24)

#Alternative version with percentage labels 
ggplot(pass_plot_data, aes(x = pass_direction, y = avg_percent, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(avg_percent, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = 1,
            hjust = -0.2,
            size = 3) +
  scale_fill_manual(values = c("Men" = "orange", "Women" = "steelblue")) +
  labs(
    x = "Pass Direction",
    y = "% of Total Passes",
    fill = "Gender"
  ) +
  coord_flip() +
  theme_minimal(base_size = 24)
#####Overall passing accuracy by direction#####
pass_accuracy_avg <- pass_angles_combined %>%
  group_by(pass_direction) %>%
  summarise(accuracy = mean(accuracy, na.rm = TRUE))

#Plot pass accuracy by direction
ggplot(pass_accuracy_avg, aes(x = reorder(pass_direction, -accuracy), y = accuracy)) +
  geom_bar(stat = "identity", fill = "#6E8972") +
  labs(
    x = "Pass Direction",
    y = "Passing Accuracy"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 24)
#####Passing Accuracy Over Time#####
get_acc_over_time <- function(matches, gender) {
  
  acc_over_time <- list()
  
  for (i in 1:nrow(matches)) {
    events <- get.matchFree(matches[i, ])
    
    passes <- events %>% filter(type.name == "Pass" & is.na(pass.type.name))
    
    passes <- passes %>%
      mutate(
        time_bucket = floor(minute / 10) * 10, #rounds to nearest whole number to bucket each pass
        successful = ifelse(is.na(pass.outcome.name), 1, 0)
      )
    
    accuracy_by_time <- passes %>%
      group_by(match_id, time_bucket) %>%
      summarise(
        total_passes = n(),
        successful_passes = sum(successful),
        accuracy = successful_passes / total_passes,
        .groups = "drop"
      )
    
    accuracy_by_time <- accuracy_by_time %>%
      mutate(gender = gender)
    
    acc_over_time[[i]] <- accuracy_by_time
  }
  bind_rows(acc_over_time)
}

acc_over_time_m <- get_acc_over_time(matches_euro24, "Men")
acc_over_time_w <- get_acc_over_time(matches_euro25, "Women")

acc_over_time_both <- rbind(acc_over_time_m, acc_over_time_w)

#Aggregate across all matches by time bucket and gender
acc_over_time_summary <- acc_over_time_both %>%
  group_by(gender, time_bucket) %>%
  summarise(
    total_passes = sum(total_passes),
    successful_passes = sum(successful_passes),
    accuracy = successful_passes / total_passes,
    .groups = "drop"
  )

acc_over_time_both_summary <- acc_over_time_both %>%
  group_by(match_id, time_bucket, gender) %>%
  summarise(
    total_passes = sum(total_passes),
    successful_passes = sum(successful_passes),
    accuracy = successful_passes / total_passes,
    .groups = "drop"
  )

#Plot the average accuracy per gender
ggplot(acc_over_time_summary, aes(x = time_bucket, y = accuracy, colour = gender)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  labs(
    title = "Passing Accuracy Over Match Time by Gender",
    x = "Match Time (Minutes)",
    y = "Passing Accuracy",
    color = "Gender"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Men" = "orange", "Women" = "steelblue"))

#Plot the accuracy per team, per match per gender
ggplot(acc_over_time_both_summary, aes(x = time_bucket, y = accuracy, group = match_id, colour = gender)) +
  geom_line(alpha = 0.4) +
  labs(
    title = "Passing Accuracy Over Time by Match",
    x = "Match Time (Minutes)",
    y = "Passing Accuracy",
    color = "Gender"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("Men" = "orange", "Women" = "steelblue"))



#####Dataframe with calculated pass direction and pressure####
##Run for loop for men
all_passes <- list()

for (i in 1:nrow(matches_euro24)) {
  events <- get.matchFree(matches_euro24[i, ])
  
  passes <- events %>%
    filter(type.name == "Pass" & is.na(pass.type.name)) %>%
    mutate(
      match_id = match_id,
      under_pressure = ifelse(!is.na(under_pressure) & under_pressure == TRUE, 1, 0),
      pressure_status = ifelse(under_pressure == 1, "Under Pressure", "Not Pressured"),
      pass_direction = case_when(
        pass.angle >= -pi/4 & pass.angle <= pi/4 ~ "Forward",
        ((pass.angle > pi/4 & pass.angle <= 3*pi/4) | (pass.angle < -pi/4 & pass.angle >= -3*pi/4)) ~ "Side",
        abs(pass.angle) > 3*pi/4 ~ "Backward",
        TRUE ~ "Diagonal"
      ),
      pass_length_bin = case_when(
        pass.length < 15 ~ "Short",
        pass.length < 30 ~ "Medium",
        TRUE ~ "Long"
      ),
      successful = ifelse(is.na(pass.outcome.name), 1, 0)
    )
  
  all_passes[[i]] <- passes
}

#Combine all matches into one data frame
passes_combined_m <- bind_rows(all_passes)
passes_combined_m$gender <- "Men"

##Women
all_passes <- list()

for (i in 1:nrow(matches_euro25)) {
  events <- get.matchFree(matches_euro25[i, ])
  
  #Filter for successful passes
  passes <- events %>%
    filter(type.name == "Pass" & is.na(pass.type.name)) %>%
    mutate(
      match_id = match_id,
      under_pressure = ifelse(!is.na(under_pressure) & under_pressure == TRUE, 1, 0),
      pressure_status = ifelse(under_pressure == 1, "Under Pressure", "Not Pressured"),
      pass_direction = case_when(
        pass.angle >= -pi/4 & pass.angle <= pi/4 ~ "Forward",
        ((pass.angle > pi/4 & pass.angle <= 3*pi/4) | (pass.angle < -pi/4 & pass.angle >= -3*pi/4)) ~ "Side",
        abs(pass.angle) > 3*pi/4 ~ "Backward",
        TRUE ~ "Diagonal"
      ),
      pass_length_bin = case_when(
        pass.length < 15 ~ "Short",
        pass.length < 30 ~ "Medium",
        TRUE ~ "Long"
      ),
      successful = ifelse(is.na(pass.outcome.name), 1, 0)
    )
  
  all_passes[[i]] <- passes
}

#Combine all matches into one data frame
passes_combined_w <- bind_rows(all_passes)
passes_combined_w$gender <- "Women"

#The column #'s are mismatched, so need to find the difference 
setdiff(colnames(passes_combined_m), colnames(passes_combined_w))
setdiff(colnames(passes_combined_w), colnames(passes_combined_m))

passes_combined_m <- passes_combined_m %>%
  select(-c(goalkeeper.success_in_play, player_off.permanent))

passes_combined_w <- passes_combined_w %>%
  select(-c(goalkeeper.lost_out))

passes_combined <- rbind(passes_combined_m, passes_combined_w)
#####Plot pass accuracy by team & by gender#####

#Need to first group by team and by gender
pass_accuracy <- passes_combined %>%
  group_by(team.name,gender) %>%
  summarise(
    total_passes = n(),
    successful_passes = sum(successful, na.rm = TRUE),
    accuracy = successful_passes / total_passes * 100, 
    .groups = 'drop'
  )

#Update the team names 
pass_accuracy <- pass_accuracy %>%
  mutate(team.name = ifelse(gender == "Men", paste(team.name, "Men's"), team.name))

#Plot the initial passing accuracy plot first 
ggplot(pass_accuracy, aes(x = reorder(team.name, accuracy), y = accuracy)) +
  geom_bar(stat = "identity", fill = "#6E8972") +
  labs(title = "Pass Accuracy per Team",
       x = "Team",
       y = "Accuracy") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1))

#Plot the passing accuracy that is colour coded by gender
ggplot(pass_accuracy, aes(x = reorder(team.name, accuracy), y = accuracy, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Pass Accuracy per Team",
       x = "Team",
       y = "Accuracy",
       fill = "Gender") +
  theme_minimal(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1)) +
  scale_fill_manual(values = c("Men" = "orange", "Women" = "steelblue"))

#Calculate statistical significance 
model1 <- glm(accuracy ~ gender, data = pass_accuracy)
summary(model1)

t.test(accuracy ~ gender, data = pass_accuracy)

#####Plot Average Passing Length#####
#average pass length data is stored in "pass_length_both" which was created for the initial descriptive plots
#need to aggregate by team first 
pass_length_summary <- pass_length_both %>%
  group_by(team.name, gender) %>%
  summarise(avg_pass_length = mean(avg_pass_length, na.rm = TRUE),
            .groups = "drop")

#plot the average pass length by gender
ggplot(pass_length_summary, aes(x = gender, y = avg_pass_length, fill = gender)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  scale_fill_manual(values = c("Men" = "orange", "Women" = "steelblue")) +
  labs(title = "Average Pass Length by Gender",
       x = "Gender",
       y = "Average Pass Length (m)") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 14))

t.test(avg_pass_length ~ gender, data = pass_length_summary)
#####Interaction between passing accuracy and passing length#####

#Need to aggregate data so that the rows are at a team level 
pass_length_interaction <- pass_length_both %>%
  group_by(team.name, gender) %>%
  summarise(avg_pass_length = mean(avg_pass_length, na.rm = TRUE),
            accuracy = mean(accuracy, na.rm = TRUE),
            .groups = "drop")


model4 <- glm(accuracy ~ gender * avg_pass_length, data = pass_length_interaction)
summary(model4)

#Plot the interaction effect 
library(interactions)
interaction_plot <- interact_plot(model4, pred = avg_pass_length, modx = gender,
                                  plot.points = TRUE, interval = TRUE)

interaction_plot +
  labs(
    x = "Average Pass Length (m)",
    y = "Accuracy",
    title = "Interaction Between Pass Length and Gender"
  ) +
  scale_color_manual(
    values = c("Men" = "orange", "Women" = "steelblue"),
    name = "Gender"
  ) +
  scale_linetype_manual(
    values = c("Men" = "solid", "Women" = "solid"),
    guide = "none"
  ) +
  theme_minimal()

#####Proportion of passes made under pressure#####
#Recalculate proportion of under-pressure passes
pass_pressure_summary <- passes_combined %>%
  group_by(gender, pass_direction) %>%
  summarise(
    total_passes = n(),
    under_pressure_passes = sum(under_pressure == 1)
  ) %>%
  mutate(
    percent_under_pressure = under_pressure_passes / total_passes * 100
  )

ggplot(pass_pressure_summary, aes(x = pass_direction, y = percent_under_pressure, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Men" = "orange", "Women" = "steelblue")) +
  labs(
    title = "Proportion of Passes Made Under Pressure by Direction and Gender",
    x = "Pass Direction",
    y = "Under Pressure Passes (%)",
    fill = "Gender"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 8)

######Pass Accuracy by pass direction and applied pressure 
pressure_labels <- c(
  "Under Pressure" = "Pressure",
  "Not Pressured" = "No Pressure"
)

direction_labels <- c(
  "Forward" = "Forward Passes",
  "Backward" = "Backward Passes",
  "Side" = "Side Passes"
)

pass_acc_context <- passes_combined %>%
  group_by(match_id, team.name, gender, pass_direction, pressure_status) %>%
  summarise(avg_accuracy = mean(successful, na.rm = TRUE)) %>%
  ggplot(aes(x = avg_accuracy, fill = gender)) +
  ylim(0, 55) +
  scale_fill_manual(values = c("Men" = "orange" , "Women" = "steelblue")) +
  geom_histogram(binwidth = 0.02, position = "identity", alpha = 0.7) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  facet_grid(
    pressure_status ~ pass_direction,
    labeller = labeller(
      pressure_status = as_labeller(pressure_labels),
      pass_direction = as_labeller(direction_labels)
    )
  ) +
  labs(x = "Pass Accuracy", y = "Number of Teams", fill = "Gender") +
  theme_minimal(base_size = 10) +
  theme(
    strip.text.x = element_text(size = 10, face = "bold"),
    strip.text.y = element_text(size = 10, angle = 90, face = "bold")
  )

pass_acc_context

#ggsave("~/Downloads/pass_acc_context_euro.pdf", plot = last_plot(), width = 24, height = 16, dpi = 5000)

##Statistical test for the above plot

#Passing accuracy in general
model5 <- glm(successful ~ gender, data = passes_combined, family = binomial)
summary(model5)

#Filter for forward passes made under pressure 
passes_combined_filtered <- passes_combined %>%
  filter(pressure_status == "Under Pressure",
         pass_direction == "Forward")

#Model looking specifically at forward passes that are made under pressure
model6 <- glm(successful ~ gender, data = passes_combined_filtered, family = binomial)
summary(model6)
