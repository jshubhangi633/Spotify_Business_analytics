# Spotify_Business_Analytics
Source of Code- Kaggle (https://www.kaggle.com/code/darshanajayr/random-forset/input?select=spotify_history.csv)


# Installing Packages to load data 
    
    install.packages("tidyverse")  
    library(tidyverse)
    library(dplyr)
    
# Loading the dataset
    
    spotify_df <- read_csv("file.path")


# Check structure and missing data
    colSums(is.na(spotify_df))

# Drop NA values
    spotify_df <- na.omit(spotify_df)

# Separate time and date
    spotify_df <- spotify_df %>%
      +     separate(ts, into = c("date", "time"), sep = " ")

# Data Exploration 
    
# 1- Most Used Platforms
    
    library(dplyr)
    library(ggplot2)

  ## Group and count the platform usage
    platform_usage <- spotify_df %>%
        group_by(platform) %>%
        summarise(count = n()) %>%
        arrange(desc(count))

  ## Bar Chart
    ggplot(platform_usage, aes(x = reorder(platform, -count), y = count, fill = platform)) +
        geom_bar(stat = "identity") +
        labs(title = "Platform Usage Count", x = "Platform", y = "Count") +
        theme_minimal()

## Interpretation
The graph shows that the Android platform dominates usage, with significantly higher user counts compared to other platforms. Other platforms like iOS, Mac, and Web Player have minimal
engagement, suggesting Android should be the primary focus for optimization and strategy.

# 2- Average Listening Time per Platform
  
  ## Convert ms_played to minutes
    spotify_df <- spotify_df %>%
       mutate(minutes_played = ms_played / 60000)

  ## Calculate average minutes played per platform
    avg_time <- spotify_df %>%
      group_by(platform) %>%
      summarise(avg_minutes = mean(minutes_played, na.rm = TRUE)) %>%
      arrange(desc(avg_minutes))

  ## Bar Chart for average minutes played by platform
    ggplot(avg_time, aes(x = reorder(platform, -avg_minutes), y = avg_minutes, fill = platform)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Minutes Played by Platform", y = "Average Minutes", x = "Platform") +
      theme_minimal()
## Interpretation
The graph shows that Mac users have the highest average minutes played, followed closely by cast to device and Windows users. Android and Web Player have moderate engagement, while iOS users exhibit the lowest average playtime. This indicates Mac users are the most engaged, while iOS might need attention for improvement.

# 3- Skipping Behavior & Shuffle Mode

  ## Summarize skip behavior by shuffle mode
    shuffle_skip_summary <- spotify_df %>%
     group_by(shuffle, skipped) %>%
     summarise(count = n(), .groups = "drop") %>%
     group_by(shuffle) %>%
     mutate(percent = count / sum(count) * 100)

  ## Create a bar plot
    ggplot(shuffle_skip_summary, aes(x = factor(shuffle, labels = c("Shuffle OFF", "Shuffle ON")),
                                       y = percent,
                                       fill = factor(skipped, labels = c("Completed", "Skipped")))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Skip Behavior by Shuffle Mode",
           x = "Shuffle Mode",
           y = "Percentage (%)",
           fill = "Track Status") +
      theme_minimal()
## Interpretation
The graph illustrates that the majority of tracks are completed in both shuffle modes (ON and OFF), with very few being skipped. The proportion of skips is slightly higher when shuffle mode is ON, indicating that shuffling might influence skipping behavior.

# 4- What Are the Most Common Patterns Behind Starting and Ending a Track?
  
    top_reasons <- spotify_df %>%
      count(reason_start, reason_end, sort = TRUE) %>%
      head(5)

    print(top_reasons)
 
  ## Plot top 5 reason combinations
    ggplot(top_reasons, aes(x = reorder(paste(reason_start, reason_end, sep = " → "), -n), y = n)) +
      geom_bar(stat = "identity", fill = "#1DB954") +  # Spotify green
      labs(title = "Top 5 Reason Start → End Combinations",
           x = "Start → End Reason", y = "Count") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))

## Interpretation
The graph indicates that the most common start-to-end reason combination is trackdone →
trackdone, with significantly higher counts compared to other combinations. This suggests that most users let tracks play to completion. The next most frequent combination, fwdbtn → fwdbtn,indicates some user-initiated skipping behavior, while other combinations occur less frequently.
 
 
      
# 5- What patterns in user behavior can be identified through correlations between session attributes?
  ## Load necessary libraries
       library(corrplot)
      
  ## Step 1: Feature Engineering
       spotify_df_clean <- spotify_df %>%
        +     mutate(
          +         shuffle = as.numeric(shuffle),
          +         skipped = as.numeric(skipped),
          +         reason_start_grouped = as.numeric(as.factor(reason_start)),
          +         reason_end_grouped = as.numeric(as.factor(reason_end))
          +      %>%
        +     select(hour, minutes_played, shuffle, skipped, reason_start_grouped, reason_end_grouped)
      
   ## Step 2: 
  Compute correlation matrix
            
            corr_matrix <- cor(spotify_df_clean, use = "complete.obs")
   
   ## Step 3: Plot the correlation matrix 
        corrplot(corr_matrix,
                 +          method = "color",
                 +          type = "upper",
                 +          tl.col = "black",
                 +          tl.srt = 45,
                 +          addCoef.col = "black",
                 +          col = colorRampPalette(c("darkgreen", "green", "white"))(200),
                 +          number.cex = 0.7)

 
## Interpretation 
The correlation matrix reveals several key relationships between session attributes:

● shuffle and skipped (positive correlation):
There is a moderate positive correlation between shuffling and skipping behavior. This
suggests that users are more likely to skip tracks when listening in shuffle mode, possibly due
to lack of control over song order.

● minutes_played and skipped (negative or weak correlation):
A negative or weak correlation indicates that sessions with more skipped songs tend to have
shorter listening durations.

● reason_start_grouped and minutes_played
Positive: Certain reasons for starting playback (like user-initiated actions vs. algorithmic
recommendations) may lead to longer listening sessions.

● reason_start_grouped and reason_end_grouped (positive correlation):
A positive correlation between reasons for starting and ending sessions may indicate
predictable behavior flows—e.g., sessions started by user intent may also end more naturally
(rather than by interruption).


 
# 5- Check Structure and Missing Data
    colSums(is.na(spotify_df)) 
  ## convert ms_played to minutes played
    spotify_df <- spotify_df %>%
    +     mutate (minutes_played = ms_played / 60000) ,

  ## Drop Variables 
    spotify_df %>%
    select(-spotify_tracks, -album_name,ms_played), 
    spotify_df <- na.omit(spotify_df) ,

# 6-  Identifying Outliers

    boxplot(spotify_df$ms_played, main = "Boxplot of  Time played ", horizontal = TRUE)
    ower_bound <- quantile(spotify_df$ms_played, 0.05)
    upper_bound <- quantile(spotify_df$ms_played, 0.95)
    spotify_df_clean <- spotify_df %>% filter(ms_played >= lower_bound & ms_played <= upper_bound)
    boxplot(spotify_df_clean$ms_played, main = "Boxplot After Removing Outliers")

## Interpretation of the Boxplot (After Removing Outliers):

1. Typical Listening Duration:
Most songs are played for 0–400,000 ms (up to ~6.6 minutes), aligning with average track
lengths on Spotify. The median (~3.3 minutes) reflects common listening habits.
2. Spread and Skewness:
The interquartile range shows most listening occurs between ~50 seconds and ~6.6 minutes,
with a slight right skew due to a few longer sessions.

# 7-  Feature Extraction 
 ## Adding new variables minutes_played by mutating ms_played (milliseconds played) 
    spotify_df <- spotify_df %>%
    + mutate(minutes_played = ms_played / 60000) 
 ## Adding a new variable time_block by breaking ts (time stamp) original variable. 
    spotify_df <- spotify_df %>%
    separate(ts, into = c("date", "time"), sep = " ") %>%
    mutate(hour = as.numeric(substr(time, 1, 2))) %>%
    mutate(time_block = case_when(
     hour >= 0 & hour< 6 ~ "Late Night",
     hour >= 6 & hour < 12 ~ "Morning",
     hour >= 12 & hour < 18 ~ "Afternoon",
     hour >= 18 & hour <= 23 ~ "Evening" ))
 
# 8- Dropping Irrelevant Columns
 
    #Drop Variables 
    spotify_df %>%
    select(-spotify_tracks, -album_name,ms_played)
    spotify_df <- na.omit(spotify_df)

# 9- Hypothesis Testing 
    
## Hypothesis 1: Time of Day Affects Listening Time
 ### Null Hypothesis (H₀): Time of the day has no significant impact on Listening Time
  ### Alternative Hypothesis (H₁): Time of the day has significant impact on Listening Time
    

    library(dplyr)
    library(ggplot2)
 
    # Step 1: Group by time block and calculate average minutes played
    avg_listening_time <- spotify_df %>%
    +     group_by(time_block) %>%
    +     summarise(
    +         avg_minutes = mean(minutes_played, na.rm = TRUE),
    +         count = n()
    +     ) %>%
    +     arrange(desc(avg_minutes))
      print(avg_listening_time)


### Visualize with Bar Plot
 
    ggplot(count_listening_time, aes(x = time_block, y = play_count, fill = time_block)) +
    +     geom_bar(stat = "identity") +
    +     theme_minimal() +
    +     labs(
    +         title = "Number of Plays by Time of Day",
    +         x = "Time Block",
    +         y = "Play Count"
    +     ) +
    +     scale_fill_brewer(palette = "Set2")
 
### Statistical Analysis using ANOVA
    
    anova_result <- aov(minutes_played ~ time_block, data = spotify_df)
    summary(anova_result)
 
 ## Interpretation 
  Since p-value (< 2e-16) is extremely small (far below 0.05) → This means the differences in means are statistically significant. 
Hence, You can reject the null hypothesis (H₀). There is statistically significant evidence that time of day affects listening time.

Time of Day Influences Listening Habits
Engagement peaks during Evening, followed by Late Night and Morning hours. These patterns suggest opportunities for time-specific playlists and ad campaigns to increase retention and satisfaction.
  
# Hypothesis 2: Skipping is Influenced by Playback Settings Eg- Shuffling 
    
  ## Null Hypothesis (H₀): Shuffle mode has no significant impact on whether a song is skipped.
  ## Alternative Hypothesis (H₁): Shuffle mode has a significant influence on the likelihood of skipping.
    
    #Using GGplot for visualizing the data
    ggplot(spotify_df, aes(x = shuffle, fill = as.factor(skipped))) +
      +     geom_bar(position = "fill") +
      +     labs(
      +         title = "Skip Rate by Shuffle Setting",
      +         x = "Shuffle",
      +         y = "Proportion",
      +         fill = "Skipped"
      +     ) +
    +     theme_minimal()
  
  
  ## Conducting Logistic regression for statistical confirmation 
      logit_model <- glm(skipped ~ shuffle, data = spotify_df, family = binomial)
      summary(logit_model)

## Conclusion
Since the p-value is well below 0.05, we reject the null hypothesis.

Hence, Shuffle Mode Raises Skip Rates
Users are about 2.5 times more likely to skip songs when shuffle is enabled. This highlights
a need for a more intelligent shuffle algorithm that considers user preferences and listening
context for a smoother experience.

# Hypothesis 3- Does Platform Influence Listening Duration?
 
   ## Null Hypothesis (H₀): Platform has no impact on listening time.
  ## Alternate Hypothesis (H₁): Platform has a significant impact on listening time.
  ### Graphical Analysis using Boxplot 
  
    library(ggplot2)
    ggplot(spotify_df, aes(x = platform, y = minutes_played, fill = platform)) +
      +     geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
      +     theme_minimal() +
      +     labs(
      +         title = "Boxplot of Listening Duration by Platform",
      +         x = "Platform",
      +         y = "Minutes Played"
      +     ) +
    +     theme(
      +         plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      +         axis.text.x = element_text(angle = 45, hjust = 1),
      +         legend.position = "none"
      +     ) +
    +     scale_fill_brewer(palette = "Greens")
  
  
  ### Load necessary libraries
    > library(dplyr)
    > library(randomForest)
    > library(caret)
    > library(ggplot2)
    
  ### STEP 1: Prepare the Data
      spotify_rf <- spotify_df %>%
      filter(!is.na(skipped), platform != "iOS") %>%  # Remove NA and iOS (insignificant obs)
    +     mutate(
      +         skipped = as.factor(as.numeric(skipped)),
      +         shuffle = as.numeric(shuffle),
      +         platform = as.factor(platform),
      +         reason_start = as.factor(reason_start),
      +         reason_end = as.factor(reason_end),
      +         minutes_played = as.numeric(minutes_played),
      +         
        +     )
  
  ### STEP 2: Remove rows with missing values in key variables
        spotify_rf <- spotify_rf %>% 
    +     filter(complete.cases(platform, minutes_played, hour, shuffle, reason_start, reason_end))
   
  ### STEP 3: Train-test split
       set.seed(123)
       train_index <- createDataPartition(spotify_rf$minutes_played, p = 0.8, list = FALSE)
       train_data <- spotify_rf[train_index, ]
       test_data <- spotify_rf[-train_index, ]
   
  ### STEP 4: Train Random Forest Regression Model
      rf_model <- randomForest(minutes_played ~ platform + shuffle + hour + reason_start + reason_end,
                               +                          data = train_data, ntree = 100, importance = TRUE)
 
  ### STEP 5: Predict on Test Set
         predicted_minutes <- predict(rf_model, newdata = test_data)
   
  ### STEP 6: Evaluate Performance
     actuals <- test_data$minutes_played
     rmse <- sqrt(mean((predicted_minutes - actuals)^2))
     mae <- mean(abs(predicted_minutes - actuals))
     sst <- sum((actuals - mean(actuals))^2)
     sse <- sum((actuals - predicted_minutes)^2)
     r_squared <- 1 - (sse / sst)
   
  ### STEP 7: Output Metrics
       cat("R-squared:",r_squared, "\n")
        R-squared: 0.6231 
        > cat("RMSE:",rmse, "\n")
        RMSE: 1.2116 
        > cat("MAE:",mae, "\n")
        MAE: 0.8514 
  
  ### Now we see if platform alone has any effect 
      
       library(dplyr)
       library(randomForest)
       library(caret)
       library(ggplot2)
   
      spotify_rf <- spotify_df %>%
    +     filter(!is.na(skipped), platform != "iOS") %>%  # Remove NA and iOS (insignificant obs)
    +     mutate(
      +         skipped = as.factor(as.numeric(skipped)),
      +         shuffle = as.numeric(shuffle),
      +         platform = as.factor(platform),
      +         reason_start = as.factor(reason_start),
      +         reason_end = as.factor(reason_end),
      +         minutes_played = as.numeric(minutes_played),
      +         
        +     )
   
### Remove rows with missing values in key variables
     spotify_rf <- spotify_rf %>% 
    +     filter(complete.cases(platform, minutes_played, hour, shuffle, reason_start, reason_end))
   
  ### Train-test split
       set.seed(123)
     train_index <- createDataPartition(spotify_rf$minutes_played, p = 0.8, list = FALSE)
     train_data <- spotify_rf[train_index, ]
     test_data <- spotify_rf[-train_index, ]
   
  ### Train Random Forest Regression Model
     rf_model <- randomForest(minutes_played ~ platform,
                               +                          data = train_data, ntree = 100, importance = TRUE)
   
  ### Predict on Test Set
     predicted_minutes <- predict(rf_model, newdata = test_data)
   
  ### Evaluate Performance
     actuals <- test_data$minutes_played
     rmse <- sqrt(mean((predicted_minutes - actuals)^2))
     mae <- mean(abs(predicted_minutes - actuals))
     sst <- sum((actuals - mean(actuals))^2)
     sse <- sum((actuals - predicted_minutes)^2)
     r_squared <- 1 - (sse / sst)
     
  ### Output
     
      cat("R-squared:", r_squared, "\n")
     R-squared: 0.6231276 
      cat("RMSE:",rmse, "\n")
     RMSE: 1.211585 
      cat("MAE:",mae, "\n")
     MAE: 0.8513765 

 ## Interpretation 
 Random Forest regression model, which included multiple features such as platform, shuffle status,
hour of play, and start/end reasons, resulted in an R-squared of 0.623.
This means that 62.3% of the variation in listening duration (minutes played) is explained by
the model, indicating a good level of predictive power.
 
  ### Check if platform alone has any effect on the duration 
     
  ### STEP 1: Prepare the Data
     spotify_rf <- spotify_df %>%
     filter(!is.na(skipped), platform != "iOS") %>%  # Remove NA and iOS (insignificant obs)
       +     mutate(
         +         skipped = as.factor(as.numeric(skipped)),
         +         shuffle = as.numeric(shuffle),
         +         platform = as.factor(platform),
         +         reason_start = as.factor(reason_start),
         +         reason_end = as.factor(reason_end),
         +         minutes_played = as.numeric(minutes_played),
         +         
           +     )
     
  ### STEP 2: Remove rows with missing values in key variables
     spotify_rf <- spotify_rf %>% 
       +     filter(complete.cases(platform, minutes_played, hour, shuffle, reason_start, reason_end))
     
  ### STEP 3: Train-test split
     set.seed(123)
     train_index <- createDataPartition(spotify_rf$minutes_played, p = 0.8, list = FALSE)
     train_data <- spotify_rf[train_index, ]
     test_data <- spotify_rf[-train_index, ]
   
 ### STEP 4: Train Random Forest Regression Model
    rf_model <- randomForest(minutes_played ~ platform,  data = train_data, ntree = 100, importance = TRUE)  
  
  ### STEP 5: Predict on Test Set
    predicted_minutes <- predict(rf_model, newdata = test_data)
     
  ### STEP 6: Evaluate Performance
    actuals <- test_data$minutes_played
     rmse <- sqrt(mean((predicted_minutes - actuals)^2))
     mae <- mean(abs(predicted_minutes - actuals))
     sst <- sum((actuals - mean(actuals))^2)
     sse <- sum((actuals - predicted_minutes)^2)
     r_squared <- 1 - (sse / sst)
     
  ### STEP7- Output 
   
    cat("R-squared:",r_squared, "\n")
    R-squared: 0.01183302 
    cat("RMSE:",rmse, "\n")
    RMSE: 1.961878 
    cat("MAE:",mae, "\n")
    MAE: 1.656534 

## Interpretation

● The R-squared value is just 1.18%, meaning that platform explains almost none of the
variability in listening duration.

● Compared to the full model (R² = 62.3%), this model performs significantly worse.

● The RMSE and MAE have both increased, further confirming that predictions are less
accurate when platform is the only feature.
Hence, Platform Has Minimal Impact; Context Matters More
While slight differences exist across platforms, playback context, time of day, and shuffle
status are stronger predictors of listening duration. This supports a shift toward context-
driven personalization rather than platform-based targeting
