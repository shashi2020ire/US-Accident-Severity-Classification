#Classification Model

#Choosing the libraries
library(tidyverse)
library(tidyselect)
library(scales)
library(lubridate)
library(plotly)
library(gridExtra)
library(tidytext)
library(modelr)
library(caret)
library(ROSE)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(maps)
library(Amelia)
options(warn = -1)


# Importing dataset
df <- read_csv("G:/NCI/Data Mining/Project/US Accidents_Classification/USAccidentData_NJ.csv", col_types = cols(.default = col_character())) %>% 
  type_convert()
df %>% head(5)

View(df)
#Data Pre-processing
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}


# Variable with high NA
df %>% summarise_all(~ mean(is.na(.))) %>% 
  pivot_longer(1:49, names_to = "Variables to drop", values_to = "NA proportion") %>% 
  filter(`NA proportion` >= 0.5)

# Missing values
missmap(df_drop, main = "Missing values vs observed")

#drop_na_cols <- c("End_Lat", "End_Lng", "Number", "Wind_Chill(F)", "Precipitation(in)")

# Droping Unuseful variables
not_useful <- c("ID", "Source", "Timezone", "Airport_Code", "Weather_Timestamp", 
                "Wind_Direction", "Description")

not_useful1 <- drop_na_cols <- c("End_Lat", "End_Lng", "Number", "Wind_Chill(F)", "Precipitation(in)")


df_drop <-select(df, -not_useful, -not_useful1)
view(df_drop)
# Wind Dirrect Impact on Severity
fig(13, 8)
ggplot(df, aes(Wind_Direction, ..prop.., group = Severity)) +
  geom_bar(aes(fill = Severity), position = "dodge") +
  scale_y_continuous(labels = percent) +
  labs(x = "Wind Direction",
       y = "Proportion",
       title = "Wind direction does not have a great impact on severity") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.6))


# renaming variables
df_drop <-  df_drop %>%
  rename("Distance" = `Distance(mi)`, "Temperature" = `Temperature(F)`, "Humidity" = `Humidity(%)`, 
         "Pressure" = `Pressure(in)`, "Visibility" = `Visibility(mi)`, "Wind_Speed" = `Wind_Speed(mph)`)


#original date and time
df_drop %>% select(Start_Time, End_Time) %>% head(5)
view(df_drop)

#Transformation time
df_time <- df_drop %>%
  mutate(Duration = as.numeric(End_Time - Start_Time)) %>%
  # accident duration should be positive
  filter(!(Duration < 0)) %>%
  separate(Start_Time, into = c("Date", "Time"), sep = " ") %>%
  mutate("Year" = str_sub(Date, 1, 4), "Month" = str_sub(Date, 6, 7), "Day" = str_sub(Date, 9, 10), 
         "Wday" = as.character(wday(Date)), "Hour" = str_sub(Time, 1, 2)) %>%
  select(-c("Date", "Time", "End_Time")) %>%
  select(TMC, Severity, Year, Month, Day, Hour, Wday, Duration, everything())


#After transformation
df_time %>%
  select(Year, Month, Day, Hour, Wday, Duration) %>%
  head(5)

#Weather condition NA values
df_time %>% filter(is.na(Weather_Condition)) %>% select(Temperature:Weather_Condition) %>%
  head(10)

# Removing NA values of Weather_Condition
df_weather <- df_time %>% filter(!is.na(Weather_Condition))

#Handle TMC NA values
df_TMC <- df_weather %>%
  mutate(TMC = replace_na(TMC, "NA_TMC"))

fig(13, 8)
df_weather %>% 
  ggplot(aes(factor(TMC), ..prop..)) +
  geom_bar(aes(group = Severity, fill = factor(Severity)), show.legend = F) +
  facet_wrap(~ Severity, scales = "free") +
  labs(x = "TMC",
       y = "Proportion",
       title = "TMC distribution in each severity level") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.6),
        legend.position = "top") +
  scale_fill_brewer(palette = "Set1")

#Modify variable type
address <- c("Country", "City", "County", "Street", "Zipcode")
df_TMC %>%
  select(address) %>%
  head(5)

#Drop location variables
df_add <- select(df_TMC, -address)


#Modifying the variable type
df_add <- df_add %>% 
  mutate(TMC = as.character(TMC), Severity = as.character(Severity)) %>% 
  mutate_if(is.logical, as.character)


#Handling NA values in continous variables
df_mean <- df_add %>%
  mutate_if(is.numeric, ~ replace_na(., mean(., na.rm = T)))

summary(df_mean %>% select_if(is.numeric))


#NA values in categorical variable 
df_mean %>% summarise_all(~sum(is.na(.))) %>% 
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_count") %>% filter(NA_count > 0)


df_final <- df_mean %>%
  filter(!is.na(Side)) %>%
  filter(!is.na(Sunrise_Sunset))

# write into csv file
write_csv(df_final, "G:/NCI/Data Mining/Project/US Accidents_Classification/USAccidentData_NJ_Final.csv")

#Visualization
df <- read_csv("G:/NCI/Data Mining/Project/US Accidents_Classification/USAccidentData_NJ_Final.csv", col_types = cols(.default = col_character())) %>% 
  type_convert() %>%
  mutate(TMC = factor(TMC), Severity = factor(Severity), Year = factor(Year), Wday = factor(Wday)) %>%
  mutate_if(is.logical, factor) %>%
  mutate_if(is.character, factor)


#Accident count


#Distance affected by accident
  fig(13, 8)
  df %>%
    group_by(Severity) %>%
    summarise(prop = mean(Distance)) %>%
    ggplot(aes(Severity, prop, fill = !Severity %in% c(3, 4))) +
    geom_col() +
    labs(
      y = "Average affected distance (mi)",
      title = "More severe accidents tend to affect longer road distance") +
    scale_fill_discrete(name = "Severity", labels = c("More Severe: 3 or 4", "Less Severe: 1 or 2"))
  
#Accident count in each severity level
  fig(13, 8)
  df %>%
    group_by(Year, Severity) %>%
    count() %>%
    group_by(Year) %>%
    mutate(sum = sum(n)) %>%
    mutate(Proportion = n / sum) %>%
    ggplot(aes(Severity, Proportion)) +
    geom_col(aes(fill = Year), position = "dodge") +
    labs(x = "Severity",
         y = "Proportion",
         title = "Severity proportion changes by year") +
    scale_y_continuous(labels = percent)
  
# Accident count in diffrent time scale
  g_top <- df %>%
    count(Month) %>%
    ggplot(aes(Month, n)) +
    geom_line(aes(group = 1)) +
    geom_point() +
    labs(y = "Count",
         x = NULL,
         title = "Pattern between accident counts and month & day of the week") +
    scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                "Jun", "Jul", "Aug", "Sep", "Oct",
                                "Nov", "Dec")) +
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03))
  
  g_bottom <- df %>%
    ggplot(aes(Month, fill = Wday)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values = c("deepskyblue1", "coral1", "coral1","coral1","coral1","coral1", "deepskyblue1"),
                      name = "Day of the week",
                      labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1)) +
    scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                "Jun", "Jul", "Aug", "Sep", "Oct",
                                "Nov", "Dec")) +
    labs(y = "Count") +
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03))
  
  grid.arrange(g_top, g_bottom, heights = c(1/4, 3/4))
  
#Hourly
  fig(14, 6)
  right <- df %>%
    ggplot(aes(Hour, color = Wday %in% c("1", "7"), group = Wday %in% c("1", "7"))) +
    geom_freqpoly(stat = "count") +
    scale_color_discrete(name = "Is weekdays?", labels = c("No", "Yes")) +
    labs(y = NULL,
         title = " ") +
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03))
  
  left <- df %>%
    ggplot(aes(Hour, fill = !Hour %in% c("07", "08", "16", "17"))) +
    geom_bar(show.legend = F) +
    labs(x = "Hour",
         y = "No of Accidents",
         title = "Hourly Distribution of Accidents") +
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03))
  
  grid.arrange(left, right, widths = c(1/2, 1/2))
  
  
#Impact of weather condition on accident
  fig(15, 8)
  weather <- df %>% group_by(Severity) %>% count(Weather_Condition) %>% mutate(n = n / sum(n)) %>% filter(n > 0.02)
  weather <- weather$Weather_Condition
  
  df %>%
    filter(Weather_Condition %in% weather) %>%
    group_by(Severity) %>%
    count(Weather_Condition) %>%
    mutate(n = n / sum(n)) %>%
    ggplot(aes(reorder_within(Weather_Condition, n, Severity), n)) +
    geom_col(aes(fill = !Weather_Condition == "Clear"), show.legend = F) +
    facet_wrap(~ Severity, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    scale_y_continuous(breaks = seq(0, 0.4, 0.05), labels = percent) +
    geom_ref_line(h = 0.1, colour = "red", size = 1) +
    geom_ref_line(h = 0.3, colour = "red", size = 1) +
    labs(x = "Weather Condition",
         y = "Proportion",
         title = "Weather condition does not have a strong impact on accident severity")
  
# USing one state to analysis
  df_NJ <- df %>% filter(State == "NJ") %>% select(-State)
  df_NJ %>%
    head(5)
  
#Removing Weather condition and TMC
  df_NJ %>% count(Weather_Condition) %>% filter(n < 20) %>% select(Weather_Condition, n)
  
  drop_weather <- df_NJ %>% count(Weather_Condition) %>% filter(n < 20) %>% select(Weather_Condition)
  drop_weather <- drop_weather$Weather_Condition %>% unlist()
  df_NJ <- df_NJ %>% 
    filter(!(Weather_Condition %in% drop_weather)) %>% 
    mutate(Weather_Condition = factor(Weather_Condition))
  
  df_NJ %>% count(TMC) %>% filter(n < 10)
  
  drop_TMC <- df_NJ %>% count(TMC) %>% filter(n < 10) %>% select(TMC)
  drop_TMC <- drop_TMC$TMC %>% unlist()
  df_NJ <- df_NJ %>% filter(!TMC %in% drop_TMC) %>% mutate(TMC = factor(TMC))
  
# Grouping severity levels into 2
#Before grouping
  ggplot(df_NJ, aes(Severity, fill = !Severity %in% c(3, 4))) +
    geom_bar() +
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03)) +
    scale_fill_discrete(name = "Severity", labels = c("Severe: 3 or 4", "Not Severe: 1 or 2")) +
    labs(y = "Count",
         title = "Unbalanced severity levels")

#After grouping
  df_label <- df_NJ %>%
    mutate("Status" = factor(ifelse(Severity == "3" | Severity == "4", "Severe", "Not Severe"), 
                             levels = c("Not Severe", "Severe")))
  ggplot(df_label, aes(Status, fill = !Status == "Severe")) +
    geom_bar() +
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03)) +
    scale_fill_discrete(name = "Severity", labels = c("Severe", "Not Severe")) +
    labs(y = "Count",
         x = "Severity",
         title = "More balanced severity levels")
  
#Near Zero-Variance Predictors
nzv <- nearZeroVar(df_label, saveMetrics = T)
nzv[nzv$nzv,]

nzv_cols <- rownames(nzv[nzv$nzv,])
df_label <- select(df_label,-nzv_cols)

#Spliting data into training(60%), validation(20%) and test(20%)
set.seed(1)
df_parts <- resample_partition(df_label, c(train = 0.6, valid = 0.2, test = 0.2))
train_set <- as_tibble(df_parts$train)
valid_set <- as_tibble(df_parts$valid)
test_set <- as_tibble(df_parts$test)

#Modeling
#Sampling
#Unbalanced
ggplot(train_set, aes(Status)) +
  geom_bar(aes(fill = Status)) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03)) +
  labs(y = "Count",
       title = "Unbalanced severity levels")

#Balanced
new_train <- ovun.sample(Status ~ ., 
data = train_set, 
method = "both", p = 0.5, N = 90000, seed = 1)$data %>% as_tibble()

ggplot(new_train, aes(Status)) +
  geom_bar(aes(fill = Status)) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03)) +
  labs(y = "Count",
       title = "Balanced severity levels")

new_train <- new_train %>% select(-Severity)

#Decision trees
model_decision <- rpart(Status ~ ., data = new_train, method = "class", minsplit = 20, cp = 0.001)

fig(16, 8)
rpart.plot(model_decision, box.palette = "RdBu", shadow.col = "grey", )

   #model on validation
valid_set <- as_tibble(df_parts$valid)
valid_pred <- valid_set %>%
  mutate(pred = predict(model_decision, valid_set, type = "class"))

cm <- confusionMatrix(table(valid_pred$pred, valid_pred$Status))
tibble("Accuracy" = cm$overall[[1]], "Sensitivity" = cm$byClass[[1]],
       "Specificity" = cm$byClass[[2]], "Positive term" = cm$positive)
cm


# Random forest
model_rf <- randomForest(Status ~ ., data = new_train, mtry = 6, ntree = 500)

# see if ntree = 500 is enough
error_data <- model_rf$err.rate %>%
  as_tibble() %>%
  mutate("Trees" = seq_along(OOB)) %>%
  pivot_longer(cols = 1:3, names_to = "Type", values_to = "Error")
ggplot(error_data, aes(Trees, Error, color = Type)) +
  geom_line() +
  labs(x = "Number of Trees",
       title = "Error Rate")

# try different mtry
oob_values <- vector(length = 10)
for (i in 1:10) {
  temp_model <- randomForest(Status ~ ., data = new_train, mtry = (i + 5))
  oob_values[i] <- temp_model$err.rate[nrow(temp_model$err.rate), 1]
}
ggplot(tibble("Error" = oob_values), aes(x = 6:15, y = Error)) +
  geom_line(aes(group = 1)) +
  labs(x = "Number of Variables",
       title = "Error VS mtry")


# choose mtry = 13 as the best model
best_model <- randomForest(Status ~ ., data = new_train, mtry = 13, ntree = 500)
valid_pred_rf <- valid_set %>%
  add_predictions(best_model)
table(valid_pred_rf$Status)

confusionMatrix(valid_pred_rf$pred, valid_pred_rf$Status)


#Conclusion

result  <- tibble("Model" = c("Decision Tree", "Random Forest"),
                  "Accuracy" = c(0.779, 0.8164),
                  "Sensitivity" = c(0.7789, 0.8424),
                  "Specificity" = c(0.7791, 0.7635)) %>%
  pivot_longer(2:4, names_to = "type", values_to = "value")

fig(15, 8)
result %>% ggplot(aes(type, value, fill = factor(Model, levels = c("Decision Tree", "Random Forest")))) +
  geom_col(position = "dodge") +
  scale_fill_discrete(name = "Model") +
  labs(x = "Performance",
       y = NULL,
       title = "Comparison of model performance")