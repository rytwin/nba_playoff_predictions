library(tidyverse)
library(rsample)
library(Metrics)
library(caret)
source("functions.R")

# read in data, filter for years > 1996, and drop NA values
data <- read.csv("data/team_features.csv") %>%
  filter(year >= 1996) %>%
  drop_na(playoffs_1yr)

# train-test split
set.seed(365)
split <- initial_split(data, 0.8)
train <- training(split)
test <- testing(split)

# calculate true prevalance (in training set)
actual_playoff_percent <- sum(train$playoffs) / nrow(train)

# calculate metrics based on always predicting the most likely class
train <- train %>%
  mutate(monkey_pred_prob = actual_playoff_percent,
         monkey_pred = ifelse(actual_playoff_percent >= 0.5, 1, 0))
monkey_logloss <- logLoss(train$playoffs, train$monkey_pred_prob)
monkey_auc <- Metrics::auc(train$playoffs, train$monkey_pred_prob)
monkey_accuracy <- sum(train$monkey_pred == train$playoffs) / nrow(train)
monkey_precision <- sum(train$monkey_pred == 1 & train$playoffs == 1) / sum(train$monkey_pred == 1)
monkey_recall <- sum(train$monkey_pred == 1 & train$playoffs == 1) / sum(train$playoffs == 1)
monkey_f1 <- 2 * ((monkey_precision * monkey_recall) / (monkey_precision + monkey_recall))

# calculate metrics based on always predicting the same as the season before
baseline <- calculate_model_metrics(train, "playoffs ~ playoffs_1yr")

# calculate metrics based on always predicting the same as the season before
new_model <- calculate_model_metrics(train, "playoffs ~ net_rtg_1yr")





