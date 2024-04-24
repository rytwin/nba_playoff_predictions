library(tidyverse)
library(rsample)
library(Metrics)
library(caret)
library(class)
library(e1071)
library(glmnet)
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

# calculate metrics based on always predicting the most likely class (with probability equal to prevalance)
train <- train %>%
  mutate(monkey_pred_prob = actual_playoff_percent,
         monkey_pred = ifelse(actual_playoff_percent >= 0.5, 1, 0))
monkey_logloss <- logLoss(train$playoffs, train$monkey_pred_prob)
monkey_auc <- Metrics::auc(train$playoffs, train$monkey_pred_prob)
monkey_accuracy <- sum(train$monkey_pred == train$playoffs) / nrow(train)
monkey_precision <- sum(train$monkey_pred == 1 & train$playoffs == 1) / sum(train$monkey_pred == 1)
monkey_recall <- sum(train$monkey_pred == 1 & train$playoffs == 1) / sum(train$playoffs == 1)
monkey_f1 <- 2 * ((monkey_precision * monkey_recall) / (monkey_precision + monkey_recall))
monkey <- tibble(type = "monkey", name = "monkey", logloss = monkey_logloss, auc = monkey_auc, accuracy = monkey_accuracy,
                 precision = monkey_precision, recall = monkey_recall, f1_score = monkey_f1)

# calculate metrics for logistic regression model
baseline <- calculate_model_metrics(train, "glm", "playoffs ~ playoffs_1yr")
glm_metrics <- calculate_model_metrics(train, "glm", "playoffs ~ net_rtg_1yr")

# calculate metrics for knn model
knn_metrics <- calculate_model_metrics(train, "knn", "playoffs ~ net_rtg_1yr")

# calculate metrics for naive bayes model
nb_metrics <- calculate_model_metrics(train, "nb", "playoffs ~ net_rtg_1yr")

# calculate metrics for logistic regression with ridge regularization
glmnet_metrics <- calculate_model_metrics(train, "glmnet_0" , "playoffs ~ net_rtg_1yr")

# calculate metrics for logistic regression with lasso regularization
glmnet_metrics <- calculate_model_metrics(train, "glmnet_1" , "playoffs ~ net_rtg_1yr")

