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

# dynamically determine binary variables, and standardize all other variables
binary_variables <- character()
for(col in colnames(train)) {
  uniq <- unique(train[[col]])
  if(length(uniq) == 2 & 0 %in% uniq & 1 %in% uniq) {
    binary_variables <- c(binary_variables, col)
  }
}
train <- train %>%
  mutate(across(-c(year, team, all_of(binary_variables)), scale))

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

models <- c("playoffs ~ playoffs_1yr",
            "playoffs ~ net_rtg_1yr",
            "playoffs ~ playoffs_1yr + net_rtg_1yr")

# calculate metrics for logistic regression models
glm_metrics2 <- get_all_metrics(train, "glm", models)

# calculate metrics for knn models
k_values <- c(5, 10)
knn_metrics <- list()
for(k in k_values) {
  new <- get_all_metrics(train, "knn", models, k = k)
  knn_metrics <- c(knn_metrics, list(new))
}
knn_metrics <- bind_rows(knn_metrics)

# calculate metrics for naive bayes models
nb_metrics <- get_all_metrics(train, "nb", models)

# calculate metrics for logistic regressions with ridge regularization
glmnet0_metrics <- calculate_model_metrics(train, "glmnet_0" , "playoffs ~ net_rtg_1yr")

# calculate metrics for logistic regressions with lasso regularization
glmnet1_metrics <- calculate_model_metrics(train, "glmnet_1" , "playoffs ~ net_rtg_1yr")

all_metrics <- bind_rows(monkey, glm_metrics, knn_metrics, nb_metrics) %>%
  mutate(k = ifelse(type != "knn", NA, k))
