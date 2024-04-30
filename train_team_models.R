library(tidyverse)
library(rsample)
library(Metrics)
library(caret)
library(class)
library(e1071)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
source("functions.R")

# read in data, filter for years >= 1996, drop NA values, merge with player-based variables
player_data <- read.csv("data/team_stats_from_players.csv")
data <- read.csv("data/team_features.csv") %>%
  filter(year >= 1996) %>%
  drop_na(playoffs_1yr) %>%
  merge(player_data, by = c("team", "year"), all.x = TRUE)

# train-test split
set.seed(365)
split <- initial_split(data, 0.8)
train <- training(split)
test <- testing(split)

# calculate true prevalance (in training set)
actual_playoff_percent <- sum(train$playoffs) / nrow(train)

# dynamically determine binary variables and turn them into factors (so they aren't standardized)
binary_variables <- character()
for(col in colnames(train)) {
  uniq <- unique(train[[col]])
  if(length(uniq) == 2 & 0 %in% uniq & 1 %in% uniq) {
    binary_variables <- c(binary_variables, col)
  }
}
train <- train %>%
  mutate(across(c(year, all_of(binary_variables)), as.factor))

# standardize variables (and then convert binary variables back into numeric values instead of factors)
std_scaler <- preProcess(train)
train_scaled <- predict(std_scaler, train) %>%
  mutate(across(all_of(binary_variables), as.numeric),
         across(all_of(binary_variables), ~ ifelse(. == 1, 0, 1)))
test_scaled <- predict(std_scaler, test) %>%
  mutate(across(all_of(binary_variables), as.numeric),
         across(all_of(binary_variables), ~ ifelse(. == 1, 0, 1)))

train <- train %>%
  mutate(across(all_of(binary_variables), as.numeric),
         across(all_of(binary_variables), ~ ifelse(. == 1, 0, 1)))


# calculate metrics based on always predicting the most likely class (with probability equal to prevalance)
monkey_pred_prob = rep(actual_playoff_percent, nrow(test))
monkey_pred = ifelse(monkey_pred_prob >= 0.5, 1, 0)
monkey_logloss <- logLoss(test$playoffs, monkey_pred_prob)
monkey_auc <- Metrics::auc(test$playoffs, monkey_pred_prob)
monkey_accuracy <- sum(monkey_pred == test$playoffs) / nrow(test)
monkey_precision <- sum(monkey_pred == 1 & test$playoffs == 1) / sum(monkey_pred == 1)
monkey_recall <- sum(monkey_pred == 1 & test$playoffs == 1) / sum(test$playoffs == 1)
monkey_f1 <- 2 * ((monkey_precision * monkey_recall) / (monkey_precision + monkey_recall))
monkey <- tibble(type = "monkey", name = "monkey", logloss = monkey_logloss, auc = monkey_auc, accuracy = monkey_accuracy,
                 precision = monkey_precision, recall = monkey_recall, f1_score = monkey_f1)

# calculate metrics based on always predicting the same as the previous year
baseline_pred_prob = test$playoffs_1yr
baseline_logloss <- logLoss(test$playoffs, baseline_pred_prob)
baseline_auc <- Metrics::auc(test$playoffs, baseline_pred_prob)
baseline_accuracy <- sum(baseline_pred_prob == test$playoffs) / nrow(test)
baseline_precision <- sum(baseline_pred_prob == 1 & test$playoffs == 1) / sum(baseline_pred_prob == 1)
baseline_recall <- sum(baseline_pred_prob == 1 & test$playoffs == 1) / sum(test$playoffs == 1)
baseline_f1 <- 2 * ((baseline_precision * baseline_recall) / (baseline_precision + baseline_recall))
baseline <- tibble(type = "baseline", name = "playoffs_1yr", logloss = baseline_logloss, auc = baseline_auc, accuracy = baseline_accuracy,
                   precision = baseline_precision, recall = baseline_recall, f1_score = baseline_f1)


dep_var <- "playoffs"
models <- list(#c("win_pct_1yr"),
               # c("win_pct_1yr", "allnba_team_diff"))
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "second_rd_1yr"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "second_rd_1yr"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "new_coach_from_end"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "new_coach_from_end"),
               # c("win_pct_1yr", "second_rd_1yr", "new_coach_from_end"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "num_conf_teams"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "num_conf_teams"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "second_rd_1yr", "num_conf_teams"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "second_rd_1yr", "num_conf_teams"),
               # c("win_pct_1yr", "new_coach_from_end", "num_conf_teams", "allnba_team_diff"))
               # c("win_pct_1yr", "conf_win_pct_1yr", "new_coach_from_end", "num_conf_teams"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "new_coach_from_end", "num_conf_teams"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "num_conf_teams"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "num_conf_teams"),
               # c("win_pct_1yr", "conf_finals_1yr"),
               # c("win_pct_1yr", "net_rtg_1yr"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "conf_finals_1yr"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "conf_finals_1yr"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "second_rd_1yr", "conf_finals_1yr"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "second_rd_1yr", "conf_finals_1yr"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "new_coach_from_end", "conf_finals_1yr"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "new_coach_from_end", "conf_finals_1yr"),
               # c("win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_finals_1yr"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_finals_1yr"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_finals_1yr"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "num_conf_teams", "conf_finals_1yr"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "num_conf_teams", "conf_finals_1yr"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "second_rd_1yr", "num_conf_teams", "conf_finals_1yr"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "second_rd_1yr", "num_conf_teams", "conf_finals_1yr"),
               # c("win_pct_1yr", "new_coach_from_end", "num_conf_teams", "conf_finals_1yr"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "new_coach_from_end", "num_conf_teams", "conf_finals_1yr"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "new_coach_from_end", "num_conf_teams", "conf_finals_1yr"),
               # c("win_pct_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "num_conf_teams", "conf_finals_1yr"),
               # c("win_pct_1yr", "playoffs_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "num_conf_teams", "conf_finals_1yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff"),
               # c(names(train)[6:32], names(train[39:65])))
               # c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr"))
               c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr", "allnba_team_diff"))
               # c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "proj_win_pct_last2yr"),
               # c("net_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr"),
               # c("net_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr"),
               # c("net_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "proj_win_pct_last2yr"),
               # c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "num_conf_teams"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "playoffs_last2yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "playoffs_last3yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "second_rd_last2yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "second_rd_last3yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "win_pct_last3yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "proj_win_pct_last2yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "win_pct_last2yr_over60"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "win_pct_last3yr_over60"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "champion_1yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "finals_1yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "conf_finals_1yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "coach_preseason_g_tm"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "top_ws_1yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "sos_1yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "coach_preseason_win_pct_adj"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "coach_preseason_win_pct_tm_adj"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "srs_1yr"),
               # c("net_rtg_1yr", "off_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "conf_p_win_pct_1yr", "coach_win_pct_diff", "win_pct_last2yr", "proj_win_pct_last3yr", "mov_1yr"),
               # c("win_pct_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "coach_win_pct_diff", "new_coach_from_end", "second_rd_1yr", "conf_p_win_pct_1yr"))


# calculate metrics for logistic regression models
glm_metrics <- get_all_metrics(train, "glm", dep_var, models)

# calculate metrics for knn models
k_values <- c(53)
knn_metrics <- list()
for(k in k_values) {
  new <- get_all_metrics(train_scaled, "knn", dep_var, models, k = k)
  knn_metrics <- c(knn_metrics, list(new))
}
knn_metrics <- bind_rows(knn_metrics)

# calculate metrics for naive bayes models
nb_metrics <- get_all_metrics(train, "nb", dep_var, models)

# calculate metrics for logistic regressions with ridge regularization
lambda_values <- c(0.009)
glmnet0_metrics = list()
for(m in models) {
  for(l in lambda_values) {
    new <- calculate_glmnet_metrics(train_scaled, dep_var, m, alpha = 0, lambda = l)
    glmnet0_metrics <- c(glmnet0_metrics, list(new))
  }
}
glmnet0_metrics <- bind_rows(glmnet0_metrics)

# calculate metrics for logistic regressions with lasso regularization
lambda_values <- c(0.0025)
glmnet1_metrics = list()
for(m in models) {
  for(l in lambda_values) {
    new <- calculate_glmnet_metrics(train_scaled, dep_var, m, alpha = 1, lambda = l)
    glmnet1_metrics <- c(glmnet1_metrics, list(new))
  }
}
glmnet1_metrics <- bind_rows(glmnet1_metrics)

# calculate metrics for decision tree models
dt_metrics <- get_all_metrics(train, "dt", dep_var, models)

# calculate metrics for random forest models
rf_metrics <- get_all_metrics(train %>% mutate(playoffs = as.factor(playoffs)), "rf", dep_var, models)

# combine all metrics into one dataframe
all_metrics <- bind_rows(monkey, baseline, glm_metrics, glmnet0_metrics, glmnet1_metrics) #knn_metrics, nb_metrics, dt_metrics, rf_metrics)


# further evaluation of models
# glm_vars <- c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr")
glm_vars <- c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr", "allnba_team_diff")
glm_model <- glm(paste(dep_var, "~", paste(glm_vars, collapse = " + ")), train, family = "binomial")
glm_pred_prob <- predict(glm_model, test, type = "response")
glm_coef <- coef(glm_model)

# k = 66
# knn_vars <- c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr")
k = 53
knn_vars <- c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr", "allnba_team_diff")
knn_model <- knn3(as.formula(paste(dep_var, "~", paste(knn_vars, collapse = " + "))), data = train_scaled, k = k)
knn_pred_prob <- predict(knn_model, test_scaled, type = "prob")[, 2]

# nb_vars <- c("win_pct_1yr")
nb_vars <- c("win_pct_1yr", "allnba_team_diff")
nb_model <- naiveBayes(as.formula(paste(dep_var, "~", paste(nb_vars, collapse = " + "))), data = train)
nb_pred_prob <- predict(nb_model, test, type = "raw")[, 2]

# dt_vars <- c("win_pct_1yr", "conf_win_pct_1yr", "second_rd_1yr", "num_conf_teams")
dt_vars <- c("win_pct_1yr", "new_coach_from_end", "num_conf_teams", "allnba_team_diff")
dt_model <- rpart(paste(dep_var, "~", paste(dt_vars, collapse = " + ")), data = train, method = "class")
dt_pred_prob <- predict(dt_model, test, type = "prob")[, 2]
rpart.plot(dt_model)
print(dt_model)

# rf_vars <- c(names(train)[6:32], names(train)[39:59])
rf_vars <- c(names(train)[6:32], names(train)[39:65])
rf_train <- train %>%
  mutate(playoffs = as.factor(playoffs))
rf_test <- test %>%
  mutate(playoffs = as.factor(playoffs))
rf_model <- randomForest(as.formula(paste(dep_var, "~", paste(rf_vars, collapse = " + "))), data = rf_train, type = "response")
rf_pred_prob <- predict(rf_model, rf_test, type = "prob")[, 2]

lam0 <- 0.0092
glm0_vars <- c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr")
# lam0 <- 0.0104
# glm0_vars <- c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr", "allnba_team_diff")
glm0_model <- glmnet(x = as.matrix(train_scaled %>% select(all_of(glm0_vars))), y = as.matrix(train_scaled[[dep_var]]),
                     lambda = lam0, alpha = 0, family = "binomial")
glm0_pred_prob <- predict(glm0_model, as.matrix(test_scaled %>% select(all_of(glm0_vars))), type = "response")
glm0_coef <- coef(glm0_model)

# lam1 <- 0.0021
# glm1_vars <- c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr")
lam1 <- 0.0027
glm1_vars <- c("off_rtg_1yr", "def_rtg_1yr", "conf_win_pct_1yr", "second_rd_1yr", "new_coach_from_end", "coach_win_pct_diff", "win_pct_last2yr", "allnba_team_diff")
glm1_model <- glmnet(x = as.matrix(train_scaled %>% select(all_of(glm1_vars))), y = as.matrix(train_scaled[[dep_var]]),
                     lambda = lam1, alpha = 1, family = "binomial")
glm1_pred_prob <- predict(glm1_model, as.matrix(test_scaled %>% select(all_of(glm1_vars))), type = "response")
glm1_coef <- coef(glm1_model)

predictions <- tibble(team = test$team, year = test$year, actual = test$playoffs, monkey = monkey_pred_prob, baseline = baseline_pred_prob, knn = knn_pred_prob,
                      nb = nb_pred_prob, glm = glm_pred_prob, glm0 = glm0_pred_prob, glm1 = glm_pred_prob, dt = dt_pred_prob, rf = rf_pred_prob)
# predictions2 <- tibble(team = train$team, year = train$year, actual = train$playoffs, monkey = monkey_pred_prob, baseline = baseline_pred_prob, knn = knn_pred_prob,
#                        nb = nb_pred_prob, glm = glm_pred_prob, glm0 = glm0_pred_prob, glm1 = glm_pred_prob, dt = dt_pred_prob, rf = rf_pred_prob)


# choose models to evaluate (current model types and probs must have matching elements)
current_model_types <- c("glm", "rf", "dt", "glm0", "glm1")#"knn", "nb", 
probs <- list(glm_pred_prob, rf_pred_prob, dt_pred_prob, glm0_pred_prob, glm1_pred_prob) #nb_pred_prob, knn_pred_prob, 

# bin probabilities and create calibration plot
num_breaks <- 11
breaks <- (seq(0, 1, length.out = num_breaks) - (1 / (num_breaks - 1)) / 2)[-1]
cal_plot <- tibble(prob = breaks)
for(p in probs) {
  bins <- cut(p, breaks = seq(0, 1, length.out = num_breaks), include.lowest = TRUE)
  emp_prob <- numeric()
  for(i in levels(bins)) {
    inds <- which(bins == i)
    actual <- (test %>% pull(playoffs))[inds]
    emp_prob <- c(emp_prob, sum(actual) / length(actual))
  }
  cal_plot <- cbind(cal_plot, tibble(emp_prob))
}

names(cal_plot) <- c("prob", current_model_types)
cal_plot <- cal_plot %>% pivot_longer(cols = -prob, names_to = "model", values_to = "emp_prob") %>%
  mutate(model = gsub("glm0", "Logistic Ridge", model),
         model = gsub("glm1", "Logistic Lasso", model),
         model = gsub("glm", "Logistic", model),
         model = gsub("rf", "Random Forest", model),
         model = gsub("dt", "Decision Tree", model))
ggplot(cal_plot, aes(x = prob, y = emp_prob, color = model)) +
  geom_line() +
  geom_point(size = 0.75, show.legend = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = plot_colors) +
  labs(title = "Calibration", x = "Predicted probability", y = "Empirical probability") +
  proj_theme +
  theme(strip.text = element_text(size = 10),
        strip.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 8))
  facet_wrap(vars(model))


# create precision-at-k plots
outcomes <- as.logical(test[[dep_var]])
prec_plot <- tibble(k = 1:length(outcomes))
for(p in probs) {
  index_sort <- order(p, decreasing = TRUE)
  outcomes_sort <- outcomes[index_sort]
  
  prec_at_k <- numeric()
  for(k in 1:length(outcomes)) {
    prec <- sum(outcomes_sort[1:k]) / k
    prec_at_k <- c(prec_at_k, prec)
  }
  prec_plot <- cbind(prec_plot, prec_at_k)
}
names(prec_plot) <- c("k", current_model_types)
prec_plot <- prec_plot %>% pivot_longer(cols = -c(k), names_to = "model", values_to = "prec_at_k") %>%
  mutate(model = gsub("glm0", "Logistic Ridge", model),
         model = gsub("glm1", "Logistic Lasso", model),
         model = gsub("glm", "Logistic", model),
         model = gsub("rf", "Random Forest", model),
         model = gsub("dt", "Decision Tree", model))
ggplot(prec_plot, aes(x = k, y = prec_at_k, color = model)) +
  geom_line() +
  labs(title = "Precision at K", x = "K", y = "Precision") +
  scale_color_manual(values = plot_colors) +
  proj_theme +
  theme(strip.text = element_text(size = 10),
        strip.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 8))
  facet_wrap(vars(model))

  
# create recall-at-k plots
outcomes <- as.logical(test[[dep_var]])
recall_plot <- tibble(k = 1:length(outcomes))
for(p in probs) {
  index_sort <- order(p, decreasing = TRUE)
  outcomes_sort <- outcomes[index_sort]
  
  recall_at_k <- numeric()
  for(k in 1:length(outcomes)) {
    recall <- sum(outcomes_sort[1:k]) / sum(outcomes)
    recall_at_k <- c(recall_at_k, recall)
  }
  recall_plot <- cbind(recall_plot, recall_at_k)
}
names(recall_plot) <- c("k", current_model_types)
recall_plot <- recall_plot %>% pivot_longer(cols = -c(k), names_to = "model", values_to = "recall_at_k") %>%
  mutate(model = gsub("glm0", "Logistic Ridge", model),
         model = gsub("glm1", "Logistic Lasso", model),
         model = gsub("glm", "Logistic", model),
         model = gsub("rf", "Random Forest", model),
         model = gsub("dt", "Decision Tree", model))
ggplot(recall_plot, aes(x = k, y = recall_at_k, color = model)) +
  geom_line() +
  labs(title = "Recall at K", x = "K", y = "Recall") +
  scale_color_manual(values = plot_colors) +
  proj_theme +
  theme(strip.text = element_text(size = 10),
        strip.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 8))
  facet_wrap(vars(model))


# create precision-recall curve
pr_plot <- prec_plot %>%
  merge(recall_plot, by = c("k", "model"))

ggplot(pr_plot, aes(x = recall_at_k, y = prec_at_k, color = model)) +
  geom_line() +
  labs(title = "Precision-Recall Curve", x = "Recall", y = "Precision") +
  scale_color_manual(values = plot_colors) +
  proj_theme +
  theme(strip.text = element_text(size = 10),
        strip.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 8))
  facet_wrap(vars(model))


# create AUC curve
roc_df <- tibble()
for(i in 1:length(probs)) {
  roc_curve <- pROC::roc(as.numeric(test[[dep_var]]), as.numeric(probs[[i]]))
  new_roc <- tibble(model = current_model_types[i], fpr = 1 - coords(roc_curve)$specificity, tpr = coords(roc_curve)$sensitivity)
  roc_df <- rbind(roc_df, new_roc)
}
roc_df <- roc_df %>% filter(model %in% c("glm", "glm0", "glm1", "rf", "dt")) %>%
  mutate(model = gsub("glm0", "Logistic Ridge", model),
         model = gsub("glm1", "Logistic Lasso", model),
         model = gsub("glm", "Logistic", model),
         model = gsub("rf", "Random Forest", model),
         model = gsub("dt", "Decision Tree", model))
ggplot(roc_df, aes(x = fpr, y = tpr, color = model)) +
  geom_line(show.legend = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "ROC Curve", x = "False positive rate", y = "True positive rate") +
  scale_color_manual(values = plot_colors) +
  proj_theme +
  theme(strip.text = element_text(size = 10),
        strip.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  facet_wrap(vars(model))

