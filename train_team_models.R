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


dep_var <- "playoffs"
models <- list(c("playoffs_1yr", "net_rtg_1yr"),
               c("playoffs_1yr", "win_pct_1yr"))

# calculate metrics for logistic regression models
glm_metrics <- get_all_metrics(train, "glm", dep_var, models)

# calculate metrics for knn models
k_values <- c(5, 10)
knn_metrics <- list()
for(k in k_values) {
  new <- get_all_metrics(train_scaled, "knn", dep_var, models, k = k)
  knn_metrics <- c(knn_metrics, list(new))
}
knn_metrics <- bind_rows(knn_metrics)

# calculate metrics for naive bayes models
nb_metrics <- get_all_metrics(train, "nb", dep_var, models)

# calculate metrics for logistic regressions with ridge regularization
lambda_values <- c(0.001, 0.01, 0.1, 1, 10)
glmnet0_metrics = list()
for(m in models) {
  for(l in lambda_values) {
    new <- calculate_glmnet_metrics(train_scaled, dep_var, m, alpha = 0, lambda = l)
    glmnet0_metrics <- c(glmnet0_metrics, list(new))
  }
}
glmnet0_metrics <- bind_rows(glmnet0_metrics)

# calculate metrics for logistic regressions with lasso regularization
lambda_values <- c(0.001, 0.01, 0.1, 1, 10)
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
#rpart.plot(dt)
#print(dt)

# calculate metrics for random forest models
rf_metrics <- get_all_metrics(train %>% mutate(playoffs = as.factor(playoffs)), "rf", dep_var, models)

# combine all metrics into one dataframe
all_metrics <- bind_rows(monkey, glm_metrics, knn_metrics, nb_metrics, dt_metrics, rf_metrics,
                         glmnet0_metrics, glmnet1_metrics)



# further evaluation of models
ind_vars <- models[[1]]
mdl <- paste(dep_var, "~", paste(ind_vars, collapse = " + "))

glm_model <- glm(mdl, train, family = "binomial")
glm_pred_prob <- predict(glm_model, train, type = "response")

knn_model <- knn3(as.formula(mdl), data = train_scaled, k = k)
knn_pred_prob <- predict(knn_model, train_scaled, type = "prob")[, 2]

nb_model <- naiveBayes(as.formula(mdl), data = train)
nb_pred_prob <- predict(nb_model, train, type = "raw")[, 2]

dt_model <- rpart(mdl, data = train, method = "class")
dt_pred_prob <- predict(dt_model, train, type = "prob")[, 2]
rpart.plot(dt_model)
print(dt_model)

rf_model <- randomForest(as.factor(playoffs) ~ net_rtg_1yr + win_pct_1yr, data = train, type = "response")
rf_pred_prob <- predict(rf_model, train, type = "prob")[, 2]

lam <- 0.01
glm0_model <- glmnet(x = as.matrix(train_scaled %>% select(all_of(ind_vars))), y = as.matrix(train_scaled[[dep_var]]),
                     lambda = lam, alpha = 0, family = "binomial")
glm0_pred_prob <- predict(glm0_model, as.matrix(train_scaled %>% select(all_of(ind_vars))), type = "response")
glm1_model <- glmnet(x = as.matrix(train_scaled %>% select(all_of(ind_vars))), y = as.matrix(train_scaled[[dep_var]]),
                     lambda = lam, alpha = 1, family = "binomial")
glm1_pred_prob <- predict(glm1_model, as.matrix(train_scaled %>% select(all_of(ind_vars))), type = "response")


# choose models to evaluate (current model types and probs have to have matching elements)
current_model_types <- c("glm", "knn", "nb", "dt", "rf", "glm0", "glm1")
probs <- list(glm_pred_prob, knn_pred_prob, nb_pred_prob, dt_pred_prob, rf_pred_prob, glm0_pred_prob, glm1_pred_prob)
# bin probabilities and create calibration plot
num_breaks <- 11
breaks <- (seq(0, 1, length.out = num_breaks) - (1 / (num_breaks - 1)) / 2)[-1]
cal_plot <- tibble(prob = breaks)
for(p in probs) {
  bins <- cut(p, breaks = seq(0, 1, length.out = num_breaks), include.lowest = TRUE)
  emp_prob <- numeric()
  for(i in levels(bins)) {
    inds <- which(bins == i)
    actual <- (train %>% pull(playoffs))[inds]
    emp_prob <- c(emp_prob, sum(actual) / length(actual))
  }
  cal_plot <- cbind(cal_plot, tibble(emp_prob))
}

names(cal_plot) <- c("prob", current_model_types)
cal_plot <- cal_plot %>% pivot_longer(cols = -prob, names_to = "model", values_to = "emp_prob")
ggplot(cal_plot, aes(x = prob, y = emp_prob)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 0.75) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Calibration Plot", x = "Predicted probability", y = "Empirical probability") +
  facet_wrap(vars(model)) +
  proj_theme +
  theme(strip.text = element_text(size = 7),
        strip.background = element_blank())


# create precision-at-k plots
outcomes <- as.logical(train[[dep_var]])
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
prec_plot <- prec_plot %>% pivot_longer(cols = -c(k), names_to = "model", values_to = "prec_at_k")

ggplot(prec_plot, aes(x = k, y = prec_at_k)) +
  geom_line(color = "blue") +
  labs(title = "Precision-at-K Plot", x = "K", y = "Precision") +
  facet_wrap(vars(model)) +
  proj_theme +
  theme(strip.text = element_text(size = 7),
        strip.background = element_blank()) +
  facet_wrap(vars(model))

  
# create recall-at-k plots
outcomes <- as.logical(train[[dep_var]])
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
recall_plot <- recall_plot %>% pivot_longer(cols = -c(k), names_to = "model", values_to = "recall_at_k")

ggplot(recall_plot, aes(x = k, y = recall_at_k)) +
  geom_line(color = "blue") +
  labs(title = "Precision-at-K Plot", x = "K", y = "Precision") +
  facet_wrap(vars(model)) +
  proj_theme +
  theme(strip.text = element_text(size = 7),
        strip.background = element_blank()) +
  facet_wrap(vars(model))


# create precision-recall curve
pr_plot <- prec_plot %>%
  merge(recall_plot, by = c("k", "model"))

ggplot(pr_plot, aes(x = recall_at_k, y = prec_at_k, color = model)) +
  geom_line(show.legend = FALSE) +
  labs(title = "Precision-Recall Curve", x = "Recall", y = "Precision") +
  scale_color_manual(values = plot_colors) +
  proj_theme +
  theme(strip.text = element_text(size = 7),
        strip.background = element_blank(),
        legend.key = element_blank()) +
  facet_wrap(vars(model))


# create AUC curve
for(i in 1:length(probs)) {
  roc_curve <- pROC::roc(as.numeric(train[[dep_var]]), as.numeric(probs[[i]]))
  add_to_existing <- ifelse(i == 1, FALSE, TRUE)
  plot(roc_curve, main = paste0("ROC Curves"), col = plot_colors[i], lwd = 1, add = add_to_existing, mar = c(4, 4, 2, 2))
}
legend("bottomright", legend = current_model_types, col = plot_colors, bty = "n", lty = 1, lwd = 2, cex = 0.6)
