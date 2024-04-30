library(dplyr)

model1 <- read.csv("data/output/team_predictions_no_players.csv") %>%
  select(team:actual, glm0) %>%
  rename(pred_prob = glm0) %>%
  mutate(pred = ifelse(pred_prob >= 0.5, 1, 0))
model2 <- read.csv("data/output/team_predictions_players.csv") %>%
  select(team:actual, glm0) %>%
  rename(pred_prob = glm0) %>%
  mutate(pred = ifelse(pred_prob >= 0.5, 1, 0))
model3 <- read.csv("data/output/team_predictions_playersmod.csv") %>%
  select(team:actual, glm0) %>%
  rename(pred_prob = glm0) %>%
  mutate(pred = ifelse(pred_prob >= 0.5, 1, 0))

models <- c(list(model1), list(model2), list(model3))

metrics <- list()
for(i in 1:length(models)){
  conf_matrix <- confusionMatrix(as.factor(models[[i]]$pred), as.factor(models[[i]]$actual), positive = "1")
  accuracy <- conf_matrix$overall["Accuracy"]
  precision <- conf_matrix$byClass["Precision"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1_score <- conf_matrix$byClass["F1"]
  log_loss <- logLoss(models[[i]]$actual, models[[i]]$pred_prob)
  auc_roc <- Metrics::auc(models[[i]]$actual, models[[i]]$pred_prob)
  
  new_metrics <- tibble(model = i, logloss = log_loss, auc = auc_roc, accuracy = accuracy,
                        precision = precision, recall = recall, f1_score = f1_score)
  metrics <- c(metrics, list(new_metrics))
}

metrics <- bind_rows(metrics)
