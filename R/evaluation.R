library(acct)
library(caret)
library(tidyverse)
library(yardstick)

model_gbm <- read_rds("models/grade_8_gbm.rds")
model_rpart <- read_rds("models/grade_8_rpart.rds")
model_rlda <- read_rds("models/grade_8_rlda.rds")
model_nnet <- read_rds("models/grade_8_nnet.rds")
model_xgblinear <- read_rds("models/grade_8_xgbLinear.rds")
model_xgbtree <- read_rds("models/grade_8_xgbTree.rds")

prediction_data_8 <- read_csv("data/prediction_data_8.csv") %>%
    filter(cohort == 2012,
        !is.na(scale_score_mt) & !is.na(scale_score_rd) & !is.na(ready_grad)) %>%
    mutate(ready_grad = factor(if_else(ready_grad == 1, "ready", "not_ready"))) %>%
    select(system, school, ready_grad, n_absences, 
        E, I, R, S, assault, weapons, theft_vandalism, sexual_assault_harassment, drugs_alcohol,
        threat, school_rules, bullying, fighting,
        scale_score_mt, scale_score_mt_sq, scale_score_rd, scale_score_rd_sq,
        school_scale_score_mt, school_scale_score_rd, school_chronic_abs)

# Preprocess by centering, scaling, and removing zero-variance predictors for consistency
# Models were trained on centered, scaled, and zv data
train_preprocess <- prediction_data_8[c("n_absences", "E", "I", "R", "S", "assault", "weapons",
        "theft_vandalism", "sexual_assault_harassment", "drugs_alcohol", "threat", "school_rules", "bullying", "fighting",
        "scale_score_mt", "scale_score_mt_sq", "scale_score_rd", "scale_score_rd_sq",
        "school_scale_score_mt", "school_scale_score_rd", "school_chronic_abs")] %>%
    preProcess(method = c("scale", "center", "zv"))

prediction_data_8 <- predict(train_preprocess, prediction_data_8)

# Predictions and ready probabilities by model
prediction_data_8$pred_gbm <- predict(model_gbm, prediction_data_8)
prediction_data_8$prob_gbm <- predict(model_gbm, prediction_data_8, type = "prob")$ready

prediction_data_8$pred_rpart <- predict(model_rpart, prediction_data_8)
prediction_data_8$prob_rpart <- predict(model_rpart, prediction_data_8, type = "prob")$ready

prediction_data_8$pred_rlda <- predict(model_rlda, prediction_data_8)
prediction_data_8$prob_rlda <- predict(model_rlda, prediction_data_8, type = "prob")$ready

prediction_data_8$pred_nnet <- predict(model_nnet, prediction_data_8)
prediction_data_8$prob_nnet <- predict(model_nnet, prediction_data_8, type = "prob")$ready

prediction_data_8$pred_xgblinear <- predict(model_xgblinear, prediction_data_8)
prediction_data_8$prob_xgblinear <- predict(model_xgblinear, prediction_data_8, type = "prob")$ready

prediction_data_8$pred_xgbtree <- predict(model_xgbtree, prediction_data_8)
prediction_data_8$prob_xgbtree <- predict(model_xgbtree, prediction_data_8, type = "prob")$ready

# Accuracy by district
accuracy_by_district <- prediction_data_8 %>%
    mutate(accuracy_gbm = ready_grad == pred_gbm,
        accuracy_rpart = ready_grad == pred_rpart,
        accuracy_rlda = ready_grad == pred_rlda,
        accuracy_nnet = ready_grad == pred_nnet,
        accuracy_xgblinear = ready_grad == pred_xgblinear,
        accuracy_xgbtree = ready_grad == pred_xgbtree) %>%
    group_by(system) %>%
    summarise_at(vars(starts_with("accuracy")), ~ round5(100 * mean(.), 1))

ggplot(accuracy_by_district, aes(x = factor(system), y = accuracy_gbm)) +
    geom_bar(stat = "identity", width = 1) +
    scale_x_discrete(limits = factor(accuracy_by_district$system)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank())

# AUC by district
districts_list <- split(prediction_data_8, prediction_data_8$system) 

auc_by_district <- tibble(
    system = names(districts_list),
    auc_gbm = map_dbl(districts_list, roc_auc, ready_grad, prob_gbm),
    auc_rpart = map_dbl(districts_list, roc_auc, ready_grad, prob_rpart),
    auc_rlda = map_dbl(districts_list, roc_auc, ready_grad, prob_rlda),
    auc_nnet = map_dbl(districts_list, roc_auc, ready_grad, prob_nnet),
    auc_xgblinear = map_dbl(districts_list, roc_auc, ready_grad, prob_xgblinear),
    auc_xgbtree = map_dbl(districts_list, roc_auc, ready_grad, prob_xgbtree)
)

# Accuracy by school
accuracy_by_school <- prediction_data_8 %>%
    mutate(accuracy_gbm = ready_grad == pred_gbm,
        accuracy_rpart = ready_grad == pred_rpart,
        accuracy_rlda = ready_grad == pred_rlda,
        accuracy_nnet = ready_grad == pred_nnet,
        accuracy_xgblinear = ready_grad == pred_xgblinear,
        accuracy_xgbtree = ready_grad == pred_xgbtree) %>%
    group_by(system, school) %>%
    summarise_at(vars(starts_with("accuracy")), ~ round5(100 * mean(.), 1))

# AUC by school
schools_list <- prediction_data_8 %>%
    mutate(school = paste(system, school)) %>%
    split(.$school)

safely_auc <- safely(roc_auc, otherwise = NA_real_)

auc_by_school <- tibble(
    school = names(schools_list),
    auc_gbm = map(schools_list, safely_auc, ready_grad, prob_gbm) %>% map_dbl("result"),
    auc_rpart = map(schools_list, safely_auc, ready_grad, prob_rpart) %>% map_dbl("result"),
    auc_rlda = map(schools_list, safely_auc, ready_grad, prob_rlda) %>% map_dbl("result"),
    auc_nnet = map(schools_list, safely_auc, ready_grad, prob_nnet) %>% map_dbl("result"),
    auc_xgblinear = map(schools_list, safely_auc, ready_grad, prob_xgblinear) %>% map_dbl("result"),
    auc_xgbtree = map(schools_list, safely_auc, ready_grad, prob_xgbtree) %>% map_dbl("result")
)
