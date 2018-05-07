library(tidyverse)
library(caret)

## Trained models
model_gbm <- read_rds("models/grade_8_gbm.rds")
model_rpart <- read_rds("models/grade_8_rpart.rds")
model_rlda <- read_rds("models/grade_8_rlda.rds")
model_nnet <- read_rds("models/grade_8_nnet.rds")
model_xgblinear <- read_rds("models/grade_8_xgbLinear.rds")
model_xgbtree <- read_rds("models/grade_8_xgbTree.rds")

predictors <- c("n_absences", "enrollments", "E", "I", "R", "S", "assault", "weapons",
    "theft_vandalism", "sexual_assault_harassment", "drugs_alcohol", "threat", "school_rules", "bullying", "fighting",
    "scale_score_mt", "scale_score_mt_sq", "scale_score_rd", "scale_score_rd_sq",
    "school_scale_score_mt", "school_scale_score_rd", "school_chronic_abs")

## Raw data to apply trained models to
prediction_data_8 <- read_csv("data/prediction_data_8.csv",
        col_types = "idciiiiiiiiiiiiiiiiiiiidddcciiiiiiiddddddd") %>%
    filter(cohort == 2012,
        !is.na(scale_score_mt) & !is.na(scale_score_rd) & !is.na(ready_grad)) %>%
    mutate(system = if_else(system %in% c(790L, 791L), 792L, system),
        system = case_when(
            system == 792 & school %in% c(1L, 5L, 6L, 195L) ~ 793L,
            system == 792 & school %in% c(3L, 7L, 20L, 25L, 30L, 33L, 90L, 95L, 150L, 155L, 170L) ~ 794L,
            system == 792 & school %in% c(8L, 55L, 60L, 63L, 65L, 168L, 183L, 190L) ~ 795L,
            system == 792 & school %in% c(70L, 100L, 109L, 111L, 160L) ~ 796L,
            system == 792 & school == 116L ~ 797L,
            system == 792 & school %in% c(78L, 123L, 130L, 133L) ~ 798L,
            TRUE ~ system),
        ready_grad = factor(if_else(ready_grad == 1, "ready", "not_ready")),
        econ_dis = if_else(econ_dis == 1, "ED", "Non-ED"),
        swd = if_else(swd == 1, "SWD", "Non-SWD"),
        el = if_else(el == 1, "EL", "Non-EL")) %>%
    select(system, school, ready_grad, one_of(predictors), gender, race, econ_dis, el, swd)

# Preprocess by centering, scaling, and removing zero-variance predictors for consistency
# Models were trained on centered, scaled, and zv-removed data
train_preprocess <- prediction_data_8[predictors] %>%
    preProcess(method = c("scale", "center", "zv"))

prediction_data_8 <- predict(train_preprocess, prediction_data_8)

# Predictions and ready probabilities by model
evaluation_data_8 <- prediction_data_8 %>%
    mutate(
        pred_gbm = predict(model_gbm, prediction_data_8),
        prob_gbm = predict(model_gbm, prediction_data_8, type = "prob")$ready,
        pred_rpart = predict(model_rpart, prediction_data_8),
        prob_rpart = predict(model_rpart, prediction_data_8, type = "prob")$ready,
        pred_rlda = predict(model_rlda, prediction_data_8),
        prob_rlda = predict(model_rlda, prediction_data_8, type = "prob")$ready,
        pred_nnet = predict(model_nnet, prediction_data_8),
        prob_nnet = predict(model_nnet, prediction_data_8, type = "prob")$ready,
        pred_xgblinear = predict(model_xgblinear, prediction_data_8),
        prob_xgblinear = predict(model_xgblinear, prediction_data_8, type = "prob")$ready,
        pred_xgbtree = predict(model_xgbtree, prediction_data_8),
        prob_xgbtree = predict(model_xgbtree, prediction_data_8, type = "prob")$ready
    )

write_csv(evaluation_data_8, "data/evaluation_data_8.csv", na = "")
