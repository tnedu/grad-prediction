library(tidyverse)
library(caret)


for (grade in 6:8) {

    path <- paste0("data/prediction_data_", grade, ".csv")
    
    ## Trained models
    models_list <- dir("models") %>%
        grep(grade, ., value = TRUE) %>%
        paste0("models/", .) %>%
        map(read_rds)
    
    predictors <- c("n_absences", "enrollments", "E", "I", "R", "S", "assault", "weapons",
        "theft_vandalism", "sexual_assault_harassment", "drugs_alcohol", "threat", "school_rules", "bullying", "fighting",
        "scale_score_mt", "scale_score_mt_sq", "scale_score_rd", "scale_score_rd_sq",
        "school_scale_score_mt", "school_scale_score_rd", "school_chronic_abs")
    
    ## Raw data to apply trained models to
    prediction_data <- read_csv(path, col_types = "idciiiiiiiiiiiiiiiiiiiidddcciiiiiiiddddddd") %>%
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
    train_preprocess <- prediction_data[predictors] %>%
        preProcess(method = c("scale", "center", "zv"))
    
    prediction_data <- predict(train_preprocess, prediction_data)
    
    # Predictions and ready probabilities by model
    preds <- map_dfc(models_list, predict, prediction_data)
    names(preds) <- dir("models") %>% grep(grade, ., value = TRUE) %>% str_replace(".rds", "") %>% paste0("preds_", .)
    
    probs <- map(models_list, predict, prediction_data, type = "prob") %>%
        map_dfc("ready")
    names(probs) <- dir("models") %>% grep(grade, ., value = TRUE) %>% str_replace(".rds", "") %>% paste0("probs_", .)
    
    evaluation_data <- bind_cols(prediction_data, preds, probs)
    
    # Ensemble models by using each prediction as a predictor and using a lm
    train_controls <- trainControl(
        summaryFunction = twoClassSummary,  # look at classification stats
        method = "repeatedcv",
        repeats = 3,                        # repeated cv is pretty standard
        savePredictions = "final",          # only need final model predictions
        classProbs = TRUE)                  # want to generate class probabilities for AUC
    
    model_ensemble <- train(x = probs, y = evaluation_data$ready_grad,
        method = "glm", family = "binomial", trControl = train_controls)
    
    model_stack <- train(x = probs, y = evaluation_data$ready_grad,
        method = "gbm", trControl = train_controls)
    
    evaluation_data <- mutate(evaluation_data,
            pred_ensemble = predict(model_ensemble, evaluation_data),
            prob_ensemble = predict(model_ensemble, evaluation_data, type = "prob")$ready,
            pred_stack = predict(model_stack, evaluation_data),
            prob_stack = predict(model_stack, evaluation_data, type = "prob")$ready
        )
    
    write_csv(evaluation_data, paste0("data/evaluation_data_", grade, ".csv", na = ""))

    rm(list = ls())
    
}
