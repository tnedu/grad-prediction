library(tidyverse)
library(zeallot)
library(caret)
library(doParallel)
library(yardstick)

for (grade in 6:8) {

    path <- paste0("data/prediction_data_", grade, ".csv")

    prediction_data <- read_csv(path, col_types = "idciiiiiiiiiiiiiiiiiiiidddcciiiiiiiddddddd") %>%
        filter(!is.na(scale_score_mt) & !is.na(scale_score_rd) & !is.na(ready_grad)) %>%
        mutate(ready_grad = factor(if_else(ready_grad == 1, "ready", "not_ready"))) %>%
        select(cohort, ready_grad, n_absences, enrollments,
            E, I, R, S, assault, weapons, theft_vandalism, sexual_assault_harassment, drugs_alcohol,
            threat, school_rules, bullying, fighting,
            scale_score_mt, scale_score_mt_sq, scale_score_rd, scale_score_rd_sq,
            school_scale_score_mt, school_scale_score_rd, school_chronic_abs)

    predictors <- c("n_absences", "enrollments", "E", "I", "R", "S", "assault", "weapons",
        "theft_vandalism", "sexual_assault_harassment", "drugs_alcohol", "threat", "school_rules", "bullying", "fighting",
        "scale_score_mt", "scale_score_mt_sq", "scale_score_rd", "scale_score_rd_sq",
        "school_scale_score_mt", "school_scale_score_rd", "school_chronic_abs")

    c(train_set, test_set) %<-% (split(prediction_data, prediction_data$cohort) %>% map(as.data.frame))

    train_y <- train_set$ready_grad
    train_x <- train_set[predictors]
    test_y <- test_set$ready_grad
    test_x <- test_set[predictors]
    
    # Preprocess by centering, scaling, and removing zero-variance predictors
    train_preprocess <- preProcess(train_x, method = c("scale", "center", "zv"))
    
    train_x <- predict(train_preprocess, train_x)
    test_x <- predict(train_preprocess, test_x)
    
    train_controls <- trainControl(
        summaryFunction = twoClassSummary,  # look at classification stats
        method = "repeatedcv",
        repeats = 3,                        # repeated cv is pretty standard
        savePredictions = "final",          # only need final model predictions
        classProbs = TRUE,                  # want to generate class probabilities for AUC
        search = "random")                  # this is a default, may want to specify search grid later

    # A first GLM model
    model_logistic <- train(x = train_x, y = train_y,
        method = "glm", metric = "ROC", family = "binomial", trControl = train_controls)

    # Looking at the coefficients
    summary(model_logistic$finalModel)
    ggplot(varImp(model_logistic)) + theme_minimal()

    # Predicting on test set
    test_set$probs_logistic <- predict(model_logistic, test_x, type = "prob")$ready
    ggplot(test_set, aes(x = ready_grad, y = probs_logistic)) + geom_jitter(alpha = 0.1) + theme_minimal()

    preds_logistic <- predict(model_logistic, test_x)
    confusionMatrix(preds_logistic, test_y)

    write_rds(model_logistic, paste0("models/grade_", grade, "_glm.rds"))
    
    # Other methods
    methods_list <- c("gbm", "rpart", "rlda", "nnet", "xgbLinear", "xgbTree")
    
    model_list <- vector(length(methods_list), mode = "list")
    names(model_list) <- methods_list
    
    global_args <- list(x = quote(train_x), y = quote(train_y),
        metric = "AUC", tuneLength = 50,
        trControl = train_controls)
    
    fit_list <- map(names(model_list), function(m) {
    
        registerDoParallel(cores = 8) # Use parallel processing
    
        print(m) # Update you on where things are running
        
        model_args <- c(global_args, method = m) # create a new argument object, use method here
        model <- tryCatch(do.call(train, model_args), error = function(e) NULL)

        write_rds(model, paste0("models/grade_", grade, "_", m, ".rds"))

        message("Model method: ", m, " completed.")

        stopImplicitCluster()

        return(model)

        }
    )

    # Extract test Accuracies
    accuracy_list <- map(fit_list, predict, test_x) %>%
        map(~ . == test_y) %>%
        map_dbl(mean, na.rm = TRUE)

    names(accuracy_list) <- methods_list
    accuracy_list

    # Extract `ready` probabilities by model
    ready_probs <- map(fit_list, predict, test_x, type = "prob") %>%
        map("ready")

    names(ready_probs) <- methods_list

    probs_df <- as_tibble(ready_probs) %>%
        mutate(truth = test_y)

    # Extract test AUC
    auc_list <- map_dbl(
        .x = list(quote(gbm), quote(rpart), quote(rlda), quote(nnet), quote(xgbLinear), quote(xgbTree)),
        .f = ~ roc_auc(probs_df, truth = truth, !!.)[['.estimate']]
    )

    names(auc_list) <- methods_list
    auc_list

    rm(list = ls())

}
