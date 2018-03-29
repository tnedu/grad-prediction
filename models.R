library(tidyverse)
library(zeallot)
library(caret)
library(doParallel)

prediction_data_8 <- read_csv("data/prediction_data_8.csv") %>%
    filter(!is.na(scale_score_mt) & !is.na(scale_score_rd) & !is.na(ready_grad)) %>%
    mutate(ready_grad = factor(if_else(ready_grad == 1, "ready", "not_ready"))) %>%
    select(cohort, ready_grad, n_absences, 
        E, I, R, S, assault, weapons, theft_vandalism, sexual_assault_harassment, drugs_alcohol,
        threat, school_rules, bullying, fighting,
        scale_score_mt, scale_score_mt_sq, scale_score_rd, scale_score_rd_sq,
        school_scale_score_mt, school_scale_score_rd, school_chronic_abs)

c(train_set, test_set) %<-% (split(prediction_data_8, prediction_data_8$cohort) %>% map(as.data.frame))

train_y <- train_set[, 2]
train_x <- train_set[, 3:ncol(train_set)]
test_y <- test_set[, 2]
test_x <- test_set[, 3:ncol(test_set)]

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

# Other methods
methods_list <- c("gbm", "rpart", "rlda", "gpls", "nnet", "xgbLinear", "xgbTree")

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

    write_rds(model, paste0("models/grade_8_", m, ".rds"))

    message("Model method: ", m, " completed.")

    stopImplicitCluster()

    return(model)

    }
)

# A safe version of predict
safely_predict <- safely(predict)
predict_list <- map(fit_list, safely_predict, test_x)
