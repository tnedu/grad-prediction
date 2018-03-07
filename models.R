library(tidyverse)
library(zeallot)
library(caret)
library(doParallel)

# Use parallel processing
cl <- makeCluster(8)
registerDoParallel(cl)

prediction_data_8 <- read_csv("data/prediction_data_8.csv") %>%
    filter(!is.na(scale_score_mt) & !is.na(scale_score_rd) & !is.na(ready_grad)) %>%
    mutate(ready_grad = factor(if_else(ready_grad == 1, "ready", "not_ready"))) %>%
    select(cohort, ready_grad, n_absences, 
        E, I, R, S, assault, weapons, theft_vandalism, sexual_assault_harassment, drugs_alcohol,
        threat, school_rules, bullying, fighting,
        scale_score_mt, scale_score_mt_sq, scale_score_rd, scale_score_rd_sq,
        school_scale_score_mt, school_scale_score_rd, school_chronic_abs)

c(train, test) %<-% (split(prediction_data_8, prediction_data_8$cohort) %>% map(as.data.frame))

train_y <- train[, 2]
train_x <- train[, 3:ncol(train)]
test_y <- test[, 2]
test_x <- test[, 3:ncol(test)]

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

model_logistic

# Looking at the coefficients
summary(model_logistic$finalModel)
ggplot(varImp(model_logistic)) + theme_minimal()

# Predicting on test set
test$probs_logistic <- predict(model_logistic, test_x, type = "prob")$ready
ggplot(test, aes(x = ready_grad, y = probs_logistic)) + geom_jitter(alpha = 0.1) + theme_minimal()

preds_logistic <- predict(model_logistic, test_x)
confusionMatrix(preds_logistic, test_y)

stopCluster(cl)
