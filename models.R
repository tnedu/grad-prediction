library(tidyverse)
library(caret)
library(doParallel)

# Use parallel processing
cl <- makeCluster(8)
registerDoParallel(cl)

prediction_data_8 <- read_csv("data/prediction_data_8.csv") %>%
    filter(!is.na(scale_score_mt) & !is.na(scale_score_rd) & !is.na(ready_grad)) %>%
    mutate(ready_grad = factor(ready_grad))

train <- filter(prediction_data_8, cohort == 2012)
test <- filter(prediction_data_8, cohort == 2011)

ctrl <- trainControl(method = "cv", number = 10)

# A first GLM model
model_logistic <- train(ready_grad ~ n_absences +
        E + I + R + S + assault + weapons + theft_vandalism + sexual_assault_harassment + drugs_alcohol +
        threat + school_rules + bullying + fighting +
        scale_score_mt + scale_score_mt_sq + scale_score_rd + scale_score_rd_sq +
        school_scale_score_mt + school_scale_score_rd + school_chronic_abs,
    data = train, method = "glm", family = "binomial", trControl = ctrl)

model_logistic

# Looking at the coefficients
coef(model_logistic$finalModel)
varImp(model_logistic)

# Predicting on test set
test$probs_logistic <- predict(model_logistic, test, type = "prob")$`1`
ggplot(test, aes(x = ready_grad, y = probs_logistic)) + geom_jitter(alpha = 0.1) + theme_minimal()

test$preds_logistic <- predict(model_logistic, test)

confusionMatrix(test$preds_logistic, test$ready_grad)

# Random Forest
model_rf <- train(ready_grad ~ n_absences +
        E + I + R + S + assault + weapons + theft_vandalism + sexual_assault_harassment + drugs_alcohol +
        threat + school_rules + bullying + fighting +
        scale_score_mt + scale_score_mt_sq + scale_score_rd + scale_score_rd_sq +
        school_scale_score_mt + school_scale_score_rd + school_chronic_abs,
    data = train, method = "ranger", trControl = ctrl)

test$preds_rf <- predict(model_rf, test)

confusionMatrix(test$preds_rf, test$ready_grad)

# SVM with a Polynomial Kernel
model_svm <- train(ready_grad ~ n_absences +
        E + I + R + S + assault + weapons + theft_vandalism + sexual_assault_harassment + drugs_alcohol +
        threat + school_rules + bullying + fighting +
        scale_score_mt + scale_score_mt_sq + scale_score_rd + scale_score_rd_sq +
        school_scale_score_mt + school_scale_score_rd + school_chronic_abs,
    data = train, method = "svmPoly", trControl = ctrl)

test$preds_svm <- predict(model_svm, test)

confusionMatrix(test$preds_svm, test$ready_grad)

# SVM with a RBF Kernel
model_rbf <- train(ready_grad ~ n_absences +
        E + I + R + S + assault + weapons + theft_vandalism + sexual_assault_harassment + drugs_alcohol +
        threat + school_rules + bullying + fighting +
        scale_score_mt + scale_score_mt_sq + scale_score_rd + scale_score_rd_sq +
        school_scale_score_mt + school_scale_score_rd + school_chronic_abs,
    data = train, method = "svmRadial", trControl = ctrl)

test$preds_rbf <- predict(model_rbf, test)

confusionMatrix(test$preds_rbf, test$ready_grad)

# K Nearest Neighbors
model_knn <- train(ready_grad ~ n_absences +
        E + I + R + S + assault + weapons + theft_vandalism + sexual_assault_harassment + drugs_alcohol +
        threat + school_rules + bullying + fighting +
        scale_score_mt + scale_score_mt_sq + scale_score_rd + scale_score_rd_sq +
        school_scale_score_mt + school_scale_score_rd + school_chronic_abs,
    data = train, method = "knn", trControl = ctrl, tuneLength = 25)

test$preds_knn <- predict(model_knn, test)

confusionMatrix(test$preds_knn, test$ready_grad)

stopCluster(cl)
