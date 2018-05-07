library(caret)
library(tidyverse)
library(yardstick)
library(rgdal)
library(leaflet)

## Evaluation data with predictions
evaluation_data_8 <- read_csv("data/evaluation_data_8.csv") %>%
    mutate_at(vars("ready_grad", "pred_gbm", "pred_rpart", "pred_rlda", "pred_nnet", "pred_xgblinear", "pred_xgbtree"), factor)

## Accuracy by district
accuracy_by_district <- evaluation_data_8 %>%
    mutate(
        accuracy_gbm = ready_grad == pred_gbm,
        accuracy_rpart = ready_grad == pred_rpart,
        accuracy_rlda = ready_grad == pred_rlda,
        accuracy_nnet = ready_grad == pred_nnet,
        accuracy_xgblinear = ready_grad == pred_xgblinear,
        accuracy_xgbtree = ready_grad == pred_xgbtree
    ) %>%
    group_by(system) %>%
    summarise_at(vars(starts_with("accuracy")), ~ round(100 * mean(.), 1)) %>%
    mutate(accuracy_max = pmax(accuracy_gbm, accuracy_rpart, accuracy_rlda, accuracy_nnet, accuracy_xgblinear, accuracy_xgbtree)) %>%
    arrange(accuracy_max)

accuracy_by_district %>%
    select(-accuracy_max) %>%
    gather(model, accuracy, starts_with("accuracy")) %>%
    ggplot(aes(x = factor(system), y = accuracy, color = model)) +
        scale_x_discrete(limits = factor(accuracy_by_district$system)) +
        scale_y_continuous(limits = c(0, 100)) +
        geom_point(alpha = 0.5) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.x = element_blank(),
            legend.position = "bottom")

plotly::ggplotly()

## AUC by district
districts_list <- split(evaluation_data_8, evaluation_data_8$system)

auc_by_district <- tibble(
    system = as.numeric(names(districts_list)),
    district_size = map_int(districts_list, nrow),
    auc_gbm = map_dbl(districts_list, roc_auc, ready_grad, prob_gbm),
    auc_rpart = map_dbl(districts_list, roc_auc, ready_grad, prob_rpart),
    auc_rlda = map_dbl(districts_list, roc_auc, ready_grad, prob_rlda),
    auc_nnet = map_dbl(districts_list, roc_auc, ready_grad, prob_nnet),
    auc_xgblinear = map_dbl(districts_list, roc_auc, ready_grad, prob_xgblinear),
    auc_xgbtree = map_dbl(districts_list, roc_auc, ready_grad, prob_xgbtree)
) %>%
    mutate(auc_max = pmax(auc_gbm, auc_rpart, auc_rlda, auc_nnet, auc_xgblinear, auc_xgbtree)) %>%
    arrange(auc_max)

auc_by_district %>%
    select(-auc_max) %>%
    gather(model, auc, starts_with("auc")) %>%
    ggplot(aes(x = factor(system), y = auc, color = model)) +
        geom_point(alpha = 0.5) +
        scale_x_discrete(limits = factor(auc_by_district$system)) +
        scale_y_continuous(limits = c(0, 1)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.x = element_blank(),
            legend.position = "bottom")

plotly::ggplotly()

# AUC by District Size
auc_by_district %>%
    select(-auc_max) %>%
    gather(model, auc, starts_with("auc_")) %>%
    ggplot(aes(x = district_size, y = auc)) +
        geom_point() +
        scale_y_continuous(limits = c(0, 1)) +
        facet_wrap(~model, nrow = 3) + 
        theme_minimal()

# Maps of Accuracy and AUC by district ----------------------------------------------------------------------------
shapefile <- readOGR("data/shapefile/EDGE_SCHOOLDISTRICT_TL17_SY1516/schooldistrict_sy1516_tl17.shp")

shapefile <- shapefile[shapefile$STATEFP == "47", ]

shapefile@data$UNSDLEA <- as.character(shapefile@data$UNSDLEA)
shapefile@data$ELSDLEA <- as.character(shapefile@data$ELSDLEA)
shapefile@data$SCSDLEA <- as.character(shapefile@data$SCSDLEA)

shapefile@data$UNSDLEA <- pmax(shapefile$ELSDLEA, shapefile$UNSDLEA, shapefile$SCSDLEA, na.rm = TRUE)

xwalk <- read_csv("data/nces_district_crosswalk.csv")

shapefile@data <- left_join(shapefile@data, xwalk, by = "UNSDLEA") %>%
    left_join(auc_by_district, by = "system") %>%
    left_join(accuracy_by_district, by = "system") %>%
    as.data.frame()

# AUC map by district
bins_auc <- c(0, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
pal_auc <- colorBin("YlOrRd", domain = shapefile@data$auc_max, bins = bins_auc)

labels_auc <- paste(shapefile$NAME, "<br>", "AUC: ", round(shapefile$auc_max, 4)) %>%
    map(htmltools::HTML)

leaflet(shapefile) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal_auc(auc_max),
        color = "#444444", weight = 1, fillOpacity = 0.5,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = labels_auc) %>%
    addLegend(pal = pal_auc, values = ~auc_max, opacity = 0.7, title = "AUC",
        position = "bottomright")

# Accuracy map by district
bins_accuracy <- c(70, 75, 80, 85, 90, 95, 100)
pal_accuracy <- colorBin("YlOrRd", domain = shapefile@data$accuracy_max, bins = bins_accuracy)

labels_accuracy <- paste(shapefile$NAME, "<br>", "Accuracy: ", round(shapefile$accuracy_max, 4)) %>%
    map(htmltools::HTML)

leaflet(shapefile) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal_accuracy(accuracy_max),
        color = "#444444", weight = 1, fillOpacity = 0.5,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = labels_accuracy) %>%
    addLegend(pal = pal_accuracy, values = ~accuracy_max, opacity = 0.7, title = "Accuracy",
        position = "bottomright")

# Accuracy and AUC by student group -------------------------------------------------------------------------------
table(evaluation_data_8$race, evaluation_data_8$ready_grad)

groups_list <- c(
    split(evaluation_data_8, evaluation_data_8$race),
    split(evaluation_data_8, evaluation_data_8$econ_dis),
    split(evaluation_data_8, evaluation_data_8$swd),
    split(evaluation_data_8, evaluation_data_8$el)
)

accuracy_by_group <- tibble(
    group = names(groups_list),
    accuracy_gbm = map_dbl(groups_list, ~ mean(.$ready_grad == .$pred_gbm)),
    accuracy_rpart = map_dbl(groups_list, ~ mean(.$ready_grad == .$pred_rpart)),
    accuracy_rlda = map_dbl(groups_list, ~ mean(.$ready_grad == .$pred_rlda)),
    accuracy_nnet = map_dbl(groups_list, ~ mean(.$ready_grad == .$pred_nnet)),
    accuracy_xgblinear = map_dbl(groups_list, ~ mean(.$ready_grad == .$pred_xgblinear)),
    accuracy_xgbtree = map_dbl(groups_list, ~ mean(.$ready_grad == .$pred_xgbtree))
)

accuracy_by_group %>%
    gather(model, accuracy, starts_with("accuracy_")) %>%
    ggplot(aes(x = group, y = accuracy, color = model)) +
        geom_point() +
        scale_y_continuous(limits = c(0, 1)) +
        theme_minimal()

# AUC by student groups
auc_by_group <- tibble(
    group = names(groups_list),
    auc_gbm = map_dbl(groups_list, roc_auc, ready_grad, prob_gbm),
    auc_rpart = map_dbl(groups_list, roc_auc, ready_grad, prob_rpart),
    auc_rlda = map_dbl(groups_list, roc_auc, ready_grad, prob_rlda),
    auc_nnet = map_dbl(groups_list, roc_auc, ready_grad, prob_nnet),
    auc_xgblinear = map_dbl(groups_list, roc_auc, ready_grad, prob_xgblinear),
    auc_xgbtree = map_dbl(groups_list, roc_auc, ready_grad, prob_xgbtree)
)

auc_by_group %>%
    gather(model, auc, starts_with("auc_")) %>%
    ggplot(aes(x = group, y = auc, color = model)) +
        geom_point() +
        scale_y_continuous(limits = c(0, 1)) +
        theme_minimal()

# Accuracy and AUC by school --------------------------------------------------------------------------------------
## Accuracy by school
accuracy_by_school <- evaluation_data_8 %>%
    mutate(accuracy_gbm = ready_grad == pred_gbm,
        accuracy_rpart = ready_grad == pred_rpart,
        accuracy_rlda = ready_grad == pred_rlda,
        accuracy_nnet = ready_grad == pred_nnet,
        accuracy_xgblinear = ready_grad == pred_xgblinear,
        accuracy_xgbtree = ready_grad == pred_xgbtree) %>%
    group_by(system, school) %>%
    summarise_at(vars(starts_with("accuracy")), ~ round(100 * mean(.), 1)) %>%
    mutate(accuracy_max = pmax(accuracy_gbm, accuracy_rpart, accuracy_rlda, accuracy_nnet, accuracy_xgblinear, accuracy_xgbtree))

## AUC by school
schools_list <- evaluation_data_8 %>%
    mutate(school = paste(system, school)) %>%
    split(.$school)

safely_auc <- safely(roc_auc, otherwise = NA_real_)

auc_by_school <- tibble(
    school = names(schools_list),
    school_size = map_int(schools_list, nrow),
    auc_gbm = map(schools_list, safely_auc, ready_grad, prob_gbm) %>% map_dbl("result"),
    auc_rpart = map(schools_list, safely_auc, ready_grad, prob_rpart) %>% map_dbl("result"),
    auc_rlda = map(schools_list, safely_auc, ready_grad, prob_rlda) %>% map_dbl("result"),
    auc_nnet = map(schools_list, safely_auc, ready_grad, prob_nnet) %>% map_dbl("result"),
    auc_xgblinear = map(schools_list, safely_auc, ready_grad, prob_xgblinear) %>% map_dbl("result"),
    auc_xgbtree = map(schools_list, safely_auc, ready_grad, prob_xgbtree) %>% map_dbl("result")
) %>%
    mutate(auc_max = pmax(auc_gbm, auc_rpart, auc_rlda, auc_nnet, auc_xgblinear, auc_xgbtree))

# Are we over- or under-predicting ready graduates?
# This could be analogous to value-add for ready graduates
diffs <- evaluation_data_8 %>%
    group_by(system) %>%
    summarise_at(c("ready_grad", "pred_gbm", "pred_rpart", "pred_rlda", "pred_nnet", "pred_xgblinear", "pred_xgbtree"),
        ~ mean(. == "ready")) %>%
    rowwise() %>%
    mutate(pred_mean = mean(pred_gbm, pred_rpart, pred_rlda, pred_nnet, pred_xgblinear, pred_xgbtree)) %>%
    ungroup() %>%
    mutate(diff = ready_grad - pred_mean)

shapefile@data <- shapefile@data %>%
    left_join(diffs, by = "system") %>%
    as.data.frame()

pal_diffs <- colorQuantile("YlOrRd", domain = shapefile@data$diff, n = 5)

labels_diffs <- paste(shapefile$NAME, "<br>", "% Ready Grad - Predicted: ", round(shapefile$diff, 4)) %>%
    map(htmltools::HTML)

leaflet(shapefile) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal_diffs(diff),
        color = "#444444", weight = 1, fillOpacity = 0.5,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = labels_diffs) %>%
    addLegend(pal = pal_diffs, values = ~diff, opacity = 0.7, title = "Ready Grad - Predicted Percentile",
        position = "bottomright")

# AUC by School Size
auc_by_school %>%
    select(-auc_max) %>%
    gather(model, auc, starts_with("auc_")) %>%
    ggplot(aes(x = school_size, y = auc)) +
        geom_point() +
        scale_y_continuous(limits = c(0, 1)) +
        facet_wrap(~ model, nrow = 3) + 
        theme_minimal()
