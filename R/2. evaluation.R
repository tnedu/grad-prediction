library(acct)
library(caret)
library(tidyverse)
library(yardstick)
library(rgdal)
library(leaflet)

## Trained models
model_gbm <- read_rds("models/grade_8_gbm.rds")
model_rpart <- read_rds("models/grade_8_rpart.rds")
model_rlda <- read_rds("models/grade_8_rlda.rds")
model_nnet <- read_rds("models/grade_8_nnet.rds")
model_xgblinear <- read_rds("models/grade_8_xgbLinear.rds")
model_xgbtree <- read_rds("models/grade_8_xgbTree.rds")

predictors <- c("n_absences", "E", "I", "R", "S", "assault", "weapons",
    "theft_vandalism", "sexual_assault_harassment", "drugs_alcohol", "threat", "school_rules", "bullying", "fighting",
    "scale_score_mt", "scale_score_mt_sq", "scale_score_rd", "scale_score_rd_sq",
    "school_scale_score_mt", "school_scale_score_rd", "school_chronic_abs")

## Raw data to apply trained models to
prediction_data_8 <- read_csv("data/prediction_data_8.csv",
        col_types = "idciiiiiiiiiiiiiiiiiiidddcciiiiiiiddddddd") %>%
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

## Accuracy by district
accuracy_by_district <- prediction_data_8 %>%
    mutate(accuracy_gbm = ready_grad == pred_gbm,
        accuracy_rpart = ready_grad == pred_rpart,
        accuracy_rlda = ready_grad == pred_rlda,
        accuracy_nnet = ready_grad == pred_nnet,
        accuracy_xgblinear = ready_grad == pred_xgblinear,
        accuracy_xgbtree = ready_grad == pred_xgbtree) %>%
    group_by(system) %>%
    summarise_at(vars(starts_with("accuracy")), ~ round5(100 * mean(.), 1)) %>%
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
districts_list <- split(prediction_data_8, prediction_data_8$system)

auc_by_district <- tibble(
    system = as.numeric(names(districts_list)),
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
        scale_x_discrete(limits = factor(auc_by_district$system)) +
        geom_point(alpha = 0.5) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.x = element_blank(),
            legend.position = "bottom")

plotly::ggplotly()

# Maps of Accuracy and AUC by district ----------------------------------------------------------------------------
shapefile <- readOGR("data/shapefile/EDGE_SCHOOLDISTRICT_TL17_SY1516/schooldistrict_sy1516_tl17.shp")

shapefile <- shapefile[shapefile$STATEFP == "47", ]

shapefile@data$UNSDLEA <- as.character(shapefile@data$UNSDLEA)
shapefile@data$ELSDLEA <- as.character(shapefile@data$ELSDLEA)
shapefile@data$SCSDLEA <- as.character(shapefile@data$SCSDLEA)

shapefile@data$UNSDLEA <- pmax(shapefile$ELSDLEA, shapefile$UNSDLEA, shapefile$SCSDLEA, na.rm = TRUE)

xwalk <- tibble::tribble(
    ~system, ~UNSDLEA,
    10, "00090",
    12, "03240",
    20, "00180",
    30, "00240",
    40, "00270",
    50, "00300",
    51, "00060",
    52, "02700",
    60, "00330",
    61, "00690",
    70, "00420",
    80, "00450",
    92, "01890",
    93, "02010",
    94, "02790",
    95, "03900",
    97, "04490",
    100, "00510",
    101, "01110",
    110, "00570",
    120, "00600",
    130, "00630",
    140, "00660",
    150, "00750",
    151, "03210",
    160, "00780",
    161, "02610",
    162, "04200",
    170, "00850",
    180, "00900",
    190, "03180",
    200, "00960",
    210, "00990",
    220, "01020",
    230, "01050",
    231, "01080",
    240, "01170",
    250, "01230",
    260, "01290",
    271, "01950",
    272, "02970",
    273, "04100",
    274, "01390",
    275, "01400",
    280, "01410",
    290, "01440",
    300, "01470",
    301, "01500",
    310, "01530",
    320, "00001",
    330, "01590",
    340, "01620",
    350, "01650",
    360, "01680",
    370, "01740",
    371, "03660",
    380, "01770",
    390, "01800",
    391, "02460",
    400, "01830",
    401, "03360",
    410, "01860",
    420, "01920",
    430, "01980",
    440, "02070",
    450, "02100",
    460, "02160",
    470, "02220",
    480, "02280",
    490, "02310",
    500, "02340",
    510, "02430",
    520, "02490",
    521, "01200",
    530, "02520",
    531, "02400",
    540, "02820",
    541, "00120",
    542, "01140",
    550, "02880",
    560, "02550",
    570, "02580",
    580, "02640",
    581, "03540",
    590, "02670",
    600, "02760",
    610, "02910",
    620, "03000",
    621, "04050",
    630, "03030",
    640, "03060",
    650, "03090",
    660, "03270",
    661, "04260",
    670, "03330",
    680, "03390",
    690, "03420",
    700, "03450",
    710, "03480",
    720, "03510",
    721, "00930",
    730, "03590",
    740, "03600",
    750, "03690",
    760, "03720",
    761, "03300",
    770, "03750",
    780, "03780",
    792, "00148",
    793, "00152",
    794, "00153",
    795, "00149",
    796, "00151",
    798, "00150",
    800, "03870",
    810, "03960",
    820, "03990",
    821, "00360",
    822, "02190",
    830, "04020",
    840, "04080",
    850, "04170",
    860, "04230",
    870, "04290",
    880, "04320",
    890, "04350",
    900, "04380",
    901, "02130",
    910, "04440",
    920, "04470",
    930, "04500",
    940, "04530",
    941, "01260",
    950, "04550",
    951, "02370"
)

shapefile@data$order <- 1:nrow(shapefile@data)

shapefile@data <- left_join(shapefile@data, xwalk, by = "UNSDLEA") %>%
    left_join(auc_by_district, by = "system") %>%
    left_join(accuracy_by_district, by = "system") %>%
    arrange(order) %>%
    as.data.frame()

# AUC map by district
bins_auc <- c(0, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
pal_auc <- colorBin("YlOrRd", domain = shapefile@data$auc_max, bins = bins_auc)

labels_auc <- paste(shapefile$NAME, "<br>", "AUC: ", round5(shapefile$auc_max, 4)) %>%
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

labels_accuracy <- paste(shapefile$NAME, "<br>", "Accuracy: ", round5(shapefile$accuracy_max, 4)) %>%
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
table(prediction_data_8$race, prediction_data_8$ready_grad)

groups_list <- c(split(prediction_data_8, prediction_data_8$race),
    split(prediction_data_8, prediction_data_8$econ_dis),
    split(prediction_data_8, prediction_data_8$swd),
    split(prediction_data_8, prediction_data_8$el))

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
accuracy_by_school <- prediction_data_8 %>%
    mutate(accuracy_gbm = ready_grad == pred_gbm,
        accuracy_rpart = ready_grad == pred_rpart,
        accuracy_rlda = ready_grad == pred_rlda,
        accuracy_nnet = ready_grad == pred_nnet,
        accuracy_xgblinear = ready_grad == pred_xgblinear,
        accuracy_xgbtree = ready_grad == pred_xgbtree) %>%
    group_by(system, school) %>%
    summarise_at(vars(starts_with("accuracy")), ~ round5(100 * mean(.), 1)) %>%
    mutate(accuracy_max = pmax(accuracy_gbm, accuracy_rpart, accuracy_rlda, accuracy_nnet, accuracy_xgblinear, accuracy_xgbtree))

## AUC by school
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
) %>%
    mutate(auc_max = pmax(auc_gbm, auc_rpart, auc_rlda, auc_nnet, auc_xgblinear, auc_xgbtree))

# Are we over- or under-predicting ready graduates?
# This could be analogous to value-add for ready graduates
diffs <- prediction_data_8 %>%
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

labels_diffs <- paste(shapefile$NAME, "<br>", "% Ready Grad - Predicted: ", round5(shapefile$diff, 4)) %>%
    map(htmltools::HTML)

leaflet(shapefile) %>%
    addTiles() %>%
    addPolygons(fillColor = ~pal_diffs(diff),
        color = "#444444", weight = 1, fillOpacity = 0.5,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = labels_diffs) %>%
    addLegend(pal = pal_diffs, values = ~diff, opacity = 0.7, title = "Ready Grad - Predicted Percentile",
        position = "bottomright")
