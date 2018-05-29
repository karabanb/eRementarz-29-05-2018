
library(archivist)


# nie zabanglalo, nie mozna zainstalowac pakieyu bit

library(DALEX)
library(randomForest)

rf_model <- randomForest(sqm_price ~.,data = houses)


rf_expl <- DALEX::explain(rf_model, data = houses, y = houses$sqm_price)

## pojedyncza cecha

year_expl <- single_variable(rf_expl, "year")
plot(year_expl)

district_expl <- single_variable(rf_expl, "district")
plot(district_expl)

## waznosc zmiennych

global_feat_imp <- variable_importance(rf_expl)
plot(global_feat_imp)


## ocena modelu

rf_perf <- model_performance(rf_expl)
plot(rf_perf, geom = "boxplot")
plot(rf_perf, geom = "ecdf")

## diagnostyka modelu

library(auditor)

rf_audit <- audit(rf_expl)
plotPrediction(rf_audit)
plotResidualDensity(rf_audit, variable = "district")


linear_model <- lm(sqm_price ~.,data = houses)
lm_explainer <- DALEX::explain(linear_model, data = houses)
breakdown_linear <- single_prediction(lm_explainer, houses[4062, -3])
plot(breakdown_linear)

rf_explainer <- single_prediction(rf_expl, houses[4062, -3])
plot(rf_explainer)


## LIVE

library(live)
library(mlr)

set.seed(997)
new_dataset <- sample_locally2(data = houses,
                               explained_instance = houses[1089,],
                               explained_var = "sqm_price",
                               size = 1500)

with_predictions <- add_predictions2(new_dataset, rf_model)
live_explantation <- fit_explanation2(with_predictions, "regr.lm")
live_explantation

plot_explanation2(live_explantation, "forest")
plot_explanation2(live_explantation, "waterfall")
