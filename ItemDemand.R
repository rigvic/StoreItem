library(timetk)
library(tidyverse)
library(tidymodels)
library(patchwork)
library(vroom)
library(modeltime)
library(forecast)

store_train <- vroom("./train.csv")
store_test <- vroom("./test.csv")


s1 <- store_train %>% 
  filter(store==7, item==27) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)
s2 <- store_train %>% 
  filter(store==3, item==29) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)
s3 <- store_train %>% 
  filter(store==8, item==8) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)
s4 <- store_train %>% 
  filter(store==2, item==17) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)

wrap_plots(s1, s2, s3, s4)


nStores <- max(store_train$store)
nItems <- max(store_train$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    store_train <- store_train %>%
      filter(store == s, item == i)
    store_test <- store_test %>%
      filter(store == s, item == i)
    
    
    
    if(s == 1 & i ==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
  }
}

store_item <- store_train %>%
  filter(store == 4, item == 45)

my_recipe <- recipe(sales ~. , data = store_item) %>%
  step_date(date, features = "dow") %>%
  step_date(date, features = "month")

prep <- prep(my_recipe)
baked <- bake(prep, new_data = store_item)

rf_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")


rf_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_mod)

tuning_grid <- grid_regular(mtry(range=c(1,ncol(store_item)-1)),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(store_item, v = 10, repeats = 1)

CV_results <- rf_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(smape))

select_best(CV_results)

bestTune <- CV_results %>%
  select_best("smape")

collect_metrics(CV_results)

# EXPONENTIAL SMOOTHING
# STORE_ITEM1
store_item1 <- store_train %>%
  filter(store == 9, item == 33)
store_item2 <- store_train %>%
  filter(store == 8, item == 22)

split <- time_series_split(store_item1, assess="3 months", cumulative = TRUE)
split %>%
tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(split))

## Visualize CV results
plot1 <- cv_results %>%
modeltime_forecast(
                   new_data = testing(split),
                   actual_data = store_item1
) %>%
plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
modeltime_accuracy() %>%
table_modeltime_accuracy(
                         .interactive = FALSE
)


## Refit to all data then forecast
es_fullfit <- cv_results %>%
modeltime_refit(data = store_item1)

es_preds <- es_fullfit %>%
modeltime_forecast(h = "3 months") %>%
rename(date=.index, sales=.value) %>%
select(date, sales) %>%
full_join(., y=store_test, by="date") %>%
select(id, sales)

plot2 <- es_fullfit %>%
modeltime_forecast(h = "3 months", actual_data = store_item1) %>%
plot_modeltime_forecast(.interactive=FALSE)

# STORE_ITEM2
split <- time_series_split(store_item2, assess="3 months", cumulative = TRUE)
split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)


es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(split))

## Visualize CV results
plot3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(split),
    actual_data = store_item2
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )


## Refit to all data then forecast
es_fullfit <- cv_results %>%
  modeltime_refit(data = store_item2)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=store_test, by="date") %>%
  select(id, sales)

plot4 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = store_item2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(plot1, plot2, plot3, plot4, nrows = 2)

# SARIMA
store_item1 <- store_train %>%
  filter(store == 5, item == 12)
store_item1_test <- store_test %>%
  filter(store==5, item==12)

arima_recipe <- recipe(sales ~. , data = store_item1) %>%
  step_date(date, features = "dow") %>%
  step_date(date, features = "month")

arima_mod <- arima_reg(seasonal_period = 365,
                       non_seasonal_ar = 5,
                       non_seasonal_ma = 5,
                       seasonal_ar = 2,
                       seasonal_ma = 2,
                       non_seasonal_differences = 2,
                       seasonal_differences = 2) %>%
  set_engine("auto_arima")

split <- time_series_split(store_item1, assess="3 months", cumulative = TRUE)
split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_mod) %>%
  fit(data = training(split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(split))

## Visualize CV results
plot1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(split),
    actual_data = store_item1
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )


## Refit to all data then forecast
arima_fullfit <- cv_results %>%
  modeltime_refit(data = store_item1)

arima_preds <- arima_fullfit %>%
  modeltime_forecast(new_data = store_item1_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=store_test, by="date") %>%
  select(id, sales)

plot2 <- arima_fullfit %>%
  modeltime_forecast(new_data = store_item1_test, actual_data = store_item1) %>%
  plot_modeltime_forecast(.interactive=FALSE)

# STORE_ITEM2
store_item2 <- store_train %>%
  filter(store == 9, item == 4)
store_item2_test <- store_test %>%
  filter(store==9, item==4)

arima_recipe <- recipe(sales ~. , data = store_item2) %>%
  step_date(date, features = "dow") %>%
  step_date(date, features = "month")

arima_mod <- arima_reg(seasonal_period = 365,
                       non_seasonal_ar = 5,
                       non_seasonal_ma = 5,
                       seasonal_ar = 2,
                       seasonal_ma = 2,
                       non_seasonal_differences = 2,
                       seasonal_differences = 2) %>%
  set_engine("auto_arima")

arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_mod) %>%
  fit(data = training(split))

split <- time_series_split(store_item2, assess="3 months", cumulative = TRUE)
split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

## Cross-validate to tune model
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(split))

## Visualize CV results
plot3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(split),
    actual_data = store_item2
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )


## Refit to all data then forecast
arima_fullfit <- cv_results %>%
  modeltime_refit(data = store_item2)

arima_preds <- arima_fullfit %>%
  modeltime_forecast(new_data = store_item2_test) %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=store_test, by="date") %>%
  select(id, sales)

plot4 <- arima_fullfit %>%
  modeltime_forecast(new_data = store_item2_test, actual_data = store_item2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(plot1, plot2, plot3, plot4, nrows = 2)

# PROPHET
store_item1 <- store_train %>%
  filter(store == 6, item == 31)

split <- time_series_split(store_item1, assess="3 months", cumulative = TRUE)
split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

pro_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(pro_model,
                                  new_data = testing(split))

## Visualize CV results
plot1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(split),
    actual_data = store_item1
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )


## Refit to all data then forecast
pro_fullfit <- cv_results %>%
  modeltime_refit(data = store_item1)

pro_preds <- pro_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=store_test, by="date") %>%
  select(id, sales)

plot2 <- pro_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = store_item1) %>%
  plot_modeltime_forecast(.interactive=FALSE)

# STORE_ITEM2
store_item2 <- store_train %>%
  filter(store == 10, item == 22)

split <- time_series_split(store_item2, assess="3 months", cumulative = TRUE)
split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

pro_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(pro_model,
                                  new_data = testing(split))

## Visualize CV results
plot3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(split),
    actual_data = store_item2
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )


## Refit to all data then forecast
pro_fullfit <- cv_results %>%
  modeltime_refit(data = store_item2)

pro_preds <- pro_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=store_test, by="date") %>%
  select(id, sales)

plot4 <- pro_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = store_item2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(plot1, plot3, plot2, plot4, nrows = 2)


# KAGGLE SUBMISSION
nstores <- max(store_train$store)
nitems <- max(store_train$item)
for(s in 1:nstores){
  for(i in 1:nitems){
    train <- store_train %>%
      filter(store == s, item == 9)
    test <- store_test %>%
      filter(store == s, item == i)
    split <- time_series_split(store_train, assess="3 months", cumulative = TRUE)
    split %>%
      tk_time_series_cv_plan() %>% #Put into a data frame
      plot_time_series_cv_plan(date, sales, .interactive=FALSE)
    
    pro_model <- prophet_reg() %>%
      set_engine(engine = "prophet") %>%
      fit(sales ~ date, data = training(split))
    
    ## Cross-validate to tune model
    cv_results <- modeltime_calibrate(pro_model,
                                      new_data = testing(split))
    
    ## Evaluate the accuracy
    cv_results %>%
      modeltime_accuracy() %>%
      table_modeltime_accuracy(
        .interactive = FALSE
      )
    
    
    ## Refit to all data then forecast
    pro_fullfit <- cv_results %>%
      modeltime_refit(data = store_train)
    
    pro_preds <- pro_fullfit %>%
      modeltime_forecast(new_data = store_test) %>%
      rename(date=.index, sales=.value) %>%
      select(date, sales) %>%
      full_join(., y=store_test, by="date") %>%
      select(id, sales)
    
    if(s == 1 & i == 1){
      all_preds <- pro_preds
    } else {
      all_preds <- bind_rows(all_preds, pro_preds)
    }
  }
}

vroom_write(all_preds, file = "prophet.csv", delim = ",")



