build_train_model <- function(df_all) {
  set.seed(42)
  
  # Put 3/4 of the data into the training set 
  data_split <- initial_split(df_all, 
                              prop = 3/4, 
                              strata = profit, 
                              breaks = 4)
  
  # Create dataframes for the two sets:
  train_data <- training(data_split) 
  test_data <- testing(data_split)
  
  df_train <- train_data %>% 
    select(id, name, unit_price_gold_sells, unit_price_gold_buys, type, rarity, level) %>% 
    drop_na()
  
  sells_rec <- 
    recipe(unit_price_gold_sells ~ ., data = df_train) %>% 
    update_role(id, name, new_role = "ID") %>% 
    step_dummy(all_nominal_predictors())%>% 
    step_zv(all_predictors()) %>%  # remove zero vectors
    step_center(all_predictors()) %>%
    step_scale(all_predictors())
  
  lasso_mod <- 
    linear_reg(penalty = 0.1, mixture = 1) %>% 
    set_engine("glmnet")
  
  sells_fit <- 
    workflow() %>% 
    add_recipe(sells_rec) %>%
    add_model(lasso_mod) %>% 
    fit(data = df_train)
}