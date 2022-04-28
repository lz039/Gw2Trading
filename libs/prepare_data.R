prepare_data <- function(df_sells, df_buys) {
  
  # clean
  df_sells <- df_sells %>% 
    drop_na(unit_price, quantity) %>% 
    mutate(rarity = as.factor(rarity),
           type = as.factor(type),
           item_type = as.factor(item_type),
           item_weight_class = as.factor(item_weight_class),
           unit_price_gold = unit_price / 10000) %>% 
    select(-unit_price)
  
  df_buys <- df_buys %>% 
    drop_na(unit_price, quantity) %>% 
    mutate(rarity = as.factor(rarity),
           type = as.factor(type),
           item_type = as.factor(item_type),
           item_weight_class = as.factor(item_weight_class),
           unit_price_gold = unit_price / 10000) %>% 
    select(-unit_price)  
  
  # filter
  df_max_buys <-  df_buys %>% 
    group_by(name) %>% 
    slice(which.max(unit_price_gold))
  
  df_min_sells <-  df_sells %>% 
    group_by(name) %>% 
    slice(which.min(unit_price_gold))
  
  # join
  df_all <- df_max_buys %>%
    mutate(quantity_buys = quantity,
           unit_price_gold_buys = unit_price_gold) %>% 
    select(-quantity, -unit_price_gold) %>% 
    right_join(df_min_sells %>% 
                 mutate(quantity_sells = quantity,
                        unit_price_gold_sells = unit_price_gold) %>% 
                 select(id, quantity_sells, unit_price_gold_sells), by = "id") %>% 
    mutate(name = name.x) %>% 
    select(-name.x, -name.y)
  
  # calculate profit
  df_all <- df_all %>% 
    mutate(unit_price_gold_diff = unit_price_gold_sells - unit_price_gold_buys,
           profit = 0.85 * unit_price_gold_sells - unit_price_gold_buys,
           more_sells = quantity_sells - quantity_buys)
  
  # filter again
  df_all <- df_all %>% 
    subset(profit > 0.04 & profit < 0.4)
  
  # drop items with profit < vendor value
  df_all <- df_all %>% 
    subset(profit * 100 > vendor_value) %>% 
    arrange(desc(profit))
  
  return(df_all)
}
