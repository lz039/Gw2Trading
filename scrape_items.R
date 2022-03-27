# load packages ----------------------------------------------------------------

library(tidyverse)
library(httr)
library(jsonlite)
library(readr)

# function: scrape_pac ---------------------------------------------------------

scrape_items <- function() {
  
  cat("Starting function")
  
  listings_response <- GET("https://api.guildwars2.com/v2/commerce/listings")
  all_items <-  content(listings_response, "text") %>% fromJSON()
  
  item_list <- tibble()
  price_list_buys <- NULL
  price_list_sells <- NULL
  
  cat(paste("Found ", nrow(all_items), "items in total"))
  all_items <- all_items %>% head(2000)
  cat(paste("Took ", nrow(all_items), "items for processing"))
  
  for (item in all_items)
  {
    # get price details
    # rather use ?ids=24,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84
    price_response <- GET(paste("https://api.guildwars2.com/v2/commerce/listings?ids=24,68,69", item,
                                sep = ""))
    price_response_n <- content(price_response, "text") %>% fromJSON()
    
    price_response_n$buys <- price_response_n$buys %>% as.tibble() %>% 
      mutate(id = item)
    price_response_n$sells <- price_response_n$sells %>% as.tibble() %>% 
      mutate(id = item)
    
    price_list_buys <- price_list_buys %>% bind_rows(price_response_n$buys)
    price_list_sells <- price_list_sells %>% bind_rows(price_response_n$sells)
    
    # get items details
    item_response <- GET(paste("https://api.guildwars2.com/v2/items/", item, sep = ""))
    item_response_n <- content(item_response, "text") %>% fromJSON()
    
    item_df <- as.data.frame(enframe(unlist(item_response_n))) %>% 
      pivot_wider(names_from = "name", values_from = "value") %>% 
      select(any_of(c("name", "description", "type", "rarity",
                      "vendor_value", "id", "icon"))) %>% 
      mutate(
        id = as.integer(id),
        vendor_value = as.double(vendor_value)
      )
    
    if (count(item_list) == 0)
    {
      item_list <- item_df
    }
    else
    {
      item_list <- item_list %>% 
        add_row(item_df)
    }
    
    cat(paste("Currently at ", nrow(item_list), "items."))
  }
  
  item_list_buys <- item_list %>% left_join(price_list_buys)
  item_list_sells <- item_list %>% left_join(price_list_sells)
  
  dir <- getwd()
  write_csv(item_list_buys, file = paste(dir, "/gw2-all-buys.csv", sep = ""))
  write_csv(item_list_sells, file = paste(dir, "/gw2-all-sells.csv", sep = ""))
}

scrape_items()