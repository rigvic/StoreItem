library(timetk)
library(tidyverse)
library(patchwork)

store_train <- read.csv("~/Desktop/StoreItem/train.csv")
store_test <- read.csv("~/Desktop/StoreItem/test.csv")


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
