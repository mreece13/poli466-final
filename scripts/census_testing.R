rm(list=ls())

library(tidyverse)

census <- read_csv("data/secc_deduped.csv", col_types = cols_only(
  state = "c",
  district = "c",
  tehsil = "c",
  gender = "c",
  age = "d",
  deprivation_count = "c",
  total_members = "d",
  hh_summary_deprivation = "d"
)) %>% 
  distinct()

census %>% 
  group_by(state) %>% 
  summarise(age_median = median(age, na.rm = TRUE),
            #deprivation_count_max = group_by(.x, deprivation_count) %>% tally() %>% arrange(-n) %>% select(n) %>% top_n(1),
            total_members_median = median(total_members, na.rm = TRUE),
            deprivation_median = median(hh_summary_deprivation, na.rm = TRUE)
  )

common_counter <- function(df){
  df %>% group_by(deprivation_count) %>% tally() %>% arrange(-n) %>% select(-n) %>% top_n(1)
  
}

census %>% 
  select(state, district, deprivation_count) %>% 
  group_by(state, district) %>% 
  nest() %>% 
  mutate(deprivation_common = lapply(data, common_counter)) %>% 
  select(-data) %>% 
  unnest(deprivation_common)