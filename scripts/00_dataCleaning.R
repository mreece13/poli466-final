rm(list=ls())
library(tidyverse)

df <- read_csv("/Users/mason/Documents/Github/poll-station-metadata/poll_station_metadata_all.csv", guess_max = 100000)

turnout <- read_csv("pcwise_voterturnout.csv")

df %>% 
  group_by(district) %>% 
  tally


df %>% 
  group_by(`PS is in Govt building/Premises`) %>% 
  mutate(`PS is in Govt building/Premises` = recode(`PS is in Govt building/Premises`, "YES" = "Yes", "NO" = "No")) %>% 
  tally %>% 
  #drop_na() %>% 
  mutate(perc = n/sum(n))




