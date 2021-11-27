rm(list=ls())

library(tidyverse)
library(caret)

pp <- read_csv("/Users/mason/Documents/Github/poll-station-metadata/poll_station_metadata_all.csv", guess_max = 100000)

#turnout <- read_csv("data/pcwise_voterturnout.csv")
turnout <- read_csv("data/detailed_voterTurnout.csv")

bjp_votes <- turnout %>% 
  mutate(across(c(state_name, pc_name, ac_name), str_trim)) %>% 
  mutate(ac_name = str_replace(ac_name, "\\([^()]*\\)", "")) %>% 
  mutate(across(c(state_name, pc_name, ac_name), str_to_title)) %>% 
  mutate(across(c(state_name, pc_name, ac_name), str_trim)) %>%
  select(state_name, ac_name, party, candidate_votes) %>% 
  filter(party == "BJP") %>% 
  rename(bjp_votes = candidate_votes) %>%
  select(-party)

turnout_cleaned <- turnout %>% 
  mutate(across(c(state_name, pc_name, ac_name), str_trim)) %>% 
  mutate(ac_name = str_replace(ac_name, "\\([^()]*\\)", "")) %>% 
  mutate(across(c(state_name, pc_name, ac_name), str_to_title)) %>% 
  mutate(across(c(state_name, pc_name, ac_name), str_trim)) %>%
  group_by(state_name, ac_name) %>% 
  summarise(electors = mean(electors),
            voters = sum(candidate_votes),
            turnout = voters/electors
            ) %>% 
  ungroup() %>% 
  left_join(bjp_votes, by = c("state_name", "ac_name")) %>% 
  mutate(bjp_voteShare = bjp_votes/voters) %>% 
  select(-bjp_votes)

yes_no_conversion <- function(row){
  case_when(
    row == "YES" ~ 1,
    row == "NO" ~ 0,
    TRUE ~ NA_real_
  )
}

typos <- c("Bordumsa - Diyun" = "Bordumsa - Diyum",
           "Mechuka" = "Mechukha",
           "Chitrakot" = "Chitrakoat",
           "Narayanpur" = "Narainpur",
           "Dantewara" = "Dantewada",
           "Chitrakot" = "Chitrakoat",
           "Krishnarajpet" = "Krishnarajapete",
           "Chalakkudy" = "Chalakudy",
           "Manapur" = "Manpur",
           "Dhamamgaon Railway" = "Dhamangaon Railway",
           "Brahmapuri" = "Bramhapuri",
           "Sindkheda" = "Sindkhed Raja",
           "Arjuni-Morgaon" = "Arjuni Morgaon",
           "Gondiya" = "Gondia",
           "Bishnupur" = "Bishenpur",
           "Chokpot" = "Chockpot",
           "Aizawl North - I" = "Aizawl North I",
           "Aizawl  North - I" = "Aizawl North I",
           "Aizawl East - I" = "Aizawl East I",
           "Aizawl North - Ii" = "Aizawl North Ii",
           "Aizawl North-Iii" = "Aizawl North Iii",
           "Aizawl South-Iii" = "Aizawl South Iii",
           "Siaha" = "Saiha",
           "Laxmipur" = "Lakshmipur",
           "Umarkote" = "Umerkote",
           "Bhagha Purana" = "Baghapurana",
           "Nihal Singhwala" = "Nihal Singh Wala",
           "Danta Ramgarh" = "Dantaramgarh",
           "Maneybung-Dentam" = "Maneybong-Dentam",
           "Soreng-Chakung" = "Soreong-Chakung",
           "Yoksam-Tashiding" = "Yuksom Tashiding",
           "Ambur" = NA_character_, #All NAs here were part of the Vellore election, which was postponed to August. We have data on the number of voters who voted, but not on the electors.
           "Anaikattu" = NA_character_,
           "Gudiyattam" = NA_character_,
           "Kilvaithinankuppam" = NA_character_,
           "Vaniyambadi" = NA_character_,
           "Vellore" = NA_character_,
           "Wardhanapet" = "Waradhanapet",
           "Kakraban-Salgarh" = "Kakraban-Shalgara",
           "Radhakishorepur" = "Radhakishorpur",
           "Mandaibazar" = "Mandai Bazar",
           "Town Bordowali" = "Town Bardowali",
           "Karnprayag" = "Karanprayag",
           "Dehradun Cantt." = "Dehradun Cantonment",
           "B.h.e.l. Ranipur" = "Bhel Ranipur",
           "Hardwar" = "Haridwar",
           "Hardwar Rural" = "Haridwar Rural",
           "Pirankaliyar" = "Piran Kaliyar",
           "Lalkuwa" = "Lalkuan",
           "Ghanshali" = "Ghansali",
           "Nanak Matta" = "Nanakmatta",
           "Cooch Behar Uttar" = "Cooch Behar Dakshin",
           "Chandannagore" = "Chandannagar"
)

pp_cleaned <- pp %>% 
  select(-blo:-supplementary) %>% 
  rename(state_name = state_or_ut) %>% 
  rename(building_quality = `Building Quality`) %>% 
  rename(ps_small = `PS with less than 20 sqmts`) %>%
  rename(ps_dilapidated = `PS buildings is dilapidated or dangerous`) %>%
  rename(ps_govBuilding = `PS is in Govt building/Premises`) %>%
  rename(ps_institution_religion = `PS located in an institution/religious place`) %>%
  rename(ps_school = `PS in School/College building`) %>%
  rename(ps_ground_floor = `PS in ground floor`) %>%
  rename(ps_separate_doors = `PS having Separate door for Entry and Exit`) %>%
  rename(ps_nearby_party_office = `political party office situated within 200 meters of PS premises`) %>%
  rename(ps_water = `PS is having drinking water facilities in the premises`) %>%
  rename(ps_electricity = `PS buildings having Electricity Supply`) %>%
  rename(ps_lighting = `PS buildings with Proper lighting, Fixtures etc.`) %>%
  rename(ps_toilet = `PS buildings with Toilet(Male/Female)`) %>%
  rename(ps_ramps = `PS with ramps For Disable`) %>%
  rename(ps_furniture = `PS buildings with Adequate Furniture`) %>%
  rename(ps_shelter = `PS with shade/shelter for protection from sun/rain etc.`) %>%
  rename(ps_road = `PS with Proper road connectivity`) %>%
  rename(ps_obstacle = `PS where voters have to cross river/valley/ravine or natural obstacle to reach PS`) %>% 
  rename(ps_landline = `PS with Landline Telephone/Fax Connection`) %>%
  rename(ps_mobile = `PS with Mobile connectivity`) %>%
  rename(ps_internet = `PS with Internet facility`) %>%
  rename(ps_signage = `PS with Proper signage of Building name and address`) %>%
  rename(ps_lwe_insurgency = `PS with in LWE/insurgency affected area`) %>%
  rename(ps_forest = `PS With in forest/semi-forest area`) %>%
  rename(ps_vulnerable_locations = `PS in vulnerable critical location`) %>%
  rename(ps_sensitive = `sensitive/hyper-sensitive PS`) %>% 
  mutate(ac = str_replace(ac, "\\([^()]*\\)", "")) %>% 
  mutate(across(building_quality:ps_sensitive, str_to_upper)) %>% 
  mutate(across(state_name:ac, str_to_title)) %>% 
  mutate(across(c(state_name, district, ac), str_trim)) %>% 
  mutate(building_quality = case_when(
    building_quality == "PUCCA" ~ 3,
    building_quality == "SEMI-PUCCA" ~ 2,
    building_quality == "KUTCHA" ~ 1,
    building_quality == "KUCHHA" ~ 1,
    TRUE ~ NA_real_
  )) %>% 
  mutate(across(ps_small:ps_sensitive, yes_no_conversion)) %>% 
  drop_na(building_quality:ps_sensitive) %>% 
  mutate(ac = recode(ac, !!!typos)) %>% 
  mutate(ac = case_when(
    district == "Daman" & ac == "Daman & Diu" ~ "Daman",
    district == "Diu" & ac == "Daman & Diu" ~ "Diu",
    state_name == "West Bengal" & ac == "Bishenpur" ~ "Bishnupur",
    state_name == "Uttarakhand" & ac == "Dharampur" ~ "Dharmpur",
    state_name == "Gujarat" & ac == "Dharmpur" ~ "Dharampur",
    state_name == "Himachal Pradesh" & ac == "Dharmpur" ~ "Dharampur",
    TRUE ~ ac
  )) %>% 
  drop_na(ac)

nzv <- nearZeroVar(pp_cleaned, saveMetrics = TRUE)
pp_cleaned_filtered <- pp_cleaned[, -nzv]

pp_cleaned %>% 
  write_csv("../poli466-final/data/pollingStations_cleaned.csv")

pp_grouped <- pp_cleaned %>% 
  group_by(state_name, ac) %>% 
  summarise(across(building_quality:ps_sensitive, ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()

merged <- pp_grouped %>% 
  left_join(turnout_cleaned, by = c("state_name", "ac" = "ac_name"))

merged %>% 
  write_csv("data/pollingTurnout_merged.csv")
