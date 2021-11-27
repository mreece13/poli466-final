rm(list=ls())

library(tidyverse)
library(polycor)
library(psych)

# how to make sense of factors. for every observation, can get a factor score for f1, f2... fn.
# correlate those scores between all of the individual variables: try to figure out what the factor means.
# understand what each is actually capturing. Extract factor scores, etc.
# 
# also test additive index
# 
# only run factor analysis with the variables I consider to be quality. 
# Then just take factor 1 and ignore the others. Little bit simpler to work with. 
# 
# correlation between additive index and 1-factor score.
# 
# I created 4 different versions of the outcome variable. Similar to the wasted vote paper.

df <- read_csv("data/pollingTurnout_merged.csv", guess_max = 100000)

pp_factor <- df %>% 
  select(-state_name, -ac, -electors:-bjp_voteShare, -ps_govBuilding:-ps_school)

pp_factor_important <- df %>% 
  select(ps_small, ps_dilapidated, ps_ground_floor, ps_water, 
         ps_electricity, ps_lighting, ps_toilet, ps_ramps, ps_shelter, 
         ps_road, ps_obstacle, ps_signage, ps_forest)

# All variables
nfactors(pp_factor)
pp_f3 <- fa(pp_factor, nfactors=1)

pp_f3_scores <- factor.scores(pp_factor, pp_f3)

# Just the important variables (according to me)
nfactors(pp_factor_important)
pp_important_f3 <- fa(pp_factor_important, nfactors=1)

pp_important_f3_scores <- factor.scores(pp_factor_important, pp_important_f3)

# extract the score from the first factor for both, add to df
# also create the additive index.
df_scored <- df %>% 
  mutate(pp_f3_factor_score = pp_f3_scores$scores[, 1]) %>% 
  mutate(pp_f3_important_factor_score = pp_important_f3_scores$scores[, 1]) %>% 
  mutate(pp_index = building_quality/3 - ps_small - ps_dilapidated + ps_ground_floor + 
           ps_separate_doors + ps_nearby_party_office + ps_water + ps_electricity + ps_lighting + 
           ps_toilet + ps_ramps + ps_furniture + ps_shelter + ps_road - ps_obstacle + ps_landline + 
           ps_mobile + ps_internet + ps_signage - ps_lwe_insurgency - ps_forest - 
           ps_vulnerable_locations - ps_sensitive) %>% 
  mutate(pp_index_important = -ps_small - ps_dilapidated + ps_ground_floor + 
           ps_water + ps_electricity + ps_lighting + 
           ps_toilet + ps_ramps + ps_shelter + ps_road - ps_obstacle - ps_forest + ps_signage)

cors <- df_scored %>% select(pp_f3_factor_score:pp_index_important)

#find the correlation between all the vars and just the important ones
round(cor(cors), 3)

df_scored %>% write_csv("data/pollingTurnout_scored.csv")
