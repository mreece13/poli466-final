rm(list=ls())

library(tidyverse)
library(plm)
library(lmtest)
library(texreg)
library(caret)

df <- read_csv("data/pollingTurnout_scored.csv")

nz <- nearZeroVar(df, saveMetrics = TRUE)

df$turnout <- df$turnout*100

# measure 1
plm_f3_all <- plm(turnout ~ pp_f3_factor_score + electors + bjp_voteShare, index = "state_name", data = df)
cf_1 <- coeftest(plm_f3_all, vcov. = vcovHC)
se_1 <- cf_1[, 2]
pval_1 <- cf_1[, 4]

# measure 2
plm_f3_important <- plm(turnout ~ pp_f3_important_factor_score + electors + bjp_voteShare, index = "state_name", data = df)
cf_2 <- coeftest(plm_f3_important, vcov. = vcovHC)
se_2 <- cf_2[, 2]
pval_2 <- cf_2[, 4]

# measure 3
plm_index_all <- plm(turnout ~ pp_index + electors + bjp_voteShare, index = "state_name", data = df)
cf_3 <- coeftest(plm_index_all, vcov. = vcovHC)
se_3 <- cf_3[, 2]
pval_3 <- cf_3[, 4]

# measure 4
plm_index_important <- plm(turnout ~ pp_index_important + electors + bjp_voteShare, index = "state_name", data = df)
cf_4 <- coeftest(plm_index_important, vcov. = vcovHC)
se_4 <- cf_4[, 2]*100
pval_4 <- cf_4[, 4]

texreg(list(plm_f3_all, plm_f3_important, plm_index_all, plm_index_important), 
       dcolumn = TRUE, booktabs = TRUE, use.packages = FALSE, single.row = TRUE,
       label = "tab:results_main", caption = "Effects of Polling Place Quality", float.pos = "H", 
       override.se = list(se_1, se_2, se_3, se_4), override.pvalues = list(pval_1, pval_2, pval_3, pval_4), 
       custom.coef.names = c("Quality Measure 1", "# Electors",
                             "% BJP Vote Share", "Quality Measure 2", "Quality Measure 3", "Quality Measure 4"),
       reorder.coef = c(1, 4, 5, 6, 2, 3))

# model 1
plm_all <- plm(turnout ~ ps_small + ps_dilapidated + ps_ground_floor + ps_separate_doors + 
                 ps_nearby_party_office + ps_water + ps_electricity + ps_lighting + ps_toilet + 
                 ps_ramps + ps_furniture + ps_shelter + ps_road + ps_obstacle + ps_landline +
                 ps_mobile + ps_internet + ps_signage + ps_lwe_insurgency + ps_forest + ps_vulnerable_locations + 
                 ps_sensitive + building_quality,
               index = "state_name", data = df)
cf_all <- coeftest(plm_all, vcov. = vcovHC)
se_all <- cf_all[, 2]
pval_all <- cf_all[, 4]

# model 2
plm_important <- plm(turnout ~ ps_small + ps_dilapidated + ps_ground_floor + ps_water + ps_electricity + ps_lighting + 
                       ps_toilet + ps_ramps + ps_shelter + ps_road + ps_obstacle + ps_signage + ps_forest, 
                     index = "state_name", data = df)
cf_important <- coeftest(plm_important, vcov. = vcovHC)
se_important <- cf_important[, 2]
pval_important <- cf_important[, 4]

texreg(plm_all, dcolumn = TRUE, booktabs = TRUE, use.packages = FALSE, 
       label = "tab:model5_results", caption = "Model 5 Results", float.pos = "H", 
       override.se = se_all, override.pvalues = pval_all, single.row = TRUE,
       custom.coef.names = c("Small", "Dilapidated",
                             "Ground Floor", "Separate Doors", "Nearby Party Office", "Water", "Electricity",
                             "Lighting", "Toilet", "Ramps", "Furniture", "Shelter", "Connecting Road", 
                             "Natural Obstacles", "Landline or Fax", "Mobile Service", "Internet", "Signage",
                             "LWE/Insurgency Affected", "Forest", "Vulnerable Location", "Sensitive", "Building Quality"))


