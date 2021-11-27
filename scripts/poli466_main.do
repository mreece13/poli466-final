encode bjp_voteshare, gen(bjp_voteshare_num)
drop bjp_voteshare
rename bjp_voteshare_num bjp_voteshare 

label variable state_name "State or UT"
label variable ac "Assembly Constituency"
label variable building_quality "Building Quality (1-3)"
label variable ps_small "Small"
label variable ps_dilapidated "Dilapidated"
label variable ps_ground_floor "Ground Floor"
label variable ps_separate_doors "Separate doors"
label variable ps_nearby_party_office "Nearby party office"
label variable ps_water "Water"
label variable ps_electricity "Electricity"
label variable ps_lighting "Lighting"
label variable ps_toilet "Toilet"
label variable ps_ramps "Ramps"
label variable ps_furniture "Furniture"
label variable ps_shelter "Shade and/or shelter"
label variable ps_road "Road"
label variable ps_obstacle "Natural obstacles"
label variable ps_landline "Landline or fax"
label variable ps_mobile "Mobile"
label variable ps_internet "Internet"
label variable ps_signage "Signage"
label variable ps_lwe_insurgency "LWE or Insurgency"
label variable ps_forest "Forest"
label variable ps_vulnerable_locations "Vulnerable"
label variable ps_sensitive "Sensitive"
label variable electors "Potential Voters"
label variable voters "Actual Voters"
label variable turnout "Voter Turnout"

gen tot_all= building_quality + ps_small + ps_dilapidated + ps_ground_floor + ps_separate_doors + ps_nearby_party_office + ps_water + ps_electricity + ps_lighting + ps_toilet + ps_ramps + ps_furniture + ps_shelter + ps_road + ps_obstacle + ps_landline + ps_mobile + ps_internet + ps_signage + ps_lwe_insurgency + ps_forest + ps_vulnerable_locations + ps_sensitive
gen tot_filtered = ps_small + ps_separate_doors + ps_nearby_party_office + ps_electricity + ps_lighting + ps_ramps + ps_furniture + ps_shelter + ps_landline + ps_mobile + ps_internet + ps_sensitive

label variable tot_all "Additive Index - All"
label variable tot_filtered "Additive Index - Filtered"

** factor analysis

// all
factor building_quality-ps_dilapidated ps_ground_floor-ps_sensitive, blanks(0.3)
rotate, oblimax blanks(0.3)
predict all_f1 all_f2 all_f3 all_f4, bartlett

label variable all_f1 "Factor 1"
label variable all_f2 "Factor 2"
label variable all_f3 "Factor 3"
label variable all_f4 "Factor 4"

// filtered
factor ps_small ps_separate_doors ps_nearby_party_office ps_electricity ps_lighting ps_ramps ps_furniture ps_shelter ps_landline ps_mobile ps_internet ps_sensitive, blanks(0.3)
rotate, oblimax blanks(0.3)
predict filtered_f1 filtered_f2 filtered_f3, bartlett

label variable filtered_f1 "Factor A"
label variable filtered_f2 "Factor B"
label variable filtered_f3 "Factor C"

** regression

xtset, clear
xtset state_name
eststo: xtreg turnout filtered_f1 filtered_f2 filtered_f3 bjp_voteshare electors
eststo: xtreg turnout all_f1 all_f2 all_f3 all_f4 bjp_voteshare electors
esttab est1 est2 using "results/factors.tex", booktabs alignment(D{.}{.}{-1}) se label compress b(3) se(3) nobaselevels replace 
est clear

eststo: xtreg turnout building_quality-ps_dilapidated ps_ground_floor-ps_sensitive bjp_voteshare electors
eststo: xtreg turnout ps_small ps_separate_doors ps_nearby_party_office ps_electricity ps_lighting ps_ramps ps_furniture ps_shelter ps_landline ps_mobile ps_internet ps_sensitive bjp_voteshare electors
esttab est1 est2 using "results/vars.tex", booktabs alignment(D{.}{.}{-1}) se label wide compress b(3) se(3) nobaselevels replace 
est clear

eststo: xtreg turnout tot_all bjp_voteshare electors
eststo: xtreg turnout tot_filtered bjp_voteshare electors
esttab est1 est2 using "results/additive.tex", booktabs alignment(D{.}{.}{-1}) se label compress b(3) se(3) nobaselevels replace 
esttab est1 est2 using "results/additive.csv", se label b(3) se(3) nobaselevels replace 
est clear

