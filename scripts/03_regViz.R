rm(list=ls())

library(tidyverse)

results_chars <- read_csv("results/vars.csv")

results_chars %>% 
  drop_na(coef_filtered) %>% 
  ggplot(aes(x = reorder(variable, coef_filtered), 
             y = coef_filtered, 
             ymin = coef_filtered-se_filtered, 
             ymax = coef_filtered+se_filtered)) +
  geom_hline(yintercept = 0) +
  geom_crossbar(fill = "#b6dce9ff", size = 0.5, fatten=1) +
  geom_point() + 
  coord_flip() +
  theme_bw() +
  labs(x = "", y = "Coefficient", title = "Filtered Variable Results") +
  theme(
    axis.text = element_text(size = 15),
    title = element_text(size = 20, vjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent_format())
