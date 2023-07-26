# purpose: process emergence data from airtable
# created: 7/26/2023
# notes: would like to figure out how to interact/download directly from airtable

library(tidyverse)
library(janitor)
library(tidytext)

# data --------------------------------------------------------------------

draw <- read_csv("data_byhandAT/2023-07-26_Plot_EmergenceData-Grid view.csv")

d <- 
  draw %>% 
  clean_names() %>% 
  mutate(sub_plot = ifelse(is.na(sub_plot), 1, sub_plot)) %>% 
  filter(!is.na(plot))


# calculations ------------------------------------------------------------

#--convert sampled area to m2
d1 <- 
  d %>% 
  mutate(sample_width_in = quadrat_width_rows_from_fields_from_plots * row_spacing_in_from_plot,
         sample_length_in = quadrat_length_m_from_fields_from_plots * 39.37,
         sample_area_in2 = sample_width_in * sample_length_in,
         sample_area_m2 = sample_area_in2 * 0.00064516) %>% 
  select(plot, fields_from_plot, crop_from_treatment_from_plot, treatment_from_plot, msmt_date_mmddyyyy, plot_from_plot, sub_plot, plants, sample_area_m2)

#--plants per m2
d2 <- 
  d1 %>% 
  mutate(pl_m2 = plants/sample_area_m2)


# viz ---------------------------------------------------------------------

d2 %>% 
  ggplot(aes(treatment_from_plot, pl_m2)) + 
  geom_jitter() + 
  coord_flip() +
  facet_grid(fields_from_plot~.)

d2 %>% 
  separate(col = fields_from_plot, into = c("site", "field"), sep = "_") %>% 
  ggplot(aes(treatment_from_plot, pl_m2)) + 
  geom_jitter(aes(color = site), width = 0.1) +
  facet_grid(crop_from_treatment_from_plot~., scales = "free_y") + 
  coord_flip()

d2 %>% 
  separate(col = fields_from_plot, into = c("site", "field"), sep = "_") %>% 
  group_by(site, treatment_from_plot) %>% 
  summarise(pl_m2 = mean(pl_m2)) %>% 
  ggplot(aes(treatment_from_plot, pl_m2)) + 
  geom_point(aes(color = site), size= 4) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  coord_flip()

d2 %>% 
  separate(col = fields_from_plot, into = c("site", "field"), sep = "_") %>% 
  ggplot(aes(treatment_from_plot, pl_m2)) + 
  stat_summary(aes(color = site), size= 1) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  coord_flip() + 
  labs(title = "Emergence data up until 7/26/2023",
       x = NULL,
       y = "Plants per m2")

ggsave("figs/emergence.png")
