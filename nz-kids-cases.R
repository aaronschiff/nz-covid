# Number of child cases in NZ

library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(scales)
library(RcppRoll)

dat <- read_csv(file = here("data/covid_cases_2021-11-25.csv")) |> 
  clean_names()

dat_nz_delta_l3_kids <- dat |> 
  mutate(age_group_2 = case_when(
    age_group == "0 to 9" ~ "0 to 9", 
    age_group == "10 to 19" ~ "10 to 19", 
    TRUE ~ "Adults"
  )) |> 
  filter(report_date >= ymd("2021-09-22")) |> 
  filter(dhb != "Managed Isolation & Quarantine", 
         is.na(historical)) |> 
  arrange(report_date) |> 
  mutate(outbreak_day = as.integer(report_date - ymd("2021-09-22"))) |> 
  filter(outbreak_day != max(outbreak_day)) |> 
  count(age_group_2, outbreak_day) |> 
  complete(outbreak_day = 0:max(outbreak_day), 
           age_group_2 = c("0 to 9", "10 to 19", "Adults"), 
           fill = list(n = 0L)) |> 
  group_by(age_group_2) |> 
  mutate(rolling_mean = roll_mean(x = n, 
                                  n = 7, 
                                  align = "right", 
                                  fill = NA_real_)) |> 
  ungroup() |> 
  mutate(age_group_2 = factor(x = age_group_2, 
                              levels = c("0 to 9", "10 to 19", "Adults"), 
                              labels = c("Children aged 0 to 9", 
                                         "Children & young adults aged 10 to 19", 
                                         "Adults aged 20+"), 
                              ordered = TRUE)) 

chart_dat_nz_delta_l3_kids <- dat_nz_delta_l3_kids |> 
  ggplot(mapping = aes(x = outbreak_day, 
                       y = rolling_mean, 
                       colour = fct_rev(age_group_2))) + 
  geom_line(size = 0.75) + 
  xlab("Outbreak day since Auckland AL3") + 
  ylab("Average daily\nnumber of\ncases reported") + 
  ggtitle(label = "Delta outbreak 7-day average daily cases") + 
  scale_x_continuous(breaks = seq(0, 200, 10)) + 
  scale_y_continuous(breaks = seq(0, 160, 10),
                     labels = comma_format(accuracy = 1)) +
  scale_colour_manual(values = c("Children aged 0 to 9" = "firebrick", 
                                 "Children & young adults aged 10 to 19" = "orange", 
                                 "Adults aged 20+" = "cornflowerblue"), 
                      name = NULL) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.25, 0.92))

ggsave(filename = here("outputs/nz-vs-au/nz_kids_cases_outbreak_day.png"), 
       plot = chart_dat_nz_delta_l3_kids, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")
