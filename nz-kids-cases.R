# Covid and kids in NZ -- vaccination and cases since start of 2022

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(scales)
library(RcppRoll)

# *****************************************************************************


# *****************************************************************************
# Load data ---- 

# Data source: https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics
dat <- read_csv(file = here("data/covid_cases_2022-02-17.csv"), 
                col_types = "Dcccccc") |> 
  clean_names() |> 
  mutate(age_group_2 = case_when(
    age_group == "0 to 9" ~ "0 to 9", 
    age_group == "10 to 19" ~ "10 to 19", 
    age_group == "20 to 29" ~ "20 to 29", 
    age_group == "30 to 39" ~ "30 to 59", 
    age_group == "40 to 49" ~ "30 to 59", 
    age_group == "50 to 59" ~ "30 to 59", 
    age_group == "60 to 69" ~ "60+", 
    age_group == "70 to 79" ~ "60+", 
    age_group == "80 to 89" ~ "60+", 
    age_group == "90+" ~ "60+"
  )) |> 
  filter(dhb != "Managed Isolation & Quarantine") |> 
  filter(report_date >= ymd("2022-01-01"))

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
  count(age_group_2, report_date, dhb) |> 
  complete(report_date = seq(from = ymd("2021-09-22"), 
                             to = max(dat$report_date), 
                             by = "1 day"), 
           age_group_2 = c("0 to 9", "10 to 19", "Adults"), 
           dhb, 
           fill = list(n = 0L)) |> 
  arrange(dhb, age_group_2, report_date) |> 
  group_by(age_group_2, dhb) |> 
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
                              ordered = TRUE)) |> 
  mutate(outbreak_day = as.integer(report_date - ymd("2021-09-22"))) |>
  group_by(age_group_2, dhb) %>%
  slice_min(order_by = report_date, n = max(.$outbreak_day) - 1) |> 
  ungroup()

chart_nz_delta_l3_kids <- dat_nz_delta_l3_kids |> 
  filter(dhb %in% c("Auckland", "Waitemata", "Counties Manukau")) |> 
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
  facet_wrap(facets = vars(dhb), nrow = 1) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        panel.spacing.x = unit(16, "pt"), 
        strip.text = element_text(face = "bold"), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = "top", 
        legend.margin = margin(0, 0, 0, 0))

ggsave(filename = here("outputs/kids/nz_kids_cases_outbreak_day_dhb.png"), 
       plot = chart_nz_delta_l3_kids, 
       width = 3000, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

dat_nz_delta_l3_kids_total <- dat_nz_delta_l3_kids |> 
  group_by(report_date, outbreak_day, age_group_2) |> 
  summarise(n = sum(n)) |> 
  arrange(age_group_2, report_date) |> 
  group_by(age_group_2) |> 
  mutate(rolling_mean = roll_mean(x = n, 
                                  n = 7, 
                                  align = "right", 
                                  fill = NA_real_)) |> 
  ungroup()

chart_nz_delta_l3_kids_total <- dat_nz_delta_l3_kids_total |> 
  ggplot(mapping = aes(x = report_date, 
                       y = rolling_mean, 
                       colour = fct_rev(age_group_2))) + 
  geom_line(size = 0.75) + 
  ylab("Average daily\nnumber of\ncases reported") + 
  xlab("") + 
  ggtitle(label = "7-day average daily cases") + 
  scale_x_date(labels = date_format(format = "%b %Y")) + 
  #scale_y_continuous(breaks = seq(0, 160, 10),
#                     labels = comma_format(accuracy = 1)) +
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

ggsave(filename = here("outputs/kids/nz_kids_cases_outbreak_day_total.png"), 
       plot = chart_nz_delta_l3_kids_total, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

chart_nz_delta_l3_kids_total_scatter <- 
  dat_nz_delta_l3_kids_total |> 
  filter(age_group_2 != "Children & young adults aged 10 to 19") |> 
  select(outbreak_day, age_group_2, rolling_mean) |> 
  pivot_wider(names_from = age_group_2, values_from = rolling_mean) |> 
  clean_names() |> 
  ggplot(mapping = aes(x = adults_aged_20, y = children_aged_0_to_9)) + 
  geom_point(colour = "firebrick") + 
  geom_segment(mapping = aes(
    xend = c(tail(adults_aged_20, n = -1), NA), 
    yend = c(tail(children_aged_0_to_9, n = -1), NA)
  ), 
  colour = "firebrick") + 
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 20)) + 
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) + 
  xlab("Daily cases in adults aged 20+ (7-day average)") + 
  ylab("Daily cases\nin children aged 0-9\n(7-day average)") + 
  ggtitle(label = "Delta outbreak 7-day average daily cases since Auckland AL3") + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")))

ggsave(filename = here("outputs/kids/nz_kids_cases_outbreak_day_total_scatter.png"), 
       plot = chart_nz_delta_l3_kids_total_scatter, 
       width = 2400, 
       height = 2200, 
       units = "px", 
       device = "png", 
       bg = "white")

chart_nz_delta_l3_kids_per_adult <- dat_nz_delta_l3_kids_total |> 
  filter(age_group_2 != "Children & young adults aged 10 to 19") |> 
  select(-n) |> 
  pivot_wider(names_from = age_group_2, values_from = rolling_mean) |> 
  clean_names() |> 
  mutate(child_adult_ratio = children_aged_0_to_9 / adults_aged_20) |> 
  ggplot(mapping = aes(x = report_date, 
                       y = child_adult_ratio)) + 
  geom_line(size = 0.75, colour = "firebrick4") + 
  xlab("") + 
  ylab("Average daily\nnumber of\ncases reported") + 
  ggtitle(label = "Ratio of 7-day average daily cases for children aged 0-9 vs adults aged 20+") + 
  scale_x_date(labels = date_format("%b %Y")) + 
  scale_y_continuous(limits = c(0, 0.6), 
                     breaks = seq(0, 1, 0.1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.25, 0.92))

ggsave(filename = here("outputs/kids/nz_kids_per_adult.png"), 
       plot = chart_nz_delta_l3_kids_per_adult, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")
