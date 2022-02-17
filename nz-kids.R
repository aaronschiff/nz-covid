# Covid and kids in NZ -- vaccination and cases 

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(scales)
library(ragg)
library(systemfonts)

start_date <- ymd("2022-01-18")
include_dhbs <- c("Auckland", 
                  "Waitematā", 
                  "Counties Manukau", 
                  "Waikato", 
                  "Northland")

# *****************************************************************************


# *****************************************************************************
# Utility functions ----

# DHB ordering factor 
factor_dhb <- function(x) {
  y <- factor(x = x, 
              levels = c("All DHB areas", 
                         "Northland", 
                         "Auckland", 
                         "Waitematā", 
                         "Counties Manukau", 
                         "Waikato", 
                         "Bay of Plenty", 
                         "Taranaki", 
                         "Lakes", 
                         "Tairāwhiti", 
                         "Whanganui", 
                         "MidCentral", 
                         "Hawke's Bay", 
                         "Capital & Coast", 
                         "Hutt Valley", 
                         "Wairarapa", 
                         "Nelson Marlborough", 
                         "West Coast", 
                         "Canterbury", 
                         "South Canterbury", 
                         "Southern"), 
              ordered = TRUE)
  return(y)
}

# Age group ordering factor
factor_age <- function(x) {
  y <- factor(x = x, 
              levels = c("0 to 9", 
                         "10 to 19", 
                         "20 to 29", 
                         "30 to 59", 
                         "60+"), 
              labels = c("0 to 9 years old", 
                         "10 to 19 years old", 
                         "20 to 29 years old", 
                         "30 to 59 years old", 
                         "60+ years old"), 
              ordered = TRUE)
  return(y)
}

# *****************************************************************************


# *****************************************************************************
# Font setup ----

register_font(
  name = "National 2 Custom", 
  plain = system_fonts() |> filter(family == "National 2", style == "Regular") |> pull(path), 
  bold = system_fonts() |> filter(family == "National 2", style == "Extrabold") |> pull(path), 
  italic = system_fonts() |> filter(family == "National 2", style == "Regular Italic") |> pull(path), 
  bolditalic = system_fonts() |> filter(family == "National 2", style == "Extrabold Italic") |> pull(path), 
  features = font_feature(ligatures = c("discretionary", 
                                        "standard", 
                                        "contextual"), 
                          numbers = c("lining", "proportional"))
)

# *****************************************************************************


# *****************************************************************************
# Load data ---- 

# Cases
# Data source: https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics
dat_cases <- read_csv(file = here("data/covid_cases_2022-02-17.csv"), 
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
  mutate(age_group_2 = factor_age(age_group_2)) |> 
  filter(dhb != "Managed Isolation & Quarantine") |> 
  mutate(dhb = case_when(
    dhb == "Waitemata" ~ "Waitematā", 
    dhb == "Tairawhiti" ~ "Tairāwhiti", 
    dhb == "Capital and Coast" ~ "Capital & Coast", 
    TRUE ~ dhb
  )) |> 
  mutate(dhb = factor_dhb(dhb)) |>  
  filter(report_date >= start_date) |> 
  arrange(report_date, dhb, age_group_2)

# Population
dat_pop <- read_csv(file = here("data/Population_Estimated_population_by_sex_age_group_and_DHB_at_June_19962021.csv"), 
                    col_types = "icccccnccc") |> 
  clean_names() |> 
  filter(year_as_at_30_june == 2021, 
         sex == "Total", 
         district_health_board != "New Zealand", 
         age_group %in% c("0-4", "5-9", 
                          "10-14", "15-19", 
                          "20-24", "25-29", 
                          "30-34", "35-39", 
                          "40-44", "45-49", 
                          "50-54", "55-59", 
                          "60-64", "65-69", 
                          "70-74", "75-79", 
                          "80-84", "85-89", 
                          "90-*")) |> 
  select(age_group, dhb = district_health_board, pop = value) |> 
  mutate(age_group_2 = case_when(
    age_group == "0-4" ~ "0 to 9", 
    age_group == "5-9" ~ "0 to 9", 
    age_group == "10-14" ~ "10 to 19", 
    age_group == "15-19" ~ "10 to 19", 
    age_group == "20-24" ~ "20 to 29", 
    age_group == "25-29" ~ "20 to 29", 
    age_group == "30-34" ~ "30 to 59", 
    age_group == "35-39" ~ "30 to 59", 
    age_group == "40-44" ~ "30 to 59", 
    age_group == "45-49" ~ "30 to 59", 
    age_group == "50-54" ~ "30 to 59", 
    age_group == "55-59" ~ "30 to 59", 
    age_group == "60-64" ~ "60+", 
    age_group == "65-69" ~ "60+", 
    age_group == "70-74" ~ "60+", 
    age_group == "75-79" ~ "60+", 
    age_group == "80-84" ~ "60+", 
    age_group == "85-89" ~ "60+", 
    age_group == "90-*" ~ "60+"
  )) |> 
  mutate(age_group_2 = factor_age(age_group_2)) |> 
  mutate(dhb = case_when(
    dhb == "Hutt" ~ "Hutt Valley", 
    TRUE ~ dhb
  )) |> 
  mutate(dhb = factor_dhb(dhb)) |> 
  group_by(dhb, age_group_2) |> 
  summarise(pop = sum(pop)) |> 
  ungroup() 

# *****************************************************************************


# *****************************************************************************
# Combine data ----

# Cases by age group, dhb, and date
dat_cases_by_age_dhb_date <- dat_cases |> 
  count(dhb, age_group_2, report_date) |> 
  complete(dhb, age_group_2, report_date, fill = list(n = 0L)) |> 
  filter(dhb %in% include_dhbs) |> 
  left_join(y = dat_pop, by = c("dhb", "age_group_2")) |> 
  mutate(rate = n / (pop / 100000)) |> 
  arrange(dhb, age_group_2, report_date) |> 
  group_by(dhb, age_group_2) |>
  mutate(t = row_number()) |> 
  ungroup() |> 
  nest_by(dhb, age_group_2) |> 
  mutate(gam_m = list(mgcv::gam(formula = rate ~ s(t, bs = "cs"), 
                                data = data))) |> 
  mutate(rate_smoothed = list(predict(gam_m))) |> 
  select(-gam_m) |> 
  unnest(cols = c(data, rate_smoothed)) |> 
  ungroup() 

# Cases by age group and date for all DHBs
dat_cases_by_age_date <- dat_cases_by_age_dhb_date |> 
  group_by(age_group_2, report_date) |> 
  summarise(n = sum(n), pop = sum(pop)) |> 
  mutate(rate = n / (pop / 100000)) |> 
  group_by(age_group_2) |> 
  mutate(t = row_number()) |> 
  ungroup() |> 
  nest_by(age_group_2) |> 
  mutate(gam_m = list(mgcv::gam(formula = rate ~ s(t, bs = "cs"), 
                                data = data))) |> 
  mutate(rate_smoothed = list(predict(gam_m))) |> 
  select(-gam_m) |> 
  unnest(cols = c(data, rate_smoothed)) |> 
  ungroup() |> 
  mutate(dhb = "All DHB areas") |> 
  mutate(dhb = factor_dhb(dhb))

# Combined
dat_combined <- bind_rows(
  dat_cases_by_age_dhb_date, 
  dat_cases_by_age_date
)

# *****************************************************************************


# *****************************************************************************
# Visualise cases ---- 

# Age group labels so they can be coloured
dat_age_labels <- tibble(
  x = median(dat_combined$report_date), 
  y = max(dat_combined$rate), 
  age_group_2 = c("0 to 9", 
                  "10 to 19", 
                  "20 to 29", 
                  "30 to 59", 
                  "60+"), 
  dhb = "All DHB areas"
) |> 
  mutate(age_group_2 = factor_age(age_group_2), 
         dhb = factor_dhb(dhb))

# Chart
chart_cases_by_age_dhb_day <- dat_combined |> 
  ggplot() + 
  geom_text(mapping = aes(x = x, y = y,
                          label = age_group_2, 
                          colour = age_group_2), 
            family = "National 2 Custom", 
            fontface = "bold", 
            size = 2.2, 
            data = dat_age_labels) + 
  geom_line(mapping = aes(x = report_date, 
                          y = rate_smoothed, 
                          group = line_group), 
            colour = grey(0.65), 
            size = 0.25, 
            data = dat_combined |> 
              mutate(line_group = paste(dhb, age_group_2)) |> 
              select(-age_group_2)) + 
  geom_point(mapping = aes(x = report_date, 
                           y = rate, 
                           fill = age_group_2), 
             shape = 21, 
             stroke = 0, 
             size = 0.7) + 
  geom_line(mapping = aes(x = report_date, 
                          y = rate_smoothed, 
                          colour = age_group_2), 
            size = 0.4) + 
  facet_grid(rows = vars(dhb), 
             cols = vars(age_group_2)) + 
  scale_x_date(breaks = seq(from = start_date, 
                            to = start_date + dweeks(52), 
                            by = "7 days"), 
               labels = date_format("%b %d"), 
               limits = c(start_date, max(dat_combined$report_date) + ddays(1)), 
               expand = expansion(0, 0)) + 
  scale_colour_manual(values = c("0 to 9 years old" = "#4053d3", 
                                 "10 to 19 years old" = "#b51d14", 
                                 "20 to 29 years old" = "#ddb310", 
                                 "30 to 59 years old" = "#00beff", 
                                 "60+ years old" = "#fb49b0"), 
                      aesthetics = c("colour", "fill"), 
                      guide = "none") + 
  labs(x = "", 
       y = "", 
       title = "Daily COVID-19 cases per 100,000 people", 
       caption = "Chart by Aaron Schiff using cases data from the NZ Ministry of Health and population data from Stats NZ. CC-BY 4.0. schiff.nz/covid/kids") + 
  theme_minimal(base_family = "National 2 Custom", 
                base_size = 7) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.15, colour = grey(0.9)), 
        panel.spacing.y = unit(12, "pt"), 
        panel.spacing.x = unit(24, "pt"), 
        strip.text.y = element_text(face = "bold"), 
        strip.text.x = element_blank(), 
        axis.text.x = element_text(size = rel(0.95)), 
        plot.caption = element_text(face = "italic"), 
        plot.title = element_text(size = rel(1.5), 
                                  margin = margin(0, 0, 12, 0, "pt")), 
        plot.margin = margin(4, 4, 4, 0, "pt"))

ggsave(filename = here("outputs/kids/cases_per_100k.png"), 
       plot = chart_cases_by_age_dhb_day, 
       width = 2400, 
       height = 1700, 
       units = "px", 
       device = agg_png, 
       bg = "white")

# *****************************************************************************
  