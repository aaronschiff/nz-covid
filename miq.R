# Looking at cases blocked by MIQ


# *****************************************************************************
# Setup ---- 

library(tidyverse)
library(here)
library(janitor)
library(scales)
library(ragg)
library(systemfonts)
library(lubridate)

# *****************************************************************************


# *****************************************************************************
# Custom font setup ----

register_font(
  name = "Fira Sans Custom", 
  plain = system_fonts() |> filter(family == "Fira Sans", style == "Regular") |> pull(path), 
  bold = system_fonts() |> filter(family == "Fira Sans", style == "ExtraBold") |> pull(path), 
  italic = system_fonts() |> filter(family == "Fira Sans", style == "Italic") |> pull(path), 
  bolditalic = system_fonts() |> filter(family == "Fira Sans", style == "ExtraBold Italic") |> pull(path), 
  features = font_feature(ligatures = c("discretionary", 
                                        "standard", 
                                        "contextual"), 
                          numbers = c("lining", "proportional"))
)

# *****************************************************************************


# *****************************************************************************
# Load data ----

# https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics
dat_cases <- read_csv(file = here("data/covid_cases_2022-02-05.csv")) |>   
  clean_names() |> 
  mutate(type = ifelse(dhb == "Managed Isolation & Quarantine", 
                       "miq", 
                       "community")) |> 
  mutate(date_count = as.integer(report_date - min(report_date))) |>
  arrange(report_date, type) |> 
  group_by(type, report_date) |> 
  mutate(x = row_number() + 1L) |> 
  ungroup() |> 
  mutate(x = ifelse(type == "miq", -x, x)) 
  # count(report_date, type) |> 
  # complete(report_date = seq(from = min(report_date), 
  #                            to = max(report_date), 
  #                            by = "1 day"), 
  #          type = c("miq", "community"), 
  #          fill = list(n = 0L)) 

dat_cases |> 
  ggplot(mapping = aes(y = date_count, 
                       x = x, 
                       colour = type)) + 
  geom_point(size = 0.5) + 
  geom_vline(xintercept = 0, size = 1) + 
  scale_colour_manual(values = c("miq" = "firebrick", 
                                 "community" = "cornflowerblue"), 
                      guide = "none") + 
  scale_y_continuous(trans = "reverse") + 
  theme_minimal()


