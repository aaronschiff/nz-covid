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
  mutate(date_count = as.integer(report_date - ymd("2020-01-01"))) |>
  arrange(report_date, type) |> 
  group_by(type, report_date) |> 
  mutate(x = row_number() + 0.5) |> 
  ungroup() |> 
  mutate(x = ifelse(type == "miq", -x, x)) 

# Dates sequence
dates <- tibble(
  date = seq(from = ymd("2020-01-01"), 
             to = ymd("2021-12-31"), 
             by = "1 day")
) |> 
  mutate(date_count = row_number() - 1)

# Date labels for chart
dates_labels <- dates |> 
  filter(day(date) == 1L) |> 
  mutate(month_label = month.abb[month(date)]) |> 
  mutate(label = ifelse(month(date) == 1L, 
                        paste(year(date), month_label), 
                        month_label))

# Alert levels data 
dat_al <- tribble(
  ~start_date, ~end_date, ~al, ~al_text, 
  ymd("2020-03-26"), ymd("2020-04-27"), 4L, "Alert Level 4", 
  ymd("2020-04-28"), ymd("2020-05-13"), 3L, "Alert Level 3", 
  ymd("2020-05-14"), ymd("2020-06-08"), 2L, "Alert Level 2", 
  ymd("2020-06-09"), ymd("2020-08-11"), 1L, "Alert Level 1", 
  ymd("2020-08-12"), ymd("2020-08-30"), 3L, "Alert Level 3 (Auckland) / 2 (elsewhere)", 
  ymd("2020-08-31"), ymd("2020-09-21"), 2L, "Alert Level 2.5 (Auckland) / 2 (elsewhere)", 
  ymd("2020-09-22"), ymd("2020-10-06"), 2L, "Alert Level 2.5 & 2 (Auckland) / 1 (elsewhere)", 
  ymd("2020-10-07"), ymd("2021-02-14"), 1L, "Alert Level 1", 
  ymd("2021-02-15"), ymd("")
)

# *****************************************************************************


# *****************************************************************************
# Chart ---- 

dat_cases |> 
  filter(year(report_date) < 2022) |> 
  ggplot(mapping = aes(y = date_count, 
                       x = x, 
                       fill = type)) + 
  geom_point(size = 0.5, 
             pch = 21, 
             colour = "white", 
             stroke = 0.1) + 
  geom_vline(xintercept = 0, size = 1) + 
  annotate(geom = "text", 
           x = -50, y = 15, 
           label = "Cases in MIQ", 
           colour = "firebrick", 
           fontface = "bold") +
  annotate(geom = "text", 
           x = 50, y = 15, 
           label = "Cases in the community", 
           colour = "orange", 
           fontface = "bold") +
  scale_colour_manual(values = c("miq" = "firebrick", 
                                 "community" = "orange"), 
                      guide = "none", 
                      aesthetics = c("colour", "fill")) + 
  scale_y_continuous(trans = "reverse", 
                     breaks = dates_labels$date_count, 
                     labels = dates_labels$label, 
                     limits = c(NA, 0), 
                     expand = expansion(0, 0)) + 
  scale_x_continuous(limits = c(-100, 250), 
                     breaks = seq(-100, 250, 50), 
                     labels = abs(seq(-100, 250, 50)), 
                     expand = expansion(0, 0)) + 
  xlab("Daily dases") + 
  ylab("") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())


# *****************************************************************************
