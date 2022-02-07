# International benchmarking of COVID-19 outcomes

# *****************************************************************************
# Setup ----

library(tidyverse)
library(here)
library(glue)
library(lubridate)
library(janitor)
library(scales)
library(httr)
library(ggrepel)
library(colorspace)
library(ggbeeswarm)
library(ggh4x)      # devtools::install_github("teunbrand/ggh4x")
library(ggtext)

# *****************************************************************************


# *****************************************************************************
# Load data ----

# Our World in Data coronavirus data explorer
dat_owd_raw <- httr::GET("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
content_owd <- content(x = dat_owd_raw, as = "raw")
writeBin(object = content_owd, 
         con = here("data/coronavirus-data-explorer.csv"))
rm(dat_owd_raw, content_owd)
dat_owd <- read_csv(file = here("data/coronavirus-data-explorer.csv"))

# *****************************************************************************


# *****************************************************************************
# Utility functions ----

# Find most recent non-NA value of a vector. Assumes vector is already sorted
# by date
most_recent_numeric <- function(x) {
  x_no_na <- as.numeric(na.omit(x))
  if (length(x_no_na) > 0) {
    y <- tail(x = x_no_na, n = 1)
  } else {
    y <- NA_real_
  }
  return(y)
}

# Custom maximum value for chart scales
scale_max_val <- function(d, m, u, round_digits = 0) { 
  y <- ceiling(
    d |> 
      filter(measure == m) |> 
      pull(value) |> 
      max(na.rm = TRUE) / u
  ) * u + u/4
  return(round(y, digits = round_digits))
}

# *****************************************************************************
# Assemble benchmarking data ----

# Select benchmarking countries (names may differ in different datasets)
dat_countries <- tribble(
  ~owd_country, ~country_abbr, ~country_area, 
  "Australia", "AU", "Australasia",
  "Austria", "AT", "Europe", 
  "Belgium", "BE", "Europe", 
  "Canada", "CA", "Americas", 
  "Denmark", "DK", "Europe", 
  "Finland", "FI", "Europe", 
  "France", "FR", "Europe", 
  "Germany", "DE", "Europe", 
  "Ireland", "IE", "Europe", 
  "Italy", "IT", "Europe", 
  "Japan", "JP", "Australasia", 
  "Netherlands", "NL", "Europe", 
  "New Zealand", "NZ", "New Zealand", 
  "Norway", "NO", "Europe", 
  "Portugal", "PT", "Europe", 
  "Singapore", "SG", "Australasia", 
  "South Korea", "KR", "Australasia", 
  "Spain", "ES", "Europe", 
  "Sweden", "SE", "Europe", 
  "Switzerland", "CH", "Europe", 
  "Taiwan", "TW", "Australasia", 
  "United Kingdom", "UK", "Europe", 
  "United States", "US", "Americas"
) |> 
  arrange(country_abbr)

# Assemble text for country key at bottom of chart
i <- 1L
j <- 1L
dat_countries_key <- ""
while (i <= nrow(dat_countries)) {
  dat_countries_key <- paste0(dat_countries_key, 
                              "**", 
                              dat_countries$country_abbr[i], 
                              "**: ", 
                              dat_countries$owd_country[i])
  if (j == 8L) {
    dat_countries_key <- paste0(dat_countries_key, "<br>")
    j <- 1L
  } else {
    if (i < nrow(dat_countries)) {
      dat_countries_key <- paste0(dat_countries_key, ", ")
    }
    j <- j + 1L
  }
  i <- i + 1L
}

# Benchmarking data for past 14 days
dat_bench_14days <- dat_countries |> 
  left_join(y = dat_owd |> 
              select(location, date, 
                     new_cases, new_deaths, new_tests_smoothed, 
                     reproduction_rate, 
                     people_fully_vaccinated, 
                     stringency_index, 
                     population), 
            by = c("owd_country" = "location")) |> 
  # Weekly summary for last 2 weeks of OWD data
  filter(date > max(dat_owd$date) - days(14)) |> 
  arrange(owd_country, country_abbr, country_area, date) |> 
  group_by(owd_country, country_abbr, country_area) |> 
  mutate(cases_valid = ifelse(is.na(new_cases), 0L, 1L),
         deaths_valid = ifelse(is.na(new_deaths), 0L, 1L), 
         tests_valid = ifelse(is.na(new_tests_smoothed), 0L, 1L), 
         reproduction_rate_valid = ifelse(is.na(reproduction_rate), 0L, 1L), 
         vax_valid = ifelse(is.na(people_fully_vaccinated), 0L, 1L),
         stringency_valid = ifelse(is.na(stringency_index), 0L, 1L)) |> 
  summarise(total_cases = sum(new_cases, na.rm = TRUE), 
            total_cases_n = sum(cases_valid), 
            mean_tests = mean(new_tests_smoothed, na.rm = TRUE), 
            total_tests_n = sum(tests_valid), 
            total_deaths = sum(new_deaths, na.rm = TRUE), 
            total_deaths_n = sum(deaths_valid), 
            latest_reproduction_rate = most_recent_numeric(reproduction_rate), 
            max_vax = most_recent_numeric(people_fully_vaccinated), 
            total_vax_n = sum(vax_valid), 
            mean_stringency = mean(stringency_index, na.rm = TRUE), 
            total_stringency_n = sum(stringency_valid), 
            population = mean(population, na.rm = TRUE)) |> 
  mutate(max_vax = ifelse(is.infinite(max_vax), NA_real_, max_vax)) |> 
  mutate(mean_daily_new_cases_per_5m = total_cases / (population / 5000000) / 14, 
         mean_daily_new_tests_per_5m = mean_tests / (population / 5000000), 
         mean_daily_new_deaths_per_5m = total_deaths / (population / 5000000) / 14, 
         max_vax_rate = 100 * max_vax / population, 
         mean_stringency = mean(mean_stringency)) |> 
  ungroup() |> 
  mutate(country_group = case_when(
    owd_country == "New Zealand" ~ "nz", 
    TRUE ~ "other"
  ))

# Benchmarking data for entire pandemic
dat_bench_pandemic <- dat_countries |> 
  left_join(y = dat_owd |> 
              select(location, date, 
                     new_cases, new_deaths,
                     stringency_index, 
                     population), 
            by = c("owd_country" = "location")) |> 
  group_by(owd_country, country_abbr, country_area) |> 
  summarise(total_cases = sum(new_cases, na.rm = TRUE), 
            total_deaths = sum(new_deaths, na.rm = TRUE), 
            mean_stringency_overall = mean(stringency_index, na.rm = TRUE), 
            population = mean(population, na.rm = TRUE)) |> 
  mutate(mean_total_cases_per_5m = total_cases / (population / 5000000), 
         mean_total_deaths_per_5m = total_deaths / (population / 5000000)) |> 
  ungroup() |> 
  mutate(country_group = case_when(
    owd_country == "New Zealand" ~ "nz", 
    TRUE ~ "other"
  ))

# *****************************************************************************


# *****************************************************************************
# Visualise results ----

# Date for chart title
last_date <- strftime(max(dat_owd$date), format = "%d %B %Y")

# Measure metadata
measures <- tribble(
  ~measure_level, ~measure_label, 
  "mean_daily_new_cases_per_5m", "Average daily cases per 5 million people (14-day average)",
  "mean_total_cases_per_5m", "Total cases per 5 million people for the entire pandemic",
  "mean_daily_new_deaths_per_5m", "Average daily deaths per 5 million people (14-day average)",
  "mean_total_deaths_per_5m", "Total deaths per 5 million people for the entire pandemic",
  "mean_daily_new_tests_per_5m", "Average daily tests per 5 million people (14-day average)",
  "latest_reproduction_rate", "Estimated effective reproduction rate (most recent data)", 
  "max_vax_rate", "Fully vaccinated proportion of the total population (most recent data)",
  "mean_stringency", "Average government response stringency index (14-day average)"
)

# Chart data
dat_chart <- bind_rows(
  # Past 14 days
  dat_bench_14days |> 
    select(owd_country, 
           country_group, 
           country_abbr, 
           country_area, 
           mean_daily_new_cases_per_5m, 
           mean_daily_new_deaths_per_5m, 
           mean_daily_new_tests_per_5m, 
           latest_reproduction_rate, 
           max_vax_rate, 
           mean_stringency) |> 
    pivot_longer(cols = c(-owd_country, -country_group, -country_abbr, -country_area), 
                 names_to = "measure", values_to = "value"), 
  
  # Entire pandemic
  dat_bench_pandemic |> 
    select(owd_country, 
           country_group, 
           country_abbr, 
           country_area, 
           mean_total_cases_per_5m, 
           mean_total_deaths_per_5m) |> 
    pivot_longer(cols = c(-owd_country, -country_group, -country_abbr, -country_area), 
                 names_to = "measure", values_to = "value")
) |> 
  mutate(measure = factor(x = measure, 
                          levels = measures$measure_level, 
                          labels = measures$measure_label, 
                          ordered = TRUE)) |> 
  mutate(country_area = factor(x = country_area, 
                               levels = c("New Zealand", 
                                          "Australasia", 
                                          "Europe", 
                                          "Americas"), 
                               ordered = TRUE)) |> 
  arrange(measure, fct_rev(country_area), owd_country)

# Max values for scales
max_mean_daily_new_cases_per_5m <- scale_max_val(d = dat_chart, 
                                                 m = measures$measure_label[1], 
                                                 u = 5000)

max_mean_total_cases_per_5m <- scale_max_val(d = dat_chart, 
                                             m = measures$measure_label[2], 
                                             u = 250000)

max_mean_daily_new_deaths_per_5m <- scale_max_val(d = dat_chart, 
                                                  m = measures$measure_label[3], 
                                                  u = 1) + 0.25

max_mean_total_deaths_per_5m <- scale_max_val(d = dat_chart,
                                              m = measures$measure_label[4], 
                                              u = 1000) + 200

max_mean_daily_new_tests_per_5m <- scale_max_val(d = dat_chart,
                                                 m = measures$measure_label[5],
                                                 u = 50000)

max_latest_reproduction_rate <- scale_max_val(d = dat_chart, 
                                              m = measures$measure_label[6], 
                                              u = 0.05, 
                                              round_digits = 1)

max_max_vax_rate <- 101

max_mean_stringency <- 101

# Chart code
chart <- dat_chart |> 
  ggplot(mapping = aes(x = "1", 
                       y = value, 
                       fill = country_area, 
                       label = country_abbr)) + 
  geom_hline(data = dat_chart |> filter(measure == measures$measure_label[6]), 
             mapping = aes(yintercept = 1.0), 
             size = 0.25, 
             colour = grey(0.5)) + 
  geom_quasirandom(shape = 21, 
                   stroke = 0.5, 
                   size = 4, 
                   colour = "white", 
                   bandwidth = 10, 
                   method = "quasirandom") + 
  geom_text(position = position_quasirandom(bandwidth = 10, 
                                            method = "quasirandom"), 
            colour = "white", 
            fontface = "bold", 
            size = 1.6) + 
  coord_flip() + 
  facet_wrap(facets = vars(measure), 
             scales = "free", 
             ncol = 1) + 
  facetted_pos_scales(y = list(
    scale_y_continuous(breaks = seq(0, max_mean_daily_new_cases_per_5m, 5000),
                       limits = c(-500, max_mean_daily_new_cases_per_5m),
                       labels = comma_format(accuracy = 1),
                       expand = expansion(0, 0),
                       position = "right"),
    scale_y_continuous(breaks = seq(0, max_mean_total_cases_per_5m, 250000),
                       limits = c(-20000, max_mean_total_cases_per_5m),
                       labels = comma_format(accuracy = 1),
                       expand = expansion(0, 0),
                       position = "right"), 
    scale_y_continuous(breaks = seq(0, max_mean_daily_new_deaths_per_5m, 1),
                       limits = c(-0.5, max_mean_daily_new_deaths_per_5m),
                       expand = expansion(0, 0),
                       position = "right"),
    scale_y_continuous(breaks = seq(0, max_mean_total_deaths_per_5m, 1000),
                       limits = c(-100, max_mean_total_deaths_per_5m),
                       expand = expansion(0, 0),
                       labels = comma_format(accuracy = 1),
                       position = "right"),
    scale_y_continuous(breaks = seq(0, max_mean_daily_new_tests_per_5m, 50000), 
                       limits = c(-2000, max_mean_daily_new_tests_per_5m), 
                       labels = comma_format(accuracy = 1), 
                       expand = expansion(0, 0), 
                       position = "right"), 
    scale_y_continuous(breaks = seq(0, max_latest_reproduction_rate, 0.1),
                       limits = c(0, max_latest_reproduction_rate + 0.05),
                       labels = comma_format(accuracy = 0.1),
                       expand = expansion(0, 0),
                       position = "right"),
    scale_y_continuous(breaks = seq(0, max_max_vax_rate, 5),
                       limits = c(0, max_max_vax_rate),
                       expand = expansion(0, 0),
                       position = "right"),
    scale_y_continuous(breaks = seq(0, max_mean_stringency, 10),
                       limits = c(0, max_mean_stringency),
                       expand = expansion(0, 0),
                       position = "right")
  )) +
  scale_fill_manual(values = c("New Zealand" = "#6929c4", 
                               "Australasia" = lighten(col = "#33b1ff", amount = 0.2), 
                               "Europe" = lighten(col = "#007d79", amount = 0.3), 
                               "Americas" = lighten(col = "#ff7eb6", amount = 0.2)),
                    name = NULL, 
                    aesthetics = c("colour", "fill")) +
  labs(title = glue("COVID-19 benchmarks from selected countries for {last_date}"), 
       subtitle = "Chart by Aaron Schiff with data from Our World in Data (CC-BY 4.0 - schiff.nz/covid/nz-benchmarking)", 
       caption = dat_countries_key) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(size = 0.2, grey(0.8)), 
        panel.background = element_rect(fill = "seashell1", 
                                        size = 0), 
        axis.text.y = element_blank(), 
        axis.text.x = element_text(colour = grey(0.5)), 
        axis.title = element_blank(),
        plot.title = element_text(size = rel(1.2), face = "bold", 
                                  margin = margin(0, 0, 4, 0, "pt")), 
        plot.subtitle = element_text(size = rel(0.8), 
                                     margin = margin(0, 0, 0, 0, "pt")), 
        plot.caption = element_markdown(hjust = 0, 
                                        lineheight = 1.2,
                                        margin = margin(12, 0, 0, 0, "pt")), 
        plot.margin = margin(4, 4, 4, 4, "pt"), 
        legend.margin = margin(8, 0, 0, 0, "pt"), 
        strip.text = element_text(hjust = 0, 
                                  face = "bold", 
                                  margin = margin(0, 0, 2, -0.25, "pt")), 
        strip.placement = "outside", 
        panel.spacing = unit(12, "pt"), 
        legend.position = "top", 
        legend.direction = "horizontal")

ggsave(filename = here(glue("outputs/benchmarking/benchmarking-{last_date}.png")), 
       plot = chart, 
       width = 2000, 
       height = 3200, 
       units = "px", 
       device = "png", 
       bg = "white")

# *****************************************************************************
