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
library(ggh4x)      # devtools::install_github("teunbrand/ggh4x")


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

# World Bank population in 2020
dat_pop <- read_csv(file = here("data/API_SP.POP.TOTL_DS2_en_csv_v2_3011530/API_SP.POP.TOTL_DS2_en_csv_v2_3011530.csv"), 
                    skip = 3) |> 
  pivot_longer(cols = 5:last_col(), names_to = "year", values_to = "pop") |> 
  mutate(year = as.integer(year)) |> 
  filter(!is.na(year), !is.na(pop)) |> 
  filter(year == 2020) |> 
  clean_names() |> 
  select(-indicator_name, -indicator_code, -year)

# *****************************************************************************


# *****************************************************************************
# Assemble benchmarking data ----

# Select benchmarking countries (names may differ in different datasets)
dat_countries <- tribble(
  ~owd_country, ~wb_country, ~country_abbr, 
  "Australia", "Australia", "AU", 
  "Austria", "Austria", "AT", 
  "Belgium", "Belgium", "BE", 
  "Canada", "Canada", "CA", 
  "Denmark", "Denmark", "DK", 
  "Finland", "Finland", "FI", 
  "France", "France", "FR", 
  "Germany", "Germany", "DE", 
  "Ireland", "Ireland", "IE", 
  "Italy", "Italy", "IT", 
  "Japan", "Japan", "JP", 
  "Netherlands", "Netherlands", "NL", 
  "New Zealand", "New Zealand", "NZ", 
  "Norway", "Norway", "NO", 
  "Singapore", "Singapore", "SG", 
  "South Korea", "Korea, Rep.", "KR", 
  "Spain", "Spain", "ES", 
  "Sweden", "Sweden", "SE",
  "Switzerland", "Switzerland", "CH", 
  "United Kingdom", "United Kingdom", "UK", 
  "United States", "United States", "US"
)

# Benchmarking data for past 14 days
dat_bench_14days <- dat_countries |> 
  left_join(y = dat_owd |> 
              select(location, date, 
                     new_cases, new_deaths, new_tests_smoothed, 
                     people_fully_vaccinated, 
                     stringency_index), 
            by = c("owd_country" = "location")) |> 
  # Weekly summary for last 2 weeks of OWD data
  filter(date > max(dat_owd$date) - days(14)) |> 
  group_by(owd_country, wb_country, country_abbr) |> 
  summarise(total_cases = sum(new_cases, na.rm = TRUE), 
            total_tests = sum(new_tests_smoothed, na.rm = TRUE), 
            total_deaths = sum(new_deaths, na.rm = TRUE), 
            mean_vax = mean(people_fully_vaccinated, na.rm = TRUE), 
            mean_stringency = mean(stringency_index, na.rm = TRUE)) |> 
  left_join(y = dat_pop, by = c("wb_country" = "country_name")) |> 
  mutate(mean_daily_new_cases_per_5m = total_cases / (pop / 5000000) / 14, 
         mean_daily_new_tests_per_5m = total_tests / (pop / 5000000) / 14, 
         mean_daily_new_deaths_per_5m = total_deaths / (pop / 5000000) / 14, 
         mean_vax_rate = 100 * mean_vax / pop, 
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
                     stringency_index), 
            by = c("owd_country" = "location")) |> 
  group_by(owd_country, wb_country, country_abbr) |> 
  summarise(total_cases = sum(new_cases, na.rm = TRUE), 
            total_deaths = sum(new_deaths, na.rm = TRUE), 
            mean_stringency_overall = mean(stringency_index, na.rm = TRUE)) |> 
  left_join(y = dat_pop, by = c("wb_country" = "country_name")) |> 
  mutate(mean_total_cases_per_5m = total_cases / (pop / 5000000), 
         mean_total_deaths_per_5m = total_deaths / (pop / 5000000)) |> 
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

# Chart data
dat_chart <- bind_rows(
  # Past 14 days
  dat_bench_14days |> 
    select(owd_country, 
           country_group, 
           country_abbr, 
           mean_daily_new_cases_per_5m, 
           mean_daily_new_deaths_per_5m, 
           mean_daily_new_tests_per_5m, 
           mean_vax_rate, 
           mean_stringency) |> 
    pivot_longer(cols = c(-owd_country, -country_group, -country_abbr), 
                 names_to = "measure", values_to = "value"), 
  
  # Entire pandemic
  dat_bench_pandemic |> 
    select(owd_country, 
           country_group, 
           country_abbr, 
           mean_total_cases_per_5m, 
           mean_total_deaths_per_5m, 
           mean_stringency_overall) |> 
    pivot_longer(cols = c(-owd_country, -country_group, -country_abbr), 
                 names_to = "measure", values_to = "value")
) |> 
  mutate(measure = factor(x = measure, 
                          levels = c("mean_daily_new_cases_per_5m", 
                                     "mean_total_cases_per_5m", 
                                     "mean_daily_new_deaths_per_5m", 
                                     "mean_total_deaths_per_5m", 
                                     "mean_daily_new_tests_per_5m", 
                                     "mean_vax_rate", 
                                     "mean_stringency", 
                                     "mean_stringency_overall"), 
                          labels = c("Average daily cases per 5 million people in the past 14 days", 
                                     "Total cases per 5 million people for the entire pandemic", 
                                     "Average daily deaths per 5 million people in the past 14 days", 
                                     "Total deaths per 5 million people for the entire pandemic", 
                                     "Average daily tests per 5 million people in the past 14 days", 
                                     "Average fully vaccinated proportion of the total population over the past 14 days", 
                                     "Average government response stringency index in the past 14 days", 
                                     "Average government response stringency index for the entire pandemic"), 
                          ordered = TRUE))

chart <- dat_chart |> 
  ggplot(mapping = aes(y = 1, 
                       x = value, 
                       fill = country_group, 
                       colour = country_group, 
                       label = country_abbr)) + 
  geom_point(data = dat_chart |> filter(country_group == "other"), 
             shape = 21, 
             colour = "white", 
             size = 1.75, 
             stroke = 0.25) + 
  geom_point(data = dat_chart |> filter(country_group == "nz"), 
             shape = 21, 
             colour = "white", 
             size = 1.75, 
             stroke = 0.25) + 
  geom_text_repel(size = 1.75, 
                  segment.size = 0.2, 
                  min.segment.length = 0.25, 
                  direction = "y", 
                  ylim = c(0.95, 1.05), 
                  max.overlaps = 20) + 
  facet_wrap(facets = vars(measure), 
             scales = "free_x", 
             ncol = 1) + 
  scale_y_continuous(limits = c(0.95, 1.05), 
                     expand = expansion(0, 0)) + 
  facetted_pos_scales(x = list(
    scale_x_continuous(breaks = seq(0, 3500, 250), 
                       limits = c(0, 3300), 
                       labels = comma_format(accuracy = 1), 
                       expand = expansion(0, 0), 
                       position = "top"), 
    scale_x_continuous(breaks = seq(0, 700000, 100000), 
                       limits = c(0, 720000), 
                       labels = comma_format(accuracy = 1), 
                       expand = expansion(0, 0), 
                       position = "top"), 
    scale_x_continuous(breaks = seq(0, 26, 2), 
                       limits = c(-0.1, 23), 
                       expand = expansion(0, 0), 
                       position = "top"), 
    scale_x_continuous(breaks = seq(0, 12000, 1000), 
                       limits = c(-100, 12500), 
                       expand = expansion(0, 0), 
                       labels = comma_format(accuracy = 1), 
                       position = "top"), 
    scale_x_continuous(breaks = seq(0, 200000, 20000), 
                       limits = c(0, 145000), 
                       labels = comma_format(accuracy = 1), 
                       expand = expansion(0, 0), 
                       position = "top"), 
    scale_x_continuous(breaks = seq(50, 100, 5), 
                       limits = c(49, 101), 
                       expand = expansion(0, 0), 
                       position = "top"), 
    scale_x_continuous(breaks = seq(0, 100, 10), 
                       limits = c(0, 101), 
                       expand = expansion(0, 0), 
                       position = "top"), 
    scale_x_continuous(breaks = seq(0, 100, 10), 
                       limits = c(0, 101), 
                       expand = expansion(0, 0), 
                       position = "top")
  )) + 
  scale_fill_manual(values = c("nz" = "red", 
                               "other" = grey(0.1)), 
                    guide = "none", 
                    aesthetics = c("colour", "fill")) + 
  ggtitle(glue("COVID-19 benchmarks from selected countries for {last_date}")) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(size = 0.2, 
                                          colour = darken(col = "lightskyblue3", 
                                                          amount = 0.1)), 
        axis.text.y = element_blank(), 
        axis.title = element_blank(),
        plot.title = element_text(size = rel(1.2), face = "bold", 
                                  margin = margin(0, 0, 8, 0, "pt"), 
                                  colour = "royalblue"), 
        panel.background = element_rect(fill = lighten(col = "lightskyblue3", 
                                                       amount = 0.2), 
                                        size = 0), 
        strip.text = element_text(hjust = 0, 
                                  face = "bold", 
                                  margin = margin(0, 0, 2, -0.25, "pt")), 
        strip.placement = "outside", 
        panel.spacing = unit(20, "pt"))

ggsave(filename = here(glue("outputs/benchmarking-{last_date}.png")), 
       plot = chart, 
       width = 2000, 
       height = 2600, 
       units = "px", 
       device = "png", 
       bg = "white")

  # *****************************************************************************
