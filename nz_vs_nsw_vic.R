# Compare NZ, NSW, and VIC delta outbreaks

# *****************************************************************************
# Setup ---- 

library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(scales)
library(readxl)

# *****************************************************************************


# *****************************************************************************
# Load data ---- 

# Case data files
dat_nsw <- read_csv(file = here("data/confirmed_cases_table1_location.csv"))   # https://data.nsw.gov.au/search/dataset/ds-nsw-ckan-aefcde60-3b0c-4bc0-9af1-6fe652944ec2/details?q=
dat_nz <- read_csv(file = here("data/covid_cases_2021-11-04.csv")) |>   # https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics
  clean_names()
dat_vic <- read_csv(file = here("data/ncov-covid-cases-by-lga-csv.csv")) |>   # https://www.coronavirus.vic.gov.au/victorian-coronavirus-covid-19-data
  clean_names()

# NZ vax rate (fully vax of total population)
dat_nz_vax <- read_excel(path = here("data/nz_vax_by_date.xlsx")) |>   # https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-vaccine-data
  clean_names() |> 
  mutate(date = as_date(date)) |> 
  mutate(fully_vax_people = cumsum(second_dose_administered)) |> 
  mutate(fully_vax_rate = fully_vax_people / 5122600) |>  # Stats NZ population estimate at 30 June 2021
  select(date, fully_vax_rate) |> 
  mutate(delta_day = as.integer(date - ymd("2021-08-17")), 
         delta_level3_day = as.integer(date - ymd("2021-09-22")))

# NSW vax rate (fully vax of total population)
dat_nsw_vax <- read_excel(path = here("data/nsw_second_doses.xlsx")) |>    # https://covidlive.com.au/report/daily-vaccinations-people/nsw
  clean_names() |> 
  mutate(date = as_date(date)) |> 
  arrange(date) |> 
  mutate(fully_vax_rate = second_doses / 8176400) |>     # ABS population estimate at 31 March 2021
  mutate(outbreak_day = as.integer(date - ymd("2021-06-16"))) 

# VIC vax rate (fully vax of total population)
dat_vic_vax <- read_excel(path = here("data/vic_second_doses.xlsx")) |>    # https://covidlive.com.au/report/daily-vaccinations-people/vic
  clean_names() |> 
  mutate(date = as_date(date)) |> 
  arrange(date) |> 
  mutate(fully_vax_rate = second_doses / 6648600) |>      # ABS population estimate at 31 March 2021
  mutate(outbreak_day = as.integer(date - ymd("2021-08-05")))

# *****************************************************************************


# *****************************************************************************
# Manipulate data ----

# Delta outbreak local cases in NSW since 16 June
dat_nsw_delta <- dat_nsw |> 
  filter(notification_date >= ymd("2021-06-16")) |> 
  filter(!is.na(lga_name19)) |> 
  mutate(outbreak_day = as.integer(notification_date - ymd("2021-06-16"))) |> 
  count(outbreak_day)

# Delta outbreak local cases in VIC since 5 August
dat_vic_delta <- dat_vic |> 
  filter(diagnosis_date >= ymd("2021-08-05")) |> 
  filter(localgovernmentarea != "Overseas") |> 
  mutate(outbreak_day = as.integer(diagnosis_date - ymd("2021-08-05"))) |> 
  count(outbreak_day)

# Delta outbreak local cases in NZ since 17 August
dat_nz_delta <- dat_nz |> 
  filter(report_date >= ymd("2021-08-17")) |> 
  filter(dhb != "Managed Isolation & Quarantine", 
         is.na(historical)) |> 
  arrange(report_date) |> 
  mutate(outbreak_day = as.integer(report_date - ymd("2021-08-17"))) |> 
  filter(outbreak_day != max(outbreak_day)) |> 
  count(outbreak_day)

# Delta outbreak local cases in NZ since Auckland level 3 on 22 September
dat_nz_delta_l3 <- dat_nz |> 
  filter(report_date >= ymd("2021-09-22")) |> 
  filter(dhb != "Managed Isolation & Quarantine", 
         is.na(historical)) |> 
  arrange(report_date) |> 
  mutate(outbreak_day = as.integer(report_date - ymd("2021-09-22"))) |> 
  filter(outbreak_day != max(outbreak_day)) |> 
  count(outbreak_day)

# *****************************************************************************


# *****************************************************************************
# Visualise ----

# Chart of NZ vs NSW and VIC full delta outbreak
chart1_dat <- bind_rows(
  # NSW
  dat_nsw_delta |> 
    mutate(area = "nsw") |> 
    left_join(y = dat_nsw_vax |> 
                select(outbreak_day, fully_vax_rate), 
              by = "outbreak_day"), 
  
  # VIC 
  dat_vic_delta |> 
    mutate(area = "vic") |> 
    left_join(y = dat_vic_vax |> 
                select(outbreak_day, fully_vax_rate), 
              by = "outbreak_day"), 
  
  # NZ
  dat_nz_delta |> 
    mutate(area = "nz") |> 
    left_join(y = dat_nz_vax |> 
                select(delta_day, fully_vax_rate), 
              by = c("outbreak_day" = "delta_day"))
) |> 
  mutate(area = factor(x = area, 
                       levels = c("nsw", 
                                  "vic", 
                                  "nz"), 
                       labels = c("New South Wales since 16 June", 
                                  "Victoria since 5 August", 
                                  "NZ since 17 August"), 
                       ordered = TRUE))

chart1 <- chart1_dat |> 
  ggplot(mapping = aes(x = outbreak_day, 
                       y = n, 
                       colour = area)) + 
  geom_line(size = 1.25) + 
  xlab("Outbreak day") + 
  ylab("Number of\ncases reported") + 
  scale_colour_manual(values = c("New South Wales since 16 June" = grey(0.35), 
                                 "Victoria since 5 August" = grey(0.7), 
                                 "NZ since 17 August" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) + 
  scale_y_continuous(breaks = seq(0, 2600, 200), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.21, 0.92))

ggsave(filename = here("outputs/nz_vs_nsw_vic_1.png"), 
       plot = chart1, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

# Chart of NZ vs NSW full delta outbreak vs vaccination rate
chart1_with_vax <- chart1_dat |> 
  ggplot(mapping = aes(x = fully_vax_rate, 
                       y = n, 
                       colour = area)) + 
  geom_line(size = 1.25) + 
  xlab("Fully vaccinated (% of total population)") + 
  ylab("Number of\ncases reported") + 
  scale_colour_manual(values = c("New South Wales since 16 June" = grey(0.35), 
                                 "Victoria since 5 August" = grey(0.7), 
                                 "NZ since 17 August" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 0.8, 0.1), 
                     labels = percent_format(accuracy = 1)) + 
  scale_y_continuous(breaks = seq(0, 2400, 200), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.2, 0.92))

ggsave(filename = here("outputs/nz_vs_nsw_vic_1_with_vax.png"), 
       plot = chart1_with_vax, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

# Chart of NZ from level 3 vs NSW and VIC full delta outbreak
chart2_dat <- bind_rows(
  # NSW
  dat_nsw_delta |> 
    mutate(area = "nsw") |> 
    left_join(y = dat_nsw_vax |> 
                select(outbreak_day, fully_vax_rate), 
              by = "outbreak_day"), 
  # VIC 
  dat_vic_delta |> 
    mutate(area = "vic") |> 
    left_join(y = dat_vic_vax |> 
                select(outbreak_day, fully_vax_rate), 
              by = "outbreak_day"), 
  
  # NZ since level 3
  dat_nz_delta_l3 |> 
    mutate(area = "nz") |> 
    left_join(y = dat_nz_vax |> 
                select(delta_level3_day, fully_vax_rate), 
              by = c("outbreak_day" = "delta_level3_day"))
) |> 
  mutate(area = factor(x = area, 
                       levels = c("nsw", 
                                  "vic", 
                                  "nz"), 
                       labels = c("New South Wales since 16 June", 
                                  "Victoria since 5 August", 
                                  "NZ since 22 September"), 
                       ordered = TRUE))

chart2 <- chart2_dat |> 
  ggplot(mapping = aes(x = outbreak_day, 
                       y = n, 
                       colour = area)) + 
  geom_line(size = 1.25) + 
  xlab("Outbreak day") + 
  ylab("Number of\ncases reported") + 
  scale_colour_manual(values = c("New South Wales since 16 June" = grey(0.35), 
                                 "Victoria since 5 August" = grey(0.7), 
                                 "NZ since 22 September" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) + 
  scale_y_continuous(breaks = seq(0, 2400, 200), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.2, 0.92))

ggsave(filename = here("outputs/nz_vs_nsw_vic_2.png"), 
       plot = chart2, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

chart2_log_scale <- chart2_dat |> 
  ggplot(mapping = aes(x = outbreak_day, 
                       y = n, 
                       colour = area)) + 
  geom_line(size = 1.25) + 
  xlab("Outbreak day") + 
  ylab("Number of\ncases reported") + 
  scale_colour_manual(values = c("New South Wales since 16 June" = grey(0.35), 
                                 "Victoria since 5 August" = grey(0.7), 
                                 "NZ since 22 September" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) + 
  scale_y_log10(labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.2, 0.92))

ggsave(filename = here("outputs/nz_vs_nsw_vic_2_log.png"), 
       plot = chart2_log_scale, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

# Chart of NZ from level 3 vs NSW full delta outbreak vs vaccination rate
chart2_with_vax <- chart2_dat |> 
  ggplot(mapping = aes(x = fully_vax_rate, 
                       y = n, 
                       colour = area)) + 
  geom_line(size = 1.25) + 
  xlab("Fully vaccinated (% of total population)") + 
  ylab("Daily number of\ncases reported") + 
  scale_colour_manual(values = c("New South Wales since 16 June" = grey(0.35), 
                                 "Victoria since 5 August" = grey(0.7), 
                                 "NZ since 22 September" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 0.8, 0.1), 
                     labels = percent_format(accuracy = 1)) + 
  scale_y_continuous(breaks = seq(0, 2400, 200), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.2, 0.92))

ggsave(filename = here("outputs/nz_vs_nsw_vic_2_with_vax.png"), 
       plot = chart2_with_vax, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

chart2_with_vax_with_log <- chart2_dat |> 
  ggplot(mapping = aes(x = fully_vax_rate, 
                       y = n, 
                       colour = area)) + 
  geom_line(size = 1.25) + 
  xlab("Fully vaccinated (% of total population)") + 
  ylab("log(Daily number of\ncases reported)") + 
  scale_colour_manual(values = c("New South Wales since 16 June" = grey(0.35), 
                                 "Victoria since 5 August" = grey(0.7), 
                                 "NZ since 22 September" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 0.8, 0.1), 
                     labels = percent_format(accuracy = 1)) + 
  scale_y_log10(labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.2, 0.92))

ggsave(filename = here("outputs/nz_vs_nsw_vic_2_with_vax_log.png"), 
       plot = chart2_with_vax_with_log, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

