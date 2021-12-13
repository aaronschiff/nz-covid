# Compare NZ, NSW, and VIC delta outbreaks

# *****************************************************************************
# Setup ---- 

library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(scales)
library(readxl)
library(RcppRoll)
library(ragg)
library(systemfonts)

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

# Case data files
# https://data.nsw.gov.au/search/dataset/ds-nsw-ckan-aefcde60-3b0c-4bc0-9af1-6fe652944ec2/details?q=
dat_nsw <- read_csv(file = here("data/confirmed_cases_table1_location.csv"))   

# https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-case-demographics
dat_nz <- read_csv(file = here("data/covid_cases_2021-12-13.csv")) |>   
  clean_names()

# https://www.coronavirus.vic.gov.au/victorian-coronavirus-covid-19-data
dat_vic <- read_csv(file = here("data/ncov-covid-cases-by-lga-csv.csv")) |>   
  clean_names()

# NZ vax rate (fully vax of total population)
# https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-news-and-media-updates
# Manually updated from MoH press releases
dat_nz_vax <- read_excel(path = here("data/nz_vax_by_date.xlsx")) |>   
  clean_names() |> 
  mutate(date = as_date(date)) |> 
  mutate(fully_vax_people = cumsum(second_dose_administered)) |> 
  mutate(fully_vax_rate = fully_vax_people / 5122600) |>  # Stats NZ population estimate at 30 June 2021
  select(date, fully_vax_rate) |> 
  mutate(delta_day = as.integer(date - ymd("2021-08-17")), 
         delta_level3_day = as.integer(date - ymd("2021-09-22")))

# NSW vax rate (fully vax of total population)
# https://covidlive.com.au/report/daily-vaccinations-people/nsw
dat_nsw_vax <- read_excel(path = here("data/nsw_second_doses.xlsx")) |>    
  clean_names() |> 
  mutate(date = as_date(date)) |> 
  arrange(date) |> 
  mutate(fully_vax_rate = second_doses / 8176400) |>     # ABS population estimate at 31 March 2021
  mutate(outbreak_day = as.integer(date - ymd("2021-06-16"))) 

# VIC vax rate (fully vax of total population)
# https://covidlive.com.au/report/daily-vaccinations-people/vic
dat_vic_vax <- read_excel(path = here("data/vic_second_doses.xlsx")) |>    
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

# Delta outbreak local cases in NZ since 18 August
dat_nz_delta <- dat_nz |> 
  filter(report_date >= ymd("2021-08-18")) |> 
  filter(dhb != "Managed Isolation & Quarantine", 
         is.na(historical)) |> 
  arrange(report_date) |> 
  mutate(outbreak_day = as.integer(report_date - ymd("2021-08-18"))) |> 
  filter(outbreak_day != max(outbreak_day)) |> 
  count(outbreak_day, report_date)

# Delta outbreak local cases in NZ since Auckland level 3 on 22 September
dat_nz_delta_l3 <- dat_nz |> 
  filter(report_date >= ymd("2021-09-22")) |> 
  filter(dhb != "Managed Isolation & Quarantine", 
         is.na(historical)) |> 
  arrange(report_date) |> 
  mutate(outbreak_day = as.integer(report_date - ymd("2021-09-22"))) |> 
  filter(outbreak_day != max(outbreak_day)) |> 
  count(outbreak_day, report_date)

# *****************************************************************************


# *****************************************************************************
# Visualise ----

# Chart of delta outbreak daily cases
chart_outbreak_day_dat <- bind_rows(
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
    select(-report_date) |> 
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
                                  "NZ since 18 August"), 
                       ordered = TRUE))

chart_outbreak_day <- chart_outbreak_day_dat |> 
  ggplot(mapping = aes(x = outbreak_day, 
                       y = n, 
                       colour = area)) + 
  geom_vline(xintercept = 0, 
             colour = "cornflowerblue", 
             linetype = "dotted", 
             size = 0.4) + 
  geom_vline(xintercept = 35, 
             colour = "cornflowerblue", 
             linetype = "dotted", 
             size = 0.4) + 
  geom_vline(xintercept = 107, 
             colour = "cornflowerblue", 
             linetype = "dotted", 
             size = 0.4) + 
  geom_line(size = 0.75, alpha = 0.75) + 
  geom_smooth(method = "gam", se = FALSE, size = 0.35) + 
  annotate(geom = "text", 
           x = 1, 
           y = 2250, 
           hjust = 0, 
           label = "Auckland AL4", 
           family = "Fira Sans Custom", 
           fontface = "bold", 
           size = 2, 
           colour = "cornflowerblue") + 
  annotate(geom = "text", 
           x = 36, 
           y = 2250, 
           hjust = 0, 
           label = "Auckland AL3", 
           family = "Fira Sans Custom", 
           fontface = "bold", 
           size = 2, 
           colour = "cornflowerblue") + 
  annotate(geom = "text", 
           x = 108, 
           y = 2250, 
           hjust = 0, 
           label = "CPF", 
           family = "Fira Sans Custom", 
           fontface = "bold", 
           size = 2, 
           colour = "cornflowerblue") + 
  xlab("Outbreak day") + 
  ylab("Daily number of\ncases reported") + 
  ggtitle(label = "Delta outbreak daily cases", 
          subtitle = "Chart by Aaron Schiff. CC-BY 4.0. Code: schiff.nz/covid/nz-delta. Data sources: health.govt.nz, data.nsw.gov.au, coronavirus.vic.gov.au") + 
  scale_colour_manual(values = c("New South Wales since 16 June" = grey(0.35), 
                                 "Victoria since 5 August" = grey(0.7), 
                                 "NZ since 18 August" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) + 
  scale_y_continuous(breaks = seq(0, 2400, 200), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = "top", 
        legend.margin = margin(0, 0, 0, 0, "pt"), 
        plot.margin = margin(4, 12, 4, 4, "pt"), 
        plot.subtitle = element_text(size = rel(0.6)))

ggsave(filename = here("outputs/nz-vs-au/nz_vs_nsw_vic_by_outbreak_day.png"), 
       plot = chart_outbreak_day, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = agg_png, 
       bg = "white")

chart_outbreak_day_mean_log <- chart_outbreak_day_dat |> 
  group_by(area) |> 
  mutate(mean_n = roll_mean(x = n, n = 7L, align = "right", fill = NA_real_)) |> 
  ggplot(mapping = aes(x = outbreak_day, 
                       y = mean_n, 
                       colour = area)) + 
  geom_vline(xintercept = 0, 
             colour = "cornflowerblue", 
             linetype = "dotted", 
             size = 0.4) + 
  geom_vline(xintercept = 35, 
             colour = "cornflowerblue", 
             linetype = "dotted", 
             size = 0.4) + 
  geom_vline(xintercept = 107, 
             colour = "cornflowerblue", 
             linetype = "dotted", 
             size = 0.4) + 
  geom_line(size = 0.75) + 
  annotate(geom = "text", 
           x = 1, 
           y = 2500, 
           hjust = 0, 
           label = "Auckland AL4", 
           family = "Fira Sans Custom", 
           fontface = "bold", 
           size = 2, 
           colour = "cornflowerblue") +
  annotate(geom = "text", 
           x = 36, 
           y = 2500, 
           hjust = 0, 
           label = "Auckland AL3", 
           family = "Fira Sans Custom", 
           fontface = "bold", 
           size = 2, 
           colour = "cornflowerblue") + 
  annotate(geom = "text", 
           x = 108, 
           y = 2500, 
           hjust = 0, 
           label = "CPF", 
           family = "Fira Sans Custom", 
           fontface = "bold", 
           size = 2, 
           colour = "cornflowerblue") + 
  xlab("Outbreak day") + 
  ylab("Daily number of\ncases reported\n(log scale)") + 
  ggtitle(label = "Delta outbreak daily cases: 7-day moving average (log scale)", 
          subtitle = "Chart by Aaron Schiff. CC-BY 4.0. Code: schiff.nz/covid/nz-delta. Data sources: health.govt.nz, data.nsw.gov.au, coronavirus.vic.gov.au") + 
  scale_colour_manual(values = c("New South Wales since 16 June" = grey(0.35), 
                                 "Victoria since 5 August" = grey(0.7), 
                                 "NZ since 18 August" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) + 
  scale_y_log10(labels = comma_format(accuracy = 1), 
                breaks = c(1, 10, 100, 1000), 
                limits = c(1, 2500)) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = "top", 
        legend.margin = margin(0, 0, 0, 0, "pt"), 
        plot.margin = margin(4, 12, 4, 4, "pt"), 
        plot.subtitle = element_text(size = rel(0.6)))

ggsave(filename = here("outputs/nz-vs-au/nz_vs_nsw_vic_by_outbreak_day_mean_log.png"), 
       plot = chart_outbreak_day_mean_log, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = agg_png, 
       bg = "white")

# Chart of NZ vs NSW and VIC full delta outbreak vs vaccination rate
chart_outbreak_day_vax <- chart_outbreak_day_dat |> 
  ggplot(mapping = aes(x = fully_vax_rate, 
                       y = n, 
                       colour = area)) + 
  geom_vline(xintercept = c(0.1865, 0.3266, 0.7111), 
             colour = "cornflowerblue", 
             linetype = "dotted", 
             size = 0.3) + 
  geom_point(size = 0.5) + 
  geom_line(size = 0.3) + 
  annotate(geom = "text", 
           x = 0.1865 + 0.005, 
           y = 2250, 
           hjust = 0, 
           label = "Auckland AL4", 
           family = "Fira Sans Custom", 
           fontface = "bold", 
           size = 2, 
           colour = "cornflowerblue") + 
  annotate(geom = "text", 
           x = 0.3266 + 0.005, 
           y = 2250, 
           hjust = 0, 
           label = "Auckland AL3", 
           family = "Fira Sans Custom", 
           fontface = "bold", 
           size = 2, 
           colour = "cornflowerblue") + 
  annotate(geom = "text", 
           x = 0.7111 + 0.005, 
           y = 2250, 
           hjust = 0, 
           label = "CPF", 
           family = "Fira Sans Custom", 
           fontface = "bold", 
           size = 2, 
           colour = "cornflowerblue") + 
  xlab("Fully vaccinated (% of total population)") + 
  ylab("Daily number of\ncases reported") + 
  ggtitle(label = "Delta outbreak daily cases vs vaccination rate",
          subtitle = "Chart by Aaron Schiff. CC-BY 4.0. Code: schiff.nz/covid/nz-delta. Data sources: health.govt.nz, data.nsw.gov.au, coronavirus.vic.gov.au, covidlive.com.au") + 
  scale_colour_manual(values = c("New South Wales since 16 June" = grey(0.35), 
                                 "Victoria since 5 August" = grey(0.7), 
                                 "NZ since 18 August" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 0.8, 0.1), 
                     labels = percent_format(accuracy = 1)) + 
  scale_y_continuous(breaks = seq(0, 2400, 200), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans Custom") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = "top", 
        legend.margin = margin(0, 0, 0, 0, "pt"), 
        plot.margin = margin(4, 12, 4, 4, "pt"), 
        plot.subtitle = element_text(size = rel(0.6)))

ggsave(filename = here("outputs/nz-vs-au/nz_vs_nsw_vic_by_vax_rate.png"), 
       plot = chart_outbreak_day_vax, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = agg_png, 
       bg = "white")

# *****************************************************************************
