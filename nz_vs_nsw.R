# Compare NZ and NSW delta outbreaks

library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(scales)
library(readxl)

# Case data files
dat_nsw <- read_csv(file = here("data/confirmed_cases_table1_location.csv"))
dat_nz <- read_csv(file = here("data/covid_cases_2021-11-01.csv")) |> 
  clean_names()

# Delta outbreak local cases in NSW
dat_nsw_delta <- dat_nsw |> 
  filter(notification_date >= ymd("2021-06-16")) |> 
  filter(!is.na(lga_name19)) |> 
  mutate(outbreak_day = as.integer(notification_date - ymd("2021-06-16"))) |> 
  count(outbreak_day)

# Delta outbreak local cases in NZ
dat_nz_delta <- dat_nz |> 
  filter(report_date >= ymd("2021-08-17")) |> 
  filter(dhb != "Managed Isolation & Quarantine", 
         is.na(historical)) |> 
  arrange(report_date) |> 
  mutate(outbreak_day = as.integer(report_date - ymd("2021-08-17"))) |> 
  filter(outbreak_day != max(outbreak_day)) |> 
  count(outbreak_day)

# Delta outbreak local cases in NZ since level 3
dat_nz_delta_l3 <- dat_nz |> 
  filter(report_date >= ymd("2021-09-22")) |> 
  filter(dhb != "Managed Isolation & Quarantine", 
         is.na(historical)) |> 
  arrange(report_date) |> 
  mutate(outbreak_day = as.integer(report_date - ymd("2021-09-22"))) |> 
  filter(outbreak_day != max(outbreak_day)) |> 
  count(outbreak_day)

# NZ vax rate (fully vax of total population)
dat_nz_vax <- read_excel(path = here("data/nz_vax_by_date.xlsx")) |> 
  clean_names() |> 
  mutate(date = as_date(date)) |> 
  mutate(fully_vax_people = cumsum(second_dose_administered)) |> 
  mutate(fully_vax_rate = fully_vax_people / 5122600) |>  # Stats NZ population estimate at 30 June 2021
  select(date, fully_vax_rate) |> 
  mutate(delta_day = as.integer(date - ymd("2021-08-17")), 
         delta_level3_day = as.integer(date - ymd("2021-09-22")))

# NSW vax rate (fully vax of total population)
dat_nsw_vax <- read_excel(path = here("data/nsw_second_doses.xlsx")) |> 
  clean_names() |> 
  mutate(date = as_date(date)) |> 
  arrange(date) |> 
  mutate(fully_vax_rate = second_doses / 8176400) |>     # ABS population estimate at 31 March 2021
  mutate(outbreak_day = as.integer(date - ymd("2021-06-16"))) |> 
  print()

# Chart of NZ vs NSW full delta outbreak
chart1_dat <- bind_rows(
  # NSW
  dat_nsw_delta |> 
    mutate(area = "New South Wales") |> 
    left_join(y = dat_nsw_vax |> 
                select(outbreak_day, fully_vax_rate), 
              by = "outbreak_day"), 
  # NZ
  dat_nz_delta |> 
    mutate(area = "NZ since start of delta outbreak") |> 
    left_join(y = dat_nz_vax |> 
                select(delta_day, fully_vax_rate), 
              by = c("outbreak_day" = "delta_day"))
) 

chart1 <- chart1_dat |> 
  ggplot(mapping = aes(x = outbreak_day, 
                       y = n, 
                       colour = area)) + 
  geom_line(size = 1.5) + 
  xlab("Outbreak day") + 
  ylab("Number of\ncases reported") + 
  scale_colour_manual(values = c("New South Wales" = grey(0.65), 
                                 "NZ since start of delta outbreak" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) + 
  scale_y_continuous(breaks = seq(0, 1600, 100), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.21, 0.92))

ggsave(filename = here("outputs/nz_vs_nsw_1.png"), 
       plot = chart1, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

chart1_with_vax <- chart1_dat |> 
  ggplot(mapping = aes(x = fully_vax_rate, 
                       y = n, 
                       colour = area)) + 
  geom_line(size = 1.5) + 
  xlab("Fully vaccinated (% of total population)") + 
  ylab("Number of\ncases reported") + 
  scale_colour_manual(values = c("New South Wales" = grey(0.65), 
                                 "NZ since start of delta outbreak" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 0.8, 0.1), 
                     labels = percent_format(accuracy = 1)) + 
  scale_y_continuous(breaks = seq(0, 1600, 100), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.75, 0.92))

ggsave(filename = here("outputs/nz_vs_nsw_1_with_vax.png"), 
       plot = chart1_with_vax, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

# Chart of NZ from level 3 vs NSW full delta outbreak
chart2_dat <- bind_rows(
  # NSW
  dat_nsw_delta |> 
    mutate(area = "New South Wales") |> 
    left_join(y = dat_nsw_vax |> 
                select(outbreak_day, fully_vax_rate), 
              by = "outbreak_day"), 
  # NZ since level 3
  dat_nz_delta_l3 |> 
    mutate(area = "NZ delta outbreak since Auckland level 3") |> 
    left_join(y = dat_nz_vax |> 
                select(delta_level3_day, fully_vax_rate), 
              by = c("outbreak_day" = "delta_level3_day"))
)

chart2 <- chart2_dat |> 
  ggplot(mapping = aes(x = outbreak_day, 
                       y = n, 
                       colour = area)) + 
  geom_line(size = 1.5) + 
  xlab("Outbreak day") + 
  ylab("Number of\ncases reported") + 
  scale_colour_manual(values = c("New South Wales" = grey(0.65), 
                                 "NZ delta outbreak since Auckland level 3" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) + 
  scale_y_continuous(breaks = seq(0, 1600, 100), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.25, 0.92))

ggsave(filename = here("outputs/nz_vs_nsw_2.png"), 
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
  geom_line(size = 1.5) + 
  xlab("Outbreak day") + 
  ylab("Number of\ncases reported") + 
  scale_colour_manual(values = c("New South Wales" = grey(0.65), 
                                 "NZ delta outbreak since Auckland level 3" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) + 
  scale_y_log10() + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.25, 0.92))

ggsave(filename = here("outputs/nz_vs_nsw_2_log.png"), 
       plot = chart2_log_scale, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

chart2_with_vax <- chart2_dat |> 
  ggplot(mapping = aes(x = fully_vax_rate, 
                       y = n, 
                       colour = area)) + 
  geom_line(size = 1.5) + 
  xlab("Fully vaccinated (% of total population)") + 
  ylab("Number of\ncases reported") + 
  scale_colour_manual(values = c("New South Wales" = grey(0.65), 
                                 "NZ delta outbreak since Auckland level 3" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 0.8, 0.1), 
                     labels = percent_format(accuracy = 1)) + 
  scale_y_continuous(breaks = seq(0, 1600, 100), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.75, 0.92))

ggsave(filename = here("outputs/nz_vs_nsw_2_with_vax.png"), 
       plot = chart2_with_vax, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

chart2_with_vax_shifted <- chart2_dat |> 
  mutate(fully_vax_rate = ifelse(area == "NZ delta outbreak since Auckland level 3", 
                                 fully_vax_rate - (0.33159743 - 0.03172998), 
                                 fully_vax_rate)) |> 
  mutate(area = ifelse(area == "NZ delta outbreak since Auckland level 3", 
                       "NZ delta outbreak since Auckland level 3 (SHIFTED)", 
                       area)) |> 
  ggplot(mapping = aes(x = fully_vax_rate, 
                       y = n, 
                       colour = area)) + 
  geom_line(size = 1.5) + 
  ylab("Number of\ncases reported") + 
  xlab("Fully vaccinated (% of total population); shifted for NZ") + 
  scale_colour_manual(values = c("New South Wales" = grey(0.65), 
                                 "NZ delta outbreak since Auckland level 3 (SHIFTED)" = "cornflowerblue"), 
                      name = NULL) + 
  scale_x_continuous(breaks = seq(0, 0.8, 0.1), 
                     labels = percent_format(accuracy = 1)) + 
  scale_y_continuous(breaks = seq(0, 1600, 100), 
                     labels = comma_format(accuracy = 1)) + 
  theme_minimal(base_family = "Fira Sans") + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(size = 0.2), 
        axis.title.y = element_text(angle = 0, hjust = 0, margin = margin(0, 8, 0, 0, "pt")), 
        axis.title.x = element_text(margin = margin(8, 0, 0, 0, "pt")), 
        legend.position = c(0.75, 0.92))

ggsave(filename = here("outputs/chart2_with_vax_shifted.png"), 
       plot = chart2_with_vax_shifted, 
       width = 2400, 
       height = 1600, 
       units = "px", 
       device = "png", 
       bg = "white")

