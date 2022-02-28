dat_bench <- dat_countries |> 
  left_join(y = dat_owd |> 
              select(location, date, 
                     new_cases, new_deaths, new_tests_smoothed, 
                     reproduction_rate, 
                     people_fully_vaccinated, 
                     stringency_index, 
                     population), 
            by = c("owd_country" = "location")) |> 
  # Weekly summary for last 2 weeks of OWD data
  filter(date >= ymd("2021-12-01")) |> 
  mutate(new_deaths = ifelse((owd_country == "France") & (new_deaths > 2500), NA_real_, new_deaths)) |>     # France outlier   
  arrange(owd_country, country_abbr, country_area, date) |> 
  group_by(owd_country, country_abbr, country_area) |> 
  mutate(cases_valid = ifelse(is.na(new_cases), 0L, 1L),
         deaths_valid = ifelse(is.na(new_deaths), 0L, 1L), 
         tests_valid = ifelse(is.na(new_tests_smoothed), 0L, 1L), 
         reproduction_rate_valid = ifelse(is.na(reproduction_rate), 0L, 1L), 
         vax_valid = ifelse(is.na(people_fully_vaccinated), 0L, 1L),
         stringency_valid = ifelse(is.na(stringency_index), 0L, 1L)) |> 
  summarise(max_new_cases = max(new_cases, na.rm = TRUE), 
            total_cases_n = sum(cases_valid), 
            max_new_deaths = max(new_deaths, na.rm = TRUE), 
            total_deaths_n = sum(deaths_valid), 
            max_reproduction_rate = max(reproduction_rate, na.rm = TRUE), 
            population = mean(population, na.rm = TRUE)) |> 
  mutate(max_new_cases_per_5m = max_new_cases / (population / 5000000), 
         max_new_deaths_per_5m = max_new_deaths / (population / 5000000)) |> 
  ungroup() |> 
  mutate(country_group = case_when(
    owd_country == "New Zealand" ~ "nz", 
    TRUE ~ "other"
  )) |> 
  select(country_group, owd_country, max_reproduction_rate, max_new_cases_per_5m, max_new_deaths_per_5m) |> 
  pivot_longer(cols = c(-owd_country, -country_group), names_to = "measure", values_to = "value") |> 
  arrange(measure, value) |> 
  mutate(measure_i = row_number()) |> 
  mutate(value_rounded = ifelse(measure == "max_reproduction_rate", 
                                comma(value, 0.1), 
                                comma(value, 1))) |> 
  mutate(measure = factor(x = measure, 
                          levels = c("max_new_cases_per_5m", 
                                     "max_new_deaths_per_5m", 
                                     "max_reproduction_rate"), 
                          labels = c("Maximum daily new cases per 5 million people", 
                                     "Maximum daily deaths per 5 million people", 
                                     "Maximum estimated effective reproduction rate"), 
                          ordered = TRUE))

chart <- dat_bench |> 
  filter(measure == "Maximum estimated effective reproduction rate") |> 
  ggplot(mapping = aes(x = measure_i, 
                       y = value, 
                       fill = country_group)) + 
  geom_col() + 
  scale_x_continuous(labels = dat_bench$owd_country, 
                   breaks = dat_bench$measure_i, 
                   expand = expansion(0, 0)) +
  coord_flip() + 
  scale_fill_manual(values = c("nz" = "firebrick", 
                               "other" = grey(0.6)), 
                    guide = "none") + 
  xlab("") + 
  ylab("") + 
  ggtitle("Daily maximum estimated reproduction rate from 1 December 2021 to 26 February 2022") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank())
