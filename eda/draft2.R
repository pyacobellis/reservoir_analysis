install.packages('zoo')

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(zoo)

path <- "scenario_data.xlsx"

raw <- read_excel(path, sheet = "in") %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    date = as.Date(datetime)
  )

# Split "met" vars (no depth) vs profile vars (have depth)
met <- raw %>%
  filter(variable %in% c("rainfall", "evapotranspiration")) %>%
  select(date, variable, value) %>%
  group_by(date, variable) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = value)

profiles <- raw %>%
  filter(!variable %in% c("rainfall", "evapotranspiration")) %>%
  filter(!is.na(depth)) %>%
  mutate(
    depth_bin = floor(depth)  # 1m bins; change to floor(depth/5)*5 for 5m bins
  )

temp_surface <- profiles %>%
  filter(variable == "water temperature", depth <= 5) %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(temp = mean(value, na.rm = TRUE), .groups = "drop")

temp_surface <- temp_surface %>%
  arrange(date) %>%
  mutate(
    roll7 = zoo::rollmean(temp, k = 7, fill = NA, align = "right"),
    long_mean = mean(temp, na.rm = TRUE),
    delta = roll7 - long_mean
  )

ggplot(temp_surface, aes(x = date)) +
  geom_line(aes(y = temp), alpha = 0.4) +
  geom_line(aes(y = roll7), linewidth = 1) +
  geom_hline(aes(yintercept = long_mean), linetype = "dashed") +
  labs(
    title = "Surface water temperature: rolling mean bias check",
    y = "Temperature"
  ) +
  theme_minimal()
