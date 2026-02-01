
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(zoo)
library(cluster)

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
  filter(!variable %in% c("rainfall", "evapotranspiration"))  %>%
  mutate(
    depth_bin = floor(depth)  # 1m bins; change to floor(depth/5)*5 for 5m bins
  )


library(dplyr)
library(lubridate)
library(dplyr)
library(lubridate)
library(skimr)

skim(raw)

raw %>%
  mutate(
    depth_missing = is.na(depth),
    date = as.Date(datetime)
  ) %>%
  group_by(date) %>%
  summarise(
    missing_depth_n = sum(depth_missing),
    total_n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(missing_depth_n)) %>% View()
  head(10)
