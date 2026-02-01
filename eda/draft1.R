library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)

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

# Quick QA summary
qa <- raw %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    pct_na = mean(is.na(value)),
    min = suppressWarnings(min(value, na.rm = TRUE)),
    max = suppressWarnings(max(value, na.rm = TRUE)),
    .groups = "drop"
  )
print(qa)

# Heatmap helper: aggregate to manageable grid (time x depth_bin)
grid <- profiles %>%
  mutate(dt_hour = floor_date(datetime, "hour")) %>%
  group_by(variable, dt_hour, depth_bin) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")


grid_day <- profiles %>%
  mutate(dt = as.Date(datetime)) %>%
  group_by(variable, dt, depth_bin) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")




##### heat maps


# ---- SETTINGS ----
time_agg  <- "hour"   # choose: "hour" or "day"
depth_bin <- 5        # metres per bin (e.g., 1, 2, 5, 10)

# ---- PREP: create binned/aggregated grid for heatmaps ----
profiles_binned <- profiles %>%
  filter(!is.na(depth), !is.na(value)) %>%
  mutate(
    depth_bin = floor(depth / depth_bin) * depth_bin,
    dt = case_when(
      time_agg == "hour" ~ floor_date(datetime, "hour"),
      time_agg == "day"  ~ as.POSIXct(as.Date(datetime), tz = "UTC"),
      TRUE ~ floor_date(datetime, "hour")
    )
  )

grid <- profiles_binned %>%
  group_by(variable, dt, depth_bin) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

# ---- PLOT FUNCTION ----
plot_heatmap <- function(grid_df, var_name,
                         title = NULL,
                         fill_name = "Value",
                         drop_sparse_days = FALSE) {
  
  df <- grid_df %>% filter(variable == var_name)
  
  # Optional: drop dates with very sparse coverage (keeps plot cleaner)
  if (drop_sparse_days) {
    df <- df %>%
      mutate(day = as.Date(dt)) %>%
      group_by(day) %>%
      filter(n() >= quantile(n(), 0.25, na.rm = TRUE)) %>%
      ungroup() %>%
      select(-day)
  }
  
  if (is.null(title)) title <- paste("Heatmap:", var_name)
  
  ggplot(df, aes(x = dt, y = depth_bin, fill = value)) +
    geom_tile() +
    scale_y_reverse() +
    labs(
      title = title,
      x = "Time",
      y = paste0("Depth (m), binned to ", unique(diff(sort(unique(df$depth_bin))))[1], " m"),
      fill = fill_name
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

# ---- EXAMPLES ----
plot_heatmap(grid, "water temperature", fill_name = "Temp")
plot_heatmap(grid, "turbidity", fill_name = "Turbidity")
plot_heatmap(grid, "dissolved oxygen", fill_name = "DO")

# heat map end



plot_heatmap <- function(var_name) {
  ggplot(filter(grid_day, variable == var_name),
         aes(x = dt_hour, y = depth_bin, fill = value)) +
    geom_tile() +
    scale_y_reverse() +
    labs(title = paste("Heatmap:", var_name),
         x = "Time", y = "Depth (m)", fill = "Value") +
    theme_minimal()
}

plot_heatmap("water temperature")
plot_heatmap("turbidity")
plot_heatmap("dissolved oxygen")

# Stratification index: surface minus deep (example bins)
strat <- profiles %>%
  mutate(layer = case_when(
    depth_bin <= 5 ~ "surface_0_5m",
    depth_bin >= 65 ~ "deep_65_72m",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(layer)) %>%
  mutate(dt_day = floor_date(datetime, "day")) %>%
  group_by(variable, dt_day, layer) %>%
  summarise(v = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = layer, values_from = v) %>%
  mutate(delta = surface_0_5m - deep_65_72m)

ggplot(filter(strat, variable == "water temperature"),
       aes(x = dt_day, y = delta)) +
  geom_line() +
  labs(title = "Temperature stratification (surface minus deep)",
       x = "Date", y = "Δ Temp (°C)") +
  theme_minimal()

# Rainfall vs turbidity response (daily mean near-surface)
surf_turb <- profiles %>%
  filter(variable == "turbidity", depth_bin <= 5) %>%
  group_by(date) %>%
  summarise(turbidity_surface = mean(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(met, by = "date") %>%
  arrange(date) %>%
  mutate(
    rain_lag1 = lag(rainfall, 1),
    rain_lag2 = lag(rainfall, 2)
  )

# Simple correlation checks (transparent + defensible)
with(surf_turb, cor(rainfall, turbidity_surface, use = "complete.obs"))
with(surf_turb, cor(rain_lag1, turbidity_surface, use = "complete.obs"))
with(surf_turb, cor(rain_lag2, turbidity_surface, use = "complete.obs"))

ggplot(surf_turb, aes(x = date)) +
  geom_col(aes(y = rainfall), alpha = 0.4) +
  geom_line(aes(y = turbidity_surface)) +
  labs(title = "Daily rainfall (bars) vs surface turbidity (line)",
       x = "Date", y = "Rainfall / Turbidity (scaled differently)") +
  theme_minimal()
