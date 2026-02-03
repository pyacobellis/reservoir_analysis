# install.packages('skimr')
# install.packages('tsibble')
# install.packages('feasts')
# install.packages('patchwork')
# install.packages('corrplot')
# install.packages('gt')
# install.packages('forecast')

library(readxl)
library(dplyr)
library(patchwork)
library(forecast)
library(tidyr)
library(ggplot2)


## Read data
getwd()
reservoir_raw <- readxl::read_excel('data/scenario_data.xlsx') 
reservoir_edit <- reservoir_raw



## biased data check:


# Split "met" vars (no depth) vs profile vars (have depth)
met <- reservoir_edit %>% 
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    date = as.Date(datetime)
  ) %>% 
  filter(variable %in% c("rainfall", "evapotranspiration")) %>%
  select(date, variable, value) %>%
  group_by(date, variable) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = value)

profiles <- reservoir_edit %>% 
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    date = as.Date(datetime)
  ) %>%
  filter(!variable %in% c("rainfall", "evapotranspiration")) %>%
  filter(!is.na(depth)) %>%
  mutate(
    depth_bin = floor(depth)  # 1m bins; change to floor(depth/5)*5 for 5m bins
  )

temp_surface <- profiles %>%
  filter(variable == "turbidity", depth <= 5) %>%
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

surface_check <- ggplot(temp_surface, aes(x = date)) +
  geom_line(aes(y = temp), alpha = 0.4) +
  geom_line(aes(y = roll7), linewidth = 1) +
  geom_hline(aes(yintercept = long_mean), linetype = "dashed") +
  labs(
    title = "Surface turbidity: rolling mean bias check",
    y = "Temperature"
  ) +
  theme_minimal()


### Outlier detection ----


outliers_evap <- ggplot(reservoir_edit %>% filter(variable == 'evapotranspiration'), aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(
    x = "Variable",
    y = "Value"
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 2,
    colour = "red"
  ) +

  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


outliers_rest <- ggplot(reservoir_edit %>% filter(variable != 'evapotranspiration'), aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(
    x = "Variable",
    y = "Value"
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 2,
    colour = "red"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

turbidity_outliers <- reservoir_edit %>%
  filter(variable == "turbidity") %>%
  #group_by(depth_bin) %>%
  mutate(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    is_outlier = value < (q1 - 1.5 * iqr) | value > (q3 + 1.5 * iqr)
  )

outlier_summary <- turbidity_outliers %>%
  summarise(
    n = n(),
    n_outliers = sum(is_outlier, na.rm = TRUE),
    prop_outliers = n_outliers / n,
    .groups = "drop"
  )


outliers_evap + outliers_rest

turbidity_flagged <- reservoir_edit %>%
  filter(variable == "turbidity") %>%
  mutate(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    outlier = value < (q1 - 1.5 * iqr) | value > (q3 + 1.5 * iqr)
  ) %>%
  ungroup()


sum(turbidity_flagged$outlier) / nrow(turbidity_flagged)



turbidity_outlier_plot <-ggplot(turbidity_flagged, aes(x = datetime, y = value)) +
              geom_line(color = "grey70") +
              geom_point(
                aes(color = outlier),
                alpha = 0.6,
                size = 1.5
              ) +
              scale_color_manual(
                values = c("FALSE" = "black", "TRUE" = "red"),
                labels = c("Regular", "Outlier"),
                name = ""
              ) +
              labs(
                title = "Turbidity time series with outliers highlighted",
                x = "Date",
                y = "Turbidity"
              ) +
              theme_minimal()





#Aggregate data by by day, variable ----
reservoir_edit %>% filter(!variable %in% c('evapotranspiration', 'rainfall')) %>% 
  mutate(date = as.Date(datetime)) %>%
  group_by(variable, date) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() +
  facet_grid(~ variable, scales = "free_y") +
  labs(
    x = "Date",
    y = "Observation count",
    title = "Daily observation count by variable"
  ) +
  theme_minimal()



daily <- reservoir_edit %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(variable, date) %>%
  summarise(
    daily_mean = mean(value, na.rm = TRUE), 
    daily_median = median(value, na.rm = TRUE),
    .groups = "drop"
  )


variable_plot_daily <- ggplot(daily, aes(date, daily_mean)) +
  geom_line(alpha=0.45) + geom_smooth() + facet_grid(~ variable)




## patchwork 1 ----

surface_check / (outliers_evap + outliers_rest)  / turbidity_outlier_plot &
  theme(
    panel.border = element_rect(
      colour = "grey40",
      fill = NA,
      linewidth = 0.8
    )
  )
  

#Cross Correlation plot ----


daily_wide <- daily %>% select(-daily_median) %>% 
  pivot_wider(
    names_from = variable,
    values_from = daily_mean
  )


cross_corr <- ccf(daily_wide$rainfall, daily_wide$turbidity, na.action = na.omit, main  = "", xlab = 'Lag (Days)', ylab = 'Cross Correlation' )

cross_cor

## depth binning analysis          #######################################################
## depth binning analysis         
## depth binning analysis          

df_binned <- reservoir_edit %>%
  mutate(
    depth_bin = case_when(
      depth <= 15 ~ "0 - 15 m",
      depth <= 40 ~ "15 - 40 m",
      TRUE ~ "40+ m"
    )
  )



do_plot_daily <- df_binned %>%
 filter(variable == "dissolved oxygen") %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date, depth_bin, variable) %>%
  summarise(
    mean_do = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = date, y = mean_do)) +
  geom_point(alpha = 0.4) + geom_smooth() +  facet_grid(~depth_bin) + labs(title = 'Dissolved Oxygen, by depth')


tur_plot_daily <- df_binned %>%
  filter(variable == "turbidity") %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date, depth_bin) %>%
  summarise(
    mean_turbidity = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = date, y = mean_turbidity)) +
  geom_point(alpha = 0.4) + geom_smooth() +  facet_grid(~depth_bin) + labs(title = 'Turbidity, by depth')



 temp_plot_daily <- df_binned %>%
  filter(variable == "turbidity") %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date, depth_bin) %>%
  summarise(
    mean_water_temp = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = date, y = mean_water_temp)) +
  geom_point(alpha = 0.4) + geom_smooth() +  facet_grid(~depth_bin) + labs(title = 'Water Temp, by Depth')


 (do_plot_daily / tur_plot_daily / temp_plot_daily)
 
  






  