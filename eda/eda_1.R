install.packages('skimr')
install.packages('tsibble')
install.packages('feasts')
install.packages('patchwork')
install.packages('corrplot')
install.packages('gt')
install.packages('forecast')

library(readxl)
library(skimr)
library(dplyr)
library(patchwork)
library(gt)
library(forecast)
library(tidyr)
library(ggplot2)


## Initial EDA
getwd()
reservoir_raw <- readxl::read_excel('data/scenario_data.xlsx') 
reservoir_edit <- reservoir_raw

skim(reservoir_edit)




outliers <- ggplot(reservoir_edit, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(
    x = "Variable",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)

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




turbidity_flagged <- reservoir_edit %>%
  filter(variable == "turbidity") %>%
  ##group_by(depth_bin) %>%
  mutate(
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    outlier = value < (q1 - 1.5 * iqr) | value > (q3 + 1.5 * iqr)
  ) %>%
  ungroup()


sum(turbidity_flagged$outlier) / nrow(turbidity_flagged)



turbidity_outlier_plot <-

ggplot(turbidity_flagged, aes(x = datetime, y = value)) +
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

outlier_summary




library(ggplot2)
library(dplyr)
library(dplyr)
library(ggplot2)

df_plot <- reservoir_edit %>%
  filter(variable %in% c("turbidity", "rainfall")) %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date, variable) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

turb_max <- max(df_plot$value[df_plot$variable == "turbidity"], na.rm = TRUE)
rain_max <- max(df_plot$value[df_plot$variable == "rainfall"], na.rm = TRUE)

scale_factor <- turb_max / rain_max

ggplot() +
  geom_col(
    data = df_plot %>% filter(variable == "rainfall"),
    aes(x = date, y = value * scale_factor),
    fill = "skyblue",
    alpha = 0.4
  ) +
  geom_line(
    data = df_plot %>% filter(variable == "turbidity"),
    aes(x = date, y = value),
    colour = "brown",
    linewidth = 0.8
  ) +
  scale_y_continuous(
    name = "Turbidity",
    sec.axis = sec_axis(~ . / scale_factor, name = "Rainfall")
  ) +
  labs(
    x = "Date",
    title = "Rainfall and turbidity over time"
  ) +
  theme_minimal()



library(dplyr)
library(ggplot2)
library(tidyr)

# Daily rainfall
rain_daily <- reservoir_edit %>%
  filter(variable == "rainfall") %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(rainfall = mean(value, na.rm = TRUE), .groups = "drop")

# Daily turbidity
turb_daily <- reservoir_edit %>%
  filter(variable == "turbidity") %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(turbidity = mean(value, na.rm = TRUE), .groups = "drop")

# Global outliers for turbidity (IQR rule)
q1 <- quantile(turb_daily$turbidity, 0.25, na.rm = TRUE)
q3 <- quantile(turb_daily$turbidity, 0.75, na.rm = TRUE)
iqr <- q3 - q1
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

turb_daily <- turb_daily %>%
  mutate(outlier = turbidity < lower | turbidity > upper)

turb_max <- max(turb_daily$turbidity, na.rm = TRUE)
rain_max <- max(rain_daily$rainfall, na.rm = TRUE)
scale_factor <- turb_max / rain_max

ggplot() +
  # Rainfall bars (scaled for display)
  geom_col(
    data = rain_daily,
    aes(x = date, y = rainfall * scale_factor),
    fill = "skyblue",
    alpha = 0.35
  ) +
  # Turbidity line
  geom_line(
    data = turb_daily,
    aes(x = date, y = turbidity),
    colour = "brown",
    linewidth = 0.8
  ) +
  # Turbidity points coloured by outlier
  geom_point(
    data = turb_daily,
    aes(x = date, y = turbidity, colour = outlier),
    size = 1.6,
    alpha = 0.8
  ) +
  scale_color_manual(
    values = c("FALSE" = "black", "TRUE" = "red"),
    labels = c("Regular", "Outlier"),
    name = ""
  ) +
  scale_y_continuous(
    name = "Turbidity",
    sec.axis = sec_axis(~ . / scale_factor, name = "Rainfall")
  ) +
  labs(
    x = "Date",
    title = "Rainfall and turbidity over time (outliers highlighted)"
  ) +
  theme_minimal()


# Create long format skimr table - convert all to character
skim(reservoir_edit) %>%
  as_tibble() %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(
    cols = -c(skim_type, skim_variable),
    names_to = "statistic",
    values_to = "value"
  ) %>%
  filter(!is.na(value) & value != "NA") %>%
  gt(groupname_col = "skim_variable") %>%
  tab_header(title = "Reservoir Data - Descriptive Statistics") %>%
  cols_label(
    skim_type = "Type",
    statistic = "Statistic",
    value = "Value"
  ) %>%
  gtsave("skim_table_long.html")

variable_plot + variable_plot_daily / do_plot_daily

summary(reservoir_edit)


# Open the HTML file and screenshot




reservoir_edit %>% group_by(variable) %>% summarise(
  across(where(is.numeric), 
         list(mean = ~mean(.x, na.rm=T),
              sd = ~sd(.x, na.rm=T),
              min =~min(.x, na.rm=T),
              max = ~max(.x, na.rm=T)))
)


library(tsibble)


# make note of 6 duplicate timedates
duplicates(reservoir_edit_clean, index = datetime)



reservoir_edit_clean <- reservoir_edit %>%
  distinct(datetime, .keep_all = TRUE)


ts <- reservoir_edit_clean %>%
  as_tsibble(index = datetime)


library(ggplot2)



variable_plot <- ggplot(ts, aes(datetime, value)) +
  geom_line() + facet_grid(~ variable)

#Aggregate data by by day, variable

library(dplyr)
library(lubridate)
library(dplyr)
library(lubridate)

daily <- reservoir_edit %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(variable, date) %>%
  summarise(
    value = mean(value, na.rm = TRUE),  #could use median, but values are about same
    .groups = "drop"
  )


variable_plot_daily <- ggplot(daily, aes(date, value)) +
  geom_line() + geom_smooth() + facet_grid(~ variable)

#ACF Correlation plot


daily_wide <- daily %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )

ACF <- ccf(daily_wide$rainfall, daily_wide$turbidity, na.action = na.omit, main  = "", xlab = 'Lag (Days)', ylab = 'Correlation (ACF)' )
text(labels = "x is x")


## depth binning analysis          #######################################################
## depth binning analysis          ######################################################
## depth binning analysis          #######################################################

df_binned <- reservoir_edit %>%
  mutate(
    depth_bin = case_when(
      depth <= 15 ~ "0 - 15 m",
      depth <= 40 ~ "15 - 40 m",
      TRUE ~ "40+ m"
    )
  )


df_binned %>%  filter(!variable %in% c("rainfall", "evapotranspiration")) %>% 
  group_by(depth_bin, variable) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = depth_bin, y = mean_value)) +
  geom_col() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    x = "Depth bin",
    y = "Mean value"
  ) +   theme(axis.text.x = element_text(angle = 45, hjust = 1))


variable_plot_daily <- df_binned %>% filter(variable == 'dissolved oxygen') %>% 
  mutate(date = as.Date(datetime)) %>% group_by(date, depth_bin) %>% summarise(mean_do = mean(value)) %>% 
    ggplot(., aes(date, value)) +
  geom_point() + geom_smooth() + facet_grid(~ depth_bin)



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
  filter(variable == "water temperature") %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date, depth_bin) %>%
  summarise(
    mean_water_temp = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = date, y = mean_water_temp)) +
  geom_point(alpha = 0.4) + geom_smooth() +  facet_grid(~depth_bin) + labs(title = 'Water Temp, by Depth')


 (do_plot_daily / tur_plot_daily / temp_plot_daily)
 
  
  
library(dplyr)
library(tidyr)
library(ggplot2)

daily_std <- daily %>%
  group_by(variable) %>%
  mutate(
    z_value = (value - mean(value, na.rm = TRUE)) / sd(value, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(daily_std, aes(x = date, y = variable, fill = z_value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    name = "Relative level"
  ) +
  labs(
    x = "Date",
    y = "",
    title = "Relative behaviour of reservoir variables over time"
  )


(variable_plot_daily + outliers + turbidity_outlier_plot)  / do_plot_daily

library(dplyr)
library(tidyr)

daily_wide <- daily %>%
  select(date, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value)

cor_matrix <- cor(daily_wide[,-1], use = "pairwise.complete.obs")
cor_matrix

cor_long <- as.data.frame(as.table(cor_matrix))

ggplot(cor_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), size = 3) +
  scale_fill_gradient2(
    low = "green",
    mid = "white",
    high = "red",
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank()
  )


  