install.packages('skimr')

library(readxl)
library(skimr)
library(dplyr)

reservoir_raw <- readxl::read_excel('scenario_data.xlsx') 
reservoir_edit <- resivoir_raw

skim(reservoir_edit)


reservoir_edit %>% group_by(variable) %>% across(where(is.numeric), 
                                                 list(mean = ~mean(.x, na.rm=T),
                                                      sd = ~sd(.x, na.rm=T),
                                                      min =~min(.x, na.rm=T),
                                                      max = ~max(.x, na.rm=T)))
                                                 
  