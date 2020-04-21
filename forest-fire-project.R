library(readr)
library(dplyr)
library(ggplot2)
library(purrr)

forestData <- read.csv("forestfires.csv")

View(forestData)

###
# X: X-axis spatial coordinate within the Montesinho park map: 1 to 9
# Y: Y-axis spatial coordinate within the Montesinho park map: 2 to 9
# month: Month of the year: 'jan' to 'dec'
# day: Day of the week: 'mon' to 'sun'
# FFMC: Fine Fuel Moisture Code index from the FWI system: 18.7 to 96.20
# DMC: Duff Moisture Code index from the FWI system: 1.1 to 291.3
# DC: Drought Code index from the FWI system: 7.9 to 860.6
# ISI: Initial Spread Index from the FWI system: 0.0 to 56.10
# temp: Temperature in Celsius degrees: 2.2 to 33.30
# RH: Relative humidity in percentage: 15.0 to 100
# wind: Wind speed in km/h: 0.40 to 9.40
# rain: Outside rain in mm/m2 : 0.0 to 6.4
# area: The burned area of the forest (in ha): 0.00 to 1090.84
###

# Group the data from the dataset by day

forestFiresByDay <- forestData %>% 
  group_by(day) %>% 
  summarize(n()) %>%
  as_tibble() %>% 
  mutate(day = factor(day, levels = c('mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun')))

# Group the data from the dataset by month

forestFiresByMonth <- forestData %>% 
  group_by(month) %>% 
  summarize(n()) %>% 
  as_tibble() %>% 
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))


# Visualize the forest fires by month and day
ggplot(data = forestFiresByMonth) + 
  aes(x = month, y = `n()`, fill = 'n()') + 
  geom_bar(stat = "identity") +
  labs(x = "month", y = "number of fires") + 
  theme_classic()

ggplot(data = forestFiresByDay) + 
  aes(x = day, y = `n()`, fill = 'n()') + 
  geom_bar(stat = "identity") +
  labs(x = "day", y = "number of fires") + 
  theme_classic()

# create a function to write multiple functions
plot_by_month_function <- function(x, y) {
  ggplot(data = forestData) + 
    aes(x = month, y = y) + 
    geom_boxplot() + 
    theme_minimal()
}


plot_by_day_function <- function(x, y) {
  ggplot(data = forestData) + 
    aes(x = day, y = y) + 
    geom_boxplot() + 
    theme_minimal()
}

# variables to plot 
x_var_days <- names(forestData)[4]
y_var <- forestData[,5:12]
x_var_months <- names(forestData)[3]


# Call map2() to plot the various inputs
fire_patterns_day <- map2(.x = x_var, .y = y_var, .f = plot_by_day_function)
fire_patterns_day

fire_patterns_month <- map2(x_var_months, y_var, plot_by_month_function)
fire_patterns_month

# scatter plot test
# ggplot(data = forestData) + 
#   aes(x = forestData$area, y = forestData$FFMC) + 
#   geom_point(stat = "identity") + 
#   theme_minimal()

# scatter plot functions
forest_scatter_plots <- function(x, y) {
  ggplot(data = forestData) + 
    aes(x = x, y = area) + 
    geom_point() + 
    theme_minimal()
}

# map and call the functions
map2(forestData[,5:12], names(forestData)[13], forest_scatter_plots)