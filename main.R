library(dplyr)
source("functions.R")

number_of_observations <- 300

# set up the synthetic data
real_data <- runif(number_of_observations, 0, 10)
measured_value_1 <- sapply(real_data, instrument_1)
measured_value_2 <- sapply(real_data, instrument_2)

difference =  measured_value_1 - measured_value_2

plot_differences(measured_value_1, measured_value_2)
plot_differences_wrt_data(real_data, difference)

# create a data frame to easily pass the data into functions below
data <- data.frame(real_data, difference, stringsAsFactors = FALSE)

plot_simple_ci(data)

plot_ci(linear_fit(data), data)

plot_ci(quadratic_fit(data), data)
