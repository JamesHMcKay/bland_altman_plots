# Both instruments apply some random variation to the real value,
# with a mean of zero and variance of 0.1, using the rnorm function.
# Instrument 1 adds some additional random values with a bias, by adding a random
# number from a uniform range between 0 and a value proportional to the value being
# measured.  Therefore, the bias gets larger for larger values of the value
# being measured
instrument_1 <- function(real_value) {
  measured_value = real_value + rnorm(1, 0, 1) - runif(1, 0, 0.5 * real_value + 0.02 * real_value ** 2)
  return(measured_value)
}

instrument_2 <- function(real_value) {
  measured_value = real_value + rnorm(1, 0, 1)
  return(measured_value)
}

# fit a straight line through the differences (linear regression)
linear_fit <- function(data) {
  fit <- lm(difference ~ real_data, data)

  modelled_difference <- fit$coefficients[[1]] +
    fit$coefficients[[2]] * real_data

  return(modelled_difference)
}

# fit a quadratic line through the differences
quadratic_fit <- function(data) {
  # create the quadratic term
  data$real_data_squared <- data$real_data ** 2

  quadratic_fit <- lm(difference ~ real_data + real_data_squared, data)

  modelled_difference <- quadratic_fit$coefficients[[1]] +
    quadratic_fit$coefficients[[2]] * real_data +
    quadratic_fit$coefficients[[3]] * real_data * real_data
  return(modelled_difference)
}

plot_simple_ci <- function(data) {
  mean_difference = mean(data$difference)
  sd_difference = sd(data$difference)
  percentile <- 0.975

  crude_upper_limit = mean_difference + qnorm(percentile) * sd_difference
  crude_lower_limit = mean_difference - qnorm(percentile) * sd_difference

  plot(data$real_data, data$difference)
  abline(mean_difference, 0, lty = 1)
  abline(crude_lower_limit, 0, lty = 2)
  abline(crude_upper_limit, 0, lty = 2)
}

plot_ci <- function(modeled_differences, data) {
  # save variable for convenience (instead of writing data$real_data everytime)
  real_data <- data$real_data

  data$residuals <- abs(modeled_differences - difference)

  # fit a linear model to the residuals
  fit_to_residuals <- lm(residuals ~ real_data, data)

  lower_limit <- modeled_differences +  sqrt(0.5 * pi) * (fit_to_residuals$coefficients[[1]] + fit_to_residuals$coefficients[[2]] * real_data)
  upper_limit <- modeled_differences -  sqrt(0.5 * pi) * (fit_to_residuals$coefficients[[1]] + fit_to_residuals$coefficients[[2]] * real_data)

  # plot the results
  plot(real_data, difference)
  lines(real_data[order(real_data)], modeled_differences[order(real_data)], type = "l")
  lines(real_data[order(real_data)], lower_limit[order(real_data)], type = "l", lty = 2)
  lines(real_data[order(real_data)], upper_limit[order(real_data)], type = "l", lty = 2)
}

plot_differences <- function(measured_value_1, measured_value_2) {
  plot(measured_value_2, measured_value_1)
  abline(0, 1)
}

plot_differences_wrt_data <- function(real_data, difference) {
  plot(real_data, difference)
  abline(0, 0)
}
