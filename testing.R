library(dplyr)
library(blandr)
source("functions.R")
source("bland-altman_no_proportional_bias.R")
source("bland-altman_with_proportional_bias.R")

vtas_range <- c(0, 5)
vpps_range <- c(1, 5)

reliabilitydata <- read_csv(
  "Brogan Barr_BEP and CCT _reliabilities_Feb2021.csv"
)

plot_therapist_factor <- plot_reliability_no_bias(
  reliabilitydata$VTAS_TherFac.1,
  reliabilitydata$VTAS_TherFac.2,
  "VTAS Therapist Factor Mean Score",
  "VTAS Therapist Factor Difference",
  vtas_range,
  "Therapist Factor"
)


plot_paitent_dependency_quadratic <- plot_reliability(
  reliabilitydata$VTAS_TherFac.1,
  reliabilitydata$VTAS_TherFac.2,
  "VTAS Therapist Factor Mean Score",
  "VTAS Therapist Factor Difference",
  vtas_range,
  "Therapist Factor"
)

x1 <- reliabilitydata$VTAS_TherFac.1
x2 <- reliabilitydata$VTAS_TherFac.2

data <- data.frame(x1 = x1, x2 = x2)

data <- data %>%
  mutate(mean_diff = 0.5 * (x1 + x2)) %>%
  group_by(mean_diff) %>%
  mutate(
    count = length(x1)
  ) %>%
  ungroup()




