# Load necessary library
library(ggplot2)

# Define the utility function
utility_function <- function(C, gamma) {
  if (gamma == 1) {
    return(log(C) + 4)  # Special case where gamma = 1
  } else {
    return((C^(1 - gamma)) / (1 - gamma) + 4)
  }
}

# Define the range of consumption values
C <- seq(0.1, 10, by = 0.1)  # Start from 0.1 to avoid division by zero

# Define the values of gamma
gamma_values <- c(0, 0.5, 1, 1.5, 2)
gamma_labels <- paste("Elasticity =", gamma_values)

# Create a data frame to store the results
utility_data <- data.frame(C = numeric(0), Utility = numeric(0), Gamma = factor())

# Calculate utility for each gamma value
for (gamma in gamma_values) {
  utility_values <- utility_function(C, gamma)
  temp_data <- data.frame(C = C, Utility = utility_values, Gamma = as.factor(gamma))
  utility_data <- rbind(utility_data, temp_data)
}

# Create a named vector for the gamma labels
gamma_labels_named <- setNames(gamma_labels, gamma_values)

# Plot the utility curves
ggplot(utility_data, aes(x = C, y = Utility)) +
  geom_line(size = 1, color = "black") +
  labs(x = "Consumption",
       y = "Utility / Well-being") +
  theme_bw() +
  theme(
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()
  ) +
  facet_wrap(~Gamma, scales = "free", labeller = labeller(Gamma = gamma_labels_named))

ggsave(plot = last_plot(), dpi = 300 ,filename="/Users/jeancjw/Documents/03_Results/Figures for paper/newfigures/utilityfn.png")


library(dplyr)
# Load necessary library
library(ggplot2)

# Define the utility function
utility_function <- function(C, gamma) {
  if (gamma == 1) {
    return(log(C) + 4)  # Special case where gamma = 1
  } else {
    return((C^(1 - gamma)) / (1 - gamma) + 4)
  }
}

# Define the range of consumption values
C <- seq(0.1, 10, by = 0.1)  # Start from 0.1 to avoid division by zero

# Define the specific value of gamma
gamma <- 1.5

# Calculate utility for the specific gamma value
utility_values <- utility_function(C, gamma)
utility_data <- data.frame(C = C, Utility = utility_values)

# Plot the utility curve
ggplot(utility_data, aes(x = C, y = Utility)) +
  geom_line(size = 1, color = "black") +
  labs(x = "Consumption",
       y = "Utility/Well-being") +
  theme_bw() +
  theme(
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()
  )

ggsave(plot = last_plot(), dpi = 300 , width = 5, height = 5, filename="/Users/jeancjw/Documents/03_Results/Figures for paper/newfigures/utilityfn-1_5.png")

