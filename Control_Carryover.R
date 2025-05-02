# -------------------------------------------------------------
# Author: Justin Ho
#
# Description:
#   Simulates two-period cross-over experiments under different
#   carryover effect structures, fits linear models, and
#   visualizes how estimated treatment effects vary with carryover
#   magnitude and model specification.
#
#   - Model 1: Interactive carryover between periods
#   - Model 2: Compounding single-effect carryover
#
#   Results are aggregated over multiple simulation runs, then
#   plotted with error bars to show min, max, and mean estimates.
#
# -------------------------------------------------------------


rm(list = ls())

library(tools)
library(tidyverse)
library(stringr)

# Number of subjects
N <- 100

simulation <- function(param) {

  # Simulate treatment assignment for period 1
  
  Z1 <- rbinom(N, 1, 0.5)        # Random assignment to treatment (1) or control (0)
  Z2 <- rbinom(N, 1, 0.5)        # Re-randomization
  
  # Simulate covariates
  
  X1 <- rnorm(N)                # Covariate at period 1
  X2 <- X1 + rnorm(N)           # Covariate at period 2 (could be correlated with X1)
  gap <- runif(N, 1, 5)          # Time gap between periods (for decay model)
  
  # Simulate period 1 outcome
  alpha0 <- 2; alpha1 <- 5; alpha2 <- 5
  sigma1 <- 1
  
  Y1 <- alpha0 + alpha1 * Z1 + alpha2 * X1 + rnorm(N, 0, sigma1)
  
  # Parameters for period 2 outcome models
  beta0 <- 2; beta1 <- 5; beta2 <- 5
  sigma2 <- 1
  
  
  # Model 1: Interactive Carryover Model
  Y2_interact <- beta0 + beta1 * Z2 + param * (Z1 * Z2) + beta2 * X2 + rnorm(N, 0, sigma2)
  
  # Model 2: Compounding Effect Single Effect
  Y2_compound <- beta0 + beta1^(1+ param*Z1)*(Z2) + beta2 * X2 + rnorm(N, 0, sigma2)
  
  interact <- summary(lm(Y2_interact  ~ Z2 + X2))$coefficients[2,1]
  interact_fixed <- summary(lm(Y2_interact ~ Z2 + Z1 + X2))$coefficients[2,1]
  
  compound <- summary(lm(Y2_compound  ~ Z2 + X2))$coefficients[2,1]
  compound_fixed <- summary(lm(Y2_compound ~ Z2 + Z1 + X2))$coefficients[2,1]
  
  
  return( c(interact, interact_fixed, compound, compound_fixed))

}

## Carryover parameters to evaluate
parameters <- seq(-5, 5, length.out = 100)

# Run first batch of simulations and assemble results
d <- as.data.frame(do.call(rbind, lapply(parameters, function(i) simulation(i))))

colnames(d) <- c("interaction", "interaction_fixed", "compound", "compound_fixed")
d$parameters <- parameters
d$simulation <- 1


# Repeat simulations for 100 runs to capture variability

n_runs <- 100

for (i in 2:n_runs) {
  d1 <- as.data.frame(do.call(rbind, lapply(parameters, function(i) simulation(i))))
  
  
  d1$parameters <- parameters
  d1$simulation <- i
  
  colnames(d1) <- colnames(d)
  d <- rbind(d, d1)
  
}

##===============================================================
## Simulation Visualization
##===============================================================

# Gather estimates into long format, compute mean, min, max
d |>
  gather(key = "Type", value = "Estimate", 1:4) |>
  group_by(Type, parameters) |>
  summarize( Mean = mean(Estimate),
             Max = max(Estimate),
             Min = min(Estimate) ) |>
  mutate(
    Estimation = ifelse(grepl("_", Type), "No Control", "Fixed Effect"),
    Structure = toTitleCase(sub("\\_.*", "", Type))
  )  -> d1

# Plot simulated results
ggplot(data = d1, aes(x=parameters, y=Mean, color=Estimation)) +
  geom_point(size = 0.2) + 
  geom_line(aes(y = 5), col = "red") +
  geom_errorbar(aes(ymin=Min, ymax=Max), position = "dodge", 
                size=.1, color="black", width=.1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12), 
        axis.title=element_text(size=10,face="bold"), legend.position = "top") +
  xlab("Magnitude of Carryover Effect") +
  ylab("Estimate") +
  facet_wrap(~ Structure, scale = "free_y") 

ggsave("./Figures/CarryoverEffect.png", device = "png", width = 8, height = 4)
