# Set seed for reproducibility
set.seed(123)

# Number of subjects
N <- 1000

# Simulate treatment assignment for period 1
Z1 <- rbinom(N, 1, 0.5)        # Random assignment to treatment (1) or control (0)
Z2 <- rbinom(N, 1, 0.5)        # Re-randomization

# Simulate covariates
X1 <- rnorm(N)                # Covariate at period 1
X2 <- rnorm(N)                # Covariate at period 2 (could be correlated with X1)
gap <- runif(N, 1, 5)          # Time gap between periods (for decay model)

# Simulate period 1 outcome
alpha0 <- 0; alpha1 <- 1; alpha2 <- 1
sigma1 <- 1
Y1 <- alpha0 + alpha1 * Z1 + alpha2 * X1 + rnorm(N, 0, sigma1)

# Parameters for period 2 outcome models
beta0 <- 0; beta1 <- 1; beta2 <- 1
gamma <- 1.5; delta <- 1; theta <- 0.5; lambda <- 0.5
sigma2 <- 1

# Model 1: Baseline (No Carryover)
Y2_baseline <- beta0 + beta1 * Z2 + beta2 * X2 + rnorm(N, 0, sigma2)

# Model 2: Fixed-Effect Carryover
Y2_fixed <- beta0 + beta1 * Z2 + gamma * Z1 + beta2 * X2 + rnorm(N, 0, sigma2)

# Model 3: Interactive Carryover Model
Y2_interact <- beta0 + beta1 * Z2 + gamma * Z1 + delta * (Z1 * Z2) + beta2 * X2 + rnorm(N, 0, sigma2)

# Model 4: Lagged Outcome (Dynamic) Model
Y2_lag <- beta0 + beta1 * Z2 + gamma * Z1 + theta * Y1 + beta2 * X2 + rnorm(N, 0, sigma2)

# Model 5: Moderated Carryover Model
Y2_mod <- beta0 + beta1 * Z2 + gamma * Z1 + delta * (Z1 * X1) + beta2 * X2 + rnorm(N, 0, sigma2)

# Model 6: Decay Model
Y2_decay <- beta0 + beta1 * Z2 + gamma * exp(-lambda * gap) * Z1 + beta2 * X2 + rnorm(N, 0, sigma2)

# Combine all variables into a data frame
sim_data <- data.frame(ID = 1:N, Z1, Z2, X1, X2, gap, Y1, 
                       Y2_baseline, Y2_fixed, Y2_interact, Y2_lag, Y2_mod, Y2_decay)

summary(lm(Y2_baseline ~ Z1 + Z2 + X2, sim_data))

summary(lm(Y2_fixed ~ Z1 + Z2 + X2, sim_data))

summary(lm(Y2_interact ~ Z1 + Z2 + X2, sim_data))

summary(lm(Y2_lag ~ Z1 + Z2 + X2, sim_data))

summary(lm(Y2_mod ~ Z1 + Z2 + X2, sim_data))

summary(lm(Y2_decay ~ Z1 + Z2 + X2, sim_data))
