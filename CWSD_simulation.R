# -----------------------------------------------------------------------------
# Author: Justin Ho
#
# This script simulates and compares the performance of various estimation 
# strategies in two experimental designs: 
# (1) Counterbalanced Within-Subjects Design (CWSD) 
# (2) Sequential Randomization.
# 
# For each design, it evaluates five estimation strategies:
# - No control
# - Direct control for covariates
# - Propensity score adjustment
# - Misspecification of covariates
# - Fixed effects
#
# The simulation varies the effect size of treatment on covariates and estimates 
# treatment effects across num_times (100 is default) replications per level. 
# Results are visualized 
# to show bias and robustness of each strategy under different conditions.
#
# Output:
# - A plot saved to ./Figures/Design_Comparison.png showing estimated effects 
#   with error bars across the range of covariate effects, by design and strategy.
# -----------------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

## Function to simulate the two designs
simulate_cwsd <- function(eff_size_covariate, control=c("no control", "direct control", "propensity score", "misspecification", "fixed effects"), rerandomize = FALSE){
  
  N <- 1000
  eff_size <- 1
  
  # Data Generation at t1
  e_t1 <- rnorm(N) ## error
  X_t1 <- rnorm(N) ## covariates
  Z_t1 <- rbinom(N, 1, 0.5) ## treatment
  
  Y_t1 <- eff_size*Z_t1 + X_t1 + e_t1 ## outcome
  
  # Data Generation at t2
  e_t2 <- rnorm(N) ## error
  Z_t2 <- ifelse(Z_t1 == 1, 0, 1) ## treatment
  
  if (rerandomize) Z_t2 <- rbinom(N, 1, 0.5)
  
  X_t2 <- X_t1 + eff_size_covariate*Z_t1 ## covariates
  
  Y_t2 <- eff_size*Z_t2 + X_t2 + e_t2 ## outcome
  
  ## Stacking t1 and t2
  Y <- c(Y_t1, Y_t2)
  Z <- c(Z_t1, Z_t2)
  X <- c(X_t1, X_t2)
  
  if(control == "direct control"){
    with_ctrl <- lm(Y ~ Z + X)$coefficients[["Z"]]
    return(with_ctrl)
  } else if(control == "no control") {
    no_ctrl <- lm(Y ~ Z)$coefficients[["Z"]]
    return(no_ctrl)
  } else if(control == "propensity score"){
    score <- glm(Z ~ X, family = "binomial")$fitted.values
    prop_ctrl <- lm(Y ~ Z + score)$coefficients[["Z"]]
  } else if(control == "misspecification"){
    U <- rnorm(length(X))
    wrong_ctrl <- lm(Y ~ Z + U)$coefficients[["Z"]]
    return(wrong_ctrl)
  } else if(control == "fixed effects"){
    t <- as.factor(c(rep(0, N), rep(1, N)))
    fixed_eff <- lm(Y ~ Z + t)$coefficients[["Z"]]
    return(fixed_eff)
  }
  
}

## Number of replications
num_times <- 100

## Grids of effect size
eff_size_cov <- rep(seq(-1000,1000,20)/100, num_times)

##============================================================
## Simulation of CWSD with different estimation strategies
##============================================================

## Simulation with different estimation strategies in CWSD
no_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "no control"))
with_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "direct control"))
prop_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "propensity score"))
fixed_eff <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "fixed effects"))
wrong_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "misspecification"))

## Combining to a tibble
df_cwsd <- tibble(
  eff_size_cov = eff_size_cov,
  `No Control` = no_ctrl, 
  `Correct Specification` = with_ctrl,
  `Propensity Score` = prop_ctrl,
  `Fixed Effects` = fixed_eff,
  Misspecification = wrong_ctrl,
  Design = "Counterbalanced Within-Subjects"
)

##=====================================================================
## Simulation of Seq Randomization with different estimation strategies
##=====================================================================

no_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "no control", rerandomize = T))
with_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "direct control", rerandomize = T))
prop_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "propensity score", rerandomize = T))
fixed_eff <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "fixed effects", rerandomize = T))
wrong_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "misspecification", rerandomize = T))

## Combining to a tibble
df_rerandom <- tibble(
  eff_size_cov = eff_size_cov,
  `No Control` = no_ctrl, 
  `Correct Specification` = with_ctrl,
  `Propensity Score` = prop_ctrl,
  `Fixed Effects` = fixed_eff,
  Misspecification = wrong_ctrl,
  Design = "Sequential Randomization"
)

## Combine both CWSD and Sequential Randomization
df <- rbind(df_cwsd, df_rerandom)

##====================================================
##        Simulation Visualization
##====================================================

df |>
  gather(key = "Control", value = "estimate", 2:(length(colnames(df)) - 1)) |>
  group_by(eff_size_cov, Control, Design) |>
  summarize(
    expected_estimate = mean(estimate),
    ub_estimate = max(estimate),
    lb_estimate = min(estimate)
  ) -> df_plot

ggplot(data = df_plot, aes(x=eff_size_cov, y=expected_estimate, color=Control)) +
  geom_point(size = 0.2) + 
  geom_errorbar(aes(ymin=lb_estimate, ymax=ub_estimate, color=Control), position = "dodge", 
                size=.1, color="black", width=.1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12), 
        axis.title=element_text(size=10,face="bold"), legend.position = "top") +
  xlab("Effect Size on Covariates") +
  ylab("Estimated Effect") +
  facet_wrap(~ Design)

ggsave("./Figures/Design_Comparison.png", device = "png", width = 8, height = 4)
