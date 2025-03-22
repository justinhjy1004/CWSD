library(tidyverse)

rm(list = ls())

simulate_cwsd <- function(eff_size_covariate, control=c("no control", "direct control", "propensity score", "misspecification", "fixed effects")){
  
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

num_times <- 100
eff_size_cov <- rep(seq(-1000,1000,20)/100, num_times)
no_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "no control"))
with_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "direct control"))
#prop_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "propensity score"))
fixed_eff <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "fixed effects"))
#wrong_ctrl <- sapply(eff_size_cov, function(i) simulate_cwsd(i, "misspecification"))

df <- tibble(
  eff_size_cov = eff_size_cov,
  `No Control` = no_ctrl, 
  `Correct Specification` = with_ctrl,
  #`Propensity Score` = prop_ctrl,
  `Fixed Effects` = fixed_eff
  #Misspecification = wrong_ctrl
)

df |>
  gather(key = "Control", value = "estimate", 2:length(colnames(df))) |>
  group_by(eff_size_cov, Control) |>
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
  ylab("Estimated Effect")
  

ggplot(data = df_plot[df_plot$Control != "No Control", ], aes(x=eff_size_cov, y=expected_estimate, color=Control)) +
  geom_point(size = 0.2) + 
  geom_errorbar(aes(ymin=lb_estimate, ymax=ub_estimate, color=Control), position = "dodge", 
                size=.1, color="black", width=.1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=12), 
        axis.title=element_text(size=10,face="bold"), legend.position = "top") +
  xlab("Effect Size on Covariates") +
  ylab("Estimated Effect")
  
  
 