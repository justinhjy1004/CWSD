library(haven)
library(tidyverse)

data <- read_dta("study2data.dta")

no_pretreat <- data[data$pretreat == 0,] 
lmod1 <- summary(lm(aiddv ~ treat + ideo, no_pretreat))

pretreat <- data[data$pretreat == 1,]
lmod2 <- summary(lm(aiddv ~ treat + ideo + aid1, pretreat))

design <- c("Between-Subjects", "Pre-Post")
estimates <- c(lmod1$coefficients[['treat', "Estimate"]], lmod2$coefficients[['treat', "Estimate"]]) 
stderror <-  c(lmod1$coefficients[['treat', "Std. Error"]], lmod2$coefficients[['treat', "Std. Error"]]) 

d <- tibble(design = design, estimates = estimates, se = stderror)

se_width <- 1.96
ggplot(data = d, aes(y=estimates, x = design)) +
  geom_point(position=position_dodge(width = 1), size=1, color="black") +
  geom_errorbar(aes(ymin=estimates-(se*se_width), ymax=estimates+(se*se_width)), position = position_dodge(width=.6), 
                size=.25, color="black", width=.15)  +
  theme_classic() +
  ylab("Estimate") +
  xlab("Design")

ggsave("estimates.jpg", device = "jpg",width = 5.3, height = 3.7, units = "in")
