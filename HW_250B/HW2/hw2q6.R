setwd("~/Desktop/UCLA_Study/Bio250B/HW2")
rm(list=ls())

library(haven)
library(broom)
library(tidyverse)
theme_set(theme_classic())
data <- read_dta("arsenic.dta")

mod <- lm(arsnails~., data=data)
summary(mod)
plot(mod)

model.diag.metrics <- augment(mod)

# Cook's distance
plot(mod, 4)
# Residuals vs Leverage
plot(mod, 5)
model.diag.metrics %>%
  top_n(3, wt = .cooksd)

library(car)
outlierTest(mod)
