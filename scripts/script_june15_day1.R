library(tidyverse)

y <- cars$dist
M1 <- lm(y ~ 1) # iid normal model
mu_hat <- coef(M1)
sigma_hat <- sigma(M1)
sigma_mle <- sqrt(mean(residuals(M1)^2))
# the log of the probability of each observation
dnorm(y, mean = mu_hat, sd = sigma_hat, log = TRUE)

# sum of the log of the probability of each observation
# aka model's log likelihood
sum(dnorm(y, mean = mu_hat, sd = sigma_hat, log = TRUE))

logLik(M1)



# using mle
sum(dnorm(y, mean = mu_hat, sd = sigma_mle, log = TRUE))


# Regression model of y ---------------------------------------------------

x <- cars$speed
M2 <- lm(y ~ x) # normal linear model, aka simple linear regression

logLik(M2)


-206 - -232
exp(26)

# RSS of M2
sum(residuals(M2)^2)
# RSS of M1
sum(residuals(M1)^2)



# Root mean square error --------------------------------------------------

sqrt(mean(residuals(M1)^2))
sqrt(mean(residuals(M2)^2))



# Logistic regresion and deviance -----------------------------------------

cars_df <- mutate(cars, z = dist > median(dist))

# constant probability of being above average distance
M3 <- glm(z ~ 1, 
          family = binomial(link = 'logit'),
          data = cars_df)

# prob of being above average dist is function of speed
M4 <- glm(z ~ speed, 
          family = binomial(link = 'logit'),
          data = cars_df)

logLik(M3) # the probability of observing outcome given M3 (and mle's of unknowns)
logLik(M4) # the probability of observing outcome given M4 (and mle's of unknowns)

logLik(M4) - logLik(M3) # log of the likelihod ratio

-2 * logLik(M3)
-2 * logLik(M4)


# Nested model comparisons ------------------------------------------------

M5 <- lm(Fertility ~ Agriculture + Education, data = swiss)
M6 <- lm(Fertility ~ Agriculture + Education + Catholic, data = swiss)

RSS_5 <- sum(residuals(M5)^2)
RSS_6 <- sum(residuals(M6)^2)

RSS_5
RSS_6

pie <- (RSS_5 - RSS_6) / RSS_6

# Null hypothesis test on model fits of nested models
anova(M5, M6)

# F statistic
(RSS_5 - RSS_6) / (RSS_6/M6$df.residual)

# likelihood ratio
logLik(M6) - logLik(M5)


# drop1 nested model comparison -------------------------------------------

drop1(M6, scope = ~ Catholic, test = 'F')
drop1(M6, scope = ~ Agriculture, test = 'F')
drop1(M6, scope = ~ Education + Agriculture, test = 'F')

sum(residuals(lm(Fertility ~ Agriculture + Catholic, data = swiss))^2)
sum(residuals(lm(Fertility ~ Education + Catholic, data = swiss))^2)


# Sequential anova: Type I sums of squares --------------------------------

anova(M6)
sum(residuals(lm(Fertility ~ 1, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture, data = swiss))^2)
sum(residuals(lm(Fertility ~ Agriculture, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture + Education, data = swiss))^2)
sum(residuals(lm(Fertility ~ Education + Agriculture, data = swiss))^2) - sum(residuals(lm(Fertility ~ Agriculture + Education + Catholic, data = swiss))^2)
sum(residuals(lm(Fertility ~ Agriculture + Education + Catholic, data = swiss))^2)

M6 <- lm(Fertility ~ Agriculture + Education + Catholic, data = swiss)
M6a <- lm(Fertility ~ Catholic + Agriculture + Education, data = swiss)
anova(M6a)

# proportion decrease in error --------------------------------------------

(RSS_5 - RSS_6) / RSS_5
M7 <- lm(Fertility ~ 1, data = swiss) # null model
RSS_7 <- sum(residuals(M7)^2)

(RSS_7 - RSS_6) / RSS_7 # R^2 of M6

anova(M7, M6)



# Nested model comparison in GLMs -----------------------------------------

swiss_df <- mutate(swiss, z = Fertility > median(Fertility))

M8 <- glm(z ~ Agriculture + Education, 
          family = binomial(link = 'logit'),
          data = swiss_df)

M9 <- glm(z ~ Agriculture + Education + Catholic, 
          family = binomial(link = 'logit'),
          data = swiss_df)

logLik(M8)
logLik(M9)
logLik(M9) - logLik(M8)

deviance(M8)
deviance(M9)
deviance(M8) - deviance(M9)

# deviance comparison in glm's using Chi sq, Wilks's theorem 
anova(M8, M9, test = 'Chisq')

# p-value manually calculated
pchisq(deviance(M8) - deviance(M9), df =1, lower.tail = F)



# Get some utils ----------------------------------------------------------

source("https://raw.githubusercontent.com/mark-andrews/msms03/main/utils/utils3.R")

# loo cross validation  ---------------------------------------------------


# read in housing data
housing_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/msms03/main/data/housing.csv")
housing_df <- mutate(housing_df, logprice = log(price))
ggplot(housing_df, aes(x = price)) + geom_histogram(bins=50)

# normal model
M10 <- lm(price ~ 1, data = housing_df)
# log normal model
M11 <- lm(logprice ~ 1, data = housing_df)

lm_loo_cv(M10)
lm_loo_cv(M11)

# deviance scale
lm_loo_cv(M10, deviance_scale = TRUE) # -2 * elpd of M10
lm_loo_cv(M11, deviance_scale = TRUE) # -2 * elpd of M11

lm_loo_cv(M10, deviance_scale = TRUE) - lm_loo_cv(M11, deviance_scale = TRUE)


# AIC ---------------------------------------------------------------------

logLik(M10)      # log likelihood 
-2 * logLik(M10) # deviance (-2 x log likelihood)
2 * 2 - 2 * logLik(M10) # AIC: 2K + deviance
AIC(M10) # using built in AIC

# AIC of M11
AIC(M11)

# estimate standard errors for elpd
lm_loo_cv(M10, deviance_scale = TRUE, se = TRUE)
lm_loo_cv(M11, deviance_scale = TRUE, se = TRUE)


# Small sample correction -------------------------------------------------

AIC(M10)
AICc(M10)

AIC(M11)
AICc(M11)
