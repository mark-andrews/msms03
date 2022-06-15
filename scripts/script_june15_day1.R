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

