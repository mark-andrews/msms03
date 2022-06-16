library(tidyverse)

student_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/msms03/main/data/student.csv")

M12 <- lm(math ~ ., data = student_df)

M12_step_back <- step(M12, direction = 'backward')
AIC(M12) # notice how this is different to the reported AIC in step

M13 <- lm(math ~ 1, data = student_df)
M13_step_forward <- step(M13, direction = 'forward', scope = formula(M12))

M14_step_both <- step(M12, direction = 'both')
M14_step_both

M15_step_both <- step(M13, direction = 'both', scope = formula(M12))

# all subsets regression
library(MuMIn)

M16 <- lm(Fertility ~ ., data = swiss, na.action = 'na.fail')
M16_all_subsets <- dredge(M16)

conf_set <- get.models(M16_all_subsets, cumsum(weight) < 0.95)


# coefs versus Anova ------------------------------------------------------

round(car::Anova(M16), 3)
round(summary(M16)$coefficients, 3)

M17 <- lm(weight ~ group, data = PlantGrowth)
round(car::Anova(M17), 3)
round(summary(M17)$coefficients, 3)



# Lasso, ridge, elastic nets ----------------------------------------------

library(glmnet)

# lasso
y <- student_df$math
X <- as.matrix(select(student_df, -math))

M18_lasso <- glmnet(X,y, alpha = 1)
plot(M18_lasso, xvar = 'lambda', label = TRUE)

M18_lasso_cv <- cv.glmnet(X, y, alpha = 1)
plot(M18_lasso_cv)

coef(M18_lasso, 
     s = c(M18_lasso_cv$lambda.min,
           M18_lasso_cv$lambda.1se))

#  ridge
M19_ridge <- glmnet(X,y, alpha = 0)
plot(M19_ridge, xvar = 'lambda', label = TRUE)

M19_ridge_cv <- cv.glmnet(X, y, alpha = 0)
plot(M19_ridge_cv)

coef(M19_ridge, 
     s = c(M19_ridge_cv$lambda.min,
           M19_ridge_cv$lambda.1se))



# Bayesian regularized variable selection ---------------------------------

student_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/msms03/main/data/student_scaled.csv")

M20 <- brm(math ~ .,
           data = student_df,
           prior = set_prior("horseshoe(df=3)"))

library(bayesplot)
mcmc_areas(M20, 
           pars = vars(-c("b_Intercept", "sigma", "lprior", 'lp__')),
           prob = 0.95)


# More Bayesian model selection -------------------------------------------

sleep_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/msms03/main/data/sleepstudy.csv")

ggplot(sleep_df,
       aes(x = Days, y = Reaction, colour = Subject)) + 
  geom_point() + 
  stat_smooth(method ='lm' , se = F) + 
  facet_wrap(~Subject)


# Bayesian Linear Regression
M21 <- brm(Reaction ~ Days, data = sleep_df)

# normal linear mixed effects
M22 <- brm(Reaction ~ Days + (Days|Subject), data = sleep_df)

# robust linear mixed effects
M23 <- brm(Reaction ~ Days + (Days|Subject), 
           family = student(),
           data = sleep_df)

waic(M21)
waic(M22)
waic(M23)

loo_compare(waic(M21), waic(M22), waic(M23))
