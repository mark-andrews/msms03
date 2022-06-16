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
