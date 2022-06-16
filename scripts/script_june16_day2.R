library(tidyverse)

student_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/msms03/main/data/student.csv")

M12 <- lm(math ~ ., data = student_df)

M12_step_back <- step(M12, direction = 'backward')
AIC(M12) # notice how this is different to the reported AIC in step
