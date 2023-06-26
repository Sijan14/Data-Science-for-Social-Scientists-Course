df <- read_csv("survey.csv")

model1 <- lm(Moptim ~ sex + age + sex*age, data = df)
summary(model1)

model2 <- lm(Moptim ~ sex*age, data = df) # the interaction effect includes the main effect
summary(model2)

install.packages("interactions", dep = TRUE)
library(interactions)

interact_plot(model2, pred = age, modx = sex)

interact_plot(model2, pred = sex, modx = age)

sim_slopes(model2, pred = age, modx = sex, jonshon_neyman = FALSE)

# MLM in R
library(haven)
nurses <- read_sav("nurses.sav")

library(lme4)
intercept_only_model <- lmer(stress ~ (1|hospital), data = nurses)
summary(intercept_only_model)

mlm_model <- lmer(stress ~ age + gender + experien + (1|hospital), data = nurses)
summary(mlm_model)

mlm_model2 <- lmer(stress ~ age + gender + experien + as.factor(hospsize) + (1|hospital), data = nurses)
summary(mlm_model2)

library(lmerTest)
summary(mlm_model2)
