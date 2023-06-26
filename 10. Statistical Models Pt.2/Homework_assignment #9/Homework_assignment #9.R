library(tidyverse)
library(haven)
library(confintr)
library(lmerTest)

# Question 1
loan <- read_sav("bankloan.sav")

names(loan)
model <- glm(default ~ age + employ + income + creddebt, data = loan, family = "binomial")
summary(model)

exp(cbind(OR = coef(model), confint.default(model)))

# Question 2
tita <- read_csv("titanic.csv")

model1 <- glm(Survived ~ Sex + Age + Pclass, data = tita, family = "binomial")
summary(model1)

new <- data.frame(Age = c(40), Sex = c(0, 0, 0, 1, 1, 1), Pclass = c(1, 2, 3, 1, 2, 3))

predict(model1, newdata = new)

# Question 3
survey <- read_csv("survey.csv")

survey$Mnegaff_cent <- survey$Mnegaff - mean(survey$Mnegaff, na.rm = TRUE)

model2 <- lm(Moptim ~ Mnegaff_cent + sex + Mnegaff_cent * sex, data = survey)
summary(model2)

library(car)
vif(model2)

library(interactions)
sim_slopes(model2, pred = Mnegaff_cent, modx = sex, johnson_neyman = FALSE)
interact_plot(model2, pred = Mnegaff_cent, modx = sex)

# Question 4
pop <- read_sav("popular2.sav")

library(lme4)

intercept_only <- lmer(popular ~ (1|class), data = pop) 
summary(intercept_only)

0.7021 / (0.7021 + 1.2218)

mlm_model <- lmer(popular ~ extrav + (1|class), data = pop)
summary(mlm_model)
options(scipen = 999)

# Question 5
gpa <- read_sav("gpa2long.sav")
attributes(gpa$job)

mlm_model1 <- lmer(gpa ~ sex + as.factor(job) + (1|student), data = gpa)
summary(mlm_model1)
