# Question 2
library(haven)
df <- read_sav("patient.sav")

model <- glm(doa ~ age + gender + choles, data = df, family = binomial)
summary(model)

exp(coef(model))

# Question 3
survey <- read_csv("survey.csv")

survey$Mposaff_cent <- survey$Mposaff - mean(survey$Mposaff, na.rm = TRUE)
survey$Mnegaff_cent <- survey$Mnegaff - mean(survey$Mnegaff, na.rm = TRUE)

model1 <- lm(Mlifesat ~ Mposaff + Mnegaff + Mposaff * Mnegaff, data = survey)
summary(model1)

model2 <- lm(Mlifesat ~ Mposaff_cent + Mnegaff_cent + Mposaff_cent * Mnegaff_cent, data = survey)
summary(model2)

library(car)
vif(model1)
vif(model2)

# Question 4
model3 <- lm(Mslfest ~ Mpstress + age + Mpstress * age, data = survey)
summary(model3)

library(interactions)
sim_slopes(model3, pred = Mpstress, modx = age, johnson_neyman = FALSE)
interact_plot(model3, pred = Mpstress, modx = age)

# Question 5
pupil <- read_sav("pupil.sav")

intercept_only <- lmer(achiev ~ (1|pschool), data = pupil)
summary(intercept_only)

ICC <- 0.1756 / (0.5771 + 0.1756)

mlm_model <- lmer(achiev ~ pupsex + pupses + (1|pschool), data = pupil)
summary(mlm_model)
