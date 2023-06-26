library(tidyverse)
library(haven)
library(stats)
install.packages("lsr")
library(lsr)

# Question 1
df <- read_sav("hourlywagedata.sav")
attributes(df$position)

t.test(hourwage ~ position, data = df, var.equal = TRUE)
cohensD(hourwage ~ position, data = df)

# Question 2
df1 <- read_sav("bankloan.sav")

t.test(df1$preddef1, df1$preddef2, paired = TRUE)

# Question 3
sf <- read_sav("satisfaction.sav")
attributes(sf$price)

cor(sf[, c("price", "numitems", "org", "service", "quality", "overall")], method = "spearman")

# Question 4
ed <- read_sav("employee_data.sav")
attributes(ed$jobcat)

library(car)

class(ed$jobcat)
ed$jobcat <- as.factor(ed$jobcat)

anova <- aov(ed$salary ~ ed$jobcat)
summary(anova)

TukeyHSD(anova)

leveneTest(anova)

# Question 5
model <- lm(salary ~ salbegin + jobtime, data = ed)
summary(model)

install.packages("QuantPsyc")
library(QuantPsyc)

lm.beta(model)

new <- data.frame(salbegin = c(50000), jobtime = c(60))

predict(model, newdata = new)

# Question 6
model1 <- lm(preddef1 ~ age + employ + debtinc + creddebt, data = df1)
summary(model1)

library(car)
vif(model1)
1/vif(model1)

hist(residuals(model1))
plot(model1, 2)

plot(model1, 3)

# Question 7
rearr <- read_sav("recidivism.sav")
attributes(rearr$gender)

tbl <- table(rearr$gender, rearr$arrest2)
chisq.test(tbl)
