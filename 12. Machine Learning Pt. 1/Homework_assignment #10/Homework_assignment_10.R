library(tidyverse)
library(caret)
library(caTools)
library(haven)

# Question 1
loan <- read.csv("loan.csv")
set.seed(555)

spl <- sample.split(loan, SplitRatio = 0.6)

train <- subset(loan, spl == TRUE)
test <- subset(loan, spl == FALSE)

logit <- glm(as_factor(default) ~ income + debtinc + creddebt + othdebt, 
             data = train, family = "binomial")
summary(logit)

test$predict <- round(predict(logit, test, type = "response"))

confusionMatrix(as_factor(test$default), as_factor(test$predict))

# Question 2
set.seed(1234)

train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 10)

model_kfold <- train(as_factor(default) ~ income + debtinc + creddebt + othdebt,
                     data = loan,
                     trControl = train_control,
                     method = "glm",
                     family = "binomial")
print(model_kfold)

# Question 3
patient <- read_sav("patient_hw10.sav")
set.seed(5432)

train_control_loocv <- trainControl(method = "LOOCV")

model_loocv <- train(as.numeric(cost) ~ age + diabetes + choles,
                     data = patient,
                     trControl = train_control_loocv,
                     method = "lm")
print(model_loocv)

# Question 4
survey <- read.csv("survey.csv")

model <- lm(Mlifesat ~ Mposaff + Mnegaff + Mpstress + age, data = survey)
summary(model)

model_loess <- loess(Mlifesat ~ Mposaff + Mnegaff + Mpstress + age, data = survey)
summary(model_loess)

survey$lm_predict <- predict(model, survey)
survey$lo_predict <- predict(model_loess, survey)

c <- cor(survey$lo_predict, survey$Mlifesat, method = "pearson", use = "complete.obs")
R_square_loess <- c^2
R_square_loess 

# Question 5
# k = 1
knn_train_1 <- knn3(as_factor(default) ~ income + debtinc + creddebt + othdebt, 
                  data = train, k = 1)
test$y_hat_knn1 <- predict(knn_train_1, test, type = "class")
confusionMatrix(as_factor(test$y_hat_knn1), as_factor(test$default))

# k = 10
knn_train_10 <- knn3(as_factor(default) ~ income + debtinc + creddebt + othdebt, 
                    data = train, k = 10)
test$y_hat_knn10 <- predict(knn_train_10, test, type = "class")
confusionMatrix(as_factor(test$y_hat_knn10), as_factor(test$default))

# k = 20
knn_train_20 <- knn3(as_factor(default) ~ income + debtinc + creddebt + othdebt, 
                    data = train, k = 20)
test$y_hat_knn20 <- predict(knn_train_20, test, type = "class")
confusionMatrix(as_factor(test$y_hat_knn20), as_factor(test$default))




