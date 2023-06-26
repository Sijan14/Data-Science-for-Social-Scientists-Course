library(tidyverse)
library(caret)
titanic <- read.csv("titanic.csv")

logit <- glm(Survived ~ Sex + Age + Pclass, data = titanic, family = "binomial")
summary(logit)

install.packages("regclass")

library(regclass)
confusion_matrix(logit)

# creating predicted binary variable
titanic$psurvival <- predict(logit, titanic, type = "response")
titanic$pbinary <- ifelse(titanic$psurvival >= 0.50, 1, 0)
#or
titanic$predict <- round(predict(logit, titanic, type = "response"))

confusionMatrix(data = as_factor(titanic$Survived), reference = as_factor(titanic$pbinary))

library(pROC)
roc <-  roc(titanic$Survived ~ titanic$pbinary, plot = TRUE, print.auc = TRUE)
as.numeric(roc$auc)

# Holdout method
install.packages("caTools", dependencies = TRUE)
install.packages("rlang")
library(rlang)
library(caTools)
set.seed(123)

spl <-  sample.split(titanic, SplitRatio = 0.7)

train <- subset(titanic, spl == TRUE)
test <- subset(titanic, spl == FALSE)

logit_train <- glm(Survived ~ Sex + Age + Pclass, data = train, family = "binomial")
test$predict <- round(predict(logit_train, test, type = "response"))

confusionMatrix(as_factor(test$Survived), as_factor(test$predict))

### binary logistic regression model
### repeated k-fold cross-validation
# define training control
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

# train the model on traininng set
model_kfold <- train(as_factor(Survived) ~ Sex + Age + Pclass, 
                     data = titanic, 
                     trControl = train_control, 
                     method = "glm",
                     family = "binomial")
print(model_kfold)

### LOOCV k-fold cross-validation
# define training control
train_control <- trainControl(method = "LOOCV", number = 5)

# train the model on traininng set
model_kfold <- train(as_factor(Survived) ~ Sex + Age + Pclass, 
                     data = titanic, 
                     trControl = train_control, 
                     method = "glm",
                     family = "binomial")
print(model_kfold)

### linear regression model
### repeated k-fold cross-validation
# define training control
train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

# train the model on traininng set
model_kfold_lm <- train(Fare ~ Sex + Age + Pclass, 
                        data = titanic, 
                        trControl = train_control, 
                        method = "lm")
print(model_kfold_lm)

