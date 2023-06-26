library(tidyverse)
library(caret)
library(regclass)

# Question 1
df <- read.csv("binary.csv")

logit <- glm(admit ~ gpa + gre + rank, data = df, family = "binomial")
summary(logit)

df$predict <- round(predict(logit, df, type = "response"))

confusionMatrix(as_factor(df$admit), as_factor(df$predict))

# Question 2
library(pROC)
roc <-  roc(df$admit ~ df$predict, plot = TRUE, print.auc = TRUE)
as.numeric(roc$auc)

# Question 3
set.seed(257)
library(caTools)

spl <-  sample.split(df, SplitRatio = 0.7)

binary_train <- subset(df, spl == TRUE)
binary_test <- subset(df, spl == FALSE)

logit1 <- glm(admit ~ gpa + gre + rank, data = binary_train, family = "binomial")
binary_test$predict <- round(predict(logit1, binary_test, type = "response"))

confusionMatrix(as_factor(binary_test$admit), as_factor(binary_test$predict))

# Question 4
set.seed(717)

train_control <- trainControl(method = "repeatedcv",  number = 5, repeats = 5)

model_kfold <- train(as_factor(admit) ~ gpa + gre + rank, 
                     data = df, 
                     trControl = train_control, 
                     method = "glm",
                     family = "binomial")
print(model_kfold)

# Question 5
set.seed(500)

train_control1 <- trainControl(method = "repeatedcv",  number = 10, repeats = 5)

model_kfold_lm <- train(gre ~ gpa + rank, 
                        data = df, 
                        trControl = train_control1, 
                        method = "lm")
print(model_kfold_lm)
