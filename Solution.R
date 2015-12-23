#' ---------------------------------------------------------------
#' @version 1.1.0 Poster Competition Data Analysis
#' @title census
#' 
#' @description 
#' This script is used to analyze the Poster Competition data. 
#' 
#' @author Vijayan Nagarajan
#' ---------------------------------------------------------------

#Libraries
library(randomForest)

#Read data
posterData <- read.csv("D:\\Poster Competition\\PosterData.csv")
colnames(posterData) <- c('Order', 'Quantity', 'Delivery')
str(posterData)
summary(posterData)
plot(posterData$Quantity, posterData$Delivery)
with(posterData, plot(Delivery ~ Quantity))
with(posterData, cor(Quantity, Delivery))
with(posterData, cor(Quantity, Delivery))^2

#Multiple Linear Regression
fit <- lm(Delivery ~ Quantity, data=posterData)
summary(fit)

#Plot graphs
layout(matrix(c(1,2,3,4),2,2))
plot(fit)
anova(fit)

randomSample <- sample(1:100, 75)
posterData$Order <- NULL 

#Random Forest
trainTempData <- posterData[-1,]
train <- trainTempData[1:75,]
test <- trainTempData[c(-1:-75),]
set.seed(14)
fit <- randomForest( train$Delivery ~ train$Delivery )
predict <- predict(fit, test, type="class")


#Second Method
fit <- train(Delivery~., data = train, method = 'rpart')
train.caret <- predict(fit, newdata = train)
table(train.caret, posterData$)
