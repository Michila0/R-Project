library(readxl)
library(tidyverse)
library(neuralnet)
library(ggplot2)
library(MLmetrics)
library(Metrics)
#data read
data <- read_excel("C:/Users/ASUS/Desktop/ML/uow_consumption.xlsx")
data = data[, 4]#select the 4th column
head(data)
#B
#making time-delays
delay1 = lag(data, 1)
delay2 = lag(data, 2)
delay3 = lag(data, 3)
delay4 = lag(data, 4)

data <- cbind(data, delay1, delay2, delay3, delay4)
head(data)
data <- na.omit(data)#remove the NA rows
sum(is.na(data))
head(data)

colnames(data) <- c("Original", "Delay_1", "Delay_2", "Delay_3", "Delay_4")#rename of the current table columns
head(data)
scaled_data <- scale(data)#scale the data
#C
normalizer <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalized_data <- normalizer(scaled_data)

train_data <- normalized_data[1:380, ]
#head(train_data)
test_data <- normalized_data[381:length(data), ]
#head(test_data)
x_test <- test_data[, -1] # x_test is other columns ex: delay1, delay2,..
y_test <- test_data[, 1] # y_test is original column
head(x_test)
head(y_test)

forecast <- function (expected, mlp_test) {
  
  expectedTest <- cbind (expected, as.data.frame(mlp_test$net.result))
  
  colnames(expectedTest) <- c("Expected Output", "Model Output")
  
  return(expectedTest)
}

#D
#Model 1
r1 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN1 <- neuralnet(formula = r1, data = train_data, hidden = c(4,3))
plot(mlp_NN1)
ncol(x_test)#number of the columns

y_p1 <- neuralnet::compute(mlp_NN1, x_test) #model result
evaluateData1 <- forecast(y_test, y_p1)
#get the statical indices
mlpTestResult1 <- evaluationFunction(actual = evaluateData1$'Expected Output', predict = evaluateData1$'Model Output')
head(mlpTestResult1)

#Model2
r2 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN2 <- neuralnet(formula = r2, data = train_data, hidden = c(4))
plot(mlp_NN2)
ncol(x_test)#number of the columns

y_p2 <- neuralnet::compute(mlp_NN2, x_test)
evaluateData2 <- forecast(y_test, y_p2)
#get the statical indices
mlpTestResult2 <- evaluationFunction(actual = evaluateData2$'Expected Output', predict = evaluateData2$'Model Output')
head(mlpTestResult2)

#Model3
r3 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN3 <- neuralnet(formula = r3, data = train_data, hidden = c(3))
plot(mlp_NN3)
ncol(x_test)#number of the columns

y_p3 <- neuralnet::compute(mlp_NN3, x_test)
evaluateData3 <- forecast(y_test, y_p3)
#get the statical indices
mlpTestResult3 <- evaluationFunction(actual = evaluateData3$'Expected Output', predict = evaluateData3$'Model Output')
head(mlpTestResult3)

#Model4
r4 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN4 <- neuralnet(formula = r4, data = train_data, hidden = c(3,2))
plot(mlp_NN4)
ncol(x_test)#number of the columns

y_p4 <- neuralnet::compute(mlp_NN4, x_test)
evaluateData4 <- forecast(y_test, y_p4)
#get the statical indices
mlpTestResult4 <- evaluationFunction(actual = evaluateData4$'Expected Output', predict = evaluateData4$'Model Output')
head(mlpTestResult4)

#Model5
r5 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN5 <- neuralnet(formula = r5, data = train_data, hidden = c(2))
plot(mlp_NN5)
ncol(x_test)#number of the columns

y_p5 <- neuralnet::compute(mlp_NN5, x_test)
evaluateData5 <- forecast(y_test, y_p5)
#get the statical indices
mlpTestResult5 <- evaluationFunction(actual = evaluateData5$'Expected Output', predict = evaluateData5$'Model Output')
head(mlpTestResult5)

#Model6
r6 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN6 <- neuralnet(formula = r6, data = train_data, hidden = c(5))
plot(mlp_NN6)
ncol(x_test)#number of the columns

y_p6 <- neuralnet::compute(mlp_NN6, x_test)
evaluateData6 <- forecast(y_test, y_p6)
#get the statical indices
mlpTestResult6 <- evaluationFunction(actual = evaluateData6$'Expected Output', predict = evaluateData6$'Model Output')
head(mlpTestResult6)

#Model7
r7 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN7 <- neuralnet(formula = r7, data = train_data, hidden = c(6))
plot(mlp_NN7)
ncol(x_test)#number of the columns

y_p7 <- neuralnet::compute(mlp_NN7, x_test)
evaluateData7 <- forecast(y_test, y_p7)
#get the statical indices
mlpTestResult7 <- evaluationFunction(actual = evaluateData7$'Expected Output', predict = evaluateData7$'Model Output')
head(mlpTestResult7)

#Model8
r8 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN8 <- neuralnet(formula = r8, data = train_data, hidden = c(6, 5))
plot(mlp_NN8)
ncol(x_test)#number of the columns

y_p8 <- neuralnet::compute(mlp_NN8, x_test)
evaluateData8 <- forecast(y_test, y_p8)
#get the statical indices
mlpTestResult8 <- evaluationFunction(actual = evaluateData8$'Expected Output', predict = evaluateData8$'Model Output')
head(mlpTestResult8)

#Model9
r9 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN9 <- neuralnet(formula = r9, data = train_data, hidden = c(7))
plot(mlp_NN9)
ncol(x_test)#number of the columns

y_p9 <- neuralnet::compute(mlp_NN9, x_test)
evaluateData9 <- forecast(y_test, y_p9)
#get the statical indices
mlpTestResult9 <- evaluationFunction(actual = evaluateData9$'Expected Output', predict = evaluateData9$'Model Output')
head(mlpTestResult9)

#Model10
r10 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN10 <- neuralnet(formula = r10, data = train_data, hidden = c(8, 9))
plot(mlp_NN10)
ncol(x_test)#number of the columns

y_p10 <- neuralnet::compute(mlp_NN10, x_test)
evaluateData10 <- forecast(y_test, y_p10)
#get the statical indices
mlpTestResult10 <- evaluationFunction(actual = evaluateData10$'Expected Output', predict = evaluateData10$'Model Output')
head(mlpTestResult10)

#Model11
r11 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN11 <- neuralnet(formula = r11, data = train_data, hidden = c(9))
plot(mlp_NN11)
ncol(x_test)#number of the columns

y_p11 <- neuralnet::compute(mlp_NN11, x_test)
evaluateData11 <- forecast(y_test, y_p11)
#get the statical indices
mlpTestResult11 <- evaluationFunction(actual = evaluateData11$'Expected Output', predict = evaluateData11$'Model Output')
head(mlpTestResult11)

#Model12
r12 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN12 <- neuralnet(formula = r12, data = train_data, hidden = c(6,3))
plot(mlp_NN12)
ncol(x_test)#number of the columns

y_p12 <- neuralnet::compute(mlp_NN12, x_test)
evaluateData12 <- forecast(y_test, y_p12)
#get the statical indices
mlpTestResult12 <- evaluationFunction(actual = evaluateData12$'Expected Output', predict = evaluateData12$'Model Output')
head(mlpTestResult12)

#Model13
r13 <- as.formula("Original~Delay_1+Delay_2+Delay_3+Delay_4")
mlp_NN13 <- neuralnet(formula = r13, data = train_data, hidden = c(9,3))
plot(mlp_NN13)
ncol(x_test)#number of the columns

y_p13 <- neuralnet::compute(mlp_NN13, x_test)
evaluateData13 <- forecast(y_test, y_p13)
#get the statical indices
mlpTestResult13 <- evaluationFunction(actual = evaluateData13$'Expected Output', predict = evaluateData13$'Model Output')
head(mlpTestResult13)


# Evaluation Function
rmse <- function(actual, predicted) { #RMSE calculation function
  RMSE <- sqrt(mean((actual - predicted)^2))
  return(RMSE)
}

#
evaluationFunction <- function(actual, predict) {
  rmse_mlp <- rmse(actual = actual, predicted = predict)
  mae_mlp <- Metrics::mae(actual = actual, predicted = predict)
  mape_mlp <- MAPE(y_pred = predict, y_true = actual)
  smape_mlp <- smape(predicted = predict, actual = actual)
  return(list(RMSE = rmse_mlp, MAE = mae_mlp, MAPE = mape_mlp, sMAPE = smape_mlp))
}