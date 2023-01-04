library(imbalance)
library(tidyverse)
library(caret)
library(pROC)
library(nnet)

remove_invalid <- function(data) {
  for (i in 1:8) {
    indexes_to_remove <- which(data[[i]] < 0)
    if (length(indexes_to_remove) != 0) {
      data <- data[-indexes_to_remove,]
    }
  }
  
  indexes_to_remove <- which(data[[7]] > 1)
  if (length(indexes_to_remove) != 0) {
    data <- data[-indexes_to_remove,]
  }
  
  return(data)
}

fix_invalid <- function(data) {
  for (i in 1:8) {
    indexes_to_fix <- which(data[[i]] < 0)
    if (length(indexes_to_fix) != 0) {
      data[indexes_to_fix, i] <- 0
    }
  }
  
  indexes_to_fix <- which(data[[7]] > 1)
  if (length(indexes_to_fix) != 0) {
    data[indexes_to_fix, i] <- 1
  }
  
  return(data)
}

patients <- read.csv(file = '.\\data\\classification\\diabetes.csv', header = T)

head(patients_balanced)
n <- 8
min(patients_balanced[[n]])
max(patients_balanced[[n]])
mean(patients_balanced[[n]])
median(patients_balanced[[n]])
sd(patients_balanced[[n]])
var(patients_balanced[[n]])

patients <- fix_invalid(patients)

prevalence <- max(table(patients$Outcome) / length(patients[[1]]))
set.seed(100)
newPDFOS <- pdfos(patients, numInstances = abs(max(table(patients$Outcome)) -
                                                 length(patients[[1]])),
                  classAttr = 'Outcome')
patients_balanced <- merge(patients, newPDFOS, all = T)
prevalence <- max(table(patients_balanced$Outcome) / length(patients_balanced[[1]]))

# Use remove_invalid to obtain a non perfectly balanced dataset
patients_balanced <- fix_invalid(patients_balanced)
prevalence <- max(table(patients_balanced$Outcome) / length(patients_balanced[[1]]))

patients_balanced$Outcome <- ifelse(patients_balanced$Outcome == 1, 'Unhealthy', 'Healthy')
patients_balanced$Outcome <- as.factor(patients_balanced$Outcome)

set.seed(100)
ctrl <- trainControl(method = 'cv', number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = T, savePredictions = T)
logistic_regression <- train(Outcome ~ ., data = patients_balanced,
                            method = 'glm', trControl = ctrl, metric = 'ROC')

roc_logistic = roc(as.numeric(logistic_regression$trainingData$.outcome == 'Unhealthy'),
                   aggregate(Unhealthy ~ rowIndex, logistic_regression$pred, mean)[,'Unhealthy'], ci=T)


plot(roc_logistic)
c = coords(roc_logistic, x = "best", best.method = 'youden')
abline(v = c[2])
abline(h = c[3])

set.seed(100)
ctrl <- trainControl(method = 'cv', number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = T, savePredictions = T)
nnGrid <- expand.grid(size = seq(1, 10), decay = seq(0, 0.5, 0.1))

Sys.time()
neural_network <- train(Outcome ~ ., data = patients_balanced,
                       method = 'nnet', maxit = 5000, tuneGrid = nnGrid,
                       trace = F, trControl = ctrl, metric = 'ROC')
Sys.time()

roc_nn = roc(as.numeric(neural_network$trainingData$.outcome == 'Unhealthy'),
             aggregate(Unhealthy ~ rowIndex, neural_network$pred, mean)[,'Unhealthy'], ci=T)


lines(roc_nn, col = 'red')
c = coords(roc_nn, x = "best", best.method = 'youden')
abline(v = c[2], col = 'red')
abline(h = c[3], col = 'red')

logistic_cm <- confusionMatrix(logistic_regression$pred$pred,
                               reference = logistic_regression$pred$obs)

nn_cm <- confusionMatrix(neural_network$pred$pred,
                               reference = neural_network$pred$obs)

logistic_cm$overall
nn_cm$overall

logistic_cm$byClass
nn_cm$byClass

