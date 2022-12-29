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

# k_fold <- function(patients, model, k) {
#   elements_in_split <- as.integer(length(patients[[1]]) / k)
# 
#   mean_accuracy <- 0
#   mean_TPR <- 0
#   mean_TNR <- 0
#   mean_precision <- 0
#   mean_recall <- 0
#   print(Sys.time())
#   for (i in 1:k) {
#     start_index <- NA
#     end_index <- NA
#     if (i == 1) {
#       start_index <- 1
#       end_index <- elements_in_split
#     } else if (i == k) {
#       start_index <- (i-1) * elements_in_split
#       end_index <- length(patients[[1]])
#     } else {
#       start_index <- (i-1) * elements_in_split
#       end_index <- i * elements_in_split
#     }
# 
#     test_set <- patients[start_index : end_index,]
#     training_set <- patients[-(start_index : end_index),]
# 
#     set.seed(100)
#     m <- NA
#     ctrl <- trainControl(method = 'cv', number = 10,
#                          summaryFunction = twoClassSummary,
#                          classProbs = T, savePredictions = T)
#     if (model == 'glm') {
#       m <- train(Outcome ~ ., data = training_set,
#                                    method = 'glm', trControl = ctrl,
#                                    metric = 'ROC')
#       roc = roc(as.numeric(m$trainingData$.outcome == 'Unhealthy'),
#                 aggregate(Unhealthy ~ rowIndex, m$pred,mean)[,'Unhealthy'], ci = T)
#       t = coords(roc, x = "best", best.method = 'youden')$threshold
#       cm <- confusionMatrix(predict(m, newdata = test_set),
#                             reference = test_set[['Outcome']])
#       mean_accuracy <- mean_accuracy + cm$overall[[1]]
#       mean_TPR <- mean_TPR + cm$byClass[[1]]
#       mean_TNR <- mean_TNR + cm$byClass[[2]]
#       mean_precision <- mean_precision + cm$byClass[[5]]
#       mean_recall <- mean_recall + cm$byClass[[6]]
#     } else if (model == 'nnet') {
#       m <- train(Outcome ~ ., data = training_set,
#                  method = 'nnet', maxit = 50000, trace = F,
#                  trControl = ctrl, metric = 'ROC')
# 
#       cm <- confusionMatrix(predict(m, newdata = test_set),
#                             reference = test_set[['Outcome']])
#       mean_accuracy <- mean_accuracy + cm$overall[[1]]
#       mean_TPR <- mean_TPR + cm$byClass[[1]]
#       mean_TNR <- mean_TNR + cm$byClass[[2]]
#       mean_precision <- mean_precision + cm$byClass[[5]]
#       mean_recall <- mean_recall + cm$byClass[[6]]
#     }
#   }
#   mean_accuracy <- mean_accuracy / k
#   mean_TPR <- mean_TPR / k
#   mean_TNR <- mean_TNR / k
#   mean_precision <- mean_precision / k
#   mean_recall <- mean_recall / k
# 
#   print(Sys.time())
#   return(data.frame(mean_accuracy, mean_TPR, mean_TNR, mean_precision, mean_recall))
# }

patients <- read.csv(file = '.\\data\\classification\\diabetes.csv', header = T)

head(patients_balanced)
n <- 8
min(patients_balanced[[n]])
max(patients_balanced[[n]])
mean(patients_balanced[[n]])
median(patients_balanced[[n]])
sd(patients_balanced[[n]])
var(patients_balanced[[n]])

patients <- remove_invalid(patients)

prevalence <- max(table(patients$Outcome) / length(patients[[1]]))
set.seed(100)
newPDFOS <- pdfos(patients, numInstances = abs(max(table(patients$Outcome)) -
                                                 length(patients[[1]])),
                  classAttr = 'Outcome')
patients_balanced <- merge(patients, newPDFOS, all = T)
prevalence <- max(table(patients_balanced$Outcome) / length(patients_balanced[[1]]))
patients_balanced <- remove_invalid(patients_balanced)
prevalence <- max(table(patients_balanced$Outcome) / length(patients_balanced[[1]]))

patients_balanced$Outcome <- ifelse(patients_balanced$Outcome == 1, 'Unhealthy', 'Healthy')
patients_balanced$Outcome <- as.factor(patients_balanced$Outcome)

# set.seed(100)
# training_ids <- sample(rownames(patients_balanced), length(patients_balanced[[1]]) * 0.8)
# training_data <- patients_balanced[rownames(patients_balanced) %in% training_ids,]
# test_data <- patients_balanced[!rownames(patients_balanced) %in% training_ids,]


# k_fold(patients_balanced, 'glm', 10)
# k_fold(patients_balanced, 'nnet', 10)

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

