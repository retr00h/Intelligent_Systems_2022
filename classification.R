library(imbalance)
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

patients <- read.csv(file = '.\\data\\classification\\diabetes.csv', header = T)

head(patients)
n <- 8
min(patients[[n]])
max(patients[[n]])
mean(patients[[n]])
median(patients[[n]])
sd(patients[[n]])
var(patients[[n]])

patients <- remove_invalid(patients)

prevalence <- max(table(patients$Outcome) / length(patients[[1]]))
newPDFOS <- pdfos(patients, numInstances = abs(max(table(patients$Outcome)) -
                                                 length(patients[[1]])),
                  classAttr = 'Outcome')
patients_balanced <- merge(patients, newPDFOS, all = T)
prevalence <- max(table(patients_balanced$Outcome) / length(patients_balanced[[1]]))
patients_balanced <- remove_invalid(patients_balanced)
prevalence <- max(table(patients_balanced$Outcome) / length(patients_balanced[[1]]))


patients_balanced$Outcome <- ifelse(patients_balanced$Outcome == 1, 'Unhealthy', 'Healthy')
patients_balanced$Outcome <- as.factor(patients_balanced$Outcome)

set.seed(100)
ctrl <- trainControl(method = 'repeatedcv', number = 10,
                     repeats = 20, summaryFunction = twoClassSummary,
                     classProbs = T, savePredictions = T)
logistic_regression <- train(Outcome ~ ., data = patients_balanced,
                             method = 'glm', trControl = ctrl, metric = 'ROC')

roc_logistic = roc(as.numeric(logistic_regression$trainingData$.outcome=='Unhealthy'),
                   aggregate(Unhealthy~rowIndex,logistic_regression$pred,mean)[,'Unhealthy'], ci=T)

plot(roc_logistic)
c = coords(roc_logistic, x = "best", best.method = 'youden')
abline(v = c[2])
abline(h = c[3])

set.seed(100)
ctrl <- trainControl(method = 'repeatedcv', number = 10,
                     repeats = 20, summaryFunction = twoClassSummary,
                     classProbs = T, savePredictions = T)
neural_network <- train(Outcome ~ ., data = patients_balanced,
                             method = 'nnet', trControl = ctrl, metric = 'ROC')

roc_nn = roc(as.numeric(neural_network$trainingData$.outcome=='Unhealthy'),
             aggregate(Unhealthy~rowIndex,neural_network$pred,mean)[,'Unhealthy'], ci=T)
lines(roc_nn, col = 'red')
c = coords(roc_nn, x = "best", best.method = 'youden')
abline(v = c[2], col = 'red')
abline(h = c[3], col = 'red')

logistic_cm_train <- confusionMatrix(predict(logistic_regression,
                                             newdata = patients_balanced),
                                     reference = patients_balanced[['Outcome']])
logistic_cm_train

nn_cm_train <- confusionMatrix(predict(neural_network,
                                       newdata = patients_balanced),
                               reference = patients_balanced[['Outcome']])
nn_cm_train









