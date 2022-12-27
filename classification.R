library(imbalance)
library(caret)
library(pROC)

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
n <- 7
min(patients[[n]])
mean(patients[[n]])
median(patients[[n]])
var(patients[[n]])
sd(patients[[n]])
max(patients[[n]])

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
ctrl <- trainControl(method = 'repeatedcv', number = 10,
                     repeats = 20, summaryFunction = twoClassSummary,
                     classProbs = T, savePredictions = T)
logistic_regression <- train(Outcome ~ ., data = patients_balanced,
                             method = 'glm', trControl = ctrl, metric = 'ROC')

roc = roc(as.numeric(logistic_regression$trainingData$.outcome=='Unhealthy'),
          aggregate(Unhealthy~rowIndex,logistic_regression$pred,mean)[,'Unhealthy'], ci=T)

plot(roc, print.auc = T)
c = coords(roc, x = "best", best.method = 'youden')
abline(v = c[2])
abline(h = c[3])


cm <- confusionMatrix(predict(logistic_regression, newdata = patients_balanced),
                      reference = patients_balanced[['Outcome']])





