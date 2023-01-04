library(FSinR)
library(caret)
library(nnet)

remove_invalid <- function(data) {
  indexes_to_remove <- which(data[[7]] > 1)
  if (length(indexes_to_remove) != 0) {
    data <- data[-indexes_to_remove,]
  }
  
  return(data)
}


select_features <- function(data) {
  searcher <- searchAlgorithm('sequentialForwardSelection')
  evaluator <- filterEvaluator('giniIndex')
  res <- featureSelection(data, 'Outcome', searcher, evaluator)
  selected_features <- c(which(res$bestFeatures == 1), 9)
  print(selected_features)
  return(data[selected_features])
}


patients <- read.csv(file = '.\\data\\classification\\diabetes.csv', header = T)
patients <- remove_invalid(patients)

head(patients)
n <- 8
min(patients[[n]])
max(patients[[n]])
mean(patients[[n]])
median(patients[[n]])
sd(patients[[n]])
var(patients[[n]])

patients <- select_features(patients)

patients$Outcome <- ifelse(patients$Outcome == 1, 'Unhealthy', 'Healthy')
patients$Outcome <- as.factor(patients$Outcome)

set.seed(100)
inTrain <- createDataPartition(
  y = patients$Outcome,
  p = 0.8,
  list = FALSE
)

set.seed(100)
ctrl <- trainControl(method = 'cv', number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = T, savePredictions = T)
logistic_regression <- train(Outcome ~ ., data = patients[inTrain,],
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
neural_network <- train(Outcome ~ ., data = patients[inTrain,],
                       method = 'nnet', maxit = 5000, tuneGrid = nnGrid,
                       trace = F, trControl = ctrl, metric = 'ROC')
Sys.time()

roc_nn = roc(as.numeric(neural_network$trainingData$.outcome == 'Unhealthy'),
             aggregate(Unhealthy ~ rowIndex, neural_network$pred, mean)[,'Unhealthy'], ci=T)


lines(roc_nn, col = 'red')
c = coords(roc_nn, x = "best", best.method = 'youden')
abline(v = c[2], col = 'red')
abline(h = c[3], col = 'red')

logistic_predictions <- predict(logistic_regression, newdata = patients[-inTrain,])
nn_predictions <- predict(neural_network, newdata = patients[-inTrain,])

logistic_cm <- confusionMatrix(logistic_predictions,
                               reference = patients[-inTrain, 4])
nn_cm <- confusionMatrix(nn_predictions,
                               reference = patients[-inTrain, 4])

logistic_cm$overall
nn_cm$overall

logistic_cm$byClass
nn_cm$byClass

