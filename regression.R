library(FSinR)
library(MASS)
library(caret)

split_floor <- function(houses) {
  floor <- houses[['Floor']]
  
  splitted_floor <- strsplit(floor, ' ')
  new_floor <- list()
  new_total_floors <- list()
  
  for (sf in splitted_floor) {
    floor_to_append <- NA
    total_floor_to_append <- sf[[length(sf)]]
    if ('basement' %in% sf || 'Basement' %in% sf) {
      floor_to_append <- -1
    } else if ('Ground' %in% sf || 'ground' %in% sf) {
      floor_to_append <- 0
    } else {
      floor_to_append <- sf[[1]]
    }
    
    new_floor <- append(new_floor, floor_to_append)
    new_total_floors <- append(new_total_floors, total_floor_to_append)
  }
  
  houses$Floor <- as.numeric(unlist(new_floor))
  houses$Total.Floors <- as.numeric(unlist(new_total_floors))
  
  indexes_to_remove <- which(is.na(houses[['Total.Floors']]))
  houses <- houses[-indexes_to_remove,]
  
  return(houses)
}

prepare_data <- function(houses) {
  houses <- houses[-c(1,7,10)]
  houses <- houses[-4061,]
  
  return(houses)
}

factorize <- function(houses) {
  houses[['BHK']] <- as.integer(houses[['BHK']])
  houses[['Area.Type']] <- as.factor(houses[['Area.Type']])
  houses[['City']] <- as.factor(houses[['City']])
  houses[['Furnishing.Status']] <- as.factor(houses[['Furnishing.Status']])
  
  return(houses)
}

select_features <- function(houses) {
  searcher <- searchAlgorithm('sequentialBackwardSelection')
  evaluator <- filterEvaluator('determinationCoefficient')
  res <- featureSelection(houses, 'Rent', searcher, evaluator)
  selected_features <- c(which(res$bestFeatures == 1))
  return(houses[selected_features])
}

remove_outliers <- function(houses) {
  while(T) {
    m <- lm(Rent ~ ., houses)
    distances <- cooks.distance(m)
    if (!is.nan(max(distances)) && max(distances) < 4 / length(houses[[1]])) {
      break
    }
    indexe_to_remove <- which.max(distances)
    houses <- houses[-indexe_to_remove,]
  }
  return(houses)
}

mean_absolute_error <- function(predictions, target) {
  return(sum(abs(predictions - target)) / length(predictions))
}

root_mean_square_error <- function(predictions, target) {
  return(sqrt(sum((predictions - target)^2) / length(predictions)))
}

r_squared <- function(predictions, target) {
  ss_res <- sum((target - predictions)^2)
  ss_tot <- sum((target - mean(target))^2)
  return(1 - ss_res / ss_tot)
}

cross_validation <- function(houses, model) {
  MAE <- 0
  RMSE <- 0
  Rsquared <- 0
  
  for (i in seq(1, length(houses[[1]]), 1)) {
    if (i %% 100 == 0) {
      print(i)
    }
    test_instance <- houses[i,]
    training_set <- houses[-i,]
    
    set.seed(100)
    m <- NA
    if (model == 'lm') {
      m <- lm(Rent ~ ., training_set)
    } else if (model == 'lms') {
      m <- lmsreg(Rent ~ ., training_set, method = 'lms')
    }
    predictions <- predict(m, newdata = test_instance)
    MAE <- MAE + mean_absolute_error(predictions, test_instance[[2]])
    RMSE <- RMSE + root_mean_square_error(predictions, test_instance[[2]])
  }
  
  l <- length(houses[[1]])
  
  MAE <- MAE / l
  RMSE <- RMSE / l
  
  m <- NA
  if (model == 'lm') {
    m <- lm(Rent ~ ., houses)
  } else if (model == 'lms') {
    m <- lmsreg(Rent ~ ., houses, method = 'lms')
  }
  Rsquared <- r_squared(predict(m, newdata = houses), houses[[2]])
  
  return(data.frame(MAE, RMSE, Rsquared))
}

houses <- read.csv(file = '.\\data\\regression\\House_Rent_Dataset.csv', header = T)


Rent <- houses[['Rent']]
Size <- houses[['Size']]
BHK <- houses[['BHK']]
Bathroom <- houses[['Bathroom']]
City <- houses[['City']]
FurnishingStatus <- houses[['Furnishing.Status']]
TenantPreferred <- houses[['Tenant.Preferred']]
PointOfContact <- houses[['Point.of.Contact']]

plot(table(City))
plot(table(FurnishingStatus))
plot(table(TenantPreferred))
plot(table(PointOfContact))

plot(Rent ~ Size)
plot(Rent ~ BHK)
plot(Rent ~ Bathroom)


houses <- split_floor(houses)
houses <- prepare_data(houses)
houses <- select_features(houses)
houses_OLS <- remove_outliers(houses)
houses_OLS <- factorize(houses_OLS)


set.seed(100)
inTrain <- createDataPartition(
  y = houses_OLS$City,
  p = 0.8,
  list = FALSE
)

training_set_OLS <- houses_OLS[inTrain,]
test_set_OLS <- houses_OLS[-inTrain,]

ols <- lm(Rent ~ ., data = training_set_OLS)
predictions <- predict(ols, newdata = test_set_OLS)
root_mean_square_error(predictions, test_set_OLS[[2]])
mean_absolute_error(predictions, test_set_OLS[[2]])
r_squared(predictions, test_set_OLS[[2]])

set.seed(100)
inTrain <- createDataPartition(
  y = houses$City,
  p = 0.8,
  list = FALSE
)

training_set <- houses_OLS[inTrain,]
test_set <- houses_OLS[-inTrain,]
lms <- lmsreg(Rent ~ ., training_set, method = 'lms')
predictions <- predict(ols, newdata = test_set)
root_mean_square_error(predictions, test_set[[2]])
mean_absolute_error(predictions, test_set[[2]])
r_squared(predictions, test_set[[2]])
