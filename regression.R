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
  houses <- houses[-c(3079, 3966, 4061),]
  
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
  removed_datapoints <- 0
  max_datapoints_to_remove <- length(houses[[1]]) * 0.1
  while(T) {
    m <- lm(Rent ~ ., houses)
    distances <- cooks.distance(m)
    if (removed_datapoints > max_datapoints_to_remove ||
        (!is.nan(max(distances)) && max(distances) < 4 / length(houses[[1]]))) {
      break
    }
    index_to_remove <- which.max(distances)
    houses <- houses[-index_to_remove,]
    removed_datapoints <- removed_datapoints + 1
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

houses <- read.csv(file = '.\\data\\regression\\House_Rent_Dataset.csv', header = T)


Rent <- houses[['Rent']]
Size <- houses[['Size']]
BHK <- houses[['BHK']]
Bathroom <- houses[['Bathroom']]
City <- houses[['City']]
FurnishingStatus <- houses[['Furnishing.Status']]
TenantPreferred <- houses[['Tenant.Preferred']]
PointOfContact <- houses[['Point.of.Contact']]
AreaType <- houses[['Area.Type']]


plot(table(City), xlab = 'City', ylab = 'Number of instances')
plot(table(FurnishingStatus), xlab = 'Furnishing status', ylab = 'Number of instances')
plot(table(TenantPreferred),xlab = 'Type of preferred tenant', ylab = 'Number of instances')
plot(table(PointOfContact), xlab = 'Point of contact', ylab = 'Number of instances')

plot(table(AreaType), xlab = 'Measurement method', ylab = 'Number of instances')
plot(Rent ~ Size, xlab = 'Size (ft^2)', ylab = 'Rent (INR/month)')
plot(Rent ~ BHK, xlab = 'BHK', ylab = 'Rent (INR/month)')
plot(Rent ~ Bathroom, xlab = 'Number of bathrooms', ylab = 'Rent (INR/month)')


houses <- split_floor(houses)
Rent <- houses[['Rent']]
Floor <- houses[['Floor']]
TotalFloors <- houses[['Total.Floors']]
plot(table(Floor), xlab = 'Floor number', ylab = 'Number of instances')
plot(table(TotalFloors), xlab = 'Total floors', ylab = 'Number of instances')
plot(Rent ~ Floor, xlab = 'Floor number', ylab = 'Rent (INR/month)')
plot(Rent ~ TotalFloors, xlab = 'Total floors', ylab = 'Rent (INR/month)')


houses <- prepare_data(houses)
houses <- select_features(houses)
houses_OLS <- remove_outliers(houses)


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

training_set <- houses[inTrain,]
test_set <- houses[-inTrain,]
lms <- lmsreg(Rent ~ ., training_set, method = 'lms')
predictions <- predict(ols, newdata = test_set)
root_mean_square_error(predictions, test_set[[2]])
mean_absolute_error(predictions, test_set[[2]])
r_squared(predictions, test_set[[2]])


ols_outliers <- lm(Rent ~ ., data = training_set)
predictions <- predict(ols_outliers, newdata = test_set)
root_mean_square_error(predictions, test_set[[2]])
mean_absolute_error(predictions, test_set[[2]])
r_squared(predictions, test_set[[2]])
