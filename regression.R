library(FSinR)
library(MASS)

split_floor <- function(houses) {
  floor <- houses[['Floor']]
  houses <- houses[-5]
  
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

mode <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

entropy <- function(x) {
  freq <- table(x)/length(x)
  v <- as.data.frame(freq)[, 2]
  v <- v[v > 0]
  return(-sum(v * log2(v)))
}

prepare_data_OLS <- function(houses) {
  houses[['City']] <- as.factor(houses[['City']])
  houses[['Furnishing.Status']] <- as.factor(houses[['Furnishing.Status']])
  houses[['Point.of.Contact']] <- as.factor(houses[['Point.of.Contact']])
  
  houses <- houses[-c(5,6,9)]
  houses <- houses[-4061,]
  
  return(houses)
}

prepare_data_LMS <- function(houses) {
  houses <- houses[-c(5,6,9)]
  houses <- houses[-4061,]
  
  return(houses)
}

select_features <- function(houses) {
  searcher <- searchAlgorithm('sequentialBackwardSelection')
  evaluator <- filterEvaluator('determinationCoefficient')
  res <- featureSelection(houses[-c(1)], 'Rent', searcher, evaluator)
  selected_features <- c(1,3)
  return(houses[c('Posted.On', 'Rent', colnames(res$bestFeatures))])
}

remove_outliers <- function(houses) {
  while(T) {
    m <- lm(Rent ~ ., houses[-1])
    distances <- cooks.distance(m)
    if (!is.nan(max(distances)) && max(distances) > 4 / length(houses[[1]])) {
      break
    }
    indexe_to_remove <- which.max(distances)
    houses <- houses[-indexe_to_remove,]
  }
  return(houses)
}

remove_outliers_percent <- function(houses, k) {
  removed_points <- 0
  max_outliers <- length(houses[[1]]) * k
  while(T) {
    m <- lm(Rent ~ ., houses[-1])
    distances <- cooks.distance(m)
    if (!is.nan(max(distances)) && max(distances) > 4 / length(houses[[1]]) &&
        removed_points >= max_outliers) {
      break
    }
    indexe_to_remove <- which.max(distances)
    houses <- houses[-indexe_to_remove,]
    removed_points <- removed_points + 1
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

k_fold <- function(houses, model, k) {
  sorted_indexes <- order(houses$Posted.On)
  elements_in_split <- as.integer(length(houses[[1]]) / k)
  
  MAE <- 0
  RMSE <- 0
  Rsquared <- 0
  
  for (i in 1:k) {
    start_index <- NA
    end_index <- NA
    if (i == 1) {
      start_index <- 1
      end_index <- elements_in_split
    } else if (i == k) {
      start_index <- (i-1) * elements_in_split
      end_index <- length(houses[[1]])
    } else {
      start_index <- (i-1) * elements_in_split
      end_index <- i * elements_in_split
    }
    
    test_set <- houses[sorted_indexes[start_index : end_index],]
    training_set <- houses[sorted_indexes[start_index : end_index],]
    
    set.seed(100)
    m <- NA
    if (model == 'lm') {
      m <- lm(Rent ~ ., training_set[-1])
    } else if (model == 'lms') {
      m <- lmsreg(Rent ~ ., training_set[-1], method = 'lms')
    }
    predictions <- predict(m, newdata = test_set[-1])
    MAE <- MAE + mean_absolute_error(predictions, test_set[[2]])
    RMSE <- RMSE + root_mean_square_error(predictions, test_set[[2]])
    Rsquared <- Rsquared + r_squared(predictions, test_set[[2]])
  }
  
  MAE <- MAE / k
  RMSE <- RMSE / k
  Rsquared <- Rsquared / k
  
  return(data.frame(MAE, RMSE, Rsquared))
}

houses <- read.csv(file = '.\\data\\regression\\House_Rent_Dataset.csv', header = T)

head(houses)
min_rent <- min(houses_OLS[['Total.Floors']])
mean_rent <- mean(houses_OLS[['Total.Floors']])
median_rent <- median(houses_OLS[['Total.Floors']])
variance_rent <- var(houses_OLS[['Total.Floors']])
std_dev <- sd(houses_OLS[['Total.Floors']])
max_rent <- max(houses_OLS[['Total.Floors']])

# Rho: 0.0455, p-value: 0.0017
cor.test(as.numeric(as.factor(houses[['Floor']])), houses[['Rent']], method = 'spearman')

houses <- split_floor(houses)

# Rho: 0.4834, p-value: 2.2e-16
cor.test(houses[['Floor']], houses[['Rent']], method = 'spearman')
# Rho: 0.5818, p-value: 2.2e-16
cor.test(houses[['Total.Floors']], houses[['Rent']], method = 'spearman')
# Rho: 0.5686, p-value: 2.2e-16
cor.test(houses[['BHK']], houses[['Rent']], method = 'spearman')
# Rho: 0.5215, p-value: 2.2e-16
cor.test(houses[['Size']], houses[['Rent']], method = 'spearman')
# Rho: -0.3674, p-value: 2.2e-16
cor.test(as.numeric(as.factor(houses[['Area.Type']])), houses[['Rent']], method = 'spearman')
# Rho: -0.0014, p-value: 0.9197
cor.test(as.numeric(as.factor(houses[['Area.Locality']])), houses[['Rent']], method = 'spearman')
# Rho: 0.3058, p-value: 2.2e-16
cor.test(as.numeric(as.factor(houses[['City']])), houses[['Rent']], method = 'spearman')
# Rho: -0.2987, p-value: 2.2e-16
cor.test(as.numeric(as.factor(houses[['Furnishing.Status']])), houses[['Rent']], method = 'spearman')
# Rho: 0.0117, p-value: 0.4168
cor.test(as.numeric(as.factor(houses[['Tenant.Preferred']])), houses[['Rent']], method = 'spearman')
# Rho: 0.6631, p-value: 2.2e-16
cor.test(houses[['Bathroom']], houses[['Rent']], method = 'spearman')
# Rho: -0.5967, p-value: 2.2e-16
cor.test(as.numeric(as.factor(houses[['Point.of.Contact']])), houses[['Rent']], method = 'spearman')

mode(houses[['Point.of.Contact']])
entropy(houses[['Point.of.Contact']])

shapiro.test(houses[['Floor']])
shapiro.test(houses[['Total.Floors']])
shapiro.test(houses[['BHK']])
shapiro.test(houses[['Size']])
shapiro.test(as.numeric(as.factor(houses[['City']])))
shapiro.test(as.numeric(as.factor(houses[['Furnishing.Status']])))
shapiro.test(houses[['Bathroom']])
shapiro.test(as.numeric(as.factor(houses[['Point.of.Contact']])))


houses_OLS <- prepare_data_OLS(houses)
houses_OLS <- select_features(houses_OLS)
houses_OLS_05 <- remove_outliers_percent(houses_OLS, 0.05)
houses_OLS_10 <- remove_outliers_percent(houses_OLS, 0.10)
houses_OLS_15 <- remove_outliers_percent(houses_OLS, 0.15)
houses_OLS_20 <- remove_outliers_percent(houses_OLS, 0.20)
houses_OLS_25 <- remove_outliers_percent(houses_OLS, 0.25)
houses_OLS_30 <- remove_outliers_percent(houses_OLS, 0.30)
houses_OLS_35 <- remove_outliers_percent(houses_OLS, 0.35)
houses_OLS_40 <- remove_outliers_percent(houses_OLS, 0.40)

houses_OLS <- prepare_data_OLS(houses)
houses_OLS <- select_features(houses_OLS)
houses_OLS <- remove_outliers(houses_OLS)


metrics_OLS <- k_fold(houses_OLS, 'lm', 10)
metrics_OLS_05 <- k_fold(houses_OLS_05, 'lm', 5)
metrics_OLS_10 <- k_fold(houses_OLS_10, 'lm', 5)
metrics_OLS_15 <- k_fold(houses_OLS_15, 'lm', 5)
metrics_OLS_20 <- k_fold(houses_OLS_20, 'lm', 5)
metrics_OLS_25 <- k_fold(houses_OLS_25, 'lm', 5)
metrics_OLS_30 <- k_fold(houses_OLS_30, 'lm', 5)
metrics_OLS_35 <- k_fold(houses_OLS_35, 'lm', 5)
metrics_OLS_40 <- k_fold(houses_OLS_40, 'lm', 5)

houses_LMS <- prepare_data_LMS(houses)
houses_LMS <- select_features(houses_LMS)
metrics_LMS <- k_fold(houses_LMS, 'lms', 10)

metrics_OLS
metrics_LMS

metrics_OLS_05
metrics_OLS_10
metrics_OLS_15
metrics_OLS_20
metrics_OLS_25
metrics_OLS_30
metrics_OLS_35
metrics_OLS_40