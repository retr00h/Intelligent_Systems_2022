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

houses <- read.csv(file = '.\\data\\regression\\House_Rent_Dataset.csv', header = T)

head(houses)
min_rent <- min(houses[['Rent']])
mean_rent <- mean(houses[['Rent']])
median_rent <- median(houses[['Rent']])
variance_rent <- var(houses[['Rent']])
max_rent <- max(houses[['Rent']])

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
