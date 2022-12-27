library(imbalance)

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

