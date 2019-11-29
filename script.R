# ECONOMETRIC EXERCISE 
# MACHHOURI FATIMA, COMPUTER SCIENCE AND MANAGEMENT SECTION, POLYTECH MONTPELLIER

# Question : We have measurements of a sonar (60 variables 'V1'...'V60') and the object detected (variable 'Class' : '1' for 'rock' and '0' for 'mine'). 
# Are we able to correctly detect mines ?

library(ggplot2)
library(caret)
library(pROC)

dir <- 'your_path' #Replace your_path by the directory path
setwd(dir)

# STEP 1 : We load the datas
Sonar <- read.table(file = 'datas/Sonar.csv', header = TRUE, sep = ';')

# STEP 2 : We check that the dataset is balanced
table(Sonar$Class)
ggplot(Sonar) + geom_bar(aes(as.factor(Class))) + xlab('Class') + theme_bw()

# STEP 3 : We split the dataset into 2 datasets
# -> the training dataset composed of 80% 
# -> the test dataset composed of 20%
n <- nrow(Sonar)
train_index <- sample(x = 1:n, size = round(0.8 * n), replace = FALSE)
training_data <- Sonar[train_index,] 
test_data <- Sonar[-train_index,] 

# STEP 4 : Model Training and Backward Selection
log_reg <- glm(Class ~ ., data = training_data, family = 'binomial', maxit = 1000)
log_reg <- step(log_reg, direction = 'backward')
summary(log_reg)

# STEP 5 : Prediction on test dataset
hat_pi <- predict(log_reg, newdata = test_data, type = 'response')
hat_y <- as.integer(hat_pi > 0.5)

# STEP 6 : Confusion Matrix
table(hat_y, test_data$Class)
confusionMatrix(data= as.factor(hat_y), reference = as.factor(test_data$Class), positive = '1')

# Sensibility and Specificity functions according to the threshold
sensibility <- function(threshold, df) {
  out <- sum(as.integer(hat_pi > threshold) == 1 & df$Class == 1)/sum(df$Class == 1)
  return(out)
}

specificity <- function(threshold, df) {
  out <- sum(as.integer(hat_pi > threshold) == 0 & df$Class == 0)/sum(df$Class == 0)
  return(out)
}

# Possible values for the threshold
treshold <- seq(0, 1, 0.001)

# Calculation of sensitivity and specificity based on the possible values for the threshold
sens <- sapply(treshold, sensibility, df = test_data)
spec <- sapply(treshold, specificity, df = test_data)

data2plot <- data.frame(treshold = rep(treshold, 2), value = c(sens, spec), tag = rep(c('sensitivity', 'specificity'), each = length(treshold)))
ggplot(data2plot, aes(x = treshold, y = value)) + geom_line(aes(col = tag)) + theme_bw() + theme(legend.title = element_blank())

# ROC and AUC curves
data2plot <- data.frame(treshold = treshold, sensitivity = sens, specificity = spec)
ggplot(data2plot, aes(x = 1 - specificity, y = sensitivity)) + geom_line() + geom_area(fill='blue', alpha = 0.2, position = 'identity') + theme_bw()

# AUC
auc(test_data$Class, hat_pi)


