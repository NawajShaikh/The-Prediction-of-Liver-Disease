library(mlbench)
library(caret)
library(caretEnsemble)
library(readr)
liver_data <- read_csv("C:\\Users\\Shaikh Nawaj\\Desktop\\project\\Indian Liver Patient Dataset.csv")
summary(liver_data)
# in the summary we have seen that there is 4 missing values present in the alkphos column
boxplot(liver_data$alkphos)
# in the boxplot we see that outliers are present in the alkphos, for that purpose we replace missing values by median instead of mean 
alkphos_median <- median(liver_data$alkphos,na.rm = T)
liver_data$alkphos[is.na(liver_data$alkphos)] <- alkphos_median
sum(is.na(liver_data))
# now we see that there is no missing value in the liver data
liver_data$is_patient[liver_data$is_patient == 2] <- 0
#now we convert age,gender and result columns into factor
factor_data <- lapply(liver_data[,c(1,2,11)], factor)
# now we normalize all continuous variables
normalize <- function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
data_norm <- as.data.frame(lapply(liver_data[,c(3:10)],FUN=normalize))
final_data <- cbind(data_norm,factor_data)
str(final_data)

# splitting data into training and testing data
final_data_1 <- final_data[sample(nrow(final_data)),]
train <- final_data_1[1:as.integer(0.70*nrow(final_data)),]
test <- final_data_1[-c(1:as.integer(0.70*nrow(final_data))),]


set.seed(786)
control <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
metric <- "Accuracy"
# Bagged CART
fit.treebag <- train(is_patient~.,data = train, method="treebag", metric=metric, trControl=control)
# Stochastic Gradient Boosting
fit.gbm <- train(is_patient~., data = train, method="gbm", metric=metric, trControl=control, verbose=FALSE)
# summarize results
results <- resamples(list(bagging=fit.treebag,boosting=fit.gbm))
summary(results)
dotplot(results)


# predict bagging results
pred_bagging <- predict(fit.treebag, test)
confusionMatrix(pred_bagging,test$is_patient)


# predict boosting results
pred_boosting <- predict(fit.gbm, test)
confusionMatrix(pred_boosting,test$is_patient)
