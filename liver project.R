library(ROCR)
library(lattice)
library(ggplot2)
library(caret)
library(caTools)
library(e1071)
library(gplots)
library(corrplot)
library(pROC)

library(readr)
liver.df <- read_csv("C:\\Users\\Shaikh Nawaj\\Desktop\\project\\Indian Liver Patient Dataset.csv")


#____________________________Preprocessing________________________________________________________

#Replacing 2's in is_patient with 0
liver.df$is_patient <- ifelse(liver.df$is_patient == 2,0,1)
# 1 is patient
# 0 is not patient

summary(liver.df)
#alkphos has 4 missing values
#Replacing with mean

alkphos_median <- median(liver.df$alkphos,na.rm = T)

liver.df$alkphos[is.na(liver.df$alkphos)] <- alkphos_median
sum(is.na(liver.df))

#Creating factor levels for is_patient
liver.df$is_patient <- factor(liver.df$is_patient , levels =  c(0,1))

set.seed(1)
liver_1.df<- liver.df[sample(nrow(liver.df)),]


#Splitting into train and test
train_1.df <- liver_1.df[1:as.integer(0.70*nrow(liver.df)),]
test_1.df <- liver_1.df[-c(1:as.integer(0.70*nrow(liver.df))),]


#Upsampling the training data to equalize number of is_patient
train_1.df$is_patient <- factor(train_1.df$is_patient)
train_1.df <- upSample(x = train_1.df, train_1.df$is_patient)
prop.table(table(train_1.df$is_patient))


#Creating a dummy variable for Gender
train_1.df$isFemale <- ifelse(train_1.df$gender == 'Female',1,0) 
train_1.df$isFemale <- factor(train_1.df$isFemale)
test_1.df$isFemale <- ifelse(test_1.df$gender == 'Female',1,0) 
test_1.df$isFemale <- factor(test_1.df$isFemale)


#_____________Logistic Regression model____________________________________________
#correlations matter less. easy to do on a pen and paper. easy to visualize. you can easily update your model to take in new data.


#formula for model 1 
mod_f1 <- is_patient ~ age + tot_bilirubin + direct_bilirubin + tot_proteins + albumin + ag_ratio+ sgpt + sgot + alkphos + isFemale
model1 <- glm(mod_f1 , data = train_1.df , family = binomial(link = "logit"))
model1
pred_1 <- predict(model1, test_1.df, type = "response")
pred_logreg_1 <- ifelse(pred_1 >= 0.5,1,0) 
pred_logreg_1 <- factor(pred_logreg_1)
#Evaluating the accuracy of Logistic reg model
confusionMatrix(pred_logreg_1, test_1.df$is_patient)

#Accuracy of logistic regression model 1 = 0.6629 
# Sensitivity of logistic regression model 1 = 0.8235
summary(model1)

#___________________________________________________________________________________
#Using p values (< 0.05) to consider only significant variables
# age , sgpt , sgot,tot_prot, albumin         + sgot

#formula for model 2 
mod_f2 <- is_patient ~ age + tot_proteins + albumin + sgpt  + + sgot
model2 <- glm(mod_f2 , data = train_1.df , family = binomial(link = "logit"))
model2
pred_2 <- predict(model2, test_1.df, type = "response")
pred_logreg_2 <- ifelse(pred_2 >= 0.5,1,0) 
pred_logreg_2 <- factor(pred_logreg_2)
#Evaluating the accuracy of Logistic reg model
confusionMatrix(pred_logreg_2, test_1.df$is_patient)
summary(model2)
#Accuracy of logistic regression model 2 = 65.14%
# Sensitivity for logistic regression model 2 is 70.59%


#Therefore model 1 has a better sensitivity and accuracy

#_______________Naive Bayesian model__________________________________________

#Naive Bayes Assumes all columns are independent of each other
#naive bayes performs particularly well if conditional independence exists and only a little training data is available.


NB1 <- naiveBayes(is_patient ~ ., data = train_1.df)
summary(NB1)

pred_nb1 <- predict(NB1, test_1.df,type = "raw")
pred_nb1.df <- data.frame(pred_nb1)
pred_class <- ifelse(pred_nb1.df$X0 > pred_nb1.df$X1 ,0,1)
test_1.df <- cbind(test_1.df, pred_nb1 = pred_class)
pred_class <- factor(pred_class)
#evaluating the accuracy of model
confusionMatrix(pred_class, test_1.df$is_patient)

## Accuracy of naive bayes model 1 = 56.57 %
## Sensitivity : 0.9070  %


#Since naive Bayes assumes that variables are independent we find correlated variables.
#so finding the correlated variables
corrplot(cor(liver_1.df[, c(1,3:10)]), method = "number")
#Correlated variables---> tot_bilirubin - direct_bilirubin , sgpt - sgot
#excluding  direct_bilirubin and sgot

#__________ Naive Bayes model 2 __________________________________

NB2 <- naiveBayes(is_patient ~ age + isFemale + tot_bilirubin + tot_proteins + albumin + ag_ratio + sgot,
                  data = train_1.df)
summary(NB2)

pred_nb2 <- predict(NB2, test_1.df,type = "raw")
pred_nb2.df <- data.frame(pred_nb2)
pred_class <- ifelse(pred_nb2.df$X0 >= pred_nb2.df$X1 , 0,1)
test_1.df <- cbind(test_1.df, pred_nb2 = pred_class)
pred_class <- factor(pred_class)
#evaluating the accuracy of model
confusionMatrix(pred_class, test_1.df$is_patient)
#Accuracy of naive bayesian model 2 = 56.57%  
#Sensitivity : 90.70%          


#evaluating the accuracy of both models
par(mfrow = c(1,1))

ROC_pred1 <- prediction(test_1.df$pred_nb1, test_1.df$is_patient)
ROC_pred1 <- performance(ROC_pred1, 'tpr','fpr')
plot(ROC_pred1, colorize = F, text.adj = c(-0.2,1.7), main = "ROC for Bayesian model 1")

ROC_pred2 <- prediction(test_1.df$pred_nb2, test_1.df$is_patient)
ROC_pred2 <- performance(ROC_pred2, 'tpr','fpr')
plot(ROC_pred2, colorize = F, text.adj = c(-0.2,1.7), main = "ROC for Bayesian model 2")

print(paste("AUC ROC of Bayesian model 1  = ",auc(test_1.df$is_patient, test_1.df$pred_nb1)))
print(paste("AUC ROC of Bayesian model 2  = ",auc(test_1.df$is_patient, test_1.df$pred_nb2)))
#Model 1 provides a slightly better ROC being slightly closer to the top left corner  

############################### Random Forest ###################################
model_rf = train(
  is_patient~., data = training_data,method ='rf', 
  trControl=trctrl, preProcess=c("center","scale")
)
rf_predictions = predict(model_rf, newdata = testing_data)
rf_predictions
rf_result = confusionMatrix(rf_predictions, testing_data[,11])
accuracy[1:11, count] = as.data.frame(rf_result$byClass)
accuracy[12, count] = as.data.frame(rf_result$overall['Accuracy'])
names(accuracy)[count] = "Random Forest"
count = count + 1
print(accuracy)

#################################### Decision Trees #################################
model_dt <- train(
  is_patient ~., data = training_data, method = "rpart",
  parms = list(split = "gini"),
  trControl=trctrl, preProcess = c("center", "scale"), 
  tuneLength = 10
)
prp(model_dt$finalModel, box.palette = "Reds", tweak = 1.2)
dt_predictions = predict(model_dt, newdata = testing_data)
dt_result = confusionMatrix(dt_predictions, testing_data[, 11])
accuracy[1:11, count] = as.data.frame(dt_result$byClass)
accuracy[12, count] = as.data.frame(dt_result$overall['Accuracy'])
names(accuracy)[count] = "Decision Trees"
count = count + 1
print(accuracy)

#_____________________________  KNN _________________________________
# k-nearest neighbors requires no training. k-nearest neighbors can learn non-linear boundaries as well.


library(class)

#Normalizing the numeric data 
normalize <- function(x) {
  return ((x - min(x)) /(max(x) - min(x)))}
liver_1.df[c(1,3:10)]  <- sapply(liver_1.df[c(1,3:10)], normalize)

#Creating a dummy variable for Gender
liver_1.df$isFemale <- ifelse(liver_1.df$gender == 'Female',1,0) 


#Splitting into train and test
train_3.df <- liver_1.df[1:as.integer(0.70*nrow(liver.df)),]
test_3.df <- liver_1.df[-c(1:as.integer(0.70*nrow(liver.df))),]


#________________________ k = 1________________________________________

#Exlcuding first two and 11th feature
pred_knn_1 <- knn(train = train_3.df[,-c(2,11)],
                  test = test_3.df[,-c(2,11)],
                  cl = train_3.df$is_patient ,
                  k = 1)

#Evaluating the accuracy of KNN model
confusionMatrix(pred_knn_1, test_3.df$is_patient)
#               Accuracy : 0.6571          
#            Sensitivity : 0.5385          




#________________________ k = 3_______________________________________

pred_knn_3 <- knn(train = train_3.df[,-c(2,11)],
                  test = test_3.df[,-c(2,11)],
                  cl = train_3.df$is_patient ,
                  k = 3)

#Evaluating the accuracy of KNN model
confusionMatrix(pred_knn_3, test_3.df$is_patient)

#               Accuracy : 0.6571          
#            Sensitivity : 0.5385          


#________________________ k = 5_________________________________________

#Exlcuding first two and 11th feature
pred_knn_5 <- knn(train = train_3.df[,-c(2,11)],
                  test = test_3.df[,-c(2,11)],
                  cl = train_3.df$is_patient ,
                  k = 5)

#Evaluating the accuracy of KNN model
confusionMatrix(pred_knn_5, test_3.df$is_patient)
#               Accuracy : 0.6571          

#               Sensitivity : 0.5385          

#Usually square root of # of observations is taken as k value
sqrt(nrow(liver.df))

#this can be verified looking at the error rate vs k value
#Setting up a holding variable  to store the performance at each K.  
liver_acc <- numeric()
for(i in 1:50){
  #Apply knn with k = i
  predict <- knn(train_3.df[,-c(2,11)], test_3.df[,-c(2,11)], train_3.df$is_patient, k=i)
  liver_acc <- c(liver_acc, mean(predict == test_3.df$is_patient))
}
#Plot k= 1 through 30
plot(1-liver_acc,type="l",ylab="Error Rate", xlab="K",main="Error Rate for liver patient With Varying K")



#___________k = 24 square root of (# of observations in liver.df)________________

#Exlcuding first two and 11th feature
pred_knn_24 <- knn(train = train_3.df[,-c(2,11)],
                   test = test_3.df[,-c(2,11)],
                   cl = train_3.df$is_patient ,
                   k = 24)

#Evaluating the accuracy of KNN model
confusionMatrix(pred_knn_24, test_3.df$is_patient)

#               Accuracy : 0.7086          
#            Sensitivity : 0.07692         



#________________________________ Support Vector machine_________________________________________
#High accuracy. works well even when data isnt linearly seperable, allows for non linear/radial/polynomial curves. However, requires high computation power

#We will run an SVM model using an RBF

#we will first obtain a copy of the NA corrected dataframe and upsample it. This is needed since dataset has too few negatives to train with, and balancing this bias helps in a better trained model.
liversvm <- data.frame(liver.df)

liversvm$is_patient <- factor(liversvm$is_patient)
liversvm <- upSample(x = liversvm, liversvm$is_patient)
liversvm <- liversvm[sample(nrow(liversvm)),]

liversvm <- subset(liversvm[c(1:11)])
intermediate2 <- createDataPartition(y = liversvm$is_patient, p=0.7, list=FALSE)

training <- liversvm[intermediate2,]
testing <- liversvm[-intermediate2,]
training[["is_patient"]] = factor(training[["is_patient"]])

#data successfully partitioned

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(7777)
#The method parameter holds the details about resampling method. We can set method with many values like  boot, boot632, cv, repeatedcv, LOOCV, LGOCV etc
#repeatedcv stands for repeated cross validation. The cross-validation process is repeated k times, with each of the k created subsamples used exactly once as the validation data.



svmModel <- train(is_patient ~., data = training, method = "svmRadial",
                  trControl=trctrl,
                  preProcess = c("center", "scale"),
                  tuneLength = 20)
#center & scale help for centering and scaling the data. After preProcessing these convert our training data with mean value as approximately 0 and standard deviation as â1â.


svmModel

#Since the harm of a false negative is not always as bad as a false positive in a scenario(or vice versa), adjusting "C values" or cost values helps to get a model better suited to our needs by weighting different errors with different costs.
#The code uses several C values to get optimal accuracy.

test_pred <- predict(svmModel,newdata = testing)
test_pred 
#Cecking our results by predicting the training dataset


confusionMatrix(test_pred, testing$is_patient )

#we get an accuracy of 0.8024
# sensitivity = 0.8629

