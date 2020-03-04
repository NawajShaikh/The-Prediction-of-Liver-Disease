library(readr)
Liver_Data <- read_csv("C:\\Users\\Shaikh Nawaj\\Desktop\\project\\Indian Liver Patient Dataset.csv")
View(Liver_Data)

Liver_Data["Class"] <- ifelse(Liver_Data$Dataset == 2,"not_patient","patient")
# 1 is patient
# 0 is not patient
Liver_Data["female"] <- ifelse(Liver_Data$Gender == "Female",1,0)
Liver_Data["Male"] <- ifelse(Liver_Data$Gender == "Male",1,0)
str(Liver_Data)
sum(is.na(Liver_Data))
summary(Liver_Data)
boxplot(Liver_Data$Albumin_and_Globulin_Ratio)
#alkphos has 4 missing values
#Replacing with median because alkphos contains outliers

ag_median <- median(Liver_Data$Albumin_and_Globulin_Ratio,na.rm = T)

Liver_Data$Albumin_and_Globulin_Ratio[is.na(Liver_Data$Albumin_and_Globulin_Ratio)] <- ag_median
sum(is.na(Liver_Data))

#Creating factor levels for is_patient
Liver_Data["Class"] <- factor(Liver_Data$Class)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Liver_Data_norm<-as.data.frame(lapply(Liver_Data[,-c(2,11:14)],FUN=normalize))
liver <- data.frame(Liver_Data_norm,Liver_Data[,c(13,14,12)]) 

liver_patient <- liver[liver$Class == "patient",]
liver_not_patient <- liver[liver$Class == "not_patient",]

liver_train <- rbind(liver_patient[1:291,],liver_not_patient[1:117,])
liver_test <- rbind(liver_patient[292:416,],liver_not_patient[118:167,])

# Using multilayered feed forward nueral network
# package nueralnet
# install.packages("neuralnet")
library(neuralnet)

# Building model
formula_nn <- paste("Class",paste(colnames(liver[-12]),collapse ="+"),sep="~")
liver_model <- neuralnet(formula_nn,data=liver_train)
str(liver_model)
plot(liver_model)


set.seed(7)
model_results <- predict(liver_model,liver_test[1:11]) #we use predict instead of compute
library(dplyr)
predicted_size_category <- ifelse(model_results[,1] > model_results[,2],"not_patient","patient")
# model_results$neurons
mean(predicted_size_category==liver_test$Class) #Testing Accuracy = 73% 
