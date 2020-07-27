concrete <- read.csv(file.choose())
View(concrete)
str(concrete)
summary(concrete)
attach(concrete)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))

######## Split data into Train & Test by Random split method (80/20 ratio) ######
library(caTools)
split<-sample.split(concrete_norm$strength,SplitRatio = 0.80)
split
table(split) # F=206 , T=824 Just to check the split ratio

concrete_train<-subset(concrete_norm,split==TRUE)
concrete_test<-subset(concrete_norm,split==FALSE)

###### Building model using Neural Networks "Model" #####

install.packages("neuralnet")
library(neuralnet)  # regression

concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
str(concrete_model)
plot(concrete_model) # Error 5.34 , Steps=24146

# Predict & Accuracy on Train data 

train_results<-compute(concrete_model,concrete_train[1:8])
str(train_results)
cor(train_results$net.result,concrete_train$strength) # 83.27% 
plot(train_results$net.result,concrete_train$strength, col="blue") 

# Predict & Accuracy on Test data

test_results <- compute(concrete_model,concrete_test[1:8])
str(test_results)
cor(test_results$net.result,concrete_test$strength) # 81.87%
plot(test_results$net.result,concrete_test$strength, col="red")

######## Improve Further the model to improve the accuracy "Model1"######

concrete_model1<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_train,hidden = c(3,3))
plot(concrete_model1) # Error 2.35 , Steps=7337

# Predict & Accuracy on Train data 

train_results1<-compute(concrete_model1,concrete_train[1:8])
cor(train_results1$net.result,concrete_train$strength) # 93.02% 
plot(train_results1$net.result,concrete_train$strength,col="blue")

# Predict & Accuracy on Test data

test_results1 <- compute(concrete_model1,concrete_test[1:8])
cor(test_results1$net.result,concrete_test$strength) # 93.21%
plot(test_results1$net.result,concrete_test$strength,col="red")

# observation Model->Model1 (hidden layer 3,3) :
# SSE : 5.34 -> 2.35 and training steps : 24146->7337
# Accuracy on Training data : 83.27% -> 93.02% 
# Accuracy on Test data     : 81.87% -> 93.21%

######## Improve Further the model to improve the accuracy "Model2"######

concrete_model2<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_train,hidden = c(5,5,5))
plot(concrete_model2) # Error 1.79 , Steps 10938

# Predict & Accuracy on Train data 

train_results2<-compute(concrete_model2,concrete_train[1:8])
cor(train_results2$net.result,concrete_train$strength) # 94.72% 
plot(train_results2$net.result,concrete_train$strength,col="blue")

# Predict & Accuracy on Test data

test_results2 <- compute(concrete_model2,concrete_test[1:8])
cor(test_results2$net.result,concrete_test$strength) # 94.11%
plot(test_results2$net.result,concrete_test$strength,col="red")

# observation Model->Model1 (3,3)->Model2 (5,5,5) :
# SSE : 5.34 -> 2.35->1.79 and training steps : 24146->7337->10938
# Accuracy on Training data : 83.27% -> 93.02% -> 94.72%
# Accuracy on Test data     : 81.87% -> 93.21% -> 94.11%

######## Improve Further the model to improve the accuracy "Model3"######

concrete_model3<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_train,hidden = c(8,8,8))
plot(concrete_model3) # Error 0.87 , Steps 7845

# Predict & Accuracy on Train data 

train_results3<-compute(concrete_model3,concrete_train[1:8])
cor(train_results3$net.result,concrete_train$strength) # 97.45% 
plot(train_results3$net.result,concrete_train$strength,col="blue")

# Predict & Accuracy on Test data

test_results3 <- compute(concrete_model3,concrete_test[1:8])
cor(test_results3$net.result,concrete_test$strength) # 95.12%
plot(test_results3$net.result,concrete_test$strength,col="red")

# observation Model->Model1 (3,3)->Model2 (5,5,5)->Model3(8,8,8) :
# SSE : 5.34 -> 2.35->1.79->0.87 and training steps : 24146->7337->10938->7845
# Accuracy on Training data : 83.27% -> 93.02% -> 94.72%-> 97.45%
# Accuracy on Test data     : 81.87% -> 93.21% -> 94.11%-> 95.12%
