startup <- read.csv(file.choose())
View(startup)
startup<-startup[-4]
str(startup)
summary(startup)
attach(startup)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

startup_norm<-as.data.frame(lapply(startup,FUN=normalize))

######## Split data into Train & Test by stratified method (80/20 ratio) ######

startup_train<-startup_norm[1:40,]
startup_test<-startup_norm[41:50,]

###### Building model using Neural Networks "Model" #####

install.packages("neuralnet")
library(neuralnet)  # regression

startup_model <- neuralnet(Profit~.,data = startup_train)
str(startup_model)
plot(startup_model) # Error 0.03 , Steps=322

# Predict & Accuracy on Train data 

train_results<-compute(startup_model,startup_train)
str(train_results)
cor(train_results$net.result,startup_train$Profit) # 97.16% 
plot(train_results$net.result,startup_train$Profit, col="purple")

# Predict & Accuracy on Test data

test_results <- compute(startup_model,startup_test)
str(test_results)
cor(test_results$net.result,startup_test$Profit) # 75.03%
plot(test_results$net.result,startup_test$Profit,col="green")

######## Improve Further the model to improve the accuracy "Model1"######

startup_model1<-neuralnet(Profit~.,data= startup_train,hidden = c(5,5))
plot(startup_model1) # Error 0.03 , Steps=263

# Predict & Accuracy on Train data 

train_results1<-compute(startup_model1,startup_train)
cor(train_results1$net.result,startup_train$Profit) # 97.26% 
plot(train_results1$net.result,startup_train$Profit,col="blue")

# Predict & Accuracy on Test data

test_results1 <- compute(startup_model1,startup_test)
cor(test_results1$net.result,startup_test$Profit) # 72.57%
plot(test_results1$net.result,startup_test$Profit,col="red")

# observation Model->Model1 (hidden layer 5,5) :
# SSE : 0.03 -> 0.03 and training steps : 322->269
# Accuracy on Training data : 97.16% -> 97.26% 
# Accuracy on Test data     : 75.03% -> 72.57%

######## Improve Further the model to improve the accuracy "Model2"######

startup_model2<-neuralnet(Profit~.,data= startup_train,hidden = c(2,2,2))
plot(startup_model2) # Error 0.03 , Steps 114

# Predict & Accuracy on Train data 

train_results2<-compute(startup_model2,startup_train)
cor(train_results2$net.result,startup_train$Profit) # 96.98% 
plot(train_results2$net.result,startup_train$Profit,col="blue")

# Predict & Accuracy on Test data

test_results2 <- compute(startup_model2,startup_test)
cor(test_results2$net.result,startup_test$Profit) # 77.13%
plot(test_results2$net.result,startup_test$Profit,col="red")

# observation Model->Model1 (5,5)-> Model2 (2,2,2) :
# SSE : 0.03 -> 0.03->0.03 and training steps : 322->269->114
# Accuracy on Training data : 97.16% -> 97.26% -> 96.98%
# Accuracy on Test data     : 75.03% -> 72.57% -> 77.13%
