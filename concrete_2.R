concrete <- read.csv(file.choose())
View(concrete)
str(concrete)
summary(concrete)
attach(concrete)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))

######## Split data into Train & Test by Stratified method (75/25 ratio) ######

concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

###### Building model using Neural Networks "Model" #####

install.packages("neuralnet")
library(neuralnet)  # regression

concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
str(concrete_model)
plot(concrete_model) # Error 5.66 , Steps=868

# Predict & Accuracy on Train data 

train_results<-compute(concrete_model,concrete_train[1:8])
str(train_results)
cor(train_results$net.result,concrete_train$strength) # 83.57% 
plot(train_results$net.result,concrete_train$strength, col="blue") 

# Predict & Accuracy on Test data

test_results <- compute(concrete_model,concrete_test[1:8])
str(test_results)
cor(test_results$net.result,concrete_test$strength) # 72.58%
plot(test_results$net.result,concrete_test$strength, col="red")

######## Improve Further the model to improve the accuracy "Model1"######

concrete_model1<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_train,hidden = c(2,2))
plot(concrete_model1) # Error 2.27 , Steps=6693

# Predict & Accuracy on Train data 

train_results1<-compute(concrete_model1,concrete_train[1:8])
cor(train_results1$net.result,concrete_train$strength) # 93.75% 
plot(train_results1$net.result,concrete_train$strength,col="blue")

# Predict & Accuracy on Test data

test_results1 <- compute(concrete_model1,concrete_test[1:8])
cor(test_results1$net.result,concrete_test$strength) # 75.10%
plot(test_results1$net.result,concrete_test$strength,col="red")

# observation Model->Model1 (hidden layer 2,2) :
# SSE : 5.66 -> 2.27 and training steps : 868->6693
# Accuracy on Training data : 83.57% -> 93.75% 
# Accuracy on Test data     : 72.58% -> 75.10%

######## Improve Further the model to improve the accuracy "Model2"######

concrete_model2<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_train,hidden = c(5,5))
plot(concrete_model2) # Error 1.37 , Steps 14321

# Predict & Accuracy on Train data 

train_results2<-compute(concrete_model2,concrete_train[1:8])
cor(train_results2$net.result,concrete_train$strength) # 96.27% 
plot(train_results2$net.result,concrete_train$strength,col="blue")

# Predict & Accuracy on Test data

test_results2 <- compute(concrete_model2,concrete_test[1:8])
cor(test_results2$net.result,concrete_test$strength) # 74.58%
plot(test_results2$net.result,concrete_test$strength,col="red")

# observation Model->Model1(2,2)->Model2 (5,5):
# SSE : 5.66->2.27->1.37 and training steps : 868->6693->14321
# Accuracy on Training data : 83.57% -> 93.75% -> 96.27%
# Accuracy on Test data     : 72.58% -> 75.10% -> 74.58%

######## Improve Further the model to improve the accuracy "Model3"######

concrete_model3<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_train,hidden = c(5,5,5))
plot(concrete_model3) # Error 1.48 , Steps 4305

# Predict & Accuracy on Train data 

train_results3<-compute(concrete_model3,concrete_train[1:8])
cor(train_results3$net.result,concrete_train$strength) # 95.96% 
plot(train_results3$net.result,concrete_train$strength,col="blue")

# Predict & Accuracy on Test data

test_results3 <- compute(concrete_model3,concrete_test[1:8])
cor(test_results3$net.result,concrete_test$strength) # 81.18%
plot(test_results3$net.result,concrete_test$strength,col="red")

# observation Model->Model1(2,2)->Model2 (5,5)-> Model3 (5,5,5):
# SSE : 5.66->2.27->1.37->1.48 and training steps : 868->6693->14321->4305
# Accuracy on Training data : 83.57% -> 93.75% -> 96.27% ->95.96%
# Accuracy on Test data     : 72.58% -> 75.10% -> 74.58% ->81.18% 

