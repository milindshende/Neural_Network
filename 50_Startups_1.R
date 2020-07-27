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

######## Split data into Train & Test by Random split method (70/30 ratio) ######
library(caTools)
split<-sample.split(startup_norm$Profit,SplitRatio = 0.70)
split
table(split) # F=15 , T=35 Just to check the split ratio

startup_train<-subset(startup_norm,split==TRUE)
startup_test<-subset(startup_norm,split==FALSE)

###### Building model using Neural Networks "Model" #####

install.packages("neuralnet")
library(neuralnet)  # regression

startup_model <- neuralnet(Profit~.,data = startup_train)
str(startup_model)
plot(startup_model) # Error 0.06 , Steps=147

# Predict & Accuracy on Train data 

train_results<-compute(startup_model,startup_train)
str(train_results)
cor(train_results$net.result,startup_train$Profit) # 97.06% 
plot(train_results$net.result,startup_train$Profit, col="purple")

# Predict & Accuracy on Test data

test_results <- compute(startup_model,startup_test)
str(test_results)
cor(test_results$net.result,startup_test$Profit) # 95.71%
plot(test_results$net.result,startup_test$Profit,col="green")

######## Improve Further the model to improve the accuracy "Model1"######

startup_model1<-neuralnet(Profit~.,data= startup_train,hidden = c(2,2))
plot(startup_model1) # Error 0.044 , Steps=372

# Predict & Accuracy on Train data 

train_results1<-compute(startup_model1,startup_train)
cor(train_results1$net.result,startup_train$Profit) # 97.87% 
plot(train_results1$net.result,startup_train$Profit,col="blue")

# Predict & Accuracy on Test data

test_results1 <- compute(startup_model1,startup_test)
cor(test_results1$net.result,startup_test$Profit) # 97.15%
plot(test_results1$net.result,startup_test$Profit,col="red")

# observation Model->Model1 (hidden layer 2,2) :
# SSE : 0.06 -> 0.04 and training steps : 147->372
# Accuracy on Training data : 97.06% -> 97.87% 
# Accuracy on Test data     : 95.71% -> 97.15%

######## Improve Further the model to improve the accuracy "Model2"######

startup_model2<-neuralnet(Profit~.,data= startup_train,hidden = c(5,5,2))
plot(startup_model2) # Error 0.04 , Steps 130

# Predict & Accuracy on Train data 

train_results2<-compute(startup_model2,startup_train)
cor(train_results2$net.result,startup_train$Profit) # 97.88% 
plot(train_results2$net.result,startup_train$Profit,col="blue")

# Predict & Accuracy on Test data

test_results2 <- compute(startup_model2,startup_test)
cor(test_results2$net.result,startup_test$Profit) # 96.87%
plot(test_results2$net.result,startup_test$Profit,col="red")

# observation Model->Model1 (2,2)->Model2 (5,5,2) :
# SSE : 0.06->0.04->0.04 and training steps : 147->372->130
# Accuracy on Training data : 97.06% -> 97.87%-> 97.88
# Accuracy on Test data     : 95.71% -> 97.15%-> 96.87
