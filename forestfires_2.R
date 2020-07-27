ff <- read.csv(file.choose())
View(ff)
ff<-ff[-c(1:2)]
str(ff)
summary(ff)
attach(ff)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

ff_norm<-as.data.frame(lapply(ff[1:28],FUN=normalize))
ff_norm<-cbind(ff_norm,ff[29])

######## Split data into Train & Test by stratified method (70/30 ratio) ######

ff_train<-ff_norm[1:362,]
ff_test<-ff_norm[363:517,]

###### Building "Model" using Neural Networks #####

install.packages("nnet")
library(nnet)  # classification technique due to Y variable is categorial

ff_model<-nnet(size_category~.,data = ff_train,size=2)
str(ff_model)

# Predict & Accuracy on Train data 
pred_train<-predict(ff_model,ff_train[-29],type="class")
table_train<-table(pred_train,ff_train$size_category)
table_train
accuracy_train<-(sum(diag(table_train))/sum(table_train))
accuracy_train  # 99.44%

# Predict & Accuracy on Test data
pred_test<-predict(ff_model,ff_test[-29],type = "class")
table_test<-table(pred_test,ff_test$size_category)
table_test
accuracy_test<-(sum(diag(table_test))/sum(table_test))
accuracy_test  #90.96%

######## Build "Model1"######

ff_model1<-nnet(size_category~.,data = ff_train,size=2,rang=1)
str(ff_model1)

# Predict & Accuracy on Train data 
pred_train1<-predict(ff_model1,ff_train[-29],type="class")
table_train1<-table(pred_train1,ff_train$size_category)
table_train1
accuracy_train1<-(sum(diag(table_train1))/sum(table_train1))
accuracy_train1  # 99.72%

# Predict & Accuracy on Test data
pred_test1<-predict(ff_model1,ff_test[-29],type = "class")
table_test1<-table(pred_test1,ff_test$size_category)
table_test1
accuracy_test1<-(sum(diag(table_test1))/sum(table_test1))
accuracy_test1  #89.03%

# observation Model->Model1 :
# Accuracy on Training data : 99.44% -> 99.72% 
# Accuracy on Test data     : 90.96% -> 89.03%

######## Build "Model2"######

ff_model2<-nnet(size_category~.,data = ff_train,size=2,rang=1,maxit=200)
str(ff_model2)

# Predict & Accuracy on Train data 
pred_train2<-predict(ff_model2,ff_train[-29],type="class")
table_train2<-table(pred_train2,ff_train$size_category)
table_train2
accuracy_train2<-(sum(diag(table_train2))/sum(table_train2))
accuracy_train2  # 95.85%

# Predict & Accuracy on Test data
pred_test2<-predict(ff_model2,ff_test[-29],type = "class")
table_test2<-table(pred_test2,ff_test$size_category)
table_test2
accuracy_test2<-(sum(diag(table_test2))/sum(table_test2))
accuracy_test2  #76.77%

# observation Model->Model1 :
# Accuracy on Training data : 99.44% -> 99.72% -> 95.85%
# Accuracy on Test data     : 90.96% -> 89.03% -> 76.77%

######## Build "Model3"######

ff_model3<-nnet(size_category~.,data = ff_train,size=2,rang=1,decay=5e-4,maxit=200)
str(ff_model3)

# Predict & Accuracy on Train data 
pred_train3<-predict(ff_model3,ff_train[-29],type="class")
table_train3<-table(pred_train3,ff_train$size_category)
table_train3
accuracy_train3<-(sum(diag(table_train3))/sum(table_train3))
accuracy_train3  # 99.72%

# Predict & Accuracy on Test data
pred_test3<-predict(ff_model3,ff_test[-29],type = "class")
table_test3<-table(pred_test3,ff_test$size_category)
table_test3
accuracy_test3<-(sum(diag(table_test3))/sum(table_test3))
accuracy_test3  #84.51%

# observation Model->Model1 :
# Accuracy on Training data : 99.44% -> 99.72% -> 95.85% -> 99.72%
# Accuracy on Test data     : 90.96% -> 89.03% -> 76.77% -> 84.51%
