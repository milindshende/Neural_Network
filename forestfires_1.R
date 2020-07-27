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

######## Split data into Train & Test by Random split method (80/20 ratio) ######

library(caTools)
split<-sample.split(ff_norm$size_category,SplitRatio = 0.80)
split
table(split) # F=104 , T=413 Just to check the split ratio

ff_train<-subset(ff_norm,split==TRUE)
ff_test<-subset(ff_norm,split==FALSE)

###### Building "Model" using Neural Networks #####

install.packages("nnet")
library(nnet)  # classification technique due to Y variable is categorial

ff_model<-nnet(size_category~.,data = ff_train,size=5)
str(ff_model)

# Predict & Accuracy on Train data 
pred_train<-predict(ff_model,ff_train[-29],type="class")
table_train<-table(pred_train,ff_train$size_category)
table_train
accuracy_train<-(sum(diag(table_train))/sum(table_train))
accuracy_train  # 99.75%

# Predict & Accuracy on Test data
pred_test<-predict(ff_model,ff_test[-29],type = "class")
table_test<-table(pred_test,ff_test$size_category)
table_test
accuracy_test<-(sum(diag(table_test))/sum(table_test))
accuracy_test  #93.26%

######## Build "Model1"######

help("nnet")
ff_model1<-nnet(size_category~.,data = ff_train,size=5,rang=1)
str(ff_model1)

# Predict & Accuracy on Train data 
pred_train1<-predict(ff_model1,ff_train[-29],type="class")
table_train1<-table(pred_train1,ff_train$size_category)
table_train1
accuracy_train1<-(sum(diag(table_train1))/sum(table_train1))
accuracy_train1  # 99.51%

# Predict & Accuracy on Test data
pred_test1<-predict(ff_model1,ff_test[-29],type = "class")
table_test1<-table(pred_test1,ff_test$size_category)
table_test1
accuracy_test1<-(sum(diag(table_test1))/sum(table_test1))
accuracy_test1  #95.19%

# observation Model->Model1 :
# Accuracy on Training data : 99.75% -> 99.51% 
# Accuracy on Test data     : 93.26% -> 95.19%

######## Build "Model2"######

ff_model2<-nnet(size_category~.,data = ff_train,size=5,rang=1,maxit=300)
str(ff_model2)

# Predict & Accuracy on Train data 
pred_train2<-predict(ff_model2,ff_train[-29],type="class")
table_train2<-table(pred_train2,ff_train$size_category)
table_train2
accuracy_train2<-(sum(diag(table_train2))/sum(table_train2))
accuracy_train2  # 100%

# Predict & Accuracy on Test data
pred_test2<-predict(ff_model2,ff_test[-29],type = "class")
table_test2<-table(pred_test2,ff_test$size_category)
table_test2
accuracy_test2<-(sum(diag(table_test2))/sum(table_test2))
accuracy_test2  #95.19%

# observation Model->Model1->Model2 :
# Accuracy on Training data : 99.75% -> 99.51% -> 100% 
# Accuracy on Test data     : 93.26% -> 95.19% -> 95.19

######## Build "Model3"######

ff_model3<-nnet(size_category~.,data = ff_train,size=5,rang=1,decay=5e-4,maxit=300)
str(ff_model3)

# Predict & Accuracy on Train data 
pred_train3<-predict(ff_model3,ff_train[-29],type="class")
table_train3<-table(pred_train3,ff_train$size_category)
table_train3
accuracy_train3<-(sum(diag(table_train3))/sum(table_train3))
accuracy_train3  # 100%

# Predict & Accuracy on Test data
pred_test3<-predict(ff_model3,ff_test[-29],type = "class")
table_test3<-table(pred_test3,ff_test$size_category)
table_test3
accuracy_test3<-(sum(diag(table_test3))/sum(table_test3))
accuracy_test3  #88.46%

# observation Model->Model1->Model2->Model3 :
# Accuracy on Training data : 99.75% -> 99.51% -> 100% -> 100%
# Accuracy on Test data     : 93.26% -> 95.19% -> 95.19 -> 88.46%
