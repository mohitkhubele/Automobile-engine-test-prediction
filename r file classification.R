mydata<-read.csv("file:///D:/r project/project on 27-9/Train.csv")
testdata<-read.csv("file:///D:/r project/project on 27-9/Test.csv")
admydata<-read.csv("file:///D:/r project/project on 27-9/Train_AdditionalData.csv")
addtrain<-mutate(admydata,'A'=1)
library(sqldf)
df1<-sqldf("SELECT mydata.ID, addtrain.A
FROM mydata
           LEFT JOIN addtrain ON mydata.ID = addtrain.TestA")
df1$A[is.na(df1$A)] <- "fail"
df1$A[df1$A==1]<-"pass"
str(df1)
df1$A<-as.factor(df1$A)
#testb
addtrain<-mutate(addtrain,'B'=1)
library(sqldf)
df2<-sqldf("SELECT mydata.ID, addtrain.B
           FROM mydata
           LEFT JOIN addtrain ON mydata.ID = addtrain.TestB")
df2$B[is.na(df2$B)] <- "fail"
df2$B[df2$B==1]<-"pass"
str(df2)
df1$A<-as.factor(df1$A)
mydata<-cbind(mydata,df1$A,df2$B)
#standardise
y<-mydata$y
mydata2<-mydata[,-c(1)]



 for (i in 1:ncol(mydata2)) {
   mydata2[,]=as.numeric(mydata2[,i])
   
 }
str(mydata2)

for (i in 1:ncol(mydata2)) {
  mydata2[,]=as.factor(as.character(mydata2[,i]))
  
}
###

summary(mydata2)
sum(is.na(mydata$y))
mydata<-mydata[,-c(1)]
mydata$Number.of.Cylinders<-as.factor(mydata$Number.of.Cylinders)
str(mydata)

#handle missing vale with central imputation
library(DMwR)
traindata<-centralImputation(mydata)
summary(traindata)

#split data
set.seed(1234)
ind<-sample(2,nrow(traindata),replace = T,prob = c(0.8,0.2))
train<-traindata[ind==1,]
val<-traindata[ind==2,]

#using logistic algorithm
log_reg <- glm(y ~ ., data = train, family = "binomial")
summary(log_reg) # model is not significant 

#build a model with significant variable
logreg1<-glm(train$y~ Number.of.Cylinders + material.grade + Bearing.Vendor + Fuel.Type + Compression.ratio + Cylinder.deactivation + Direct.injection
                  + Peak.Power + Liner.Design., data = train, family = "binomial")
summary(logreg1)
library(caret)
pred<-predict(log_reg, type="response", newdata=train)
pred<-ifelse(pred>0.5,"pass","fail")
pred<-as.factor(pred)
confusionMatrix(train$y,pred)

table(train$y)


#randomforest
library("randomForest")
rfmodel<-randomForest(y~.,train)
plot(rfmodel)
attributes(rfmodel)
tuneinrf<-tuneRF(train[,-21], train[,21],ntreeTry = 500, stepFactor=0.5,improve = 0.05,trace = TRUE)
rfmodel1<-randomForest(y~.,train,ntree=500,mtry=4)
plot(rfmodel1)
attributes(rfmodel1)



predrf1<-predict(rfmodel1,train)

confusionMatrix(predrf1,train$y)


#prediction on test data which have no dependent variable
summary(testdata)
sum(is.na(testdata))
ID<-testdata[,c(1)]
testdata<-testdata[,-c(1)]
testdata$Number.of.Cylinders<-as.factor(testdata$Number.of.Cylinders)
str(testdata)
testdata<-centralImputation(testdata)
predtest<-predict(logreg1,testdata)
predtest<-ifelse(predtest>0.5,"pass","fail")
predtest<-as.factor(predtest)
predtest<-data.frame(predtest)
finaltest<-cbind(predtest,testdata)
finaltest<-cbind(ID,finaltest)
