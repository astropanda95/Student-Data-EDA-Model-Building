#STAT 515 Final Project
#John Victor Kanaparthy (G01283591)
#Sahith Reddy Battapuram (G01337411)
#Venkata Abhilaash Annamreddi (G01336658)

#Calling relevant libraries
library(tidyverse)
library(leaps)
library(glmnet)
library(GGally)
library(corrplot)
library(rpart)
library(rpart.plot)
library(DMwR2)
library(tidyr)
library(ggplot2)
library(Amelia)
library(glmnet)

#Reading the CSV file
data<- read.csv("Final_data.csv",header=TRUE)
str(data)
data$MAIN<-as.factor(data$MAIN)
data$PREDDEG<-as.factor(data$PREDDEG)
data$HIGHDEG<-as.factor(data$HIGHDEG)
data$CONTROL<-as.factor(data$CONTROL)
data$LOCALE<-as.factor(data$LOCALE)
data$REGION<-as.factor(data$REGION)
data$DEBT_MDN<-as.numeric(data$DEBT_MDN)
data$INSTNM<-as.factor(data$INSTNM)
data$CITY<-as.factor(data$CITY)
data$ACCREDAGENCY<-as.factor(data$ACCREDAGENCY)
str(data)

#Exploratory 

missmap(data,y.at=c(1),y.labels = c(),col=c('Yellow','Black'))
names(data)

ggplot(data,aes(ADM_RATE))+geom_histogram(aes(fill=CONTROL),bins='30')+
  ggtitle('ADM_RATE AS PART OF CONTROL(Private,public,profit)')

ggplot(data,aes(TUITFTE))+geom_boxplot(aes(fill=REGION),color='black')+
  theme_bw()

ggplot(data,aes(SAT_AVG,TUITIONFEE_IN))+
  geom_point(aes(fill=HIGHDEG),shape=21,color='black')+
  ggtitle('SATAVG VS INSTATE TUTION FEE')

ggplot(data,aes(SAT_AVG,TUITIONFEE_OUT))+
  geom_point(aes(fill=PREDDEG),shape=21,color='black')+
  ggtitle('SATAVG VS OUTSTATE')

ggplot(data,aes(MD_FAMINC,DEBT_MDN))+
  geom_point(aes(fill=HIGHDEG),shape=21)+
  ggtitle('MD_FAMINC VS DEBT_MDN')

vars <- dplyr::select(data,-LATITUDE,-LONGITUDE,-UNITID,-INSTNM,-DISTANCEONLY,-CITY)
GGally::ggpairs(vars,
           lower=list(continuous='blank'
                      ,
                      combo='blank'
                      ,
                      discrete='blank'),
           upper=list(continuous="points"
                      , combo="facethist", discrete="facetbar"),
           switch="y")


#Creating a dataset removing unnecessary variables
data2<-select(data,-LATITUDE,-LONGITUDE,-UNITID,-DISTANCEONLY,
              -INSTNM,-DISTANCEONLY,-CITY,-MD_FAMINC)
str(data2)

#Partitioning data
set.seed(1)
test <- sample(1:nrow(data2), size=nrow(data2)/2) 
train <- (-test)


#Buliding models for median debt

#Using random forest
library(randomForest)
set.seed(3)
rf.data=randomForest(DEBT_MDN~.,data=data2,subset = train,importance=TRUE)
rf.data

#Computing test set MSE
rf.test=data2[-train,"DEBT_MDN"]
yhat.bag = predict(rf.data,newdata=data2[-train,]) 
mean((yhat.bag-rf.test)^2)
#MSE 7313246
#RMSE 2704.3013885

#Getting the most important predictors
varImpPlot(rf.data,
           main="Plotting importance measures for the data (Median Debt)")

#Creating a random forest model with 8 most important variables
set.seed(3)
rf.data2=randomForest(DEBT_MDN~TUITFTE+FAMINC+TUITIONFEE_IN+REGION+C150_4+
                        INEXPFTE+AVGFACSAL+SAT_AVG,
                        data=data2,subset=train,importance=TRUE)
rf.data2
yhat.rf.data2 = predict(rf.data2, newdata=data2[-train,])
rf.test2=data2[-train,"DEBT_MDN"]
mean((yhat.rf.data2-rf.test2)^2)
#MSE 8177772
#RMSE 2859.680

#Building multiple linear regression model
linear_model<- lm(DEBT_MDN~.,data=data2)
summary(linear_model)
linear_summary<-summary(linear_model)
mean(linear_summary$residuals^2)
sqrt(mean(linear_summary$residuals^2))
#MSE 7037914
#RMSE 2652.907

#Building multiple linear regression model using 8 most important variables
linear_model2<- lm(DEBT_MDN~TUITFTE+FAMINC+TUITIONFEE_IN+REGION+C150_4+
                     INEXPFTE+AVGFACSAL+SAT_AVG,data=data2)
summary(linear_model2)
linear_summary2<-summary(linear_model2)
mean(linear_summary2$residuals^2)
sqrt(mean(linear_summary2$residuals^2))
#MSE 8890161
#RMSE 2981.637


#Using classification tree (rpart) model
set.seed(1)
#Calling rpart()with small cp to create object for call to plotcp()
rpart.data=rpart(DEBT_MDN~.
                    ,data = data2,
                    method="anova", cp=0.001)


#Pruning data with se=1
rpart.data.1se <- rt.prune(rpart.data, se=1)
printcp(rpart.data.1se)
#MSE 9340200.4189
#RMSE 3056.4189

#rpart testing validation set MSE
set.seed(1)
rpart.data2.train = rpart(DEBT_MDN~., data = data2[train,],
                           method="anova", cp=0)
rpart.data2.train = rt.prune(rpart.data2.train, se=1)
yhat=predict(rpart.data2.train, newdata = data2[-train,])
data2.test = data2[-train,"DEBT_MDN"]
(MSE = mean((yhat-data2.test)^2))
sqrt(MSE)
#MSE 9899079
#RMSE 3146.28

#Results:                                                  MSE         RMSE
#RandomForest Test-Set MSE                               7313246   2704.3013885
#RandomForest Test-Set MSE(8 most-important)             8177772   2859.680
#Multiple Linear Regression                              7037914   2652.907
#Multiple Linear Regression(8 most-important)            8890161   2981.637
#Classification Tree Test-Set MSE                        9899079   3146.28




#Building model for completion rate

set.seed(3)
rf.data3=randomForest(C150_4~.,data=data2,subset = train,importance=TRUE)
rf.data3

#Computing test set MSE
rf.test3=data2[-train,"C150_4"]
yhat.bag3 = predict(rf.data3,newdata=data2[-train,]) 
mean((yhat.bag3-rf.test3)^2)
sqrt(mean((yhat.bag3-rf.test3)^2))
#MSE 0.005928073
#RMSE 0.07699398

#Getting the most important predictors
varImpPlot(rf.data3,
           main="Plotting importance measures for the data (Completion Rate)")

#Creating a random forest model with 8 most important variables
set.seed(3)
rf.data4=randomForest(C150_4~SAT_AVG+FAMINC+AVGFACSAL+DEBT_MDN+PCTPELL+
                        INEXPFTE+TUITIONFEE_OUT+ADM_RATE,
                      data=data2,subset=train,importance=TRUE)
rf.data4
yhat.rf.data4 = predict(rf.data4, newdata=data2[-train,])
rf.test4=data2[-train,"C150_4"]
mean((yhat.rf.data4-rf.test4)^2)
sqrt(mean((yhat.rf.data4-rf.test4)^2))
#MSE 0.006424464
#RMSE 0.08015276

#Building multiple linear regression model
linear_model3<- lm(C150_4~.,data=data2)
summary(linear_model3)
linear_summary3<-summary(linear_model3)
mean(linear_summary3$residuals^2)
sqrt(mean(linear_summary3$residuals^2))
#MSE 0.005196075
#RMSE 0.07208381

#Building multiple linear regression model using 8 most important variables
linear_model4<- lm(C150_4~SAT_AVG+FAMINC+AVGFACSAL+DEBT_MDN+PCTPELL+
                     INEXPFTE+TUITIONFEE_OUT+ADM_RATE,data=data2)
summary(linear_model4)
linear_summary4<-summary(linear_model4)
mean(linear_summary4$residuals^2)
sqrt(mean(linear_summary4$residuals^2))
#MSE 0.006860046
#RMSE 0.0828254



#Using classification tree (rpart) model
set.seed(1)
#Calling rpart()with small cp to create object for call to plotcp()
rpart.data3=rpart(C150_4~.
                 ,data = data2,
                 method="anova", cp=0.001)


#Pruning data with se=1
rpart.data3.1se <- rt.prune(rpart.data3, se=1)
printcp(rpart.data3.1se)
#MSE 9340200.4189


#rpart testing validation set MSE
set.seed(1)
rpart.data4.train = rpart(C150_4~., data = data2[train,],
                          method="anova", cp=0)
rpart.data4.train = rt.prune(rpart.data4.train, se=1)
yhat2=predict(rpart.data4.train, newdata = data2[-train,])
data4.test = data2[-train,"C150_4"]
(MSE = mean((yhat2-data4.test)^2))
sqrt(MSE)
#MSE 0.0074452832
#RMSE 0.0862860544


#Results:                                                  MSE         RMSE
#RandomForest Test-Set MSE                               0.00592   0.07699
#RandomForest Test-Set MSE(8 most-important)             0.00642   0.08015
#Multiple Linear Regression                              0.00519   0.07208
#Multiple Linear Regression(8 most-important)            0.00686   0.08282
#Classification Tree Test-Set MSE                        0.00744   0.08628




