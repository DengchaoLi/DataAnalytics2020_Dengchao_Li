library(DMwR)
library(e1071)
library(randomForest)
library(ggplot2)
library(nnet)
library(ClustOfVar)
library(reshape2)

#read file
getwd("Users/lidengchao/Desktop/Data analysis")
df<-read.csv("SeoulBikeData.csv",stringsAsFactors = F,header = T)

##view data
head(df)
str(df)

## missing value check
summary(df)
sum(is.na(df))

## data outlier check
df$Date<-as.Date(df$Date,"%d/%m/%Y")


summary(df$Rented.Bike.Count)

ggplot(df,aes(x=Date,y=Rented.Bike.Count,group=1))+geom_line()+scale_x_date(date_labels="%Y-%m",date_breaks="2 month")

tapply(df$Rented.Bike.Count,df$Seasons,mean)
barplot(tapply(df$Rented.Bike.Count,df$Seasons,mean))


tapply(df$Rented.Bike.Count,df$Hour,mean)
barplot(tapply(df$Rented.Bike.Count,df$Hour,mean))


par(mfrow=c(2,4))
boxplot(df$Temperature,main="Temp")
boxplot(df$Humidity...,main="Humidity")
boxplot(df$Wind.speed..m.s.,main="windspeed")
boxplot(df$Visibility..10m.,main="Visibility")
boxplot(df$Dew.point.temperature,main="Dew.point")
boxplot(df$Solar.Radiation..MJ.m2.,main="Solar.Radiation")
boxplot(df$Rainfall.mm.,main="Rainfall")
boxplot(df$Snowfall..cm.,main="Snowfall")

##feature selection

##dummy variable setting
season_dummy<-data.frame(class.ind(df$Seasons))
holiday_dummy<-data.frame(class.ind(df$Holiday))
functionday_dummy<-data.frame(class.ind(df$Functioning.Day))


df_finial<-cbind(Rent=df$Rented.Bike.Count,df[,3:11],season_dummy,holiday_dummy,functionday_dummy)

##rename
names(df_finial)[4:10]<-c("Humidity","windspeed","Visibility","Dew.point","Solar.Radiation","Rainfall","Snowfall")

##kappa test
cor<-cor(df_finial[,2:18])
kappa(cor, exact=T)

# cluster for variable
tree<-hclustvar(df_finial[,2:18])
stability(tree,B=10)

#set as 6
part<-cutreevar(tree,6,matsim = T)
summary(part)

##select feature
df_new<-df_finial[,c("windspeed","Temperature","Humidity","Spring","Holiday","No")]
cor2<-cor(df_new)
kappa(cor2, exact=T)

#set as 10
part<-cutreevar(tree,10,matsim = T)
summary(part)

##select 10 feature
df_new<-df_finial[,c("Hour","windspeed","Temperature","Humidity","Spring","Holiday","No","Solar.Radiation","Rainfall","Snowfall")]
cor2<-cor(df_new)
kappa(cor2, exact=T)

##kappa test ok
df_model<-cbind(Rent=df_finial[,"Rent"],df_new)



### make the model
#set seed
set.seed(20201128)
## split 80% as train,20% as test
select<-sample(1:nrow(df_model),nrow(df_model)*0.8)
train=df_model[select,]
test=df_model[-select,]

#lm model
lm.train<-lm(Rent~.,data=train)
summary(lm.train)
plot(lm.train)

##predcit
lm.predict<-predict(lm.train,test)

##check MSE index 
lm.result<-regr.eval(test[,'Rent'],lm.predict,train.y=test[,'Rent'])
lm.result

lm.res<-lm.predict-test$Rent
##scatter plot
plot(lm.res~lm.predict)
abline(h=0)

##qq plot
library(car)
qqnorm(lm.res);qqline(lm.res)



##RF model
rf.train.check<-randomForest(Rent~.,data=train,ntree=1000,importance=TRUE)
plot(rf.train.check)

rf.train<-randomForest(Rent~.,data=train,ntree=600,importance=TRUE)
rf.train

##RF predict
rf.predict<-predict(rf.train,test)
rf.result<-regr.eval(test[,'Rent'],rf.predict,train.y=test[,'Rent'])
rf.result

#scatter plot 
plot(x=test$Rent,y=rf.predict,xlab="Actual Rent",ylab="Predict Rent",main="RandomForest model")
abline(a=1,b=1,col="red")

##residure check 
rf.res<-rf.predict-test$Rent
##scatter plot
plot(rf.res~rf.predict)
abline(h=0)

##qq plot
qqnorm(rf.res);qqline(rf.res)


#RF VIP plot
varImpPlot(rf.train,sort=TRUE, type=1) 


###SVM model
svm.train<- svm(Rent~.,data = train)
svm.train

svm.predict<-predict(svm.train,test)
svm.result<-regr.eval(test[,'Rent'],svm.predict,train.y=test[,'Rent'])
svm.result


### adjust the parameter for linear
svm.train<- svm(Rent~.,data = train,kernel='linear')
svm.train

svm.predict<-predict(svm.train,test)
svm.result<-regr.eval(test[,'Rent'],svm.predict,train.y=test[,'Rent'])
svm.result

## adjust the parameter for polynomial
svm.train<- svm(Rent~.,data = train,kernel='polynomial')
svm.train

svm.predict<-predict(svm.train,test)
svm.result<-regr.eval(test[,'Rent'],svm.predict,train.y=test[,'Rent'])
svm.result


##adjust the parameter for cost/gamma
for (i in 1:5){
  for (j in seq(0.1,0.5,0.1)) {
    svm.train<-svm(Rent~.,data = train,cost=i,gamma=j)
    svm.predict<-predict(svm.train,test)
    svm.result<-regr.eval(test[,'Rent'],svm.predict,train.y=test[,'Rent'])
    print(paste0("Cost:",i))
    print(paste0("Gamma:",j))
    print(svm.result[5])
  }
}

## SVM finial model

svm.train<- svm(Rent~.,data = train,cost=5,gamma=0.5,kernel="radial")
svm.train

svm.predict<-predict(svm.train,test)
svm.result<-regr.eval(test[,'Rent'],svm.predict,train.y=test[,'Rent'])
svm.result


##conclusion

finial_result<-data.frame(lm=lm.result,rf=rf.result,svm=svm.result)
finial_result<-data.frame(index=row.names(finial_result),lm=lm.result,rf=rf.result,svm=svm.result,row.names = NULL)
##remove mape index
finial_result1<-finial_result[c(1:3,5:6),]
finial_melt<-melt(finial_result1,id.vars = "index")
ggplot(finial_melt,aes(x=variable,y=value,fill=variable))+
  geom_bar(stat="identity",width=0.7,size=0.25,position = position_dodge())+facet_wrap(~index,scales = "free")+labs(x="Models",y="Index Value",fill="Learners")
