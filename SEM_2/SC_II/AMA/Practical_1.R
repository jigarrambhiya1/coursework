AMA-2
library(readr)

#Practical 1 
walmart <- read.csv("C:/Users/nmims.student/Downloads/walmart.csv")

#To check dimensions
dim(walmart)
#To find type of data for each variable
sapply(walmart,class)
#Changing type od date variable from character date
walmart$Date=as.Date(walmart$Date,"%d-%m-%Y")
sapply(walmart,class)
#Checking for Missing values
sapply(walmart,function(x){ sum(is.na(x))})
#Treating missing values
library("dplyr")
#deleting variable (vehicles)
df=select(walmart,-10)

#replacing missing values of Temp with mean or median
df$Temperature[is.na(df$Temperature)]=round(mean(df$Temperature,na.rm=TRUE))
df$CPI[is.na(df$CPI)]=round(mean(df$CPI,na.rm = TRUE))

#replacing missing value for logical/categorical var IsHoliday
table(df$IsHoliday)
df$IsHoliday[is.na(df$IsHoliday)]=FALSE
str(df)
my_mode=function(x){  #create mode function
  unique_x=unique(x)#returns unique values in the function 
  tabulate_x=tabulate(match(x,unique_x)) #returns no.of false and true values
  unique_x[tabulate_x==max(tabulate_x)] #returns max value between true and false
}

df$IsHoliday[is.na(df$IsHoliday)]=my_mode(df$IsHoliday)
sapply(df,function(x) sum(is.na(x)))
#METHOD 2:MICE
library("mice")
md.pattern(walmart)

md.pattern(df)

mice_result=mice(walmart[,-10],m=5,method='pmm',seed=123)
summary(mice_result)
data_mice_imputed=complete(mice_result,1)

data=select(df,c(-3,-4,-5))
par(mfrow=c(2,4))
for(i in colnames(data)){
  boxplot(data[i],main=i)
}

#OR
boxplot(df$Store,data=df,main='STORES')
boxplot(df$Dept,data=df,main='DEPARTMENT')
boxplot(df$Size,data=df,main='SIZE')
boxplot(df$Temperature,data=df,main='TEMPERATURE')
boxplot(df$Fuel_Price,data=df,main='FUEL_PRICE')
boxplot(df$CPI,data=df,main='CONSUMER PRICE INDEX')
boxplot(df$Unemployment,data=df,main='UNEMPLOYMENT')
boxplot(df$Weekly_Sales,data=df,main='WEEKLY SALES')

#TREATING OUTLIERS 
#quantile based flooring
outlier_norm=function(x){
  q=quantile(x,probs=c(.25,.75))
  caps=quantile(x,probs=c(0.05,0.95))
  H=1.5*IQR(x,na.rm=TRUE)
  x[x<(q[1]-H)]=caps[1]
  x[x>(q[2]+H)]=caps[2]
  return(x)
}
#replacing outliers
df$Unemployment=outlier_norm(df$Unemployment)
boxplot(df$Unemployment,data=df,main='Unemployment rate')

library("moments")
#filled density plot
d=density(df$Weekly_Sales);d
plot(d,main="weekly sales")
polygon(d,col="red",border="blue")
skewness(df$Weekly_Sales)
#our dependent variableis specifically used for non-linear modelling.
#in order to understand normality and transformation we will use the below variable 

#for example
d=density(df$Unemployment)
plot(d,main="unemployment")
polygon(d,col="red",border="blue")
skewness(df$Unemployment)
#Transformation
#log
df$Unemploymentlog=log(df$Unemployment)
skewness(df$Unemploymentlog)
d=density(df$Unemploymentlog)
plot(d,main="unemploymentlog")
polygon(d,col="red",border="blue")

#power square
df$Unemploymentsqrt=sqrt(df$Unemployment)
skewness(df$Unemploymentsqrt)
d=density(df$Unemploymentsqrt)
plot(d,main="unemployment sqrt")
polygon(d,col="red",border="blue")

#power cube 
df$Unemploymentcube=df$Unemployment^(1/3)
skewness(df$Unemploymentcube)
d=density(df$Unemploymentcube)
plot(d,main="unemployment cube")
polygon(d,col="red",border="blue")

#car library
library(car)
s=powerTransform(df$Unemployment)
summary(s)#here lambda can neither be 0 or 1 as the p value is very small hence we reject null (also if lambda os = 0, we go for log transformation and any other lambda means to go for box cox)
df$UnemploymentPT=df$Unemployment^(-0.02)#-0.02 from summary
skewness(df$UnemploymentPT)
d=density(df$UnemploymentPT)
plot(d,main="unemployment PT")
polygon(d,col="red",border="blue")

#QQ plot
qqnorm(df$Unemployment, pch=1, frame= FALSE)
qqline(df$Unemployment, col="steelblue", lwd=2)


#label encoding 
df$IsHoliday=ifelse(df$IsHoliday=="TRUE",1,0)
table(df$IsHoliday)
#one hot encoding
library("caret")
dmy=dummyVars("~.",data=df,fullRank=T)
df=data.frame(predict(dmy,newdata=df))
str(df)
#scaling data (when variables have different units causing interruption in our analysis)
df$Temperature=scale(df$Temperature)
df$Fuel_Price=scale(df$Fuel_Price)
df$CPI=scale(df$CPI)

#step 5:
#feature engineering
walmart$year=format(walmart$Date,"%Y")
walmart$day=format(walmart$Date,"%d")
walmart$month=format(walmart$Date,"%m")


# Final Changes
