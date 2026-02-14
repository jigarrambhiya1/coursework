AMA-2
SYMPROSE REMEDIOS
2026-01-09
library(readr)
## Warning: package 'readr' was built under R version 4.4.2
walmart <- read_csv("C:/Users/nmims.student/Downloads/walmart.csv")
## Warning: One or more parsing issues, call `problems()` on your data frame for details,
## e.g.:
##   dat <- vroom(...)
##   problems(dat)
## Rows: 421570 Columns: 12
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): Date, Type
## dbl (8): Store, Dept, Size, Temperature, Fuel_Price, CPI, Unemployment, Week...
## lgl (2): IsHoliday, Vehicle Used
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
View(walmart)

#Practical 1 
walmart <- read.csv("C:/Users/nmims.student/Downloads/walmart.csv")

#To check dimensions
dim(walmart)
## [1] 421570     12
#To find type of data for each variable
sapply(walmart,class)
##        Store         Dept         Date    IsHoliday         Type         Size 
##    "integer"    "integer"  "character"    "logical"  "character"    "integer" 
##  Temperature   Fuel_Price          CPI Vehicle.Used Unemployment Weekly_Sales 
##    "numeric"    "numeric"    "numeric"  "character"    "numeric"    "numeric"
#Changing type od date variable from character date
walmart$Date=as.Date(walmart$Date,"%d-%m-%Y")
sapply(walmart,class)
##        Store         Dept         Date    IsHoliday         Type         Size 
##    "integer"    "integer"       "Date"    "logical"  "character"    "integer" 
##  Temperature   Fuel_Price          CPI Vehicle.Used Unemployment Weekly_Sales 
##    "numeric"    "numeric"    "numeric"  "character"    "numeric"    "numeric"
#Checking for Missing values
sapply(walmart,function(x){ sum(is.na(x))})
##        Store         Dept         Date    IsHoliday         Type         Size 
##            0            0            0           19            0            0 
##  Temperature   Fuel_Price          CPI Vehicle.Used Unemployment Weekly_Sales 
##           17            0           14       421552            0            0
#Treating missing values
library("dplyr")
## Warning: package 'dplyr' was built under R version 4.4.3
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
#deleting variable (vehicles)
df=select(walmart,-10)

#replacing missing values of Temp with mean or median
df$Temperature[is.na(df$Temperature)]=round(mean(df$Temperature,na.rm=TRUE))
df$CPI[is.na(df$CPI)]=round(mean(df$CPI,na.rm = TRUE))

#replacing missing value for logical/categorical var IsHoliday
table(df$IsHoliday)
## 
##  FALSE   TRUE 
## 391890  29661
df$IsHoliday[is.na(df$IsHoliday)]=FALSE
str(df)
## 'data.frame':    421570 obs. of  11 variables:
##  $ Store       : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Dept        : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Date        : Date, format: "2010-02-05" "2010-02-12" ...
##  $ IsHoliday   : logi  FALSE TRUE FALSE FALSE FALSE FALSE ...
##  $ Type        : chr  "A" "A" "A" "A" ...
##  $ Size        : int  151315 151315 151315 151315 151315 151315 151315 151315 151315 151315 ...
##  $ Temperature : num  42.3 38.5 39.9 46.6 46.5 ...
##  $ Fuel_Price  : num  2.57 2.55 2.51 2.56 2.62 ...
##  $ CPI         : num  211 211 211 171 171 ...
##  $ Unemployment: num  8.11 8.11 8.11 8.11 8.11 ...
##  $ Weekly_Sales: num  24925 46039 41596 19404 21828 ...
my_mode=function(x){  #create mode function
  unique_x=unique(x)#returns unique values in the function 
  tabulate_x=tabulate(match(x,unique_x)) #returns no.of false and true values
  unique_x[tabulate_x==max(tabulate_x)] #returns max value between true and false
}

df$IsHoliday[is.na(df$IsHoliday)]=my_mode(df$IsHoliday)
sapply(df,function(x) sum(is.na(x)))
##        Store         Dept         Date    IsHoliday         Type         Size 
##            0            0            0            0            0            0 
##  Temperature   Fuel_Price          CPI Unemployment Weekly_Sales 
##            0            0            0            0            0
#METHOD 2:MICE
library("mice")
## Warning: package 'mice' was built under R version 4.4.3
## 
## Attaching package: 'mice'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     cbind, rbind
md.pattern(walmart)

##        Store Dept Date Type Size Fuel_Price Unemployment Weekly_Sales CPI
## 17         1    1    1    1    1          1            1            1   1
## 421506     1    1    1    1    1          1            1            1   1
## 16         1    1    1    1    1          1            1            1   1
## 17         1    1    1    1    1          1            1            1   1
## 1          1    1    1    1    1          1            1            1   0
## 10         1    1    1    1    1          1            1            1   0
## 3          1    1    1    1    1          1            1            1   0
##            0    0    0    0    0          0            0            0  14
##        Temperature IsHoliday Vehicle.Used       
## 17               1         1            1      0
## 421506           1         1            0      1
## 16               1         0            0      2
## 17               0         1            0      2
## 1                1         1            1      1
## 10               1         1            0      2
## 3                1         0            0      3
##                 17        19       421552 421602
md.pattern(df)
##  /\     /\
## {  `---'  }
## {  O   O  }
## ==>  V <==  No need for mice. This data set is completely observed.
##  \  \|/  /
##   `-----'

##        Store Dept Date IsHoliday Type Size Temperature Fuel_Price CPI
## 421570     1    1    1         1    1    1           1          1   1
##            0    0    0         0    0    0           0          0   0
##        Unemployment Weekly_Sales  
## 421570            1            1 0
##                   0            0 0
mice_result=mice(walmart[,-10],m=5,method='pmm',seed=123)
## 
##  iter imp variable
##   1   1  IsHoliday  Temperature  CPI
##   1   2  IsHoliday  Temperature  CPI
##   1   3  IsHoliday  Temperature  CPI
##   1   4  IsHoliday  Temperature  CPI
##   1   5  IsHoliday  Temperature  CPI
##   2   1  IsHoliday  Temperature  CPI
##   2   2  IsHoliday  Temperature  CPI
##   2   3  IsHoliday  Temperature  CPI
##   2   4  IsHoliday  Temperature  CPI
##   2   5  IsHoliday  Temperature  CPI
##   3   1  IsHoliday  Temperature  CPI
##   3   2  IsHoliday  Temperature  CPI
##   3   3  IsHoliday  Temperature  CPI
##   3   4  IsHoliday  Temperature  CPI
##   3   5  IsHoliday  Temperature  CPI
##   4   1  IsHoliday  Temperature  CPI
##   4   2  IsHoliday  Temperature  CPI
##   4   3  IsHoliday  Temperature  CPI
##   4   4  IsHoliday  Temperature  CPI
##   4   5  IsHoliday  Temperature  CPI
##   5   1  IsHoliday  Temperature  CPI
##   5   2  IsHoliday  Temperature  CPI
##   5   3  IsHoliday  Temperature  CPI
##   5   4  IsHoliday  Temperature  CPI
##   5   5  IsHoliday  Temperature  CPI
## Warning: Number of logged events: 1
summary(mice_result)
## Class: mids
## Number of multiple imputations:  5 
## Imputation methods:
##        Store         Dept         Date    IsHoliday         Type         Size 
##           ""           ""           ""        "pmm"           ""           "" 
##  Temperature   Fuel_Price          CPI Unemployment Weekly_Sales 
##        "pmm"           ""        "pmm"           ""           "" 
## PredictorMatrix:
##           Store Dept Date IsHoliday Type Size Temperature Fuel_Price CPI
## Store         0    1    1         1    0    1           1          1   1
## Dept          1    0    1         1    0    1           1          1   1
## Date          1    1    0         1    0    1           1          1   1
## IsHoliday     1    1    1         0    0    1           1          1   1
## Type          1    1    1         1    0    1           1          1   1
## Size          1    1    1         1    0    0           1          1   1
##           Unemployment Weekly_Sales
## Store                1            1
## Dept                 1            1
## Date                 1            1
## IsHoliday            1            1
## Type                 1            1
## Size                 1            1
## Number of logged events:  1 
##   it im dep     meth  out
## 1  0  0     constant Type
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
## 
## Call:
##  density.default(x = df$Weekly_Sales)
## 
## Data: df$Weekly_Sales (421570 obs.); Bandwidth 'bw' = 913
## 
##        x                y            
##  Min.   : -7728   Min.   :0.000e+00  
##  1st Qu.:168164   1st Qu.:0.000e+00  
##  Median :344055   Median :5.700e-10  
##  Mean   :344055   Mean   :1.419e-06  
##  3rd Qu.:519947   3rd Qu.:4.360e-08  
##  Max.   :695838   Max.   :7.416e-05
plot(d,main="weekly sales")
polygon(d,col="red",border="blue")
skewness(df$Weekly_Sales)
## [1] 3.261997
#our dependent variableis specifically used for non-linear modelling.
#in order to understand normality and transformation we will use the below variable 

#for example
d=density(df$Unemployment)
plot(d,main="unemployment")
polygon(d,col="red",border="blue")
skewness(df$Unemployment)
## [1] 0.7303465
#Transformation
#log
df$Unemploymentlog=log(df$Unemployment)
skewness(df$Unemploymentlog)
## [1] 0.012207
d=density(df$Unemploymentlog)
plot(d,main="unemploymentlog")
polygon(d,col="red",border="blue")

#power square
df$Unemploymentsqrt=sqrt(df$Unemployment)
skewness(df$Unemploymentsqrt)
## [1] 0.373224
d=density(df$Unemploymentsqrt)
plot(d,main="unemployment sqrt")
polygon(d,col="red",border="blue")

#power cube 
df$Unemploymentcube=df$Unemployment^(1/3)
skewness(df$Unemploymentcube)
## [1] 0.2530695
d=density(df$Unemploymentcube)
plot(d,main="unemployment cube")
polygon(d,col="red",border="blue")

#car library
library(car)
## Warning: package 'car' was built under R version 4.4.3
## Loading required package: carData
## Warning: package 'carData' was built under R version 4.4.1
## 
## Attaching package: 'car'
## 
## The following object is masked from 'package:dplyr':
## 
##     recode
s=powerTransform(df$Unemployment)
summary(s)#here lambda can neither be 0 or 1 as the p value is very small hence we reject null (also if lambda os = 0, we go for log transformation and any other lambda means to go for box cox)
## bcPower Transformation to Normality 
##                 Est Power Rounded Pwr Wald Lwr Bnd Wald Upr Bnd
## df$Unemployment   -0.0175       -0.02       -0.029      -0.0061
## 
## Likelihood ratio test that transformation parameter is equal to 0
##  (log transformation)
##                            LRT df      pval
## LR test, lambda = (0) 9.052545  1 0.0026233
## 
## Likelihood ratio test that no transformation is needed
##                           LRT df       pval
## LR test, lambda = (1) 30289.8  1 < 2.22e-16
df$UnemploymentPT=df$Unemployment^(-0.02)#-0.02 from summary
skewness(df$UnemploymentPT)
## [1] 0.002248032
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
