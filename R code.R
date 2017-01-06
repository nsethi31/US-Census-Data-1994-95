# install and load package "data.table"-extension of data.frame)
install.packages("data.table")
library("data.table")

#set working directory
path= "C:/Users/sethi/Desktop/G drive/Practice/Project"
setwd(path)

#loading data files
train= fread("train.csv",na.strings = c(""," ","?","NA",NA))
test=fread("test.csv",na.strings = c(""," ","?","NA",NA))

#looking at data
dim(train); str (train); View(train)
dim(test); str (test); View(test)

#checking first few rows of data
head(train)
head(test)

#checking the target variable
unique(train$income_level)
unique(test$income_level)

#encoding the target variable
train[,income_level:= ifelse(income_level=="-50000",0,1)]
test[,income_level:= ifelse(income_level=="-50000",0,1)]

#checking the imbalance of training data
round(prop.table(table(train$income_level))*100)

#creating column classes-categorical and numerical column indexes
catcols=c(2:5,7,8:16,20:29,31:38,40,41)
#setdiff=difference between sets
numcols=setdiff(1:40,catcols) 

write.table(, "c:/mydata.txt", sep="\t") 

#subsetting data into categorical and numeric (.SD=subset of data)
#lapply returns a list value
#to apply a given function to every element of a list and obtain a list as result

#Specifies the columns of x included in .SD. May be character column
#names or numeric positions. This is useful for speed when applying a function
#through a subset of (possible very many) columns;

train[,(catcols) := lapply(.SD, factor), .SDcols = catcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(catcols) := lapply(.SD, factor), .SDcols = catcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

#By default with=TRUE and j is evaluated within the frame of x; column names
#can be used as variables. When with=FALSE j is a character vector of column
#names or a numeric vector of column positions to select, and the value returned
#is always a data.table. with=FALSE is often useful in data.table to select
#columns dynamically.

#subsetting categorical variables
cat_train = train[,catcols, with=FALSE]
cat_test = test[,catcols,with=FALSE]

#subsetting numerical variables
num_train = train[,numcols,with=FALSE]
num_test = test[,numcols,with=FALSE] 

# write.table(cat_train, "d:/cat_train.txt", sep=",") 
# write.table(cat_test, "d:/cat_test.txt", sep=",")
# write.table(num_test, "d:/num_test.txt", sep=",")
# write.table(num_train, "d:/num_train.txt", sep=",")

#removing original data files to save memory
rm(train,test)

#proportion of income level in various categories
#1 for getting proportions within that category
prop.table(table(cat_train$marital_status,cat_train$income_level),1)
prop.table(table(cat_train$class_of_worker,cat_train$income_level),1)
prop.table(table(cat_train$education,cat_train$income_level),1)

#importing library "ggplot2"
library("ggplot2")

#charts between categorical variables and dependent variable
charts <- function(i){
  ggplot(cat_train,aes(x=i,fill=income_level))+geom_bar(position = "dodge",  color="black")+
    scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle=60,hjust=1,size=10))
}

charts(cat_train$class_of_worker)
charts(cat_train$education)
charts(cat_train$marital_status)

#checking missing values in numeric variables
#inference-no missing values
table(is.na(num_train))
table(is.na(num_test))

#checking missing values per column (as a percentage) for categorical variables
mv_train=sapply(cat_train, function(x){sum(is.na(x))/length(x)}*100)
mv_test=sapply(cat_test, function(x){sum(is.na(x))/length(x)}*100)
mv_train
mv_test

#selecting columns with missing values < 5%
cat_train=subset(cat_train,select = mv_train< 5)
cat_test=subset(cat_test,select = mv_test<5)

#removing useless variables
useless=c(2,3,5,10,13,16,23,26,27)
cat_train=cat_train[,-useless,with=FALSE]
cat_test=cat_test[,-useless,with=FALSE]
num_train=subset(num_train[,-6])
num_test=subset(num_test[,-6])

#setting "NA" as "unknown" in training data
#converting factors to characters
cat_train=cat_train[,names(cat_train):= lapply(.SD,as.character), .SDcols=names(cat_train)]
#changing NA to unknown
for (i in seq_along(cat_train)) {set(cat_train, i=which(is.na(cat_train[[i]])), j=i, value = "unknown")}
#converting back to factors
cat_train=cat_train[,names(cat_train):= lapply(.SD, factor), .SDcols=names(cat_train)]

#setting NA as unknown in testing data
#converting factors to characters
cat_test=cat_test[,names(cat_test):= lapply(.SD,as.character), .SDcols=names(cat_test)]
#changing NA to unknown
for (i in seq_along(cat_test)) {set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value = "unknown")}
#converting back to factors
cat_test=cat_test[,names(cat_test):= lapply(.SD, factor), .SDcols=names(cat_test)]

#creating a scatter plot between age & wage per hour
#inference-age between 25-65 and wage per hour from 1000-4000
ggplot(data=num_train,aes(x = age, y=wage_per_hour))+geom_point(aes(colour=cat_train$income_level))+
  scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))

#loading package "caret" for calculating correlation between numeric variables
install.packages("caret")
library("caret")

#setting cutoff value of correlation coeff=0.7
ax=findCorrelation(x = cor(num_train), cutoff = 0.7)

#removing variable with correlation coeff. > 0.7-weeks worked per year
num_train =num_train[,-ax,with=FALSE]
num_test[,weeks_worked_in_year := NULL]

#caret library works like this
correlated=cor(num_train)
#correlation values in the upper triangle of the correlation matrix
summary(correlated[upper.tri(correlated)])

#combining factor levels with less than 5% values
#training
for (i in names(cat_train)) 
{
  fltrain=names(which(prop.table(table(cat_train[[i]]))<0.05))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% fltrain]="Other"
}
#testing
for (i in names(cat_test)) 
{
  fltest=names(which(prop.table(table(cat_test[[i]]))<0.05))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% fltest]="Other"
}

#checking columns with unequal levels in training and testing
#mlr-machine learning
install.packages("mlr")
library("mlr")

#"nlevs" returns the unique number of level from the given set of variables
summarizeColumns(cat_train)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"]

***********
  # #combining categorical and numeric variables
  # d_train=cbind(num_train,cat_train)
  # d_test=cbind(num_test,cat_test)
  # 
  # #creating tasks
  # train.task=makeClassifTask(data = d_train,target = "income_level")
  # test.task=makeClassifTask(data = d_test,target = "income_level")
  ***********
task=cbind(cat_train,num_train)
d_test=cbind(cat_test,num_test)
***********
  # #creating tasks
  train.task=makeClassifTask(data = task,target = "income_level")
  test.task=makeClassifTask(data = d_test,target = "income_level")
  # 
  # #removing zero variance variables
  # train.task=removeConstantFeatures(train.task)
  test.task=removeConstantFeatures(test.task)
  ************
  
install.packages("DMwR")
library("DMwR")

#applying SMOTE
#trainSplit <- SMOTE(income_level ~ ., task, perc.over = 200, perc.under=400)
system.time( train.smote <- smote(train.task,rate = 8,nn = 3))
table(getTaskTargets(train.smote))

d_train=as.data.table(getTaskData(train.task))
d_train1=as.data.table(getTaskData(train.smote))

Train_cat=subset(d_train1[,1:20])
Train_num=subset(d_train1[,22:26])

install.packages("dummies")
library("dummies")

#removing target variable from the set from which dummy variables are to be created
cat_train1=subset(cat_train[,-21])
cat_test1=subset(cat_test[,-21])

#creating dummy variables from the categorical variables
dummies_train <- dummy.data.frame(Train_cat)
dummies_test <- dummy.data.frame(cat_test1)

#combining all the variables
Train_full=cbind(dummies_train, Train_num, income_level = d_train1$income_level)
Test_full=cbind(num_test,dummies_test, income_level = cat_test$income_level)

write.table(Train_full,"d:/full_train.txt", sep=",")
write.table(Test_full,"d:/full_test.txt", sep=",")

str(d_test)
str(d_train)

logistic = glm(income_level~., family=binomial(logit), data=Train_full)
result <- predict(logistic, Test_full)
summary(result)