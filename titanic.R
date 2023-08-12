
#Loading Dataset
Titanic=read.csv(file.choose())
Titanic

#Data Preprcocessing
str(Titanic)

head(Titanic)

dim(Titanic)

summary(Titanic)

#Converting following variables to factor :
names<-c("Survived","Pclass","Sex","Embarked")
Titanic[,names]<-lapply(Titanic[,names], as.factor)
str(Titanic)

#Null Values Check
colSums(is.na(Titanic))

#It is clearly seen that Age column has null values.
#We must eliminate Cabin column as it has more no. of missing values
#It does not have significance in the survival rate. 
#For Age we can replace the value with mode respectively. 
#Converting the age variable to categorical to make it into groups.
#And also we’ll eliminate Name, PassengerId and Ticket columns as it doesn’t have any significance in the survival rate.
col<-c("PassengerId","Name","Ticket","Cabin")
Titanic[,col]<-list(NULL)
Titanic1<-Titanic
Titanic1$Age[is.na(Titanic1$Age)]<-28
Titanic1$Age<-cut(Titanic1$Age,breaks = c(0,20,28,40,Inf),labels = c("c1","c2","c3","c4"))
 
#scaling the numeric data
col_scale=c("SibSp","Parch","Fare")
Titanic1[,col_scale]<-lapply(Titanic1[,col_scale], scale)
colSums(is.na(Titanic1))

#Data Visualization

#Age Wise Distribution
library(ggplot2)
ggplot(Titanic1,aes(x=Age)) +
  geom_bar(aes(fill=Survived)) +
  labs(x = "Age Group",y="Frequency",
  title = "Age Wise Distribution")
#We can conclude that 45% of passengers survived were from the age group of 20 to 30.

#Sex wise distribution 
ggplot(Titanic1,aes(x=Sex)) + 
  geom_bar(aes(fill=Survived)) +
  labs(x = "Sex Group",y="Frequency",
       title = "Sex Wise Distribution")
#We can conclude that majority of passengers survived were female as compared to male.

#Passenger Class wise Distribution
ggplot(Titanic1,aes(x=Pclass)) + 
  geom_bar(aes(fill=Survived)) +
  labs(x="Passenger Class",y = "Frequency",
  title = "Passenger Class wise Distribution")
#We can conclude that most passengers survived were from 1st class followed by 3rd class and then 2nd class

#Splitting data
#install package lattice

library(caret)

set.seed(100) # keeping split constant in every iteration
index<-createDataPartition(Titanic1$Survived,p=0.7,list = F)
train_titanic<-Titanic1[index,]
test_titanic<-Titanic1[-index,]
dim(train_titanic) # dimension of training data 

dim(test_titanic) # dimension of testing data

#Naive Bayes Algorithm
library(e1071)

NB_model<-naiveBayes(Survived~.,data = train_titanic)
NB_pred<-predict(NB_model,test_titanic)
confusionMatrix(NB_pred,test_titanic$Survived)

#Naive Bayes algorithm gives accuracy of 81%

#Decision Tree
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

DT_model<-rpart(Survived~.,data = train_titanic)
rpart.plot(DT_model)

#checking accuracy of model on test data

DT_pred<-predict(DT_model,test_titanic,type="class")
confusionMatrix(DT_pred,test_titanic$Survived)

#Decision Tree algorithm gives accuracy of 80%
