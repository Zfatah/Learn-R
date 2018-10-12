#Load Packages


library(ggplot2)

library(e1071)

library(randomForest)

library(dplyr)

library(caret)

library(corrplot)


#Read

test <- read.csv("C:/Users/zarah/Desktop/Kaggle/test.csv", stringsAsFactors=FALSE)

View(test)

train <- read.csv("C:/Users/zarah/Desktop/Kaggle/train.csv", stringsAsFactors=FALSE)

View(train)


#Combining data

train$IsTrainSet<-TRUE

test$IsTrainSet<-FALSE

test$Survived<-NA

titanic<-rbind(train,test)



#Understand Data

str(titanic)

dim(titanic)

table(is.na(titanic))

sapply(titanic, function(x) sum(is.na(x)))



#Missing Value Manipulation



 1. #Fare

titanic[is.na(titanic$Fare),"Fare"]<-median(titanic$Fare,na.rm=TRUE)

table(is.na(titanic$Fare))

 

 

2.  #Embarked

table(is.na(titanic$Embarked))

titanic[is.na(titanic$Embarked),"Embarked"]<-median(titanic$Embarked,na.rm=TRUE)

table(is.na(titanic$Embarked))



  3. #Title

titanic[is.na(titanic$Title),"Title"]<-"Mrs"



  4. #Cabin

titanic[is.na(titanic$Cabin),"Cabin"]<-0

titanic$Cabinletter<-NULL

table(is.na(titanic))

titanic$CabinNum<-NULL



  5. #Age

upper<-boxplot.stats(titanic$Age)$stats[5]

outlier<-titanic$Age<=upper

equation= "Age~ Pclass+Sex+Fare+Parch+Embarked+SibSp"

model<-lm(formula = equation,data = titanic[outlier,])

features<-titanic[is.na(titanic$Age), c("Pclass","Sex","Fare","Parch","Embarked","SibSp")]

prediction<-predict.lm(model,newdata = features)

titanic[is.na(titanic$Age), "Age"]<-prediction

table(is.na(titanic))





#Change to Factor

titanic$Pclass<-as.factor(titanic$Pclass)

titanic$Sex<-as.factor(titanic$Sex)

titanic$Embarked<-as.factor(titanic$Embarked)

titanic$Title<-as.factor(titanic$Title)

titanic$Cabin<-as.factor(titanic$Cabin)



#EDA

Pclass_graph<-ggplot(train, aes(x=Pclass, fill=Survived)) + geom_bar(stat = "count") + labs(title="Distribution of survivors respect to Class")

Age_graph<-ggplot(train, aes(x=Age_Group, fill=Survived)) + geom_bar() + labs(title="Distribution of survivors respect to age")

Sex_graph<-ggplot(train, aes(x=Sex, fill=Survived)) + geom_bar()  + labs(title="Distribution of survivors based on sex")

corr<- titanic %>% select(-PassengerId, -SibSp, -Parch) %>% select_if(is.numeric) %>% cor(use = "complete.obs")%>% corrplot.mixed(tl.cex=0.85)



#Model

levels(test$Pclass) <- levels(train$Pclass)

levels(test$Sex) <- levels(train$Sex)

levels(test$Embarked) <- levels(train$Embarked)

levels(test$Title)<-levels(train$Title)

levels(test$CabinNum)<-levels(train$CabinNum)

levels(test$Survived) <- levels(train$Survived)



test<-titanic[titanic$IsTrainSet==FALSE,]

train<-titanic[titanic$IsTrainSet==TRUE,]

train$Survived<-as.factor(train$Survived)



survived.eq<- "Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title" 

survived.for<-as.formula(survived.eq)

modelforest<-randomForest(formula =survived.for, data=train,ntree = 500,mtry = 3,nodesize = 0.01*nrow(train))

survivedfeatures<-"Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Cabin"

Survived<-predict(modelforest, newdata = test)



#Final

PassengerId<-test$PassengerId

output<-as.data.frame(PassengerId)

output$Survived<-Survived

write.csv(output,file = "Titanicpred2.csv",row.names = FALSE)
