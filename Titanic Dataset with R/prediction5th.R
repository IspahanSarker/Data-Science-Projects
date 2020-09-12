library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

setwd("~/datasets/titanic")

train <- read.csv("~/datasets/titanic/train.csv")

test <- read.csv("~/datasets/titanic/test.csv")

test$Survived <-NA

combined_set <- rbind(train,test)

combined_set

combined_set$Name <- as.character(combined_set$Name) 

# Mother 

combined_set$Child[combined_set$Age < 14 ]<-'Child'

combined_set$Child[combined_set$Age >= 14]<-'Adult'

table (combined_set$Child,combined_set$Survived)

combined_set$Child <- factor(combined_set$Child)

combined_set$Mother <- 'Not Mother'

combined_set$Mother[combined_set$Sex=='female' & combined_set$Parch >0 
                    & combined_set$Age>18]<-'Mother'

table(combined_set$Mother,combined_set$Survived)

combined_set$Mother <- factor(combined_set$Mother)

train$Name[1]

# Title

strsplit(combined_set$Name[1], split='[,.]')

strsplit(combined_set$Name[1], split='[,.]')[[1]]

strsplit(combined_set$Name[1], split='[,.]')[[1]][2]

combined_set$Title <- sapply(combined_set$Name, FUN=function(x) {strsplit(x,split='[,.]')[[1]][2]})

combined_set$Title <-sub('','',combined_set$Title)

table(combined_set$Title)

combined_set$Title[combined_set$Title %in% c('Mme','Mlle')] <- 'Mlle'

combined_set$Title[combined_set$Title %in% c('Capt','Don','Major','Sir')] <- 'Sir'

combined_set$Title[combined_set$Title %in% c('Dona','Lady','the Countess','Jonkheer')]<-'Lady'

combined_set$Title  <-factor(combined_set$Title)

# Mother 

combined_set$Mother <-'Not Mother'

combined_set$Mother[combined_set$Sex=='female' & combined_set$Parch >0 & combined_set$Age>18 
                    & combined_set$Title!='Miss']<-'Mother'
# Cabin

combined_set$Cabin <- as.character(combined_set$Cabin)

strsplit(combined_set$Cabin[2],NULL)[[1]]

combined_set$Deck<-factor(sapply(combined_set$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


# Fare 

combined_set$Fare_type[combined_set$Fare<50]<-"low"

combined_set$Fare_type[combined_set$Fare>50 & combined_set$Fare <= 100]<-"mid1"

combined_set$Fare_type[combined_set$Fare>100 & combined_set$Fare <= 150]<-"mid2"

combined_set$Fare_type[combined_set$Fare>150 & combined_set$Fare <= 500]<-"high"

combined_set$Fare_type[combined_set$Fare>500 ]<-"vhigh"

aggregate(Survived~Fare_type,data=combined_set,mean)

# Family size

combined_set$FamilySize <- combined_set$SibSp + combined_set$Parch + 1

combined_set$Surname <- sapply(combined_set$Name, FUN=function(x) {strsplit(x,split='[,.]')[[1]][1]})

combined_set$FamilyId <- paste(as.character(combined_set$FamilySize),combined_set$Surname,sep="")

combined_set$FamilyId[combined_set$FamilySize<=2]<-'small'

combined_set$FamilySizeGroup[combined_set$FamilySize==1]<-'single'

combined_set$FamilySizeGroup[combined_set$FamilySize<5 & combined_set$FamilySize>1]<-'smaller'

combined_set$FamilySizeGroup[combined_set$FamilySize>4]<-'large'

mosaicplot(table(combined_set$FamilySizeGroup,combined_set$Survived),main='Survival affected by Family Size',shade=TRUE)

table(combined_set$FamilyId)

table(combined_set$FamilySizeGroup)

combined_set$FamilyId <- factor(combined_set$FamilyId)

combined_set$FamilySizeGroup <- factor(combined_set$FamilySizeGroup)

train <-combined_set[1:891,]

test <-combined_set[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare 
             + Embarked + Title + FamilySize + FamilyId,
             data=train,method="class")

library(rattle)

library(RColorBrewer)

fancyRpartPlot(fit)

prediction5th <- predict(fit,test,type="class")

submit <- data.frame(PassengerId=test$PassengerId,Survived=prediction5th)

write.csv(submit,file="prediction5th.csv",row.names=FALSE)


























