# install.packages('rattle')
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
# install.pacakges('randomForest')
# install.packages('party')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)

train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

test$Survived <- NA

train <- rbind(train, test)

# train$Title <- sapply(train$Name, FUN=function(x) {strsplit(x, split="[,.]")[[1]][2]})
# train$Title <- sub(' ', '', train$Title)
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
train$Title[train$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
train$Title[train$Title == 'Mlle']        <- 'Miss' 
train$Title[train$Title == 'Ms']          <- 'Miss'
train$Title[train$Title == 'Mme']         <- 'Mrs' 
train$Title[train$Title %in% rare_title]  <- 'Rare Title'

rm(rare_title)


train$FamilySize <- (train$SibSp + train$Parch + 1)

train$Surname <- sapply(train$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
train$Family <- paste(train$Surname, train$FamilySize, sep='_')


train[train$Cabin == "",]$Cabin <- "Z"
train$Cabin <- substr(train$Cabin, 0,1)


Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + Cabin,
                data=train[!is.na(train$Age),], 
                method="anova")

train[is.na(train$Age),]$Age <- predict(Agefit, train[is.na(train$Age),])
train$Age <- round(train$Age)
rm(Agefit)

train[train$Embarked == "",]$Embarked <- "S"
train[is.na(train$Fare),]$Fare <- median(train$Fare, na.rm=TRUE)
train$Fare <- round(train$Fare, digits=1)

train$Embarked <- factor(train$Embarked)
train$Sex <- factor(train$Sex)
train$Cabin <- factor(train$Cabin)
train$Title <- factor(train$Title)

test <- train[892:1309,]
train <- train[1:891,]

set.seed(415)

# fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + Cabin, data=train, method="class")
# fancyRpartPlot(fit)

# fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
#                       Embarked + Title + FamilySize + Cabin,
#                     data=train, 
#                     importance=TRUE, 
#                     ntree=2000)
# varImpPlot(fit)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FamilySize + Cabin,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")


submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "cforest.csv", row.names = FALSE)