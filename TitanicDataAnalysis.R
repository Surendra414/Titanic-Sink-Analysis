setwd("C:/Users/cdac/Desktop/Surendra Project/Titanic_1/")
train<-read.csv("train.csv",header=TRUE)
test<-read.csv("test.csv",header=TRUE)

test.survived<-data.frame(survived=rep("NONE",nrow(test)),test[,])

data.combined<-rbind (train, test.survived)

str(data.combined)

data.combined$survived<-as.factor(data.combined$survived)
data.combined$pclass<-as.factor(data.combined$pclass)


table(data.combined$survived) 
table(data.combined$pclass)

library(ggplot2)

train$pclass <- as.factor(train$pclass)

ggplot(train,aes(x = pclass, fill = factor(survived))) + 
  geom_bar(width = 0.5) + 
  
  xlab("Pclass") + 
  ylab("Total Count") + 
  labs(fill="Survived")


head(as.character(train$name))

length(unique(as.character(data.combined$name)))

dup.names<-as.character(data.combined[which(duplicated(as.character(data.combined$name))),"name"])

data.combined[which(data.combined$name %in% dup.names),]
library(stringr)

misses <- data.combined[which(str_detect(data.combined$name,"Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$name,"Mrs.")),]
mrses[1:5,]

males<-data.combined[which(train$sex=='male'),]
males[1:5,]

extractTitle<-function(name){
  name<-as.character(name)
  if(length(grep("Miss.",name))>0){
    return("Miss.")
  }else if(length(grep("Master.",name))>0){
    return("Master.")
  } else if (length(grep("Mrs.",name))>0){
    return("Mrs.")
  }else if (length(grep("Mr.",name))>0){
    return("Mr.")
  }else{
    return("Other")
  }
}
titles<-NULL
for(i in 1:nrow(data.combined)){
  titles<-c(titles,extractTitle(data.combined[i,"name"]))
}
data.combined$title<-as.factor(titles)

ggplot(data.combined[1:891,],aes(x=title,fill=survived))+
  geom_bar(width = 0.5)+
  facet_wrap(~pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill="Survived")

table(data.combined$sex)

ggplot(data.combined[1:891,],aes(x=sex,fill=survived))+
  geom_bar(width = 0.5)+
  facet_wrap(~pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill="Survived")

summary(data.combined$age)

ggplot(data.combined[1:891,],aes(x=age,fill=survived))+
  
  facet_wrap(~sex + pclass)+
  geom_histogram(binwidth = 10)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")

boys<-data.combined[which(data.combined$title=="Master."),]
summary(boys$age)

misses<-data.combined[which(data.combined$title=="Miss."),]
summary(misses$age)

ggplot(misses[misses$survived !="NONE",],aes(x=age,fill=survived))+
  facet_wrap(~pclass)+
  geom_histogram(binwidth = 5)+
  ggtitle("Age for'Miss.'by Pclass")+
  xlab("Age")+
  ylab("Total Count")

misses.alone<-misses[which(misses$sibSp==0&misses$parch==0),]
summary(misses.alone$age)
length(which(misses.alone$age<=14.5))

summary(data.combined$sibSp)

length(unique(data.combined$sibSp))

data.combined$sibSp<-as.factor(data.combined$sibSp) 

ggplot(data.combined[1:891,], aes (x=sibSp, fill=survived))+
  stat_count(width = 1)+
  facet_wrap(~pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("SibSp")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")


data.combined$parch<-as.factor(data.combined$parch)

ggplot(data.combined[1:891,], aes ( x=parch, fill=survived))+
  stat_count(width = 1)+
  facet_wrap(~pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("Parch")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

temp.sibSp<-c(train$sibSp,test$sibSp)
temp.parch<-c(train$parch,test$parch)
data.combined$family.size<-as.factor(temp.sibSp+temp.parch+1)

ggplot(data.combined[1:891,], aes ( x=family.size, fill=survived))+
  stat_count(width = 1)+
  facet_wrap(~pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("Family Size")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

str(data.combined$ticket)
data.combined$ticket<-as.character(data.combined$ticket)
data.combined$ticket[1:20]

ticket.first.char<-ifelse(data.combined$ticket==""," ",substr(data.combined$ticket,1,1))
unique(ticket.first.char)

data.combined$ticket.first.char<-as.factor(ticket.first.char)

ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=survived))+
  geom_bar()+
  ggtitle("Survivability by ticket.first.char")+
  xlab("ticket.first.char")+
  ylab("Total Count")+
  ylim(0,350)+
  labs(fill="Survived")

ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=survived))+
  geom_bar()+
  facet_wrap(~pclass)+
  ggtitle("Pclass")+
  xlab("ticket.first.char")+
  ylab("Total Count")+
  ylim(0,150)+
  labs(fill="Survived")

ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=survived))+
  geom_bar()+
  facet_wrap(~pclass+title)+
  ggtitle("Pclass,Title")+
  xlab("ticket.first.char")+
  ylab("Total Count")+
  ylim(0,200)+
  labs(fill="Survived")

summary(data.combined$fare)
length(unique(data.combined$fare))
ggplot(data.combined,aes(x=fare))+
  geom_histogram(binwidth = 5)+
  ggtitle("Combined fare Distribusion")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,200)

ggplot(data.combined[1:891,],aes(x=fare,fill=survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~pclass+title)+
  ggtitle("Pclass,Title")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,50)+
  labs(fill="Surviced")


str(data.combined$cabin)
data.combined$cabin<-as.character(data.combined$cabin)

data.combined$cabin[1:200]
data.combined[which(data.combined$cabin ==" " ), "cabin"]<-"U"
data.combined$cabin[1:100]  


cabin.first.char<-as.factor(substr(data.combined$cabin,1,1))  
str(cabin.first.char)
levels(cabin.first.char)
data.combined$cabin<-cabin.first.char  

ggplot(data.combined,aes(x=cabin.first.char,fill=survived))+
  geom_bar(width = 1)+
  ggtitle("Survivability by Cabin.first.char")+
  xlab("Cabin.first.char")+
  ylab("Total Count")+
  ylim(0,750)+
  labs(fill="Survived")

ggplot(data.combined,aes(x=cabin.first.char,fill=survived))+
  geom_bar()+
  facet_wrap(~pclass)+
  ggtitle("Survivability by Cabin.first.char")+
  xlab("Pclass")+
  ylab("Total Count")+
  ylim(0,500)+
  labs(fill="Surviced")


str(data.combined$ticket)
data.combined$ticket<-as.character(data.combined$ticket)
data.combined$ticket[1:20]

ticket.first.char<-ifelse(data.combined$ticket == ""," ",substr(data.combined$ticket,1,1))
unique(ticket.first.char)

data.combined$ticket.first.char<-as.factor(ticket.first.char)

ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=survived))+
  geom_bar()+
  ggtitle("Survivability by ticket.first.char")+
  xlab("ticket.first.char")+
  ylab("Total count")+
  ylim(0,150)+
  labs(fill="survived")

ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=survived))+
  geom_bar()+
  facet_wrap(~pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("ticket.first.char")+
  ylab("Total count")+
  ylim(0,200)+
  labs(fill="survived")

summary(data.combined$fare)
length(unique(data.combined$fare))

ggplot(data.combined,aes(x=fare))+
  geom_histogram(binwidth = 5)+
  ggtitle("Combined fare Distribution")+
  xlab("Fare")+
  ylab("Total count")+
  ylim(0,200)


ggplot(data.combined[1:891,],aes(x=fare,fill=survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("fare")+
  ylab("Total count")+
  ylim(0,50)+
  labs(fill="survived")

str(data.combined$cabin)
data.combined$cabin<-as.character(data.combined$cabin)
data.combined$cabin[1:100]

cabin.first.char<-as.factor(substr(data.combined$cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

data.combined$cabin.first.char<-cabin.first.char

ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill=survived))+
  geom_bar()+
  ggtitle("Survivability by cabin.first.char")+
  xlab("cabin.first.char")+
  ylab("Total count")+
  ylim(0,750)+
  labs(fill="survived")

ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill=survived))+
  geom_bar()+
  facet_wrap(~pclass)+
  ggtitle("Survivability by cabin.first.char")+
  xlab("Pclass")+
  ylab("Total count")+
  ylim(0,500)+
  labs(fill="survived")


ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill=survived))+
  geom_bar()+
  facet_wrap(~pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("cabin.first.char")+
  ylab("Total count")+
  ylim(0,500)+
  labs(fill="survived")

data.combined$cabin.muliple<-as.factor(ifelse(str_detect(data.combined$cabin," "),"Y","N"))

ggplot(data.combined[1:891,],aes(x=cabin.muliple,fill=survived))+
  geom_bar()+
  facet_wrap(~pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("cabin.multiple")+
  ylab("Total count")+
  ylim(0,350)+
  labs(fill="survived")

str(data.combined$embarked)
levels(data.combined$embarked)


ggplot(data.combined[1:891,],aes(x=embarked,fill=survived))+
  geom_bar()+
  facet_wrap(~pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("embarked")+
  ylab("Total count")+
  ylim(0,300)+
  labs(fill="survived")

########################################Decision Tree 
# Examine structure of dataframe
setwd("C:/Users/cdac/Desktop/Surendra Project/Titanic_1/")
train<-read.csv("train1.csv",header=TRUE)
test<-read.csv("test1.csv",header=TRUE)
str(train)

# Look at number of people who survived
table(train$Survived)
prop.table(table(train$Survived))

# Create new column in test set with our prediction that everyone dies
test$Survived <- rep(0, 418)

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

#######################
# ender patterns
summary(train$Sex)
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived), 1)

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gendermodel.csv", row.names = FALSE)

# Age patterns
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# class and fare patterns
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
# Update the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1
# Update once more to say that females who pay more for a third class fare also perish
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "genderclassmodel.csv", row.names = FALSE)

###############################
# Install and load required packages for fancy decision tree plotting
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Recreate the gender model
fit <- rpart(Survived ~ Sex, data=train, method="class")
fancyRpartPlot(fit)

# Build a deeper tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
fancyRpartPlot(fit)

# Plot it with base-R
plot(fit)
text(fit)
# And then make it look better with fancyRpartPlot!
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, survived = Prediction)
write.csv(submit, file = "myfirstdtree1.csv", row.names = FALSE)

# Let's unleash the decision tree and let it grow to the max
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfullgrowntree.csv", row.names = FALSE)

# Manually trim a decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0.005))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

############################
# Install and load required packages for fancy decision tree plotting
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# What's in a name?
train$Name[1]

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)
View(combi)
write.csv(combi,file = "Masterdataset.csv",row.names = FALSE)
# Convert to a string
combi$Name <- as.character(combi$Name)
# What's in a name, again?
combi$Name[1]

# Find the indexes for the tile piece of the name
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

# Engineered variable: Title
combi$Title <- strsplit(combi$Name, split='[,.]')[[1]][2]  # Won't work!
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Inspect new feature
table(combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Inspect new feature
table(combi$FamilyID)
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build a new tree with our new features
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "engineeredfeaturestree.csv", row.names = FALSE)

####################
# Install and load required packages for decision trees and forests
library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('party')
library(party)

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

# Fill in Age NAs
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
# Check what else might be missing
summary(combi)
# Fill in Embarked blanks
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
# Fill in Fare NAs
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# New factor for Random Forests, only allowed <32 levels, so reduce number
combi$FamilyID2 <- combi$FamilyID
# Convert back to string
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
# And convert back to factor
combi$FamilyID2 <- factor(combi$FamilyID2)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build Random Forest Ensemble
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data=train, importance=TRUE, ntree=2000)
# Variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# Build(Conditional Random Forests) condition inference tree Random Forest
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ciforest.csv", row.names = FALSE)

