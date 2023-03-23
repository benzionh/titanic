##### load data ####
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)
#add survived variable to the test set to allow combining the two data sets
test.survived <- data.frame(Survived = rep("None",nrow(test)), test[,])
#combine the sets
data.combined <- rbind(train, test.survived)
str(data.combined)
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Sex <- as.factor(data.combined$Sex)
data.combined$Embarked <- as.factor(data.combined$Embarked)
#gross survival rates
table(data.combined$Survived)
#distribution across classes
table(data.combined$Pclass)


####### visualization #######
library(ggplot2)
# hypothesis: rich survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived)))+
  geom_histogram(binwidth = 0.5, stat = "count")+
  xlab("pclass")+
  ylab("total count")+
  labs(fill ="Survived")
#examining the first names in training n test set
head(as.character(train$Name))
length(unique(as.character(data.combined$Name)))  
#get duplicate names
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
data.combined[which(data.combined$Name %in% dup.names),]
#examining titles of individuals
library(stringr)
titles <- data.combined[which(str_detect(data.combined$Name, "Miss")),]
titles[1:5,]
#hypothesis: titles correlation with age
title1 <- data.combined[which(str_detect(data.combined$Name, "Mrs")),]
title1[1:5,]
#check pattern in males
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]
#create a utility function to help with name titles extraction
extractn <- function(Name) {
  Name <- as.character(Name)
  if (length(grep("Miss", Name)) > 0) {
    return("Miss")
  } else if (length(grep("Master", Name)) > 0) {
    return("Master")
  } else if (length(grep("Mrs", Name)) > 0) {
    return("Mrs")
  } else if (length(grep("Mr", Name)) > 0) {
    return("Mr")
  } else {
    return("other")
  }
}

title <- NULL
for (i in 1:nrow(data.combined)) {
  title <- c(title, extractn(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(title)
data.combined$Title
# visualisation of survival using titles
library("ggplot2")
ggplot(data.combined[1:891,], aes(x=Title, fill=Survived))+
  geom_bar(binwidth=0.5)+
   facet_wrap(~Pclass)+
   ggtitle("Pclass")+
   xlab("Title")+
   ylab("Total count")+
   labs(fill="Survived")
# what is distribution of females to males across the data
table(data.combined$Sex)
ggplot(data.combined[1:891,], aes(x=Sex,fill=Survived))+
  geom_bar(binwidth=0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total count")+
  labs(fill="Survived")

# distribution of age in the data
summary(data.combined$Age)
ggplot(data.combined[1:891,],aes(x=Age,fill=Survived))+
  facet_wrap(~Sex + Pclass)+
  geom_histogram(binwidth = 10)+
  xlab("Age")+
  ylab("Total Count")

summary(data.combined[1:891,"Age"])

# validate master is a good proxy for male children
boys <- data.combined[which(data.combined$Title=="Master"),]
summary(boys$Age)

# evaluate miss title
misses <- data.combined[which(data.combined$Title=="Miss"),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",],aes(x = Age, fill = Survived))+
  facet_wrap(~Pclass)+
  geom_histogram(binwidth = 5)+
  ggtitle("Age for miss by pclass")+
  xlab("Age")+
  ylab("Total count")

misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

# test the sibsp variable and visualize
summary(data.combined$SibSp)
length(unique(data.combined$SibSp))
data.combined$SibSp <- as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,], aes(x = SibSp,
                                  fill = Survived))+
  geom_histogram(binwidth = 1, stat = "count")+
  facet_wrap(~Pclass + Title)+
  ggtitle("pclass, title")+
  xlab("sibsp")+
  ylab("total count")+
  ylim(0,500)+
  labs(fill = "Survived")

# test the parch variable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch,
                                  fill = Survived))+
  geom_histogram(stat = "count")+
  facet_wrap(~Pclass + Title)+
  ggtitle("pclass, title")+
  xlab("parch")+
  ylab("total count")+
  ylim(0,500)+
  labs(fill = "Survived")


# try some feature engineering, creating a family size feature
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$SibSp, test$SibSp)
data.combined$Family.size <- as.factor(temp.sibsp + temp.parch + 1)

# visualize to see if it is predictive
ggplot(data.combined[1:891,], aes(x= Family.size,
                                  fill = Survived))+
  geom_histogram(stat = "count")+
  facet_wrap(~Pclass + Title)+
  ggtitle("pclass, title")+
  xlab("family size")+
  ylab("total count")+
  labs(fill = "Survived")

# ticket variable
str(data.combined$Ticket)

# display the first 20
data.combined$Ticket[1:20]

#take the first character for each 
ticket.first.char <- ifelse(data.combined$Ticket == "",
                            "",
                            substr(data.combined$Ticket,
                                   1, 1))
unique(ticket.first.char)

# make the chars factors for analysis
data.combined$ticket.first.char <- as.factor(ticket.first.char)

ggplot(data.combined[1:891,], aes(x = ticket.first.char,
                                  fill = Survived))+
  geom_bar()+
  ggtitle("survivalibility by ticket.first.char")+
  xlab("ticket.first.char")+
  ylab("total count")+
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = ticket.first.char,
                                  fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("ticket.first.char")+
  ylab("total count")+
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = ticket.first.char,
                                  fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)
  ggtitle("Pclass, Title")+
  xlab("ticket.first.char")+
  ylab("total count")+
  labs(fill = "Survived")

  # fare variable
summary(data.combined$Fare)
length(unique(data.combined$Fare))

ggplot(data.combined, aes(x = Fare))+
  geom_histogram(binwidth = 5)+
  ggtitle("fare distribution")+
  xlab("fare")+
  ylab("total count")

ggplot(data.combined, aes(x = Fare, fill = Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass Title")+
  xlab("fare")+
  ylab("total count")+
  labs(fill = "Survived")

# cabin variable
str(data.combined$Cabin)
data.combined$Cabin[1:100]
# replace empty cabin with "u"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "u"
data.combined$Cabin[1:100]

cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

data.combined$cabin.first.char <- cabin.first.char

ggplot(data.combined[1:891,], aes(x = cabin.first.char,
                                  fill = Survived))+
  geom_bar()+
  ggtitle("survivability by cabin.first.char")+
  xlab("cabin.first.char")+
  ylab("total count")+
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = cabin.first.char,
                                  fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("survivability by cabin.first.char")+
  xlab("Pclass")+
  ylab("total count")+
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = cabin.first.char,
                                  fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass Title")+
  xlab("cabin.first.char")+
  ylab("total count")+
  labs(fill = "Survived")

# multiple cabins
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple,
                                  fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass Title")+
  xlab("cabin.multiple")+
  ylab("total count")+
  labs(fill = "Survived")

# embarked variable
str(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x = Embarked,
                                  fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass Title")+
  xlab("Embarked")+
  ylab("total count")+
  labs(fill = "Survived")





####### EXPLORATORY MODELING #########
library(randomForest)

# train a random forest with the default parameters using pclass and title
rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label,
                     importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# train a random forest using pclass title sibsp
rf.train.2 <-  data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label,
                     importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

# train a random forest using pclass title parch
rf.train.3 <-  data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label,
                     importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

# train a random forest using pclass title parchsibsp
rf.train.4 <-  data.combined[1:891, c("Pclass", "Title",
                                      "Parch", "SibSp")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label,
                     importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

# train a random forest using pclass familysize
rf.train.5 <-  data.combined[1:891, c("Pclass", "Title", "Family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label,
                     importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

# train a random forest using pclass familysize sibsp
rf.train.6 <-  data.combined[1:891, c("Pclass", "Title",
                                      "Family.size", "SibSp")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label,
                     importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

# train a random forest using pclass familysize parch
rf.train.7 <-  data.combined[1:891, c("Pclass", "Title",
                                      "Family.size", "Parch")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label,
                     importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)



####### Crossvalidation ######
# submit our test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "Title",
                                            "Family.size")]

# make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# create a csv file for submission
submit.df <- data.frame(PassengerId = rep(892:1309),
                        Survived = rf.5.preds)
write.csv(submit.df, file = "titanic_submission.csv",
          row.names = FALSE)

# the submission had accuracy score of 0.76794

# lets look into crossvalidation using caret package to get more
# accurate estimates
library(caret)
library(doSNOW)

# create 10 total folds but ensure that the ratio of survived and
# perish in each fold matches overall training set. This is
# stratified cross validation
set.seed(1234)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# check stratification
table(rf.label)

table(rf.label[cv.10.folds[[33]]])

# set up caret's traincontrol 
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, 
                       repeats = 10, index = cv.10.folds)

# set up doSNOW package for multi-core training
c1 <- makeCluster(6, type = "SOCK")
registerDoSNOW(c1)

# set seed for reproducability and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf",
                   tuneLength = 3, ntree = 1000, 
                   trControl = ctrl.1)

# shutdown the cluster
stopCluster(c1)

rf.5.cv.1


set.seed(3963)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5,
                       repeats = 10, index = cv.5.folds)

c1 <- makeCluster(6, type = "SOCK")
registerDoSNOW(c1)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf",
                   tuneLength = 3, ntree = 1000, 
                   trControl = ctrl.2)

stopCluster(c1)

rf.5.cv.2


set.seed(37396)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3,
                       repeats = 10, index = cv.5.folds)

c1 <- makeCluster(6, type = "SOCK")
registerDoSNOW(c1)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf",
                   tuneLength = 3, ntree = 64, 
                   trControl = ctrl.3)

stopCluster(c1)

rf.5.cv.3


library(rpart)
library(rpart.plot)

# create utility function
rpart.cv <- function(seed, training, labels, ctrl){
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart",
                    tuneLength = 30, trControl = ctrl)
  stopCluster(cl)
  return(rpart.cv)
}

# grab features
features <- c("Pclass", "Title", "Family.size")
rpart.train.1 <- data.combined[1:891, features]

# run cv and check results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# investigate title variable
table(data.combined$Title)

# parse last name and title
data.combined[1:25, "Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

data.combined$Last.name <- last.names

name.splits <- str_split(sapply(name.splits, "[", 2), ' ')
titles <- sapply(name.splits, "[", 2)
unique(titles)

# what's with the title the
data.combined[which(titles == "the"),]

# rewrie titles to be more exact
titles[titles %in% c("Dona.", "the", "Lady")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.", "Miss")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

data.combined$New.titles <- as.factor(titles)

# visualize the new version of titles
ggplot(data.combined[1:891,], aes(x = New.titles,
                                  fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("survival rate for new version of titles")

# collapse titles based on visuals analysis
indexes <- which(data.combined$New.titles == c("Lady."))
data.combined$New.titles[indexes] <- "Mrs."

indexes <- which(data.combined$New.titles == "Dr."|
                   data.combined$New.titles == "Rev."|
                   data.combined$New.titles == "Sir."|
                   data.combined$New.titles == "Officer")
data.combined$New.titles[indexes] <- "Mr."

# visualize changes
ggplot(data.combined[1:891,], aes(x = New.titles,
                                  fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("survival rate for new version of titles")

# grab features
features <- c("Pclass", "New.titles", "Family.size")
rpart.train.2 <- data.combined[1:891, features]

# run cv and check results
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# 1st class Mr.
indexes.first.Mr <- which(data.combined$New.titles == "Mr." &
                            data.combined$Pclass == "1")
first.Mr.df <- data.combined[indexes.first.Mr,]
summary(first.Mr.df)

first.Mr.df[first.Mr.df$Sex == "Female"]

# update new title feature
length(which(data.combined$Sex == "Female" &
               data.combined$New.titles == "Master." |
               data.combined$New.titles == "Mr."))

# refresh the dataframe
indexes.first.Mr <- which(data.combined$New.titles == "Mr." &
                            data.combined$Pclass == "1")
first.Mr.df <- data.combined[indexes.first.Mr,]


summary(first.Mr.df[first.Mr.df$Survived == "1",])

# visualise survival for 1st class "Mr." by fare
ggplot(first.Mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st class 'Mr.' survival rate by fare")

# engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for(i in 1:length(tickets)){
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[i], "Fare"] / length(party.indexes)

  for(k in 1:length(party.indexes)){
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- current.avg.fare

first.Mr.df <- data.combined[indexes.first.Mr, ]
summary(first.Mr.df)

# visualize new features
ggplot(first.Mr.df[first.Mr.df$Survived != "None", ],
       aes(x = ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("survival rates 1st class Mr. by ticket.party.size")

ggplot(first.Mr.df[first.Mr.df$Survived != "None", ],
       aes(x = avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("survival rates 1st class Mr. by avg.fare")

summary(data.combined$avg.fare)

data.combined[is.na(data.combined$avg.fare), ]

indexes <- with(data.combined, which(Pclass == "3" & Title == "Mr." &
                                       Family.size == 1 &
                                       Ticket != "3701"))
similar.na.passenders <- data.combined[indexes, ]
summary(similar.na.passenders$avg.fare)

data.combined[is.na(avg.fare), "avg.fare"] <- median(avg.fare)

# normalize data
preproc.data.combined <- data.combined[,
                                       c("ticket.party.size",
                                         "avg.fare")]
preProc <- preProcess(preproc.data.combined,
                      method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

cor(postproc.data.combined$ticket.party.size, 
    postproc.data.combined$avg.fare)

indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes],
    postproc.data.combined$avg.fare[indexes])


features <- c("Pclass", "New.titles", "Family.size",
              "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)



















