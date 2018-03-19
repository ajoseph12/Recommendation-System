#############################################################
# Data Mining Project - Recommender 
# Computer Lab Session n?1:
# Mining Data with R: Basics
#############################################################


########
# To do #
########
- find relation between features 10-24 
- feature engineering 
- try showing gaussian curve and then correct it by normalising it.
- try producing a spacrity graph (maybe something like, class value = 1, only when rows a full 
                                  given user. )

- try running algos before and after log transformation of the features 


- Algos
  - logistic (wtd/normal)
  - KNN 
  - SVMs
  - Recommendation systems
  - 

########
# Keys #
########
bi - binary data
int - integer
R - real number

0- useless
1- meh 
2- interesting
3- word

#############
# Libraries #
#############
library(ggplot2)
library(caret)
library(corrplot)
library(dplyr)
library(tidyr)
library(e1071)


#################################
# Reading, Cleaning and Preparing #
#################################
setwd("~/Desktop/MLDM master/Semester 2/Data Mining/DM_project/") #- setting to working directory

crowd_fund <- read.csv("~/Desktop/MLDM master/Semester 2/Data Mining/DM_project/recos_training.csv",
                  sep = ",", 
                  header = TRUE, 
                  na.strings = c("NA","#DIV/0!",""))
crowd_fund$id<- NULL
train <- original
## checking for NA values
any(is.na(train))

## converting 'numeric' boolean columns to factors
# which columns are boolean 
which(apply(train,2,function(x) { all(x %in% 0:1) }))
# converting these to factors
train$owner_friend <- as.factor(train$owner_friend)
train$same_owner <- as.factor(train$same_owner)
train$contrib <- as.factor((train$contrib))
# summary again
summary(train)

#######################################
# Exploration and preliminary analysis #
#######################################

## Exploration - hitting features one at a time

# owner_friend - if the head of the recommended project was one the user had supported before.(bi)
"seems like owner friend wouldn't be of much use, given a single positive instance, 
let's give that instance a look though, just to be sure."
train[train$owner_friend == 1,]
"Hahaha, seems like user 318 had contributed only because he was a friend of the owner. 
Or more like the user had created an account on kisskissbankbank for the sole purpose of supporting
his friend."
0

# dist - physical distance between user and project recommended (int, btw 0 and 1(>100km))
"Let's use distance as a predictor as to whether the user would contribute"
ggplot(train, aes(dist, fill = contrib)) + geom_histogram(position = "stack", binwidth=.1)
"So, there wasn't any definite observeable patterns here, apart from the fact that people at the 
center, half way from the center and within 100km and further had contirbuted to the project 
recommende. Definitely went against my intuition that farther the users lesser they will contribute"
2

# nb_copledgers - contirbuting users who have contributed to similar projects as the user in the past
#(R, btw 0 and 1, so if the value is 0.4, and if 100 ussers have contributed to the recommended 
# project, then 40 of them had contributed towards projects similar to the user in the past.)
"So if I'm not wrong, one hopes that the user will contribute if he/she has a high nb_copledgers 
value."
ggplot(train, aes(nb_copledgers, fill = contrib)) + geom_histogram(position = "stack", binwidth=.1)
train[train$nb_copledgers > 0.8,]
"Yes, and no. As opposed to my intuition that if the value was higher, higher the chances that the
user would contribute goes into the shitter. In fact more users with nb_copledgers value = 0
have contributed. Which basically means, if 100 people contributed, none of them had contributed to 
similar projects as the user"
2

# amount_copledger - investments by users who have invested in similar projects as given user. (R)
plot(train$nb_copledgers, train$amount_copledgers)
"I wasn't too sure what to plot, so I threw in a simple xy plot. And contrary to believes that
more the number of copledgers more will be the investement, the plot shows otherwise. More the
number of copledgers lower the investemtn."
1

# desc_score_mean - A value which express similarity between projects proposed and projects the user
# has already participated in. (R)
ggplot(train, aes(desc_score_mean, fill = contrib)) + geom_histogram(position = "stack", binwidth=.1)
"My initial intuition was more correlated the recommended projects were with the projects the user
has invested in, more would be the chance that the user would contribute. While a peak 
in  contribution is observed at 0.8, almost none is seen with at 1. And a secondary peak 
is observed when the value is 0. Which goes to say, this attribute does play a minor role in 
providing forsight as to whether the user would contribute."
2

# nb_friend - number of Fb friends of the user who has supported the recommend project. (int)
ggplot(train, aes(nb_friends, fill = contrib)) + geom_histogram(position = "stack", binwidth=.1)
"Well, less than 0.2% of the instances had at least one friend (25 instances to be precise). Off 
of which only 20% contributed (5 to be precise). So, this can't be considered a robust feature."
1

# same_owner - a boolean which expresses the fact that the user has already supported a project 
# headed by the same project leader as the recommended project.(bi)
sum(train$same_owner == 1 & train$contrib == 1)
sum(train$same_owner == 1 & train$contrib == 0)
"So, the fact that the head of the recommended project was supported by the targeted user in the
past, does seem to have an effect on the feature 'contrib' but these instances are outweighed by
the fact that more users haven't contributed towards projects wherein the haeads were people the 
users had supported in the past."
2

"To conclude, one can safely say that not many robust features were present. as will be 
corroborated by the correlation plot"
corrplot(cor(original[,c(2:9,25)]), type = "lower")

"The features the follow after 'same_owner' are principle categories - music, live.performance,
journalism, book.and.publshing, design.and.innovation, film.and.video, style, photography, social,
web.and.tech, education, art, adventure.and.sport, food, and ecology - of projects hosted on the
crowdfunding platform. Each user has a value assigned to each of these features that range from 
0-1. O would mean that the user has never participated in a project of this genre or the proposed 
project isn't in this category. 1 would mean that the user is completely into project of a given 
genre"

# mainly zeros plot 
zero_values <- as.data.frame(colSums(original[,-25] == 0))
ggplot(zero_values,aes(x=reorder(rownames(zero_values),zero_values[,]),y=zero_values[,])) + 
  geom_bar(stat="identity",fill="red")+coord_flip()+theme_bw() + ylab("Count") + xlab("Features")
## Prelimnary analysis 

#checking the effect of PCA on the genre columns 
original.pca <- prcomp(original[1:24],center = TRUE,scale = FALSE) 
summary <- summary(original.pca)
plot(summary$importance[2,], type = "l",ylab = "Importance (Variance)", xlab = "Features 10-24")

"It seems like each of the principle component captures a small portion of the variance. Hence
using PCA on the genere components wouldn't make much sense."
"Before i go ahead and discard the genre features, i wanted to try and extract only the most 
information from them. Unforunately the PCA feature had similar variance/info. So i guess i'll 
delete all of em"

#corr between desc_mean and genre
corrplot(cor(original[,c(5,10:24)]), type = "lower")
"Barely any correlation betwwen desc_mean and gneres."





#######################################
# Modeling #
#######################################


"You can show two scnearios, one logistic with PCA and another withoug"


inTrain <- createDataPartition(train$contrib, p=0.7, list=FALSE)
my_train<- train[inTrain, ]
my_test<- train[-inTrain, ]

# Logistic regression without weights
logreg_1<- glm(contrib~., data = train, family  = "binomial")
pred_logr_1<- predict(logreg_1, my_test,type = "response")
table(ActualValue = my_test$contrib, PredictedValue = pred_logr_1 > 0.5)
summary(logreg_1)
PredictedValue =  ifelse(pred_logr_1 > 0.5,1,0)
confusionMatrix(PredictedValue, my_test$contrib)
"Hopless performance with regard to recall. Also, try screening out shit variables"

# creating new train and test with important features
my_train_1<- my_train[c('Class',features)]
my_test_1<- my_test[c('Class',features)]

# Logistic regression with weights
Wt<- ifelse(my_train$contrib == 0 ,0.5, 19) # wt = (n_samples / (n_classes * np.bincount(y))
logreg_2<- glm(contrib~., data = my_train, family  = "binomial")
pred_logr_2<- predict(logreg_2, my_test,type = "response")
table(ActualValue = my_test$contrib, PredictedValue = pred_logr_2 > 0.5)
PredictedValue =  ifelse(pred_logr_2 > 0.5,1,0)
confusionMatrix(PredictedValue, my_test$contrib)
"You could run a loop to find best weight cross validate"
  

my_train_log<- log(train[inTrain, ])
my_test_log<- log(train[-inTrain, ])

#SVM 
my_train$V24<- as.factor(my_train$V24)
my_test$V24<- as.factor(my_test$V24)
svm_pca <- svm(V24 ~ ., data = my_train, cost = 100, gamma = 1,kernel = "radial")
svm_pred  <- predict(svm_pca, my_test[,-24], type = "class")
table(ActualValue = my_test$V24, PredictedValue = svm_pred)
confusionMatrix(svm_pred, my_test$V24)
logreg_2<- glm(V24~., data = my_train, family  = "binomial",weights = Wt)
pred_logr_2<- predict(logreg_2, my_test,type = "response")
table(ActualValue = my_test$V24, PredictedValue = pred_logr_2 > 0.5)
PredictedValue =  ifelse(pred_logr_2 > 0.5,1,0)
confusionMatrix(PredictedValue, my_test$V24)


svm <- svm(contrib ~ ., data = my_train, cost = 1, gamma = 1,kernel = "linear")
svm_pred  <- predict(svm, my_test[,-24], type = "class")
table(ActualValue = my_test$contrib, PredictedValue = svm_pred)
confusionMatrix(svm_pred, my_test$contrib)
logreg_2<- glm(V24~., data = my_train, family  = "binomial",weights = Wt)
pred_logr_2<- predict(logreg_2, my_test,type = "response")
table(ActualValue = my_test$V24, PredictedValue = pred_logr_2 > 0.5)
PredictedValue =  ifelse(pred_logr_2 > 0.5,1,0)
confusionMatrix(PredictedValue, my_test$V24)















