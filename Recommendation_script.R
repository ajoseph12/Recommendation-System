#############################################################
# Data Mining Project - Recommender 
# Computer Lab Session n?1:
# Mining Data with R: Basics
#############################################################


########
# To do #
########
- talk about the pair plot 

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

crowd_fund <- read.csv("~/Desktop/MLDM master/Semester 2/Data Mining/DM_project/Recommendation-System/recos_training.csv",
                  sep = ",", 
                  header = TRUE, 
                  na.strings = c("NA","#DIV/0!",""))

crowd_fund_test <- read.csv("~/Desktop/MLDM master/Semester 2/Data Mining/DM_project/Recommendation-System/recos_test.csv",
                            sep = ",", 
                            header = TRUE, 
                            na.strings = c("NA","#DIV/0!",""))
crowd_fund$id<- NULL
crowd_fund_test$id<- NULL

train <- crowd_fund
## checking for NA values
any(is.na(train))

## converting 'numeric' boolean columns to factors
# which columns are boolean 
which(apply(train,2,function(x) { all(x %in% 0:1) }))
# converting these to factors
train$owner_friend <- as.factor(train$owner_friend)
train$same_owner <- as.factor(train$same_owner)
train$contrib <- as.factor((train$contrib))
crowd_fund_test$owner_friend <- as.factor(crowd_fund_test$owner_friend )
crowd_fund_test$same_owner <- as.factor(crowd_fund_test$same_owner)
crowd_fund_test$contrib <- as.factor(crowd_fund_test$contrib)

summary(train)

#######################################
# Exploration and preliminary analysis #
#######################################

## Exploration - hitting features one at a time

# owner_friend - if the head of the recommended project was one the user had supported before.(bi)
"seems like owner friend wouldn't be of much use, given a single positive instance, 
let's give that instance a look though, just to be sure."
train[train$owner_friend == 1,]
"Seems like user 318 had contributed only because he was a friend of the owner. 
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
zero_values <- as.data.frame(colSums(crowd_fund[,-25] == 0))
ggplot(zero_values,aes(x=reorder(rownames(zero_values),zero_values[,]),y=zero_values[,])) + 
  geom_bar(stat="identity",fill="red")+theme_bw() + ylab("Count") + xlab("Features") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
## Prelimnary analysis 

#checking the effect of PCA on the genre columns 
crowd_fund_pca <- prcomp(crowd_fund[1:23],center = TRUE,scale = TRUE) 
summary <- summary(crowd_fund_pca)
crowd_fund_pca_dat<- predict(crowd_fund_pca)
crowd_fund_pca_dat <- cbind(crowd_fund_pca_dat,crowd_fund$contrib)
plot(summary$importance[2,], type = "l",ylab = "Importance (Variance)", xlab = "Features 10-24")

"It seems like each of the principle component captures a small portion of the variance. Hence
using PCA on the genere components wouldn't make much sense."
"Before i go ahead and discard the genre features, i wanted to try and extract only the most 
information from them. Unforunately the PCA feature had similar variance/info. So i guess i'll 
delete all of em"

#corr between desc_mean and genre
corrplot(cor(original[,c(5,10:24)]), method = "square")
"Barely any correlation betwwen desc_mean and gneres."

# Train set with PCA preprocessing 
train_p<- predict(crowd_fund_pca) # suffix p stands for preprocess 
train_p<- as.data.frame(train_p)
train_p <- cbind(train_p,train$contrib)
colnames(train_p)[24]<- "contrib"

# Train validation split
inTrain <- createDataPartition(crowd_fund$contrib, p=0.7, list=FALSE)
my_train_p<- train_p[inTrain, ]
my_test_p<- train_p[-inTrain, ]
my_train<- train[inTrain, ]
my_test<- train[-inTrain, ]




#############################################
# Data preparation, Modeling and Evaluation #
#############################################


#-------------------------------------------------------------------normal_logistic
### Normal logistic regression (logreg_1)

## Modeling 

# With pca and other preprocess
logreg_1_p<- glm(contrib~., data = my_train_p, family  = "binomial")
pred_logr_1_p<- predict(logreg_1_p, my_test_p,type = "response")
#table(ActualValue = my_test_p$contrib, PredictedValue = pred_logr_1_p > 0.5)
#summary(logreg_1)
PredictedValue =  ifelse(pred_logr_1_p > 0.5,1,0)
cm_n_p<- confusionMatrix(PredictedValue, my_test_p$contrib)

# Without pca and other preprocess
logreg_1<- glm(contrib~., data = my_train, family  = "binomial")
pred_logr_1<- predict(logreg_1, my_test,type = "response")
#table(ActualValue = my_test$contrib, PredictedValue = pred_logr_1 > 0.5)
#summary(logreg_1)
PredictedValue =  ifelse(pred_logr_1 > 0.5,1,0)
cm_n <- confusionMatrix(PredictedValue, my_test$contrib)

#-------------------------------------------------------------------weighted_logistic
### Weighted logistic regression (logreg_2)

## Modeling 
wt<- ifelse(my_train$contrib == 0 ,0.5, 19)

# With pca and other preprocess
logreg_2_p<- glm(contrib~., data = my_train_p, family  = "binomial" ,weights = wt)
pred_logr_2_p<- predict(logreg_2_p, my_test_p,type = "response")
#table(ActualValue = my_test_p$contrib, PredictedValue = pred_logr_2_p > 0.5)
#summary(logreg_1)
PredictedValue =  ifelse(pred_logr_2_p > 0.5,1,0)
cm_w_p<- confusionMatrix(PredictedValue, my_test_p$contrib)

# Without pca and other preprocess
logreg_2<- glm(contrib~., data = my_train, family  = "binomial",weights = wt)
pred_logr_2<- predict(logreg_2, my_test,type = "response")
#table(ActualValue = my_test$contrib, PredictedValue = pred_logr_2 > 0.5)
#summary(logreg_1)
PredictedValue =  ifelse(pred_logr_2 > 0.5,1,0)
cm_w<- confusionMatrix(PredictedValue, my_test$contrib)

#------------------------------------results for weighted/normal logistic with and without preprocess

### Storing Accuracy, Sensitivity and Specificity resutls after above modelling
norm_log <-  c(cm_n$overall['Accuracy'], cm_n$byClass['Sensitivity'], 
               cm_n$byClass['Specificity']) 
norm_log_p <- c(cm_n_p$overall['Accuracy'], cm_n_p$byClass['Sensitivity'], 
                cm_n_p$byClass['Specificity']) 
weight_log <- c(cm_w$overall['Accuracy'], cm_w$byClass['Sensitivity'], 
                cm_w$byClass['Specificity']) 
weight_log_p <- c(cm_w_p$overall['Accuracy'], cm_w_p$byClass['Sensitivity'], 
                  cm_w_p$byClass['Specificity']) 

df_resutls <- data.frame(norm_log, norm_log_p, weight_log, weight_log_p)
colnames(df_resutls)<- c("Logistic Reg", "Logistic Reg with preprocess",
                         "Weighted Logistic Reg", "Weighted Logistic Reg with preprocess")

kable(df_resutls, caption = "Accuracy, Sensitivity and Specificity for
      the Logistic Regression models realised")





### Undersampling with logistic regression, SVMs and Randomforest 

#-------------------------------------------------------------------undersampled_logistic_svm_rf

## Data preprocessing - Undersampling
temp <- data<-ubUnder(X=my_train[,-24], Y= my_train$contrib, perc = 50, method = "percPos")
data_under<-cbind(temp$X, temp$Y)
colnames(data_under)[24]<- "contrib"

## Modeling

#Logistic Regression
logreg_3<- glm(contrib~., data = data_under, family  = "binomial")
pred_logr_3<- predict(logreg_3, my_test,type = "response")
PredictedValue =  ifelse(pred_logr_3 > 0.5,1,0)
cm_u_log<- confusionMatrix(PredictedValue, my_test$contrib)

Accuracy : 0.7788 
Sensitivity : 0.78296         
Specificity : 0.62385 

#SVM
svm_1 <- svm(contrib ~ ., data = data_under,kernel = "radial")
svm_pred_1  <- predict(svm_1, my_test, type = "class")
cm_u_svm<- confusionMatrix(svm_pred_1, my_test$contrib)

Accuracy : 0.7865
Sensitivity : 0.79062         
Specificity : 0.63303 

# Random Forest
rf_1<- randomForest(contrib ~ ., data = data_under, importance = TRUE, ntree = 1000)
rf_pred_1  <- predict(rf_1, my_test, type = "class")
cm_u_rf<- confusionMatrix(rf_pred_1, my_test$contrib)

Accuracy : 0.6987
Sensitivity : 0.69704         
Specificity : 0.76147

#----------------------------------------------------------dataframe with undersampled results

## Storing Accuracy, Sensitivity and Specificity resutls after above modelling

Logistic_Undersampled <-  c(cm_u_log$overall['Accuracy'], cm_u_log$byClass['Sensitivity'], 
                            cm_u_log$byClass['Specificity']) 
SVM_Undersampled <- c(cm_u_svm$overall['Accuracy'], cm_u_svm$byClass['Sensitivity'], 
                      cm_u_svm$byClass['Specificity']) 
RandomForest_Undersampled <- c(cm_u_rf$overall['Accuracy'], cm_u_rf$byClass['Sensitivity'], 
                               cm_u_rf$byClass['Specificity']) 

df_under <- data.frame(Logistic_Undersampled, SVM_Undersampled, RandomForest_Undersampled)
kable(df_under, caption = "Accuracy, Sensitivity and Specificity for
      the Logistic Regression, SVM and Random Forest models realised on Undersampled data ")



### Oversampling with logistic regression, SVMs and Randomforest 
#-------------------------------------------------------------------oversampled_logistic_svm_rf

## Data preprocessing - Oversampling
temp<- ubOver(X=my_train[,-24],  Y= my_train$contrib, k = 0, verbose=TRUE)
data_over<-cbind(temp$X, temp$Y)
colnames(data_over)[24]<- "contrib"

## Modeling 

#Logistic Regression
logreg_4<- glm(contrib~., data = data_over, family  = "binomial")
pred_logr_4<- predict(logreg_4, my_test,type = "response")
#table(ActualValue = my_test$contrib, PredictedValue = pred_logr_4 > 0.5)
PredictedValue =  ifelse(pred_logr_4 > 0.5,1,0)
cm_o_log<- confusionMatrix(PredictedValue, my_test$contrib)

Accuracy : 0.8161 
ensitivity : 0.82173         
Specificity : 0.60550 

# SVM
svm_2 <- svm(contrib ~ ., data = data_over,kernel = "radial")
svm_pred_2  <- predict(svm_2, my_test, type = "class")
cm_o_svm<- confusionMatrix(svm_pred_2, my_test$contrib)

Accuracy : 0.7783
Sensitivity : 0.78148         
Specificity : 0.66055 

# Random Forest
rf_2<- randomForest(contrib ~ ., data = data_over, importance = TRUE, ntree = 1000)
rf_pred_2  <- predict(rf_2, my_test, type = "class")
cm_o_rf<- confusionMatrix(rf_pred_2, my_test$contrib)

Accuracy : 0.8947
Sensitivity : 0.91012        
Specificity : 0.32110 

#----------------------------------------------------------dataframe with oversampled results

## Storing Accuracy, Sensitivity and Specificity resutls after above modelling

Logistic_Oversampled <-  c(cm_o_log$overall['Accuracy'], cm_o_log$byClass['Sensitivity'], 
                           cm_o_log$byClass['Specificity']) 
SVM_Oversampled <- c(cm_o_svm$overall['Accuracy'], cm_o_svm$byClass['Sensitivity'], 
                     cm_o_svm$byClass['Specificity']) 
RandomForest_Oversampled <- c(cm_o_rf$overall['Accuracy'], cm_o_rf$byClass['Sensitivity'], 
                              cm_o_rf$byClass['Specificity']) 

df_under <- data.frame(Logistic_Oversampled, SVM_Oversampled, RandomForest_Oversampled)
kable(df_under, caption = "Accuracy, Sensitivity and Specificity for
      the Logistic Regression, SVM and Random Forest models realised on Oversampled data")



### SmoteTomek hybrid sampling with logistic regression, SVMs and Randomforest 
#-------------------------------------------------------------------SmoteTomek_logistic_svm_rf

## Data preprocessing - SmoteTomek hybrid sampling
inTrain <- createDataPartition(crowd_fund$contrib, p=0.7, list=FALSE)
my_train_num<- crowd_fund[inTrain, ]
my_test_num<- crowd_fund[-inTrain, ]

temp<- ubSMOTE(X= my_train_num[,-24],  Y= my_train$contrib,
               perc.over = 2000, k = 5, perc.under = 200, verbose = TRUE)
data_smote<-cbind(temp$X, temp$Y)
colnames(data_smote)[24]<- "contrib"

temp<- ubTomek(X= data_smote[,-24],  Y= data_smote$contrib, verbose = TRUE)
data_tomsmot<-cbind(temp$X, temp$Y)
colnames(data_tomsmot)[24]<- "contrib"
summary(data_tomsomt$contrib)

## Modeling

# Logistic Regresion
logreg_8<- glm(contrib~., data = data_tomsmot, family  = "binomial")
pred_logr_8<- predict(logreg_8, my_test_num,type = "response")
PredictedValue =  ifelse(pred_logr_8 > 0.5,1,0)
cm_st_log<- confusionMatrix(PredictedValue, my_test_num$contrib)

Accuracy : 0.9401 
Sensitivity : 0.9614          
Specificity : 0.1770 

# SVM
svm_3 <- svm(contrib ~ ., data = data_tomsomt,kernel = "radial")
svm_pred_3  <- predict(svm_3, my_test_num, type = "class")
cm_st_svm<- confusionMatrix(svm_pred_3, my_test$contrib)

Accuracy : 0.9731 
Sensitivity : 0.97358        
Specificity : 0.05505  

# Random Forest
rf_3<- randomForest(contrib ~ ., data = data_tomsomt, importance = TRUE, ntree = 1000)
rf_pred_3  <- predict(rf_3, my_test_num, type = "class")
cm_st_rf<- confusionMatrix(rf_pred_3, my_test_num$contrib)

Accuracy : 0.6987
Sensitivity : 0.69704         
Specificity : 0.76147

#---------------------------------------------------dataframe with SmoteTomek hybrid sampling results

## Storing Accuracy, Sensitivity and Specificity resutls after above modelling

Logistic_SmoteTomek <-  c(cm_st_log$overall['Accuracy'], cm_st_log$byClass['Sensitivity'], 
                          cm_st_log$byClass['Specificity']) 
SVM_SmoteTomek <- c(cm_st_svm$overall['Accuracy'], cm_st_svm$byClass['Sensitivity'], 
                     cm_st_svm$byClass['Specificity']) 
RandomForest_SmoteTomek <- c(cm_st_rf$overall['Accuracy'], cm_st_rf$byClass['Sensitivity'], 
                              cm_st_rf$byClass['Specificity']) 

df_st_hybrid <- data.frame(Logistic_SmoteTomek, SVM_SmoteTomek, RandomForest_SmoteTomek)
kable(df_st_hybrid, caption = "Accuracy, Sensitivity and Specificity for
      the Logistic Regression, SVM and Random Forest models realised on SmoteTomek sampled data")


### SmoteEnn hybrid sampling with logistic regression, SVMs and Randomforest 
#-------------------------------------------------------------------SmoteEnn_logistic_svm_rf

## Data preprocessing - SmoteEnn hybrid sampling
temp<- ubSMOTE(X= my_train_num[,-24],  Y= my_train$contrib,
               perc.over = 2000, k = 5, perc.under = 100, verbose = TRUE)
data_smote<-cbind(temp$X, temp$Y)
colnames(data_smote)[24]<- "contrib"

temp<- ubENN(X= data_smote[,-24],  Y= data_smote$contrib, k = 3, verbose = TRUE)
data_ennsmot<-cbind(temp$X, temp$Y)
colnames(data_ennsmot)[24]<- "contrib"
summary(data_ennsmot$contrib)

## Modeling

# Logistic Regression
logreg_9<- glm(contrib~., data = data_ennsmot, family  = "binomial")
pred_logr_9<- predict(logreg_9, my_test_num,type = "response")
PredictedValue =  ifelse(pred_logr_9 > 0.5,1,0)
cm_se_log<- confusionMatrix(PredictedValue, my_test_num$contrib)

Accuracy : 0.8901 
Sensitivity : 0.90583         
Specificity : 0.32743   


# SVM
svm_4 <- svm(contrib ~ ., data = data_ennsmot,kernel = "radial")
svm_pred_4  <- predict(svm_4, my_test_num, type = "class")
cm_se_svm<- confusionMatrix(svm_pred_4, my_test$contrib)

Accuracy : 0.8764
Sensitivity : 0.89556        
Specificity : 0.16514 

# Random Forest
rf_4<- randomForest(contrib ~ ., data = data_ennsmot, importance = TRUE, ntree = 1000)
rf_pred_4  <- predict(rf_4, my_test_num, type = "class")
cm_se_rf<- confusionMatrix(rf_pred_4, my_test_num$contrib)

Accuracy : 0.9327
Sensitivity : 0.9582          
Specificity : 0.0177 


#---------------------------------------------------dataframe with SmoteEnn hybrid sampling results

## Storing Accuracy, Sensitivity and Specificity resutls after above modelling

Logistic_SmoteEnn <-  c(cm_st_log$overall['Accuracy'], cm_st_log$byClass['Sensitivity'], 
                          cm_st_log$byClass['Specificity']) 
SVM_SmoteEnn <- c(cm_st_svm$overall['Accuracy'], cm_st_svm$byClass['Sensitivity'], 
                    cm_st_svm$byClass['Specificity']) 
RandomForest_SmoteEnn <- c(cm_st_rf$overall['Accuracy'], cm_st_rf$byClass['Sensitivity'], 
                             cm_st_rf$byClass['Specificity']) 

df_se_hybrid <- data.frame(Logistic_SmoteEnn, SVM_SmoteEnn, RandomForest_SmoteEnn)
kable(df_se_hybrid, caption = "Accuracy, Sensitivity and Specificity for
      the Logistic Regression, SVM and Random Forest models realised on SmoteEnn sampled data")



#############################################
# Deployment #
#############################################

# Logistic Regression weighted
pred_logr_2<- predict(logreg_2, crowd_fund_test,type = "response")
PredictedValue =  ifelse(pred_logr_2 > 0.5,1,0)
cm_wt_log<- confusionMatrix(PredictedValue, crowd_fund_test$contrib)

# Logistic Regression undersampled
#logreg_3<- glm(contrib~., data = data_under, family  = "binomial")
pred_logr_3<- predict(logreg_3, crowd_fund_test,type = "response")
PredictedValue =  ifelse(pred_logr_3 > 0.5,1,0)
cm_u_log<- confusionMatrix(PredictedValue, crowd_fund_test$contrib)

# SVM undersampled
#svm_1 <- svm(contrib ~ ., data = data_under,kernel = "radial")
svm_pred_1  <- predict(svm_1, crowd_fund_test, type = "class")
cm_u_svm<- confusionMatrix(svm_pred_1, crowd_fund_test$contrib)

# Random Forest undersampled
#rf_1<- randomForest(contrib ~ ., data = data_under, importance = TRUE, ntree = 1000)
rf_pred_1  <- predict(rf_1, crowd_fund_test, type = "class")
cm_u_rf<- confusionMatrix(rf_pred_1, crowd_fund_test$contrib)

#---------------------------------------------------dataframe with final modelling results

## Storing Accuracy, Sensitivity and Specificity resutls after above modelling

Logistic_Weighted <-  c(cm_wt_log$overall['Accuracy'], cm_wt_log$byClass['Sensitivity'], 
                        cm_wt_log$byClass['Specificity']) 
Logistic_Undersampled <-  c(cm_u_log$overall['Accuracy'], cm_u_log$byClass['Sensitivity'], 
                            cm_u_log$byClass['Specificity']) 
SVM_Undersampled <- c(cm_u_svm$overall['Accuracy'], cm_u_svm$byClass['Sensitivity'], 
                      cm_u_svm$byClass['Specificity']) 
RandomForest_Undersampled <- c(cm_u_rf$overall['Accuracy'], cm_u_rf$byClass['Sensitivity'], 
                               cm_u_rf$byClass['Specificity']) 

df_final <- data.frame(Logistic_Weighted,Logistic_Undersampled, 
                       SVM_Undersampled, RandomForest_Undersampled)
kable(df_final, caption = "Accuracy, Sensitivity and Specificity for the final set of
      choosen models ")


















#############
# ARCHIVE #
#############


#-------------------------------------------------------------------tomek_logistic

# inorder to computer tomek links, only numeric feauters are allowed 
inTrain <- createDataPartition(crowd_fund$contrib, p=0.7, list=FALSE)
my_train_num<- crowd_fund[inTrain, ]
my_test_num<- crowd_fund[-inTrain, ]

temp<- ubTomek(X= my_train_num[,-24],  Y= my_train_num$contrib, verbose = TRUE)
data_tomek<-cbind(temp$X, temp$Y)
colnames(data_tomek)[24]<- "contrib"

logreg_5<- glm(contrib~., data = data_tomek, family  = "binomial")
pred_logr_5<- predict(logreg_5, my_test_num,type = "response")
table(ActualValue = my_test_num$contrib, PredictedValue = pred_logr_5 > 0.5)
summary(logreg_7)
PredictedValue =  ifelse(pred_logr_5 > 0.5,1,0)
confusionMatrix(PredictedValue, my_test_num$contrib)


#-------------------------------------------------------------------smote_logistic

temp<- ubSMOTE(X= my_train[,-24],  Y= my_train$contrib,
               perc.over = 2000, k = 5, perc.under = 200, verbose = TRUE)
data_smote<-cbind(temp$X, temp$Y)
colnames(data_smote)[24]<- "contrib"

logreg_6<- glm(contrib~., data = data_smote, family  = "binomial")
pred_logr_6<- predict(logreg_6, my_test,type = "response")
table(ActualValue = my_test$contrib, PredictedValue = pred_logr_6 > 0.5)
summary(logreg_7)
PredictedValue =  ifelse(pred_logr_6 > 0.5,1,0)
confusionMatrix(PredictedValue, my_test$contrib)


#-------------------------------------------------------------------ENN_logistic

temp<- ubENN(X= my_train_num[,-24],  Y= my_train_num$contrib, k = 3, verbose = TRUE)
data_enn<-cbind(temp$X, temp$Y)
colnames(data_enn)[24]<- "contrib"

logreg_7<- glm(contrib~., data = data_enn, family  = "binomial")
pred_logr_7<- predict(logreg_7, my_test_num,type = "response")
table(ActualValue = my_test_num$contrib, PredictedValue = pred_logr_7 > 0.5)
summary(logreg_7)
PredictedValue =  ifelse(pred_logr_7 > 0.5,1,0)
confusionMatrix(PredictedValue, my_test_num$contrib)


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
Wt<- ifelse(train$contrib == 0 ,0.5, 19) # wt = (n_samples / (n_classes * np.bincount(y))
logreg_2<- glm(contrib~., data = train, family  = "binomial",weights = wt)
pred_logr_2<- predict(logreg_2, my_test,type = "response")
table(ActualValue = my_test$contrib, PredictedValue = pred_logr_2 > 0.5)
PredictedValue =  ifelse(pred_logr_2 > 0.5,1,0)
confusionMatrix(PredictedValue, my_test$contrib)
"You could run a loop to find best weight cross validate"
  

temp <- data<-ubUnder(X=my_train[,-24], Y= my_train$contrib, perc = 50, method = "percPos")
data_under<-cbind(temp$X, temp$Y)
colnames(data_under)[24]<- "contrib"

## Modeling

#Logistic Regression
logreg_3<- glm(contrib~., data = data_under, family  = "binomial")
PredictedValue =  ifelse(pred_logr_3 > 0.5,1,0)
confusionMatrix(PredictedValue, crowd_fund_test$contrib)








