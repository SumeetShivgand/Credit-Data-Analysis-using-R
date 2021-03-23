
#installed readxl package
install.packages("readxl")
library(readxl)
library(tidyverse)

#1. Use the readxl package to read the sheet from  "Credit_Risk6_final.xlsx" file.
sheet1<- read_excel("D:\\CIT\\Data Science and Analytics\\Assignment\\Credit_Risk6_?inal.xlsx", sheet="Training_Data")

#Generate the dataframe for above generated sheet.
df <- data.frame(sheet1)
View(df)
str(df)

#install DataExplorer package
install.packages("DataExplorer")
library(DataExplorer)

#Checking for missing values before divi?g deep into the analysis.
plot_missing(df)

#copying original data
df1<- df

summary(df1)

install.packages("VIM")
library(VIM)

df1<- kNN(df1, variable = c("Housing","Personal.Status","Employment"),k=5)
View(df1)
plot_missing(df1)
str(df1)

df1<- subset(d?1, select = Checking.Acct:Credit.Standing)
View(df1)
sum(is.na(df1))

#Convert all character variables to Factors
library(magrittr)

cols1 <- c("Checking.Acct","Credit.History","Loan.Reason","Savings.Acct","Employment",
           "Personal.Status","Housin?","Job.Type","Foreign.National","Credit.Standing" )
df1 %<>%
  mutate_each_(funs(factor(.)),cols1)
str(df1)

#######Trivariate Analysis############
ggplot(data = df1)+geom_bar(aes(x=Credit.Standing,fill=as.factor(Credit.History)))+
  ggtitle(label = "Triva?iate analysis")+
  theme_bw()+facet_wrap(~Checking.Acct)


ggplot(data = df1)+geom_bar(aes(x=Credit.Standing,fill=as.factor(Credit.History)))+
  ggtitle(label = "Trivariate analysis")+
  theme_bw()+facet_wrap(~Job.Type)
################Question B##########?#

install.packages("tree")
library(tree)


Credit.Standing <- df1$Credit.Standing

tree.df1 = tree(Credit.Standing~., df1)
summary(tree.df1)
table(df1$Credit.Standing)/nrow(df1)

windows(20,40)
plot(tree.df1)
text(tree.df1,pretty=1)
tree.df1

set.seed(850?
train=sample(1:nrow(df1), 585)
df1.test=df1[-train,]

credit.test<- Credit.Standing[-train]

tree.df1 = tree(Credit.Standing~.,df1,subset=train)
tree.pred = predict(tree.df1, df1.test, type="class")
table(tree.pred,credit.test)


library(caret)
confusionM?trix(table(tree.pred,credit.test))

cv.df1 <- cv.tree(tree.df1,K=10, FUN=prune.misclass)

#10 folds of cross validation are being used.

names(cv.df1)
cv.df1

# size 13 is the best cross validation error rate, i.e.

par(mfrow=c(1,2))
plot(cv.df1$size,cv.df?$dev,type="b")
plot(cv.df1$k,cv.df1$dev,type="b")


#Look up this misclassification parameter k in ISLR page 309 formula 8.4 where it is called alpha.

# Size of tree = 8 looks best here, using occam's razor, ie.all else being equal.

prune.df1<-prune.misc?ass(tree.df1,best=9)
plot(prune.df1)
text(prune.df1,pretty=0)
tree.pred.df1<-predict(prune.df1,df1.test,type="class")
table(tree.pred.df1,credit.test)
confusionMatrix(table(tree.pred.df1,credit.test))
#Accuracy increase


##########################BY Rpart?##################

id<- sample(2,nrow(df1),prob = c(0.70,0.30),replace = TRUE) #79.27%(75/25 %)
df1_train<-df1[id==1,]
df1_test<-df1[id==2,]
View(df1_train)
library(rpart)
train1<-sample(1:nrow(df1_train))
View(train1)
cr.test<- Credit.Standing[-train1]
V?ew(cr.test)

set.seed(850)
df1_model<-rpart(Credit.Standing~., data = df1_train,
                 control = rpart.control(cp = 0.002,xval = 10,maxdepth = 5))
df1_model
summary(df1_model)
plot(df1_model,margin = 0.1)
text(df1_model,use.n = TRUE,pretty = TRU?,cex=0.8)

pred_df1<-predict(df1_model,newdata = df1_test,type = "class")
pred_df1

table(pred_df1,df1_test$Credit.Standing)

confusionMatrix(table(pred_df1,df1_test$Credit.Standing))
#newone 72.6


########################Question C ##############


sheet?<- read_excel("D:\\CIT\\Data Science and Analytics\\Assignment\\Credit_Risk6_final.xlsx", sheet="Scoring_Data")

#Generate the dataframe for above generated sheet.
Scoring_data <- data.frame(sheet2)
View(Scoring_data)
str(Scoring_data)

#Rename the column ?ame
setnames(Scoring_data, old= "Residence.Time", new= "Residence.Time..In.current.district.")

#Adding extra columns to match the arguments in train data
Scoring_data$Credit.Standing = ""

Scoring_test<- subset(Scoring_data, select = Checking.Acct:Credit.?tanding)

cols <- c("Checking.Acct","Credit.History","Loan.Reason","Savings.Acct","Employment",
          "Personal.Status","Housing","Job.Type","Foreign.National","Credit.Standing" )
Scoring_test %<>%
  mutate_each_(funs(factor(.)),cols)

str(Scoring_test?
df_training <- df1
set.seed(850)
colnames(Scoring_test)

df1_pmodel<-rpart(Credit.Standing~., data = df_training)
df1_pmodel
summary(df1_pmodel)

plot(df1_pmodel,margin = 0.1)
text(df1_pmodel,use.n = TRUE,pretty = TRUE,cex=0.8)

pred_scoring<-predict(df1_?model,newdata = Scoring_test,type = "class")
pred_scoring


#################Question D##################
library(randomForest)
set.seed(850)
rf<- randomForest(Credit.Standing~., data = df1_train,ntree = 540,mtry = 3,importance = TRUE,proximity=TRUE)

prin?(rf)                                 #440/3=24.23
attributes(rf)

#Prediction and confusion matrix for train data
library(caret)
p1<-predict(rf,df1_train)

head(p1)
head(df1_train$Credit.Standing)
confusionMatrix(p1,df1_train$Credit.Standing)
# there is mi?match between OOB[Out Of Bag](26.41) and Accuracy(95.75)

#Prediction and confusion matrix for test data
p2<-predict(rf,df1_test)
confusionMatrix(p2,df1_test$Credit.Standing)

#Error rate of random Forest
plot(rf)

#Tune mtry
set.seed(850)
t<-tuneRF(df1_tr?in[,-13],df1_train[,13],stepFactor = 0.5,plot = TRUE,ntreeTry = 2000,trace = TRUE,improve = 0.05)


#No. of nodes for trees
hist(treesize(rf),main = "No. of nodes for the trees",col = "blue")

#Variable Importance
varImpPlot(rf) #or
varImpPlot(rf,sort = TR?E,n.var = 10,main = "Top 10 Importatn Variables")
importance(rf)
varUsed(rf)

##################ALternate random#############
set.seed(850)
credit.rf <- randomForest(Credit.Standing~.,data = df1, mtry=3, importance=TRUE)
print(credit.rf)
importance(credit.?f)
varImpPlot(credit.rf)

set.seed(850)
credit.rf1 <- randomForest(Credit.Standing~.,data = df1,subset = train1,ntree=1000, mtry=3, importance=TRUE)
print(credit.rf1) #OBB=23.67(76.33)


#Tune mtry
set.seed(850)
tuneRF(df1[,-13],df1[,13],subset=train1,step?actor = ,mtryStart = 3,ntreeTry = 500)

credit.rf<- randomForest(Credit.Standing~.,data = df1,subset = train1, mtry=3, importance=TRUE)
print(credit.rf)     #OBB23.13(76.87)
importance(credit.rf)
varImpPlot(credit.rf)

# Check on test set
set.seed(850)
pre?_rf2 <- predict(credit.rf, df1_test, type = "prob") # fill this in here but use type = "prob"
hist(pred_rf2[,1])
hist(pred_rf2[,2])
# Comment
pred_rf2 <- predict(credit.rf, df1_test, type = "class")

table(pred_rf2,cr.test)


###################BAgging wit? Random Forest############

set.seed(850)
rf_bagging<- randomForest(Credit.Standing~., data =df1_train,mtry = 13,importance = TRUE)
rf_bagging   


###############################################################
### Boosting for classification - gbm
instal?.packages("gbm")
library(gbm)
?gbm

df1$Credit.Standing1 <- as.numeric(df1$Credit.Standing)
df1$Credit.Standing1
df1$Credit.Standing
df1$Credit.Standing1 <- df1$Credit.Standing1-1 
df1$Credit.Standing1
# 1 is for Yes and 0 is for No Credit standing

set.se?d(850)
boost.df1=gbm(Credit.Standing1~.-Credit.Standing,data=df1[train1,],distribution="bernoulli",n.trees=5000, interaction.depth=4)
#shrinkage=0.01 is a paramater option
summary(boost.df1)
boost.df1

par(mfrow=c(1,2))
plot(boost.df1,i="Loan.Reason")
plot?boost.df1,i= "Months.since.Checking.Acct.opened")
plot(boost.df1,i="Age")
plot(boost.df1,i="Residence.Time..In.current.district.")
plot(boost.df1,i="Credit.History")
yhat_df_boost=predict(boost.df1,newdata=df1[-train1,],n.trees=5000,type = "response")

his?(yhat_df_boost)
predict_class1 <- ifelse(yhat_df_boost<0.5,"Bad","Good")

table(predict_class1,df1[-train1,"Credit.Standing1"])
#(118+58)/245= 71.8

#####################################################

install.packages("adabag")
library(adabag)
?adabag
??oosting
df.adabag <- boosting(Credit.Standing~.-Credit.Standing1, data = df1, boos = TRUE, mfinal=100)
df.adabag$importance
sort(df.adabag$importance)
importanceplot(df.adabag)

?boosting.cv

set.seed(850)
df.adabag.cv <- boosting.cv(Credit.Standing~.-Cred?t.Standing1, data = df1[train1,],v=10, boos = TRUE, mfinal=20)
df.adabag.cv
#(154+246)/535=74.77
#0.785 this is worse than gbm boosting, but is it a fair comparison?

set.seed(850)
df.adabag.cv <- boosting.cv(Credit.Standing~.-Credit.Standing1, data = df1[?rain1,],v=10, boos = TRUE, mfinal=50)
df.adabag.cv
#(152+246)/535 =74.39
# 0.805, This is better than with mfinal = 20.

set.seed(850)
df.adabag.cv <- boosting.cv(Credit.Standing~.-Credit.Standing1, data = df1[train1,],v=10, boos = TRUE, mfinal=30)
df.adab?g.cv
#(154+250)/535=75.51
# 0.80 this is similar to mfinal = 50

set.seed(850)
df.adabag.cv <- boosting.cv(Credit.Standing~.-Credit.Standing1, data = df1[train1,],v=10, boos = TRUE, mfinal=15)
df.adabag.cv
#(155+248)/535=75.32
# 0.80 this is the same as fo? mfinal = 50


# Now change rpart to maxdepth = 5
?rpart.control

df.adabag.cv <- boosting.cv(Credit.Standing~.-Credit.Standing1, data = df1[train1,],v=10, boos = TRUE, mfinal=20,control=rpart.control(maxdepth=5))
df.adabag.cv
#(152+252)/535=75.51
#0.81 th?s is the best so far.

df.adabag.cv <- boosting.cv(Credit.Standing~.-Credit.Standing1, data = df1[train1,],v=10, boos = TRUE, mfinal=30,control=rpart.control(maxdepth=5))
df.adabag.cv
#(149+246)/535=73.83
#0.825 this is the best adabag so far.

df.adabag.c? <- boosting.cv(Credit.Standing~.-Credit.Standing1, data = df1[train1,],v=10, boos = TRUE, mfinal=50,control=rpart.control(maxdepth=5))
df.adabag.cv
#(151+246)/535=74.20
#0.76 this is worse.

df.adabag.cv <- boosting.cv(Credit.Standing~.-Credit.Standing1, ?ata = df1[train1,],v=10, boos = TRUE, mfinal=15,control=rpart.control(maxdepth=5))
df.adabag.cv
#(132+254)/535=72.15
#This is worst of all

# When tuning parameters is finished, check on test set; here best parameters are mfinal =20, maxdepth = 5.

df.adab?g <- boosting(Credit.Standing~.-Credit.Standing1, data = df1[train1,], boos = TRUE, mfinal=20,control=rpart.control(maxdepth=5))

yhat_df_adabag=predict(df.adabag,newdata=df1[-train1,],n.trees=20,type = "response")

hist(yhat_df_boost)
predict_class <- ife?se(yhat_df_boost<0.5,"Bad","Good")
#mean((yhat.boost-boston.test)^2)
table(predict_class,df1[-train1,"Credit.Standing1"])
#(58+118)/245=71.83


#############Question E(K-means)
library(cluster)
library(dplyr)
library(ggplot2)
install.packages("Rtsne")
libr?ry(Rtsne)

K_df<-df1
#Kdf<- select(df1,c(1,2,3,4,5,6,7,8,9,10,11,12))
View(K_df)
#' Compute Gower distance
gower_dist <- daisy(K_df, metric = "gower")

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

#' Print most similar clients
df1[which(gower_m?t == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]

#' Print most dissimilar clients
df1[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]


# Calculate silhouette width for many k using PAM
set.?eed(850)
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
sil_width
# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab =?"Silhouette Width")
lines(1:10, sil_width)

#Cluster Interpretation Via Descriptive Statistics
pam_fit <- pam(gower_dist, diss = TRUE, k=2)
pam_results <- df1 %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary?.))
pam_results$the_summary

table(df1$Credit.Standing)
#Via Visualization
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(ae?(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


tsne_data %>%
  filter(X > 15 & X < 25,
         Y > -15 & Y < -10) %>%
  left_join(df1, by = "Credit.Standing") %>%
  collect %>%
  .[["Credit.Standing"]]






