setwd(file.choose())

#knn - k nearest neighbour algorithm practice date : 30-9-2014
# knn uses Euclidean distance
# if p and q are the two examples to be compared, and each one has n features 
# dist(p,q) = sqrt((p1-q1)2 + (p2-q2)2 +(p3-q3)2 + .... +  (pn-qn)2)
# bias variance tradeoff : choosing the right "k" 
#if k is large it might overfit the data, k is small it might underfit the data
# k can be set something around square root of no. of training examples

#prepping up data for knn 
#each feature should contribute equally to the euclidean distance formula
#if a feature has an extremely long range, it can dominate the results and other features would be dwarfed
#this is made sure by rescaling each feature
# min-max normalization : x' = (x-min(x))/(max(x)-min(x))
# z-score standardization : x' = x-mean(x)/sd(x)

#lazy learner: merely stores the training data => training phase is fast
#process of making predictions is relatively slow
#non-parametric based learning-- no parameters are learned abput the data
#allows the learner to find natural patterns rather than fitting data into the preconceived form

#install relevant packages
install.packages("class")
install.packages("caret")
library("class")
install.packages("gmodels")
library("gmodels")
library("caret")
#get the data
wbcd <- read.csv(file.choose() , header=T)
View(wbcd)

#get a snapshot of the data
str(wbcd)
summary(wbcd)

#normalize the data using min-max
normalize <- function(x)
{
  x <- (x-min(x))/(max(x)-min(x))
}

#feature at position 1 is serial no. and at position 2 is the outcome variable
df <- sapply(wbcd[3:32],normalize)
df <- as.data.frame(df)
wbcd[3:32] <- df
remove(df)
wbcd <- wbcd[-1]  # remove id

#outcome
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(x=wbcd$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))
round(prop.table(x=table(wbcd$diagnosis))*100,digits=2)
train_indices <- sample(c(1:569) ,469 )
wbcd_train <- wbcd[train_indices,]
wbcd_test <- wbcd[-train_indices,]

#the algo
wbcd_train_labels <- wbcd_train[,1]
pred <- knn(train=wbcd_train[2:31],test=wbcd_test[2:31],k=21,cl=wbcd_train_labels)


df <- read.csv("knn_list.csv")
confusionMatrix(df$truth,df$predicted)
factor(df$truth)
factor(df$prediction)
#alternative normalization-- z-score standardization
#z-score standardization
wbcd_z <- wbcd[,-1]
temp <- scale(x=wbcd[,-1])
temp <- as.data.frame(temp)
wbcd_z[2:31] <- temp
train_indices <- sample(c(1:569) ,469 )
wbcd_z_train <- wbcd_z[train_indices,]
wbcd_z_test <- wbcd_z[-train_indices,]
pred_z <- knn(train=wbcd_z_train[2:31],test=wbcd_z_test[2:31],k=21,cl=wbcd_z_train$diagnosis)

confusionMatrix(pred_z ,wbcd_z_test$diagnosis)
