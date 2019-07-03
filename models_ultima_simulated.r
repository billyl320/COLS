library(xtable) #for table creation for latex
library(ggplot2)#for graphics
library(MASS)#for qda
library(scales)#for scientific notation
library(RColorBrewer) #for base r plot
library(class) #for base r plot
library(plyr)#for obtaining means by factor
library(e1071)#for svm
library(tree)#for tree based methods
library(nnet)#for multinomial regression
#importing custom functions to calculate classes via COLS
source('cols.r')

#defining proper scientific notation

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

#custom theme
mytheme.scat<-theme(

	plot.title = element_text(size=60, face="bold", hjust = 0.5),
	axis.text.x  = element_text(size=20, face="bold"),
	axis.text.y=element_text(size=20, face="bold"),
	axis.title.x=element_text(size=28, face='bold'),
	axis.title.y=element_text(size=28, face='bold'),
	strip.background=element_rect(fill="gray80"),
	panel.background=element_rect(fill="gray80"),
	axis.ticks= element_blank(),
	axis.text=element_text(colour="black"),
  strip.text = element_text(size=25)

	)

#getting theoretical values
n <-c(3:8)

#matrix to hold results
model_rslts<-matrix(nrow=5, ncol=2, data=0)
colnames(model_rslts)<-c("Train", "Validation")
rownames(model_rslts)<-c("SVM", "Tree", "LDA", "LR", "COLS")

#setup for validation plot

valid_results<-matrix(nrow=5, ncol=4, data=0)
colnames(valid_results)<-c("n=3", "n=4", "n=5", "n=6")
rownames(valid_results)<-c("SVM", "Tree", "LDA", "LR", "COLS")

#setup for training plot
train_results<-matrix(nrow=5, ncol=4, data=0)
colnames(train_results)<-c("n=3", "n=4", "n=5", "n=6")
rownames(train_results)<-c("SVM", "Tree", "LDA", "LR", "COLS")

m = 1000

#creating data
set.seed(829)

#creating data
cov_mat<-matrix(nrow=3, data=c(1, .90, 1,
															 .90, 1, .90,
															 1, .90, 1))
means<-c(1, 25, 1)

#first class
data1<-mvrnorm(n=m, mu=means, Sigma=cov_mat)

#second class
cov_mat<-matrix(nrow=3, data=c(1, 1.0, 1.0,
															 1.0, 1, 1.0,
															 1.0, 1.0, 1))
means<-c(0.25, 10, 4)

data2<-mvrnorm(n=m, mu=means, Sigma=cov_mat)
#data2<-data1+10

data<-rbind(data1, data2)

df<-as.data.frame(data)

#cleaning data for ggplot2 and analysis
labs<-as.factor(c(rep(1, m), rep(2, m) ) )

#counts plot
labs2<-as.factor(labs)

##################################
## training sample size = 3
##################################

n=3

#################
# modeling
#################

set.seed(5076719)


#initialize objects to hold results
qda_train<-c()
qda_valid<-c()
svm_train<-c()
svm_valid<-c()
tree_train<-c()
tree_valid<-c()
lda_train<-c()
lda_valid<-c()
lr_train<-c()
lr_valid<-c()
cols_train<-c()
cols_valid<-c()

#simuiltion size
sim=100

for (i in 1:sim) {

    train1<-sample(1:m, n)
    train2<-sample(1:m, n)

    mytrain<-rbind(data1[train1,], data2[train2,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n)) )

    myvalid<-rbind(data1[-train1,], data2[-train2,])

    labs_valid<-as.factor(c(rep(1, m-n),
                            rep(2, m-n) ) )

    #######
    #QDA
    #######

    #creating model
    #qda.fit = qda(equa, data=train)
    #qda.fit #rank deficiency - ie unable to compute

    #predicting
    #qda.pred=predict(qda.fit, train)
    #qda.class = qda.pred$class

    #results
    #table(qda.class, labs_train)
    #overall classification rate for training
    #qda_train[i]<- mean(qda.class==as.factor(as.numeric(labs_train)))

    #######
    #SVM
    #######

    equa=labs ~ V1 + V2 + V3

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)<-c("labs", "V1", "V2", "V3")

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)<-c("labs", "V1", "V2", "V3")

    #creating model
    svmfit=svm(labs ~ V1 + V2 + V3, data=as.data.frame(train),
           kernel="linear",
           #cost=2, #coef0= 1, degree=2,
           scale=FALSE)

    #plot(svmfit , train)
    #summary(svmfit)

    ypred=round(predict(svmfit ,as.data.frame(train)))
    #table(predict=ypred, truth=train$labs)
    svm_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=round(predict(svmfit ,as.data.frame(valid) ))
    #table(predict=ypred_valid, truth=valid$labs)
    svm_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))


    ######
    # Tree
    #######

    #training tree mdoel
    treefit =tree(labs_train~ V1+V2+V3, data=as.data.frame(train) )
    #summary(treefit)

    ypred_train=predict(treefit ,train, type='class')
    #table(predict=ypred_train, truth=as.factor(train$labs))
    tree_train<-mean(ypred_train==as.factor((train$labs)))

    #plot(treefit )
    #text(treefit ,pretty =0)

    ypred_valid=predict(treefit ,valid, type='class')
    #table(predict=ypred_valid, truth=valid$labs)
    tree_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))

    #######
    #LDA
    #######

    #creating model
    lda.fit = lda(equa, data=train)
    #qda.fit #rank deficiency - ie unable to compute

    #predicting
    lda.pred=predict(lda.fit, train)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_train)
    #overall classification rate for training
    lda_train[i]<- mean(lda.class==as.factor(as.numeric(labs_train)))

    ####
    #now predict on validation

    #predicting
    lda.pred=predict(lda.fit, valid)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_valid)
    #overall classification rate for training
    lda_valid[i]<-mean(lda.class==as.factor(as.numeric(labs_valid)))

    ################################
    #Multinomial Logistic Regression
    ################################

    #creating model
    lr.fit=multinom(equa, data=train)

    ypred=predict(lr.fit ,train)
    #table(predict=ypred, truth=train$labs)
    lr_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=predict(lr.fit ,valid)
    #table(predict=ypred_valid, truth=valid$labs)
    lr_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))

    ####################
    #COLS
    ####################
    fit<-list()
  	equa= V1 ~ V2+ V3
    fit[[1]]<-lm(equa,
  	         data=as.data.frame(data1[train1,]) )
  	fit[[2]]<-lm(equa,
  	         data=as.data.frame(data2[train2,]) )

  	k<-length(fit)
  	residuals<-matrix(nrow=dim(mytrain)[1], ncol=2, data=NA)

		for(j in 1:k){

		    residuals[,j]<-predict(fit[[j]],
															 newdata=as.data.frame(mytrain) )

  		}

  	cols_class<-apply(FUN=which.min, X=residuals^2, MARGIN=1)

		cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs_train)))

		#predicting classes for validation
		residuals<-matrix(nrow=dim(myvalid)[1], ncol=2, data=NA)

		for(j in 1:k){

		    residuals[,j]<-predict(fit[[j]],
															 newdata=as.data.frame(myvalid) )

		}

		cols_class<-apply(FUN=which.min, X=residuals^2, MARGIN=1)

		cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs_valid)))

}


#################
## Model Results
#################

#SVM
model_rslts[1,1]<-mean(svm_train)
model_rslts[1,2]<-mean(svm_valid)

#tree
model_rslts[2,1]<-mean(tree_train)
model_rslts[2,2]<-mean(tree_valid)

#LDA
model_rslts[3,1]<-mean(lda_train)
model_rslts[3,2]<-mean(lda_valid)

#LR
model_rslts[4,1]<-mean(lr_train)
model_rslts[4,2]<-mean(lr_valid)

#COL
model_rslts[5,1]<-mean(cols_train)
model_rslts[5,2]<-mean(cols_valid)

sd(svm_train)
sd(svm_valid)
sd(tree_train)
sd(tree_valid)
sd(lda_train)
sd(lda_valid)
sd(lr_valid)
sd(lr_train)
sd(cols_train)
sd(cols_valid)


#display results
model_rslts

xtable(model_rslts, digits=2)

valid_results[,1]<-model_rslts[,2]
train_results[,1]<-model_rslts[,1]

##################################
## training sample size = 4
##################################

n=4

#################
# modeling
#################

set.seed(9041520)


#initialize objects to hold results
qda_train<-c()
qda_valid<-c()
svm_train<-c()
svm_valid<-c()
tree_train<-c()
tree_valid<-c()
lda_train<-c()
lda_valid<-c()
lr_train<-c()
lr_valid<-c()
cols_train<-c()
cols_valid<-c()

#simuiltion size
sim=100

for (i in 1:sim) {

    train1<-sample(1:m, n)
    train2<-sample(1:m, n)

    mytrain<-rbind(data1[train1,], data2[train2,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n)) )

    myvalid<-rbind(data1[-train1,], data2[-train2,])

    labs_valid<-as.factor(c(rep(1, m-n),
                            rep(2, m-n) ) )

    #######
    #SVM
    #######

    equa=labs ~ V1 + V2 + V3

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)<-c("labs", "V1", "V2", "V3")

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)<-c("labs", "V1", "V2", "V3")

    #creating model
    svmfit=svm(labs ~ V1 + V2 + V3, data=as.data.frame(train),
           kernel="linear",
           #cost=2, #coef0= 1, degree=2,
           scale=FALSE)

    #plot(svmfit , train)
    #summary(svmfit)

    ypred=round(predict(svmfit ,as.data.frame(train)))
    #table(predict=ypred, truth=train$labs)
    svm_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=round(predict(svmfit ,as.data.frame(valid) ))
    #table(predict=ypred_valid, truth=valid$labs)
    svm_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))


    ######
    # Tree
    #######

    #training tree mdoel
    treefit =tree(labs_train~ V1+V2+V3, data=as.data.frame(train) )
    #summary(treefit)

    ypred_train=predict(treefit ,train, type='class')
    #table(predict=ypred_train, truth=as.factor(train$labs))
    tree_train<-mean(ypred_train==as.factor((train$labs)))

    #plot(treefit )
    #text(treefit ,pretty =0)

    ypred_valid=predict(treefit ,valid, type='class')
    #table(predict=ypred_valid, truth=valid$labs)
    tree_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))

    #######
    #LDA
    #######

    #creating model
    lda.fit = lda(equa, data=train)
    #qda.fit #rank deficiency - ie unable to compute

    #predicting
    lda.pred=predict(lda.fit, train)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_train)
    #overall classification rate for training
    lda_train[i]<- mean(lda.class==as.factor(as.numeric(labs_train)))

    ####
    #now predict on validation

    #predicting
    lda.pred=predict(lda.fit, valid)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_valid)
    #overall classification rate for training
    lda_valid[i]<-mean(lda.class==as.factor(as.numeric(labs_valid)))

    ################################
    #Multinomial Logistic Regression
    ################################

    #creating model
    lr.fit=multinom(equa, data=train)

    ypred=predict(lr.fit ,train)
    #table(predict=ypred, truth=train$labs)
    lr_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=predict(lr.fit ,valid)
    #table(predict=ypred_valid, truth=valid$labs)
    lr_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))

    ####################
    #COLS
    ####################
    fit<-list()
  	equa= V1 ~ V2+ V3
    fit[[1]]<-lm(equa,
  	         data=as.data.frame(data1[train1,]) )
  	fit[[2]]<-lm(equa,
  	         data=as.data.frame(data2[train2,]) )

  	k<-length(fit)
  	residuals<-matrix(nrow=dim(mytrain)[1], ncol=2, data=NA)

		for(j in 1:k){

		    residuals[,j]<-predict(fit[[j]],
															 newdata=as.data.frame(mytrain) )

  		}

  	cols_class<-apply(FUN=which.min, X=residuals^2, MARGIN=1)

		cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs_train)))

		#predicting classes for validation
		residuals<-matrix(nrow=dim(myvalid)[1], ncol=2, data=NA)

		for(j in 1:k){

		    residuals[,j]<-predict(fit[[j]],
															 newdata=as.data.frame(myvalid) )

		}

		cols_class<-apply(FUN=which.min, X=residuals^2, MARGIN=1)

		cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs_valid)))

}


#################
## Model Results
#################

#SVM
model_rslts[1,1]<-mean(svm_train)
model_rslts[1,2]<-mean(svm_valid)

#tree
model_rslts[2,1]<-mean(tree_train)
model_rslts[2,2]<-mean(tree_valid)

#LDA
model_rslts[3,1]<-mean(lda_train)
model_rslts[3,2]<-mean(lda_valid)

#LR
model_rslts[4,1]<-mean(lr_train)
model_rslts[4,2]<-mean(lr_valid)

#COL
model_rslts[5,1]<-mean(cols_train)
model_rslts[5,2]<-mean(cols_valid)

sd(svm_train)
sd(svm_valid)
sd(tree_train)
sd(tree_valid)
sd(lda_train)
sd(lda_valid)
sd(lr_valid)
sd(lr_train)
sd(cols_train)
sd(cols_valid)


#display results
model_rslts

xtable(model_rslts, digits=2)

valid_results[,2]<-model_rslts[,2]
train_results[,2]<-model_rslts[,1]


##################################
## training sample size = 5
##################################

n=5

#################
# modeling
#################

set.seed(3878471)


#initialize objects to hold results
qda_train<-c()
qda_valid<-c()
svm_train<-c()
svm_valid<-c()
tree_train<-c()
tree_valid<-c()
lda_train<-c()
lda_valid<-c()
lr_train<-c()
lr_valid<-c()
cols_train<-c()
cols_valid<-c()

#simuiltion size
sim=100

for (i in 1:sim) {

    train1<-sample(1:m, n)
    train2<-sample(1:m, n)

    mytrain<-rbind(data1[train1,], data2[train2,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n)) )

    myvalid<-rbind(data1[-train1,], data2[-train2,])

    labs_valid<-as.factor(c(rep(1, m-n),
                            rep(2, m-n) ) )

    #######
    #SVM
    #######

    equa=labs ~ V1 + V2 + V3

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)<-c("labs", "V1", "V2", "V3")

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)<-c("labs", "V1", "V2", "V3")

    #creating model
    svmfit=svm(labs ~ V1 + V2 + V3, data=as.data.frame(train),
           kernel="linear",
           #cost=2, #coef0= 1, degree=2,
           scale=FALSE)

    #plot(svmfit , train)
    #summary(svmfit)

    ypred=round(predict(svmfit ,as.data.frame(train)))
    #table(predict=ypred, truth=train$labs)
    svm_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=round(predict(svmfit ,as.data.frame(valid) ))
    #table(predict=ypred_valid, truth=valid$labs)
    svm_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))


    ######
    # Tree
    #######

    #training tree mdoel
    treefit =tree(labs_train~ V1+V2+V3, data=as.data.frame(train) )
    #summary(treefit)

    ypred_train=predict(treefit ,train, type='class')
    #table(predict=ypred_train, truth=as.factor(train$labs))
    tree_train<-mean(ypred_train==as.factor((train$labs)))

    #plot(treefit )
    #text(treefit ,pretty =0)

    ypred_valid=predict(treefit ,valid, type='class')
    #table(predict=ypred_valid, truth=valid$labs)
    tree_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))

    #######
    #LDA
    #######

    #creating model
    lda.fit = lda(equa, data=train)
    #qda.fit #rank deficiency - ie unable to compute

    #predicting
    lda.pred=predict(lda.fit, train)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_train)
    #overall classification rate for training
    lda_train[i]<- mean(lda.class==as.factor(as.numeric(labs_train)))

    ####
    #now predict on validation

    #predicting
    lda.pred=predict(lda.fit, valid)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_valid)
    #overall classification rate for training
    lda_valid[i]<-mean(lda.class==as.factor(as.numeric(labs_valid)))

    ################################
    #Multinomial Logistic Regression
    ################################

    #creating model
    lr.fit=multinom(equa, data=train)

    ypred=predict(lr.fit ,train)
    #table(predict=ypred, truth=train$labs)
    lr_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=predict(lr.fit ,valid)
    #table(predict=ypred_valid, truth=valid$labs)
    lr_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))

    ####################
    #COLS
    ####################
    fit<-list()
  	equa= V1 ~ V2+ V3
    fit[[1]]<-lm(equa,
  	         data=as.data.frame(data1[train1,]) )
  	fit[[2]]<-lm(equa,
  	         data=as.data.frame(data2[train2,]) )

  	k<-length(fit)
  	residuals<-matrix(nrow=dim(mytrain)[1], ncol=2, data=NA)

		for(j in 1:k){

		    residuals[,j]<-predict(fit[[j]],
															 newdata=as.data.frame(mytrain) )

  		}

  	cols_class<-apply(FUN=which.min, X=residuals^2, MARGIN=1)

		cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs_train)))

		#predicting classes for validation
		residuals<-matrix(nrow=dim(myvalid)[1], ncol=2, data=NA)

		for(j in 1:k){

		    residuals[,j]<-predict(fit[[j]],
															 newdata=as.data.frame(myvalid) )

		}

		cols_class<-apply(FUN=which.min, X=residuals^2, MARGIN=1)

		cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs_valid)))

}


#################
## Model Results
#################

#SVM
model_rslts[1,1]<-mean(svm_train)
model_rslts[1,2]<-mean(svm_valid)

#tree
model_rslts[2,1]<-mean(tree_train)
model_rslts[2,2]<-mean(tree_valid)

#LDA
model_rslts[3,1]<-mean(lda_train)
model_rslts[3,2]<-mean(lda_valid)

#LR
model_rslts[4,1]<-mean(lr_train)
model_rslts[4,2]<-mean(lr_valid)

#COL
model_rslts[5,1]<-mean(cols_train)
model_rslts[5,2]<-mean(cols_valid)

sd(svm_train)
sd(svm_valid)
sd(tree_train)
sd(tree_valid)
sd(lda_train)
sd(lda_valid)
sd(lr_valid)
sd(lr_train)
sd(cols_train)
sd(cols_valid)


#display results
model_rslts

xtable(model_rslts, digits=2)

valid_results[,3]<-model_rslts[,2]
train_results[,3]<-model_rslts[,1]


##################################
## training sample size = 6
##################################

n=6

#################
# modeling
#################

set.seed(5396479)


#initialize objects to hold results
qda_train<-c()
qda_valid<-c()
svm_train<-c()
svm_valid<-c()
tree_train<-c()
tree_valid<-c()
lda_train<-c()
lda_valid<-c()
lr_train<-c()
lr_valid<-c()
cols_train<-c()
cols_valid<-c()

#simuiltion size
sim=100

for (i in 1:sim) {

    train1<-sample(1:m, n)
    train2<-sample(1:m, n)

    mytrain<-rbind(data1[train1,], data2[train2,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n)) )

    myvalid<-rbind(data1[-train1,], data2[-train2,])

    labs_valid<-as.factor(c(rep(1, m-n),
                            rep(2, m-n) ) )

    #######
    #SVM
    #######

    equa=labs ~ V1 + V2 + V3

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)<-c("labs", "V1", "V2", "V3")

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)<-c("labs", "V1", "V2", "V3")

    #creating model
    svmfit=svm(labs ~ V1 + V2 + V3, data=as.data.frame(train),
           kernel="linear",
           #cost=2, #coef0= 1, degree=2,
           scale=FALSE)

    #plot(svmfit , train)
    #summary(svmfit)

    ypred=round(predict(svmfit ,as.data.frame(train)))
    #table(predict=ypred, truth=train$labs)
    svm_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=round(predict(svmfit ,as.data.frame(valid) ))
    #table(predict=ypred_valid, truth=valid$labs)
    svm_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))


    ######
    # Tree
    #######

    #training tree mdoel
    treefit =tree(labs_train~ V1+V2+V3, data=as.data.frame(train) )
    #summary(treefit)

    ypred_train=predict(treefit ,train, type='class')
    #table(predict=ypred_train, truth=as.factor(train$labs))
    tree_train<-mean(ypred_train==as.factor((train$labs)))

    #plot(treefit )
    #text(treefit ,pretty =0)

    ypred_valid=predict(treefit ,valid, type='class')
    #table(predict=ypred_valid, truth=valid$labs)
    tree_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))

    #######
    #LDA
    #######

    #creating model
    lda.fit = lda(equa, data=train)
    #qda.fit #rank deficiency - ie unable to compute

    #predicting
    lda.pred=predict(lda.fit, train)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_train)
    #overall classification rate for training
    lda_train[i]<- mean(lda.class==as.factor(as.numeric(labs_train)))

    ####
    #now predict on validation

    #predicting
    lda.pred=predict(lda.fit, valid)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_valid)
    #overall classification rate for training
    lda_valid[i]<-mean(lda.class==as.factor(as.numeric(labs_valid)))

    ################################
    #Multinomial Logistic Regression
    ################################

    #creating model
    lr.fit=multinom(equa, data=train)

    ypred=predict(lr.fit ,train)
    #table(predict=ypred, truth=train$labs)
    lr_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=predict(lr.fit ,valid)
    #table(predict=ypred_valid, truth=valid$labs)
    lr_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))

    ####################
    #COLS
    ####################
    fit<-list()
  	equa= V1 ~ V2+ V3
    fit[[1]]<-lm(equa,
  	         data=as.data.frame(data1[train1,]) )
  	fit[[2]]<-lm(equa,
  	         data=as.data.frame(data2[train2,]) )

  	k<-length(fit)
  	residuals<-matrix(nrow=dim(mytrain)[1], ncol=2, data=NA)

		for(j in 1:k){

		    residuals[,j]<-predict(fit[[j]],
															 newdata=as.data.frame(mytrain) )

  		}

  	cols_class<-apply(FUN=which.min, X=residuals^2, MARGIN=1)

		cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs_train)))

		#predicting classes for validation
		residuals<-matrix(nrow=dim(myvalid)[1], ncol=2, data=NA)

		for(j in 1:k){

		    residuals[,j]<-predict(fit[[j]],
															 newdata=as.data.frame(myvalid) )

		}

		cols_class<-apply(FUN=which.min, X=residuals^2, MARGIN=1)

		cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs_valid)))

}


#################
## Model Results
#################

#SVM
model_rslts[1,1]<-mean(svm_train)
model_rslts[1,2]<-mean(svm_valid)

#tree
model_rslts[2,1]<-mean(tree_train)
model_rslts[2,2]<-mean(tree_valid)

#LDA
model_rslts[3,1]<-mean(lda_train)
model_rslts[3,2]<-mean(lda_valid)

#LR
model_rslts[4,1]<-mean(lr_train)
model_rslts[4,2]<-mean(lr_valid)

#COL
model_rslts[5,1]<-mean(cols_train)
model_rslts[5,2]<-mean(cols_valid)

sd(svm_train)
sd(svm_valid)
sd(tree_train)
sd(tree_valid)
sd(lda_train)
sd(lda_valid)
sd(lr_valid)
sd(lr_train)
sd(cols_train)
sd(cols_valid)


#display results
model_rslts

xtable(model_rslts, digits=2)

valid_results[,4]<-model_rslts[,2]
train_results[,4]<-model_rslts[,1]

train_results

valid_results

xtable(valid_results)

xtable(train_results)

ultima<-as.data.frame(rbind(train_results, valid_results))

fcts<-as.factor(c(rep(1, 5), rep(2, 5)))

ultima<-cbind(ultima, fcts)

ultima

xtable(ultima)


#final results plot

models<-( rep(c("SVM", "Tree", "LDA", "LR", "COLS"), 10 ) )
set<-( rep(c(rep("Training", 5), rep("Validation", 5)), 4) )
acc<-c(ultima[,1], ultima[,2], ultima[,3], ultima[,4])
samp<-c( rep(3.0, 10), rep(4.0, 10), rep(5.0, 10), rep(6.0, 10))
mydata<-as.data.frame(cbind(models, (acc), set, as.numeric(samp) ) )

colnames(mydata)[2]<-"Acc"
colnames(mydata)[4]<-"Samp"

colors <- c("SVM" = "Green", "Tree" = "khaki2",
            "LDA" = "Cyan", "LR" = "Purple", "COLS" = "Navy")

ultima_plot<-ggplot(data=mydata,
            aes(x = as.numeric(as.character(mydata$Samp)),
                y = as.numeric(as.character(mydata$Acc)),
                colour = as.factor(mydata$models),
                shape= as.factor(mydata$set),
                linetype= as.factor(mydata$set),
                group=interaction(as.factor(mydata$models), as.factor(mydata$set))
                ) )+
          geom_point(size=4, alpha=0.5)+
          geom_line(size=2, alpha=0.5 )+
          #geom_ribbon(aes(ymin=temp$lower, ymax=temp$upper), linetype=2, alpha=0.1)+
	 	  ggtitle("Overall Results for\nSimulated\nMultivariate Normal")+
		  xlab("Training Size")+
		  ylab("Overall Accuracy")+
		  labs(colour= "Model", shape="Data Set", linetype="Data Set")+
	      #scale_y_discrete(limits=c(0, 1.00))+
          #scale_x_discrete(breaks=c(3, 4, 5, 7, 10, 20))+
          mytheme.scat+
          scale_colour_manual(values = colors,
                              breaks=c("SVM", "Tree","LDA", "LR", "COLS"))+
          #scale_color_discrete(breaks=c("Training", "Validation"))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ultima_plot

ggsave(filename="plots/OverallAcc_norm.png", plot=ultima_plot,
       width=9, height=7)


#
