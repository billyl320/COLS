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

#matrix to hold results
model_rslts<-matrix(nrow=7, ncol=2, data=0)
colnames(model_rslts)<-c("Train", "Validation")
rownames(model_rslts)<-c("CNN", "QDA", "SVM", "Tree", "LDA", "LR", "COLS")

##importing data for traditional image histograms
edge   <- read.table("edge.txt", sep=",", header=TRUE)
spiral <- read.table("spiral.txt", sep=",", header=TRUE)
elip   <- read.table("elip.txt", sep=",", header=TRUE)

#cleaning data for ggplot2 and analysis
labs<-as.factor(c(rep(1, dim(edge)[1]), rep(2, dim(spiral)[1]), rep(3, dim(elip)[1])) )

mydata<-rbind(edge, spiral, elip)


temp<-as.data.frame(cbind(labs, mydata))
labs2<-as.factor(c(rep("Edge", dim(edge)[1]), rep("Spiral", dim(spiral)[1]), rep("Ellipse", dim(elip)[1]) ))

#setup for results plot

valid_results<-matrix(nrow=7, ncol=6, data=0)
colnames(valid_results)<-c("n=3", "n=4", "n=5", "n=7", "n=10", "n=20")
rownames(valid_results)<-c("CNN", "QDA", "SVM", "Tree", "LDA", "LR", "COLS")

#setup for training plot
train_results<-matrix(nrow=7, ncol=6, data=0)
colnames(train_results)<-c("n=3", "n=4", "n=5", "n=7", "n=10", "n=20")
rownames(train_results)<-c("CNN", "QDA", "SVM", "Tree", "LDA", "LR", "COLS")

m=3

##################################
## training sample size = 3
##################################

n=3

#cnn results for n=3
model_rslts[1,]<-c(1.00, 0.55)

#################
# modeling
#################

#finding those observations to train and validate on

set.seed(76526)

#initialize objects to hold results
lda_train<-c()
lda_valid<-c()
lr_train<-c()
lr_valid<-c()
cols_train<-c()
cols_valid<-c()

#simuiltion size
sim=100

for (i in 1:sim) {

    train1<-sample(1:75,  n)
    train2<-sample(1:223, n)
    train3<-sample(1:225, n)

    mytrain<-rbind(edge[train1,], spiral[train2,],
                   elip[train3,])
    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                            rep(3, n) ) )
    myvalid<-rbind(edge[-train1,], spiral[-train2,],
                   elip[-train3,])
    labs_valid<-as.factor(c(rep(1, 75-n), rep(2, 225-n),
                            rep(3, 223-n) ) )


    #######
    #LDA
    #######
    temp<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(temp)[1]<-"labs"


    #creating model
    lda.fit = lda(labs ~ white + black, data=temp)
    #qda.fit #rank deficiency - ie unable to compute

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_train)
    #overall classification rate for training
    lda_train[i]<- mean(lda.class==as.factor(as.numeric(labs_train)))

    ####
    #now predict on validation
    temp<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(temp)[1]<-"labs"

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_valid)
    #overall classification rate for training
    lda_valid[i]<-mean(lda.class==as.factor(as.numeric(labs_valid)))

    ################################
    #Multinomial Logistic Regression
    ################################

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)[1]<-"labs"

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)[1]<-"labs"

    #creating model
    lr.fit=multinom(labs ~ white + black, data=train)

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

    beta0<-c(0, 0, 0)
    #edge
    sps<-edge[train1,1]/(edge[train1,1]+edge[train1,2])
    beta_edge<-(1/mean(sps))-1
    #spiral
    sps<-edge[train2,1]/(spiral[train2,1]+spiral[train2,2])
    beta_spiral<-(1/mean(sps))-1
    #elip
    sps<-elip[train3,1]/(elip[train3,1]+elip[train3,2])
    beta_elip<-(1/mean(sps))-1

    #combine into a single vector
    beta1s<-c(beta_edge, beta_spiral, beta_elip)

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=beta1s)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs_train)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=beta1s)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs_valid)))

}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-1.00
model_rslts[1,2]<-0.53

#QDA
model_rslts[2,1]<-0.94
model_rslts[2,2]<-0.56

#SVM
model_rslts[3,1]<-0.92
model_rslts[3,2]<-0.61

#tree
model_rslts[4,1]<-0.44
model_rslts[4,2]<-0.33

#LDA
model_rslts[5,1]<-mean(lda_train)
model_rslts[5,2]<-mean(lda_valid)

#LR
model_rslts[6,1]<-mean(lr_train)
model_rslts[6,2]<-mean(lr_valid)

#COLS
model_rslts[7,1]<-mean(cols_train)
model_rslts[7,2]<-mean(cols_valid)

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

#cnn results for n=4
model_rslts[1,]<-c(1.00, 0.56)

#################
# modeling
#################

#finding those observations to train and validate on

set.seed(872596)

#initialize objects to hold results
lda_train<-c()
lda_valid<-c()
lr_train<-c()
lr_valid<-c()
cols_train<-c()
cols_valid<-c()

#simuiltion size
sim=100

for (i in 1:sim) {

    train1<-sample(1:75,  n)
    train2<-sample(1:223, n)
    train3<-sample(1:225, n)

    mytrain<-rbind(edge[train1,], spiral[train2,],
                   elip[train3,])
    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                            rep(3, n) ) )
    myvalid<-rbind(edge[-train1,], spiral[-train2,],
                   elip[-train3,])
    labs_valid<-as.factor(c(rep(1, 75-n), rep(2, 225-n),
                            rep(3, 223-n) ) )


    #######
    #LDA
    #######
    temp<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(temp)[1]<-"labs"


    #creating model
    lda.fit = lda(labs ~ white + black, data=temp)
    #qda.fit #rank deficiency - ie unable to compute

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_train)
    #overall classification rate for training
    lda_train[i]<- mean(lda.class==as.factor(as.numeric(labs_train)))

    ####
    #now predict on validation
    temp<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(temp)[1]<-"labs"

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_valid)
    #overall classification rate for training
    lda_valid[i]<-mean(lda.class==as.factor(as.numeric(labs_valid)))

    ################################
    #Multinomial Logistic Regression
    ################################

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)[1]<-"labs"

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)[1]<-"labs"

    #creating model
    lr.fit=multinom(labs ~ white + black, data=train)

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

    beta0<-c(0, 0, 0)
    #edge
    sps<-edge[train1,1]/(edge[train1,1]+edge[train1,2])
    beta_edge<-(1/mean(sps))-1
    #spiral
    sps<-edge[train2,1]/(spiral[train2,1]+spiral[train2,2])
    beta_spiral<-(1/mean(sps))-1
    #elip
    sps<-elip[train3,1]/(elip[train3,1]+elip[train3,2])
    beta_elip<-(1/mean(sps))-1

    #combine into a single vector
    beta1s<-c(beta_edge, beta_spiral, beta_elip)

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=beta1s)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs_train)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=beta1s)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs_valid)))

}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-1.00
model_rslts[1,2]<-0.54

#QDA
model_rslts[2,1]<-0.92
model_rslts[2,2]<-0.60

#SVM
model_rslts[3,1]<-0.88
model_rslts[3,2]<-0.62

#tree
model_rslts[4,1]<-0.67
model_rslts[4,2]<-0.48

#LDA
model_rslts[5,1]<-mean(lda_train)
model_rslts[5,2]<-mean(lda_valid)

#LR
model_rslts[6,1]<-mean(lr_train)
model_rslts[6,2]<-mean(lr_valid)

#COLS
model_rslts[7,1]<-mean(cols_train)
model_rslts[7,2]<-mean(cols_valid)

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

#cnn results for n=5
model_rslts[1,]<-c(1.00, 0.56)

#################
# modeling
#################

#finding those observations to train and validate on

set.seed(50976)

#initialize objects to hold results
lda_train<-c()
lda_valid<-c()
lr_train<-c()
lr_valid<-c()
cols_train<-c()
cols_valid<-c()

#simuiltion size
sim=100

for (i in 1:sim) {

    train1<-sample(1:75,  n)
    train2<-sample(1:223, n)
    train3<-sample(1:225, n)

    mytrain<-rbind(edge[train1,], spiral[train2,],
                   elip[train3,])
    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                            rep(3, n) ) )
    myvalid<-rbind(edge[-train1,], spiral[-train2,],
                   elip[-train3,])
    labs_valid<-as.factor(c(rep(1, 75-n), rep(2, 225-n),
                            rep(3, 223-n) ) )


    #######
    #LDA
    #######
    temp<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(temp)[1]<-"labs"


    #creating model
    lda.fit = lda(labs ~ white + black, data=temp)
    #qda.fit #rank deficiency - ie unable to compute

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_train)
    #overall classification rate for training
    lda_train[i]<- mean(lda.class==as.factor(as.numeric(labs_train)))

    ####
    #now predict on validation
    temp<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(temp)[1]<-"labs"

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_valid)
    #overall classification rate for training
    lda_valid[i]<-mean(lda.class==as.factor(as.numeric(labs_valid)))

    ################################
    #Multinomial Logistic Regression
    ################################

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)[1]<-"labs"

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)[1]<-"labs"

    #creating model
    lr.fit=multinom(labs ~ white + black, data=train)

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

    beta0<-c(0, 0, 0)
    #edge
    sps<-edge[train1,1]/(edge[train1,1]+edge[train1,2])
    beta_edge<-(1/mean(sps))-1
    #spiral
    sps<-edge[train2,1]/(spiral[train2,1]+spiral[train2,2])
    beta_spiral<-(1/mean(sps))-1
    #elip
    sps<-elip[train3,1]/(elip[train3,1]+elip[train3,2])
    beta_elip<-(1/mean(sps))-1

    #combine into a single vector
    beta1s<-c(beta_edge, beta_spiral, beta_elip)

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=beta1s)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs_train)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=beta1s)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs_valid)))

}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-0.99
model_rslts[1,2]<-0.55

#QDA
model_rslts[2,1]<-0.88
model_rslts[2,2]<-0.62

#SVM
model_rslts[3,1]<-0.83
model_rslts[3,2]<-0.62

#tree
model_rslts[4,1]<-0.87
model_rslts[4,2]<-0.53

#LDA
model_rslts[5,1]<-mean(lda_train)
model_rslts[5,2]<-mean(lda_valid)

#LR
model_rslts[6,1]<-mean(lr_train)
model_rslts[6,2]<-mean(lr_valid)

#COLS
model_rslts[7,1]<-mean(cols_train)
model_rslts[7,2]<-mean(cols_valid)

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
## training sample size = 7
##################################

n=7

#cnn results for n=7
model_rslts[1,]<-c(1.00, 0.58)

#################
# modeling
#################

#finding those observations to train and validate on

set.seed(35522)

#initialize objects to hold results
lda_train<-c()
lda_valid<-c()
lr_train<-c()
lr_valid<-c()
cols_train<-c()
cols_valid<-c()

#simuiltion size
sim=100

for (i in 1:sim) {

    train1<-sample(1:75,  n)
    train2<-sample(1:223, n)
    train3<-sample(1:225, n)

    mytrain<-rbind(edge[train1,], spiral[train2,],
                   elip[train3,])
    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                            rep(3, n) ) )
    myvalid<-rbind(edge[-train1,], spiral[-train2,],
                   elip[-train3,])
    labs_valid<-as.factor(c(rep(1, 75-n), rep(2, 225-n),
                            rep(3, 223-n) ) )


    #######
    #LDA
    #######
    temp<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(temp)[1]<-"labs"


    #creating model
    lda.fit = lda(labs ~ white + black, data=temp)
    #qda.fit #rank deficiency - ie unable to compute

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_train)
    #overall classification rate for training
    lda_train[i]<- mean(lda.class==as.factor(as.numeric(labs_train)))

    ####
    #now predict on validation
    temp<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(temp)[1]<-"labs"

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_valid)
    #overall classification rate for training
    lda_valid[i]<-mean(lda.class==as.factor(as.numeric(labs_valid)))

    ################################
    #Multinomial Logistic Regression
    ################################

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)[1]<-"labs"

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)[1]<-"labs"

    #creating model
    lr.fit=multinom(labs ~ white + black, data=train)

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

    beta0<-c(0, 0, 0)
    #edge
    sps<-edge[train1,1]/(edge[train1,1]+edge[train1,2])
    beta_edge<-(1/mean(sps))-1
    #spiral
    sps<-edge[train2,1]/(spiral[train2,1]+spiral[train2,2])
    beta_spiral<-(1/mean(sps))-1
    #elip
    sps<-elip[train3,1]/(elip[train3,1]+elip[train3,2])
    beta_elip<-(1/mean(sps))-1

    #combine into a single vector
    beta1s<-c(beta_edge, beta_spiral, beta_elip)

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=beta1s)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs_train)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=beta1s)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs_valid)))

}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-0.95
model_rslts[1,2]<-0.56

#QDA
model_rslts[2,1]<-0.85
model_rslts[2,2]<-0.65

#SVM
model_rslts[3,1]<-0.83
model_rslts[3,2]<-0.64

#tree
model_rslts[4,1]<-0.81
model_rslts[4,2]<-0.57

#LDA
model_rslts[5,1]<-mean(lda_train)
model_rslts[5,2]<-mean(lda_valid)

#LR
model_rslts[6,1]<-mean(lr_train)
model_rslts[6,2]<-mean(lr_valid)

#COLS
model_rslts[7,1]<-mean(cols_train)
model_rslts[7,2]<-mean(cols_valid)

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

##################################
## training sample size = 10
##################################

n=10

#cnn results for n=10
model_rslts[1,]<-c(1.00, 0.60)

#################
# modeling
#################

#finding those observations to train and validate on

set.seed(1275148)

#initialize objects to hold results
lda_train<-c()
lda_valid<-c()
lr_train<-c()
lr_valid<-c()
cols_train<-c()
cols_valid<-c()

#simuiltion size
sim=100

for (i in 1:sim) {

    train1<-sample(1:75,  n)
    train2<-sample(1:223, n)
    train3<-sample(1:225, n)

    mytrain<-rbind(edge[train1,], spiral[train2,],
                   elip[train3,])
    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                            rep(3, n) ) )
    myvalid<-rbind(edge[-train1,], spiral[-train2,],
                   elip[-train3,])
    labs_valid<-as.factor(c(rep(1, 75-n), rep(2, 225-n),
                            rep(3, 223-n) ) )


    #######
    #LDA
    #######
    temp<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(temp)[1]<-"labs"


    #creating model
    lda.fit = lda(labs ~ white + black, data=temp)
    #qda.fit #rank deficiency - ie unable to compute

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_train)
    #overall classification rate for training
    lda_train[i]<- mean(lda.class==as.factor(as.numeric(labs_train)))

    ####
    #now predict on validation
    temp<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(temp)[1]<-"labs"

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_valid)
    #overall classification rate for training
    lda_valid[i]<-mean(lda.class==as.factor(as.numeric(labs_valid)))

    ################################
    #Multinomial Logistic Regression
    ################################

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)[1]<-"labs"

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)[1]<-"labs"

    #creating model
    lr.fit=multinom(labs ~ white + black, data=train)

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

    beta0<-c(0, 0, 0)
    #edge
    sps<-edge[train1,1]/(edge[train1,1]+edge[train1,2])
    beta_edge<-(1/mean(sps))-1
    #spiral
    sps<-edge[train2,1]/(spiral[train2,1]+spiral[train2,2])
    beta_spiral<-(1/mean(sps))-1
    #elip
    sps<-elip[train3,1]/(elip[train3,1]+elip[train3,2])
    beta_elip<-(1/mean(sps))-1

    #combine into a single vector
    beta1s<-c(beta_edge, beta_spiral, beta_elip)

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=beta1s)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs_train)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=beta1s)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs_valid)))

}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-0.89
model_rslts[1,2]<-0.57

#QDA
model_rslts[2,1]<-0.83
model_rslts[2,2]<-0.67

#SVM
model_rslts[3,1]<-0.81
model_rslts[3,2]<-0.64

#tree
model_rslts[4,1]<-0.57
model_rslts[4,2]<-0.59

#LDA
model_rslts[5,1]<-mean(lda_train)
model_rslts[5,2]<-mean(lda_valid)

#LR
model_rslts[6,1]<-mean(lr_train)
model_rslts[6,2]<-mean(lr_valid)

#COLS
model_rslts[7,1]<-mean(cols_train)
model_rslts[7,2]<-mean(cols_valid)

sd(lda_train)
sd(lda_valid)
sd(lr_valid)
sd(lr_train)
sd(cols_train)
sd(cols_valid)


#display results
model_rslts

xtable(model_rslts, digits=2)

valid_results[,5]<-model_rslts[,2]
train_results[,5]<-model_rslts[,1]

##################################
## training sample size = 20
##################################

n=20

#cnn results for n=29
model_rslts[1,]<-c(1.00, 0.64)

#################
# modeling
#################

#finding those observations to train and validate on

set.seed(5924544)

#initialize objects to hold results
lda_train<-c()
lda_valid<-c()
lr_train<-c()
lr_valid<-c()
cols_train<-c()
cols_valid<-c()

#simuiltion size
sim=100

for (i in 1:sim) {

    train1<-sample(1:75,  n)
    train2<-sample(1:223, n)
    train3<-sample(1:225, n)

    mytrain<-rbind(edge[train1,], spiral[train2,],
                   elip[train3,])
    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                            rep(3, n) ) )
    myvalid<-rbind(edge[-train1,], spiral[-train2,],
                   elip[-train3,])
    labs_valid<-as.factor(c(rep(1, 75-n), rep(2, 225-n),
                            rep(3, 223-n) ) )


    #######
    #LDA
    #######
    temp<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(temp)[1]<-"labs"


    #creating model
    lda.fit = lda(labs ~ white + black, data=temp)
    #qda.fit #rank deficiency - ie unable to compute

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_train)
    #overall classification rate for training
    lda_train[i]<- mean(lda.class==as.factor(as.numeric(labs_train)))

    ####
    #now predict on validation
    temp<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(temp)[1]<-"labs"

    #predicting
    lda.pred=predict(lda.fit, temp)
    lda.class = lda.pred$class

    #results
    #table(qda.class, labs_valid)
    #overall classification rate for training
    lda_valid[i]<-mean(lda.class==as.factor(as.numeric(labs_valid)))

    ################################
    #Multinomial Logistic Regression
    ################################

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)[1]<-"labs"

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)[1]<-"labs"

    #creating model
    lr.fit=multinom(labs ~ white + black, data=train)

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

    beta0<-c(0, 0, 0)
    #edge
    sps<-edge[train1,1]/(edge[train1,1]+edge[train1,2])
    beta_edge<-(1/mean(sps))-1
    #spiral
    sps<-edge[train2,1]/(spiral[train2,1]+spiral[train2,2])
    beta_spiral<-(1/mean(sps))-1
    #elip
    sps<-elip[train3,1]/(elip[train3,1]+elip[train3,2])
    beta_elip<-(1/mean(sps))-1

    #combine into a single vector
    beta1s<-c(beta_edge, beta_spiral, beta_elip)

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=beta1s)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs_train)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=beta1s)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs_valid)))

}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-0.94
model_rslts[1,2]<-0.60

#QDA
model_rslts[2,1]<-0.79
model_rslts[2,2]<-0.68

#SVM
model_rslts[3,1]<-0.78
model_rslts[3,2]<-0.65

#tree
model_rslts[4,1]<-0.87
model_rslts[4,2]<-0.63

#LDA
model_rslts[5,1]<-mean(lda_train)
model_rslts[5,2]<-mean(lda_valid)

#LR
model_rslts[6,1]<-mean(lr_train)
model_rslts[6,2]<-mean(lr_valid)

#COLS
model_rslts[7,1]<-mean(cols_train)
model_rslts[7,2]<-mean(cols_valid)

sd(lda_train)
sd(lda_valid)
sd(lr_valid)
sd(lr_train)
sd(cols_train)
sd(cols_valid)


#display results
model_rslts

xtable(model_rslts, digits=2)

valid_results[,6]<-model_rslts[,2]
train_results[,6]<-model_rslts[,1]


#summary of results
train_results

valid_results

xtable(valid_results)

xtable(train_results)

ultima<-as.data.frame(rbind(train_results, valid_results))

fcts<-as.factor(c(rep(1, 7), rep(2, 7)))

ultima<-cbind(ultima, fcts)

ultima

xtable(ultima)


#final results plot

models<-( rep(c("CNN", "QDA", "SVM", "Tree", "LDA", "LR", "COLS"), 12 ) )
set<-( rep(c(rep("Training", 7), rep("Validation", 7)), 6) )
acc<-c(ultima[,1], ultima[,2], ultima[,3],
       ultima[,4], ultima[,5], ultima[,6])
samp<-c( rep(3.0, 14), rep(4.0, 14), rep(5.0, 14),
         rep(7.0, 14), rep(10.0, 14), rep(20.0, 14))
mydata<-as.data.frame(cbind(models, (acc), set, as.numeric(samp) ) )

colnames(mydata)[2]<-"Acc"
colnames(mydata)[4]<-"Samp"

colors <- c("CNN" = "red", "QDA" = "blue", "SVM" = "Green", "Tree" = "khaki2",
            "LDA" = "Cyan", "LR" = "Purple", "COLS" = "Navy")

ultima_plot<-ggplot(data=mydata,
            aes(x = as.numeric(as.character(mydata$Samp)),
                y = as.numeric(as.character(mydata$Acc)),
                colour = as.factor(mydata$models),
                shape= as.factor(mydata$set),
                linetype= as.factor(mydata$set),
                group=interaction(as.factor(mydata$models), as.factor(mydata$set))
                ) )+
          geom_point(size=4)+
          geom_line(size=2 )+
	 	  ggtitle("Overall Results for\nGalaxy Shapes")+
		  xlab("Training Size")+
		  ylab("Overall Accuracy")+
		  labs(colour= "Model", shape="Data Set", linetype="Data Set")+
          mytheme.scat+
          scale_colour_manual(values = colors,
                              breaks=c("CNN", "QDA", "SVM", "Tree","LDA", "LR", "COLS"))+
          #scale_color_discrete(breaks=c("Training", "Validation"))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ultima_plot

ggsave(filename="plots/OverallAcc_galaxy.png", plot=ultima_plot,
       width=9, height=7)


#
