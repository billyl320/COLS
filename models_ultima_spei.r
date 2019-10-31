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
model_rslts<-matrix(nrow=6, ncol=2, data=0)
colnames(model_rslts)<-c("Train", "Validation")
rownames(model_rslts)<-c("CNN", "SVM", "Tree", "LDA", "LR", "COLS")

model_rslts[1,]<-c(1.00, 0.80)

#importing data for encircled image histograms
circ1 <-read.table("circ1_temp.txt", sep=",", header=TRUE)
circ2 <-read.table('circ2_temp.txt', sep=",", header=TRUE)
circ3 <-read.table("circ3_temp.txt", sep=",", header=TRUE)
circ<-rbind(circ1, circ2, circ3)
caps <-read.table("caps_temp_new.txt", sep=",", header=TRUE)


#cleaning data for ggplot2 and analysis
labs<-as.factor(c(rep(1, dim(circ)[1]), rep(2, dim(caps)[1]) ) )

mydata<-rbind(circ, caps)

#counts plot
temp<-as.data.frame(cbind(labs, mydata))
labs2<-as.factor(c(rep("Circle", dim(circ)[1]), rep("Capsule", dim(caps)[1]) ))

scat<-ggplot(data=temp, aes(x = white, y = black, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("EI for Circles\nand Capsules")+
		      xlab("White Counts")+
					ylab("Black Counts")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c("Circle","Capsule"))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/Encircled_Image_Histograms_NIH_CIRC_CAP_better.png", plot=scat,
       width=9, height=7)


#setup for validation plot

valid_results<-matrix(nrow=6, ncol=4, data=0)
colnames(valid_results)<-c("n=3", "n=4", "n=5", "n=6")
rownames(valid_results)<-c("CNN", "SVM", "Tree", "LDA", "LR", "COLS")

#setup for training plot
train_results<-matrix(nrow=6, ncol=4, data=0)
colnames(train_results)<-c("n=3", "n=4", "n=5", "n=6")
rownames(train_results)<-c("CNN", "SVM", "Tree", "LDA", "LR", "COLS")


##################################
## training sample size = 3
##################################

n=3

#cnn results for n=3
model_rslts[1,]<-c(1.00, 0.94)


#################
# modeling
#################

#finding those observations to train and validate on

set.seed(83150)

#initialize objects to hold results
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

    train1<-sample(1:dim(circ)[1], n)
    train2<-sample(1:dim(caps)[1],  n)

    mytrain<-rbind(circ[train1,], caps[train2,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n) ) )


    myvalid<-rbind(circ[-train1,], caps[-train2,] )

    labs_valid<-as.factor(c(rep(1, dim(circ)[1]-n), rep(2, dim(caps)[1]-n) ) )

    #######
    #SVM
    #######

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)[1]<-"labs"

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)[1]<-"labs"

    #creating model
    svmfit=svm(labs ~ white + black, data=train, kernel="polynomial",
           cost=2, coef0= 1, degree=2,
           scale=FALSE)

    #plot(svmfit , train)
    #summary(svmfit)

    ypred=predict(svmfit ,train)
    table(predict=ypred, truth=train$labs)
    svm_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=predict(svmfit ,valid)
    #table(predict=ypred_valid, truth=valid$labs)
    svm_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))


    ######
    # Tree
    #######

    #training tree mdoel
    treefit =tree(labs ~ white + black, data=train )
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

    beta0<-c(0, 0)
    #circ
    sps<-pi/4
    beta_circ<-(1/sps)-1
    #caps
    sps<-caps[train2,1]/(caps[train2,1]+caps[train2,2])
    beta_caps<-(1/mean(sps))-1

    #combine into a single vector
    beta1s<-c(beta_circ, beta_caps)

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


#SVM
model_rslts[2,1]<-mean(svm_train)
model_rslts[2,2]<-mean(svm_valid)

#tree
model_rslts[3,1]<-mean(tree_train)
model_rslts[3,2]<-mean(tree_valid)

#LDA
model_rslts[4,1]<-mean(lda_train)
model_rslts[4,2]<-mean(lda_valid)

#LR
model_rslts[5,1]<-mean(lr_train)
model_rslts[5,2]<-mean(lr_valid)

#COLS
model_rslts[6,1]<-mean(cols_train)
model_rslts[6,2]<-mean(cols_valid)

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

#cnn results for n=4
model_rslts[1,]<-c(1.00, 0.96)


#################
# modeling
#################

#finding those observations to train and validate on

set.seed(6204064)
#initialize objects to hold results
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

    train1<-sample(1:dim(circ)[1], n)
    train2<-sample(1:dim(caps)[1],  n)

    mytrain<-rbind(circ[train1,], caps[train2,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n) ) )


    myvalid<-rbind(circ[-train1,], caps[-train2,] )

    labs_valid<-as.factor(c(rep(1, dim(circ)[1]-n), rep(2, dim(caps)[1]-n) ) )

    #######
    #SVM
    #######

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)[1]<-"labs"

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)[1]<-"labs"

    #creating model
    svmfit=svm(labs ~ white + black, data=train, kernel="polynomial",
           cost=2, coef0= 1, degree=2,
           scale=FALSE)

    #plot(svmfit , train)
    #summary(svmfit)

    ypred=predict(svmfit ,train)
    table(predict=ypred, truth=train$labs)
    svm_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=predict(svmfit ,valid)
    #table(predict=ypred_valid, truth=valid$labs)
    svm_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))


    ######
    # Tree
    #######

    #training tree mdoel
    treefit =tree(labs ~ white + black, data=train )
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

    beta0<-c(0, 0)
    #circ
    sps<-pi/4
    beta_circ<-(1/sps)-1
    #caps
    sps<-caps[train2,1]/(caps[train2,1]+caps[train2,2])
    beta_caps<-(1/mean(sps))-1

    #combine into a single vector
    beta1s<-c(beta_circ, beta_caps)

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


#SVM
model_rslts[2,1]<-mean(svm_train)
model_rslts[2,2]<-mean(svm_valid)

#tree
model_rslts[3,1]<-mean(tree_train)
model_rslts[3,2]<-mean(tree_valid)

#LDA
model_rslts[4,1]<-mean(lda_train)
model_rslts[4,2]<-mean(lda_valid)

#LR
model_rslts[5,1]<-mean(lr_train)
model_rslts[5,2]<-mean(lr_valid)

#COLS
model_rslts[6,1]<-mean(cols_train)
model_rslts[6,2]<-mean(cols_valid)

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

#cnn results for n=5
model_rslts[1,]<-c(1.00, 0.96)


#################
# modeling
#################

#finding those observations to train and validate on

set.seed(9197216)
#initialize objects to hold results
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

    train1<-sample(1:dim(circ)[1], n)
    train2<-sample(1:dim(caps)[1],  n)

    mytrain<-rbind(circ[train1,], caps[train2,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n) ) )


    myvalid<-rbind(circ[-train1,], caps[-train2,] )

    labs_valid<-as.factor(c(rep(1, dim(circ)[1]-n), rep(2, dim(caps)[1]-n) ) )

    #######
    #SVM
    #######

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)[1]<-"labs"

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)[1]<-"labs"

    #creating model
    svmfit=svm(labs ~ white + black, data=train, kernel="polynomial",
           cost=2, coef0= 1, degree=2,
           scale=FALSE)

    #plot(svmfit , train)
    #summary(svmfit)

    ypred=predict(svmfit ,train)
    table(predict=ypred, truth=train$labs)
    svm_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=predict(svmfit ,valid)
    #table(predict=ypred_valid, truth=valid$labs)
    svm_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))


    ######
    # Tree
    #######

    #training tree mdoel
    treefit =tree(labs ~ white + black, data=train )
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

    beta0<-c(0, 0)
    #circ
    sps<-pi/4
    beta_circ<-(1/sps)-1
    #caps
    sps<-caps[train2,1]/(caps[train2,1]+caps[train2,2])
    beta_caps<-(1/mean(sps))-1

    #combine into a single vector
    beta1s<-c(beta_circ, beta_caps)

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


#SVM
model_rslts[2,1]<-mean(svm_train)
model_rslts[2,2]<-mean(svm_valid)

#tree
model_rslts[3,1]<-mean(tree_train)
model_rslts[3,2]<-mean(tree_valid)

#LDA
model_rslts[4,1]<-mean(lda_train)
model_rslts[4,2]<-mean(lda_valid)

#LR
model_rslts[5,1]<-mean(lr_train)
model_rslts[5,2]<-mean(lr_valid)

#COLS
model_rslts[6,1]<-mean(cols_train)
model_rslts[6,2]<-mean(cols_valid)

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

#cnn results for n=6
model_rslts[1,]<-c(1.00, 0.98)


#################
# modeling
#################

#finding those observations to train and validate on

set.seed(57275)
#initialize objects to hold results
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

    train1<-sample(1:dim(circ)[1], n)
    train2<-sample(1:dim(caps)[1],  n)

    mytrain<-rbind(circ[train1,], caps[train2,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n) ) )


    myvalid<-rbind(circ[-train1,], caps[-train2,] )

    labs_valid<-as.factor(c(rep(1, dim(circ)[1]-n), rep(2, dim(caps)[1]-n) ) )

    #######
    #SVM
    #######

    train<-as.data.frame(cbind(as.factor(labs_train), mytrain))
    colnames(train)[1]<-"labs"

    valid<-as.data.frame(cbind(as.factor(labs_valid), myvalid))
    colnames(valid)[1]<-"labs"

    #creating model
    svmfit=svm(labs ~ white + black, data=train, kernel="polynomial",
           cost=2, coef0= 1, degree=2,
           scale=FALSE)

    #plot(svmfit , train)
    #summary(svmfit)

    ypred=predict(svmfit ,train)
    table(predict=ypred, truth=train$labs)
    svm_train[i]<-mean(ypred==as.factor(as.numeric(labs_train)))

    #now on valid
    ypred_valid=predict(svmfit ,valid)
    #table(predict=ypred_valid, truth=valid$labs)
    svm_valid[i]<-mean(ypred_valid==as.factor(as.numeric(labs_valid)))


    ######
    # Tree
    #######

    #training tree mdoel
    treefit =tree(labs ~ white + black, data=train )
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

    beta0<-c(0, 0)
    #circ
    sps<-pi/4
    beta_circ<-(1/sps)-1
    #caps
    sps<-caps[train2,1]/(caps[train2,1]+caps[train2,2])
    beta_caps<-(1/mean(sps))-1

    #combine into a single vector
    beta1s<-c(beta_circ, beta_caps)

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


#SVM
model_rslts[2,1]<-mean(svm_train)
model_rslts[2,2]<-mean(svm_valid)

#tree
model_rslts[3,1]<-mean(tree_train)
model_rslts[3,2]<-mean(tree_valid)

#LDA
model_rslts[4,1]<-mean(lda_train)
model_rslts[4,2]<-mean(lda_valid)

#LR
model_rslts[5,1]<-mean(lr_train)
model_rslts[5,2]<-mean(lr_valid)

#COLS
model_rslts[6,1]<-mean(cols_train)
model_rslts[6,2]<-mean(cols_valid)

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

fcts<-as.factor(c(rep(1, 6), rep(2, 6)))

ultima<-cbind(ultima, fcts)

ultima

xtable(ultima)


#final results plot

models<-( rep(c("CNN", "SVM", "Tree", "LDA", "LR", "COLS"), 4*3 ) )
set<-( rep(c(rep("Training", 6), rep("Validation", 6)), 4) )
acc<-c(ultima[,1], ultima[,2], ultima[,3], ultima[,4])
samp<-c( rep(3.0, 12), rep(4.0, 12), rep(5.0, 12), rep(6.0, 12))
mydata<-as.data.frame(cbind(models, (acc), set, as.numeric(samp) ) )

colnames(mydata)[2]<-"Acc"
colnames(mydata)[4]<-"Samp"

colors <- c("CNN" = "red", "SVM" = "Green", "Tree" = "khaki2",
            "LDA" = "Cyan", "LR" = "Purple", "COLS" = "Navy")

ultima_plot<-ggplot(data=mydata,
            aes(x = as.numeric(as.character(mydata$Samp)),
                y = as.numeric(as.character(mydata$Acc)),
                colour = as.factor(mydata$models),
                shape= as.factor(mydata$set),
                linetype= as.factor(mydata$set),
                group=interaction(as.factor(mydata$models), as.factor(mydata$set))
                ) )+
          geom_point(size=4, alpha=0.4)+
          geom_line(size=2, alpha=0.4 )+
          #geom_ribbon(aes(ymin=temp$lower, ymax=temp$upper), linetype=2, alpha=0.1)+
	 	  ggtitle("Overall Results for\nNML NIH Pills")+
		  xlab("Training Size")+
		  ylab("Overall Accuracy")+
		  labs(colour= "Model", shape="Data Set", linetype="Data Set")+
	      #scale_y_discrete(limits=c(0, 1.00))+
          #scale_x_discrete(breaks=c(3, 4, 5, 7, 10, 20))+
          mytheme.scat+
          scale_colour_manual(values = colors,
                              breaks=c("CNN", "SVM", "Tree","LDA", "LR", "COLS"))+
          #scale_color_discrete(breaks=c("Training", "Validation"))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ultima_plot

ggsave(filename="plots/OverallAcc_nml_circ_cap.png", plot=ultima_plot,
       width=9, height=7)


#
