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
source('poly_prop.r')
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
props<-c()
for (i in 1:length(n)) {

   props[i]<-poly_prop(n[i])

}

#beta1 values for each class
betas<- (1/props) - 1
beta0<-rep(0, length(betas))

#matrix to hold results
model_rslts<-matrix(nrow=7, ncol=2, data=0)
colnames(model_rslts)<-c("Train", "Validation")
rownames(model_rslts)<-c("CNN", "QDA", "SVM", "Tree", "LDA", "LR", "COLS")

model_rslts[1,]<-c(0.95, 0.27)

#importing data for encircled image histograms
tris <- read.table("tris.txt", sep=",", header=TRUE)
squs <- read.table("squs.txt", sep=",", header=TRUE)
pens <- read.table("pens.txt", sep=",", header=TRUE)
hexs <- read.table("hexs.txt", sep=",", header=TRUE)
hepts <- read.table("hepts.txt", sep=",", header=TRUE)
octs <- read.table("octs.txt", sep=",", header=TRUE)


#cleaning data for ggplot2 and analysis
labs<-as.factor(c(rep(1, dim(tris)[1]), rep(2, dim(squs)[1]),
                  rep(3, dim(pens)[1]), rep(4, dim(hexs)[1]),
                  rep(5, dim(hepts)[1]), rep(6, dim(octs)[1]) ) )

mydata<-rbind(tris, squs, pens, hexs, hepts, octs)

#counts plot
temp<-as.data.frame(cbind(labs, mydata))
labs2<-as.factor(c(rep("s=3", dim(tris)[1]), rep("s=4", dim(squs)[1]), rep("s=5", dim(pens)[1]),
                rep("s=6", dim(hexs)[1]), rep("s=7", dim(hepts)[1]),   rep("s=8", dim(octs)[1]) ))


scat<-ggplot(data=temp, aes(x = white, y = black, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("EI for\nCreated Polygons")+
		      xlab("White Counts")+
					ylab("Black Counts")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c("s=3","s=4","s=5", "s=6",
                                        "s=7", "s=8"))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/Encircled_Image_Histograms.png", plot=scat,
       width=9, height=7)


#setup for result plot

valid_results<-matrix(nrow=7, ncol=6, data=0)
colnames(valid_results)<-c("n=3", "n=4", "n=5", "n=6", "n=7", "n=8")
rownames(valid_results)<-c("CNN", "QDA", "SVM", "Tree", "LDA", "LR", "COLS")

#setup for training plot
train_results<-matrix(nrow=7, ncol=6, data=0)
colnames(train_results)<-c("n=3", "n=4", "n=5", "n=6", "n=7", "n=8")
rownames(train_results)<-c("CNN", "QDA", "SVM", "Tree", "LDA", "LR", "COLS")

##################################
## training sample size = 3
##################################

n=3

#cnn results for n=3
model_rslts[1,]<-c(0.95, 0.27)


#################
# modeling
#################

#finding those observations to train and validate on

set.seed(695304)

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

    train3<-sample(1:125, n)
    train4<-sample(1:125, n)
    train5<-sample(1:125, n)
    train6<-sample(1:125, n)
    train7<-sample(1:125, n)
    train8<-sample(1:125, n)

    mytrain<-rbind(tris[train3,], squs[train4,], pens[train5,],
                   hexs[train6,], hepts[train7,], octs[train8,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                      rep(3, n), rep(4, n),
                      rep(5, n), rep(6, n) ) )


    myvalid<-rbind(tris[-train3,], squs[-train4,], pens[-train5,],
                   hexs[-train6,], hepts[-train7,], octs[-train8,])

    labs_valid<-as.factor(c(rep(1, 125-n), rep(2, 125-n),
                      rep(3, 125-n), rep(4, 125-n),
                      rep(5, 125-n), rep(6, 125-n) ) )
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

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=betas)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=betas)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs)))


}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-0.95
model_rslts[1,2]<-0.27

#QDA
model_rslts[2,1]<-0.94
model_rslts[2,2]<-0.70

#SVM
model_rslts[3,1]<-0.96
model_rslts[3,2]<-0.66

#tree
model_rslts[4,1]<-0.33
model_rslts[4,2]<-0.19

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

#cnn results for n=1
model_rslts[1,]<-c(0.95, 0.27)


#################
# modeling
#################

#finding those observations to train and validate on

set.seed(555665)

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

    train3<-sample(1:125, n)
    train4<-sample(1:125, n)
    train5<-sample(1:125, n)
    train6<-sample(1:125, n)
    train7<-sample(1:125, n)
    train8<-sample(1:125, n)

    mytrain<-rbind(tris[train3,], squs[train4,], pens[train5,],
                   hexs[train6,], hepts[train7,], octs[train8,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                      rep(3, n), rep(4, n),
                      rep(5, n), rep(6, n) ) )


    myvalid<-rbind(tris[-train3,], squs[-train4,], pens[-train5,],
                   hexs[-train6,], hepts[-train7,], octs[-train8,])

    labs_valid<-as.factor(c(rep(1, 125-n), rep(2, 125-n),
                      rep(3, 125-n), rep(4, 125-n),
                      rep(5, 125-n), rep(6, 125-n) ) )
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

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=betas)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=betas)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs)))


}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-0.91
model_rslts[1,2]<-0.30

#QDA
model_rslts[2,1]<-0.92
model_rslts[2,2]<-0.77

#SVM
model_rslts[3,1]<-0.96
model_rslts[3,2]<-0.73

#tree
model_rslts[4,1]<-0.42
model_rslts[4,2]<-0.20

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

#cnn results for n=1
model_rslts[1,]<-c(0.95, 0.27)


#################
# modeling
#################

#finding those observations to train and validate on

set.seed(723019)

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

    train3<-sample(1:125, n)
    train4<-sample(1:125, n)
    train5<-sample(1:125, n)
    train6<-sample(1:125, n)
    train7<-sample(1:125, n)
    train8<-sample(1:125, n)

    mytrain<-rbind(tris[train3,], squs[train4,], pens[train5,],
                   hexs[train6,], hepts[train7,], octs[train8,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                      rep(3, n), rep(4, n),
                      rep(5, n), rep(6, n) ) )


    myvalid<-rbind(tris[-train3,], squs[-train4,], pens[-train5,],
                   hexs[-train6,], hepts[-train7,], octs[-train8,])

    labs_valid<-as.factor(c(rep(1, 125-n), rep(2, 125-n),
                      rep(3, 125-n), rep(4, 125-n),
                      rep(5, 125-n), rep(6, 125-n) ) )
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

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=betas)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=betas)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs)))


}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-0.86
model_rslts[1,2]<-0.32

#QDA
model_rslts[2,1]<-0.90
model_rslts[2,2]<-0.81

#SVM
model_rslts[3,1]<-0.94
model_rslts[3,2]<-0.76

#tree
model_rslts[4,1]<-0.50
model_rslts[4,2]<-0.22

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
## training sample size = 6
##################################

n=6

#cnn results for n=20
model_rslts[1,]<-c(0.95, 0.27)


#################
# modeling
#################

#finding those observations to train and validate on

set.seed(442644)

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

    train3<-sample(1:125, n)
    train4<-sample(1:125, n)
    train5<-sample(1:125, n)
    train6<-sample(1:125, n)
    train7<-sample(1:125, n)
    train8<-sample(1:125, n)

    mytrain<-rbind(tris[train3,], squs[train4,], pens[train5,],
                   hexs[train6,], hepts[train7,], octs[train8,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                      rep(3, n), rep(4, n),
                      rep(5, n), rep(6, n) ) )


    myvalid<-rbind(tris[-train3,], squs[-train4,], pens[-train5,],
                   hexs[-train6,], hepts[-train7,], octs[-train8,])

    labs_valid<-as.factor(c(rep(1, 125-n), rep(2, 125-n),
                      rep(3, 125-n), rep(4, 125-n),
                      rep(5, 125-n), rep(6, 125-n) ) )
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

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=betas)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=betas)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs)))


}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-0.92
model_rslts[1,2]<-0.37

#QDA
model_rslts[2,1]<-0.88
model_rslts[2,2]<-0.82

#SVM
model_rslts[3,1]<-0.93
model_rslts[3,2]<-0.77

#tree
model_rslts[4,1]<-0.53
model_rslts[4,2]<-0.23

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
## training sample size = 7
##################################

n=7

#cnn results for n=25
model_rslts[1,]<-c(0.95, 0.27)


#################
# modeling
#################

#finding those observations to train and validate on

set.seed(459237)

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

    train3<-sample(1:125, n)
    train4<-sample(1:125, n)
    train5<-sample(1:125, n)
    train6<-sample(1:125, n)
    train7<-sample(1:125, n)
    train8<-sample(1:125, n)

    mytrain<-rbind(tris[train3,], squs[train4,], pens[train5,],
                   hexs[train6,], hepts[train7,], octs[train8,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                      rep(3, n), rep(4, n),
                      rep(5, n), rep(6, n) ) )


    myvalid<-rbind(tris[-train3,], squs[-train4,], pens[-train5,],
                   hexs[-train6,], hepts[-train7,], octs[-train8,])

    labs_valid<-as.factor(c(rep(1, 125-n), rep(2, 125-n),
                      rep(3, 125-n), rep(4, 125-n),
                      rep(5, 125-n), rep(6, 125-n) ) )
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

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=betas)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=betas)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs)))


}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-0.94
model_rslts[1,2]<-0.42

#QDA
model_rslts[2,1]<-0.88
model_rslts[2,2]<-0.82

#SVM
model_rslts[3,1]<-0.92
model_rslts[3,2]<-0.77

#tree
model_rslts[4,1]<-0.52
model_rslts[4,2]<-0.24

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
## training sample size = 8
##################################

n=8

#cnn results for n=1
model_rslts[1,]<-c(0.95, 0.27)


#################
# modeling
#################

#finding those observations to train and validate on

set.seed(326668)

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

    train3<-sample(1:125, n)
    train4<-sample(1:125, n)
    train5<-sample(1:125, n)
    train6<-sample(1:125, n)
    train7<-sample(1:125, n)
    train8<-sample(1:125, n)

    mytrain<-rbind(tris[train3,], squs[train4,], pens[train5,],
                   hexs[train6,], hepts[train7,], octs[train8,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n),
                      rep(3, n), rep(4, n),
                      rep(5, n), rep(6, n) ) )


    myvalid<-rbind(tris[-train3,], squs[-train4,], pens[-train5,],
                   hexs[-train6,], hepts[-train7,], octs[-train8,])

    labs_valid<-as.factor(c(rep(1, 125-n), rep(2, 125-n),
                      rep(3, 125-n), rep(4, 125-n),
                      rep(5, 125-n), rep(6, 125-n) ) )
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

    #predicting classes for training
    cols_class<-pred.cols.class(y=train$black, x=train$white, beta0=beta0, beta1=betas)

    cols_train[i]<-mean(cols_class==as.factor(as.numeric(labs)))

    #predicting classes for validation
    cols_class<-pred.cols.class(y=valid$black, x=valid$white, beta0=beta0, beta1=betas)

    cols_valid[i]<-mean(cols_class==as.factor(as.numeric(labs)))


}

#################
## Model Results
#################

#CNN
model_rslts[1,1]<-0.92
model_rslts[1,2]<-0.46

#QDA
model_rslts[2,1]<-0.87
model_rslts[2,2]<-0.83

#SVM
model_rslts[3,1]<-0.92
model_rslts[3,2]<-0.79

#tree
model_rslts[4,1]<-0.54
model_rslts[4,2]<-0.24

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
         rep(6.0, 14), rep(7.0, 14), rep(8.0, 14))
mydata<-as.data.frame(cbind(models, acc, set, as.numeric(samp) ) )

colnames(mydata)[2]<-"Acc"
colnames(mydata)[4]<-"Samp"

models<-( rep(c("CNN", "QDA", "SVM", "Tree", "LDA", "LR", "COLS"), 6 ) )
acc<-c( 0, 0, 0, 0, 0, 0, 0.7853333,
       ultima[8:13,1], 0.7853333, ultima[8:13,2], 0.7853333, ultima[8:13,3], 0.7853333,
       ultima[8:13,4], 0.7853333, ultima[8:13,5], 0.7853333, ultima[8:13,6], 0.7853333)
samp<-c( rep(0.0, 7),
         rep(3.0, 7), rep(4.0, 7), rep(5.0, 7),
         rep(6.0, 7), rep(7.0, 7), rep(8.0, 7))
myvalid<-as.data.frame(cbind(models, acc, as.numeric(samp) ) )

colnames(myvalid)[2]<-"Acc"
colnames(myvalid)[3]<-"Samp"

colors <- c("CNN" = "red", "QDA" = "blue", "SVM" = "Green", "Tree" = "khaki2",
            "LDA" = "Cyan", "LR" = "Purple", "COLS" = "Navy")

ultima_plot<-ggplot(data=myvalid,
            aes(x = as.numeric(as.character(myvalid$Samp)),
                y = as.numeric(as.character(myvalid$Acc)),
                colour = as.factor(myvalid$models)#,
                #shape= as.factor(mydata$set),
                #linetype= as.factor(mydata$set),
                #group=interaction(as.factor(mydata$models), as.factor(mydata$set))
                ) )+
          geom_point(size=4)+
          geom_line(size=2, linetype=2 )+
	 	  ggtitle("Overall Results for\nCreated Polygons")+
		  xlab("Training Size")+
		  ylab("Overall Accuracy")+
		  labs(colour= "Model")+#, shape="Data Set", linetype="Data Set")+
          mytheme.scat+
          scale_colour_manual(values = colors,
                              breaks=c("CNN", "QDA", "SVM", "Tree","LDA", "LR", "COLS") )+
          #scale_color_discrete(breaks=c("Training", "Validation"))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ultima_plot

ggsave(filename="plots/OverallAcc_poly.png", plot=ultima_plot,
       width=9, height=7)
#
