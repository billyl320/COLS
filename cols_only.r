library(xtable) #for table creation for latex
library(ggplot2)#for graphics
library(MASS)#for qda, lda, and mvrnorm
library(scales)#for scientific notation
library(RColorBrewer) #for base r plot
library(class) #for base r plot
library(plyr)#for obtaining means by factor
library(e1071)#for svm
library(tree)#for tree based methods
library(nnet)#for multinomial regression
#importing custom functions to calculate classes via COLS
source('cols.r')

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


##################################
## training sample size = 3
##################################

n=3

#################
# modeling
#################

#finding those observations to train and validate on

set.seed(83150)

#creating data
cov_mat<-matrix(nrow=3, data=c(10, 0, 0,
															 0, 5, 0,
															 0, 0, 1))
means<-c(0, 0, 0)

#first class
data1<-mvrnorm(n=100, mu=means, Sigma=cov_mat)

#second class
cov_mat<-matrix(nrow=3, data=c(10, 0, 0,
															 0, 5, 0,
															 0, 0, 5))
means<-c(6, 10, 5)

data2<-mvrnorm(n=100, mu=means, Sigma=cov_mat)
#data2<-data1+10

data<-rbind(data1, data2)

df<-as.data.frame(data)

#cleaning data for ggplot2 and analysis
labs<-as.factor(c(rep(1, 100), rep(2, 100) ) )

#counts plot
labs2<-as.factor(labs)

plot(df, col=labs)

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

    train1<-sample(1:100, n)
    train2<-sample(1:100, n)

    mytrain<-rbind(data1[train1,], data2[train2,])

    labs_train<-as.factor(c(rep(1, n), rep(2, n)) )

    myvalid<-rbind(data1[-train1,], data2[-train2,])

    labs_valid<-as.factor(c(rep(1, 100-n),
														rep(2, 100-n) ) )


		####################
		#COLS
		####################
		fit<-list()
		equa=V1 ~ V2 + V3
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

mean(cols_train)
mean(cols_valid)


#wt ~ hp + qsec == 63%
