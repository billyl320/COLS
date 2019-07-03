library(xtable) #for table creation for latex
library(ggplot2)#for graphics
library(scales)#for scientific notation
library(RColorBrewer) #for base r plot
library(class) #for base r plot
library(plyr)#for obtaining means by factor
library(MASS)#for qda and lda

#importing custom function to calculate proportion of white pixels
source('poly_prop.r')

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
props<-c()

for (i in 1:length(n)) {

  props[i]<-poly_prop(n[i])

}

#beta1 values for each class
betas<- (1/props) - 1
beta0<-rep(0, length(betas))

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
labs2<-as.factor(c(rep("n=3", dim(tris)[1]), rep("n=4", dim(squs)[1]), rep("n=5", dim(pens)[1]),
                rep("n=6", dim(hexs)[1]), rep("n=7", dim(hepts)[1]),   rep("n=8", dim(octs)[1]) ))


scat<-ggplot(data=temp, aes(x = white, y = black, colour = as.factor(labs2)))+
          geom_point(size=2)+
	 	      ggtitle("Encircled\nImage Histogram")+
		      xlab("White Counts")+
					ylab("Black Counts")+
			 		labs(colour= "Legend")+
					scale_y_continuous(label=scientific_10)+
          scale_x_continuous(label=scientific_10)+
          mytheme.scat+
          scale_color_discrete(breaks=c("n=3","n=4","n=5", "n=6",
                                        "n=7", "n=8"))+
          theme(legend.text=element_text(size=18),
                legend.title=element_text(size=24))

ggsave(filename="plots/Encircled_Image_Histograms.png", plot=scat,
       width=9, height=7)


#predicting classes via COLS
cols_class<-pred.class(y=mydata$black, x=mydata$white, beta0=beta0, beta1=betas)

table(cols_class, labs)

mean(cols_class==as.factor(as.numeric(labs)))


#compare to lda on all data
temp<-as.data.frame(cbind(as.factor(labs), mydata))
colnames(temp)[1]<-"labs"
lda.fit = lda(labs ~ white + black, data=temp)


#predicting
lda.pred=predict(lda.fit, temp)
lda.class = lda.pred$class

#results
table(lda.class, labs)
#overall classification rate for training
mean(lda.class==as.factor(as.numeric(labs)))


#compare to qda on all data
temp<-as.data.frame(cbind(as.factor(labs), mydata))
colnames(temp)[1]<-"labs"
qda.fit = qda(labs ~ white + black, data=temp)
#qda.fit #rank deficiency - ie unable to compute

#predicting
qda.pred=predict(qda.fit, temp)
qda.class = qda.pred$class

#results
table(qda.class, labs)
#overall classification rate for training
mean(qda.class==as.factor(as.numeric(labs)))



#
