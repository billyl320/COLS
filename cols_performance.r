#performance rate calculation

cnn<-c(0, 0.27, 0.30,
       0.32, 0.37, 0.42, 0.46)

qda<-c(0, 0.70, 0.77,
       0.81, 0.82, 0.82, 0.83)

svm<-c(0, 0.66, 0.73,
       0.76, 0.77, 0.77, 0.79)

tree<-c(0, 0.19, 0.20,
       0.22, 0.23, 0.24, 0.24)

lda<-c(0, 0.40, 0.41,
      0.42, 0.42, 0.42, 0.43)

lr<-c(0, 0.40, 0.41,
      0.42, 0.42, 0.42, 0.43)

cols<-c(rep(0.785,7))

#
mean(c(cols[-1]/lr[-1], cols[-1]/lda[-1],
       cols[-1]/tree[-1],cols[-1]/svm[-1],
       cols[-1]/qda[-1], cols[-1]/cnn[-1]) )


#
