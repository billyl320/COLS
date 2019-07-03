#performance rate calculation



svm<-c(rep(1, 4))

tree<-c(0.50, 0.50, 0.99, 1.00)

lda<-c(rep(1, 4))

lr<-c(0.99, rep(0.98, 3))

cols<-c(rep(0.99,4))

#
mean(c(cols/lr, cols/lda,
       cols/tree,cols/svm ) )


#
