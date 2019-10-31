#performance rate calculation

cnn<-c(0.94, 0.96,
       0.96, 0.98)

svm<-c(0.98, 0.99,
       1.00, 0.99)

tree<-c(0.50, 0.50,
        0.93, 0.95)

lda<-c(0.98, 0.99,
       1.00, 1.00)

lr<-c(0.99, 1.00,
      1.00, 0.99)

cols<-c(rep(1.00,4))

#
mean(c(cols/lr,   cols/lda,
       cols/tree, cols/svm,
       cols/cnn) )


#
