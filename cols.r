#r script for COLS

#only usable for y= beta0 + beta1*x

#function to calculate the residuals and beta values if needed
cols<-function(x, y, beta0=0, beta1=NA){

  if(is.na(beta1)==TRUE){

    if(length(x)<2){

      stop('Need at least 2 observations.')

    }

    fit<-lm(y~x)
    beta0<-fit$coefficients[1]
    beta1<-fit$coefficients[2]

  }

  yhat=(x*beta1)+beta0
  residual = (yhat-y)^2

  ultima<-c(beta0, beta1, residual)
  names(ultima)[1:2]<-c("Beta0", "Beta1")
  res_names<-c()
  for(i in 1:length(ultima)-2){

    res_names[i]<-paste("Residual",as.character(i))

  }
  names(ultima)[3:length(ultima)]<-res_names

  return(ultima)

}


#example

#x<-c(5)
#y<-c(10)
#results<-cols(x=x, y=y, beta1=2)

#predicting which class an observation belongs to


pred.cols.class<-function(y, x, beta0=NA, beta1=NA){

  if( (is.na(beta1)==TRUE) || (is.na(beta0)==TRUE)){

    stop("beta0 and/or beta1 need values.")

  }

  if(length(beta0)!=length(beta1)){

    stop("beta0 and beta1 need to have the same number of coefficients.")

  }

  k<-length(beta1)
  residuals<-matrix(nrow=length(y), ncol=length(beta1), data=NA)

  for(i in 1:k){

      residuals[,i]<-cols(y=y, x=x, beta0=beta0[i], beta1=beta1[i])[1:length(x)+2]

  }

  class<-apply(FUN=which.min, X=residuals, MARGIN=1)
  return(class)

}

#example

#x<-c(5, 10)
#y<-c(10, 20)
#beta0=c(0, 0)
#beta1=c(2, 10)
#class<-pred.class(x=x, y=y, beta0=beta0, beta1=beta1)


#
