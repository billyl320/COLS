#data preprocessing
from keras.utils import to_categorical

#building the model
from keras.models import Sequential
from keras.layers import Dense, Conv2D, Flatten, MaxPooling2D


#my imported packages
import numpy as np#typical package (Kinser 2018)
import itertools as it #I don't think that this is used
import scipy.misc as sm #typical image package (Kinser 2018)
import convert as cvt #custom functions
import rpy2.robjects as robjects #interface with R
from keras.callbacks import EarlyStopping
from keras.regularizers import l2
from keras.layers import SpatialDropout2D

#loading images


dirs = ["circle1", "circle2", "circle3"]
circ = cvt.GetAllImagesCNN(dirs)
dirs = ["caps", "none"]
caps = cvt.GetAllImagesCNN(dirs)

#creating labels for each class
circ_labels  = [0]*len(circ)
caps_labels  = [1]*len(caps)


##################################
## training sample size = 3
##################################

#sample size
n=3
#initialize arrays to hold results
train_vals=np.zeros(100)
test_vals=np.zeros(100)

#splitting data into training and testing
rans = robjects.r("""
set.seed(83150)
n=3
#training datasets
train3<-matrix(nrow=100, ncol=n)
train4<-matrix(nrow=100, ncol=n)
#testing training sets
test3<-matrix(nrow=100, ncol=906-n)
test4<-matrix(nrow=100, ncol=332-n)
for(i in 1:100){
    train3[i,]<-sample(0:905,  n)
    temp<-0:905
    test3[i,]<-temp[-(train3[i,]+1)]
    train4[i,]<-sample(0:331, n)
    temp<-0:331
    test4[i,]<-temp[-(train4[i,]+1)]
}
""")
circ_ran = np.asarray(robjects.r["train3"])
caps_ran = np.asarray(robjects.r["train4"])

circ_test = np.asarray(robjects.r["test3"])
caps_test = np.asarray(robjects.r["test4"])


for j in range(0,100):
    #############
    #all classes
    #############
    #training
    #training y labels
    circ_y_train =  [circ_labels[i] for i in circ_ran[j]]
    caps_y_train =  [caps_labels[i] for i in caps_ran[j]]
    y_train = np.concatenate( (circ_y_train, caps_y_train), axis=0 )
    #training images
    circ_x_train =  [circ[i] for i in circ_ran[j]]
    caps_x_train =  [caps[i] for i in caps_ran[j]]
    X_train = np.concatenate( (circ_x_train, caps_x_train), axis=0 )
    #testing
    #testing y labels
    circ_y_test =  [circ_labels[i] for i in circ_test[j]]
    caps_y_test =  [caps_labels[i] for i in caps_test[j]]
    y_test = np.concatenate( (circ_y_test, caps_y_test), axis=0 )
    #testing images
    circ_x_test =  [circ[i] for i in circ_test[j]]
    caps_x_test =  [caps[i] for i in caps_test[j]]
    X_test = np.concatenate( (circ_x_test, caps_x_test), axis=0 )
    ##################
    #data pre-processing
    ##################
    #reshape data to fit model
    X_train = X_train.reshape(len(X_train),160,240,1)
    X_test = X_test.reshape(len(X_test),160,240,1)
    #convert
    y_train = to_categorical(y_train)
    y_test = to_categorical(y_test)
    ##################
    #building the model
    ##################
    #3 conv layers
    #3 pooling layers
    #with epochs with early stopping
    #create model
    model = Sequential()
    #add early stopping component
    ES = [EarlyStopping(monitor='loss', min_delta=0.001, patience=10)]
    #add model layers
    #first C layer
    #Rectified Linear Unit activation, 64 nodes, 3x3 filter matrix, 160x240x1 images
    model.add(Conv2D(64, kernel_size=3, activation='relu', input_shape=(160, 240, 1), activity_regularizer=l2(0.001) ))
    #first pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    #first spatial dropout
    model.add(SpatialDropout2D(0.2))
    #second C layer
    #Rectified Linear Unit activation, 32 nodes, 3x3 filter matrix
    model.add(Conv2D(32, kernel_size=3, activation='relu', activity_regularizer=l2(0.001)))
    #second pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    #second spatial dropout
    model.add(SpatialDropout2D(0.2))
    #third C layer
    #Rectified Linear Unit activation, 32 nodes, 3x3 filter matrix
    model.add(Conv2D(32, kernel_size=3, activation='relu', activity_regularizer=l2(0.001)))
    #third pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2)))
    #third spatial dropout
    model.add(SpatialDropout2D(0.2))
    #adding 'flatten' layer
    model.add(Flatten())
    #6 nodes, softmax activation
    model.add(Dense(2, activation='softmax'))
    ##################
    #compiling the model
    ##################
    #lower score indicates better performance
    #also provides accuracy measure
    model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])
    ##################
    #training the model
    ##################
    #uses validation dat
    #epoch =100  means that the entire dataset is passed forward and backward through NN 100 times
    # see https://towardsdatascience.com/epoch-vs-iterations-vs-batch-size-4dfb9c7ce9c9
    model.fit(X_train, y_train, epochs=100, callbacks=ES, verbose=0)
    ##################
    #making predictions
    ##################
    #obtaining testing accuracy
    vals=model.predict(X_train)
    vals_pred = np.argmax(vals, axis=1)
    #checking with truth
    vals_truth = np.argmax(y_train, axis=1)
    train_vals[j]=sum((vals_pred==vals_truth)+0.0) / (len(vals_truth)+0.0)
    #predicting on testing
    vals=model.predict(X_test)
    vals_pred = np.argmax(vals, axis=1)
    #checking with truth
    vals_truth = np.argmax(y_test, axis=1)
    test_vals[j]=sum((vals_pred==vals_truth)+0.0) / (len(vals_truth)+0.0)



print('##################################')

print("Results for n=" + str(n))
print("Mean Training")
print(train_vals.mean())
print("SD Training")
print(train_vals.std())

print("Mean Testing")
print(test_vals.mean())
print("SD Training")
print(test_vals.std())

print('##################################')


##################################
## training sample size = 4
##################################

#sample size
n=4
#initialize arrays to hold results
train_vals=np.zeros(100)
test_vals=np.zeros(100)

#splitting data into training and testing
rans = robjects.r("""
set.seed(6204064)
n=4
#training datasets
train3<-matrix(nrow=100, ncol=n)
train4<-matrix(nrow=100, ncol=n)
#testing training sets
test3<-matrix(nrow=100, ncol=906-n)
test4<-matrix(nrow=100, ncol=332-n)
for(i in 1:100){
    train3[i,]<-sample(0:905,  n)
    temp<-0:905
    test3[i,]<-temp[-(train3[i,]+1)]
    train4[i,]<-sample(0:331, n)
    temp<-0:331
    test4[i,]<-temp[-(train4[i,]+1)]
}
""")
circ_ran = np.asarray(robjects.r["train3"])
caps_ran = np.asarray(robjects.r["train4"])

circ_test = np.asarray(robjects.r["test3"])
caps_test = np.asarray(robjects.r["test4"])


for j in range(0,100):
    #############
    #all classes
    #############
    #training
    #training y labels
    circ_y_train =  [circ_labels[i] for i in circ_ran[j]]
    caps_y_train =  [caps_labels[i] for i in caps_ran[j]]
    y_train = np.concatenate( (circ_y_train, caps_y_train ), axis=0 )
    #training images
    circ_x_train =  [circ[i] for i in circ_ran[j]]
    caps_x_train =  [caps[i] for i in caps_ran[j]]
    X_train = np.concatenate( (circ_x_train, caps_x_train ), axis=0 )
    #testing
    #testing y labels
    circ_y_test =  [circ_labels[i] for i in circ_test[j]]
    caps_y_test =  [caps_labels[i] for i in caps_test[j]]
    y_test = np.concatenate( (circ_y_test, caps_y_test ), axis=0 )
    #testing images
    circ_x_test =  [circ[i] for i in circ_test[j]]
    caps_x_test =  [caps[i] for i in caps_test[j]]
    X_test = np.concatenate( (circ_x_test, caps_x_test ), axis=0 )
    ##################
    #data pre-processing
    ##################
    #reshape data to fit model
    X_train = X_train.reshape(len(X_train),160,240,1)
    X_test = X_test.reshape(len(X_test),160,240,1)
    #convert
    y_train = to_categorical(y_train)
    y_test = to_categorical(y_test)
    ##################
    #building the model
    ##################
    #3 conv layers
    #3 pooling layers
    #with epochs with early stopping
    #create model
    model = Sequential()
    #add early stopping component
    ES = [EarlyStopping(monitor='loss', min_delta=0.001, patience=10)]
    #add model layers
    #first C layer
    #Rectified Linear Unit activation, 64 nodes, 3x3 filter matrix, 160x240x1 images
    model.add(Conv2D(64, kernel_size=3, activation='relu', input_shape=(160, 240, 1), activity_regularizer=l2(0.001) ))
    #first pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    #first spatial dropout
    model.add(SpatialDropout2D(0.2))
    #second C layer
    #Rectified Linear Unit activation, 32 nodes, 3x3 filter matrix
    model.add(Conv2D(32, kernel_size=3, activation='relu', activity_regularizer=l2(0.001)))
    #second pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    #second spatial dropout
    model.add(SpatialDropout2D(0.2))
    #third C layer
    #Rectified Linear Unit activation, 32 nodes, 3x3 filter matrix
    model.add(Conv2D(32, kernel_size=3, activation='relu', activity_regularizer=l2(0.001)))
    #third pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2)))
    #third spatial dropout
    model.add(SpatialDropout2D(0.2))
    #adding 'flatten' layer
    model.add(Flatten())
    #6 nodes, softmax activation
    model.add(Dense(2, activation='softmax'))
    ##################
    #compiling the model
    ##################
    #lower score indicates better performance
    #also provides accuracy measure
    model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])
    ##################
    #training the model
    ##################
    #uses validation dat
    #epoch =100  means that the entire dataset is passed forward and backward through NN 100 times
    # see https://towardsdatascience.com/epoch-vs-iterations-vs-batch-size-4dfb9c7ce9c9
    model.fit(X_train, y_train, epochs=100, callbacks=ES, verbose=0)
    ##################
    #making predictions
    ##################
    #obtaining testing accuracy
    vals=model.predict(X_train)
    vals_pred = np.argmax(vals, axis=1)
    #checking with truth
    vals_truth = np.argmax(y_train, axis=1)
    train_vals[j]=sum((vals_pred==vals_truth)+0.0) / (len(vals_truth)+0.0)
    #predicting on testing
    vals=model.predict(X_test)
    vals_pred = np.argmax(vals, axis=1)
    #checking with truth
    vals_truth = np.argmax(y_test, axis=1)
    test_vals[j]=sum((vals_pred==vals_truth)+0.0) / (len(vals_truth)+0.0)



print('##################################')

print("Results for n=" + str(n))
print("Mean Training")
print(train_vals.mean())
print("SD Training")
print(train_vals.std())

print("Mean Testing")
print(test_vals.mean())
print("SD Training")
print(test_vals.std())

print('##################################')


##################################
## training sample size = 5
##################################

#sample size
n=5
#initialize arrays to hold results
train_vals=np.zeros(100)
test_vals=np.zeros(100)

#splitting data into training and testing
rans = robjects.r("""
set.seed(9197216)
n=5
#training datasets
train3<-matrix(nrow=100, ncol=n)
train4<-matrix(nrow=100, ncol=n)
#testing training sets
test3<-matrix(nrow=100, ncol=906-n)
test4<-matrix(nrow=100, ncol=332-n)
for(i in 1:100){
    train3[i,]<-sample(0:905,  n)
    temp<-0:905
    test3[i,]<-temp[-(train3[i,]+1)]
    train4[i,]<-sample(0:331, n)
    temp<-0:331
    test4[i,]<-temp[-(train4[i,]+1)]
}
""")
circ_ran = np.asarray(robjects.r["train3"])
caps_ran = np.asarray(robjects.r["train4"])

circ_test = np.asarray(robjects.r["test3"])
caps_test = np.asarray(robjects.r["test4"])



for j in range(0,100):
    #############
    #all classes
    #############
    #training
    #training y labels
    circ_y_train =  [circ_labels[i] for i in circ_ran[j]]
    caps_y_train =  [caps_labels[i] for i in caps_ran[j]]
    y_train = np.concatenate( (circ_y_train, caps_y_train ), axis=0 )
    #training images
    circ_x_train =  [circ[i] for i in circ_ran[j]]
    caps_x_train =  [caps[i] for i in caps_ran[j]]
    X_train = np.concatenate( (circ_x_train, caps_x_train ), axis=0 )
    #testing
    #testing y labels
    circ_y_test =  [circ_labels[i] for i in circ_test[j]]
    caps_y_test =  [caps_labels[i] for i in caps_test[j]]
    y_test = np.concatenate( (circ_y_test, caps_y_test ), axis=0 )
    #testing images
    circ_x_test =  [circ[i] for i in circ_test[j]]
    caps_x_test =  [caps[i] for i in caps_test[j]]
    X_test = np.concatenate( (circ_x_test, caps_x_test ), axis=0 )
    ##################
    #data pre-processing
    ##################
    #reshape data to fit model
    X_train = X_train.reshape(len(X_train),160,240,1)
    X_test = X_test.reshape(len(X_test),160,240,1)
    #convert
    y_train = to_categorical(y_train)
    y_test = to_categorical(y_test)
    ##################
    #building the model
    ##################
    #3 conv layers
    #3 pooling layers
    #with epochs with early stopping
    #create model
    model = Sequential()
    #add early stopping component
    ES = [EarlyStopping(monitor='loss', min_delta=0.001, patience=10)]
    #add model layers
    #first C layer
    #Rectified Linear Unit activation, 64 nodes, 3x3 filter matrix, 160x240x1 images
    model.add(Conv2D(64, kernel_size=3, activation='relu', input_shape=(160, 240, 1), activity_regularizer=l2(0.001) ))
    #first pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    #first spatial dropout
    model.add(SpatialDropout2D(0.2))
    #second C layer
    #Rectified Linear Unit activation, 32 nodes, 3x3 filter matrix
    model.add(Conv2D(32, kernel_size=3, activation='relu', activity_regularizer=l2(0.001)))
    #second pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    #second spatial dropout
    model.add(SpatialDropout2D(0.2))
    #third C layer
    #Rectified Linear Unit activation, 32 nodes, 3x3 filter matrix
    model.add(Conv2D(32, kernel_size=3, activation='relu', activity_regularizer=l2(0.001)))
    #third pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2)))
    #third spatial dropout
    model.add(SpatialDropout2D(0.2))
    #adding 'flatten' layer
    model.add(Flatten())
    #6 nodes, softmax activation
    model.add(Dense(2, activation='softmax'))
    ##################
    #compiling the model
    ##################
    #lower score indicates better performance
    #also provides accuracy measure
    model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])
    ##################
    #training the model
    ##################
    #uses validation dat
    #epoch =100  means that the entire dataset is passed forward and backward through NN 100 times
    # see https://towardsdatascience.com/epoch-vs-iterations-vs-batch-size-4dfb9c7ce9c9
    model.fit(X_train, y_train, epochs=100, callbacks=ES, verbose=0)
    ##################
    #making predictions
    ##################
    #obtaining testing accuracy
    vals=model.predict(X_train)
    vals_pred = np.argmax(vals, axis=1)
    #checking with truth
    vals_truth = np.argmax(y_train, axis=1)
    train_vals[j]=sum((vals_pred==vals_truth)+0.0) / (len(vals_truth)+0.0)
    #predicting on testing
    vals=model.predict(X_test)
    vals_pred = np.argmax(vals, axis=1)
    #checking with truth
    vals_truth = np.argmax(y_test, axis=1)
    test_vals[j]=sum((vals_pred==vals_truth)+0.0) / (len(vals_truth)+0.0)



print('##################################')

print("Results for n=" + str(n))
print("Mean Training")
print(train_vals.mean())
print("SD Training")
print(train_vals.std())

print("Mean Testing")
print(test_vals.mean())
print("SD Training")
print(test_vals.std())

print('##################################')


##################################
## training sample size = 6
##################################

#sample size
n=6
#initialize arrays to hold results
train_vals=np.zeros(100)
test_vals=np.zeros(100)

#splitting data into training and testing
rans = robjects.r("""
set.seed(57275)
n=6
#training datasets
train3<-matrix(nrow=100, ncol=n)
train4<-matrix(nrow=100, ncol=n)
#testing training sets
test3<-matrix(nrow=100, ncol=906-n)
test4<-matrix(nrow=100, ncol=332-n)
for(i in 1:100){
    train3[i,]<-sample(0:905,  n)
    temp<-0:905
    test3[i,]<-temp[-(train3[i,]+1)]
    train4[i,]<-sample(0:331, n)
    temp<-0:331
    test4[i,]<-temp[-(train4[i,]+1)]
}
""")
circ_ran = np.asarray(robjects.r["train3"])
caps_ran = np.asarray(robjects.r["train4"])

circ_test = np.asarray(robjects.r["test3"])
caps_test = np.asarray(robjects.r["test4"])



for j in range(0,100):
    #############
    #all classes
    #############
    #training
    #training y labels
    circ_y_train =  [circ_labels[i] for i in circ_ran[j]]
    caps_y_train =  [caps_labels[i] for i in caps_ran[j]]
    y_train = np.concatenate( (circ_y_train, caps_y_train ), axis=0 )
    #training images
    circ_x_train =  [circ[i] for i in circ_ran[j]]
    caps_x_train =  [caps[i] for i in caps_ran[j]]
    X_train = np.concatenate( (circ_x_train, caps_x_train ), axis=0 )
    #testing
    #testing y labels
    circ_y_test =  [circ_labels[i] for i in circ_test[j]]
    caps_y_test =  [caps_labels[i] for i in caps_test[j]]
    y_test = np.concatenate( (circ_y_test, caps_y_test ), axis=0 )
    #testing images
    circ_x_test =  [circ[i] for i in circ_test[j]]
    caps_x_test =  [caps[i] for i in caps_test[j]]
    X_test = np.concatenate( (circ_x_test, caps_x_test ), axis=0 )
    ##################
    #data pre-processing
    ##################
    #reshape data to fit model
    X_train = X_train.reshape(len(X_train),160,240,1)
    X_test = X_test.reshape(len(X_test),160,240,1)
    #convert
    y_train = to_categorical(y_train)
    y_test = to_categorical(y_test)
    ##################
    #building the model
    ##################
    #3 conv layers
    #3 pooling layers
    #with epochs with early stopping
    #create model
    model = Sequential()
    #add early stopping component
    ES = [EarlyStopping(monitor='loss', min_delta=0.001, patience=10)]
    #add model layers
    #first C layer
    #Rectified Linear Unit activation, 64 nodes, 3x3 filter matrix, 160x240x1 images
    model.add(Conv2D(64, kernel_size=3, activation='relu', input_shape=(160, 240, 1), activity_regularizer=l2(0.001) ))
    #first pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    #first spatial dropout
    model.add(SpatialDropout2D(0.2))
    #second C layer
    #Rectified Linear Unit activation, 32 nodes, 3x3 filter matrix
    model.add(Conv2D(32, kernel_size=3, activation='relu', activity_regularizer=l2(0.001)))
    #second pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2), strides=(2, 2)))
    #second spatial dropout
    model.add(SpatialDropout2D(0.2))
    #third C layer
    #Rectified Linear Unit activation, 32 nodes, 3x3 filter matrix
    model.add(Conv2D(32, kernel_size=3, activation='relu', activity_regularizer=l2(0.001)))
    #third pooling layer
    model.add(MaxPooling2D(pool_size=(2, 2)))
    #third spatial dropout
    model.add(SpatialDropout2D(0.2))
    #adding 'flatten' layer
    model.add(Flatten())
    #6 nodes, softmax activation
    model.add(Dense(2, activation='softmax'))
    ##################
    #compiling the model
    ##################
    #lower score indicates better performance
    #also provides accuracy measure
    model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])
    ##################
    #training the model
    ##################
    #uses validation dat
    #epoch =100  means that the entire dataset is passed forward and backward through NN 100 times
    # see https://towardsdatascience.com/epoch-vs-iterations-vs-batch-size-4dfb9c7ce9c9
    model.fit(X_train, y_train, epochs=100, callbacks=ES, verbose=0)
    ##################
    #making predictions
    ##################
    #obtaining testing accuracy
    vals=model.predict(X_train)
    vals_pred = np.argmax(vals, axis=1)
    #checking with truth
    vals_truth = np.argmax(y_train, axis=1)
    train_vals[j]=sum((vals_pred==vals_truth)+0.0) / (len(vals_truth)+0.0)
    #predicting on testing
    vals=model.predict(X_test)
    vals_pred = np.argmax(vals, axis=1)
    #checking with truth
    vals_truth = np.argmax(y_test, axis=1)
    test_vals[j]=sum((vals_pred==vals_truth)+0.0) / (len(vals_truth)+0.0)



print('##################################')

print("Results for n=" + str(n))
print("Mean Training")
print(train_vals.mean())
print("SD Training")
print(train_vals.std())

print("Mean Testing")
print(test_vals.mean())
print("SD Training")
print(test_vals.std())

print('##################################')


#
