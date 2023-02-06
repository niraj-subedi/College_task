#The following are the library that are used to perform the provided tasks
library(matlib)
library(ggplot2)
library(rsample)

#First the data for the X is imported from the given file X_16.csv
X=as.matrix(read.csv(file = "~/Desktop/Task/X_16.csv",header= F))
colnames(X)=c("X1","X2","X3","X4")

Y=as.matrix(read.csv(file = "~/Desktop/Task/y_16.csv",header = F))
colnames(Y)=c("Y")

Time= read.csv("~/Desktop/Task/time_16.csv",header = F,skip = 1)
Time= as.matrix(rbind(0,Time))


#Perfroming the TASK-1: Time series plot
X.ts=ts(X,start = c(min(Time),max(Time)),frequency =1)
Y.ts=ts(Y,start = c(min(Time),max(Time)),frequency =1)


plot(X.ts,main = "Time series plot of X Signal(Input signal)", xlab = "Time", ylab = "Input signal")
plot(Y.ts,main = "Time series plot of Y Signal(Output signal)", xlab = "Time", ylab = "Output signal")


#Perfroming the Task-2: Density plot 
#Creating a density if X signal
dis=density(X)
plot(dis,main = "Density plot of all the input signal")
# Creating a Histrogram of X signal
hist(X,freq = FALSE,main = "Density and Histogram")
#Adding density in the histogram
lines(dis,lwd=2,col="red")
rug(jitter(X))

#Creating a density if X1 signal
dis_X1=density(X[,"X1"])
hist(X[,"X1"],freq = FALSE,main = "Histogram and density plot of X1 input",xlab = "X1 Signal")
lines(dis_X1,lwd=2,col="red")
# Add the data-poins with noise in the X-axis
rug(jitter(X[,"X1"]))

#Creating a density if X2 signal
dis_X2=density(X[,"X2"])
hist(X[,"X2"],freq = FALSE,main = "Histogram and density plot of X2 input",xlab = "X2 Signal")
lines(dis_X2,lwd=2,col="red")
rug(jitter(X[,"X2"]))
#Creating a density if X3 signal
dis_X3=density(X[,"X3"])
hist(X[,"X3"],freq = FALSE,main = "Histogram and density plot of X3 input",xlab = "X3 Signal")
lines(dis_X3,lwd=2,col="red")
rug(jitter(X[,"X3"]))

#Creating a density if X4 signal
dis_X4=density(X[,"X4"])
hist(X[,"X4"],freq = FALSE,main = "Histogram and density plot of X4 input",xlab = "X4 Signal")
lines(dis_X1,lwd=2,col="red")
rug(jitter(X[,"X4"]))

#Creating a density if Y signal
dis_y=density(Y)
plot(dis_y,main = "Density plot of Y (output)",,xlab = "Output Signal")
hist(Y,freq = FALSE,main = "Density and Histogram plot of Y (output)",xlab = "Output Signal")
lines(dis_y,lwd=2,col="red")
rug(jitter(Y))

## Task 1: Creating Correlation and Scatter plot.
# Plotting X1 against Y
plot(X[,"X1"],Y,main = "Correlation betweeen X1 and Y signal", xlab = "X1 signal", ylab = "Output
signal" )
# Plotting X2 against Y
plot(X[,"X2"],Y,main = "Correlation betweeen X2 and Y signal", xlab = "X2 signal", ylab = "Output
signal")
# Plotting X3 against Y
plot(X[,"X3"],Y,main = "Correlation betweeen X3 and Y signal", xlab = "X3 signal", ylab = "Output
signal")
# Plotting X4 against Y
plot(X[,"X4"],Y,main = "Correlation betweeen X4 and Y signal", xlab = "X4 signal", ylab = "Output
signal")
#Task 2
# Calculating ones for binding the data
ones = matrix(1 , length(X)/4,1)
ones


#Task 2.1
#Calculating thetahat of Model 1
#Binding data from equation of model 1.
X_model1<-cbind(ones,(X[,"X4"]),(X[,"X1"])^2,(X[,"X1"])^3,(X[,"X3"])^4)
X_model1
#Calculating thetahat of model 1
Model1_thetahat=solve(t(X_model1) %*% X_model1) %*% t(X_model1) %*% Y
Model1_thetahat
#Model 2
#Binding data from equation of model 2.
X_model2<-cbind(ones,(X[,"X3"])^3,(X[,"X3"])^4)
X_model2
#Calculating thetahat of Model 2
Model2_thetahat=solve(t(X_model2) %*% X_model2) %*% t(X_model2) %*% Y
Model2_thetahat
#For model 3
#Binding data from equation of model 3.
X_model3<-cbind(ones,X[,"X2"],(X[,"X1"])^3,(X[,"X3"])^4)
X_model3
#Calculating thetahat of Model 3
Model3_thetahat=solve(t(X_model3) %*% X_model3) %*% t(X_model3) %*% Y
Model3_thetahat
#For model 4
#Binding data from equation of model 4.
X_model4<-cbind(ones,X[,"X4"],X[,"X1"]^3,X[,"X3"]^4)
X_model4
#Calculating thetahat of Model 4
Model4_thetahat=solve(t(X_model4) %*% X_model4) %*% t(X_model4) %*% Y
Model4_thetahat
#Binding data from equation of model 5.
X_model5<-cbind(ones,X[,"X4"],X[,"X1"]^2,X[,"X1"]^3,X[,"X3"]^4,X[,"X1"]^4)
X_model5
#Calculating thetahat of Model 5
Model5_thetahat=solve(t(X_model5) %*% X_model5) %*% t(X_model5) %*% Y
Model5_thetahat
##Task 2.2
#Calculating Y-hat and RSS Model 1
Y_hat_m1 = X_model1 %*% Model1_thetahat
Y_hat_m1
#Calculating RSS
RSS_Model_1=sum((Y-Y_hat_m1)^2)
RSS_Model_1
#Calculating Y-hat and RSS of model 2
Y_hat_m2 = X_model2 %*% Model2_thetahat
Y_hat_m2
RSS_Model_2=sum((Y-Y_hat_m2)^2)
RSS_Model_2

#Calculating Y-hat and RSS of model 3
Y_hat_m3 = X_model3 %*% Model3_thetahat
Y_hat_m3
RSS_Model_3=sum((Y-Y_hat_m3)^2)
RSS_Model_3
#Calculating Y-hat and RSS of model 4
Y_hat_m4 = X_model4 %*% Model4_thetahat
Y_hat_m4
RSS_Model_4=sum((Y-Y_hat_m4)^2)
RSS_Model_4
#Calculating Y-hat and RSS of model 5
Y_hat_m5 = X_model5 %*% Model5_thetahat
Y_hat_m5
RSS_Model_5=sum((Y-Y_hat_m5)^2)
RSS_Model_5
### Task 2.3 Calculating likelihood and Variance of each model
N=length(Y)
#Calculating the Variance of Model 1
Variance_model1=RSS_Model_1/(N-1)
Variance_model1
#Calculating the log-likelihood of Model 1
likehood_Model_1=
  -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model1))-(1/(2*Variance_model1))*RSS_Model_1
likehood_Model_1
#Calculating Variance and log-likelihood of Model 2
Variance_model2=RSS_Model_2/(N-1)
Variance_model2
likehood_Model_2=
  -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model2))-(1/(2*Variance_model2))*RSS_Model_2
likehood_Model_2
#Calculating Variance and log-likelihood of Model 3
Variance_model3=RSS_Model_3/(N-1)
Variance_model3
likehood_Model_3=
  -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model3))-(1/(2*Variance_model3))*RSS_Model_3
likehood_Model_3
#Calculating Variance and log-likelihood of Model 4
Variance_model4=RSS_Model_4/(N-1)
Variance_model4

likehood_Model_4=
  -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model4))-(1/(2*Variance_model4))*RSS_Model_4
likehood_Model_4

#Calculating Variance and log-likelihood of Model 5
Variance_model5=RSS_Model_5/(N-1)
Variance_model5
likehood_Model_5=
  -(N/2)*(log(2*pi))-(N/2)*(log(Variance_model5))-(1/(2*Variance_model5))*RSS_Model_5
likehood_Model_5
### Task 2.4 Calculating AIC And BIC of Each model
##Calculating AIC and BIC of model 1
K_model1<-length(Model1_thetahat)
K_model1
AIC_model1=2*K_model1-2*likehood_Model_1
AIC_model1
BIC_model1=K_model1*log(N)-2*likehood_Model_1
BIC_model1
## thetahat of model 2
K_model2<-length(Model2_thetahat)
K_model2
##Calculating AIC and BIC of model 2
AIC_model2=2*K_model2-2*likehood_Model_2
AIC_model2
BIC_model2=K_model2*log(N)-2*likehood_Model_2
BIC_model2
## thetahat of model 3
K_model3<-length(Model3_thetahat)
K_model3
##Calculating AIC and BIC of model 3
AIC_model3=2*K_model3-2*likehood_Model_3
AIC_model3
BIC_model3=K_model3*log(N)-2*likehood_Model_3
BIC_model3
## thetahat of model 4
K_model4<-length(Model1_thetahat)
K_model4
##Calculating AIC and BIC of model 4
AIC_model4=2*K_model4-2*likehood_Model_4
AIC_model4
BIC_model4=K_model4*log(N)-2*likehood_Model_4

BIC_model4
## thetahat of model 5
K_model5<-length(Model5_thetahat)
K_model5
##Calculating AIC and BIC of model 5
AIC_model5=2*K_model5-2*likehood_Model_5
AIC_model5
BIC_model5=K_model5*log(N)-2*likehood_Model_5
BIC_model5

## Task 2.5
## Error of model1
model1_error <- Y-Y_hat_m1
## Plotting the graph QQplot and QQ line of model 1
qqnorm(model1_error, col = "blue",main = "QQ plot of model 1")
qqline(model1_error, col = "red",lwd=1)
## Error of model2
model2_error <- Y-Y_hat_m2 # error of model 2
## Plotting QQplot and QQ line of model 2
qqnorm(model2_error, col = "blue",main = "QQ plot of model 2")
qqline(model2_error, col = "red")
## Error of model3
model3_error <- Y- Y_hat_m3
## Plotting QQplot and QQ line of model 3
qqnorm(model3_error, col = "blue",main = "QQ plot of model 3")
qqline(model3_error, col = "red")
## Error of model4
model4_error <- Y-Y_hat_m4
## Plotting QQplot and QQ line of model 4
qqnorm(model4_error, col = "blue",main = "QQ plot of model 4")
qqline(model4_error, col = "red")
## Error of model5
model5_error <- Y- Y_hat_m5
## Plotting QQplot and QQ line of model 5
qqnorm(model5_error, col = "blue",main = "QQ plot of model 5")
qqline(model5_error, col = "red")

### Task 2.7
## Spliting the data of y into 2 form i.e. Traning and testing data set.
split_Y=initial_split(data = as.data.frame(Y),prop=.7)

## Traning Y data split
Y_training_set<-training(split_Y)
Y_testing_set<-as.matrix(testing(split_Y))
## Testing Y data split
Y_training_data<-as.matrix(Y_training_set)
## Spliting the data of y into 2 form i.e. Traning and testing data set.
split_X<-initial_split(data = as.data.frame(X),prop=.7)
## Traning X data split
X_training_set<-training(split_X)
## Testing X data split
X_testing_set<-as.matrix(testing(split_X))
X_testing_data<-as.matrix(X_testing_set)
X_training_data<-as.matrix(X_training_set)
### Estimating model parameters using Traning set
traning_ones=matrix(1 , length(X_training_set$X1),1)
X_traning_model<-cbind(traning_ones,X_training_set[,"X2"],(X_training_set[,"X1"])^3,(X_training_set[,"X3"])^4) 

traning_thetahat=solve(t(X_traning_model) %*% X_traning_model) %*% t(X_traning_model)

### Model out/Prediction
Y_testing_hat = X_testing_data %*% traning_thetahat
Y_testing_hat
RSS_testing=sum((Y_testing_set - Y_testing_hat)^2)
RSS_testing

t.test(Y_training_data, mu=500, alternative="two.sided", conf.level=0.95)

C_I1=-0.2783950
C_I2=0.2762101

p2 <- plot(density(Y_training_data), col="blue", lwd=2,main="Distribution of Traning Data set")
abline(v=C_I1,col="red", lty=2)
abline(v=C_I2,col="red", lty=2)

thetaHat_training=solve(t(X_training_data) %*% X_training_data) %*% t(X_training_data) %*%
Y_training_data
thetaHat_training
length(thetaHat_training)

dis_test=density(Y_training_data)
plot((dis_test))
plot(dis_test,main = "Density plot of Y Signal")

### Calculating Confidential interval
z=1.96 ##(95%) Confidential interval
error=((Y_testing_set-Y_testing_hat))
n_len=length(Y_testing_hat)
C_I_1= z * sqrt( (error * (1-error) ) / n_len)
C_I_1
error
C_I_2= z * sqrt( (error * (1+error) / n_len))
  
##Task 3
## Model 3 will be used, parameter are selected and kept constant.
rr_1=0
arr_2=0
f_value=0
s_value=0
Model3_thetahat
#values from thetahat
thetebias <- 0.448299550 #choosen parameter
thetaone <- 0.038109255 # chosen prarameter
thetatwo <- 0.009827804 # constant value
thetafour <- 0.002092558 # constant value
Epison <- RSS_Model_3 * 2 ## fixing value of eplision
num <- 100 #number of iteration
##Calculating Y-hat for performing rejection ABC
counter <- 0
for (i in 1:num) {
  range1 <- runif(1,-0.448299550,0.448299550) # calculating the range
  range1
  range2 <- runif(1,-0.038109255,0.038109255)
  New_thetahat <- matrix(c(range1,range2,thetatwo,thetafour))
  New_Y_Hat <- X_model3 %*% New_thetahat ## New Y hat
  new_RSS <- sum((Y-New_Y_Hat)^2)
  new_RSS
  if (new_RSS > Epison){
    arr_1[i] <- range1
    arr_2[i] <- range2
    counter = counter+1
    f_value <- matrix(arr_1)
    s_value <- matrix(arr_2)
  }
}
hist(f_value)             
hist(s_value)

###ploting the graph
plot(f_value,s_value, col = c("red", "blue"), main = "Joint and Marginal Posterior Distribution")

