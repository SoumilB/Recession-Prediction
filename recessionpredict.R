rm(list=ls())

library(readr)
library(foreign)
library(randomForest)
library(dplyr)
library(tictoc)
library(glmnet)

#setwd("~/Desktop/NUS/Year 4/SEMESTER 7/EC4308/Project")

#install.packages("dplyr")

#Pre processing
RMSE <- function(pred, truth){ 
  return(sqrt(mean((truth - pred)^2)))
} 
fredmd = read_csv("FRED MD.csv")
usrec = read_csv("USREC.csv")
usrec = usrec[-c(1:1249),] #Removing all unnecessary recession times

#Making all variables Stationary

fredmd_new = fredmd[-1,] #remove transform row

for (i in 2:ncol(fredmd)) {
  if (fredmd[1,i] == 2) {
    for (j in nrow(fredmd_new):2) {
      fredmd_new[j,i] = fredmd_new[j,i] - fredmd_new[j-1,i]
    }
    fredmd_new[1,i] = NA
  }
  else if (fredmd[1,i] == 3) {
    for (j in nrow(fredmd_new):2) {
      fredmd_new[j,i] = fredmd_new[j,i] - fredmd_new[j-1,i]
      fredmd_new[j,i] = (fredmd_new[j,i])^2
    }
    fredmd_new[1,i] = NA
  }
  else if (fredmd[1,i] == 4) {
    fredmd_new[i] = log(fredmd_new[i])
  }
  else if (fredmd[1,i] == 5) {
    fredmd_new[i] = log(fredmd_new[i])
    for (j in nrow(fredmd_new):2) {
      fredmd_new[j,i] = fredmd_new[j,i] - fredmd_new[j-1,i]
    }
    fredmd_new[1,i] = NA
  }
  else if (fredmd[1,i] == 6) {
    fredmd_new[i] = log(fredmd_new[i])
    for (j in nrow(fredmd_new):2) {
      fredmd_new[j,i] = fredmd_new[j,i] - fredmd_new[j-1,i]
      fredmd_new[j,i] = (fredmd_new[j,i])^2
    }
    fredmd_new[1,i] = NA
  }
  else if (fredmd[1,i] == 7) {
    for (k in nrow(fredmd_new):2) {
      fredmd_new[k,i] = ((fredmd_new[k,i])/(fredmd_new[k-1,i])) - 1
    }
    for (j in nrow(fredmd_new):2) {
      fredmd_new[j,i] = fredmd_new[j,i] - fredmd_new[j-1,i]
    }
    fredmd_new[1,i] = NA
  }
}

#joining table and pre processing
fredmd_new = cbind(usrec[2], fredmd_new)
fredmd_new = fredmd_new[-nrow(fredmd_new),] #removing NA date
rownames(fredmd_new) = fredmd_new$sasdate
fredmd_new = fredmd_new[,-2] #remove date column
fredmd_new = fredmd_new %>% mutate(IS = GS10-TB3MS) ##Creating IS column



##Main Piece
Y = fredmd_new[-c(1,733:740),] #Eliminate first value and 2020

Y = Y %>%
  select_if(~ !any(is.na(.))) #Remove NA columns

Y_data = as.matrix(Y) #making into a matrix

nprev = 156 #Indices of 2007-2019, last 13 years

oosy=tail(Y[,1],nprev) ##Y values

####### Benchmark1: AR(p) forecast


source("modified-func-dy.R")

dyprobit1p=dy.rolling.window(Y_data,nprev,1,lag=1,type = "dynamic") #1-step dynamic probit forecast
dyprobit3p=dy.rolling.window(Y_data,nprev,1,lag=3,type = "dynamic") #3-step dynamic probit forecast
dyprobit6p=dy.rolling.window(Y_data,nprev,1,lag=6,type = "dynamic") #6-step dynamic probit forecast
dyprobit12p=dy.rolling.window(Y_data,nprev,1,lag=12,type = "dynamic") #12-step dynamic probit forecast

#Dynamic probit forecasts RMSE:

dy.rmse1=dyprobit1p$errors
dy.rmse3=dyprobit3p$errors
dy.rmse6=dyprobit6p$errors
dy.rmse12=dyprobit12p$errors

# AR benchmark
source("modified-func-ar.R")

ar1p=ar.rolling.window(Y_data,nprev,1,1,type = "autoreg") #1-step AutoRegressive dynamic probit forecast
ar3p=ar.rolling.window(Y_data,nprev,1,3,type = "autoreg") #3-step AutoRegressive dynamic probit forecast
ar6p=ar.rolling.window(Y_data,nprev,1,6,type = "autoreg") #6-step AutoRegressive dynamic probit forecast
ar12p=ar.rolling.window(Y_data,nprev,1,12,type = "autoreg") #12-step AutoRegressive dynamic probit forecast

#AutoRegressive dynamic probit forecasts RMSE:

ar.rmse1=ar1p$errors
ar.rmse3=ar3p$errors
ar.rmse6=ar6p$errors
ar.rmse12=ar12p$errors

##### Random Forest Prediction

source("modified-func-rf.R")

rf1c=rf.rolling.window(Y_data,nprev,1,1)
rf3c=rf.rolling.window(Y_data,nprev,1,3)
rf6c=rf.rolling.window(Y_data,nprev,1,6)
rf12c=rf.rolling.window(Y_data,nprev,1,12)

#See the RMSE:
rf.rmse1=rf1c$errors
rf.rmse3=rf3c$errors
rf.rmse6=rf6c$errors
rf.rmse12=rf12c$errors

### 
##Plots
##

#Create the time series object collecting 1-step best=performing ML forecasts
ml1.ts=ts(cbind(oosy,dyprobit1p$pred,ar1p$pred,rf1c$pred_prob), start=c(2007,1), end=c(2019,12), freq=12)
colnames(ml1.ts)=c("True Value","Dynamic Probit","AR Dynamic Probit","RF")

#Create the time series object collecting 3-step best=performing ML forecasts
ml3.ts=ts(cbind(oosy,dyprobit3p$pred,ar3p$pred,rf3c$pred_prob), start=c(2007,1), end=c(2019,12), freq=12)
colnames(ml3.ts)=c("True Value","Dynamic Probit","AR Dynamic Probit","RF")

#Create the time series object collecting 6-step best=performing ML forecasts
ml6.ts=ts(cbind(oosy,dyprobit6p$pred,ar6p$pred,rf6c$pred_prob), start=c(2007,1), end=c(2019,12), freq=12)
colnames(ml6.ts)=c("True Value","Dynamic Probit","AR Dynamic Probit","RF")

#Create the time series object collecting 12-step best=performing ML forecasts
ml12.ts=ts(cbind(oosy,dyprobit12p$pred,ar12p$pred,rf12c$pred_prob), start=c(2007,1), end=c(2019,12), freq=12)
colnames(ml12.ts)=c("True Value","Dynamic Probit","AR Dynamic Probit","RF")

#Plot the graph for 1-step forecasts
#windows()
plot.ts(ml1.ts[,1], main="1-step ML forecasts", cex.axis=1.5, lwd=2, ylab="US Recession")
points(ml1.ts[,2], type="l", col="blue",lwd=1.8)
points(ml1.ts[,3], type="l", col="green",lwd=1.8)
points(ml1.ts[,4], type="l", col="red",lwd=1.8)
legend("topright", c("Recession","Dynamic Probit","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","green","red"))

#Plot the graph for 3-step forecasts
#windows()
plot.ts(ml3.ts[,1], main="3-step ML forecasts", cex.axis=1.5, lwd=2, ylab="US Recession")
points(ml3.ts[,2], type="l", col="blue",lwd=1.8)
points(ml3.ts[,3], type="l", col="green",lwd=1.8)
points(ml3.ts[,4], type="l", col="red",lwd=1.8)
legend("topright", c("Recession","Dynamic Probit","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","green","red"))

#Plot the graph for 6-step forecasts
#windows()
plot.ts(ml6.ts[,1], main="6-step ML forecasts", cex.axis=1.5, lwd=2, ylab="US Recession")
points(ml6.ts[,2], type="l", col="blue",lwd=1.8)
points(ml6.ts[,3], type="l", col="green",lwd=1.8)
points(ml6.ts[,4], type="l", col="red",lwd=1.8)
legend("topright", c("Recession","Dynamic Probit","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","green","red"))

#Plot the graph for 12-step forecasts
#windows()
plot.ts(ml12.ts[,1], main="12-step ML forecasts", cex.axis=1.5, lwd=2, ylab="US Recession")
points(ml12.ts[,2], type="l", col="blue",lwd=1.8)
points(ml12.ts[,3], type="l", col="green",lwd=1.8)
points(ml12.ts[,4], type="l", col="red",lwd=1.8)
legend("topright", c("Recession","Dynamic Probit","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","green","red"))

######ACTUAL PREDICTIONS OF 1-0


dml1.ts=ts(cbind(oosy,ar1p$pred_actual,rf1c$pred), start=c(2007,1), end=c(2019,12), freq=12)
colnames(dml1.ts)=c("True Value","AR Dynamic Probit","RF")

#Create the time series object collecting 3-step best=perf_actualorming ML forecasts
dml3.ts=ts(cbind(oosy,ar3p$pred_actual,rf3c$pred), start=c(2007,1), end=c(2019,12), freq=12)
colnames(dml3.ts)=c("True Value","AR Dynamic Probit","RF")

#Create the time series object collecting 6-step best=performing ML forecasts
dml6.ts=ts(cbind(oosy,ar6p$pred_actual,rf6c$pred), start=c(2007,1), end=c(2019,12), freq=12)
colnames(dml6.ts)=c("True Value","AR Dynamic Probit","RF")

#Create the time series object collecting 12-step best=performing ML forecasts
dml12.ts=ts(cbind(oosy,ar12p$pred_actual,rf12c$pred), start=c(2007,1), end=c(2019,12), freq=12)
colnames(dml12.ts)=c("True Value","AR Dynamic Probit","RF")

#Plot the graph for 1-step forecasts
#windows()
plot.ts(dml1.ts[,1], main="1-step ML forecasts", cex.axis=1.5, lwd=2, ylab="US Recession")
points(dml1.ts[,2], type="l", col="blue",lwd=1.8)
points(dml1.ts[,3], type="l", col="red",lwd=1.8)
legend("topright", c("Recession","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","red"))

#Plot the graph for 3-step forecasts
#windows()
plot.ts(dml3.ts[,1], main="3-step ML forecasts", cex.axis=1.5, lwd=2, ylab="US Recession")
points(dml3.ts[,2], type="l", col="blue",lwd=1.8)
points(dml3.ts[,3], type="l", col="red",lwd=1.8)
legend("topright", c("Recession","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","red"))

#Plot the graph for 6-step forecasts
#windows()
plot.ts(dml6.ts[,1], main="6-step ML forecasts", cex.axis=1.5, lwd=2, ylab="US Recession")
points(dml6.ts[,2], type="l", col="blue",lwd=1.8)
points(dml6.ts[,3], type="l", col="red",lwd=1.8)
legend("topright", c("Recession","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","red"))

#Plot the graph for 12-step forecasts
#windows()
plot.ts(dml12.ts[,1], main="12-step ML forecasts", cex.axis=1.5, lwd=2, ylab="US Recession")
points(dml12.ts[,2], type="l", col="blue",lwd=1.8)
points(dml12.ts[,3], type="l", col="red",lwd=1.8)
legend("topright", c("Recession","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","red"))


###Prediction loss

#Compute squared loss for different horizons (RF)
lrf1c=abs(oosy-rf1c$pred)
lrf3c=abs(oosy-rf3c$pred)
lrf6c=abs(oosy-rf6c$pred)
lrf12c=abs(oosy-rf12c$pred)

#Compute squared loss for different horizons (AR)
lar1c=abs(oosy-ar1p$pred_actual)
lar3c=abs(oosy-ar3p$pred_actual)
lar6c=abs(oosy-ar6p$pred_actual)
lar12c=abs(oosy-ar12p$pred_actual)

#Compute loss differentials (d_t) for different horizons (AR-RF)
darrf1=lar1c-lrf1c
darrf3=lar3c-lrf3c
darrf6=lar6c-lrf6c
darrf12=lar12c-lrf12c

#Create ts object containing loss differentials
dtarrf.ts=ts(cbind(darrf1,darrf3,darrf6,darrf12), start=c(2007,1), end=c(2019,12), freq=12)
colnames(dtarrf.ts)=c("1-step dt","3-step dt","6-step dt","12-step dt")
#Plot them to examine stationarity:
plot.ts(dtarrf.ts, main="Loss differential AR-RF",cex.axis=1.8)


###PRediction Probabilities

#Compute squared loss for different horizons (RF)
plrf1c=(oosy-rf1c$pred)^2
plrf3c=(oosy-rf3c$pred)^2
plrf6c=(oosy-rf6c$pred)^2
plrf12c=(oosy-rf12c$pred)^2

#Compute squared loss for different horizons (AR)
plar1c=(oosy-ar1p$pred)^2
plar3c=(oosy-ar3p$pred)^2
plar6c=(oosy-ar6p$pred)^2
plar12c=(oosy-ar12p$pred)^2

#Compute loss differentials (d_t) for different horizons (AR-RF)
pdarrf1=plar1c-plrf1c
pdarrf3=plar3c-plrf3c
pdarrf6=plar6c-plrf6c
pdarrf12=plar12c-plrf12c

#Create ts object containing loss differentials
pdtarrf.ts=ts(cbind(pdarrf1,pdarrf3,pdarrf6,pdarrf12), start=c(2007,1), end=c(2019,12), freq=12)
colnames(pdtarrf.ts)=c("1-step dt","3-step dt","6-step dt","12-step dt")
#Plot them to examine stationarity:
plot.ts(pdtarrf.ts, main="Loss differential AR-RF - Probabilities",cex.axis=1.8)




###### PIECE ON 2020

Z = fredmd_new[-c(1,740),]
Z = Z %>%
  select_if(~ !any(is.na(.))) #Remove NA columns

Z_data = as.matrix(Z) #making into a matrix

nprev1 = 19 #Indices of 2019-2020

oosy_2020=tail(Z[,1],nprev1) ##Y values


## AR Forecasts

source("modified-func-ar.R")

ar1p_20=ar.rolling.window(Z_data,nprev1,1,1,type = "autoreg") #1-step AutoRegressive dynamic probit forecast
ar3p_20=ar.rolling.window(Z_data,nprev1,1,3,type = "autoreg") #3-step AutoRegressive dynamic probit forecast
ar6p_20=ar.rolling.window(Z_data,nprev1,1,6,type = "autoreg") #6-step AutoRegressive dynamic probit forecast
ar12p_20=ar.rolling.window(Z_data,nprev1,1,12,type = "autoreg") #12-step AutoRegressive dynamic probit forecast


##### Random Forest Prediction

source("modified-func-rf.R")

rf1c_20=rf.rolling.window(Z_data,nprev1,1,1)
rf3c_20=rf.rolling.window(Z_data,nprev1,1,3)
rf6c_20=rf.rolling.window(Z_data,nprev1,1,6)
rf12c_20=rf.rolling.window(Z_data,nprev1,1,12)


#Create the time series object collecting 1-step best=performing ML forecasts
ml1_20.ts=ts(cbind(oosy_2020,ar1p_20$pred,rf1c_20$pred_prob), start=c(2019,1), end=c(2020,7), freq=12)
colnames(ml1_20.ts)=c("True Value","AR Dynamic Probit","RF")

#Create the time series object collecting 3-step best=performing ML forecasts
ml3_20.ts=ts(cbind(oosy_2020,ar3p_20$pred,rf3c_20$pred_prob), start=c(2019,1), end=c(2020,7), freq=12)
colnames(ml3_20.ts)=c("True Value","AR Dynamic Probit","RF")

#Create the time series object collecting 6-step best=performing ML forecasts
ml6_20.ts=ts(cbind(oosy_2020,ar6p_20$pred,rf6c_20$pred_prob), start=c(2019,1), end=c(2020,7), freq=12)
colnames(ml6_20.ts)=c("True Value","AR Dynamic Probit","RF")

#Create the time series object collecting 12-step best=performing ML forecasts
ml12_20.ts=ts(cbind(oosy_2020,ar12p_20$pred,rf12c_20$pred_prob), start=c(2019,1), end=c(2020,7), freq=12)
colnames(ml12_20.ts)=c("True Value","AR Dynamic Probit","RF")

#Plot the graph for 1-step forecasts
#windows()
plot.ts(ml1_20.ts[,1], main="1-step ML forecasts 2019-2020", cex.axis=1.5, lwd=2, ylab="US Recession")
points(ml1_20.ts[,2], type="l", col="blue",lwd=1.8)
points(ml1_20.ts[,3], type="l", col="red",lwd=1.8)

legend("topleft", c("Recession","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","red"))

#Plot the graph for 3-step forecasts
#windows()
plot.ts(ml3_20.ts[,1], main="3-step ML forecasts 2019-2020", cex.axis=1.5, lwd=2, ylab="US Recession")
points(ml3_20.ts[,2], type="l", col="blue",lwd=1.8)
points(ml3_20.ts[,3], type="l", col="red",lwd=1.8)

legend("topleft", c("Recession","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","red"))

#Plot the graph for 6-step forecasts
#windows()
plot.ts(ml6_20.ts[,1], main="6-step ML forecasts 2019-2020", cex.axis=1.5, lwd=2, ylab="US Recession")
points(ml6_20.ts[,2], type="l", col="blue",lwd=1.8)
points(ml6_20.ts[,3], type="l", col="red",lwd=1.8)

legend("topleft", c("Recession","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","red"))

#Plot the graph for 12-step forecasts
#windows()
plot.ts(ml12_20.ts[,1], main="12-step ML forecasts 2019-2020", cex.axis=1.5, lwd=2, ylab="US Recession")
points(ml12_20.ts[,2], type="l", col="blue",lwd=1.8)
points(ml12_20.ts[,3], type="l", col="red",lwd=1.8)

legend("topleft", c("Recession","AR Dynamic Probit","RF"), lty=1 ,col=c("black","blue","red"))

#### Random Forest Variable Importance

imp1=matrix(NA,156,124)
imp3=matrix(NA,156,124)
imp6=matrix(NA,156,124)
imp12=matrix(NA,156,124)
            
for (i in 1:156) {
  imp1[i,] = rf1c$importance[[i]][,3]
  imp3[i,] = rf3c$importance[[i]][,3]
  imp6[i,] = rf6c$importance[[i]][,3]
  imp12[i,] = rf12c$importance[[i]][,3]
}

naming = c(colnames(Y_data),"Comp 1","Comp 2","Comp 3","Comp 4")

varimp1 = colMeans(imp1)
varimp3 = colMeans(imp3)
varimp6 = colMeans(imp6)
varimp12 = colMeans(imp12)

names(varimp1) = naming
names(varimp3) = naming
names(varimp6) = naming
names(varimp12) = naming

varimp1 = sort(varimp1, decreasing = TRUE)[1:5]
varimp3 = sort(varimp3, decreasing = TRUE)[1:5]
varimp6 = sort(varimp6, decreasing = TRUE)[1:5]
varimp12 = sort(varimp12, decreasing = TRUE)[1:5]

varimp1
varimp3
varimp6
varimp12

##GINI
gimp1=matrix(NA,156,124)
gimp3=matrix(NA,156,124)
gimp6=matrix(NA,156,124)
gimp12=matrix(NA,156,124)

for (i in 1:156) {
  gimp1[i,] = rf1c$importance[[i]][,4]
  gimp3[i,] = rf3c$importance[[i]][,4]
  gimp6[i,] = rf6c$importance[[i]][,4]
  gimp12[i,] = rf12c$importance[[i]][,4]
}


gvarimp1 = colMeans(gimp1)
gvarimp3 = colMeans(gimp3)
gvarimp6 = colMeans(gimp6)
gvarimp12 = colMeans(gimp12)

names(gvarimp1) = naming
names(gvarimp3) = naming
names(gvarimp6) = naming
names(gvarimp12) = naming

gvarimp1 = sort(gvarimp1, decreasing = TRUE)[1:5]
gvarimp3 = sort(gvarimp3, decreasing = TRUE)[1:5]
gvarimp6 = sort(gvarimp6, decreasing = TRUE)[1:5]
gvarimp12 = sort(gvarimp12, decreasing = TRUE)[1:5]

gvarimp1
gvarimp3
gvarimp6
gvarimp12

##What if we do not know if we are in Recession??

## DO for 1 and 3 period

source("modified-func-rf-unknown.R")

urf1c=urf.rolling.window(Y_data,nprev,1,1)
urf3c=urf.rolling.window(Y_data,nprev,1,3)


#Create the time series object collecting 1-step best=performing ML forecasts
uml1.ts=ts(cbind(oosy,rf1c$pred_prob,urf1c$pred_prob), start=c(2007,1), end=c(2019,12), freq=12)
colnames(uml1.ts)=c("True Value","RF - with lag","RF - without lag")

#Create the time series object collecting 1-step best=performing ML forecasts
uml3.ts=ts(cbind(oosy,rf3c$pred_prob,urf3c$pred_prob), start=c(2007,1), end=c(2019,12), freq=12)
colnames(uml3.ts)=c("True Value","RF - with lag","RF - without lag")

#Plot the graph for 1-step forecasts
#windows()
plot.ts(uml1.ts[,1], main="1-step ML forecasts", cex.axis=1.5, lwd=2, ylab="US Recession")
points(uml1.ts[,2], type="l", col="blue",lwd=1.8)
points(uml1.ts[,3], type="l", col="red",lwd=1.8)
legend("topright", c("Recession","RF - with lag","RF - without lag"), lty=1 ,col=c("black","blue","red"))

#Plot the graph for 3-step forecasts
#windows()
plot.ts(uml3.ts[,1], main="3-step ML forecasts", cex.axis=1.5, lwd=2, ylab="US Recession")
points(uml3.ts[,2], type="l", col="blue",lwd=1.8)
points(uml3.ts[,3], type="l", col="red",lwd=1.8)
legend("topright", c("Recession","RF - with lag","RF - without lag"), lty=1 ,col=c("black","blue","red"))

uimp1=matrix(NA,156,119)
uimp3=matrix(NA,156,119)

for (i in 1:156) {
  uimp1[i,] = urf1c$importance[[i]][,3]
  uimp3[i,] = urf3c$importance[[i]][,3]
}

naming2 = colnames(Y_data)[-1]

varimp1u = colMeans(uimp1)
varimp3u = colMeans(uimp3)


names(varimp1u) = naming2
names(varimp3u) = naming2

varimp1u = sort(varimp1u, decreasing = TRUE)[1:5]
varimp3u = sort(varimp3u, decreasing = TRUE)[1:5]

varimp1u
varimp3u

urf1c$errors
urf3c$errors

#Save results to workspace file for later use:
save.image("project_results.RData")

