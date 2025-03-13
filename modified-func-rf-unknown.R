

#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#3) lag - the forecast horizon

urunrf=function(Y,indice,lag){
  
  
  aux=embed(Y,1+lag) #create 4 lags + forecast horizon shift (=lag option)
  y=as.factor(aux[,indice]) #  Y variable aligned/adjusted for missing data due do lags
  aux1 = aux[,-indice]
  X=aux1[,-c(1:(ncol(Y)*lag))]  # lags of Y (predictors) corresponding to forecast horizon 
  
  if(lag==1){
    X.out=tail(aux1,1)[1:ncol(X)]   #retrieve the last  observations if one-step forecast
  }else{
    X.out=aux1[,-c(1:(ncol(Y)*(lag-1)))] #delete first (h-1) columns of aux,
    X.out=tail(X.out,1)[1:ncol(X)]  #last observations: y_T,y_t-1...y_t-h
  }
  
  model=randomForest(X,y,importance = TRUE) #fit the random forest on default settings
  pred = as.numeric(predict(model,X.out)) - 1
  pred_prob= predict(model,X.out,type = "prob")[2] #generate forecast
  
  return(list("model"=model,"pred"=pred,"pred_prob"=pred_prob)) #return the estimated model and h-step forecast
}


#This function will repeatedly call the previous function in the rolling window h-step forecasting

#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) nprev - number of out-of-sample observations (at the end of the sample)

#3) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#4) lag - the forecast horizon

urf.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  save.importance=list() #blank for saving variable importance
  save.pred=matrix(NA,nprev,1) ##blank for forecasts
  save.pred_prob=matrix(NA,nprev,1) ##blank for forecasts
  for(i in nprev:1){#NB: backwards FOR loop: going from 180 down to 1
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    rf=urunrf(Y.window,indice,lag)#call the function to fit the Random Forest and generate h-step forecast
    save.pred[(1+nprev-i),]=rf$pred #save the forecast
    save.pred_prob[(1+nprev-i),]=rf$pred_prob
    save.importance[[i]]=importance(rf$model) #save variable importance
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=Y[,indice]#get actual values
  plot(real,type="l",main = paste("Random Forest:",lag,"- step forecast"))
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red") 
  lines(c(rep(NA,length(real)-nprev),save.pred_prob),col="blue") 
  legend("topleft",legend=c("Actual","RF Pred","RF Pred:Prob"), col = c("black","red","blue"), lty = 1)
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  
  rmse_1=sqrt(mean((tail(real,nprev)-save.pred_prob)^2)) #compute RMSE
  mae_1=mean(abs(tail(real,nprev)-save.pred_prob)) #compute MAE (Mean Absolute Error)
  
  errors=c("rmse_pred"=rmse,"misclass%_pred"=mae,"rmse_prob"=rmse_1,"misclass%_prob"=mae_1) #stack errors in a vector
  
  return(list("pred"=save.pred,"pred_prob" = save.pred_prob,"errors"=errors,"importance"=save.importance)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
}
