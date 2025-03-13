
#Inputs for the function:

#1) Data matrix Y: includes all variables

#2) indice - index for dependent variable: 1 for CPI inflation, 2 for PCE inflation

#3) lag - the forecast horizon

#4) type -  "fixed" will use the AR(4) in all cases; "bic" will select lags using BIC

runAR=function(Y_data,indice,lag,type="autoreg"){
  
  Y = Y_data[,c("USREC", "IS")]
  aux=embed(Y,1+lag)
  y= aux[,1]
  
  X=as.matrix(aux[,-c(1:(ncol(Y)*lag))]) # lags of Y (predictors) corresponding to forecast horizon to be removed 
  if(lag==1){ 
    X.out=tail(aux,1)[1:ncol(X)] #retrieve 4 last observations if one-step forecast 
  }else{
    X.out=aux[,-c(1:(ncol(Y)*(lag-1)))] #delete first (h-1) columns of aux,  
    X.out=tail(X.out,1)[1:ncol(X)] #last observations: y_T,y_t-1...y_t-h
  }
  model = glm(y~X,family=binomial(link="probit")) 
  coef = model$coefficients
  
  pred = pnorm(c(1,X.out)%*%coef) #make a forecast using the last few observations: a direct h-step forecast
  
  pred_actual = ifelse(pred>0.5,1,0)
  
  return(list("model"=model,"pred"=pred,"pred_actual"=pred_actual,"coef"=coef)) #save estimated AR regression, prediction, and estimated coefficients
}



ar.rolling.window=function(Y,nprev,indice=1,lag=1,type = "autoreg"){
  
  save.coef=matrix(NA,nprev,3) #blank matrix for coefficients at each iteration
  save.pred=matrix(NA,nprev,1) #blank for forecasts
  save.pred_actual=matrix(NA,nprev,1) #blank for forecasts
  for(i in nprev:1){  #NB: backwards FOR loop: going from 180 down to 1
    
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    fact=runAR(Y.window,indice,lag) #call the function to fit the AR(p) selected on BIC and generate h-step forecast
    save.coef[(1+nprev-i),]=fact$coef #save estimated coefficients
    save.pred[(1+nprev-i),]=fact$pred #save the forecast
    save.pred_actual[(1+nprev-i),]=fact$pred_actual
    cat("iteration",(1+nprev-i),"\n") #display iteration number
  }
  
  #Some helpful stuff:
  real=Y[,indice] #get actual values
  plot(real,type="l",main = paste("AutoReg Dynamic Probit:",lag,"- step forecast"))
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red")
  lines(c(rep(NA,length(real)-nprev),save.pred_actual),col="blue") 
  legend("topleft",legend=c("Actual","AR Pred:Prob", "AR:Prediction"), col = c("black","red","blue"), lty = 1)
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2)) #compute RMSE
  mae=mean(abs(tail(real,nprev)-save.pred)) #compute MAE (Mean Absolute Error)
  
  rmse_1=sqrt(mean((tail(real,nprev)-save.pred_actual)^2)) #compute RMSE
  mae_1=mean(abs(tail(real,nprev)-save.pred_actual)) #compute MAE (Mean Absolute Error)
  
  errors=c("rmse_pred"=rmse_1,"misclass%_pred"=mae_1,"rmse_prob"=rmse,"misclass%_prob"=mae) #stack errors in a vector
  
  return(list("pred"=save.pred,"pred_actual" = save.pred_actual,"errors"=errors)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
}

