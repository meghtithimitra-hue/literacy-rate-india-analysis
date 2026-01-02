 Reg.mat.pro <- read.delim("C:/Users/ADMIN/Desktop/Reg mat pro.txt")
 View(Reg.mat.pro)
 model=lm(Literacy.Rate ~ Sex.Ratio + Population.Density + Poverty.Rate + Unemployment.Rate, data= Reg.mat.pro)
 model
 summary(model)
 par(mfrow=c(2,2))
 plot(model)
 install.packages("MASS") 
 library(MASS)             
 bc= boxcox(model)
 lambda=bc$x[which(bc$y==max(bc$y))]
 lambda
 model2=lm((Literacy.Rate^lambda) ~ Sex.Ratio + Population.Density + Poverty.Rate + Unemployment.Rate, data= Reg.mat.pro)
 model2
 summary(model2)
 plot(model2)
 mae=mean(abs(model2$residuals))
 mse=mean((model2$residuals)^2)
 rmse=sqrt(mse)
 mape=mean(abs(model2$residuals)/(model2$residuals+model2$fitted.values))*100
 cat("Mean Absolute Error(MAE):",mae,"\n")
 cat("Mean Square Error(MSE):",mse,"\n")
 cat("Root Mean Square Error(RMSE):",rmse,"\n")
 cat("Mean Absolute Percentage Error(MAPE):",mape,"\n")