#LIBRARIES
library(TSA)
library(forecast)
library(foreach)
library(doSNOW)
library(doParallel)
library(plyr)

#Custom Function for fitting automatically chossing the best model between ARIMA and ETS
fitting.function <- function(x, h){
  temp.mod.arima <- auto.arima(x, seasonal = TRUE)
  temp.mod.ets <- ets(x)
  if((temp.mod.arima$aic)-(temp.mod.ets$aic) > 0){
    temp.mod <- temp.mod.ets
  }
  else{
    temp.mod <- temp.mod.arima
  }
  return(temp.mod)
}

forecast.function <- function(x,h){
  temp1 <- fitting.function(x,h)
  if(class(temp1)[1]=="ARIMA"){
    temp2 <- forecast(temp1)
    return(temp2$mean[1,h])
  }
  else {
    temp2 <- predict(temp1, n.ahead=h)
    return(as.vector(temp2$mean)[1:h])
  }
}

#importing data
df <- read.table("parts_usage.csv", header = TRUE, sep=",", check.names=FALSE)
df[is.na(df)] <- 0
df$month <- NULL
df <- df[, colSums(df !=0) > 0]
n <- ncol(df)
h <- 3

df <- ts(df, start=c(2012,8), end=c(2017, 7), frequency = 12)

forecast.df <- matrix(NA, ncol=n, nrow=h)

cores=detectCores()
cl <- make.Cluster(cores-1)
registerDoParallel(cl)
strt <- Sys.time()

forecast.df <- foreach(i=1:n) %dopar% forecast.function(df[,i],h)

print(Sys.time()-strt)

final <- data.frame(matric(unlist(forecast.df), nrow=n, byrow=TRUE))

final <- rename(final, c("X1"="August 2017", "X2"="September 2017", "X3"="October 2017"))
names <- colnames(df[,1:n])
row.name(final) <- names
write.csv(final, file="forecast.csv")