library(datasets)
ozone <- airquality$Ozone
rad <- airquality$Solar.R
rem <- is.na(ozone) | is.na(rad)
ozone <- ozone[!rem]; rad <- rad[!rem]
# разделим выборку на обучающую и экзаменующую
N <- length(ozone); E <- 20; T <- N-E
train.obs <- (1:T)
eval.obs <- (T+1):N
t.rad <- rad[train.obs]; t.ozone <- ozone[train.obs]
e.rad <- rad[eval.obs]; e.ozone <- ozone[eval.obs]

fit.par$coefficients[1]

fit.par <- lm(ozone ~ radiation,
              data=data.frame(radiation=t.rad,ozone=t.ozone),
              weights=NULL)

# другой вариант
fit.par <- lm( ~ t.rad)

qt(0.05/2,df=100)



#install.packages("tseries")
library("tseries")

data <- ozone
newdata <- rad

model$coef
ar1 <- arma(log(ozone), order = c(1,0))
model <- ar1
model$coef[2]

model$fitted.values[length(data)]
model$coef[1]
fit <- model$fitted.values+model$coef[1]

                           
                           

plot(ozone, type = "l")
lines (exp(model$fitted.values), col = "blue")

model$residuals


(t(as.matrix(na.remove(model$residuals)))%*%as.matrix(na.remove(model$residuals)))


model$n.used
line()predict.arma <- function(model, data, newdata, alpha = 0.05) {
  
  
  
  
  
  df <- t - length(data)
  model <- arma(c(data, newdata), c(1,1))
  sigma.squared <- t(model$residuals)%*%model$residuals
  list(fit = fit, lower = fit + delta*qt(alpha/2,df=df),
       upper = fit - delta*qt(alpha/2,df=df))
  
}
ar1.frc <- predict.arma(model = ar1,
                        data = log(t.ozone)[1:(T-1)],
                        newdata = log(e.ozone), alpha = 0.1)
par.frc <- predict(fit.par,
                   newdata=data.frame(rad=e.rad[2:E],rad2=e.rad[2:E]^2),
                   se.fit=TRUE,interval="prediction",level=0.90)

frc <- exp(ar1.frc$fit[2:E] + par.frc$fit[,"fit"])
