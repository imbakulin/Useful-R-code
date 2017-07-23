library("ghyp")
library("xts")
#install.packages("forecast")
library("forecast")
install.packages("MASS")
library("MASS")
setwd("C:/Users/User/Downloads")

data <- read.csv("CBOM_160101_171211.csv", sep = ";")
data$X.DATE. <- as.Date(as.character(data$X.DATE.), format = "%Y%m%d")
number.of.candels <- as.vector(table(as.vector(data$X.DATE.)))
number.of.candels <- number.of.candels[!number.of.candels %in% boxplot.stats(number.of.candels)$out]

plot(number.of.candels, type = "l")
lines(fitted(X), col = "red")
lines(predict(X, 67)$pred, col = "blue")
Poisson.distr <- fitdistr(number.of.candels[length(number.of.candels)-254:length(number.of.candels)], densfun="poisson")



data <- xts(data$X.CLOSE., order.by = data$X.DATE.)
returnes <- (data/lag(data)-1)[-1]
returnes <- returnes[returnes != 0]

plot(density(number.of.candels[((length(number.of.candels))-250):(length(number.of.candels)-200)]))
lines(density(rpois(500,Poisson.distr$estimate)))

aiccandles <- stepAIC.ghyp(number.of.candels, silent = TRUE)
aic <- stepAIC.ghyp(returnes, dist=c("ghyp", "hyp","t", "gauss"), silent=TRUE, symmetric = F)
aic$best.model
aic$fit.table
hist(aic$best.model)
aiccandles$fit.table
par(mfrow = c(1,2))
hist(aiccandles$best.model)
hist(number.of.candels, breaks = 20)

mat <- matrix(nrow = 17, ncol = 15)
for (i in 1:17) mat[i,] <- number.of.candels[(15*(i-1)+1):(15*i)]
plot(as.data.frame(mat))

plot_ly(z = mat) %>% add_surface()

par(mfrow=c(3,6))
for (i in 1:17) plot(density(mat[i,]), pch = i)
plot(density(mat[i,]))

prices <- as.vector(data)
n <- length(data)
number.of.days <- 31
MC.returnes <-  rghyp(number.of.days, object = aic$all.models[[1]])


for (i in 1:number.of.days){
  prices[[n+i]] <- round((as.numeric(MC.returnes[i])+1)*as.numeric(prices[n+i-1]), digits = 1)
}


# Simulator

Pt <- NULL
Total.PnL <- NULL
V <- NULL
PnL <- NULL
for(j in 1:100000){
  MC.number.of.candels <- sum(rghyp(number.of.days, object = aiccandles$all.models[[1]]))
  MC.returnes <-  rghyp(MC.number.of.candels, object = aic$all.models[[1]])
  Pt[1] <- as.vector(data[length(data)])
  V[1] <- 5
  PnL[1] <- -V[1]*Pt[1]
  spread <- 0.002
  
  for(i in 2:number.of.days){
    V[i] <- V[i-1]
    Pt[i] <- Pt[i-1]*(1+MC.returnes[i])
    PnL[i] <- PnL[i-1]
    
    if(V[i] == 0){ 
      PnL[i] <- PnL[i-1] - Pt[i]
      V[i] <- V[i] + 1
    } else {
      PnL[i] <- PnL[i-1]
    }
    
    if (MC.returnes[i] > spread){
      V[i] <- V[i]-1
      PnL[i] <- PnL[i] + Pt[i]
    }
    
    if (MC.returnes[i] < -spread){
      V[i] <- V[i]+1
      PnL[i] <- PnL[i] - Pt[i]
    }
  }
  Total.PnL[j] <- PnL[number.of.days] + V[number.of.days]*Pt[number.of.days]
}

hist(Total.PnL, breaks = 50)
mean(Total.PnL)
density(Total.PnL)


sort(Total.PnL)[length(Total.PnL)*0.05]


plot(density(Total.PnL, type = "l"))

plot(prices[(n-1000):(n+number.of.days)], type = "l")
