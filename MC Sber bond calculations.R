# Monte-Carlo simulator

# Upload required libraries
library("ghyp")
library("xts")
library("forecast")
library("MASS")
library("bizdays")
library("RQuantLib")
library("TTR")
# Setting working directory
setwd("C:/Users/User/Desktop")


# Uploading data
data <- read.csv("USD000000TOD_150401_161216 (1).csv", sep = ";")
data$X.DATE. <- as.Date(as.character(data$X.DATE.), format = "%Y%m%d")
data <- xts(data$X.CLOSE., order.by = data$X.DATE.)

# Load calendar
load_quantlib_calendars(ql_calendars = "Russia", from = "2016-12-16", to = "2017-12-21")


# Creating number of candels (for not absolutely liquid stocks only)
# number.of.candels <- as.vector(table(as.vector(data$X.DATE.)))
# plot(number.of.candels, type = "l")
# lines(fitted(X), col = "red")
# lines(predict(X, 67)$pred, col = "blue")
# Poisson.distr <- fitdistr(number.of.candels[(length(number.of.candels)-125):length(number.of.candels)], densfun="poisson")




# Fit returnes distribution
number.of.days <- 365
prices <- data[(index(data[length(data)])-index(data) <= number.of.days)]
returnes <- log(prices/lag(prices))[-1]
prices <- prices[-1]

aic <- stepAIC.ghyp(returnes, silent=TRUE)

aic$fit.table


hist(aic$best.model, breaks = 80)



# prices <- as.vector(data)
# n <- length(data)
# number.of.days <- 182
# MC.returnes <-  rghyp(67, object = aic$all.models[[3]])




# Simulator

Total.PnL <- NULL

vol <- sd(as.vector(returnes[(index(returnes[length(returnes)])-index(returnes) <= 182)]))*sqrt(182*8)

n <- length(prices)
MC.prices <- NULL
NewValue <- NULL

dates <- as.Date(bizseq("2016-12-16", "2017-06-16", cal = "QuantLib/Russia"))

Realizations <- NULL
DeltaRealization <- NULL

for(j in 1:1500){
    MC.prices <- prices
    MC.returnes <- returnes
    for(i in 1:length(dates)){
      for (k in 1:8){
        NewReturn <- xts(as.numeric(rghyp(1, object = aic$best.model)),  order.by = dates[i])
        NewPrice <- xts(x=(as.numeric(NewReturn)+1)*as.numeric(MC.prices[n+(i-1)*8+k-1]), order.by = dates[i])
        #prices[[n+(i-1)*8+k]]<- xts(x=(as.numeric(MC.returnes)+1)*as.numeric(prices[n+(i-1)*8+k-1]), order.by = dates[i])
        MC.prices <- rbind(MC.prices, NewPrice)
        MC.returnes <- rbind(MC.returnes, NewReturn)
      }
    }
    # plot(runSD(MC.returnes, n = 8*21)*sqrt(252*8))
    # plot(MC.returnes, type = "l")
    BSd <- function(S, K, r, q, sigma, Tau){
      d1 <- (log(S/K)+(r+q+sigma^2/2)*Tau)/(sigma*sqrt(Tau))
      d2 <- d1 - sigma*sqrt(Tau)
      return(list(d1 = d1,d2 = d2))
    }
    
    # --- Parameters of d
    sigma <- runSD(MC.returnes, n = 8*21)*sqrt(252*8)
    Tau <- cbind(MC.returnes = MC.returnes, days = bizdays(index(MC.returnes), index(MC.returnes[length(MC.returnes)]))/365)[,2]
    r <- 0.08
    q <- 0.074
    K.left <- 65.00
    K.right <- 72.07
    d1l <- BSd(S = MC.prices, K = K.left, r = r, q = q, sigma = sigma, Tau = Tau)$d1
    d1r <- BSd(MC.prices, K= K.right, r, q, sigma = sigma, Tau = Tau)$d1
    d2l <- BSd(S = MC.prices, K = K.left, r = r, q = q, sigma = sigma, Tau = Tau)$d2
    d2r <- BSd(MC.prices, K= K.right, r, q, sigma = sigma, Tau = Tau)$d2
    
    alpha <- 15.40323
    beta <- 108.9008
    I <- 1000
    
    delta <- -alpha*exp(-q*Tau)*(pnorm(d1r)-pnorm(d1l))-beta*exp(-q*Tau)*dnorm(d1r)/(MC.prices*sigma*sqrt(Tau))
    PriceOfBond <- I/(1 + r*Tau) - beta*exp(-r * Tau) * pnorm(d2r) - alpha * (exp(-q * Tau) * MC.prices * (pnorm(d1r) -
                                                  pnorm(d1l)) - exp(-r * Tau) * (K.right * pnorm(d2r) - K.left * pnorm(d2l)))
    OurSample <- subset(delta, index(delta)>as.Date("2016-12-16") & index(delta) < as.Date("2017-06-16"))
    Bond <- subset(PriceOfBond/I, index(delta)>as.Date("2016-12-16") & index(delta) < as.Date("2017-06-16"))
    
    Realizations <- cbind(Realizations, Bond)
    DeltaRealization <- cbind(DeltaRealization, OurSample)
}

    MeanPrice <- cbind(rowMeans(Realizations), OurSample)
    MeanDelta <- cbind(rowMeans(DeltaRealization), OurSample)
    plot(Bond)
    
    TauMassive <- 182:0/365
    StockMassive <- 40:90
    PriceOfBondMassive <- merge(TauMassive, StockMassive)
    
    
    PriceOfBondMassive <- data.frame()

    for(i in 1:length(TauMassive)){
      for(j in 1:length(StockMassive)){
        d1lm <- BSd(S = StockMassive[j], K = K.left, r = r, q = q, sigma = 0.15, Tau = TauMassive[i])$d1
        d1rm <- BSd(S = StockMassive[j], K= K.right, r, q, sigma = 0.15, Tau = TauMassive[i])$d1
        d2lm <- BSd(S = StockMassive[j], K = K.left, r = r, q = q, sigma = 0.15, Tau = TauMassive[i])$d2
        d2rm <- BSd(S = StockMassive[j], K= K.right, r, q, sigma = 0.15, Tau = TauMassive[i])$d2
        PriceOfBondMassive[i,j] <- I/(1 + r*TauMassive[i]) - beta*exp(-r * TauMassive[i]) * pnorm(d2rm) - 
          alpha * (exp(-q * TauMassive[i]) * StockMassive[j] * (pnorm(d1rm) -
          pnorm(d1lm)) - exp(-r * TauMassive[i]) * (K.right * pnorm(d2rm) - K.left * pnorm(d2lm)))
      }
    }
    
library("plot3D")
    install.packages("plot3D")
    plot_ly(z = as.matrix(PriceOfBondMassive), x = StockMassive) %>% add_surface()
p <- surf3D(TauMassive, StockMassive, as.matrix(PriceOfBondMassive)) 
P    <- surf3D(TauMassive, StockMassive, outer(TauMassive, StockMassive, FUN="PriceOfBondMassive"))
    
    plot(MeanDelta[,1])
    plot(subset(MC.prices, index(MC.prices)>as.Date("2016-12-16") & index(delta) < as.Date("2017-06-16")))
    
    
    

    
    
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

hist(Total.PnL)
mean(Total.PnL)*70000




plot(PnL, type = "l")

plot(prices[(n-1000):(n+number.of.days)], type = "l")
