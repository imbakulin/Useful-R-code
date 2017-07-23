data <- read.csv("DFORequestTable1.csv", sep = ";")

 <- merge(data[,1:21], data[,22], all = TRUE)

changes <- NULL
for(i in 1:21){
changes[i] <- length(setdiff(data[,i+1],data[,i]))
}
changes
t <- c(1996:2016)
plot(y=changes, x = t, type = "lines")

library('quantmod')
getSymbols("SPR", from = as.Date("31-12-1994"), to = as.Date("31-12-1997"), src = "google")


first <- NULL
second <- NULL
dual <- NULL
Biesless.set <- list()
Gelman.set <- NULL
Reality.lose <- NULL
Reality.surv <- NULL
Gelman.set.dynamic <- list()
##
for(i in 1:21){
  first[i] <- length(setdiff(data[,i+1],data[,1]))
}
##
for(i in 2:21){
  second[i] <- length(setdiff(data[,i+1],data[,2]))/length(data[,1])
}
##

for (j in 3:22){
Gelman.set.dynamic[[j]] <- union(data[,1], data[,j])
}
dual <- matrix(nrow = 22-3+1, ncol = 22)

for(j in 3:22){
for(i in 1:22){
  dual[j-2,i] <- length(setdiff(data[,i],Gelman.set.dynamic[[j]]))
}
}

install.packages("plotly")
library("plotly")
plot_ly(z = dual, type = "surface") %>% layout(xaxis = list(title= "point of right cut"), yaxis = list(title = "time"))
planes3d(dual)
persp(dual)

##
Biesless.set[[1]]<-data[,1]


for(i in 2:21){
  Biesless.set[[i]] <- union(data[,i],Biesless.set[[i-1]])
}

Gelman.set <- union(data[,1], data[,22])

for(i in 1:21){
  Reality.lose[i] <- length(setdiff(data[,i],Gelman.set))
}


for(i in 1:21){
  Reality.surv[i] <- length(setdiff(Gelman.set, Biesless.set[[i]]))
}


plot(Reality.surv, type = "line")
plot(y = Reality.lose, x = t, type = "lines")


plot(dual, type = "lines")
plot(first, x = t,type = "lines")
lines(second)
max(Reality.lose)
