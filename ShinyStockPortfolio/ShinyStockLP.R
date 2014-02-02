rm(list=ls(all=TRUE))
setwd("/home/dev/work/Insofe/Mini-Project-2/ShinyStockPortfolio")

#Define Risk Profiles in the order Lo, Med, Hi risk distr

profile.conservative <- c(0.5, 0.3, 0.2)
profile.moderate <- c(0.3,0.4,0.3)
profile.aggressive <- c(0.2,0.3,0.5)

profile.sel <- profile.aggressive

#Read the returns data from returns file
stock.returns <- read.csv("StockReturns.csv")
colnames(stock.returns)[1] <- "Name"
stock.returns

#Select any 5 stocks
num.stocks <- 5
sel.stocks <- stock.returns[sample(1:nrow(stock.returns),num.stocks,replace=FALSE),]

#Order the data based on Risk
sel.stocks <- sel.stocks[order(sel.stocks$SD),]
sel.stocks

#Bin the Risk
library(infotheo)
risk.level <- discretize(sel.stocks$SD,disc="equalfreq", nbins=3)
colnames(risk.level) <- "RiskLevel"
sel.stocks$RiskLevel <- risk.level
head(sel.stocks)

#Define objective functions
obj <- sel.stocks$Mean

#Define constraints
w1 <- c(1,0,0,0,0) 
w2 <- c(0,1,0,0,0) 
w3 <- c(0,0,1,0,0) 
w4 <- c(0,0,0,1,0) 
w5 <- c(0,0,0,0,1) 
weights <- data.frame(cbind(w1,w2,w3,w4,w5))
sum.weights <- apply(weights,1,sum)

min.weight <- 0.05 # Minimum investment in each stock

#Low Risk
lo.risk <- apply(weights[which(sel.stocks$RiskLevel==1)],1,sum)
#Med Risk
med.risk <- apply(weights[which(sel.stocks$RiskLevel==2)],1,sum)
#Hi Risk
hi.risk <- apply(weights[which(sel.stocks$RiskLevel==3)],1,sum) 

cons <- rbind(w1, w2, w3, w4, w5, sum.weights, lo.risk, med.risk, hi.risk)
dir <- c(rep(">=", 5), "=",  "<=", "<=", "<=")
rhs <- c(rep(min.weight,5), 1, profile.sel[1], profile.sel[2], profile.sel[3])
library(lpSolve)
res <- lp("max", obj, cons, dir, rhs, compute.sens=0)
if (sum(res$solution) == 0) {
  res
} else {
  output <- cbind(sel.stocks,res$solution)
  cat("Invest as follows: \n")
  output
}
