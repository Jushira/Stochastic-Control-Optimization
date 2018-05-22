library(quadprog)

func <- function(L){ # function for num of machines produced given a level of L
  K = (100000 - 12*L)/15
  num_machines = 0.05*L^(2/3)*K^(1/3)
  return(-num_machines)
}
S = optim(2000, func, method="BFGS")
S


#Question 2
Stocks <- read.csv("homework4stocks.csv")
MeanReturns = colMeans(Stocks[,2:ncol(Stocks)])
VarReturns = apply(Stocks[,2:ncol(Stocks)], 2, var)
StdDev = sqrt(VarReturns)

#For the new portfolio
Correlation = cor(Stocks[,2:ncol(Stocks)], use="pairwise.complete.obs")
CovMatrix = diag(StdDev) %*% Correlation %*% diag(StdDev)

#Arguments for solve.QP
Dmat = 2*CovMatrix
dvec = rep(0,27)
Amat = matrix(c(rep(1,27),rep(-1,27),MeanReturns),27)
bvec = c(1,-1,0.01)

#Using Solve.QP
Sol=solve.QP(Dmat,dvec,Amat,bvec)
Sol
sum(Sol$solution * MeanReturns)
sum(Sol$solution * VarReturns)
sum(Sol$solution * sqrt(VarReturns))

#Question 3
Variables <- read.csv("variable_selection.csv")

lm1 = lm(Variables$y ~ Variables$x1)
lm2 = lm(Variables$y ~ Variables$x2)
lm3 = lm(Variables$y ~ Variables$x3)
lm4 = lm(Variables$y ~ Variables$x1 + Variables$x2)
lm5 = lm(Variables$y ~ Variables$x2 + Variables$x3)
lm6 = lm(Variables$y ~ Variables$x3 + Variables$x1)

sum(resid(lm1)^2)
sum(resid(lm2)^2)
sum(resid(lm3)^2)
sum(resid(lm4)^2)
sum(resid(lm5)^2)
sum(resid(lm6)^2)

#Question 4
Dmat = diag(5)
Dmat[2,2] = 3
Dmat[3,3] = 4
Dmat[4,4] = 6
Dmat[5,5] = 12 
dvec = rep(0,5)
Amat = matrix(c(1,0,1,0,0,-1,0,0,1,1,0,1,-1,-1,0),5,3)
bvec = c(710,0,0)
S=solve.QP(Dmat,dvec,Amat,bvec)
S
S$solution
S$value
S$unconstrained.solution
S$iterations
S$Lagrangian
S$iact

#Question 5
nfl <- read.csv('nflratings.csv')
head(nfl)

nfl$actual_point_spread = nfl$X13 - nfl$X10
func <- function(R){
  pred_error = 0
  for (i in 1:nrow(nfl)) {
    pred_error = pred_error + (nfl$actual_point_spread[i] - (R[nfl[i,2]] - R[nfl[i,3]] + R[33]))^2
  }
  return(pred_error)
}
S = optim(rep(0, 33), func, method="BFGS")
Sol = S$par[1:32] + (85 - mean(S$par[1:32]))
Sol = c(Sol, S$par[33])
mean(Sol[1:32])
Sol
