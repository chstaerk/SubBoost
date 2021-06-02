source("AdaSubBoost_main_functions_current.R")

RNGkind("L'Ecuyer-CMRG")


########### Example ###############

n = 100 
n_test = 1000 
p = 20 
s0 = 4
S0 = c(1:4) 
beta = numeric(p)
beta[S0] =  c(-2,-1,1,2)  
sigma.normal = 1
corr = 0.8 

set.seed(12)
data = simdata.toeplitz.corr(n =n ,p =p ,beta =beta ,sigma.normal = sigma.normal,corr=corr) 
data.test = simdata.toeplitz.corr(n_test,p,beta=beta,sigma.normal= sigma.normal,corr= corr)

data$y = data$y - mean(data$y)
data$x = scale(data$x)

colnames(data$x) = c("X1","X2","X3","X4",rep("",p-s0))

#################################################

K = 100
q = 10 
tau = 0.01 # 0.01
tau_L2Boost = 0.1
criterion = "EBIC"
Iter = 1000
automatic.stopping = FALSE

const = 0 # 1 # gamma (Penalty in EBIC)

PFER = 10

savings = 1

size.fixed = 2  # NULL

update = "S" 
conservative = TRUE

# AdaSubBoost 
set.seed(12)
start.time <- Sys.time()
output = AdaSubBoost(data=data, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=20, savings=savings,
                         conservative = conservative, update=update, automatic.stopping = automatic.stopping)
end.time <- Sys.time()
timeAdaSubBoost = as.double(end.time-start.time,units = "secs")
timeAdaSubBoost
AdaSubBoost_beta = output$beta.cur
AdaSubBoost_model = which(AdaSubBoost_beta[-1]!=0)
AdaSubBoost_model
S0

# RSubBoost (without adapting probabilities, adaptive=FALSE)
set.seed(12)
start.time <- Sys.time()
outputRSub = AdaSubBoost(data=data, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=20, savings=savings,
                         conservative = TRUE, update=update, adaptive=FALSE, automatic.stopping = automatic.stopping)
end.time <- Sys.time()
timeRSubBoost = as.double(end.time-start.time,units = "secs")
timeRSubBoost
RSubBoost_beta = outputRSub$beta.cur
RSubBoost_model = which(RSubBoost_beta[-1]!=0)
RSubBoost_model
S0



# SubBoost (with full enumeration)
if (p<=30){
start.time <- Sys.time()
outputSub = SubBoost(data=data,Iter=Iter,size.fixed=size.fixed, tau = tau, const=const, savings=1, automatic.stopping = automatic.stopping)
end.time <- Sys.time()
timeSubBoost = as.double(end.time-start.time,units = "secs")
timeSubBoost
Sub_beta = outputSub$beta.cur
Sub_model = which(Sub_beta[-1]!=0)
Sub_model
S0
}

# L2Boosting
set.seed(12)
mstop.max = 1000
mod <- glmboost(y = data$y, x = data$x, control = boost_control(mstop = mstop.max, nu = tau_L2Boost)) ### mstop needs tuning
cv10f <- cv(model.weights(mod), type = "kfold")
cvm <-cvrisk(mod, papply = lapply,  folds = cv10f)
m.stop = mstop(cvm)
mod.boost = mod[m.stop]
Boost_model = which(coef(mod.boost, which=1:p)!=0)
length(Boost_model) - s0 #number of false positives

# Selected models
Boost_model
Sub_model
RSubBoost_model
AdaSubBoost_model

# Convergence check 
nstop = p/size.fixed

for (i in nstop:length(outputRSub$A.list)) {
  if (all(sapply(outputRSub$A.list,length)[(i-nstop+1):i] == 0 )) {
    RSubBoost_stop = i 
    break
  }
}

for (i in nstop:length(output$A.list)) {
  if (all(sapply(output$A.list,length)[(i-nstop+1):i] == 0 )) {
    AdaSubBoost_stop = i 
    break
  }
}

SubBoost_stop <- min(which(sapply(outputSub$A.list,length) == 0))

SubBoost_stop
RSubBoost_stop
AdaSubBoost_stop
 
####### Plots 

cex.size = 1.4
cex.lab.size = 1.2
cex.axis.size = 1.1
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size)
if (p<=30) {
  par(mfrow=c(4,1),las=1)
} else {
  par(mfrow=c(3,1),las=1)
}


plot(mod[mstop.max], main="L2Boosting",ylim=c(-3,3))
abline(v=m.stop, col="red") # ,lty="dotted")
for (i in S0) abline(h = beta[i], col="black",lty="dotted")

if (p<=30) {
  matplot(t(outputSub$beta.mat[Sub_model+1,,drop=F]),type="l",xlab="Number of boosting iterations",ylab="Coefficients",ylim=c(-3,3),col="red",lty="solid", main="SubBoost")
  for (i in S0) abline(h = beta[i], col="black",lty="dotted")
  axis(side=4, at=outputSub$beta.cur[Sub_model+1], labels=c("X1","X2","X3","X4")) 
  abline(h=0,col="gray")
  abline(v=SubBoost_stop, col="red") # ,lty="dotted")
  
}

matplot(t(outputRSub$beta.mat[RSubBoost_model+1,,drop=F]),type="l",xlab="Number of boosting iterations",ylab="Coefficients",ylim=c(-3,3),col="red",lty="solid", main="RSubBoost")
for (i in S0) abline(h = beta[i], col="black",lty="dotted")
axis(side=4, at=outputRSub$beta.cur[RSubBoost_model+1], labels=c("X1","X2","X3","X4")) 
abline(h=0,col="gray")
abline(v=RSubBoost_stop, col="red") # ,lty="dotted")


matplot(t(output$beta.mat[AdaSubBoost_model+1,,drop=F]),type="l",xlab="Number of boosting iterations",ylab="Coefficients",ylim=c(-3,3),col="red",lty="solid", main="AdaSubBoost")
for (i in S0) abline(h = beta[i], col="black",lty="dotted")
axis(side=4, at=output$beta.cur[AdaSubBoost_model+1], labels=c("X1","X2","X3","X4")) 
abline(h=0,col="gray")
abline(v=AdaSubBoost_stop, col="red") # ,lty="dotted")













