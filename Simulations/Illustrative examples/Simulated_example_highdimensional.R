#library("devtools")
#install_github("chstaerk/SubBoost")

library("SubBoost")

RNGkind("L'Ecuyer-CMRG")

########### Example ###############


n_test = 1000 

# High-dimensional example 
n = 100 
p = 1000 
s0 = 4
S0 = c(1:4) 
beta = numeric(p)
beta[S0] =  c(-2,-1,1,2) 

sigma.normal = 1
corr = 0.8 
automatic.stopping = FALSE



set.seed(1234)
data = simdata.toeplitz.corr(n =n ,p =p ,beta =beta ,sigma.normal = sigma.normal,corr=corr) 
data.test = simdata.toeplitz.corr(n_test,p,beta=beta,sigma.normal= sigma.normal,corr= corr)

data$y = data$y - mean(data$y)
data$x = scale(data$x)

colnames(data$x) = c(paste("X", 1:s0, sep=""),rep("",p-s0))
#################################################

K = 100
q = 10 
s = 20
tau = 0.01 # 0.01
tau_L2Boost = 0.1
criterion = "EBIC"
Iter = 1000

const = 1 # 1 # gamma (Penalty in EBIC)

PFER = 10

savings = 1

size.fixed = 2  # NULL
nstop = Iter #p/size.fixed

update = "S" 
conservative = TRUE

# AdaSubBoost 
set.seed(123)
start.time <- Sys.time()
output = AdaSubBoost(data=data, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=20, savings=savings,
                     conservative = conservative, update=update, nstop=nstop, automatic.stopping = automatic.stopping)
end.time <- Sys.time()
timeAdaSubBoost = as.double(end.time-start.time,units = "secs")
timeAdaSubBoost
output$mstop
AdaSubBoost_beta = output$beta.cur
AdaSubBoost_model = which(AdaSubBoost_beta[-1]!=0)
AdaSubBoost_model
S0

output$A.list[1:1000]

# RSubBoost (without adapting probabilities, adaptive=FALSE)
set.seed(123)
start.time <- Sys.time()
outputRSub = AdaSubBoost(data=data, Iter=Iter, size.fixed=size.fixed, K=K, q=q, tau = tau, const=const, U_C=20, savings=savings,
                         conservative = TRUE, update=update, adaptive=FALSE, nstop=nstop, automatic.stopping = automatic.stopping)
end.time <- Sys.time()
timeRSubBoost = as.double(end.time-start.time,units = "secs")
timeRSubBoost
outputRSub$mstop
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
  Sub_model = which(outputSub[-1]!=0)
  Sub_model
  S0
}

# L2Boosting
set.seed(123)
set.seed(12)
mstop.max = 1000
mod <- glmboost(y = data$y, x = data$x, control = boost_control(mstop = mstop.max, nu = tau_L2Boost)) ### mstop needs tuning
cv10f <- cv(model.weights(mod), type = "kfold")
cvm <-cvrisk(mod, papply = lapply,  folds = cv10f)
m.stop = mstop(cvm)
mod.boost = mod[m.stop]
Boost_model = which(coef(mod.boost, which=1:p)!=0)
length(Boost_model) 

# Selected models
Boost_model
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

RSubBoost_stop
AdaSubBoost_stop


####### Plots 

par(mar=c(5.1,4.1,4.1,2.1))

#### Only L2Boosting
cex.size = 1.4
cex.lab.size = 1.2
cex.axis.size = 1.1
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size)
mod <- glmboost(y = data$y, x = data$x, control = boost_control(mstop = mstop.max))
par(mfrow=c(1,1),las=1)
plot(mod,ylim=c(-3,3),main="L2Boosting")
for (i in S0) abline(h = beta[i], col="black",lty="dotted")
abline(v=m.stop, col="red", lty="dashed")

#######################

#### L2Boosting, RSubBoost and AdaSubBoost
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
  matplot(t(outputSubl$beta.mat[Sub_model+1,,drop=F]),type="l",xlab="Number of boosting iterations",ylab="Coefficients",ylim=c(-3,3),col="red",lty="solid", main="SubBoost")
  for (i in S0) abline(h = beta[i], col="black",lty="dotted")
  axis(side=4, at=outputSub$beta.cur[Sub_model+1], labels=c("X1","X2","X3","X4")) 
  abline(h=0,col="gray")
}

matplot(t(outputRSub$beta.mat[RSubBoost_model+1,,drop=F]),type="l",xlab="Number of boosting iterations",ylab="Coefficients",ylim=c(-3,3),col="red",lty="solid", main="RSubBoost")
for (i in S0) abline(h = beta[i], col="black",lty="dotted")
axis(side=4, at=outputRSub$beta.cur[RSubBoost_model+1], labels=c("X1","X2","X4")) 
abline(h=0,col="gray")
abline(v=RSubBoost_stop, col="red") # ,lty="dotted")


matplot(t(output$beta.mat[AdaSubBoost_model+1,,drop=F]),type="l",xlab="Number of boosting iterations",ylab="Coefficients",ylim=c(-3,3),col="red",lty="solid", main="AdaSubBoost")
for (i in S0) abline(h = beta[i], col="black",lty="dotted")
axis(side=4, at=output$beta.cur[AdaSubBoost_model+1], labels=c("X1","X2","X3","X4")) 
abline(h=0,col="gray")
abline(v=AdaSubBoost_stop, col="red") # ,lty="dotted")


# Save results 

output1 <- output
outputRSub1 <- outputRSub
mod.boost1 <- mod.boost
mod1 <- mod
iterations_ex1 <- Iter

m.stop1 <- m.stop
RSubBoost_stop1 <- RSubBoost_stop
AdaSubBoost_stop1 <- AdaSubBoost_stop

data.test1 <- data.test
data1 <-data

#################################################################################################

## Plots of training and test error 

######### (not the most efficient way)
mboost_coeff1 = matrix(NA,nrow = mstop.max, ncol = p)
#for (i in mstop.max:1) mboost_coeff[i,] = coef(mod[i],which=1:p)
for (i in c(seq(iterations_ex1, 1, -50),1)) mboost_coeff1[i,] = coef(mod1[i],which=1:p)
#############
residual_Boost_train1 = numeric(iterations_ex1)
residual_Boost_test1 = numeric(iterations_ex1)
residual_AdaSubBoost_test1 = numeric(iterations_ex1)
residual_RSubBoost_test1 = numeric(iterations_ex1)
for (i in iterations_ex1:1) residual_Boost_train1[i] = mean ((data1$y - data1$x %*% mboost_coeff1[i,])^2 )
for (i in iterations_ex1:1) residual_Boost_test1[i] = mean ((data.test1$y - data.test1$x %*% mboost_coeff1[i,])^2 )
for (i in 1:iterations_ex1) residual_AdaSubBoost_test1[i] = mean ((data.test1$y - cbind(1,data.test1$x) %*% output1$beta.mat[,i])^2 )
for (i in 1:iterations_ex1) residual_RSubBoost_test1[i] = mean ((data.test1$y - cbind(1,data.test1$x) %*% outputRSub1$beta.mat[,i])^2 )

cex.size = 1.4
cex.lab.size = 1.2
cex.axis.size = 1.1
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size)


############################################################################
m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
layout(mat = m, heights = c( 0.9, 0.1))
############################################################################



plot(output1$values^2, pch=20, type="l",lty = 1,lwd = 2, xlab = "Number of iterations", ylab = "MSE",ylim = c(0,max(output1$values^2)),main="MSE on training data", col="blue")
lines(c(seq(iterations_ex1, 1, -50),1),residual_Boost_train1[c(seq(iterations_ex1, 1, -50),1)],col="red", lty = 2,lwd = 2)
lines(outputRSub1$values^2,col="black", lty = 4,lwd = 3)
abline(v=AdaSubBoost_stop1, col="blue", lty=1)
abline(v=m.stop1, col="red" , lty=2)
abline(v=RSubBoost_stop1, col="black", lty=4)


plot(residual_AdaSubBoost_test1, pch=20, type="l",lty = 1,lwd = 2, xlab = "Number of iterations", ylab = "MSE",ylim = c(0,max(residual_AdaSubBoost_test1)),main="MSE on test data", col="blue")
lines(c(seq(iterations_ex1, 1, -50),1), residual_Boost_test1[c(seq(iterations_ex1, 1, -50),1)],col="red", lty = 2,lwd = 2)
lines(residual_RSubBoost_test1,col="black", pch=20, lty = 4, lwd = 3)
abline(v=AdaSubBoost_stop1, col="blue", lty=1)
abline(v=m.stop1,col="red", lty=2)
abline(v=RSubBoost_stop1, col="black", lty=4)



# Plot legend
par(mar=c(0,0,0,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "center",inset = 0,
       legend = c("L2Boosting", "RSubBoost", "AdaSubBoost"), 
       col=c("red", "black", "blue"), cex=1.5, box.lty=1, ncol = 3, pt.bg = 'white', lty=c(2, 4, 1), lwd = c(2,3,2))
par(mar=c(5.1, 4.1, 4.1, 2.1))




