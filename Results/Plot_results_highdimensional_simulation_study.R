library("unikn")

setwd("//epi-login/daten/CS/AdaSubBoost_Revision")

# Load results for high-dimensional setting (a)
load("Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_fixedS0_10_autostop_24_05.RData")


take_element <- function(list,element1,element2,nSim){
  #l = length(list)
  l = nSim 
  vec = numeric(l)
  for (i in 1:l) vec[i] = list[[i]][[element1]][[element2]]
  return(vec)
}


take_element3 <- function(list,element1,element2,element3,nSim){
  l = nSim 
  vec = numeric(l)
  for (i in 1:l) vec[i] = list[[i]][[element1]][[element2]][[element3]]
  return(vec)
}


n_algos = length(results[[1]])
nSim = results$nSim



par(mar=c(4.2,2.5,2.5,1))

color_mean = c("black")

pal_col <- rev(seecol(pal_signal, n = 4, alpha = 0.67))

color_boxes = c(pal_col[2:4], rep("gray",6))


method_names = c("RSubBoost", "AdaSubBoost", "L2Boost", "TwinBoost", "StabSel",
                 "Lasso", "ENet", "ReLasso")

cex.size = 1.2
cex.lab.size = 1.1
cex.axis.size = 0.9 #0.75
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
angle = 45
cex.names = 1

par(mfrow=c(2,2))
boxplot(take_element(results,1,1,nSim), take_element(results,2,1,nSim), take_element(results,3,1,nSim), take_element(results,4,1,nSim), take_element(results,5,1,nSim), take_element(results,6,1,nSim),
        take_element(results,7,1,nSim), take_element(results,8,1,nSim), 
        xaxt="n", main ="False Positives", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19, ylim=c(0,200))
text(1:length(method_names), par("usr")[3] - 5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element(results,1,2,nSim), take_element(results,2,2,nSim), take_element(results,3,2,nSim), take_element(results,4,2,nSim), take_element(results,5,2,nSim), take_element(results,6,2,nSim),
        take_element(results,7,2,nSim), take_element(results,8,2,nSim), 
        xaxt="n", main ="False Negatives", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.25, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element(results,1,3,nSim), take_element(results,2,3,nSim), take_element(results,3,3,nSim), take_element(results,4,3,nSim), take_element(results,5,3,nSim), take_element(results,6,3,nSim),
        take_element(results,7,3,nSim), take_element(results,8,3,nSim),  
        xaxt="n", main ="Estimation Error (MSE)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element(results,1,4,nSim), take_element(results,2,4,nSim), take_element(results,3,4,nSim), take_element(results,4,4,nSim), take_element(results,5,4,nSim), take_element(results,6,4,nSim),
        take_element(results,7,4,nSim), take_element(results,8,4,nSim), 
        xaxt="n", main ="Prediction Error (RMSE)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.05, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)



# Load results for high-dimensional setting (b)
load("Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_randomS0_10_autostop_24_05.RData")

cex.size = 1.2
cex.lab.size = 1.1
cex.axis.size = 0.9 #0.75
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
angle = 45
cex.names = 1

par(mfrow=c(2,2))
boxplot(take_element(results,1,1,nSim), take_element(results,2,1,nSim), take_element(results,3,1,nSim), take_element(results,4,1,nSim), take_element(results,5,1,nSim), take_element(results,6,1,nSim),
        take_element(results,7,1,nSim), take_element(results,8,1,nSim), 
        xaxt="n", main ="False Positives", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19, ylim=c(0,200))
text(1:length(method_names), par("usr")[3] - 5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element(results,1,2,nSim), take_element(results,2,2,nSim), take_element(results,3,2,nSim), take_element(results,4,2,nSim), take_element(results,5,2,nSim), take_element(results,6,2,nSim),
        take_element(results,7,2,nSim), take_element(results,8,2,nSim), 
        xaxt="n", main ="False Negatives", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19,  ylim=c(0,10))
text(1:length(method_names), par("usr")[3] - 0.25, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element(results,1,3,nSim), take_element(results,2,3,nSim), take_element(results,3,3,nSim), take_element(results,4,3,nSim), take_element(results,5,3,nSim), take_element(results,6,3,nSim),
        take_element(results,7,3,nSim), take_element(results,8,3,nSim),  
        xaxt="n", main ="Estimation Error (MSE)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element(results,1,4,nSim), take_element(results,2,4,nSim), take_element(results,3,4,nSim), take_element(results,4,4,nSim), take_element(results,5,4,nSim), take_element(results,6,4,nSim),
        take_element(results,7,4,nSim), take_element(results,8,4,nSim), 
        xaxt="n", main ="Prediction Error (RMSE)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.08, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)



# Load results for high-dimensional setting (c)
load("Sim_Toeplitz08_p1000_n1000_nSim500_Iter5000_fixedS0_100_autostop_choice_of_criterion.RData")
results_criterion <- results

load("Sim_Toeplitz08_p1000_n1000_nSim500_Iter10000_fixedS0_100_autostop_24_05.RData")

method_names = c("RSubBoost \n AIC", "AdaSubBoost \n AIC", "RSubBoost \n EBIC1", "AdaSubBoost \n EBIC1", 
                 "L2Boost", "TwinBoost", "StabSel",
                 "Lasso", "ENet", "ReLasso")

color_boxes = c(pal_col[2:3], pal_col[2:4], rep("gray",6))


cex.size = 1.2
cex.lab.size = 1.1
cex.axis.size = 0.9 #0.75
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
angle = 45
cex.names = 1

par(mfrow=c(2,2))
boxplot(take_element3(results_criterion,1,1,1,nSim), take_element3(results_criterion,2,1,1,nSim), 
        take_element3(results_criterion,1,3,1,nSim), take_element3(results_criterion,2,3,1,nSim), 
        take_element(results,3,1,nSim), take_element(results,4,1,nSim), take_element(results,5,1,nSim), take_element(results,6,1,nSim),
        take_element(results,7,1,nSim), take_element(results,8,1,nSim), 
        xaxt="n", main ="False Positives", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 12, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element3(results_criterion,1,1,2,nSim), take_element3(results_criterion,2,1,2,nSim), 
        take_element3(results_criterion,1,3,2,nSim), take_element3(results_criterion,2,3,2,nSim), 
        take_element(results,3,2,nSim), take_element(results,4,2,nSim), take_element(results,5,2,nSim), take_element(results,6,2,nSim),
        take_element(results,7,2,nSim), take_element(results,8,2,nSim), 
        xaxt="n", main ="False Negatives", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 2.5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element3(results_criterion,1,1,3,nSim), take_element3(results_criterion,2,1,3,nSim), 
        take_element3(results_criterion,1,3,3,nSim), take_element3(results_criterion,2,3,3,nSim), 
        take_element(results,3,3,nSim), take_element(results,4,3,nSim), take_element(results,5,3,nSim), take_element(results,6,3,nSim),
        take_element(results,7,3,nSim), take_element(results,8,3,nSim),  
        xaxt="n", main ="Estimation Error (MSE)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 4, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element3(results_criterion,1,1,4,nSim), take_element3(results_criterion,2,1,4,nSim), 
        take_element3(results_criterion,1,3,4,nSim), take_element3(results_criterion,2,3,4,nSim), 
        take_element(results,3,4,nSim), take_element(results,4,4,nSim), take_element(results,5,4,nSim), take_element(results,6,4,nSim),
        take_element(results,7,4,nSim), take_element(results,8,4,nSim), 
        xaxt="n", main ="Prediction Error (RMSE)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.15, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)




###########################################################################





















