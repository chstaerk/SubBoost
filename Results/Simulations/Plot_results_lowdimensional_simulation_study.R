
library("unikn")

load("Sim_Toeplitz08_p20_n100_nSim500_Iter1000_fixedS0_4_autostop_24_05.RData")


#results

take_element <- function(list,element1,element2,nSim){
  #l = length(list)
  l = nSim 
  vec = numeric(l)
  for (i in 1:l) vec[i] = list[[i]][[element1]][[element2]]
  return(vec)
}

n_algos = length(results[[1]])
nSim = results$nSim



par(mar=c(4.2,2.5,2.5,1))

color_mean = c("black")

pal_col <- rev(seecol(pal_signal, n = 4, alpha = 0.67))

color_boxes = c(pal_col[1:4], rep("gray",5))

method_names = c("SubBoost", "RSubBoost", "AdaSubBoost", "L2Boost", "TwinBoost", "StabSel",
                 "Lasso", "ENet", "ReLasso")

cex.size = 1.2
cex.lab.size = 1.1
cex.axis.size = 0.9
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
angle = 45
cex.names = 1

par(mfrow=c(2,2))
boxplot(take_element(results,1,1,nSim), take_element(results,2,1,nSim), take_element(results,3,1,nSim), take_element(results,4,1,nSim), take_element(results,5,1,nSim), take_element(results,6,1,nSim),
        take_element(results,7,1,nSim), take_element(results,8,1,nSim), take_element(results,9,1,nSim), 
        xaxt="n", main ="False Positives", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.4, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element(results,1,2,nSim), take_element(results,2,2,nSim), take_element(results,3,2,nSim), take_element(results,4,2,nSim), take_element(results,5,2,nSim), take_element(results,6,2,nSim),
        take_element(results,7,2,nSim), take_element(results,8,2,nSim), take_element(results,9,2,nSim), 
        xaxt="n", main ="False Negatives", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.09, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element(results,1,3,nSim), take_element(results,2,3,nSim), take_element(results,3,3,nSim), take_element(results,4,3,nSim), take_element(results,5,3,nSim), take_element(results,6,3,nSim),
        take_element(results,7,3,nSim), take_element(results,8,3,nSim), take_element(results,9,3,nSim),
        xaxt="n", main ="Estimation Error (MSE)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.15, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

boxplot(take_element(results,1,4,nSim), take_element(results,2,4,nSim), take_element(results,3,4,nSim), take_element(results,4,4,nSim), take_element(results,5,4,nSim), take_element(results,6,4,nSim),
        take_element(results,7,4,nSim), take_element(results,8,4,nSim), take_element(results,9,4,nSim),
        xaxt="n", main ="Prediction Error (RMSE)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.02, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)




















#########################################################################################

boxplot(take_element(results,1,5,nSim), take_element(results,2,5,nSim), take_element(results,3,5,nSim), take_element(results,4,5,nSim), take_element(results,5,5,nSim), take_element(results,6,5,nSim),
        take_element(results,7,5,nSim), take_element(results,8,5,nSim), take_element(results,9,5,nSim),
        names = method_names, main ="Computational time (s)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)







####################################################################

par(mar=c(2.5,2.5,2.5,1))

color_mean = c("black")

pal_col <- rev(seecol(pal_signal, n = 4, alpha = 0.67))

color_boxes = c(pal_col[1:4], "gray","gray")

cex.size = 1.2
cex.lab.size = 1.1
cex.axis.size = 0.75
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)

par(mfrow=c(2,2))
boxplot(take_element(results,1,1,nSim), take_element(results,2,1,nSim), take_element(results,3,1,nSim), take_element(results,4,1,nSim), take_element(results,5,1,nSim), take_element(results,6,1,nSim),
        names = c("SubBoost", "RSubBoost", "AdaSubBoost", "L2Boost", "TwinBoost", "StabSel"), main ="False Positives", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
boxplot(take_element(results,1,2,nSim), take_element(results,2,2,nSim), take_element(results,3,2,nSim), take_element(results,4,2,nSim), take_element(results,5,2,nSim), take_element(results,6,2,nSim),
        names = c("SubBoost", "RSubBoost", "AdaSubBoost", "L2Boost", "TwinBoost", "StabSel"), main ="False Negatives", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
boxplot(take_element(results,1,3,nSim), take_element(results,2,3,nSim), take_element(results,3,3,nSim), take_element(results,4,3,nSim), take_element(results,5,3,nSim), take_element(results,6,3,nSim),
        names =c("SubBoost", "RSubBoost", "AdaSubBoost", "L2Boost", "TwinBoost", "StabSel"), main ="Estimation Error (MSE)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
boxplot(take_element(results,1,4,nSim), take_element(results,2,4,nSim), take_element(results,3,4,nSim), take_element(results,4,4,nSim), take_element(results,5,4,nSim), take_element(results,6,4,nSim),
        names = c("SubBoost", "RSubBoost", "AdaSubBoost", "L2Boost", "TwinBoost", "StabSel"), main ="Prediction Error (RMSE)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)


#par(mfrow=c(1,1))
#boxplot(take_element(results,1,5,nSim), take_element(results,2,5,nSim), take_element(results,4,5,nSim), take_element(results,6,5,nSim),
#        names = c("AdaSubBoost","Boost","TwinBoost","StabSel"), main ="Computation times (in s)",ylab="s", 
#        col = color_boxes,
#        outcol=color_boxes,outpch = 19)
# size of plots for talk: 10 times 5

##################################################
