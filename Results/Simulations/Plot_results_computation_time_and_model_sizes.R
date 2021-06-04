library("unikn")

setwd("//epi-login/daten/CS/AdaSubBoost_Revision")

# Load results for low-dimensional setting 
load("Sim_Toeplitz08_p20_n100_nSim500_Iter1000_fixedS0_4_autostop_24_05.RData")
results_low <- results

# Load results for high-dimensional setting (a)
#load("Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_fixedS0_10_autostop.RData")
load("Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_fixedS0_10_autostop_24_05.RData")
results_high_a <- results

# Load results for high-dimensional setting (b)
load("Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_randomS0_10_autostop_24_05.RData")
results_high_b <- results

# Load results for high-dimensional setting (c)
load("Sim_Toeplitz08_p1000_n1000_nSim500_Iter10000_fixedS0_100_autostop_24_05.RData")
results_high_c <- results


take_element <- function(list,element1,element2,nSim){
  #l = length(list)
  l = nSim 
  vec = numeric(l)
  for (i in 1:l) vec[i] = list[[i]][[element1]][[element2]]
  return(vec)
}


take_element2 <- function(list,element1){
  l = length(list)
  vec = numeric(l)
  for (i in 1:l) vec[i] = list[[i]][[element1]]
  return(vec)
}

n_algos = length(results[[1]])
nSim = results$nSim



par(mar=c(4.7,3,4,1))

color_mean = c("black")

pal_col <- rev(seecol(pal_signal, n = 4, alpha = 0.67))

color_boxes = c(pal_col[1:4], rep("gray",6))


method_names = c("SubBoost","RSubBoost", "AdaSubBoost", "L2Boost", "TwinBoost", "StabSel",
                 "Lasso", "ENet", "ReLasso")

cex.size = 1.2
cex.lab.size = 1.1
cex.axis.size = 0.9 #0.75
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
angle = 45
cex.names = 1

par(mfrow=c(2,2))
boxplot(take_element(results_low,1,5,nSim), take_element(results_low,2,5,nSim), take_element(results_low,3,5,nSim), take_element(results_low,4,5,nSim), take_element(results_low,5,5,nSim), take_element(results_low,6,5,nSim),
        take_element(results_low,7,5,nSim), take_element(results_low,8,5,nSim), take_element(results_low,9,5,nSim), 
        xaxt="n", main ="Low-dimensional simulation setting", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 0.75, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

color_boxes = c(pal_col[2:4], rep("gray",6))
method_names = c("RSubBoost", "AdaSubBoost", "L2Boost", "TwinBoost", "StabSel",
                 "Lasso", "ENet", "ReLasso")
title(main = ("Computational time (in s)"),  line = 0.4, cex.main = 1.1)

boxplot(take_element(results_high_a,1,5,nSim), take_element(results_high_a,2,5,nSim), take_element(results_high_a,3,5,nSim), take_element(results_high_a,4,5,nSim), take_element(results_high_a,5,5,nSim), take_element(results_high_a,6,5,nSim),
        take_element(results_high_a,7,5,nSim), take_element(results_high_a,8,5,nSim), 
        xaxt="n", main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names) , par("usr")[3] - 5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Computational time (in s)"),  line = 0.4, cex.main = 1.1)


boxplot(take_element(results_high_b,1,5,nSim), take_element(results_high_b,2,5,nSim), take_element(results_high_b,3,5,nSim), take_element(results_high_b,4,5,nSim), take_element(results_high_b,5,5,nSim), take_element(results_high_b,6,5,nSim),
        take_element(results_high_b,7,5,nSim), take_element(results_high_b,8,5,nSim), 
        xaxt="n", main ="High-dimensional simulation setting (b)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names) , par("usr")[3] - 5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Computational time (in s)"),  line = 0.4, cex.main = 1.1)


boxplot(take_element(results_high_c,1,5,nSim), take_element(results_high_c,2,5,nSim), take_element(results_high_c,3,5,nSim), take_element(results_high_c,4,5,nSim), take_element(results_high_c,5,5,nSim), take_element(results_high_c,6,5,nSim),
        take_element(results_high_c,7,5,nSim), take_element(results_high_c,8,5,nSim), 
        xaxt="n", main ="High-dimensional simulation setting (c)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names) , par("usr")[3] - 30, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Computational time (in s)"),  line = 0.4, cex.main = 1.1)



#########################################################################
##### Comp. time for biomedical data applications
#########################################################################

load("LOOCV_Bodyfat_CV_24_05.RData")
results_bodyfat <- LOOCV_results 
load("LOOCV_Diabetes_CV_24_05.RData") 
results_diabetes <- LOOCV_results 
load("LOOCV_Riboflavin_CV_24_05.RData")
results_riboflavin <- LOOCV_results 
load("LOOCV_PCR_CV_24_05.RData")
results_PCR <- LOOCV_results 

par(mar=c(4.7,3,4,1))

color_mean = c("black")

pal_col <- rev(seecol(pal_signal, n = 4, alpha = 0.67))

color_boxes = c(pal_col[1:4], rep("gray",6))


method_names = c("SubBoost","RSubBoost", "AdaSubBoost", "L2Boost", "TwinBoost", "StabSel",
                 "Lasso", "ENet", "ReLasso")

cex.size = 1.2
cex.lab.size = 1.1
cex.axis.size = 1 #0.75
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
angle = 45
cex.names = 1

par(mfrow=c(2,2))

boxplot(take_element2(results_bodyfat,"timeSubBoost"), 
        take_element2(results_bodyfat,"timeRSubBoost"),
        take_element2(results_bodyfat,"timeAdaSubBoost"), 
        take_element2(results_bodyfat,"timeBoosting"), 
        take_element2(results_bodyfat,"timeTwinBoost"), 
        take_element2(results_bodyfat,"timeStability"),
        take_element2(results_bodyfat,"timeLasso"), 
        take_element2(results_bodyfat,"timeENet"),
        take_element2(results_bodyfat,"timeReLasso"), 
        xaxt="n", main ="Bodyfat data (n=71, p=9)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Computational time (in s)"),  line = 0.4, cex.main = 1.1)


boxplot(take_element2(results_diabetes,"timeSubBoost"), 
        take_element2(results_diabetes,"timeRSubBoost"),
        take_element2(results_diabetes,"timeAdaSubBoost"), 
        take_element2(results_diabetes,"timeBoosting"), 
        take_element2(results_diabetes,"timeTwinBoost"), 
        take_element2(results_diabetes,"timeStability"),
        take_element2(results_diabetes,"timeLasso"), 
        take_element2(results_diabetes,"timeENet"),
        take_element2(results_diabetes,"timeReLasso"), 
        xaxt="n", main ="Diabetes data (n=442, p=10)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Computational time (in s)"),  line = 0.4, cex.main = 1.1)

method_names = c("RSubBoost", "AdaSubBoost", "L2Boost", "TwinBoost", "StabSel",
                 "Lasso", "ENet", "ReLasso")
color_boxes = c(pal_col[2:4], rep("gray",6))

boxplot(take_element2(results_riboflavin,"timeRSubBoost"),
        take_element2(results_riboflavin,"timeAdaSubBoost"), 
        take_element2(results_riboflavin,"timeBoosting"), 
        take_element2(results_riboflavin,"timeTwinBoost"), 
        take_element2(results_riboflavin,"timeStability"),
        take_element2(results_riboflavin,"timeLasso"), 
        take_element2(results_riboflavin,"timeENet"),
        take_element2(results_riboflavin,"timeReLasso"), 
        xaxt="n", main ="Riboflavin data (n=71, p=4088)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 20, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Computational time (in s)"),  line = 0.4, cex.main = 1.1)

boxplot(take_element2(results_PCR,"timeRSubBoost"),
        take_element2(results_PCR,"timeAdaSubBoost"), 
        take_element2(results_PCR,"timeBoosting"), 
        take_element2(results_PCR,"timeTwinBoost"), 
        take_element2(results_PCR,"timeStability"),
        take_element2(results_PCR,"timeLasso"), 
        take_element2(results_PCR,"timeENet"),
        take_element2(results_PCR,"timeReLasso"), 
        xaxt="n", main ="PCR data (n=60, p=22,575)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
text(1:length(method_names), par("usr")[3] - 50, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Computational time (in s)"),  line = 0.4, cex.main = 1.1)




####################################################################################
### Model sizes 
####################################################################################

SubBoost_sizes_low <- take_element(results_low,1,1,nSim) + (4 - take_element(results_low,1,2,nSim))

RSubBoost_sizes_low <- take_element(results_low,2,1,nSim) + (4 - take_element(results_low,2,2,nSim))
RSubBoost_sizes_high_a <- take_element(results_high_a,1,1,nSim) + (10 - take_element(results_high_a,1,2,nSim))
RSubBoost_sizes_high_b <- take_element(results_high_b,1,1,nSim) + (10 - take_element(results_high_b,1,2,nSim))
RSubBoost_sizes_high_c <- take_element(results_high_c,1,1,nSim) + (100 - take_element(results_high_c,1,2,nSim))


AdaSubBoost_sizes_low <- take_element(results_low,3,1,nSim) + (4 - take_element(results_low,3,2,nSim))
AdaSubBoost_sizes_high_a <- take_element(results_high_a,2,1,nSim) + (10 - take_element(results_high_a,2,2,nSim))
AdaSubBoost_sizes_high_b <- take_element(results_high_b,2,1,nSim) + (10 - take_element(results_high_b,2,2,nSim))
AdaSubBoost_sizes_high_c <- take_element(results_high_c,2,1,nSim) + (100 - take_element(results_high_c,2,2,nSim))


S0_sizes_low <- take_element(results_low,10,1,nSim) + (4 - take_element(results_low,10,2,nSim))

S0_sizes_high_a <-take_element(results_high_a,9,1,nSim) + (10 - take_element(results_high_a,9,2,nSim))

S0_sizes_high_b <-take_element(results_high_b,9,1,nSim) + (10 - take_element(results_high_b,9,2,nSim))

S0_sizes_high_c <-take_element(results_high_c,9,1,nSim) + (100 - take_element(results_high_c,9,2,nSim))

par(mar=c(4.7,3,4,1))

cex.size = 1.2
cex.lab.size = 1.1
cex.axis.size = 1
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
angle = 45
cex.names = 1.1


method_names <- c("S[0]", "SubBoost", "RSubBoost", "AdaSubBoost")

par(mfrow=c(1,4))
color_boxes = c("gray", pal_col[1:3])
boxplot(S0_sizes_low, SubBoost_sizes_low, RSubBoost_sizes_low, AdaSubBoost_sizes_low, 
        ylim=c(0, 20), 
        col = color_boxes, outcol=color_boxes,
        xaxt="n",
        main = "Low-dimensional simulation setting")
text(1:length(method_names), par("usr")[3] - 0.2, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Model size (number of variables)"),  line = 0.4, cex.main = 1.1)
abline(h=4, lty=1, col="red")

method_names <- c("S[0]", "RSubBoost", "AdaSubBoost")

color_boxes = c("gray", pal_col[2:3])
boxplot(S0_sizes_high_a, RSubBoost_sizes_high_a, AdaSubBoost_sizes_high_a, 
        ylim=c(0, 20), 
        col = color_boxes, outcol=color_boxes,
        xaxt="n",
        main = "High-dimensional simulation setting (a)")
text(1:length(method_names), par("usr")[3] - 0.2, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Model size (number of variables)"),  line = 0.4, cex.main = 1.1)
abline(h=10, lty=1, col="red")


boxplot(S0_sizes_high_b, RSubBoost_sizes_high_b, AdaSubBoost_sizes_high_b,
        ylim=c(0, 20),
        col = color_boxes, outcol=color_boxes,
        xaxt="n",
        main = "High-dimensional simulation setting (b)")
text(1:length(method_names), par("usr")[3] - 0.2, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Model size (number of variables)"),  line = 0.4, cex.main = 1.1)
abline(h=10, lty=1, col="red")


boxplot(S0_sizes_high_c, RSubBoost_sizes_high_c, AdaSubBoost_sizes_high_c, 
        ylim=c(0, 200),
        col = color_boxes, outcol=color_boxes,
        xaxt="n",
        main = "High-dimensional simulation setting (c)")
text(1:length(method_names), par("usr")[3] - 2, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)
title(main = ("Model size (number of variables)"),  line = 0.4, cex.main = 1.1)
abline(h=100, lty=1, col="red")






