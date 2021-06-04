library("unikn")

# Load results for high-dimensional setting (a)
load("Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_fixedS0_10_autostop_choice_of_criterion.RData")

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


results$const_values



#par(mar=c(2.5,2.5,2.5,1))
par(mar=c(4.7,2.5,4,1))

color_mean = c("black")

pal_col <- rev(seecol(pal_signal, n = 4, alpha = 0.67))

color_boxes = c(rep(c(pal_col[2],pal_col[3]),3))


#method_names = c("RSubBoost", paste("AdaSubBoost", 1:5, sep=""))#, "EBIC")
#method_names = paste("K = ",  c(0, results$K_values[-length(results$K_values)]), sep="")
#method_names = paste("K = ",  c(0, results$K_values), sep="")
method_names = c("RSubBoost \n AIC", "AdaSubBoost \n AIC", "RSubBoost \n BIC", "AdaSubBoost \n BIC", "RSubBoost \n EBIC1", "AdaSubBoost \n EBIC1")

cex.size = 1.2
cex.lab.size = 1.1
cex.axis.size = 1
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)
par(font.axis = 2)
angle = 45
cex.names = 1

par(mfrow=c(2,2))
boxplot(take_element3(results,1,1,1,nSim), take_element3(results,2,1,1,nSim), 
        take_element3(results,1,2,1,nSim), take_element3(results,2,2,1,nSim),
        take_element3(results,1,3,1,nSim), take_element3(results,2,3,1,nSim),
        xaxt="n", main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
title(main = "False Positives",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 12, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)


boxplot(take_element3(results,1,1,2,nSim), take_element3(results,2,1,2,nSim), 
        take_element3(results,1,2,2,nSim), take_element3(results,2,2,2,nSim),
        take_element3(results,1,3,2,nSim), take_element3(results,2,3,2,nSim),
        xaxt="n", main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "False Negatives",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 0.7, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)


boxplot(take_element3(results,1,1,3,nSim), take_element3(results,2,1,3,nSim), 
        take_element3(results,1,2,3,nSim), take_element3(results,2,2,3,nSim),
        take_element3(results,1,3,3,nSim), take_element3(results,2,3,3,nSim),
        xaxt="n", main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "Estimation Error (MSE)",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 1.3, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)


boxplot(take_element3(results,1,1,4,nSim), take_element3(results,2,1,4,nSim), 
        take_element3(results,1,2,4,nSim), take_element3(results,2,2,4,nSim),
        take_element3(results,1,3,4,nSim), take_element3(results,2,3,4,nSim),
        xaxt="n", main ="High-dimensional simulation setting (a)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "Prediction Error (RMSE)",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 0.12, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)

#Choice_of_criterion_high-dimensional_a_12_6

# Load results for high-dimensional setting (b)
load("Sim_Toeplitz08_p1000_n100_nSim500_Iter5000_randomS0_10_autostop_choice_of_criterion.RData")


par(mfrow=c(2,2))
boxplot(take_element3(results,1,1,1,nSim), take_element3(results,2,1,1,nSim), 
        take_element3(results,1,2,1,nSim), take_element3(results,2,2,1,nSim),
        take_element3(results,1,3,1,nSim), take_element3(results,2,3,1,nSim),
        xaxt="n", main ="High-dimensional simulation setting (b)", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
title(main = "False Positives",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 12, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)


boxplot(take_element3(results,1,1,2,nSim), take_element3(results,2,1,2,nSim), 
        take_element3(results,1,2,2,nSim), take_element3(results,2,2,2,nSim),
        take_element3(results,1,3,2,nSim), take_element3(results,2,3,2,nSim),
        xaxt="n", main ="High-dimensional simulation setting (b)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "False Negatives",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 0.6, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)


boxplot(take_element3(results,1,1,3,nSim), take_element3(results,2,1,3,nSim), 
        take_element3(results,1,2,3,nSim), take_element3(results,2,2,3,nSim),
        take_element3(results,1,3,3,nSim), take_element3(results,2,3,3,nSim),
        xaxt="n", main ="High-dimensional simulation setting (b)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "Estimation Error (MSE)",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 1, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)


boxplot(take_element3(results,1,1,4,nSim), take_element3(results,2,1,4,nSim), 
        take_element3(results,1,2,4,nSim), take_element3(results,2,2,4,nSim),
        take_element3(results,1,3,4,nSim), take_element3(results,2,3,4,nSim),
        xaxt="n", main ="High-dimensional simulation setting (b)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "Prediction Error (RMSE)",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 0.12, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)


# Load results for high-dimensional setting (c)
load("Sim_Toeplitz08_p1000_n1000_nSim500_Iter5000_fixedS0_100_autostop_choice_of_criterion.RData")


par(mfrow=c(2,2))
boxplot(take_element3(results,1,1,1,nSim), take_element3(results,2,1,1,nSim), 
        take_element3(results,1,2,1,nSim), take_element3(results,2,2,1,nSim),
        take_element3(results,1,3,1,nSim), take_element3(results,2,3,1,nSim),
        xaxt="n", main ="High-dimensional simulation setting (c)", 
        col = color_boxes,
        outcol= color_boxes,outpch = 19)
title(main = "False Positives",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)


boxplot(take_element3(results,1,1,2,nSim), take_element3(results,2,1,2,nSim), 
        take_element3(results,1,2,2,nSim), take_element3(results,2,2,2,nSim),
        take_element3(results,1,3,2,nSim), take_element3(results,2,3,2,nSim),
        xaxt="n", main ="High-dimensional simulation setting (c)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "False Negatives",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 5, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)


boxplot(take_element3(results,1,1,3,nSim), take_element3(results,2,1,3,nSim), 
        take_element3(results,1,2,3,nSim), take_element3(results,2,2,3,nSim),
        take_element3(results,1,3,3,nSim), take_element3(results,2,3,3,nSim),
        xaxt="n", main ="High-dimensional simulation setting (c)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "Estimation Error (MSE)",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 8, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)


boxplot(take_element3(results,1,1,4,nSim), take_element3(results,2,1,4,nSim), 
        take_element3(results,1,2,4,nSim), take_element3(results,2,2,4,nSim),
        take_element3(results,1,3,4,nSim), take_element3(results,2,3,4,nSim),
        xaxt="n", main ="High-dimensional simulation setting (c)", 
        col = color_boxes,
        outcol=color_boxes,outpch = 19)
title(main = "Prediction Error (RMSE)",  line = 0.4, cex.main = 1.1)
text(1:length(method_names), par("usr")[3] - 0.3, labels = method_names, srt = angle, adj = 1, xpd = TRUE, cex=cex.names)




