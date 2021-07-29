##################################################
# Please download PCR data from JRSSB 
# JRSSB Datasets Vol. 77(5), Song and Liang (2015) 
# Website: https://rss.onlinelibrary.wiley.com/hub/journal/14679868/series-b-datasets/pre_2016a

unAsIs <- function(X) {
  if("AsIs" %in% class(X)) {
    class(X) <- class(X)[-match("AsIs", class(X))]
  }
  X
}

##################################################
library("TH.data")
bodyfat

data = list()
data$x = as.matrix(bodyfat[,-2]) 
data$y = as.vector(bodyfat$DEXfat)

n = length(data$y)
p = dim(data$x)[2]

correlations_bodyfat <- cor(data$x)[lower.tri(cor(data$x))]


##################################################
library("lars")
data(diabetes)

data = list()
data$x = unAsIs(as.matrix(diabetes$x)) 
#data$x = unAsIs(as.matrix(diabetes$x2)) 
data$y = unAsIs(as.vector(diabetes$y) - mean(diabetes$y))


n = length(data$y)
p = dim(data$x)[2]

correlations_diabetes <- cor(data$x)[lower.tri(cor(data$x))]
#?diabetes

##################################################
library("hdi")

data(riboflavin)
n = length(riboflavin$y)
p = dim(riboflavin$x)[2]
Xnames = colnames(riboflavin$x)

data = list()
data$x = as.matrix(riboflavin$x) 
data$y = as.vector(riboflavin$y)

unAsIs <- function(X) {
  if("AsIs" %in% class(X)) {
    class(X) <- class(X)[-match("AsIs", class(X))]
  }
  X
}

data$x = unAsIs(data$x)
data$y = unAsIs(data$y)

correlations_riboflavin <- cor(data$x)[lower.tri(cor(data$x))]


##################################################
# Please download PCR data from JRSSB 
# JRSSB Datasets Vol. 77(5), Song and Liang (2015) 
# Website: https://rss.onlinelibrary.wiley.com/hub/journal/14679868/series-b-datasets/pre_2016a
X = t( read.table("Xgenes.txt") )
Y = scan("Y3.txt")
Xnames = scan("gene_id.txt",what=character())

data = list()
data$x = as.matrix(X)
data$y = as.vector(Y)
colnames(data$x) = Xnames

n = length(data$y)
p = dim(data$x)[2]

correlations_PCR <- cor(data$x)[lower.tri(cor(data$x))]

##################################################
library(unikn)

pal_col <- rev(seecol(pal_signal, n = 10, alpha = 0.9))

color_boxes = c(pal_col, "gray","gray")

#summary(correlations)
#boxplot(correlations)
#sum(abs(correlations)>0.9)
par(mfrow=c(2,2))
par(mar=c(4,4,4,1))
cex.size = 1.3
cex.lab.size = 1.1
cex.axis.size = 0.9
par(cex.axis = cex.axis.size, cex.lab = cex.lab.size, cex.main = cex.size, las = 1)


hist(correlations_bodyfat,xlim=c(-1,1), freq=TRUE, breaks=seq(-1, 1, 0.1), main="Bodyfat data (n=71, p=9)", col=c(rev(pal_col), pal_col), xlab="Correlation", ylab="", ylim=c(0,8))
title(main = ("Histogram of correlations"),  line = 0.4, cex.main = 1)
hist(correlations_diabetes,xlim=c(-1,1), freq=TRUE, breaks=seq(-1, 1, 0.1), main="Diabetes data (n=442, p=10)", col=c(rev(pal_col), pal_col), xlab="Correlation", ylab="", ylim=c(0,12))
title(main = ("Histogram of correlations"),  line = 0.4, cex.main = 1)
hist(correlations_riboflavin,xlim=c(-1,1), freq=TRUE, breaks=seq(-1, 1, 0.1), main="Riboflavin data (n=71, p=4088)", col=c(rev(pal_col), pal_col), xlab="Correlation", ylab="", ylim=c(0,8*10^5))
title(main = ("Histogram of correlations"),  line = 0.4, cex.main = 1)
hist(correlations_PCR,xlim=c(-1,1), freq=TRUE, breaks=seq(-1, 1, 0.1), main="PCR data (n=60, p=22,575)", col=c(rev(pal_col), pal_col), xlab="Correlation", ylab="", ylim=c(0,6*10^7))
title(main = ("Histogram of correlations"),  line = 0.4, cex.main = 1)

