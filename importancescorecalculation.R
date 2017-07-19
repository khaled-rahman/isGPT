#rfdata <- read.csv("rfmodel_304_posTrimer.rds")
#amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y")
#all
# pif = -0.0009376378
# psf =  -0.0008415906
# gap = 0.03536461

#3000 
 pif = 0.02884622 
 psf = 0.001006389 
 gap = 0.1139051
 
 colours <- c("red", "blue", "green")
 cdata = matrix(c(psf, pif, gap),nrow = 1, ncol = 3)
 barplot(
   cdata, xlab = "Feature Type", ylab = "MeanDecreaseAccuracy", cex.main = 1.2, cex.lab = 1.2, box.lty = 4, box.lwd = 4,xaxis.cex.axis=1.3,yaxis.cex.axis=1.3, lwd = 2, yaxis.lwd = 2, xaxis.lwd = 4, yaxis.las = 1, beside =
     TRUE, col = colours, names.arg = c("PSF", "n-grams","nGDip"), axes = TRUE, ylim = c(0,0.12)
 )