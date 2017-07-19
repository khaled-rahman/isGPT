data <- read.csv("trainingset.csv")
sgrnas <- data$Sequence
classt <- data$Class
sgrnalist <- unlist(sgrnas)
sgrnaLength <- 20
transVal <- c(1:sgrnaLength) * 0
cisVal <- c(1:sgrnaLength) * 0
amins = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y");
for (i in 1:length(sgrnalist)){
  s <- toString(sgrnas[i])
  if(classt[i] == "Trans-Golgi"){
    for(j in 1:sgrnaLength){
      if(gregexpr(pattern = amins[j],s)[[1]][1]>0){
        transVal[j] = transVal[j] + length(gregexpr(pattern = amins[j],s)[[1]])
      }
    } 
  }else{
    for(j in 1:sgrnaLength){
      if(gregexpr(pattern = amins[j],s)[[1]][1]>0){
        cisVal[j] = cisVal[j] + length(gregexpr(pattern = amins[j],s)[[1]])
      }
    }
  }
}
transVal = transVal/sum(transVal)
cisVal = cisVal/sum(cisVal)
data <- matrix(c(transVal,cisVal), nrow = 2, ncol = 20,byrow = TRUE)
colours <- c("red", "green")
barplot(
  data, xlab = "Amino Acid Residue", ylab = "Normalized Read Count", cex.main = 1.2, cex.lab = 1.2, box.lty = 4, box.lwd = 4,xaxis.cex.axis=1.3,yaxis.cex.axis=1.3, lwd = 2, yaxis.lwd = 2, xaxis.lwd = 4, yaxis.las = 1, beside =
    TRUE, col = colours, names.arg = amins, axes = TRUE
)
legend(
  "topright", c("trans-Golgi","cis-Golgi"), cex = 1.3, bty =
    "n", fill = colours
)
