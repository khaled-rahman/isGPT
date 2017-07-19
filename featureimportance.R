impindex = 4
rfmodel = readRDS(rfmodelFile);
rankedt = (rfmodel$importance[order(-rfmodel$importance[,impindex]),])
cname = rownames(rankedt)
pif = 0
psf = 0
gap = 0

for(i in 1:8000){
  if(gregexpr(pattern = "C_", cname[i])[[1]][1]>0){
    pif = pif + rankedt[i,impindex]
  } else if(gregexpr(pattern = "P_", cname[i])[[1]][1]>0){
    psf = psf + rankedt[i,impindex]
  }  else{
    gap = gap + rankedt[i,impindex]
  }
}
cat(pif,"\n")
cat(psf,"\n")
cat(gap,"\n")