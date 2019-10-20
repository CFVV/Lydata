##### generic data setup:
set.seed(977) # this makes the simulation exactly reproducible
ni     = 100  # 100 people
nj     =  10  # 10 week study
id     = rep(1:ni, each=nj)
cond   = rep(c("control", "diet"), each=nj*(ni/2))
base   = round(rep(rnorm(ni, mean=250, sd=10), each=nj))
week   = rep(1:nj, times=ni)
y      = round(base + rnorm(ni*nj, mean=0, sd=1))

# Base data
base_data = cbind(id,week,cond,base, y)


# MCAR
prop.m = .07  # 7% missingness
mcar   = runif(ni*nj, min=0, max=1)
y.mcar = ifelse(mcar<prop.m, NA, y)  # unrelated to anything
MCAR = cbind(id, week, cond, base, y, y.mcar)
write.csv(MCAR,"/Volumes/HDD/Documents/Nasa Space Apps/MCAR.csv", row.names = FALSE)


# MAR
y.mar = matrix(y, ncol=nj, nrow=ni, byrow=TRUE)
for(i in 1:ni){
  for(j in 4:nj){
    dif1 = y.mar[i,j-2]-y.mar[i,j-3]
    dif2 = y.mar[i,j-1]-y.mar[i,j-2]
    if(dif1>0 & dif2>0){  # if weight goes up twice, drops out
      y.mar[i,j:nj] = NA;  break
    }
  }
}
y.mar = as.vector(t(y.mar))
MAR = cbind(id, week, cond, base, y, y.mar)
write.csv(MAR,"/Volumes/HDD/Documents/Nasa Space Apps/MAR.csv", row.names = FALSE)

# NMAR
sort.y = sort(y, decreasing=TRUE)
nmar   = sort.y[ceiling(prop.m*length(y))]
#y.nmar = ifelse(y>nmar, NA, y)  # doesn't show up when heavier
NMAR = cbind(id, week, cond, base, y, y.nmar)
write.csv(NMAR,"/Volumes/HDD/Documents/Nasa Space Apps/NMAR.csv", row.names = FALSE)
