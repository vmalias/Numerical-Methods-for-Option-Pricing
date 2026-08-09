library(ggplot2)
library(tibble)
library(openxlsx)

########################################################################################
##################################   Lamda = 1.22474   #################################
########################################################################################

Periods = seq(5,100)
K = seq(60,140,1)
DiffKamRit = matrix(0, length(Periods), length(K))
KamRit = matrix(0, length(Periods), length(K))
DiffCRR = matrix(0, length(Periods), length(K))
CRR = matrix(0, length(Periods), length(K))
RealValue = matrix(0, 1, length(K))

for (j in seq(1,length(K))) {
  RealValue[j] = KammradAmericanPut(100,K[j],1,1000,1.22474,0.2,0.05)
}

for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(K))) {
    CRR[i,j] = BinTreeAmPut(100,K[j],1,Periods[i],0.2,0.05)
    KamRit[i,j] = KammradAmericanPut(100,K[j],1,Periods[i],1.22474,0.2,0.05)
    DiffCRR[i,j] = - CRR[i,j] + RealValue[j]
    DiffKamRit[i,j] =  - KamRit[i,j] + RealValue[j]
  }
}

colnames(CRR) <- as.character(K)
rownames(CRR) <- as.character(Periods)

colnames(KamRit) <- as.character(K)
rownames(KamRit) <- as.character(Periods)

colnames(RealValue) <- as.character(K)

colnames(DiffCRR) <- as.character(K)
rownames(DiffCRR) <- as.character(Periods)

colnames(DiffKamRit) <- as.character(K)
rownames(DiffKamRit) <- as.character(Periods)



y = which(colnames(DiffCRR) == 90)
x = which(colnames(DiffCRR) == 110)
k = which(colnames(DiffCRR) == 100)


z5 = which(rownames(DiffCRR) == 5)
z10 = which(rownames(DiffCRR) == 10) 
z20 = which(rownames(DiffCRR) == 20)
z40 = which(rownames(DiffCRR) == 40)
z60 = which(rownames(DiffCRR) == 60)
z80 = which(rownames(DiffCRR) == 80)
z100 = which(rownames(DiffCRR) == 100)

#write.xlsx(CRR[c(z5,z10,z20,z40,z60,z80,z100),c(y,k,x)], "CRR.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(KamRit[c(z5,z10,z20,z40,z60,z80,z100),c(y,k,x)], "KamRit.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(RealValue[,c(y,k,x)], "RealValue.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffCRR[c(z5,z10,z20,z40,z60,z80,z100),c(y,k,x)], "DiffCRR.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffKamRit[c(z5,z10,z20,z40,z60,z80,z100),c(y,k,x)], "DiffKamRit.xlsx", colNames = TRUE, rowNames = TRUE)


########################################################################################
##################################   K = 90    #########################################
########################################################################################

Periods = seq(5,100)
lamda  = seq(1.1,2.1,0.2)
lamda[2] = 1.22474
DiffKamRit90 = matrix(0, length(Periods), length(lamda))
RealValue90 = KammradAmericanPut(100,90,1,1000,1.22474,0.2,0.05)
DiffCRR90 = as.matrix(DiffCRR[,y])  
  
for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(lamda))) {
    DiffKamRit90[i,j] = - KammradAmericanPut(100,90,1,Periods[i],lamda[j],0.2,0.05) + RealValue90
  }
}
colnames(DiffKamRit90) <- as.character(lamda)
rownames(DiffKamRit90) <- as.character(Periods)
rownames(DiffCRR90) <- as.character(Periods)
colnames(DiffCRR90) <- "CRR"

Mapping90 = data.frame(DiffKamRit90, DiffCRR90)
colnames(Mapping90) <- c("Lamda_1.1", "Lamda_1.22474", "Lamda_1.5", "Lamda_1.7", "Lamda_1.9", "Lamda_2.1", "CRR")


ggplot(Mapping90)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.1), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping90)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.22474), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping90)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.5), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping90)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.7), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping90)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.9), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping90)+
  geom_line(mapping = aes(x = Periods, y = Lamda_2.1), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

########################################################################################
##################################   K = 110    #########################################
########################################################################################

Periods = seq(5,100)
lamda  = seq(1.1,2.1,0.2)
lamda[2] = 1.22474
DiffKamRit110 = matrix(0, length(Periods), length(lamda))
RealValue110 = KammradAmericanPut(100,110,1,1000,1.22474,0.2,0.05)
DiffCRR110 = as.matrix(DiffCRR[,x])  

for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(lamda))) {
    DiffKamRit110[i,j] = - KammradAmericanPut(100,110,1,Periods[i],lamda[j],0.2,0.05) + RealValue110
  }
}
colnames(DiffKamRit110) <- as.character(lamda)
rownames(DiffKamRit110) <- as.character(Periods)
rownames(DiffCRR110) <- as.character(Periods)
colnames(DiffCRR110) <- "CRR"

Mapping110 = data.frame(DiffKamRit110, DiffCRR110)
colnames(Mapping110) <- c("Lamda_1.1", "Lamda_1.22474", "Lamda_1.5", "Lamda_1.7", "Lamda_1.9", "Lamda_2.1", "CRR")



ggplot(Mapping110)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.1), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping110)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.22474), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping110)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.5), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping110)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.7), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping110)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.9), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping110)+
  geom_line(mapping = aes(x = Periods, y = Lamda_2.1), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()


########################################################################################
##################################   K = 100    #########################################
########################################################################################

Periods = seq(5,100)
lamda  = seq(1.1,2.1,0.2)
lamda[2] = 1.22474
DiffKamRit100 = matrix(0, length(Periods), length(lamda))
RealValue100 = KammradAmericanPut(100,100,1,1000,1.22474,0.2,0.05)
DiffCRR100 = as.matrix(DiffCRR[,k])  

for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(lamda))) {
    DiffKamRit100[i,j] = - KammradAmericanPut(100,100,1,Periods[i],lamda[j],0.2,0.05) + RealValue100
  }
}
colnames(DiffKamRit100) <- as.character(lamda)
rownames(DiffKamRit100) <- as.character(Periods)
rownames(DiffCRR100) <- as.character(Periods)
colnames(DiffCRR100) <- "CRR"

Mapping100 = data.frame(DiffKamRit100, DiffCRR100)
colnames(Mapping100) <- c("Lamda_1.1", "Lamda_1.22474", "Lamda_1.5", "Lamda_1.7", "Lamda_1.9", "Lamda_2.1", "CRR")


ggplot(Mapping100)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.1), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping100)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.22474), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping100)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.5), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping100)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.7), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping100)+
  geom_line(mapping = aes(x = Periods, y = Lamda_1.9), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()

ggplot(Mapping100)+
  geom_line(mapping = aes(x = Periods, y = Lamda_2.1), col ='deeppink4')+
  geom_line(mapping = aes(x = Periods, y = CRR), col ='cornflowerblue')+
  xlab("Number of Iterations")+
  ylab("Distance from Black-Scholes")+
  theme_classic()


#########################################################################################
############################## Sensitivity Analysis (At The Money) ######################
#########################################################################################


################################### volatility (sigma) ##################################

Periods = seq(5,100)
sigma  = c(0.15, 0.18, 0.24)
DiffKamRitsigma = matrix(0, length(Periods), length(sigma))
KamRitsigma = matrix(0, length(Periods), length(sigma))
DiffCRRsigma = matrix(0, length(Periods), length(sigma))
CRRsigma = matrix(0, length(Periods), length(sigma))
RealValuesigma = matrix(0, 1, length(sigma))

for (j in seq(1, length(sigma))) {
  RealValuesigma[j] = KammradAmericanPut(100,100,1,1000,1.22474,sigma[j],0.05)
}

for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(sigma))) {
    CRRsigma[i,j] = BinTreeAmPut(100,100,1,Periods[i],sigma[j],0.05)
    KamRitsigma[i,j] = KammradAmericanPut(100,100,1,Periods[i],1.22474,sigma[j],0.05)
    DiffCRRsigma[i,j] = - CRRsigma[i,j] + RealValuesigma[j]
    DiffKamRitsigma[i,j] =  - KamRitsigma[i,j] + RealValuesigma[j]
  }
}

colnames(CRRsigma) <- as.character(sigma)
rownames(CRRsigma) <- as.character(Periods)

colnames(KamRitsigma) <- as.character(sigma)
rownames(KamRitsigma) <- as.character(Periods)

colnames(RealValuesigma) <- as.character(sigma)

colnames(DiffCRRsigma) <- as.character(sigma)
rownames(DiffCRRsigma) <- as.character(Periods)

colnames(DiffKamRitsigma) <- as.character(sigma)
rownames(DiffKamRitsigma) <- as.character(Periods)

#write.xlsx(CRRsigma[c(z5,z10,z20,z40,z60,z80,z100),], "CRRsigma.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(KamRitsigma[c(z5,z10,z20,z40,z60,z80,z100),], "KamRitsigma.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(RealValuesigma, "RealValuesigma.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffCRRsigma[c(z5,z10,z20,z40,z60,z80,z100),], "DiffCRRsigma.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffKamRitsigma[c(z5,z10,z20,z40,z60,z80,z100),], "DiffKamRitsigma.xlsx", colNames = TRUE, rowNames = TRUE)


################################### risk free rate (r) ##################################

Periods = seq(5,100)
riskfree = c(0.01, 0.04, 0.07)
DiffKamRitrf = matrix(0, length(Periods), length(riskfree))
KamRitrf = matrix(0, length(Periods), length(riskfree))
DiffCRRrf = matrix(0, length(Periods), length(riskfree))
CRRrf = matrix(0, length(Periods), length(riskfree))
RealValuerf = matrix(0, 1, length(riskfree))

for (j in seq(1,length(riskfree))) {
  RealValuerf[j] = KammradAmericanPut(100,100,1,1000,1.22474,0.2,riskfree[j])
}

for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(riskfree))) {
    CRRrf[i,j] = BinTreeAmPut(100,100,1,Periods[i],0.2,riskfree[j])
    KamRitrf[i,j] = KammradAmericanPut(100,100,1,Periods[i],1.22474,0.2,riskfree[j])
    DiffCRRrf[i,j] = -CRRrf[i,j] + RealValuerf[j]
    DiffKamRitrf[i,j] = -  KamRitrf[i,j] + RealValuerf[j]
  }
}

colnames(CRRrf) <- as.character(riskfree)
rownames(CRRrf) <- as.character(Periods)

colnames(KamRitrf) <- as.character(riskfree)
rownames(KamRitrf) <- as.character(Periods)

colnames(RealValuerf) <- as.character(riskfree)

colnames(DiffCRRrf) <- as.character(riskfree)
rownames(DiffCRRrf) <- as.character(Periods)

colnames(DiffKamRitrf) <- as.character(riskfree)
rownames(DiffKamRitrf) <- as.character(Periods)

#write.xlsx(CRRrf[c(z5,z10,z20,z40,z60,z80,z100),], "CRRrf.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(KamRitrf[c(z5,z10,z20,z40,z60,z80,z100),], "KamRitrf.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(RealValuerf, "RealValuerf.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffCRRrf[c(z5,z10,z20,z40,z60,z80,z100),], "DiffCRRrf.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffKamRitrf[c(z5,z10,z20,z40,z60,z80,z100),], "DiffKamRitrf.xlsx", colNames = TRUE, rowNames = TRUE)

################################### Time (T) ##################################

Periods = seq(5,100)
time = c(2/12, 5/12, 8/12)
DiffKamRittime = matrix(0, length(Periods), length(time))
KamRittime = matrix(0, length(Periods), length(time))
DiffCRRtime = matrix(0, length(Periods), length(time))
CRRtime = matrix(0, length(Periods), length(time))
RealValuetime = matrix(0, 1, length(time))

for (j in seq(1,length(time))) {
  RealValuetime[j] = KammradAmericanPut(100,100,time[j],1000,1.22474,0.2,0.05)
}

for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(time))) {
    CRRtime[i,j] = BinTreeAmPut(100,100,time[j],Periods[i],0.2,0.05)
    KamRittime[i,j] = KammradAmericanPut(100,100,time[j],Periods[i],1.22474,0.2,0.05)
    DiffCRRtime[i,j] = - CRRtime[i,j] + RealValuetime[j]
    DiffKamRittime[i,j] = - KamRittime[i,j] + RealValuetime[j]
  }
}

colnames(CRRtime) <- as.character(time)
rownames(CRRtime) <- as.character(Periods)

colnames(KamRittime) <- as.character(time)
rownames(KamRittime) <- as.character(Periods)

colnames(RealValuetime) <- as.character(time)

colnames(DiffCRRtime) <- as.character(time)
rownames(DiffCRRtime) <- as.character(Periods)

colnames(DiffKamRittime) <- as.character(time)
rownames(DiffKamRittime) <- as.character(Periods)

#write.xlsx(CRRtime[c(z5,z10,z20,z40,z60,z80,z100),], "CRRtime.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(KamRittime[c(z5,z10,z20,z40,z60,z80,z100),], "KamRittime.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(RealValuetime, "RealValuetime.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffCRRtime[c(z5,z10,z20,z40,z60,z80,z100),], "DiffCRRtime.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffKamRittime[c(z5,z10,z20,z40,z60,z80,z100),], "DiffKamRittime.xlsx", colNames = TRUE, rowNames = TRUE)