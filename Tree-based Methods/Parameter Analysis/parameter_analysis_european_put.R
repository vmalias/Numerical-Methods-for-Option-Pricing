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
BS = matrix(0, 1, length(K))


for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(K))) {
    CRR[i,j] = BinTreeEuPut(100,K[j],1,Periods[i],0.2,0.05)
    KamRit[i,j] = KammradAmEuPut(100,K[j],1,Periods[i],1.22474,0.2,0.05)
    BS[j] = BlackScholesEuPut(100,K[j],1,0.2,0.05)
    DiffCRR[i,j] = - BinTreeEuPut(100,K[j],1,Periods[i],0.2,0.05) + BlackScholesEuPut(100,K[j],1,0.2,0.05)
    DiffKamRit[i,j] =  - KammradAmEuPut(100,K[j],1,Periods[i],1.22474,0.2,0.05) + BlackScholesEuPut(100,K[j],1,0.2,0.05)
  }
}

colnames(CRR) <- as.character(K)
rownames(CRR) <- as.character(Periods)

colnames(KamRit) <- as.character(K)
rownames(KamRit) <- as.character(Periods)

colnames(BS) <- as.character(K)

colnames(DiffCRR) <- as.character(K)
rownames(DiffCRR) <- as.character(Periods)

colnames(DiffKamRit) <- as.character(K)
rownames(DiffKamRit) <- as.character(Periods)

#write.xlsx(CRR, "CRR.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(KamRit, "KamRit.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(BS, "BS.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffCRR, "DiffCRR.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffKamRit, "DiffKamRit.xlsx", colNames = TRUE, rowNames = TRUE)
MappingSo = data.frame((DiffCRR[nrow(DiffCRR),]), (DiffKamRit[nrow(DiffKamRit),]))
colnames(MappingSo) = c("DiffCRR" , "DiffKamRit")

ggplot(MappingSo)+
  geom_line(mapping = aes(x = K, y = DiffKamRit, col = "KR"))+
  geom_line(mapping = aes(x = K, y = DiffCRR, col = "CRR"))+
  scale_color_manual(values = c('KR' = 'cornflowerblue', 'CRR' = 'deeppink4')) +
  labs(color = '')+
  xlab("Strike Price (K)")+
  ylab("Distance of Black-Scholes")+
  theme_classic()+
  theme(legend.position = "right")+
  ggtitle("Strike Price Sensitivity Analysis European Put")



########################################################################################
##################################   K = 90    #########################################
########################################################################################

Periods = seq(5,100)
lamda  = seq(1.1,2.1,0.2)
lamda[2] = 1.22474
DiffKamRit90 = matrix(0, length(Periods), length(lamda))
DiffCRR90 = matrix(0, length(Periods), 1)

for (i in seq(1,length(Periods))) {
  DiffCRR90[i] = - BinTreeEuPut(100,90,1,Periods[i],0.2,0.05) + BlackScholesEuPut(100,90,1,0.2,0.05)
  for (j in seq(1,length(lamda))) {
    DiffKamRit90[i,j] = - KammradAmEuPut(100,90,1,Periods[i],lamda[j],0.2,0.05) + BlackScholesEuPut(100,90,1,0.2,0.05)
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
DiffCRR110 = matrix(0, length(Periods), 1)

for (i in seq(1,length(Periods))) {
  DiffCRR110[i] = - BinTreeEuPut(100,110,1,Periods[i],0.2,0.05) + BlackScholesEuPut(100,110,1,0.2,0.05)
  for (j in seq(1,length(lamda))) {
    DiffKamRit110[i,j] = - KammradAmEuPut(100,110,1,Periods[i],lamda[j],0.2,0.05) + BlackScholesEuPut(100,110,1,0.2,0.05)
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
DiffCRR100 = matrix(0, length(Periods), 1)

for (i in seq(1,length(Periods))) {
  DiffCRR100[i] = - BinTreeEuPut(100,100,1,Periods[i],0.2,0.05) + BlackScholesEuPut(100,100,1,0.2,0.05)
  for (j in seq(1,length(lamda))) {
    DiffKamRit100[i,j] = - KammradAmEuPut(100,100,1,Periods[i],lamda[j],0.2,0.05) + BlackScholesEuPut(100,100,1,0.2,0.05)
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


######## Odd and Even Steps for CRR (Unnecessary because of put-call parity) ##########
############################### Same as European Call #################################

Periods = seq(5,100)

i = which(Periods %% 2 == 0, arr.ind = T)
j = which(Periods %% 2 == 1, arr.ind = T)

EvenPeriods = Periods[i]
DiffCRREvens = matrix(1, length(EvenPeriods))

OddPeriods = Periods[j]
DiffCRROdds = matrix(1, length(OddPeriods))

for(k in seq(1,length(EvenPeriods))){
  DiffCRREvens[k] = - BinTreeEuPut(100,100,1,EvenPeriods[k],0.2,0.05) + BlackScholesEuPut(100,100,1,0.2,0.05)
}

for (n in seq(1,length(OddPeriods))) {
  DiffCRROdds[n] = - BinTreeEuPut(100,100,1,OddPeriods[n],0.2,0.05) + BlackScholesEuPut(100,100,1,0.2,0.05)
}

ggplot()+
  geom_line(aes(EvenPeriods,DiffCRREvens), col = "cornflowerblue")+
  geom_line(aes(OddPeriods,DiffCRROdds), col = "deeppink4")+
  geom_line(aes(Periods,DiffCRR100), col = "darkolivegreen4")+
  xlab("Number of Iterations")+
  ylab("Distance of Black-Scholes")+
  theme_classic()+
  ggtitle("CRR Even-Odd Steps (ATM)")


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
BSsigma = matrix(0, 1, length(sigma))


for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(sigma))) {
    CRRsigma[i,j] = BinTreeEuPut(100,100,1,Periods[i],sigma[j],0.05)
    KamRitsigma[i,j] = KammradAmEuPut(100,100,1,Periods[i],1.22474,sigma[j],0.05)
    BSsigma[j] = BlackScholesEuPut(100,100,1,sigma[j],0.05)
    DiffCRRsigma[i,j] = - BinTreeEuPut(100,100,1,Periods[i],sigma[j],0.05) + BlackScholesEuPut(100,100,1,sigma[j],0.05)
    DiffKamRitsigma[i,j] =  - KammradAmEuPut(100,100,1,Periods[i],1.22474,sigma[j],0.05) + BlackScholesEuPut(100,100,1,sigma[j],0.05)
  }
}

colnames(CRRsigma) <- as.character(sigma)
rownames(CRRsigma) <- as.character(Periods)

colnames(KamRitsigma) <- as.character(sigma)
rownames(KamRitsigma) <- as.character(Periods)

colnames(BSsigma) <- as.character(sigma)

colnames(DiffCRRsigma) <- as.character(sigma)
rownames(DiffCRRsigma) <- as.character(Periods)

colnames(DiffKamRitsigma) <- as.character(sigma)
rownames(DiffKamRitsigma) <- as.character(Periods)

#write.xlsx(CRRsigma, "CRRsigma.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(KamRitsigma, "KamRitsigma.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(BSsigma, "BSsigma.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffCRRsigma, "DiffCRRsigma.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffKamRitsigma, "DiffKamRitsigma.xlsx", colNames = TRUE, rowNames = TRUE)


######################### PLOTS (Volatility) ####################################

Sigma = seq(0.01, 0.5, 0.01)
DiffCRR_sigma = matrix(0, 1, length(Sigma))
DiffKamRit_sigma = matrix(0, 1, length(Sigma))

for (k in seq(1,length(Sigma))) {
  DiffCRR_sigma[k] = - BinTreeEuPut(100,100,1,80,Sigma[k],0.05) + BlackScholesEuPut(100,100,1,Sigma[k],0.05)
  DiffKamRit_sigma[k] = - KammradAmEuPut(100,100,1,80,1.22474,Sigma[k],0.05) + BlackScholesEuPut(100,100,1,Sigma[k],0.05)
}

colnames(DiffCRR_sigma) = as.character(Sigma)
colnames(DiffKamRit_sigma) = as.character(Sigma)

Mapping_sigma = data.frame(t(DiffCRR_sigma), t(DiffKamRit_sigma))
colnames(Mapping_sigma) = c("CRR", "KR")

ggplot(Mapping_sigma)+
  geom_line(mapping = aes(Sigma, KR, color = 'KR'))+
  geom_line(mapping = aes(Sigma, CRR, color = 'CRR'))+
  scale_color_manual(values = c('KR' = 'cornflowerblue', 'CRR' = 'deeppink4')) +
  labs(color = '')+
  xlab("Volatility")+
  ylab("Distance of Black-Scholes")+
  theme_classic()+
  theme(legend.position = "right")+
  ggtitle("Volatility Analysis European Put")


################################### risk free rate (r) ##################################

Periods = seq(5,100)
riskfree = c(0.01, 0.04, 0.07)
DiffKamRitrf = matrix(0, length(Periods), length(sigma))
KamRitrf = matrix(0, length(Periods), length(sigma))
DiffCRRrf = matrix(0, length(Periods), length(sigma))
CRRrf = matrix(0, length(Periods), length(sigma))
BSrf = matrix(0, 1, length(sigma))


for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(riskfree))) {
    CRRrf[i,j] = BinTreeEuPut(100,100,1,Periods[i],0.2,riskfree[j])
    KamRitrf[i,j] = KammradAmEuPut(100,100,1,Periods[i],1.22474,0.2,riskfree[j])
    BSrf[j] = BlackScholesEuPut(100,100,1,0.2,riskfree[j])
    DiffCRRrf[i,j] = - BinTreeEuPut(100,100,1,Periods[i],0.2,riskfree[j]) + BlackScholesEuPut(100,100,1,0.2,riskfree[j])
    DiffKamRitrf[i,j] = - KammradAmEuPut(100,100,1,Periods[i],1.22474,0.2,riskfree[j]) + BlackScholesEuPut(100,100,1,0.2,riskfree[j])
  }
}

colnames(CRRrf) <- as.character(riskfree)
rownames(CRRrf) <- as.character(Periods)

colnames(KamRitrf) <- as.character(riskfree)
rownames(KamRitrf) <- as.character(Periods)

colnames(BSrf) <- as.character(riskfree)

colnames(DiffCRRrf) <- as.character(riskfree)
rownames(DiffCRRrf) <- as.character(Periods)

colnames(DiffKamRitrf) <- as.character(riskfree)
rownames(DiffKamRitrf) <- as.character(Periods)

#write.xlsx(CRRrf, "CRRrf.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(KamRitrf, "KamRitrf.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(BSrf, "BSrf.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffCRRrf, "DiffCRRrf.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffKamRitrf, "DiffKamRitrf.xlsx", colNames = TRUE, rowNames = TRUE)

######################### PLOTS (risk free) ####################################

risk_free = seq(0, 0.15, 0.01)
DiffCRR_rf = matrix(0, 1, length(risk_free))
DiffKamRit_rf = matrix(0, 1, length(risk_free))

for (k in seq(1,length(risk_free))) {
  DiffCRR_rf[k] = - BinTreeEuPut(100,100,1,80,0.2,risk_free[k]) + BlackScholesEuPut(100,100,1,0.2,risk_free[k])
  DiffKamRit_rf[k] = - KammradAmEuPut(100,100,1,80,1.22474,0.2,risk_free[k]) + BlackScholesEuPut(100,100,1,0.2,risk_free[k])
}

colnames(DiffCRR_rf) = as.character(risk_free)
colnames(DiffKamRit_rf) = as.character(risk_free)

Mapping_rf = data.frame(t(DiffCRR_rf), t(DiffKamRit_rf))
colnames(Mapping_rf) = c("CRR", "KR")

ggplot(Mapping_rf)+
  geom_line(mapping = aes(risk_free, KR, color = 'KR'))+
  geom_line(mapping = aes(risk_free, CRR, color = 'CRR'))+
  scale_color_manual(values = c('KR' = 'cornflowerblue', 'CRR' = 'deeppink4')) +
  labs(color = '')+
  xlab("Risk Free")+
  ylab("Distance of Black-Scholes")+
  theme_classic()+
  theme(legend.position = "right")+
  ggtitle("Risk Free Analysis European Put")


################################### Time (T) ##################################

Periods = seq(5,100)
time = c(2/12, 5/12, 8/12)
DiffKamRittime = matrix(0, length(Periods), length(time))
KamRittime = matrix(0, length(Periods), length(time))
DiffCRRtime = matrix(0, length(Periods), length(time))
CRRtime = matrix(0, length(Periods), length(time))
BStime = matrix(0, 1, length(time))


for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(time))) {
    CRRtime[i,j] = BinTreeEuPut(100,100,time[j],Periods[i],0.2,0.05)
    KamRittime[i,j] = KammradAmEuPut(100,100,time[j],Periods[i],1.22474,0.2,0.05)
    BStime[j] = BlackScholesEuPut(100,100,time[j],0.2,0.05)
    DiffCRRtime[i,j] = - BinTreeEuPut(100,100,time[j],Periods[i],0.2,0.05) + BlackScholesEuPut(100,100,time[j],0.2,0.05)
    DiffKamRittime[i,j] = - KammradAmEuPut(100,100,time[j],Periods[i],1.22474,0.2,0.05) + BlackScholesEuPut(100,100,time[j],0.2,0.05)
  }
}

colnames(CRRtime) <- as.character(time)
rownames(CRRtime) <- as.character(Periods)

colnames(KamRittime) <- as.character(time)
rownames(KamRittime) <- as.character(Periods)

colnames(BStime) <- as.character(time)

colnames(DiffCRRtime) <- as.character(time)
rownames(DiffCRRtime) <- as.character(Periods)

colnames(DiffKamRittime) <- as.character(time)
rownames(DiffKamRittime) <- as.character(Periods)

#write.xlsx(CRRtime, "CRRtime.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(KamRittime, "KamRittime.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(BStime, "BStime.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffCRRtime, "DiffCRRtime.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(DiffKamRittime, "DiffKamRittime.xlsx", colNames = TRUE, rowNames = TRUE)

######################### PLOTS (Time) #########################################

Time = seq(0.01, 1, 0.01)
DiffCRR_time = matrix(0, 1, length(Time))
DiffKamRit_time = matrix(0, 1, length(Time))

for (k in seq(1,length(Time))) {
  DiffCRR_time[k] = - BinTreeEuPut(100,100,Time[k],80,0.2,0.05) + BlackScholesEuPut(100,100,Time[k],0.2,0.05)
  DiffKamRit_time[k] = - KammradAmEuPut(100,100,Time[k],80,1.22474,0.2,0.05) + BlackScholesEuPut(100,100,Time[k],0.2,0.05)
}

colnames(DiffCRR_time) = as.character(Time)
colnames(DiffKamRit_time) = as.character(Time)

Mapping_time = data.frame(t(DiffCRR_time), t(DiffKamRit_time))
colnames(Mapping_time) = c("CRR", "KR")

ggplot(Mapping_time)+
  geom_line(mapping = aes(Time, KR, color = 'KR'))+
  geom_line(mapping = aes(Time, CRR, color = 'CRR'))+
  scale_color_manual(values = c('KR' = 'cornflowerblue', 'CRR' = 'deeppink4')) +
  labs(color = '')+
  xlab("Time")+
  ylab("Distance of Black-Scholes")+
  theme_classic()+
  theme(legend.position = "right")+
  ggtitle("Time Analysis European Put")

