library(ggplot2)
library(openxlsx)

####################### DO NOT RUN THESE ROWS. IMPORT WORKSPACE#################

n = 2000
So = matrix(round(runif(n, min = 95, max = 105),6), n, 1)
T = matrix(round(runif(n, min = 0.1, max = 1),6), n, 1) 
sigma = matrix(round(runif(n, min = 0.16, max = 0.45),6), n, 1)  
r = matrix(round(runif(n, min = 0, max = 0.15),6), n, 1)  

Periods = c(25,50,100,200,400,800)

BS = matrix(0, n, 1)
CRR = matrix(0, n, length(Periods))
colnames(CRR) = as.character(Periods)
KR = matrix(0, n, length(Periods))
colnames(KR) = as.character(Periods)


RMSE_CRR = matrix(0, length(Periods), 1)
RMSE_KR = matrix(0, length(Periods), 1) 


for (i in 1:n) {
  BS[i] = BlackScholesEuCall(So[i], 100, T[i], sigma[i], r[i])
  
  for (j in seq(1,length(Periods))){
    CRR[i,j] = BinTreeEuPut(So[i], 100, T[i], Periods[j], sigma[i], r[i])
    KR[i,j] = KammradAmEuPut(So[i], 100, T[i], Periods[j], 1.22474, sigma[i], r[i])
  }
}

#write.xlsx(BS, "BS.xlsx", colNames = FALSE, rowNames = FALSE)
#write.xlsx(CRR, "CRR.xlsx", colNames = TRUE, rowNames = FALSE)
#write.xlsx(KR, "KR.xlsx", colNames = TRUE, rowNames = FALSE)


y = which(BS >= 0.5)

for (i in seq(1,length(Periods))) {
  RMSE_CRR[i] = sqrt(mean(((CRR[y,i] - BS[y]) / BS[y])^2))
  RMSE_KR[i] = sqrt(mean(((KR[y,i] - BS[y]) / BS[y])^2))
}

RMSE_Matrix = data.frame(RMSE_CRR, RMSE_KR)
colnames(RMSE_Matrix) = c("RMSE_CRR", "RMSE_KR")
rownames(RMSE_Matrix) = as.character(Periods)
#write.xlsx(RMSE_Matrix, "RMSE_Matrix.xlsx", colNames = TRUE, rowNames = TRUE)


ggplot(RMSE_Matrix)+
  geom_line(mapping = aes(log(Periods), log(RMSE_CRR), color = 'CRR'))+
  geom_line(mapping = aes(log(Periods), log(RMSE_KR), color = 'KR'))+
  scale_color_manual(values = c('CRR' = 'deeppink4', 'KR' = 'cornflowerblue')) +
  labs(color = '')+
  xlab("Log_Iterations")+
  ylab("Log_RMSE")+
  theme_classic()+
  theme(legend.position = "top")

