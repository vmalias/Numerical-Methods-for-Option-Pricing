library(ggplot2)
library(openxlsx)

stock = seq(from=13, to=17, by=1)
dS = c(5,3,0.9)

############################### European Call ##################################

Implicit_Value_EuCall = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Implicit_Value_EuCall) = as.character(stock) 
colnames(Implicit_Value_EuCall) = as.character(dS)
Implicit_Error_EuCall = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Implicit_Error_EuCall) = as.character(stock) 
colnames(Implicit_Error_EuCall) = as.character(dS)
Explicit_Value_EuCall = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Explicit_Value_EuCall) = as.character(stock) 
colnames(Explicit_Value_EuCall) = as.character(dS)
Explicit_Error_EuCall = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Explicit_Error_EuCall) = as.character(stock) 
colnames(Explicit_Error_EuCall) = as.character(dS)

BlackScholes_Value_EuCall = BlackScholesEuCall(stock,15,6/12,0.35,0.07)

for (i in 1:length(stock)) {
  for (j in 1:length(dS)){
    Implicit_Value_EuCall[i,j] = round(BS_implicit_EuCall(stock[i],15,6/12,0.35,0.07,50,dS[j],5/1200),5)
    Implicit_Error_EuCall[i,j] = round(BlackScholes_Value_EuCall[i] - Implicit_Value_EuCall[i,j],5) 
    Explicit_Value_EuCall[i,j] = round(BS_explicit_EuCall(stock[i],15,6/12,0.35,0.07,50,dS[j],5/1200),5)
    Explicit_Error_EuCall[i,j] = round(BlackScholes_Value_EuCall[i] - Explicit_Value_EuCall[i,j],5) 
  }
}

#write.xlsx(Implicit_Value_EuCall, "Implicit_Value_EuCall.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(Explicit_Value_EuCall, "Explicit_Value_EuCall.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(Implicit_Error_EuCall, "Implicit_Error_EuCall.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(Explicit_Error_EuCall, "Explicit_Error_EuCall.xlsx", colNames = TRUE, rowNames = TRUE)


################################################################################
###ta grafimata egina gia stock = seq(from=5, to=25, by=1) kai dS = c(0.5,1)#### 
################################################################################
#
#ggplot()+
#  geom_line(mapping = aes(stock, abs(Implicit_Error_EuCall[1,]), linetype = 'solid', color = 'Implicit'))+
#  geom_point(mapping = aes(stock, abs(Implicit_Error_EuCall[1,]), color = 'Implicit'), shape = 1)+
#  geom_line(mapping = aes(stock, abs(Explicit_Error_EuCall[1,]), linetype = 'dashed', color = 'Explicit'))+
#  geom_point(mapping = aes(stock, abs(Explicit_Error_EuCall[1,]), color = 'Explicit'), shape = 1)+
#  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
#  scale_linetype_manual(values = c('dashed', 'dashed'), guide = "none")+
#  labs(title = '', 
#       x = '',
#       y = '',
#       color = '')+
#  theme_classic()+
#  theme(legend.position = "top")

#ggplot()+
#  geom_line(mapping = aes(stock, abs(Implicit_Error_EuCall[2,]), linetype = 'solid', color = 'Implicit'))+
#  geom_point(mapping = aes(stock, abs(Implicit_Error_EuCall[2,]), color = 'Implicit'), shape = 1)+
#  geom_line(mapping = aes(stock, abs(Explicit_Error_EuCall[2,]), linetype = 'dashed', color = 'Explicit'))+
#  geom_point(mapping = aes(stock, abs(Explicit_Error_EuCall[2,]), color = 'Explicit'), shape = 1)+
#  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
#  scale_linetype_manual(values = c('dashed', 'dashed'), guide = "none")+
#  labs(title = '', 
#       x = '',
#       y = '',
#       color = '')+
#  theme_classic()+
#  theme(legend.position = "top")


############################### European Put ###################################


Implicit_Value_EuPut = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Implicit_Value_EuPut) = as.character(stock) 
colnames(Implicit_Value_EuPut) = as.character(dS)
Implicit_Error_EuPut = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Implicit_Error_EuPut) = as.character(stock) 
colnames(Implicit_Error_EuPut) = as.character(dS)
Explicit_Value_EuPut = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Explicit_Value_EuPut) = as.character(stock) 
colnames(Explicit_Value_EuPut) = as.character(dS)
Explicit_Error_EuPut = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Explicit_Error_EuPut) = as.character(stock) 
colnames(Explicit_Error_EuPut) = as.character(dS)

BlackScholes_Value_EuPut = BlackScholesEuPut(stock,15,6/12,0.35,0.07)

for (i in 1:length(stock)) {
  for (j in 1:length(dS)){
    Implicit_Value_EuPut[i,j] = round(BS_implicit_EuPut(stock[i],15,6/12,0.35,0.07,50,dS[j],5/1200),5)
    Implicit_Error_EuPut[i,j] = round(BlackScholes_Value_EuPut[i] - Implicit_Value_EuPut[i,j],5) 
    Explicit_Value_EuPut[i,j] = round(BS_explicit_EuPut(stock[i],15,6/12,0.35,0.07,50,dS[j],5/1200),5)
    Explicit_Error_EuPut[i,j] = round(BlackScholes_Value_EuPut[i] - Explicit_Value_EuPut[i,j],5) 
  }
}

#write.xlsx(Implicit_Value_EuPut, "Implicit_Value_EuPut.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(Explicit_Value_EuPut, "Explicit_Value_EuPut.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(Implicit_Error_EuPut, "Implicit_Error_EuPut.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(Explicit_Error_EuPut, "Explicit_Error_EuPut.xlsx", colNames = TRUE, rowNames = TRUE)


################################################################################
### ta grafimata egina gia stock = seq(from=5, to=25, by=1) kai dS = c(0.5,1)### 
################################################################################
#
#ggplot()+
#  geom_line(mapping = aes(stock, abs(Implicit_Error_EuPut[1,]), linetype = 'solid', color = 'Implicit'))+
#  geom_point(mapping = aes(stock, abs(Implicit_Error_EuPut[1,]), color = 'Implicit'), shape = 1)+
#  geom_line(mapping = aes(stock, abs(Explicit_Error_EuPut[1,]), linetype = 'dashed', color = 'Explicit'))+
#  geom_point(mapping = aes(stock, abs(Explicit_Error_EuPut[1,]), color = 'Explicit'), shape = 1)+
#  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
#  scale_linetype_manual(values = c('dashed', 'dashed'), guide = "none")+
#  labs(title = '', 
#       x = '',
#       y = '',
#       color = '')+
#  theme_classic()+
#  theme(legend.position = "top")

#ggplot()+
#  geom_line(mapping = aes(stock, abs(Implicit_Error_EuPut[2,]), linetype = 'solid', color = 'Implicit'))+
#  geom_point(mapping = aes(stock, abs(Implicit_Error_EuPut[2,]), color = 'Implicit'), shape = 1)+
#  geom_line(mapping = aes(stock, abs(Explicit_Error_EuPut[2,]), linetype = 'dashed', color = 'Explicit'))+
#  geom_point(mapping = aes(stock, abs(Explicit_Error_EuPut[2,]), color = 'Explicit'), shape = 1)+
#  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
#  scale_linetype_manual(values = c('dashed', 'dashed'), guide = "none")+
#  labs(title = '', 
#       x = '',
#       y = '',
#       color = '')+
#  theme_classic()+
#  theme(legend.position = "top")




############################### American Put ###################################



Implicit_Value_AmPut = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Implicit_Value_AmPut) = as.character(stock) 
colnames(Implicit_Value_AmPut) = as.character(dS)
Implicit_Error_AmPut = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Implicit_Error_AmPut) = as.character(stock) 
colnames(Implicit_Error_AmPut) = as.character(dS)
Explicit_Value_AmPut = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Explicit_Value_AmPut) = as.character(stock) 
colnames(Explicit_Value_AmPut) = as.character(dS)
Explicit_Error_AmPut = matrix(0, nrow=length(stock), ncol=length(dS))
rownames(Explicit_Error_AmPut) = as.character(stock) 
colnames(Explicit_Error_AmPut) = as.character(dS)

KamradRitchken_Value_AmPut = matrix(0, nrow = length(stock), ncol = 1)

for (i in 1:length(stock)) {
  KamradRitchken_Value_AmPut[i] = KammradAmericanPut(stock[i],15,6/12,1000,1.22474,0.35,0.07)
}

for (i in 1:length(stock)) {
  for (j in 1:length(dS)){
    Implicit_Value_AmPut[i,j] = round(BS_implicit_AmPut_Gauss_Seidel(stock[i],15,6/12,0.35,0.07,50,dS[j],5/1200),5)
    Implicit_Error_AmPut[i,j] = round(KamradRitchken_Value_AmPut[i] - Implicit_Value_AmPut[i,j],5)
    Explicit_Value_AmPut[i,j] = round(BS_explicit_AmPut(stock[i],15,6/12,0.35,0.07,50,dS[j],5/1200),5)
    Explicit_Error_AmPut[i,j] = round(KamradRitchken_Value_AmPut[i] - Explicit_Value_AmPut[i,j],5)
  }
}


#write.xlsx(Implicit_Value_AmPut, "Implicit_Value_AmPut.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(Explicit_Value_AmPut, "Explicit_Value_AmPut.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(Implicit_Error_AmPut, "Implicit_Error_AmPut.xlsx", colNames = TRUE, rowNames = TRUE)
#write.xlsx(Explicit_Error_AmPut, "Explicit_Error_AmPut.xlsx", colNames = TRUE, rowNames = TRUE)


################################################################################
### ta grafimata egina gia stock = seq(from=5, to=25, by=1) kai dS = c(0.5,1)### 
################################################################################
#
#ggplot()+
#  geom_line(mapping = aes(stock, abs(Implicit_Error_AmPut[1,]), linetype = 'solid', color = 'Implicit'))+
#  geom_point(mapping = aes(stock, abs(Implicit_Error_AmPut[1,]), color = 'Implicit'), shape = 1)+
#  geom_line(mapping = aes(stock, abs(Explicit_Error_AmPut[1,]), linetype = 'dashed', color = 'Explicit'))+
#  geom_point(mapping = aes(stock, abs(Explicit_Error_AmPut[1,]), color = 'Explicit'), shape = 1)+
#  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
#  scale_linetype_manual(values = c('dashed', 'dashed'), guide = "none")+
#  labs(title = '', 
#       x = '',
#       y = '',
#       color = '')+
#  theme_classic()+
#  theme(legend.position = "top")

#ggplot()+
#  geom_line(mapping = aes(stock, abs(Implicit_Error_AmPut[2,]), linetype = 'solid', color = 'Implicit'))+
#  geom_point(mapping = aes(stock, abs(Implicit_Error_AmPut[2,]), color = 'Implicit'), shape = 1)+
#  geom_line(mapping = aes(stock, abs(Explicit_Error_AmPut[2,]), linetype = 'dashed', color = 'Explicit'))+
#  geom_point(mapping = aes(stock, abs(Explicit_Error_AmPut[2,]), color = 'Explicit'), shape = 1)+
#  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
#  scale_linetype_manual(values = c('dashed', 'dashed'), guide = "none")+
#  labs(title = '', 
#       x = '',
#       y = '',
#       color = '')+
#  theme_classic()+
#  theme(legend.position = "top")

