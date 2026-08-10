library(ggplot2)
#library(xlsx)

So=c(13,15,17); K=15; T=6/12; sigma=0.35; r=0.07; Smax=50; dS = 3
dt = seq(from=125/1200, to=5/1200, by=-20/1200)

###########################################################################
###########################################################################
#####################  EUROPEAN CALL  #####################################
###########################################################################
###########################################################################

Explicit_Error_EuCall = matrix(0,nrow=length(dt),ncol=length(So))
rownames(Explicit_Error_EuCall) = as.character(dt)
colnames(Explicit_Error_EuCall) = as.character(So)

Implicit_Error_EuCall = matrix(0,nrow=length(dt),ncol=length(So))
rownames(Implicit_Error_EuCall) = as.character(dt)
colnames(Implicit_Error_EuCall) = as.character(So)


Real_Value_EuCall = matrix(BlackScholesEuCall(So,K,T,sigma,r),nrow = 1, ncol = length(So))
rownames(Real_Value_EuCall) = 'Real Value'
colnames(Real_Value_EuCall) = as.character(So)


for (i in 1:length(dt)) {
  for (j in 1:length(So)){
    Explicit_Error_EuCall[i,j] = Real_Value_EuCall[j] - BS_explicit_EuCall(So[j],K,T,sigma,r,Smax,dS,dt[i])
    Implicit_Error_EuCall[i,j] = Real_Value_EuCall[j] - BS_implicit_EuCall(So[j],K,T,sigma,r,Smax,dS,dt[i]) 
  }
}


###################### ITM ################################################
ITM = which(colnames(Explicit_Error_EuCall) == "17" & colnames(Implicit_Error_EuCall) == "17")

ggplot()+
  geom_line(mapping = aes(dt, Implicit_Error_EuCall[,ITM], color = 'Implicit'))+
  geom_point(mapping = aes(dt, Implicit_Error_EuCall[,ITM], color = 'Implicit'), shape = 1)+
  geom_line(mapping = aes(dt, Explicit_Error_EuCall[,ITM], color = 'Explicit'))+
  geom_point(mapping = aes(dt, Explicit_Error_EuCall[,ITM], color = 'Explicit'), shape = 1)+
  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
  scale_x_reverse()+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")


###################### ATM ################################################
ATM = which(colnames(Explicit_Error_EuCall) == "15" & colnames(Implicit_Error_EuCall) == "15")


ggplot()+
  geom_line(mapping = aes(dt, Implicit_Error_EuCall[,ATM], color = 'Implicit'))+
  geom_point(mapping = aes(dt, Implicit_Error_EuCall[,ATM], color = 'Implicit'), shape = 1)+
  geom_line(mapping = aes(dt, Explicit_Error_EuCall[,ATM], color = 'Explicit'))+
  geom_point(mapping = aes(dt, Explicit_Error_EuCall[,ATM], color = 'Explicit'), shape = 1)+
  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
  scale_x_reverse()+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")


###################### OTM ################################################
OTM = which(colnames(Explicit_Error_EuCall) == "13" & colnames(Implicit_Error_EuCall) == "13")


ggplot()+
  geom_line(mapping = aes(dt, Implicit_Error_EuCall[,OTM], color = 'Implicit'))+
  geom_point(mapping = aes(dt, Implicit_Error_EuCall[,OTM], color = 'Implicit'), shape = 1)+
  geom_line(mapping = aes(dt, Explicit_Error_EuCall[,OTM], color = 'Explicit'))+
  geom_point(mapping = aes(dt, Explicit_Error_EuCall[,OTM], color = 'Explicit'), shape = 1)+
  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
  scale_x_reverse()+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")



###########################################################################
###########################################################################
########################### EUROPEAN PUT  #################################
###########################################################################
###########################################################################

Explicit_Error_EuPut = matrix(0,nrow=length(dt),ncol=length(So))
rownames(Explicit_Error_EuPut) = as.character(dt)
colnames(Explicit_Error_EuPut) = as.character(So)

Implicit_Error_EuPut = matrix(0,nrow=length(dt),ncol=length(So))
rownames(Implicit_Error_EuPut) = as.character(dt)
colnames(Implicit_Error_EuPut) = as.character(So)


Real_Value_EuPut = matrix(BlackScholesEuPut(So,K,T,sigma,r),nrow = 1, ncol = length(So))
rownames(Real_Value_EuPut) = 'Real Value_EuPut'
colnames(Real_Value_EuPut) = as.character(So)


for (i in 1:length(dt)) {
  for (j in 1:length(So)){
    Explicit_Error_EuPut[i,j] = Real_Value_EuPut[j] - BS_explicit_EuPut(So[j],K,T,sigma,r,Smax,dS,dt[i])
    Implicit_Error_EuPut[i,j] = Real_Value_EuPut[j] - BS_implicit_EuPut(So[j],K,T,sigma,r,Smax,dS,dt[i]) 
  }
}

###################### ITM ################################################
ITM = which(colnames(Explicit_Error_EuPut) == "13" & colnames(Implicit_Error_EuPut) == "13")

ggplot()+
  geom_line(mapping = aes(dt, Implicit_Error_EuPut[,ITM], color = 'Implicit'))+
  geom_point(mapping = aes(dt, Implicit_Error_EuPut[,ITM], color = 'Implicit'), shape = 1)+
  geom_line(mapping = aes(dt, Explicit_Error_EuPut[,ITM], color = 'Explicit'))+
  geom_point(mapping = aes(dt, Explicit_Error_EuPut[,ITM], color = 'Explicit'), shape = 1)+
  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
  scale_x_reverse()+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")


###################### ATM ################################################
ATM = which(colnames(Explicit_Error_EuPut) == "15" & colnames(Implicit_Error_EuPut) == "15")


ggplot()+
  geom_line(mapping = aes(dt, Implicit_Error_EuPut[,ATM], color = 'Implicit'))+
  geom_point(mapping = aes(dt, Implicit_Error_EuPut[,ATM], color = 'Implicit'), shape = 1)+
  geom_line(mapping = aes(dt, Explicit_Error_EuPut[,ATM], color = 'Explicit'))+
  geom_point(mapping = aes(dt, Explicit_Error_EuPut[,ATM], color = 'Explicit'), shape = 1)+
  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
  scale_x_reverse()+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")


###################### OTM ################################################
OTM = which(colnames(Explicit_Error_EuPut) == "17" & colnames(Implicit_Error_EuPut) == "17")


ggplot()+
  geom_line(mapping = aes(dt, Implicit_Error_EuPut[,OTM], color = 'Implicit'))+
  geom_point(mapping = aes(dt, Implicit_Error_EuPut[,OTM], color = 'Implicit'), shape = 1)+
  geom_line(mapping = aes(dt, Explicit_Error_EuPut[,OTM], color = 'Explicit'))+
  geom_point(mapping = aes(dt, Explicit_Error_EuPut[,OTM], color = 'Explicit'), shape = 1)+
  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
  scale_x_reverse()+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")



###########################################################################
###########################################################################
########################### AMEICAN PUT  #################################
###########################################################################
###########################################################################

Explicit_Error_AmPut = matrix(0,nrow=length(dt),ncol=length(So))
rownames(Explicit_Error_AmPut) = as.character(dt)
colnames(Explicit_Error_AmPut) = as.character(So)

Implicit_Error_AmPut = matrix(0,nrow=length(dt),ncol=length(So))
rownames(Implicit_Error_AmPut) = as.character(dt)
colnames(Implicit_Error_AmPut) = as.character(So)

Real_Value_AmPut = matrix(0,nrow = 1, ncol = length(So))
for (j in 1:length(So)) {
  Real_Value_AmPut[j] = KammradAmericanPut(So[j],K,T,1000,1.22474,sigma,r)
}
rownames(Real_Value_AmPut) = 'Real Value_AmPut'
colnames(Real_Value_AmPut) = as.character(So)


for (i in 1:length(dt)) {
  for (j in 1:length(So)){
    Explicit_Error_AmPut[i,j] = Real_Value_AmPut[j] - BS_explicit_AmPut(So[j],K,T,sigma,r,Smax,dS,dt[i])
    Implicit_Error_AmPut[i,j] = Real_Value_AmPut[j] - BS_implicit_AmPut_Gauss_Seidel(So[j],K,T,sigma,r,Smax,dS,dt[i]) 
  }
}

###################### ITM ################################################
ITM = which(colnames(Explicit_Error_AmPut) == "13" & colnames(Implicit_Error_AmPut) == "13")

ggplot()+
  geom_line(mapping = aes(dt, Implicit_Error_AmPut[,ITM], color = 'Implicit'))+
  geom_point(mapping = aes(dt, Implicit_Error_AmPut[,ITM], color = 'Implicit'), shape = 1)+
  geom_line(mapping = aes(dt, Explicit_Error_AmPut[,ITM], color = 'Explicit'))+
  geom_point(mapping = aes(dt, Explicit_Error_AmPut[,ITM], color = 'Explicit'), shape = 1)+
  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
  scale_x_reverse()+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")


###################### ATM ################################################
ATM = which(colnames(Explicit_Error_AmPut) == "15" & colnames(Implicit_Error_AmPut) == "15")


ggplot()+
  geom_line(mapping = aes(dt, Implicit_Error_AmPut[,ATM], color = 'Implicit'))+
  geom_point(mapping = aes(dt, Implicit_Error_AmPut[,ATM], color = 'Implicit'), shape = 1)+
  geom_line(mapping = aes(dt, Explicit_Error_AmPut[,ATM], color = 'Explicit'))+
  geom_point(mapping = aes(dt, Explicit_Error_AmPut[,ATM], color = 'Explicit'), shape = 1)+
  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
  scale_x_reverse()+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")


###################### OTM ################################################
OTM = which(colnames(Explicit_Error_AmPut) == "17" & colnames(Implicit_Error_AmPut) == "17")


ggplot()+
  geom_line(mapping = aes(dt, Implicit_Error_AmPut[,OTM], color = 'Implicit'))+
  geom_point(mapping = aes(dt, Implicit_Error_AmPut[,OTM], color = 'Implicit'), shape = 1)+
  geom_line(mapping = aes(dt, Explicit_Error_AmPut[,OTM], color = 'Explicit'))+
  geom_point(mapping = aes(dt, Explicit_Error_AmPut[,OTM], color = 'Explicit'), shape = 1)+
  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'cornflowerblue')) +
  scale_x_reverse()+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")



