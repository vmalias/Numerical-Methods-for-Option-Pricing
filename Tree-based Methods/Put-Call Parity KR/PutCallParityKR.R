library(ggplot2)

Periods = seq(5,300)
PutCallParity = matrix(0, length(Periods), 1)
BS = matrix(BlackScholesEuCall(100,100,1,0.2,0.05) - BlackScholesEuPut(100,100,1,0.2,0.05), length(Periods), 1)

for (i in seq(1, length(Periods))) {
PutCallParity[i] = KammradAmEuCall(100,100,1,Periods[i],1.22474,0.2,0.05) - KammradAmEuPut(100,100,1,Periods[i],1.22474,0.2,0.05)
}

PutCallMapping = data.frame(PutCallParity, BS)
rownames(PutCallMapping) = as.character(Periods)
colnames(PutCallMapping) = c("PutCallParity", "BS")

ggplot(PutCallMapping)+
  geom_line(mapping = aes(Periods, PutCallParity, color = 'KR'))+
  geom_line(mapping = aes(Periods, BS, color = 'Black Scholes'))+
  scale_color_manual(values = c('KR' = 'deeppink4', 'Black Scholes' = 'darkolivegreen4')) +
  labs(color = '')+
  xlab("Number of Iterations")+
  ylab("Put-Call Parity")+
  theme_classic()+
  theme(legend.position = "right")+
  ggtitle("Put-Call Parity Convergence for KR")
