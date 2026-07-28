library(ggplot2)

matval = matrix(0, nrow=1, ncol=length(seq(from=5, to=15, by=1)))
error = matrix(0, nrow=1, ncol=length(seq(from=5, to=25, by=1)))
matval1 = matrix(0, nrow=1, ncol=length(seq(from=5, to=25, by=1)))
error1 = matrix(0, nrow=1, ncol=length(seq(from=5, to=25, by=1)))

stock = seq(from=5, to=25, by=1)

for (i in 1:length(stock)){
  matval[i] = BS_implicit_EuCall(stock[i],15,6/12,0.2,0.05,50,1,5/2400)
  error[i] = BlackScholesEuCall(stock[i],15,6/12,0.2,0.05) - matval[i] 
}


for (i in 1:length(stock)){
  matval1[i] = BS_explicit_EuCall(stock[i],15,6/12,0.2,0.05,50,1,5/2400)
  error1[i] = BlackScholesEuCall(stock[i],15,6/12,0.2,0.05) - matval1[i] 
}

plot(stock, matval, col="magenta",pch = 2, type="b")
plot(stock, matval1, col="magenta",pch = 2, type="b")


plot(stock, abs(error), col="magenta",pch = 1, type="b")
plot(stock,abs(error1), col="red",pch = 1, type="b")


ggplot()+
  geom_line(mapping = aes(stock, matval, linetype = 'solid', color = 'Implicit'))+
  geom_line(mapping = aes(stock, matval1, linetype = 'dashed', color = 'Explicit'))+
  geom_point(mapping = aes(stock, matval1, color = 'Explicit'), shape = 1)+
  scale_color_manual(values = c('Implicit' = 'deeppink4', 'Explicit' = 'seagreen3')) +
  scale_linetype_manual(values = c('dashed', 'solid'), guide = "none")+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")
