library(ggplot2)

N = seq(4,12,2)
h = matrix(0, nrow = length(N), ncol = 1)
realvalue = matrix(0, nrow = length(N), ncol = max(N)-1)
estimation = matrix(0, nrow = length(N), ncol = max(N)-1)
diff = matrix(0, nrow = length(N), ncol = max(N)-1)

xi = matrix(0, nrow = length(N), ncol = max(N)-1)

for (k in seq(1,length(N))) {
  
  D = 1-0
  h[k] = D/N[k]
  i = seq(1,N[k]-1)
  xi[k,1:length(i)] = i*h[k]
  
  A <- matrix(0, nrow=N[k]-1, ncol=N[k]-1)
  
  for (j in 2:N[k]-2){
    A[j,j] <- 2 + h[k]^2
    A[j,j+1] <- -1
    A[j,j-1] <- -1
  }
  A[N[k]-1,N[k]-1] <- 2 + h[k]^2
  A[N[k]-1,N[k]-2] <- -1
  
  z = h[k]^2 * cos((pi/2) + pi*xi[k,1:length(i)])
  
  estimation[k,1:length(z)] = round(LU_decomposition(A,z), 5)
  
  
  realvalue[k,1:length(z)] = round(-sin(pi*xi[k,1:length(i)])/(1 + pi^2), 5)
  diff[k,1:length(z)] = round(abs(estimation[k,1:length(z)] - realvalue[k,1:length(z)]), 5)
}
rownames(estimation) = as.character(h)
rownames(realvalue) = as.character(h)
rownames(diff) = as.character(h)

max_error = matrix(0, nrow = length(N), ncol = 1)
for (i in seq(1,length(N))) {
  max_error[i] = max(diff[i,])
}







h_real = 1/100
i_real = seq(1,99)
xi_real = i_real * h_real
x = c(0,xi_real,1)

ggplot()+
  geom_line(mapping = aes(x, c(0,-sin(pi*xi_real)/(1 + pi^2),0), linetype = 'solid', color = 'Real Values'))+
  geom_line(mapping = aes(c(0,xi[1,1:3],1), c(0,estimation[1,1:3],0), linetype = 'dashed', color = 'h = 1/4'))+
  geom_point(mapping = aes(c(0,xi[1,1:3],1), c(0,estimation[1,1:3],0), color = 'h = 1/4'), shape = 1)+
  geom_line(mapping = aes(c(0,xi[2,1:11],1), c(0,estimation[2,1:11],0), linetype = 'dashed', color = 'h = 1/12'))+
  geom_point(mapping = aes(c(0,xi[2,1:11],1), c(0,estimation[2,1:11],0), color = 'h = 1/12'), shape = 1)+
# geom_line(mapping = aes(c(0,xi[3,1:7],1), c(0,estimation[3,1:7],0), linetype = 'dashed', color = 'h = 1/8'))+
# geom_point(mapping = aes(c(0,xi[3,1:7],1), c(0,estimation[3,1:7],0), color = 'h = 1/8'), shape = 1)+
  scale_color_manual(values = c('Real Values' = 'black', 'h = 1/4' = 'blue', 'h = 1/12' = 'red')) +
  scale_linetype_manual(values = c('dashed', 'solid'), guide = "none")+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")

#'h = 1/8' = 'seagreen3',


ggplot()+
  geom_line(mapping = aes(x, c(0,-sin(pi*xi_real)/(1 + pi^2),0)), color = 'black')+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")


ggplot()+
  geom_line(mapping = aes(c(0,xi[1,1:3],1), c(0,estimation[1,1:3],0)), color = 'red')+
  geom_point(mapping = aes(c(0,xi[1,1:3],1), c(0,estimation[1,1:3],0)), shape = 1, color = 'red')+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")


ggplot()+
  geom_line(mapping = aes(c(0,xi[2,1:11],1), c(0,estimation[2,1:11],0)), color = 'blue')+
  geom_point(mapping = aes(c(0,xi[2,1:11],1), c(0,estimation[2,1:11],0)), shape = 1, color = 'blue')+
  labs(title = '', 
       x = '',
       y = '',
       color = '')+
  theme_classic()+
  theme(legend.position = "top")



#ggplot()+
#geom_line(mapping = aes(x, sin(2*pi*x)/(1 + 4*pi^2), linetype = 'solid', color = 'Real Values'))+
#geom_line(mapping = aes(c(0,xi[1,1:4],1), c(0,estimation[1,1:4],0), linetype = 'dashed', color = 'N = 5'))+
#geom_point(mapping = aes(c(0,xi[1,1:4],1), c(0,estimation[1,1:4],0), color = 'N = 5'))+
#geom_line(mapping = aes(c(0,xi[2,1:6],1), c(0,estimation[2,1:6],0), linetype = 'dashed', color = 'N = 7'))+
#geom_point(mapping = aes(c(0,xi[2,1:6],1), c(0,estimation[2,1:6],0), color = 'N = 7'))+
#geom_line(mapping = aes(c(0,xi[3,1:8],1), c(0,estimation[3,1:8],0), linetype = 'dashed', color = 'N = 9'))+
#geom_point(mapping = aes(c(0,xi[3,1:8],1), c(0,estimation[3,1:8],0), color = 'N = 9'))+
#geom_line(mapping = aes(c(0,xi[4,1:10],1), c(0,estimation[4,1:10],0), linetype = 'dashed', color = 'N = 11'))+
#geom_point(mapping = aes(c(0,xi[4,1:10],1), c(0,estimation[4,1:10],0), color = 'N = 11'))+
#scale_color_manual(values = c('Real Values' = 'red', 'N = 5' = 'deeppink4', 'N = 7' = 'blue', 'N = 9' = 'yellow', 'N = 11' = 'cornflowerblue')) +
#scale_linetype_manual(values = c('dashed', 'solid'), guide = "none")+
#labs(color = '')+
#xlab("x")+
#ylab("Value of Function")+
#theme_classic()+
#theme(legend.position = "right")+
#ggtitle("SDE Solution Approximation")



#sol = c(0,estimation[4,1:10],0)

#plot(c(0,xi[4,1:10],1),sol, col="red",pch = 21, type="b")
#lines(c(0,xi[4,1:10],1),sin(2*pi*c(0,xi[4,1:10],1))/(1 + 4*pi^2),col="blue", pch = 21, type="b")
#grid()
