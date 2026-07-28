
N1 = 5
h1 = 1/N1

A1 <- matrix(0, nrow=N1-1, ncol=N1-1)

for (i in 2:N1-2){
  A1[i,i] <- 2 + h1^2
  A1[i,i+1] <- -1
  A1[i,i-1] <- -1
}
A1[N1-1,N1-1] <- 2 +h1^2
A1[N1-1,N1-2] <- -1


j = seq(1,N1-1)
xj = j*h1
z1 = (h1^2)*sin(2*pi*xj)

y1 = LU_decomposition(A1,z1)

sol1 = c(0,y1,0)

plot(c(0,xj,1),sol1,type="l")
plot(c(0,xj,1),sol1, col="magenta",pch = 21, type="b")
#lines(c(0,xj,1),sin(2*pi*xi)/(1 + 4*pi^2),col="red")
grid()

#xi=c(0,xj,1)
#sin(2*pi*xi)/(1 + 4*pi^2) 

rv1 = sin(2*pi*xj)/(1 + 4*pi^2) 
diff1 = abs(y1-rv1)