BS_explicit_EuPut = function(So,K,T,sigma,r,Smax,dS,dt){

  M = round(Smax/dS)
  dS = Smax/M
  N = round(T/dt)
  dt = T/N
  Price = matrix(0, nrow = M+1, ncol = N+1)
  
  vetS = seq(0, Smax, dS)
  veti = seq(0, M)
  vetj = seq(0, N)
  
  Price[,N+1] = pmax(K - vetS, 0)
  Price[1,] = K * exp(-r*dt*(N-vetj))
  Price[M+1,] = 0
  
  a = 0.5*dt*(sigma^2*veti^2 - r*veti)
  b = 1 - dt*(sigma^2*veti^2 + r)
  c = 0.5*dt*(sigma^2*veti^2 + r*veti)
  
  for (j in seq(N,1,-1)) {
    for (i in seq(2,M)) {
      Price[i,j] = a[i]*Price[i-1,j+1] + b[i]*Price[i,j+1] + c[i]*Price[i+1,j+1]
    }
  }
 return(approx(vetS,Price[,1],So))
}