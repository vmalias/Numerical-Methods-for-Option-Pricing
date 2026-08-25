BS_implicit_AmPut_LU_decomposition = function(So,K,T,sigma,r,Smax,dS,dt){
  
  M = round(Smax/dS)
  dS = Smax/M
  N = round(T/dt)
  dt = T/N
  Price = matrix(0, nrow = M+1, ncol = N+1)
  
  vetS = seq(0, Smax, dS)
  veti = seq(1, M-1)
  vetj = seq(0, N)
  
  Price[,N+1] = pmax(K - vetS, 0)
  Price[1,] = K * exp(-r*dt*(N-vetj))
  Price[M+1,] = 0
  
  a = -0.5*dt*(sigma^2*veti^2 - r*veti)
  b = 1 + dt*(sigma^2*veti^2 + r)
  c = -0.5*dt*(sigma^2*veti^2 + r*veti)
  
  A = matrix(0,M-1,M-1)
  A[1,1] = b[1] 
  A[1,2] = c[1]
  A[M-1,M-2] = a[M-1]
  A[M-1,M-1] = c[M-1]
  for (i in seq(2,M-2)) {
    A[i,i] = b[i]
    A[i,i-1] = a[i]
    A[i,i+1] = c[i]
  }
  
  Bound_Cond = matrix(0,M-1,1)
  
  for (j in seq(N,1,-1)) {
    Bound_Cond[1] = a[1] * Price[1,j]
    z = Price[seq(2,M), j+1] - Bound_Cond
    Price[seq(2,M), j] = pmax(LU_decomposition(A, z), K - veti*dS)
  }
  
  approximation = approx(vetS,Price[,1],So)
  return(approximation$y)
}  



implicit_AmPut_LU_decomposition = BS_implicit_AmPut_LU_decomposition(37,50,5/12,0.4,0.1,100,2,5/1200)