Gauss_Seidel = function(A,z,f){
  
  x = matrix(0, nrow = ncol(A), ncol = 1)
  n = ncol(A)
  
  eps = 0.000001
  error = 10

  #while (max(error)>eps){
  for (i in seq(1,130)) {
    x[1] = max(1/A[1,1] * (z[1] -  A[1,(1+1):n]%*%x[(1+1):n]), f[1])
    for (i in seq(2,(n-1))) {
      x[i] = max(1/A[i,i] * (z[i] - (A[i,1:(i-1)]%*%x[1:(i-1)] + A[i,(i+1):n]%*%x[(i+1):n])), f[i])
    }
    x[n] = max(1/A[n,n] * (z[n] -  A[n,1:(n-1)]%*%x[1:(n-1)]), f[n])
    
    error = abs(z - A%*%x)
  }
  return(x)
}

B = rbind(c(8,1,0,0),c(1,7,-1,0),c(0,-3,17,1),c(0,0,5,29))
z = c(2,3,6,13)
f = c(0.12,0.5,0.4,0.09)  
#f = c(0,0,0,0)  # deite to, oti otan f = c(0,0,0,0) trexei kanonika
                # eno otan f = c(0.12,0.5,0.4,0.09) h while den stamataei pote

Gauss_Seidel(B,z,f)
#LU_decomposition(B,z)
