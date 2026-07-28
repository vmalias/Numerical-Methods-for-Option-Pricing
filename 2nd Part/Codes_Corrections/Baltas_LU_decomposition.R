LU_decomposition <- function(A,z){
  if(nrow(A) == ncol(A) && length(z) == ncol(A)){
    if(A[1,1] != 0 && A[1,2] != 0 && A[ncol(A),ncol(A)] != 0 && A[ncol(A),ncol(A)-1] != 0  && abs(A[1,1]) > abs(A[1,2]) && abs(A[ncol(A),ncol(A)]) > abs(A[ncol(A),ncol(A)-1]) ) {
      j = 0  
      for (i in seq(2,ncol(A)-1)) {
        if(A[i,i-1] != 0 && A[i,i] != 0 && A[i,i+1] != 0 && abs(A[i,i]) >= (abs(A[i,i+1]) + abs(A[i,i-1])) ){
          j = j 
        } else {
          j = j + 1
        }
      }
      if (j == 0){
        if(length(which(A == 0)) == (ncol(A)^2 - (ncol(A) + 2 * (ncol(A)-1))) ){
          n = ncol(A)  
          
          d = matrix(0, nrow = 1, ncol = n)
          d[1] = A[1,1]
          e = matrix(0, nrow = 1, ncol = n-1)
          e[1] = A[1,2]/d[1]
          for (k in seq(2, n-1)) {
            d[k] = A[k,k] - A[k,k-1] * e[k-1]
            e[k] = A[k,k+1] / d[k]
          }
          d[n] = A[n,n] - A[n,n-1] * e[ncol(e)]
          
          L = matrix(0, nrow = n, ncol = n)
          L[1] = d[1]
          for (i in seq(2,n)) {
            L[i,i] = d[i]
            L[i,i-1] = A[i,i-1]
          }
          
          U = matrix(0, nrow = n, ncol = n)
          for (i in seq(1,n-1)) {
            U[i,i] = 1
            U[i,i+1] = e[i]
          }
          U[n,n] = 1
          
          
          w = matrix(0, nrow = 1, ncol = n)
          w[1] = z[1] / d[1]
          for(k in seq(2,n)){
            w[k] = (z[k] - A[k,k-1] * w[k-1]) / d[k]
          }
          
          
          y = matrix(0, nrow = 1, ncol = n)
          y[n] = w[n]
          for (k in seq(n-1,1,-1)) {
            y[k] = w[k] - e[k] * y[k+1]
          }
          
          return(y)
          
        } else {
          print("uparxoun mh midenika stoixeia kai ektos twn triwn diagoniwn")
        }
      } else{
        print("****uparxei mideniko stoixeio ENTOS ton diagonion h den plurountai oi sunthikes")
      }
    } else {
      print("uparxei mideniko stoixeio sta arxika h telika stoixeia ton diagonion h den plurountai oi sunthikes")
    }
  } else {
    print("not a triagonal matrix or wrong dimension of z")
  }
}