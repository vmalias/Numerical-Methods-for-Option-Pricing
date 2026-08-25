xo = 0.1
fxo = exp(2*xo)
fxo_FirstOrder = 2*exp(2*xo)
fxo_SecondOrder = 4*exp(2*xo)

h = matrix(c(0.5,0.25,0.1,0.01),1,4)

dhplus = matrix(0,length(h),1)
rownames(dhplus) = as.character(h)
Edhplus = matrix(0,length(h),1)
rownames(Edhplus) = as.character(h)

dhminus = matrix(0,length(h),1)
rownames(dhminus) = as.character(h)
Edhminus = matrix(0,length(h),1)
rownames(Edhminus) = as.character(h)

dhc = matrix(0,length(h),1)
rownames(dhc) = as.character(h)
Edhc = matrix(0,length(h),1)
rownames(Edhc) = as.character(h)

dhc2 = matrix(0,length(h),1)
rownames(dhc2) = as.character(h)
Edhc2 = matrix(0,length(h),1)
rownames(Edhc2) = as.character(h)

for (i in seq(1,length(h))) {
  dhplus[i] = (exp(2*(xo+h[i])) - exp(2*xo))/h[i]
  Edhplus[i] = abs(dhplus[i] - fxo_FirstOrder)
  
  dhminus[i] = (exp(2*xo) - exp(2*(xo-h[i])))/h[i]
  Edhminus[i] = abs(dhminus[i] - fxo_FirstOrder)
  
  dhc[i] = (exp(2*(xo+h[i])) - exp(2*(xo-h[i])))/(2*h[i])
  Edhc[i] = abs(dhc[i] - fxo_FirstOrder)
  
  dhc2[i] = (exp(2*(xo+h[i])) - 2*exp(2*xo) + exp(2*(xo-h[i])))/(h[i]^2)
  Edhc2[i] = abs(dhc2[i] - fxo_SecondOrder)
}

FirstOrder = round(data.frame(dhplus, dhminus, dhc, Edhplus, Edhminus, Edhc), digits = 5)
rownames(FirstOrder) = as.character(h)
colnames(FirstOrder) = c('dhplus', 'dhminus', 'dhc', 'Edhplus', 'Edhminus', 'Edhc')

SecondOrder = round(data.frame(dhc2, Edhc2), digits = 5)
rownames(SecondOrder) = as.character(h)
colnames(SecondOrder) = c('dhc2', 'Edhc2')

