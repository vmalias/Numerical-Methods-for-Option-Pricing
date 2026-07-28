
############################# Minimum Covergence ###############################
################################### Step #######################################



################################### Call #######################################

StrikePrice = seq(90,110,10)
Periods = seq(1,100)
DCRRcall = matrix(0, length(StrikePrice), length(Periods))
DKRcall = matrix(0, length(StrikePrice), length(Periods))

for (i in seq(1,length(StrikePrice))) {
  for (j in seq(1,length(Periods))) {
    DCRRcall[i,j] = abs((BinTreeEuCall(100,StrikePrice[i],1,Periods[j],0.2,0.05) - BlackScholesEuCall(100,StrikePrice[i],1,0.2,0.05)) / BlackScholesEuCall(100,StrikePrice[i],1,0.2,0.05))
    DKRcall[i,j] = abs((KammradAmEuCall(100,StrikePrice[i],1,Periods[j],1.22474,0.2,0.05) - BlackScholesEuCall(100,StrikePrice[i],1,0.2,0.05)) / BlackScholesEuCall(100,StrikePrice[i],1,0.2,0.05))
  }
}

colnames(DCRRcall) = as.character(Periods)
rownames(DCRRcall) = as.character(StrikePrice)

colnames(DKRcall) = as.character(Periods)
rownames(DKRcall) = as.character(StrikePrice)

PrecisionLevel = c(0.005, 0.01, 0.05)





MCSCRRcall = matrix(0, length(StrikePrice), length(PrecisionLevel))

for (i in seq(1, length(PrecisionLevel))) {
  for (j in seq(1, length(StrikePrice))) {
    
   ecrr = which(as.matrix(t(DCRRcall[j,])) < PrecisionLevel[i], arr.ind = T)
   N = 1
   while(sum(ecrr[N:nrow(ecrr),1]) != length(seq(ecrr[N,2], ecrr[nrow(ecrr),2]))) {
    N = N + 1
   }
   
   MCSCRRcall[i,j] = as.integer(ecrr[N,2])
  }
}  

colnames(MCSCRRcall) = as.character(StrikePrice)
rownames(MCSCRRcall) = as.character(PrecisionLevel) 





MCSKRcall = matrix(0, length(StrikePrice), length(PrecisionLevel))

for (i in seq(1, length(PrecisionLevel))) {
  for (j in seq(1, length(StrikePrice))) {
    
    ekr = which(as.matrix(t(DKRcall[j,])) < PrecisionLevel[i], arr.ind = T)
    N = 1
    while(sum(ekr[N:nrow(ekr),1]) != length(seq(ekr[N,2], ekr[nrow(ekr),2]))) {
      N = N + 1
    }
    
    MCSKRcall[i,j] = as.integer(ekr[N,2])
  }
}  

colnames(MCSKRcall) = as.character(StrikePrice)
rownames(MCSKRcall) = as.character(PrecisionLevel) 


############################# European Put #####################################

StrikePrice = seq(90,110,10)
Periods = seq(1,100)
DCRRput = matrix(0, length(StrikePrice), length(Periods))
DKRput = matrix(0, length(StrikePrice), length(Periods))

for (i in seq(1,length(StrikePrice))) {
  for (j in seq(1,length(Periods))) {
    DCRRput[i,j] = abs((BinTreeEuPut(100,StrikePrice[i],1,Periods[j],0.2,0.05) - BlackScholesEuPut(100,StrikePrice[i],1,0.2,0.05)) / BlackScholesEuPut(100,StrikePrice[i],1,0.2,0.05))
    DKRput[i,j] = abs((KammradAmEuPut(100,StrikePrice[i],1,Periods[j],1.22474,0.2,0.05) - BlackScholesEuPut(100,StrikePrice[i],1,0.2,0.05)) / BlackScholesEuPut(100,StrikePrice[i],1,0.2,0.05))
  }
}

colnames(DCRRput) = as.character(Periods)
rownames(DCRRput) = as.character(StrikePrice)

colnames(DKRput) = as.character(Periods)
rownames(DKRput) = as.character(StrikePrice)

PrecisionLevel = c(0.005, 0.01, 0.05)




MCSCRRput = matrix(0, length(StrikePrice), length(PrecisionLevel))

for (i in seq(1, length(PrecisionLevel))) {
  for (j in seq(1, length(StrikePrice))) {
    
    ecrr = which(as.matrix(t(DCRRput[j,])) < PrecisionLevel[i], arr.ind = T)
    N = 1
    while(sum(ecrr[N:nrow(ecrr),1]) != length(seq(ecrr[N,2], ecrr[nrow(ecrr),2]))) {
      N = N + 1
    }
    
    MCSCRRput[i,j] = as.integer(ecrr[N,2])
  }
}  

colnames(MCSCRRput) = as.character(StrikePrice)
rownames(MCSCRRput) = as.character(PrecisionLevel) 





MCSKRput = matrix(0, length(StrikePrice), length(PrecisionLevel))

for (i in seq(1, length(PrecisionLevel))) {
  for (j in seq(1, length(StrikePrice))) {
    
    ekr = which(as.matrix(t(DKRput[j,])) < PrecisionLevel[i], arr.ind = T)
    N = 1
    while(sum(ekr[N:nrow(ekr),1]) != length(seq(ekr[N,2], ekr[nrow(ekr),2]))) {
      N = N + 1
    }
    
    MCSKRput[i,j] = as.integer(ekr[N,2])
  }
}  

colnames(MCSKRput) = as.character(StrikePrice)
rownames(MCSKRput) = as.character(PrecisionLevel) 



############################## American Put ####################################

StrikePrice = seq(90,110,10)
Periods = seq(1,100)
DCRRAmput = matrix(0, length(StrikePrice), length(Periods))
DKRAmput = matrix(0, length(StrikePrice), length(Periods))

for (i in seq(1,length(StrikePrice))) {
  for (j in seq(1,length(Periods))) {
    DCRRAmput[i,j] = abs((BinTreeAmPut(100,StrikePrice[i],1,Periods[j],0.2,0.05) - KammradAmericanPut(100,StrikePrice[i],1,1000,1.22474,0.2,0.05)) / KammradAmericanPut(100,StrikePrice[i],1,1000,1.22474,0.2,0.05))
    DKRAmput[i,j] = abs((KammradAmericanPut(100,StrikePrice[i],1,Periods[j],1.22474,0.2,0.05) - KammradAmericanPut(100,StrikePrice[i],1,1000,1.22474,0.2,0.05)) / KammradAmericanPut(100,StrikePrice[i],1,1000,1.22474,0.2,0.05))
  }
}

colnames(DCRRAmput) = as.character(Periods)
rownames(DCRRAmput) = as.character(StrikePrice)

colnames(DKRAmput) = as.character(Periods)
rownames(DKRAmput) = as.character(StrikePrice)

PrecisionLevel = c(0.005, 0.01, 0.05)



MCSCRRAmput = matrix(0, length(StrikePrice), length(PrecisionLevel))

for (i in seq(1, length(PrecisionLevel))) {
  for (j in seq(1, length(StrikePrice))) {
    
    ecrr = which(as.matrix(t(DCRRAmput[j,])) < PrecisionLevel[i], arr.ind = T)
    N = 1
    while(sum(ecrr[N:nrow(ecrr),1]) != length(seq(ecrr[N,2], ecrr[nrow(ecrr),2]))) {
      N = N + 1
    }
    
    MCSCRRAmput[i,j] = as.integer(ecrr[N,2])
  }
}  

colnames(MCSCRRAmput) = as.character(StrikePrice)
rownames(MCSCRRAmput) = as.character(PrecisionLevel) 





MCSKRAmput = matrix(0, length(StrikePrice), length(PrecisionLevel))

for (i in seq(1, length(PrecisionLevel))) {
  for (j in seq(1, length(StrikePrice))) {
    
    ekr = which(as.matrix(t(DKRAmput[j,])) < PrecisionLevel[i], arr.ind = T)
    N = 1
    while(sum(ekr[N:nrow(ekr),1]) != length(seq(ekr[N,2], ekr[nrow(ekr),2]))) {
      N = N + 1
    }
    
    MCSKRAmput[i,j] = as.integer(ekr[N,2])
  }
}  

colnames(MCSKRAmput) = as.character(StrikePrice)
rownames(MCSKRAmput) = as.character(PrecisionLevel) 
