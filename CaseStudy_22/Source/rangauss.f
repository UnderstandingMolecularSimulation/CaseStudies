      Function Rangauss()
      Implicit None
 
C     Generates Random Numbers From A Gaussian Distribution
 
      Double Precision Ranf,Rangauss,V1,V2,Rsq
 
 100  V1 = 2.0d0*Ranf() - 1.0d0
      V2 = 2.0d0*Ranf() - 1.0d0
      Rsq = V1*V1 + V2*V2
 
      If (Rsq.Ge.1.0d0 .Or. Rsq.Le.0.0d0) Goto 100
 
      Rangauss = V1*Dsqrt( - 2.0d0*Dlog(Rsq)/Rsq)
 
      Return
      End
