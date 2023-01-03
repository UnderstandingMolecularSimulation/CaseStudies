      Function Ran_Gauss()
      Implicit None
 
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Random Number From A Gaussian Distribution              C
C     Mean=0; Sd=1                                            C
C                                                             C
C     X + Y*Ran_Gauss() Will Return A Gaussian Distributed    C
C     Number With Mean=X And Sd=Y                             C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      Double Precision Ran1,Ran2,Ran_Gauss,Ran_Uniform,Ransq
 
 1    Ran1  = 2.0d0*Ran_Uniform() - 1.0d0
      Ran2  = 2.0d0*Ran_Uniform() - 1.0d0
      Ransq = Ran1*Ran1 + Ran2*Ran2
 
      If (Ransq.Ge.1.0d0.Or.Ransq.Le.0.0d0) Goto 1
 
      Ran_Gauss = Ran1*Dsqrt(-2.0d0*Dlog(Ransq)/Ransq)
 
      Return
      End
