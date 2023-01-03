      Function Ran_Gauss()
      Implicit None
 
      Double Precision Ran1,Ran2,Ran_Gauss,Ran_Uniform,Ransq
 
 100  Ran1  = 2.0d0*Ran_Uniform() - 1.0d0
      Ran2  = 2.0d0*Ran_Uniform() - 1.0d0
      Ransq = Ran1*Ran1 + Ran2*Ran2
 
      If (Ransq.Ge.1.0d0 .Or. Ransq.Le.0.0d0) Goto 100
 
      Ran_Gauss = Ran1*Dsqrt(-2.0d0*Dlog(Ransq)/Ransq)
 
      Return
      End
