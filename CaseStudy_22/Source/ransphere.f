      Subroutine Ransphere(X,Y,Z)
      Implicit None
 
      Double Precision Ran1,Ran2,Ranh,Ransq,X,Y,Z,Ranf
 
 100  Ran1 = 2.0d0*Ranf() - 1.0d0
      Ran2 = 2.0d0*Ranf() - 1.0d0
      Ransq = Ran1*Ran1 + Ran2*Ran2
 
      If (Ransq.Ge.1.0d0) Goto 100
 
      Ranh = 2.0d0*Dsqrt(1.0d0 - Ransq)
      X = Ran1*Ranh
      Y = Ran2*Ranh
      Z = 1.0d0 - 2.0d0*Ransq
 
      Return
      End
