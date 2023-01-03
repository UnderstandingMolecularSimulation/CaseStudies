      Function Ranf()
      Implicit None
 
      Include 'system.inc'
 
      Integer Ia,Im,Iq,Ir,Mask,K
      Double Precision Ranf,Am
 
      Parameter (Ia=16807,Im=2147483647,
     &     Am=1.0d0/2147483647.0d0,Iq=127773,Ir=2836,Mask=123459876)
 
 100  Idum = Ieor(Idum,Mask)
      K = Idum/Iq
      Idum = Ia*(Idum - K*Iq) - Ir*K
      If (Idum.Lt.0.0d0) Idum = Idum + Im
      Ranf = Am*Idum
      Idum = Ieor(Idum,Mask)
 
      If (Ranf.Lt.0.00000001d0 .Or. Ranf.Gt.0.99999999d0) 
     &     Goto 100
 
      Return
      End
