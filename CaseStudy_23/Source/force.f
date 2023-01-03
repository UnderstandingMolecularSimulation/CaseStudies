      Subroutine Force(X,U,F)
      Implicit None
 
C     Calculate The Forces And Potential Energy
 
      Include 'system.inc'
 
      Double Precision X,U,F

      U = 0.0d0
      F = 0.0d0

      If(X.Ge.0.0d0.And.X.Le.1.0d0) Then
         U =  1.0d0 - Dcos(2.0d0*Onepi*X)
         F = -2.0d0*Onepi*Dsin(2.0d0*Onepi*X)
      Endif
 
      Return
      End
