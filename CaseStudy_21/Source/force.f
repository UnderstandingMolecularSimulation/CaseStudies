      Subroutine Force(X,U,Lsystem)
      Implicit None
 
      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate The Forces And Potential Energy      C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      Double Precision X,U,Amp
      Logical          Lsystem

      Lsystem = .True.

      If(X.Lt.-2.0d0) Then
         Lsystem = .False.
         Amp     = 0.0d0
      Elseif(X.Lt.-1.25d0) Then
         Amp = 1.0d0
      Elseif(X.Lt.-0.25d0) Then
         Amp = 2.0d0
      Elseif(X.Lt.0.75d0) Then
         Amp = 3.0d0
      Elseif(X.Lt.1.75d0) Then
         Amp = 4.0d0
      Elseif(X.Lt.2.0d0) Then
         Amp = 5.0d0
      Else
         Lsystem = .False.
         Amp     = 0.0d0
      Endif

      U = Amp*(1.0d0 + Dsin(Twopi*X))
 
      Return
      End
