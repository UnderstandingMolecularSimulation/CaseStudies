      Function In_A()
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccccccccccc
C     Check If A Point Of The Path Is In A   C
Cccccccccccccccccccccccccccccccccccccccccccccc
      
      Logical          In_A
      Double Precision R2

      R2 = (1.0d0 + Rxx)**2 + Ryy**2

      If(R2.Lt.(Radius**2)) Then
         In_A = .True.
      Else
         In_A = .False.
      Endif

      Return
      End
