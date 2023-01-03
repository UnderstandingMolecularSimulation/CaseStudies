      Subroutine Force(X,Y,U,Fx,Fy)
      Implicit None

      Include 'system.inc'

Ccccccccccccccccccccccccccccccccccccccc
C     Energy And Force Calculation    C
C     See Jcp, 108 (1999), 1964       C
Ccccccccccccccccccccccccccccccccccccccc

      Double Precision X,Y,U,Fx,Fy

      U  = Onesixth*((4.0d0*((1.0d0 - X**2 - Y**2)**2)) +
     &               (2.0d0*((X**2 - 2.0d0)**2))        +
     &               ((((X+Y)**2) - 1.0d0)**2)          +
     &               ((((X-Y)**2) - 1.0d0)**2)          - 2.0d0)

      Fx = Onesixth*((16.0d0*X*(1.0d0-X**2-Y**2))       -
     &                (8.0d0*X*(X**2 - 2.0d0))          -
     &                (4.0d0*(X+Y)*((X+Y)**2 - 1.0d0))  -
     &                (4.0d0*(X-Y)*((X-Y)**2 - 1.0d0)))

      Fy = Onesixth*((16.0d0*Y*(1.0d0-X**2-Y**2))       -
     &               (4.0d0*(X+Y)*((X+Y)**2 - 1.0d0))   +
     &               (4.0d0*(X-Y)*((X-Y)**2 - 1.0d0)))
 
      Return
      End
