      Subroutine Echoice(Ichain,Ibead,Xi,Yi,Zi,Xn,Yn,Zn,Etot)
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate The Energy Of A Specific Bead      C
Cccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      Integer          Ichain,IBead,Ii,I
      
      Double Precision Etot,Bx,By,Bz,R2,Xn(Ellmax),Yn(Ellmax),
     &                 Zn(Ellmax),Xi,Yi,Zi,R2i,R6i
 
      Etot = 0.0d0
      
      Do I = 1, Nchain

         If(I.Ne.Ichain) Then

            Do Ii = 1, Ell

               Bx = Xi - X(Ii,I)
               By = Yi - Y(Ii,I)
               Bz = Zi - Z(Ii,I)

               Bx = Bx - Box*Dble
     &              (Idint(Bx*Boxi + 9999.5d0) - 9999)
               By = By - Box*Dble
     &              (Idint(By*Boxi + 9999.5d0) - 9999)
               Bz = Bz - Box*Dble
     &              (Idint(Bz*Boxi + 9999.5d0) - 9999)

               R2 = Bx*Bx + By*By + Bz*Bz

               If(R2.Lt.Rc2) Then
                  R2i  = 1/R2
                  R6i  = R2i*R2i*R2i
                  Etot = Etot + 4.0d0*(R6i*R6i-R6i)
               Endif
            Enddo
         Endif
      Enddo

Ccccccccccccccccccccccccccc
C     Intramolecular      C
C     Full Potential !!!  C
Ccccccccccccccccccccccccccc

      If(Ibead.Ge.4) Then
         Do Ii=1,Ibead-3
                  
            Bx = Xi - Xn(Ii)
            By = Yi - Yn(Ii)
            Bz = Zi - Zn(Ii)

            R2 = Bx*Bx + By*By + Bz*Bz

            If(R2.Lt.Rc2) Then
               R2i  = 1/R2
               R6i  = R2i*R2i*R2i
               Etot = Etot + 4.0d0*(R6i*R6i-R6i)
            Endif
         Enddo
      Endif

      Return
      End
