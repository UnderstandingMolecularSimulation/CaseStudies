      Subroutine Echain(Xn,Yn,Zn,Ichain,Inum,Etot)
      Implicit None

      Include 'system.inc'

Ccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate The Energy Of A Trial Chain   C
Ccccccccccccccccccccccccccccccccccccccccccccccc
      
      Integer          Jj,I,Ichain,Ii,Inum
      Double Precision Xn(Ellmax),Yn(Ellmax),Zn(Ellmax),Etot,Bx,By,Bz, 
     &                 R2,R2i,R6i
 
      Etot = 0.0d0
   
Ccccccccccccccccccccccc
C     Intermolecular  C
Ccccccccccccccccccccccc

      Do I = 1, Nchain

         If(I.Ne.Ichain.And.I.Gt.Inum) Then

            Do Jj = 1, Ell
               Do Ii = 1, Ell

                  Bx = Xn(Jj) - X(Ii,I)
                  By = Yn(Jj) - Y(Ii,I)
                  Bz = Zn(Jj) - Z(Ii,I)

                  Bx = Bx - Box*Dble
     &                 (Idint(Bx*Boxi + 9999.5d0) - 9999)
                  By = By - Box*Dble
     &                 (Idint(By*Boxi + 9999.5d0) - 9999)
                  Bz = Bz - Box*Dble
     &                 (Idint(Bz*Boxi + 9999.5d0) - 9999)

                  R2 = Bx*Bx + By*By + Bz*Bz

                  If(R2.Lt.Rc2) Then
                     R2i  = 1/R2
                     R6i  = R2i*R2i*R2i
                     Etot = Etot + 4.0d0*(R6i*R6i-R6i)
                  Endif
               Enddo
            Enddo
         Endif
      Enddo

Cccccccccccccccccccccccc
C     Intramolecular   C
Cccccccccccccccccccccccc

      If(Ell.Ge.4) Then
         Do Ii=4,Ell
            Do Jj=1,Ii-3
                  
               Bx = Xn(Ii) - Xn(Jj)
               By = Yn(Ii) - Yn(Jj)
               Bz = Zn(Ii) - Zn(Jj)

               R2 = Bx*Bx + By*By + Bz*Bz

               If(R2.Lt.Rc2) Then
                  R2i  = 1/R2
                  R6i  = R2i*R2i*R2i
                  Etot = Etot + 4.0d0*(R6i*R6i-R6i)
               Endif
            Enddo
         Enddo
      Endif

      Return
      End
