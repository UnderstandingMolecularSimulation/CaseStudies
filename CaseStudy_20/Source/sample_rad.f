      Subroutine Sample_Rad(Ichoice)
      Implicit None

      Include 'system.inc'

Cccccccccccccccccccccccccccccccccccccccccccccccccc
C     Samples The Radial Distribution Function   C
Cccccccccccccccccccccccccccccccccccccccccccccccccc

      Integer I,J,Maxx,Ichoice,A,Ii,Jj

      Parameter(Maxx = 500)

      Double Precision Ggt,Gg(Maxx),Hb2,Delta,R2,Bx,By,Bz,
     &                 Dummy1,Dummy2

      Save Ggt,Gg,Delta,Hb2

      If(Ichoice.Eq.1) Then

Ccccccccccccccccccc
C     Initialize  C
Ccccccccccccccccccc

         Do I=1,Maxx
            Gg(I) = 0.0d0
         Enddo

         Ggt   = 0.0d0
         Hb2   = (0.5d0*Box)**2
         Delta = Dble(Maxx-1)/(0.5d0*Box)
      
      Elseif(Ichoice.Eq.2) Then

Cccccccccccccccc
C     Sample   C
Cccccccccccccccc

         Ggt = Ggt + 1.0d0

Cccccccccccccccccccccccc
C     Intermolecular   C
Cccccccccccccccccccccccc

         Do I=1,Nchain-1
            Do J=I+1,Nchain
               Do Ii=1,Ell
                  Do Jj=1,Ell

                     Bx = X(Ii,I) - X(Jj,J)
                     By = Y(Ii,I) - Y(Jj,J)
                     Bz = Z(Ii,I) - Z(Jj,J)
 
                     Bx = Bx - Box*Dble
     &                    (Idint(Bx*Boxi + 9999.5d0) - 9999)
                     By = By - Box*Dble
     &                    (Idint(By*Boxi + 9999.5d0) - 9999)
                     Bz = Bz - Box*Dble
     &                    (Idint(Bz*Boxi + 9999.5d0) - 9999)

                     R2 = Bx*Bx + By*By + Bz*Bz

                     If(R2.Lt.Hb2) Then
                  
                        A     = Idint(Dsqrt(R2)*Delta) + 1
                        Gg(A) = Gg(A)                  + 2.0d0
               
                     Endif
                  Enddo
               Enddo
            Enddo
         Enddo

Ccccccccccccccccccccccccc
C     Intramolecular    C
Ccccccccccccccccccccccccc

         If(Ell.Ge.4) Then

            Do I=1,Nchain
               Do Ii=1,Ell-3
                  Do Jj=Ii+3,Ell

                     Bx = X(Ii,I) - X(Jj,I)
                     By = Y(Ii,I) - Y(Jj,I)
                     Bz = Z(Ii,I) - Z(Jj,I)   

                     R2 = Bx*Bx + By*By + Bz*Bz

                     If(R2.Lt.Hb2) Then
                  
                        A     = 1     + Idint(Dsqrt(R2)*Delta)
                        Gg(A) = 2.0d0 + Gg(A)
                        
                     Endif
                  Enddo
               Enddo
            Enddo
         Endif
      Else

Cccccccccccccccccccccc
C     Print Results  C
Cccccccccccccccccccccc

         Dummy1 = 1.0d0/(Ggt*Dble(Nchain*Ell))
         Dummy2 = 1.0d0/Delta

         Open(24,File="rad.dat")

         Do I=1,Maxx-1
            R2 = (16.0d0*Datan(1.0d0)/3.0d0)*
     &           (Dble(Nchain*Ell)/(Box**3))*(Dummy2**3)*
     &           ((Dble(I+1))**3 - (Dble(I)**3))
            
            Write(24,*) ((Dble(I)-0.5d0)*Dummy2),Gg(I)*Dummy1/R2
         Enddo

         Close(24)

      Endif

      Return
      End
