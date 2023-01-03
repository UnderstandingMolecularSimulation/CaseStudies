      Subroutine Mcloop
      Implicit None
 
      Include 'system.inc'

      Integer I,J,Ii,Itemp,Iii
      Logical Lok
      Double Precision Move1(9),Move2(9),
     &     Swap1(9),Swap2(9),Ee1(-2100:2100,9),
     &     Ran_Uniform,Dummy,Xnew,Unew,Ee2,Rrr

      Ee2 = 0.0d0
      Rrr = 0.0d0

      Do I=1,Ntemp
         Move1(I) = 0.0d0
         Move2(I) = 0.0d0
         Swap1(I) = 0.0d0
         Swap2(I) = 0.0d0
    
         Do J=-2100,2100
            Ee1(J,I) = 0.0d0
         Enddo
      Enddo

Ccccccccccccccccccccccccccccc
C     Loop Over All Steps   C
Ccccccccccccccccccccccccccccc

      Open(35,File='History.Dat')

      Do I=1,Nstep
         Do Iii=1,1000

            If(Mod(Iii,10).Eq.0.And.I.Gt.1000.And.I.Lt.2000) Then
               Rrr = Rrr + 1.0d0
               Write(35,'(10e20.10)') 
     &              Rrr,(Xold(Itemp),Itemp=1,Ntemp)
            Endif

            Do Ii=1,Ntemp

Cccccccccccccccccccccccccccccccccccccccccccccccccc
C     Select Random Temperature                  C
C     For This Temperature, Select A Move        C
C     At Random..                                C
Cccccccccccccccccccccccccccccccccccccccccccccccccc

               Itemp = 1 + Idint(Dble(Ntemp)*Ran_Uniform())

               If(Ran_Uniform().Lt.Ptemp) Then

Cccccccccccccccccccccccccccccccccccccc
C     Swap Move                      C
C     Swap With Lower Temperature    C
Cccccccccccccccccccccccccccccccccccccc

                  If(Itemp.Ne.1) Then

                     Swap2(Itemp) = Swap2(Itemp) + 1.0d0

                     If(Ran_Uniform().Lt.
     &                    Dexp((Beta(Itemp)-Beta(Itemp-1))*
     &                    (Uold(Itemp)-Uold(Itemp-1)))) 
     &                    Then

                        Dummy         = Xold(Itemp)
                        Xold(Itemp)   = Xold(Itemp-1)
                        Xold(Itemp-1) = Dummy

                        Dummy         = Uold(Itemp)
                        Uold(Itemp)   = Uold(Itemp-1)
                        Uold(Itemp-1) = Dummy

                        Swap1(Itemp)  = Swap1(Itemp) + 
     &                       1.0d0
                     Endif
                  Endif
               Else

Ccccccccccccccccccccccccccc
C     Displacement        C
Ccccccccccccccccccccccccccc

                  Xnew = Xold(Itemp)  + 
     &                 0.2d0*(Ran_Uniform()-0.5d0)

                  Move2(Itemp) = Move2(Itemp) + 1.0d0

                  Call Force(Xnew,Unew,Lok)

                  If(Lok.And.
     &                 (Ran_Uniform().Lt.
     &                 Dexp(-Beta(Itemp)*
     &                 (Unew-Uold(Itemp))))) Then

                     Xold(Itemp)  = Xnew
                     Uold(Itemp)  = Unew
                     Move1(Itemp) = Move1(Itemp) + 1.0d0
                  Endif
               Endif
            Enddo

Ccccccccccccccccccccccccccccccccccc
C     Sample Distibutions         C
Ccccccccccccccccccccccccccccccccccc

            Ee2 = Ee2 + 1.0d0

            Do Itemp=1,Ntemp
               J            = Idint(Xold(Itemp)*1000.0d0)
               Ee1(J,Itemp) = Ee1(J,Itemp) + 1.0d0
            Enddo
         Enddo
      Enddo

      Close(35)

      Ee2 = 1.0d0/Ee2

Cccccccccccccccccccccccccccccc
C     Write Stuff To Disk    C
Cccccccccccccccccccccccccccccc

      Do I=1,Ntemp
         Ee1(0,I)=Ee1(0,I)*0.5d0
         Open(33,File='Temp'//Char(I+48)//'.Dat')
         Do J=-2100,2100
            If(Ee1(J,I).Gt.0.5d0) Write(33,'(2e20.10)') 
     &           Dble(J)*0.001d0,Ee1(J,I)*Ee2
         Enddo
         Close(33)
      Enddo

Cccccccccccccccccccccccccccccc
C     Write Info To Screen   C
Cccccccccccccccccccccccccccccc

      Do I=1,Ntemp
         Write(6,*)
         Write(6,*)
         Write(6,*) 'Temperature Level         : ',I
         Write(6,*) 'Temperature               : ',
     &        1.0d0/Beta(I)
         
         If(Move2(I).Gt.0.5d0) Then
            Write(6,*) 'Number Of Displacements   : ',
     &           Move2(I)
            Write(6,*) 'Fraction Successfull      : ',
     &           Move1(I)/Move2(I)
         Endif

         If(Swap2(I).Gt.0.5d0) Then
            Write(6,*) 'Number Of Swap (I-1)      : ',
     &           Swap2(I)
            Write(6,*) 'Fraction Successfull      : ',
     &           Swap1(I)/Swap2(I)
         Endif
      Enddo

      Return
      End
