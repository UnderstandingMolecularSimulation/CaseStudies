      Program Avhb
      Implicit None

Ccccccccccccccccccccccccccccccccccccccccccccccc
C     Calculate Hb'(T) And Hb'(T)/Hb(T)       C
C     Hb(T) Is Fitted With A Straight Line    C
C                                             C
C     Input  : Fort.21                        C
C     Output : Fort.22                        C
C                                             C
C     Written By Thijs J.H. Vlugt (1999)      C
Ccccccccccccccccccccccccccccccccccccccccccccccc

      Integer Ipoint,I,Ishift,M1,M2,J,
     &     Istart,Maxpoint

      Parameter(Maxpoint=25000)

      Double Precision Af,Ag,Sumx2,Sumxy,N,
     &     Sumx,Sumy,X(Maxpoint),Y(Maxpoint),B

      Write(*,*) 'Number Of Points ?'
      Read(*,*)  Ipoint

      Write(*,*) 'Number Of Shift ?'
      Read(*,*) Ishift

Cccccccccccccccccccccc
C     Read In Data   C
Cccccccccccccccccccccc

      Do I=1,Ipoint
         Read(21,*) J,Y(I)

         X(I) = Dble(J)
      Enddo

      Do I=1,Ipoint
         
         If(Y(I).Gt.0.1d0) Then

Cccccccccccccccccccc
C     Regression   C
Cccccccccccccccccccc

            M1 = Min(I+Ishift,Ipoint)
            M2 = Max(I-Ishift,1)

            N     = 0.0d0
            Sumx  = 0.0d0
            Sumy  = 0.0d0
            Sumx2 = 0.0d0
            Sumxy = 0.0d0
            
            Do J=M2,M1
               N     = N     + 1.0d0
               Sumx  = Sumx  + X(J)
               Sumy  = Sumy  + Y(J)
               Sumx2 = Sumx2 + X(J)*X(J)
               Sumxy = Sumxy + X(J)*Y(J)
            Enddo

            Af  = (N*Sumxy - 
     &           (Sumx*Sumy))/
     &           (N*Sumx2 - (Sumx*Sumx))
            B   = (Sumy - Af*Sumx)/N
            Ag  = Af/(Af*Dble(I) + B)

            Write(22,'(I7,2(1x,G20.10))') 
     &           I,Af,Ag
         Endif
      Enddo

      Write(*,*) 'Start Tofit The Slope At ?'
      Read(*,*) Istart

      N     = 0.0d0
      Sumx  = 0.0d0
      Sumy  = 0.0d0
      Sumx2 = 0.0d0
      Sumxy = 0.0d0
            
      Do J=Istart,Ipoint
         N     = N     + 1.0d0
         Sumx  = Sumx  + X(J)
         Sumy  = Sumy  + Y(J)
         Sumx2 = Sumx2 + X(J)*X(J)
         Sumxy = Sumxy + X(J)*Y(J)
      Enddo

      Write(6,*) 'Slope : ',
     &     (N*Sumxy - (Sumx*Sumy))/
     &     (N*Sumx2 - (Sumx*Sumx))
         
      Stop
      End
