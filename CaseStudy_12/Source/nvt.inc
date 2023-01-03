c nvt.inc
      double precision temp
      logical nvt
      integer metht
      common/nvtc/temp,metht,nvt
c
c     temp  : impost temperature
c     metht : method used in simulation:
c             1 : Andersen
c             2 : Nose-Hoover
c     NVT   : if .true. simulate NVT ensemble
c
