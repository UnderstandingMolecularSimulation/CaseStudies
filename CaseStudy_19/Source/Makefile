FCOMPFLAGS    =	-O
CCOMPFLAGS    =	-O

FFLAGS        =	$(FCOMPFLAGS)
CFLAGS        = $(CCOMPFLAGS)
LDFLAGS       =	$(FCOMPFLAGS)

LD = gfortran
FC = gfortran
CC            =	cc

MAKEFILE      =	Makefile
PROGRAM       =	cbmc

INCS	      =	system.inc

OBJS	      =	cbmc.o         \
		grow.o         \
		initlat.o      \
		mccbmc.o       \
		ran_uniform.o  \
		sample_bond.o  \
		sample_gyra.o  \
		select.o       \
		sphere.o       \
		sstmm.o

all:		$(PROGRAM)

$(PROGRAM)::	$(INCS)
		@/bin/rm -f $(OBJS) core

$(PROGRAM)::	$(OBJS) $(MAKEFILE)
		@$(LD) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

clean:;		@rm -f $(OBJS) core
