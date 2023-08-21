FCOMPFLAGS    =	-O
CCOMPFLAGS    =	-O

FFLAGS        =	$(FCOMPFLAGS)
CFLAGS        = $(CCOMPFLAGS)
LDFLAGS       =	$(FCOMPFLAGS)

LD = gfortran
FC = gfortran
CC            =	cc

MAKEFILE      =	Makefile
PROGRAM       =	dpd

INCS	      =	maxarray.inc selflist.inc system.inc

OBJS	      =	dpd.o           \
		exitt.o         \
		force.o         \
		force_minim.o   \
		minim.o         \
		ran_gauss.o     \
		ran_sphere.o    \
		ran_uniform.o   \
		readdat.o       \
		sample_dens.o   \
		sample_gyra.o   \
		sstmm.o         \
		ttime.o         \
		writepdb.o

all:		$(PROGRAM)

$(PROGRAM)::	$(INCS)
		@/bin/rm -f $(OBJS) core

$(PROGRAM)::	$(OBJS) $(MAKEFILE)
		@$(LD) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

clean:;		@rm -f $(OBJS) core
