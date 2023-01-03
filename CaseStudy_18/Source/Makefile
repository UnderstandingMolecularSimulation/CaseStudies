COMPFLAGS     =  -O #  -check_bounds  -g -trapeuv

CFLAGS	      = $(COMPFLAGS)
PFLAGS	      = $(COMPFLAGS)
FFLAGS	      = $(COMPFLAGS)
CCFLAGS       = $(COMPFLAGS)
CXXFLAGS      = $(COMPFLAGS)

DEST	      = .

# Uncomment this if you want system header files to be expanded
#
# SYSHDRS       =

EXTHDRS	      =

HDRS	      =

INSTALL	      = install

LD	      = f77

LDFLAGS	      = $(COMPFLAGS)

LIBS	      =

LINTLIBS      =

LINTFLAGS     = -u $(CFLAGS)

MAKEFILE      = Makefile

OBJS	      = adjust.o \
		bondl_g.o \
		cbmc.o \
		eneri.o \
		enerlj.o \
		enerr.o \
		enint.o \
		gauss.o \
		grow.o \
		init.o \
		initlat.o \
		mccbmc.o \
		mcmove.o \
		mcvol.o \
		potentials.o \
		ranf.o \
		ranor.o \
		rantest.o \
		sample.o \
		select.o \
		store.o \
		toterg.o

PRINT	      = pr

PRINTFLAGS    =

LP	      = lp

LPFLAGS       = 

PROGRAM       = Cbmc  

SHELL	      = /bin/sh

SRCS	      = adjust.f \
		bondl_g.f \
		cbmc.f \
		eneri.f \
		enerlj.f \
		enerr.f \
		enint.f \
		gauss.f \
		grow.f \
		init.f \
		initlat.f \
		mccbmc.f \
		mcmove.f \
		mcvol.f \
		potentials.f \
		ranf.f \
		ranor.f \
		rantest.f \
		sample.f \
		select.f \
		store.f \
		toterg.f

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS) $(LIBS) $(MAKEFILE)
		@echo "Linking $(PROGRAM) ..."
		@$(LD) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f $(OBJS) core

clobber:;	@rm -f $(OBJS) $(PROGRAM) core tags

depend:;	@mkmf -f $(MAKEFILE) ROOT=$(ROOT)

echo:;		@echo $(HDRS) $(SRCS)

-1:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@-strip $(PROGRAM)
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(PROGRAM); $(INSTALL) -f $(DEST) $(PROGRAM)); fi

lint:		$(LINTLIBS) $(SRCS) $(HDRS) $(EXTHDRS)
		lint $(LINTFLAGS) $(LINTLIBS) $(SRCS)

print:;		@$(PRINT) $(PRINTFLAGS) $(HDRS) $(SRCS) | $(LP) $(LPFLAGS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

touch:;		@touch $(SRCS) $(HDRS)
