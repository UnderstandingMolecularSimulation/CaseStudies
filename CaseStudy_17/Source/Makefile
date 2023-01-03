COMPFLAGS     = -O # -check_bounds  -g -trapeuv

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
		cell.o \
		dist2.o \
		eneri.o \
		enlat.o \
		lattice.o \
		mc_nvt.o \
		mcmove.o \
		neigcell.o \
		new_nlist.o \
		ranf.o \
		rantest.o \
		readdat.o \
		sample.o \
		setlat.o \
		store.o \
		toterg.o

PRINT	      = pr

PRINTFLAGS    =

LP	      = lp

LPFLAGS       = 

PROGRAM       = mc_nvt

SHELL	      = /bin/sh

SRCS	      = adjust.f \
		cell.f \
		dist2.f \
		eneri.f \
		enlat.f \
		lattice.f \
		mc_nvt.f \
		mcmove.f \
		neigcell.f \
		new_nlist.f \
		ranf.f \
		rantest.f \
		readdat.f \
		sample.f \
		setlat.f \
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
