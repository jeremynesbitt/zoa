# ============================================================================
# Name        : Makefile
# Author      : Jeremy Nesbitt
# Version     :
# Copyright   : Your copyright notice
# Description : Makefile for Hello World in Fortran
# ============================================================================

.PHONY: all clean

# Change this line if you are using a different Fortran compiler
FORTRAN_COMPILER = gfortran
FFLAGS= -static-libgfortran -static-libgcc -mmacosx-version-min=12.3 -fPIC -fno-align-commons -fPIC#optimization (for level 3) flags, compiler warnings and the strictest adherence to the latest standards
LFLAGS= -static-libgfortran -static-libgcc -mmacosx-version-min=12.3 -fPIC -fno-align-commons -fPIC#optimization (for level 3) flags, compiler warnings and the strictest adherence to the latest standards
# Dropped flags -fconvert=swap 
SRC := $(wildcard src/*.FOR)
SRCETC :=  $(wildcard etc/*.f90)
# Option to only have shared object and not executable. TODO switch with a flag
# PROG = src/TSTKDP.FOR
GTK  = src/zoamain.f90
GLOBFOR = src/globals.FOR src/nssmod.FOR
GLOB  = src/zoa-ui.f90 src/plotsettingtypes.f90 src/kdp-data-types.f90 src/global_widgets.f90 src/gtk-hl-zoa.f90 src/zoa-plot.f90 src/zoa-macro-ui.f90 src/zoamenubar.f90 src/zzhandlers.f90 src/kdp-draw.f90 src/ROUTEDRAWING.f90 src/lens-editor.f90 src/zoa-tab.f90 src/mod_plotrayfan.f90 src/ui-ast-fc-dist.f90 src/kdp-interfaces.f90  src/mod_plotopticalsystem.f90
IFACES = src/kdp-interfaces.f90

GTKMID = ${GTK:.f90=.mod}
#GTKOBJ := ${GTKMID:src/=bin/}
GTKOBJ = bin/zoamain.mod

#GLOBOBJ = bin/zoa-ui.mod


#IFACESOBJ := bin/kdp-interfaces.mod

# SRC := $(filter-out $(PROG),$(SRC))
SRC90 := $(wildcard src/*.f90)
SRC90 := $(filter-out $(GTK) $(GLOB),$(SRC90))
OBJ90:=${SRC90:src/%.f90=bin/%.mod}
GLOBOBJ = ${GLOB:src/%.f90=bin/%.mod}
GLOBFOROBJ = bin/globals.o bin/nssmod.o
IFACESOBJ = ${IFACES:src/%.f90=bin/%.mod}
OBJETC :=  ${SRCETC:etc/%.f90=bin/%.mod}

#MID90=${SRC:.f90=.o} #substitute .f90 with .o
#MID=${SRC:.FOR=.o} #substitute .FOR with .o
#OBJ90=${MID:src/=bin/}
#OBJ=${MID:src/=bin/}
SLIBRARY = KDPJN
OBJ:=${SRC:src/%.FOR=bin/%.o}


	
# Output F90 files into .mod files
	#@echo $(SRC90)
	#@echo $(GLOB)
	#@echo $(IFACES)
bin/%.mod: src/%.f90
#$(OBJ90)/$(SRC90): $(GLOBOBJ)
	@echo F90 Compilation	

	$(FORTRAN_COMPILER) $(FFLAGS) -g -Og -o $@ -c $< $$(pkg-config --libs --cflags gtk-4-fortran plplot-fortran plplot) 

# Output FOR files into .o files	
#	@echo $(OBJ)
bin/%.o: src/%.FOR
	@echo FOR Compilation	

	$(FORTRAN_COMPILER) $(FFLAGS) -g -Og -o $@ -c $<

$(OBJETC):etc/*.f90
	$(FORTRAN_COMPILER) $(FFLAGS) -g -Og -o $@ -c $< $$(pkg-config --libs --cflags gtk-4-fortran plplot-fortran plplot) 

#$(IFACESOBJ):$(IFACES)
#$(IFACESOBJ):bin/%.mod : src/%.f90
#	$(FORTRAN_COMPILER) $(FFLAGS) -g -Og -o $@ -c $< $$(pkg-config --libs --cflags gtk-4-fortran plplot-fortran plplot h5fortran) 

	#@echo $(GTKOBJ)	
#$(GLOBOBJ): $(GLOB)
$(GLOBOBJ): bin/%.mod : src/%.f90
	@echo Starting Global Compilation

	$(FORTRAN_COMPILER) $(FFLAGS) -g -Og -o $@ -c $< $$(pkg-config --libs --cflags gtk-4-fortran plplot-fortran plplot) 

$(GLOBFOROBJ): bin/%.o : src/%.FOR
	@echo Starting Global Compilation

	$(FORTRAN_COMPILER) $(FFLAGS) -g -Og -o $@ -c $< $$(pkg-config --libs --cflags)


$(GTKOBJ): $(GTK)
	@echo Starting Compilation
	@echo $(GTKOBJ)	
	$(FORTRAN_COMPILER) $(FFLAGS) -g -Og -c $(GTK) $$(pkg-config --libs --cflags gtk-4-fortran plplot-fortran plplot) -o $@ 

# Compile f90 files first, then F77 Files
#all: $(GTKOBJ) $(OBJ90) $(OBJ) 
#all:  $(OBJ90) $(OBJ) $(GLOBOBJ) $(GTKOBJ) 
all:  $(OBJETC) $(GLOBFOROBJ) $(GLOBOBJ) $(OBJ90) $(OBJ) $(GTKOBJ) 
	@echo $(GTKMID)
	@echo $(GTKOBJ)
	@echo $(GLOBOBJ)
	@echo Generating Executable	
#	@echo $(OBJ)
#	$(FORTRAN_COMPILER) $(LFLAGS) -g -Og -o bin/KDPJN $(OBJ) $(OBJ90)
#	$(FORTRAN_COMPILER) $(LFLAGS) -g -Og -o bin/HELLOWORLD.so $(OBJ) $(OBJ90)
#	$(FORTRAN_COMPILER) $(LFLAGS) -shared -ffree-form -g -Og -o bin/ZOA $(OBJ) $(OBJ90) $(GTKOBJ) $$(pkg-config --libs gtk-4-fortran) 
#	$(FORTRAN_COMPILER) $(LFLAGS) -o bin/ZOA $(OBJ) $(OBJ90) $(GTKOBJ) $$(pkg-config --libs gtk-4-fortran) $$(pkg-config --libs plplot-fortran)
#	$(FORTRAN_COMPILER) $(LFLAGS) -o bin/ZOA $(OBJ) $(OBJ90) $(GTKOBJ) $(GLOBOBJ) $$(pkg-config --libs --cflags gtk-4-fortran plplot-fortran plplot h5fortran)
	$(FORTRAN_COMPILER) $(LFLAGS) -o bin/ZOA $(OBJ) $(OBJETC) $(OBJ90)  $(GLOBOBJ) $(GTKOBJ) $$(pkg-config --libs --cflags gtk-4-fortran plplot-fortran plplot h5fortran hdf5_fortran hdf5_hl_fortran) -rpath /usr/local/HDF_Group/HDF5/1.13.2/lib 

#$(pkg-config --libs --cflags h5fortran hdf5_fortran hdf5_hl_fortran) -rpath /usr/local/HDF_Group/HDF5/1.13.2/lib


glob: $(GLOBOBJ)

clean:
	rm -f bin/*.o
	rm -f bin/*.mod
	rm -f *.mod
