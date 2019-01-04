.SUFFIXES: .F .o .f90

############ COMPILER SPECIFIC SETTINGS
#
############ uncomment this section for Intel Fortran
FC      = ifort

############ uncomment this section for gfortran
#FC      = gfortran
# Also uncomment these next two lines and set ICCLIBPATH to correct path on your 
# system if using gfortran but with a netCDF library that was compiled with Intel C 
# This will get rid of undefined refs to intel_fast_memset, etc. The path shown
# is for the Redrock system at NREL, as of 20101230.
#ICCLIBPATH = /projects/global/x86_64/compilers/intel/intel-11.1-cproc-064/lib/intel64
#LIB = -L$(ICCLIBPATH) -lirc
#
############ LIBRARY SETTINGS
#
# Set to the path that contains the netecdf lib and include directories on your system
#
#NETCDFPATH      =    /projects/nrel/apps/netcdf/4.0.1
#NETCDFPATH      =    /nopt/nrel/apps/netcdf/dist/netcdf
NETCDFPATH		=  	 /usr
#
############
#
LD      = $(FC)
CPP     = /lib/cpp -C -P
FCFLAGS = -g -C
FCFLAGS = -O3 -g
LDFLAGS = $(FCFLAGS)

wrftoof : wrftoof.o module_dm.o module_openfoam_bc.o
	$(LD) -o wrftoof $(LDFLAGS) wrftoof.o module_dm.o module_openfoam_bc.o \
		-L$(NETCDFPATH)/lib -L$(NETCDFPATH)/lib64 -lnetcdf -lnetcdff $(LIB)
	@ if [ -n "$(ICCLIBPATH)" ] ; then \
            echo ------------------------------------------------------------------------- ; \
            echo A D V I S O R Y : ;\
            echo "  " You may need to add: ;\
            echo "     " $(ICCLIBPATH) ;\
            echo "  " to LD_LIBRARY_PATH in your shell environment. ; \
            echo ------------------------------------------------------------------------- ; \
        fi

clean :
	/bin/rm -f *.o *.f90 *.mod

superclean : clean
	/bin/rm -f wrftoof


.F.o:
	$(CPP) $*.F > $*.f90
	$(FC) -o $@ -c $(FCFLAGS) -I$(NETCDFPATH)/include $*.f90

#
wrftoof.o : module_openfoam_bc.o module_dm.o

module_openfoam_bc.o : module_dm.o

