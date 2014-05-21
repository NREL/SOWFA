# +-------------------------------------------------------------------+
# | FAST 7.00  - makefile                                  10/10/2010 |
# |                                                                   |
# | Bare FAST/AeroDyn compile                                         |
# |                                                                   |
# | 'make FASTdll', to compile with DLL routines                      |
# |   not yet functional, missing routines                            |
# +-------------------------------------------------------------------+
# | Comment in/out sections for:                                      |
# |   linux GNU gfortran ver gcc 4.4.3                                |
# |   linux Intel Fortran ver 11.1                                    |
# |                                                                   |
# | Comment in/out sections for single/double precision               |
# |   double precision not yet functional                             |
# |                                                                   |
# | Dr. Hugh Currin, PhD, PE                                          |
# |   Oregon Institute of Technology                                  |
# |   Klamath Falls, OR                                               |
# +-------------------------------------------------------------------+

# +-------------------------------------------------------------------+
# | Set directories for your particular configuration                 |
# +-------------------------------------------------------------------+
#  Source directory for NWTC Library
NWTC_Lib_Loc   = ../NWTC-Subs/Source
#  Source directory for AeroDyn
AeroDyn_Loc    = ../AeroDyn/Source
#  Source directory for Wind
Wind_Loc       = ../AeroDyn/Source/InflowWind/Source
#  source directory for FAST
FAST_Loc       = ../FAST/Source

# +-------------------------------------------------------------------+
# | Comment in/out for GNU gfortran or Intel Fortran Compiler         |
# +-------------------------------------------------------------------+
# gnufortran
compiler = gfortran
sys      = SysGnu-linux
#
# Intel Fortran
#compiler = ifort
#sys      = SysIVF-linux

# +-------------------------------------------------------------------+
# | Comment in/out for single or double prections                     |
# |   currently only single precision is funtioning                   |
# +-------------------------------------------------------------------+
prec = SingPrec
#prec = DoubPrec

# -- Link FAST program --
objects = Precision.o Sys.o NWTC_IO.o NWTC_Num.o NWTC_Aero.o NWTC_Library.o    \
	  Blade.o Blades.o                                                     \
          SharedInflowDefs.o HHWind.o FFWind.o FDWind.o CTWind.o UserWind.o    \
	  InflowWindMod.o SharedTypes.o AeroMods.o GenSubs.o AeroSubs.o        \
	  AeroDyn.o fftpack.o FFTMod.o HydroCalc.o FAST_Mods.o Noise.o         \
	  FAST_IO.o FAST.o FAST_Lin.o FAST2ADAMS.o PitchCntrl_ACH.o            \
	  UserSubs.o UserVSCont_KP.o AeroCalc.o SetVersion.o FAST_Prog.o       \
          fastinit.o fastrun.o fastend.o fastread.o fastgetbldpos.o fastgetbldforce.o

#dll_obj = Precision.o Sys.o NWTC_IO.o NWTC_Num.o NWTC_Aero.o NWTC_Library.o  \
#	SharedInflowDefs.o HHWind.o FFWind.o FDWind.o CTWind.o UserWind.o    \
#	InflowWindMod.o SharedTypes.o AeroMods.o GenSubs.o AeroSubs.o        \
#	AeroDyn.o fftpack.o FFTMod.o HydroCalc.o FAST_Mods.o Noise.o         \
#	FAST_IO.o FAST.o FAST_Lin.o FAST2ADAMS.o BladedDLLInterface.o        \
#	UserSubs_forBladedDLL.o UserVScont_KP_forBladedDLL.o AeroCalc.o      \
#	SetVersion.o FAST_Prog.o

FAST:	$(objects) makefile
	$(compiler) $(objects) -o $@

#FAST_dll:	$(dll_obj) makefile
#	dll = YES
#	$(compiler) $(dll_obj) -o $@


# ----NWTC Library Compiles----
Precision.o:	$(NWTC_Lib_Loc)/$(prec).f90 makefile 
	$(compiler) -c $(NWTC_Lib_Loc)/$(prec).f90 -o $@ 

Sys.o:	$(NWTC_Lib_Loc)/$(sys).f90 Precision.o makefile 
	$(compiler) -c $(NWTC_Lib_Loc)/$(sys).f90 -o $@ 

NWTC_IO.o:	$(NWTC_Lib_Loc)/NWTC_IO.f90 Sys.o makefile
	$(compiler) -c $(NWTC_Lib_Loc)/NWTC_IO.f90 -o $@

NWTC_Num.o:	$(NWTC_Lib_Loc)/NWTC_Num.f90 NWTC_IO.o makefile
	$(compiler) -c $(NWTC_Lib_Loc)/NWTC_Num.f90 -o $@

NWTC_Aero.o:	$(NWTC_Lib_Loc)/NWTC_Aero.f90 NWTC_Num.o makefile
	$(compiler) -c $(NWTC_Lib_Loc)/NWTC_Aero.f90 -o $@

NWTC_Library.o:	$(NWTC_Lib_Loc)/NWTC_Library.f90 NWTC_Aero.o makefile
	$(compiler) -c $(NWTC_Lib_Loc)/NWTC_Library.f90 -o $@

# ----Blade Information----
Blade.o:	$(Wind_Loc)/Blade.f90 Precision.o makefile
	$(compiler) -c $(Wind_Loc)/Blade.f90 -o $@

Blades.o:	$(Wind_Loc)/Blades.f90 Precision.o makefile
	$(compiler) -c $(Wind_Loc)/Blades.f90 -o $@


# ----Wind Compiles----
SharedInflowDefs.o:	$(Wind_Loc)/SharedInflowDefs.f90 NWTC_Library.o makefile
	$(compiler) -c $(Wind_Loc)/SharedInflowDefs.f90 -o $@

HHWind.o:	$(Wind_Loc)/HHWind.f90 SharedInflowDefs.o makefile
	$(compiler) -c $(Wind_Loc)/HHWind.f90 -o $@

FFWind.o:	$(Wind_Loc)/FFWind.f90 SharedInflowDefs.o makefile
	$(compiler) -c $(Wind_Loc)/FFWind.f90 -o $@

FDWind.o:	$(Wind_Loc)/FDWind.f90 SharedInflowDefs.o makefile
	$(compiler) -c $(Wind_Loc)/FDWind.f90 -o $@

CTWind.o:	$(Wind_Loc)/CTWind.f90 SharedInflowDefs.o makefile
	$(compiler) -c $(Wind_Loc)/CTWind.f90 -o $@

UserWind.o:	$(Wind_Loc)/UserWind.f90 SharedInflowDefs.o makefile
	$(compiler) -c $(Wind_Loc)/UserWind.f90 -o $@

InflowWindMod.o:	$(Wind_Loc)/InflowWindMod.f90 FFWind.o HHWind.o FDWind.o  \
			CTWind.o UserWind.o  makefile
	$(compiler) -c $(Wind_Loc)/InflowWindMod.f90 -o $@


# ----AeroDyn Compiles----
SharedTypes.o:	$(AeroDyn_Loc)/SharedTypes.f90 NWTC_Library.o makefile
	$(compiler) -c $(AeroDyn_Loc)/SharedTypes.f90 -o $@

AeroMods.o:	$(AeroDyn_Loc)/AeroMods.f90 Precision.o makefile
	$(compiler) -c $(AeroDyn_Loc)/AeroMods.f90 -o $@

GenSubs.o:	$(AeroDyn_Loc)/GenSubs.f90 AeroMods.o NWTC_Library.o makefile
	$(compiler) -c $(AeroDyn_Loc)/GenSubs.f90 -o $@

AeroSubs.o:	$(AeroDyn_Loc)/AeroSubs.f90 InflowWindMod.o makefile
	$(compiler) -c $(AeroDyn_Loc)/AeroSubs.f90 -o $@

AeroDyn.o:	$(AeroDyn_Loc)/AeroDyn.f90 InflowWindMod.o makefile
	$(compiler) -c $(AeroDyn_Loc)/AeroDyn.f90 -o $@


# ----FAST Compiles----
fftpack.o:	$(FAST_Loc)/fftpack.f Precision.o makefile
	$(compiler) -c $(FAST_Loc)/fftpack.f -o $@

FFTMod.o:	$(FAST_Loc)/FFTMod.f90 NWTC_Library.o makefile
	$(compiler) -c $(FAST_Loc)/FFTMod.f90 -o $@

HydroCalc.o:	$(FAST_Loc)/HydroCalc.f90 FFTMod.o makefile
	$(compiler) -c $(FAST_Loc)/HydroCalc.f90 -o $@

FAST_Mods.o:	$(FAST_Loc)/FAST_Mods.f90 AeroDyn.o makefile
	$(compiler) -c $(FAST_Loc)/FAST_Mods.f90 -o $@

Noise.o:	$(FAST_Loc)/Noise.f90 NWTC_Library.o FAST_Mods.o  makefile
	$(compiler) -c $(FAST_Loc)/Noise.f90 -o $@

FAST_IO.o:	$(FAST_Loc)/FAST_IO.f90 FAST_Mods.o HydroCalc.o Noise.o  makefile
	$(compiler) -c $(FAST_Loc)/FAST_IO.f90 -o $@

FAST.o:	$(FAST_Loc)/FAST.f90 FAST_IO.o Noise.o makefile
	$(compiler) -c $(FAST_Loc)/FAST.f90 -o $@

FAST_Lin.o:	$(FAST_Loc)/FAST_Lin.f90 FAST.o makefile
	$(compiler) -c $(FAST_Loc)/FAST_Lin.f90 -o $@

FAST2ADAMS.o:	$(FAST_Loc)/FAST2ADAMS.f90 FAST.o makefile
	$(compiler) -c $(FAST_Loc)/FAST2ADAMS.f90 -o $@

AeroCalc.o:	$(FAST_Loc)/AeroCalc.f90 FAST2ADAMS.o FAST_Lin.o makefile
	$(compiler) -c $(FAST_Loc)/AeroCalc.f90 -o $@

SetVersion.o:	$(FAST_Loc)/SetVersion.f90 FAST.o makefile
	$(compiler) -c $(FAST_Loc)/SetVersion.f90 -o $@

FAST_Prog.o:	$(FAST_Loc)/FAST_Prog.f90 FAST2ADAMS.o FAST_Lin.o makefile
	$(compiler) -c $(FAST_Loc)/FAST_Prog.f90 -o $@

#-----With DLL-----
#  BladedDLLInterface.o:	$(FAST_Loc)/BladedDLLInterface.f90 makefile
#	$(compiler) -c $(FAST_Loc)/BladedDLLInterface.f90 -o $@
#
#  UserSubs_forBladedDLL.o:	$(FAST_Loc)/UserSubs_forBladedDLL.f90 makefile
#	$(compiler) -c $(FAST_Loc)/UserSubs_forBladedDLL.f90 -o $@
#
#  UserVSCont_KP_forBladedDLL.o:	$(FAST_Loc)/UserVSCont_KP_forBladedDLL.f90 makefile
#	$(compiler) -c $(FAST_Loc)/UserVSCont_KP_forBladedDLL.f90 -o $@

#-----NO DLL-----
  PitchCntrl_ACH.o:	$(FAST_Loc)/PitchCntrl_ACH.f90 NWTC_Library.o makefile
	$(compiler) -c $(FAST_Loc)/PitchCntrl_ACH.f90 -o $@

  UserSubs.o:	$(FAST_Loc)/UserSubs.f90 makefile
	$(compiler) -c $(FAST_Loc)/UserSubs.f90 -o $@

  UserVSCont_KP.o:	$(FAST_Loc)/UserVSCont_KP.f90 makefile
	$(compiler) -c $(FAST_Loc)/UserVSCont_KP.f90 -o $@

  fastinit.o:	$(FAST_Loc)/fastinit.f90 makefile
	$(compiler) -c $(FAST_Loc)/fastinit.f90 -o $@

  fastrun.o:	$(FAST_Loc)/fastrun.f90 makefile
	$(compiler) -c $(FAST_Loc)/fastrun.f90 -o $@

  fastend.o:	$(FAST_Loc)/fastend.f90 makefile
	$(compiler) -c $(FAST_Loc)/fastend.f90 -o $@

  fastread.o:	$(FAST_Loc)/fastread.f90 makefile
	$(compiler) -c $(FAST_Loc)/fastread.f90 -o $@

  fastgetbldpos.o:	$(FAST_Loc)/fastgetbldpos.f90 makefile
	$(compiler) -c $(FAST_Loc)/fastgetbldpos.f90 -o $@

  fastgetbldforce.o:	$(FAST_Loc)/fastgetbldforce.f90 makefile
	$(compiler) -c $(FAST_Loc)/fastgetbldforce.f90 -o $@

	
# Clean up *.o and *.mod files
.PHONY: clean
clean:
	rm *.o
	rm *.mod
	rm *.a

