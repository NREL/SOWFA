# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi


# Also load candidate modules.
module use -a /nopt/nrel/apps/modules/candidate/modulefiles


# OpenFOAM
OpenFOAM-2.4.x()
{
   # Unset OpenFOAM environment variables.
   if [ -z "$FOAM_INST_DIR" ]; then
      echo "Nothing to unset..."
   else  
      echo "Unsetting OpenFOAM environment variables..."
      . $FOAM_INST_DIR/OpenFOAM-$OPENFOAM_VERSION/etc/config/unset.sh
   fi

   # Unload any compilers already loaded
   echo "Purging modules..."
   module purge

   # Load the appropriate compiler
   echo "Loading compilers..."
   module load openmpi-gcc/1.7.3-4.8.2
   module list

   # Set the OpenFOAM version and installation directory
   export OPENFOAM_VERSION=2.4.x
   export OPENFOAM_NAME=OpenFOAM-$OPENFOAM_VERSION
   export FOAM_INST_DIR=/nopt/nrel/apps/openfoam/dist

   # Source the OpenFOAM bashrc file
   sourceFOAM
}


sourceFOAM()
{
   foamDotFile=$FOAM_INST_DIR/$OPENFOAM_NAME/etc/bashrc
   if [ -f $foamDotFile ] ; then
      echo "Sourcing $foamDotFile..."
      source $foamDotFile
   fi
   export WM_PROJECT_USER_DIR=$HOME/bin/OpenFOAM/$USER-$OPENFOAM_VERSION
   export WM_NCOMPPROCS=12
   export WM_COLOURS="white blue green cyan red magenta yellow"
   export OPENFAST_DIR=$HOME/bin/OpenFOAM/OpenFAST/install
   export HDF5_DIR=$HOME/bin/OpenFOAM/OpenFAST/modules-ext/hdf5-1.8.17
   export FOAM_USER_LIBBIN=$WM_PROJECT_USER_DIR/lib/$WM_OPTIONS
   export FOAM_USER_APPBIN=$WM_PROJECT_USER_DIR/applications/bin/$WM_OPTIONS
   export FOAM_RUN=$WM_PROJECT_USER_DIR/run
   export LD_LIBRARY_PATH=$FOAM_USER_LIBBIN:$LD_LIBRARY_PATH
   export LD_LIBRARY_PATH=$FAST_DIR/lib/:$LD_LIBRARY_PATH
   export PATH=$FOAM_USER_APPBIN:$PATH
}
