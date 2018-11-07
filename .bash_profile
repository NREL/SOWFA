# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi





# SOWFA-2.4.x
SOWFA-2.4.x()
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

   # Load the appropriate modules
   echo "Loading modules..."
   module load openmpi-gcc/1.7.3-4.8.2
   module load mkl/13.5.192
   module load cmake/3.7.2
   module list

   # Set the OpenFOAM version and installation directory
   export OPENFOAM_VERSION=2.4.x
   export OPENFOAM_NAME=OpenFOAM-$OPENFOAM_VERSION
   export FOAM_INST_DIR=/projects/windsim/OpenFOAM

   # Set the ZeroMQ compilation option to false
   echo "Disabling the compilation and usage of ZeroMQ."
   export COMPILEZEROMQ=0
   
   foamDotFile=$FOAM_INST_DIR/$OPENFOAM_NAME/etc/bashrc
   if [ -f $foamDotFile ] ; then
      echo "Sourcing $foamDotFile..."
      source $foamDotFile
   fi
   export WM_PROJECT_USER_DIR=$FOAM_INST_DIR/SOWFA-$OPENFOAM_VERSION
   export WM_NCOMPPROCS=12
   export WM_COLOURS="white blue green cyan red magenta yellow"
   export OPENFAST_DIR=$FOAM_INST_DIR/openfast-openmpi-gcc-1.7.3-4.8.2/install
   export HDF5_DIR=$FOAM_INST_DIR/openfast-openmpi-gcc-1.7.3-4.8.2/install
   export FOAM_USER_LIBBIN=$WM_PROJECT_USER_DIR/lib/$WM_OPTIONS
   export FOAM_USER_APPBIN=$WM_PROJECT_USER_DIR/applications/bin/$WM_OPTIONS
   export FOAM_RUN=$WM_PROJECT_USER_DIR/run
   export LD_LIBRARY_PATH=$FOAM_USER_LIBBIN:$LD_LIBRARY_PATH
   export LD_LIBRARY_PATH=$OPENFAST_DIR/lib/:$LD_LIBRARY_PATH
   export PATH=$FOAM_USER_APPBIN:$PATH
}

# SOWFA-2.4.x
SOWFA-2.4.x_SSC()
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

   # Load the appropriate modules
   echo "Loading modules..."
   module load openmpi-gcc/1.7.3-4.8.2
   module load mkl/13.5.192
   module load cmake/3.7.2
   module list

   # Set the OpenFOAM version and installation directory
   export OPENFOAM_VERSION=2.4.x
   export OPENFOAM_NAME=OpenFOAM-$OPENFOAM_VERSION
   export FOAM_INST_DIR=/projects/windsim/OpenFOAM

   # Set the ZeroMQ compilation option to true
   echo "Enabling the compilation and usage of ZeroMQ."
   export COMPILEZEROMQ=1
   export ZEROMQ_HOME=$HOME/OpenFOAM/zeroMQ/libzmq/install
   export ZEROMQ_INCLUDE=$ZEROMQ_HOME/include
   export ZEROMQ_LIB=$ZEROMQ_HOME/lib64
   export LD_LIBRARY_PATH=$ZEROMQ_HOME/lib:$LD_LIBRARY_PATH
   export LD_LIBRARY_PATH=$ZEROMQ_HOME/lib64:$LD_LIBRARY_PATH    
   echo "Specified ZeroMQ directory: $ZEROMQ_HOME"   
   
   
   foamDotFile=$FOAM_INST_DIR/$OPENFOAM_NAME/etc/bashrc
   if [ -f $foamDotFile ] ; then
      echo "Sourcing $foamDotFile..."
      source $foamDotFile
   fi
   export WM_PROJECT_USER_DIR=$FOAM_INST_DIR/SOWFA-$OPENFOAM_VERSION
   export WM_NCOMPPROCS=12
   export WM_COLOURS="white blue green cyan red magenta yellow"
   export OPENFAST_DIR=$FOAM_INST_DIR/openfast-openmpi-gcc-1.7.3-4.8.2/install
   export HDF5_DIR=$FOAM_INST_DIR/openfast-openmpi-gcc-1.7.3-4.8.2/install
   export FOAM_USER_LIBBIN=$WM_PROJECT_USER_DIR/lib/$WM_OPTIONS
   export FOAM_USER_APPBIN=$WM_PROJECT_USER_DIR/applications/bin/$WM_OPTIONS
   export FOAM_RUN=$WM_PROJECT_USER_DIR/run
   export LD_LIBRARY_PATH=$FOAM_USER_LIBBIN:$LD_LIBRARY_PATH
   export LD_LIBRARY_PATH=$OPENFAST_DIR/lib/:$LD_LIBRARY_PATH
   export PATH=$FOAM_USER_APPBIN:$PATH
}
