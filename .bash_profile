# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi





# OpenFOAM-2.4.x
OpenFOAM-2.4.x()
{
  export inst_loc=/nopt/nrel/ecom/wind/OpenFOAM
  export spack_loc=/nopt/nrel/ecom/wind

  echo "Purging and loading modules..."
  module purge
  module use $spack_loc/spack/share/spack/modules/linux-centos7-x86_64
  module load gcc-4.8.5-gcc-4.8.5-nkqvx2b
  module load openmpi-1.10.7-gcc-4.8.5-qu7mrny
  module load cgal-4.12-gcc-4.8.5-vd4qb7j
  module load boost-1.69.0-gcc-4.8.5-52cfbc4
  module load mpfr-3.1.6-gcc-4.8.5-kflyoj7
  module load readline-7.0-gcc-4.8.5-b4z2xcj
  module load scotch-6.0.6-gcc-4.8.5-3xkg4i7
  module load flex-2.6.4-gcc-4.8.5-htapbeu
  module load gmp-6.1.2-gcc-4.8.5-6bsovvk
  module list

   # Unset OpenFOAM environment variables.
   if [ -z "$FOAM_INST_DIR" ]; then
      echo "Nothing to unset..."
   else
      echo "     *Unsetting OpenFOAM environment variables..."
      . $FOAM_INST_DIR/OpenFOAM-$OPENFOAM_VERSION/etc/config/unset.sh
   fi

   # Set the OpenFOAM version and installation directory
   export OPENFOAM_VERSION=2.4.x
   export OPENFOAM_NAME=OpenFOAM-$OPENFOAM_VERSION
   export FOAM_INST_DIR=$inst_loc
   export WM_PROJECT_USER_DIR=/home/$USER/$USER-$OPENFOAM_VERSION

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

# OpenFOAM-2.4.x
OpenFOAM-2.4.x_SSC()
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

   export WM_NCOMPPROCS=24
   export WM_COLOURS="white blue green cyan red magenta yellow"

   alias tut='cd /home/$USER/OpenFOAM/$USER-$OPENFOAM_VERSION/tutorials'

   export SOWFA_DIR=$FOAM_INST_DIR/SOWFA-$OPENFOAM_VERSION
   export OPENFAST_DIR=$FOAM_INST_DIR/openfast-openmpi-gcc-3.1.3-7.3.0/install
   export HDF5_DIR=$OPENFAST_DIR
   
   export LD_LIBRARY_PATH=$SOWFA_DIR/lib/$WM_OPTIONS:$OPENFAST_DIR/lib:$LD_LIBRARY_PATH
   export PATH=$SOWFA_DIR/applications/bin/$WM_OPTIONS:$PATH
}
