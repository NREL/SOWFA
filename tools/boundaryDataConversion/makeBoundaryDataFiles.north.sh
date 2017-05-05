#!/bin/bash
#
# Script to take output of OpenFOAM sample patch function object and make it into format
# needed by timeVaryingFixedMapped boundary condition.  Requires a directory of sample
# patch data called "boundaryDataPre".


# Name of boundary to process.
#boundaryNameOld="west"
#boundaryNameNew="west"

boundaryNameOld="north"
boundaryNameNew="north"

#boundaryNameOld="top"
#boundaryNameNew="top"

# Create the output boundary data file directory called "boundaryData".
mkdir boundaryData
cd boundaryData

# Within that directory, create a boundary condition specific directory.
mkdir $boundaryNameNew
cd $boundaryNameNew

# Make a list of times in which patches were sampled.
ls ../../boundaryDataPre > timeList
lim=`wc -l timeList | awk '{print $1}'`;

# Loop over the various times and process the data.
for ((index = 1; index < $lim; index = index+1));
do
   # get the time for this index.
   time=`awk -v var=$index '{if (NR==var) {print}}' timeList`;

   # process the points file.
   if [[ index -eq 1 ]]
   then
      cp ../../boundaryDataPre/$time/$boundaryNameOld/points ./vertices
      cp ../../boundaryDataPre/$time/$boundaryNameOld/faces ./
      cp ../../makeBoundaryDataFiles/points.py ./
      # call the points file processing python script.
      #                                        xmin      xmax   ymin      ymax   zmin   zmax   orientation
      #./points.py vertices faces points index -0.1       0.1    0.0   10000.0    0.0 1000.1   yz
      #./points.py vertices faces points index  0.0    4000.0 4000.0    4000.0    0.0 1000.0   xz
       ./points.py vertices faces points index  0.0   10000.0 7999.0    8000.1    0.0 1000.1   xz
      #./points.py vertices faces points index  0.0    4000.0    0.0    4000.0 1000.0 1000.0   xy
      rm points.py faces vertices
   fi

   # make the time directory and process the flow variable data. Note, that this
   # is set up to process U, pd, T, nuLES, and kappaLES.  Add more variables
   # as desired.
   mkdir $time
   cd $time
   cp ../../../boundaryDataPre/$time/$boundaryNameOld/vectorField/U ./UPre
   #cp ../../../boundaryDataPre/$time/$boundaryNameOld/scalarField/pd ./pdPre
   cp ../../../boundaryDataPre/$time/$boundaryNameOld/scalarField/T ./TPre
   cp ../../../boundaryDataPre/$time/$boundaryNameOld/scalarField/k ./kPre
   #cp ../../../boundaryDataPre/$time/$boundaryNameOld/scalarField/nuLES ./nuLESPre
   #cp ../../../boundaryDataPre/$time/$boundaryNameOld/scalarField/kappaLES ./kappaLESPre
   cp ../../../makeBoundaryDataFiles/data.py ./
   # call the data file processing python script.
   ./data.py UPre U ../index
   #./data.py pdPre pd ../index
   ./data.py TPre T ../index
   ./data.py kPre k ../index
   #./data.py nuLESPre nuLES ../index
   #./data.py kappaLESPre kappaLES ../index
   rm data.py *Pre	

   cd ../
done
rm timeList
rm index
