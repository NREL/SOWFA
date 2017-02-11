#!/nopt/nrel/apps/python/2.7.6-gcc-4.8.2/bin/python

# This script reads in the data files created by OpenFOAM's sample patch
# function object and creates an updated file that works with the
# timeVaryingFixedMapped boundary condition.
#
# Usage:  ./data.py [oldDataFileName] [newDataFileName] [indexFileName]




# User input
import sys
print 'sys = ',sys.argv

oldFile = sys.argv[1]
newFile = sys.argv[2]
indexFile = sys.argv[3]



# Open the data file output by the sample patch function object.
fid = open(oldFile,'r')

# Find the number of data points.
nDataOld = '\n'
while nDataOld == '\n':
    nDataOld = fid.readline()
    
nDataOld = int(nDataOld)

# Read in data as text.
dataOld = []
fid.readline()
for i in range(nDataOld):
    dataOld.append(fid.readline())
    
# Close the data file.
fid.close()

# Find the size of the data (is it scalar, vector, tensor, symmTensor?)
dataSize = dataOld[0]
dataSize = dataSize.lstrip('(')
dataSize = dataSize.rstrip('\n')
dataSize = dataSize.rstrip(')')
dataSize = dataSize.split()
dataSize = len(dataSize)

# Read in the index file.
fid = open(indexFile,'r')

# Find the number of indices (or data points that will be kept).
nDataNew = '\n'
while nDataNew == '\n':
    nDataNew = fid.readline()
    
nDataNew = int(nDataNew)

# Read in indices
indicesNew = []
fid.readline()
for i in range(nDataNew):
    indicesNew.append(int(fid.readline()))
    
# Close the index file
fid.close()

# Open the new boundary condition data file.
fid = open(newFile,'w')

# Write the header.
if   dataSize == 1:
    dataType = 'scalar'
    average  = '0'
elif dataSize == 3:
    dataType = 'vector'
    average  = '(0 0 0)'
elif dataSize == 6:
    dataType = 'symmTensor'
    average  = '(0 0 0 0 0 0)'
elif dataSize == 9:
    dataType = 'tensor'
    average  = '(0 0 0 0 0 0 0 0 0)'
else:
    print 'Error: Unknown data type\n'
    print 'Need scalar, vector, symmTensor, or tensor'

  

# Write the file header.
fid.write('/*--------------------------------*- C++ -*----------------------------------*\\\n')
fid.write('| =========                 |                                                 |\n')
fid.write('| \\\\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox           |\n')
fid.write('|  \\\\    /   O peration     | Version:  1.6                                   |\n')
fid.write('|   \\\\  /    A nd           | Web:      http://www.OpenFOAM.org               |\n')
fid.write('|    \\\\/     M anipulation  |                                                 |\n')
fid.write('\*---------------------------------------------------------------------------*/\n')
fid.write('FoamFile\n')
fid.write('{\n')
fid.write('    version     2.0;\n')
fid.write('    format      ascii;\n')
fid.write('    class       ')
fid.write(dataType)
fid.write('AverageField;\n')
fid.write('    object      values;\n')
fid.write('}\n')
fid.write('// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //\n\n')
fid.write('// Average\n')
fid.write(average)
fid.write('\n\n\n')

fid.write(str(nDataNew))
fid.write('\n')
fid.write('(\n')
for i in range(nDataNew):
    fid.write(dataOld[indicesNew[i]])
    
fid.write(')')
fid.close()

# Print information to the screen.
print '\n'
print 'Number of data entries (original) = ', nDataOld
print 'Number of data entries (new) = ', nDataNew
