#!/nopt/nrel/apps/python/2.7.6-gcc-4.8.2/bin/python

# This script reads in the points and faces files created by OpenFOAM's sample 
# patch function object and creates a points file needed by the 
# timeVaryingFixedMapped boundary condition
#
# Usage:  ./points.py [verticesFileName] [facesFileName] [newCentroidsFileName] [newIndicesFileName] [xMin xMax yMin yMax zMin zMax] [planeType(xy,xz,yz)]





# User input
import sys
print 'sys = ',sys.argv

vertexFile = sys.argv[1]
faceFile = sys.argv[2]
centroidFile = sys.argv[3]
indexFile = sys.argv[4]

xMin = float(sys.argv[5])
xMax = float(sys.argv[6])
yMin = float(sys.argv[7])
yMax = float(sys.argv[8])
zMin = float(sys.argv[9])
zMax = float(sys.argv[10])
planeType = sys.argv[11]



# Based on the plane type, set the sort order to follow OpenFoam standard
# boundary sorting order.
if planeType == 'xy':
#   sortOrder = [2,0,1]
    sortOrder = [0,1]
elif planeType == 'xz':
#   sortOrder = [1,0,2]
    sortOrder = [0,2]
elif planeType == 'yz':
#   sortOrder = [0,2,1]
    sortOrder = [2,1]
else:
    print 'Invalid specification of planeType:'
    print 'Need xy, xz, or yz.'




# Open the boundary faces vertices file.
fid = open(vertexFile,'r')

# Find the number of vertices.
nVertices = '\n'
while nVertices == '\n':
    nVertices = fid.readline()
    
nVertices = int(nVertices)

# Read in vertices.
xVertex = []
yVertex = []
zVertex = []
fid.readline()
for i in range(nVertices):
    data = fid.readline()
    data = data.lstrip('(')
    data = data.rstrip(')\n')
    data = data.split()
    xVertex.append(float(data[0]))
    yVertex.append(float(data[1]))
    zVertex.append(float(data[2]))

# Close the vertices file.
fid.close()





# Open the faces file.
fid = open(faceFile,'r')

# Find the number of faces.
nFaces = '\n'
while nFaces == '\n':
    nFaces = fid.readline()

nFaces = int(nFaces)

# Find face centroid locations
dataCentroid = []
fid.readline()
for i in range(nFaces):
    data = fid.readline()
    data = data[data.find('(')+1:]
    data = data.rstrip(')\n')
    data = data.split()
    xCentroid = 0.0
    yCentroid = 0.0
    zCentroid = 0.0
    iCentroid = i
    for j in range(len(data)):
        xCentroid = xCentroid + xVertex[int(data[j])]
        yCentroid = yCentroid + yVertex[int(data[j])]
        zCentroid = zCentroid + zVertex[int(data[j])]

    xCentroid = xCentroid/float(len(data))
    yCentroid = yCentroid/float(len(data))
    zCentroid = zCentroid/float(len(data))
    dataCentroid.append([xCentroid,yCentroid,zCentroid,iCentroid])

# Close the faces file.
fid.close()





# Now, get rid of locations outside of the range of interest.
j = 0
dataCentroidNew = []
for i in range(nFaces):
    if (dataCentroid[i][0] >= xMin and dataCentroid[i][0] <= xMax and 
        dataCentroid[i][1] >= yMin and dataCentroid[i][1] <= yMax and 
        dataCentroid[i][2] >= zMin and dataCentroid[i][2] <= zMax):
        dataCentroidNew.append(dataCentroid[i])
        j = j+1


nFaces = j
       
# Then, sort according to direction heirarchy.
#dataCentroidNew.sort(key=lambda k: (k[sortOrder[0]],
#                                    k[sortOrder[1]],
#                                    k[sortOrder[2]]))
dataCentroidNew.sort(key=lambda k: (k[sortOrder[0]],
                                    k[sortOrder[1]]))




# Write new points data to file
fid = open(centroidFile,'w')
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
fid.write('    class       vectorField;\n')
fid.write('    object      points;\n')
fid.write('}\n')
fid.write('// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //\n\n')
fid.write(str(nFaces))
fid.write('\n')
fid.write('(\n')
for i in range(nFaces):
    fid.write('({0} {1} {2})\n'.format(str(dataCentroidNew[i][0]),
	                               str(dataCentroidNew[i][1]),
				       str(dataCentroidNew[i][2])))

fid.write(')')
fid.close()




# Write index list for use in reordering flow variable files
fid = open(indexFile,'w')
fid.write(str(nFaces))
fid.write('\n')
fid.write('(\n')
for i in range(nFaces):
   fid.write('{0}\n'.format(str(dataCentroidNew[i][3])))

fid.write(')')
fid.close()



#Print out information to the screen.
print '\n'
print 'Number of vertices = ', nVertices
print 'Number of faces (original) = ', nFaces
print '\n'
print 'x_min = ',xMin
print 'x_max = ',xMax
print 'y_min = ',yMin
print 'y_max = ',yMax
print 'z_min = ',zMin
print 'z_max = ',zMax
print '\n'
print 'Sort Order = ',sortOrder
print '\n'
print 'Number of faces (new) = ', nFaces
