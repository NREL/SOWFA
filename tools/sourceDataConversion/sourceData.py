# Function for processing SOWFA source term data.
#
#
# Matt Churchfield 
# National Renewable Energy Laboratory
# 17 Nov 2016










# Figure out how many time directories there are within a directory, and put
# them in numerical order.
def getOutputTimes(dir):
  # Import necessary modules
  import os
  
  
  data = os.listdir(dir)
  outputTimesI = []
  outputTimes = []
  nTimes = len(data)
  ii = 0
  for i in range(nTimes):
     if (unicode(data[i][0]).isnumeric()):
        outputTimesI.append(data[i])
        ii = ii + 1
      
  nTimes = len(outputTimesI)
  
  outputTimesIndex = 0
  outputTimesSort = []
  for i in range(nTimes):
     outputTimesSort.append([i,float(outputTimesI[i])])
     
  outputTimesSort = sorted(outputTimesSort, key=lambda index: index[1])
  
  for i in range(nTimes):
     outputTimes.append(outputTimesI[outputTimesSort[i][0]])
  
  
  return nTimes, outputTimes

  
  
  
  
  
  
  
  
  
# Assemble a complete source history.
def assembleSourceHistory(inputDir):
  # Import necessary modules
  import numpy as np
  
  
  # Get the number of time directories and their names.
  [nTimes, outputTimes] = getOutputTimes(inputDir)
  
  
  # Initialize the big arrays.
  timeMomentumX = []
  timeMomentumY = []
  timeMomentumZ = []
  timeTemperature = []
  sourceMomentumX = []
  sourceMomentumY = []
  sourceMomentumZ = []
  sourceTemperature = []


  # Loop through the time directories and get the source information.
  for n in range(nTimes):
     sourceName = ['SourceUXHistory','SourceUYHistory','SourceUZHistory','SourceTHistory']
     for m in range(4):
        inputFile = inputDir + '/' + outputTimes[n] + '/' + sourceName[m]

        if (m == 0):
           [heightMomentum, timeMomentumXI, sourceMomentumXI] = readSourceHistoryFile(inputFile)
         
           if (n == 0):
               timeMomentumX = timeMomentumXI
               sourceMomentumX = sourceMomentumXI
           else:
               startTime = timeMomentumXI[0]

               l = len(timeMomentumX)

               if (timeMomentumX[l-1] > startTime):
                   indEnd = (np.where(timeMomentumX >= startTime))[0][0] - 1

               else:
                   indEnd = l-1
               
               timeMomentumX = np.append(timeMomentumX[0:indEnd],timeMomentumXI)
               sourceMomentumX = np.append(sourceMomentumX[0:indEnd][:],sourceMomentumXI,axis=0)
             
               timeMomentumXI = []
               sourceMomentumXI = []
             
        elif (m == 1):
           [heightMomentum, timeMomentumYI, sourceMomentumYI] = readSourceHistoryFile(inputFile)

           if (n == 0):
               timeMomentumY = timeMomentumYI
               sourceMomentumY = sourceMomentumYI
           else:
               startTime = timeMomentumYI[0]

               l = len(timeMomentumY)

               if (timeMomentumY[l-1] > startTime):
                   indEnd = (np.where(timeMomentumY >= startTime))[0][0] - 1

               else:
                   indEnd = l-1

               timeMomentumY = np.append(timeMomentumY[0:indEnd],timeMomentumYI)
               sourceMomentumY = np.append(sourceMomentumY[0:indEnd][:],sourceMomentumYI,axis=0)

               timeMomentumYI = []
               sourceMomentumYI = []

        elif (m == 2):
           [heightMomentum, timeMomentumZI, sourceMomentumZI] = readSourceHistoryFile(inputFile)

           if (n == 0):
               timeMomentumZ = timeMomentumZI
               sourceMomentumZ = sourceMomentumZI
           else:
               startTime = timeMomentumZI[0]

               l = len(timeMomentumZ)

               if (timeMomentumZ[l-1] > startTime):
                   indEnd = (np.where(timeMomentumZ >= startTime))[0][0] - 1

               else:
                   indEnd = l-1

               timeMomentumZ = np.append(timeMomentumZ[0:indEnd],timeMomentumZI)
               sourceMomentumZ = np.append(sourceMomentumZ[0:indEnd][:],sourceMomentumZI,axis=0)

               timeMomentumZI = []
               sourceMomentumZI = []

        elif (m == 3):
           [heightTemperature, timeTemperatureI, sourceTemperatureI] = readSourceHistoryFile(inputFile)  

           if (n == 0):
               timeTemperature = timeTemperatureI
               sourceTemperature = sourceTemperatureI
           else:
               startTime = timeTemperatureI[0]

               l = len(timeTemperature)

               if (timeTemperature[l-1] > startTime):
                   indEnd = (np.where(timeTemperature >= startTime))[0][0] - 1

               else:
                   indEnd = l-1

               timeTemperature = np.append(timeTemperature[0:indEnd],timeTemperatureI)
               sourceTemperature = np.append(sourceTemperature[0:indEnd][:],sourceTemperatureI,axis=0)

               timeTemperatureI = []
               sourceTemperatureI = []
  


  return heightMomentum,heightTemperature,timeMomentumX,timeMomentumY,timeMomentumZ,timeTemperature,sourceMomentumX,sourceMomentumY,sourceMomentumZ,sourceTemperature
  
  
  
  
  
  

  
  
  
# Read a single source history file.
def readSourceHistoryFile(inputFile):
  import numpy as np
  
  # Open the file.
  fid = open(inputFile,'r')

  # Read the first line
  data = fid.readline()
  if (data[0] == 'H'):
     print 'Variable with Height'
     
     # Get the source height information.
     heights = data
     iStart = heights.find(')')
     iEnd = heights.find('\n')
     heights = heights[iStart+2:iEnd-1]
     heights = np.array([float(s) for s in heights.split(' ')])

     # Close the source file.
     fid.close()

     # Read the source data and organize it.
     data = np.loadtxt(inputFile,dtype='string',skiprows=2)
     time = np.transpose(np.array(data[:,0],dtype='float'))
     source = np.array(data[:,2:],dtype='float')

  elif (data[0] == 'T'):
     print 'Constant with Height'

     heights = np.zeros(1);

     # Close the source file.
     fid.close()

     # Read the source data and organize it.
     data = np.loadtxt(inputFile,dtype='string',skiprows=1)
     time = np.transpose(np.array(data[:,0],dtype='float'))
     source = np.array(data[:,2:],dtype='float')




  return heights, time, source
  
  
  
  
  
  
  
  
  

# Write out a source file that will become SOWFA input.
def writeSourceForInput(fileName,heightMomentum,heightTemperature,timeMomentumX,timeMomentumY,timeMomentumZ,timeTemperature,sourceMomentumX,sourceMomentumY,sourceMomentumZ,sourceTemperature):
  
  # Open the file.
  fid = open(fileName,'w')
  
  
  # Write the momentum source height list.
  fid.write('sourceHeightsMomentum\n')
  fid.write('(\n')
  for i in range(len(heightMomentum)):
     fid.write('    ' + str(heightMomentum[i]) + '\n')
     
  fid.write(');\n\n')
  
  # Write the x-momentum table
  fid.write('sourceTableMomentumX\n')
  fid.write('(\n')
  for n in range(len(timeMomentumX)):
      textStr = '    (' + str(timeMomentumX[n])
      for i in range(len(heightMomentum)):
          textStr = textStr + ' ' + str(sourceMomentumX[n][i])
          
      textStr = textStr + ')\n'
      fid.write(textStr)
              
  fid.write(');\n')
  
      
  # Write the y-momentum table
  fid.write('sourceTableMomentumY\n')
  fid.write('(\n')
  for n in range(len(timeMomentumY)):
      textStr = '    (' + str(timeMomentumY[n])
      for i in range(len(heightMomentum)):
          textStr = textStr + ' ' + str(sourceMomentumY[n][i])
          
      textStr = textStr + ')\n'
      fid.write(textStr)
              
  fid.write(');\n')
  
      
  # Write the z-momentum table
  fid.write('sourceTableMomentumZ\n')
  fid.write('(\n')
  for n in range(len(timeMomentumZ)):
      textStr = '    (' + str(timeMomentumZ[n])
      for i in range(len(heightMomentum)):
          textStr = textStr + ' ' + str(sourceMomentumZ[n][i])
          
      textStr = textStr + ')\n'
      fid.write(textStr)
              
  fid.write(');\n')
  
    
  # Write the temperature source height list.
  fid.write('sourceHeightsTemperature\n')
  fid.write('(\n')
  for i in range(len(heightTemperature)):
     fid.write('    ' + str(heightTemperature[i]) + '\n')
     
  fid.write(');\n\n')
  
      
  # Write the temperature table
  fid.write('sourceTableTemperature\n')
  fid.write('(\n')
  for n in range(len(timeTemperature)):
      textStr = '    (' + str(timeTemperature[n])
      for i in range(len(heightTemperature)):
          textStr = textStr + ' ' + str(sourceTemperature[n][i])
          
      textStr = textStr + ')\n'
      fid.write(textStr)
              
  fid.write(');\n')
  
      
  # Close the file.
  fid.close()
    
