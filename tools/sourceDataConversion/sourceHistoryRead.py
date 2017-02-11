import sourceData as sd
#import matplotlib.pyplot as plt

  
  
  
  
  
  
# Specify the directory where the source history files reside.
inputDir = './SourceHistory/'
outputFile = './sources'


# Assemble the source information.
[heightMomentum,heightTemperature,
 timeMomentumX,timeMomentumY,timeMomentumZ,timeTemperature,
 sourceMomentumX,sourceMomentumY,sourceMomentumZ,sourceTemperature] = sd.assembleSourceHistory(inputDir)


# Write the source file for input to the solver.
sd.writeSourceForInput(outputFile,
                       heightMomentum,
                       heightTemperature,timeMomentumX,timeMomentumY,timeMomentumZ,timeTemperature,
                       sourceMomentumX,sourceMomentumY,sourceMomentumZ,sourceTemperature)
