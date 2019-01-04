#!/usr/bin/env python
import sys
import numpy as np
import matplotlib.pyplot as plt
from netCDF4 import Dataset

if len(sys.argv) == 1:
    sys.exit('USAGE: '+sys.argv[0]+' wrf_soln [output_grid]')

wrfoutput = sys.argv[1]
if len(sys.argv) > 2:
    outputgrid = sys.argv[2]
else:
    outputgrid = 'output.xyz'

#
# load WRF soln
#
wrfdata = Dataset(wrfoutput, 'r')
lats = wrfdata.variables['XLAT'][:][0,:,:]
lons = wrfdata.variables['XLONG'][:][0,:,:]
z = wrfdata.variables['HGT'][:][0,:,:]

#
# output mesh to file
#
dims = z.shape
with open(outputgrid,'w') as f:
    f.write('# Dimensions: {:d} {:d}\n'.format(dims[0],dims[1]))
    f.write('#   latitude   longitude   elevation [m]\n')
    for (lat,long,elev) in zip(lats.ravel(order='F'),
                               lons.ravel(order='F'),
                                  z.ravel(order='F')):
        f.write(' {:g} {:g} {:g}\n'.format(lat,long,elev))
print 'wrote surface to',outputgrid

#
# plot elevation
#
fig, ax = plt.subplots()
levels = np.linspace(0, 3000, 31)
cont = ax.contourf(lons, lats, z, levels, cmap='viridis', extend='both')
fig.colorbar(cont, label='Height ASL [m]')
fig.suptitle('Elevation Map')

fig.savefig('elevation.png')
