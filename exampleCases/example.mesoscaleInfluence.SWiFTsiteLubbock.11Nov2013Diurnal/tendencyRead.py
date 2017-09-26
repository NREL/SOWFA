# Matt Churchfield
# National Rewable Energy Laboratory
# 8 October, 2016
#
# Cp                            Constant pressure specific heat of air.
# R                             Specific gas constant of air.
# nHrs                          Number of hours of WRF data to read in.
# nPerHr                        Number of outputs per hour of WRF data.
# nLevels                       Number of contour levels on contour plots.
# fileName                      Root file name for .nc input files.
# fieldVarName                  List of field variable names to read.
# multiplyByfc                  Flag to multiply variable by f_Coriolis or not.
# timeAvgWindowSize             Size of time sample window for time averaging.
# surfaceVarName                List of surface variables to read.




# Load important packages
import numpy as np
from netCDF4 import Dataset
import matplotlib.pyplot as plt


# User input
Cp = 1005.0
R = 286.9
p0 = 100000.0
kappa = 2.0/7.0
nHrs = 48
nPerHr = 6
nLevels = 41

fileName = '../tendency_ncfiles/SWIFT_all_w0_L0.nc'
#fileName = '../tendency_ncfiles/SWIFT_all_w0_L19000.nc'
#fileName = '../tendency_ncfiles/SWIFT_all_w60_L0.nc'
#fileName = '../tendency_ncfiles/SWIFT_all_w60_L19000.nc'
#fileName = '../tendency_ncfiles/SWIFT_all_neutral_FNL_notdivbyfc_tend_L4000'
#fileName = '.\TendencyForcing/tendency_ncfiles\SWIFT_all_neutral_FNL_notdivbyfc_1gp_tend_L0'

fieldVarName = ['U','V','Th','Uadv','Vadv','Thadv','Ug','Vg','Utend','Vtend','Ucor','Vcor','Uphys','Vphys']
multiplyByfc = [0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
timeAvgWindowSize = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1];

surfaceVarName = ['HFX','LH','TSK','T2','Psfc']

writeFields = True
writeForcings = True
writeSurfaceValues = True
writeProbes = True
probeX = [2500.0, 2500.0, 2500.0, 2500.0, 2500.0]
probeY = [ 500.0, 1500.0, 2500.0, 3500.0, 4500.0]
probeMinZ = 5.0
probeMaxZ = 1995.0
probedZ = 10.0
writeInitialValues = True
initialTime = 12.0

plotFields = False
plotSurface = True



# Declare some variables
U = []
V = []
Th = []
Uadv = []
Vadv = []
Thadv = []
Ug = []
Vg = []
Utend = []
Vtend = []
Ucor = []
Vcor = []
Uphys = []
Vphys = []
Qs = []
Ts = []
T2m = []



nc_fid = Dataset(fileName,'r')
z = nc_fid.variables['z'][:]
fc = nc_fid.variables['fc'][:]
time = (nc_fid.variables['time'][:])*24.0
t0 = time[0]
t = time - t0
nt = len(t)
nz = len(z)


# Loop through the field variables.
for m in range(len(fieldVarName)):

    
   var = np.zeros([nz,nt])
   ncVar = nc_fid.variables[fieldVarName[m]][:]
   var = np.transpose(ncVar)
   

   # If the variable needs to be multiplied by f_Coriolis, do that now.
   if (multiplyByfc[m] == 1.0):
      var = var * fc

     
   # Apply time averaging to the variables.   
   varAvg = np.zeros([nz,nt])     
   for i in range(nt):
      iMin = max(0, i - (timeAvgWindowSize[m]-1)/2)
      iMax = min(nt,i + (timeAvgWindowSize[m]-1)/2)
      varAvg[:,i] = var[:,iMin:iMax+1].mean(axis=1)
      
   if (fieldVarName[m] == 'U'):
       U = varAvg;
   elif (fieldVarName[m] == 'V'):
       V = varAvg;
   elif (fieldVarName[m] == 'Th'):
       Th = varAvg;
   elif (fieldVarName[m] == 'Uadv'):
       Uadv = varAvg;
   elif (fieldVarName[m] == 'Vadv'):
       Vadv = varAvg;
   elif (fieldVarName[m] == 'Thadv'):
       Thadv = varAvg;
   elif (fieldVarName[m] == 'Ug'):
       Ug = varAvg;
   elif (fieldVarName[m] == 'Vg'):
       Vg = varAvg;
   elif (fieldVarName[m] == 'Utend'):
       Utend = varAvg;
   elif (fieldVarName[m] == 'Vtend'):
       Vtend = varAvg;
   elif (fieldVarName[m] == 'Ucor'):
       Ucor = varAvg;
   elif (fieldVarName[m] == 'Vcor'):
       Vcor = varAvg;
   elif (fieldVarName[m] == 'Uphys'):
       Uphys = varAvg;
   elif (fieldVarName[m] == 'Vphys'):
       Vphys = varAvg;


   # Plot the field variable time-height history.
   if (plotFields):
      fig = plt.figure(m+1)
      cs = plt.contourf(t,z,varAvg,nLevels,cmap=plt.cm.Spectral_r)
      cxl = plt.xlabel('t (hr)')
      cyl = plt.ylabel('z (m)')
      ax = plt.gca()
      ax.set_ylim([0,2000])
      cbar = plt.colorbar(cs, orientation='vertical')
      cbar.set_label("%s (%s)" % (fieldVarName[m],nc_fid.variables[fieldVarName[m]].units))
      plt.show()
      


hInd = 100
U_h = U[hInd,:]
V_h = V[hInd,:]
U_derived = np.zeros(U_h.shape)
V_derived = np.zeros(V_h.shape) 
U_derived_tend = np.zeros(U_h.shape)
V_derived_tend = np.zeros(V_h.shape)
for i in range(nt):
    if (i == 0):
        U_derived[i] = U_h[i]
        V_derived[i] = V_h[i]
        U_derived_tend[i] = U_h[i]
        V_derived_tend[i] = V_h[i]
    else:
        dt = (t[i] - t[i-1])*3600.0
        U_derived[i] = U_derived[i-1] + dt*(Uadv[hInd,i]-Vg[hInd,i]+Ucor[hInd,i]+Uphys[hInd,i])
        V_derived[i] = V_derived[i-1] + dt*(Vadv[hInd,i]+Ug[hInd,i]+Vcor[hInd,i]+Vphys[hInd,i])
        U_derived_tend[i] = U_derived_tend[i-1] + dt*(Utend[hInd,i])
        V_derived_tend[i] = V_derived_tend[i-1] + dt*(Vtend[hInd,i])
        

fig = plt.figure(98)
plt.plot(t,U_h,'b-')
plt.plot(t,U_derived,'b--')
plt.plot(t,U_derived_tend,'b:')

fig = plt.figure(99)
plt.plot(t,V_h,'r-')
plt.plot(t,V_derived,'r--')
plt.plot(t,V_derived_tend,'r:')


     

# Write out the field files
if (writeFields):
   fid = open('fieldTable','w')
     
   # Write the height list for the momentum fields
   fid.write('sourceHeightsMomentum\n')    
   fid.write('(\n')
   for j in range(nz):
      fid.write('    ' + str(z[j]) + '\n')
   fid.write(');\n\n')
         
   # Write the x-velocity
   fid.write('sourceTableMomentumX\n')    
   fid.write('(\n')
   for n in range(nt):
      fid.write('    (' + str(t[n]*3600) + ' ')     
      for j in range(nz):
         fid.write(str(U[j,n]) + ' ')
      fid.write(')\n')
   fid.write(');\n\n')
         
   # Write the y-velocity
   fid.write('sourceTableMomentumY\n')    
   fid.write('(\n')
   for n in range(nt):
      fid.write('    (' + str(t[n]*3600) + ' ')     
      for j in range(nz):
         fid.write(str(V[j,n]) + ' ')
      fid.write(')\n')
   fid.write(');\n\n')
         
   # Write the z-velocity (hard coded to 0.0 zero here)
   fid.write('sourceTableMomentumZ\n')    
   fid.write('(\n')
   for n in range(nt):
      fid.write('    (' + str(t[n]*3600) + ' ')     
      for j in range(nz):
         fid.write(str(0.0) + ' ')
      fid.write(')\n')
   fid.write(');\n\n')
     
   # Write the height list for the temperature fields
   fid.write('sourceHeightsTemperature\n')    
   fid.write('(\n')
   for j in range(nz):
      fid.write('    ' + str(z[j]) + '\n')
   fid.write(');\n\n')
         
   # Write the temperature
   fid.write('sourceTableTemperature\n')    
   fid.write('(\n')
   for n in range(nt):
      fid.write('    (' + str(t[n]*3600) + ' ')     
      for j in range(nz):
         fid.write(str(Th[j,n]) + ' ')
      fid.write(')\n')
   fid.write(');\n\n')
    
   fid.close()
   
   
   
if (writeForcings):
   fid = open('forcingTable','w')
     
   # Write the height list for the momentum forcings
   fid.write('sourceHeightsMomentum\n')    
   fid.write('(\n')
   for j in range(nz):
      fid.write('    ' + str(z[j]) + '\n')
   fid.write(');\n\n')
         
   # Write the x-momentum forcing
   fid.write('sourceTableMomentumX\n')    
   fid.write('(\n')
   for n in range(nt):
      fid.write('    (' + str(t[n]*3600) + ' ')     
      for j in range(nz):
         Uforcing = Uadv[j,n] - Vg[j,n] 
         fid.write(str(Uforcing) + ' ')
      fid.write(')\n')
   fid.write(');\n\n')
         
   # Write the y-momentum forcing
   fid.write('sourceTableMomentumY\n')    
   fid.write('(\n')
   for n in range(nt):
      fid.write('    (' + str(t[n]*3600) + ' ')     
      for j in range(nz):
         Vforcing = Vadv[j,n] + Ug[j,n]
         fid.write(str(Vforcing) + ' ')
      fid.write(')\n')
   fid.write(');\n\n')
         
   # Write the z-momentum forcing (hard coded to 0.0 zero here)
   fid.write('sourceTableMomentumZ\n')    
   fid.write('(\n')
   for n in range(nt):
      fid.write('    (' + str(t[n]*3600) + ' ')     
      for j in range(nz):
         fid.write(str(0.0) + ' ')
      fid.write(')\n')
   fid.write(');\n\n')
     
   # Write the height list for the temperature forcing
   fid.write('sourceHeightsTemperature\n')    
   fid.write('(\n')
   for j in range(nz):
      fid.write('    ' + str(z[j]) + '\n')
   fid.write(');\n\n')
         
   # Write the temperature forcing
   fid.write('sourceTableTemperature\n')    
   fid.write('(\n')
   for n in range(nt):
      fid.write('    (' + str(t[n]*3600) + ' ')     
      for j in range(nz):
         fid.write(str(Thadv[j,n]) + ' ')
      fid.write(')\n')
   fid.write(');\n\n')
    
   fid.close()
   
   

if (writeInitialValues):
   ind = (np.abs(t-initialTime)).argmin()
   fid = open('initialValues','w')   
   for j in range(nz):
      fid.write('    (' + str(z[j]) + ' ' + str(U[j,ind]) + ' ' + str(V[j,ind]) + ' ' + str(Th[j,ind]) + ')\n')
   fid.close()
   
   
   
if (writeProbes):
   tol = 0.001
   probeZ = np.linspace(probeMinZ, probeMaxZ, (probeMaxZ-probeMinZ)/probedZ + 1)
   for i in range(len(probeX)):
      probeFileName = 'probe' + str(i+1)
      fid = open(probeFileName,'w')
      for j in range(len(probeZ)):
         fid.write('                   (' + str(probeX[i]+tol) + ' ' + str(probeY[i]+tol) + ' ' + str(probeZ[j]+tol) +')\n')
      fid.close()
   
   
   
# Now loop through the surface variables.
pS = np.zeros([nt])
TS = np.zeros([nt])
T2m = np.zeros([nt])
qS = np.zeros([nt])

for m in range(len(surfaceVarName)):
    
   # Read in the data file for each hour. 
   var = np.zeros([nz,nt])
   nc_fid = Dataset(fileName, 'r') 
   ncVar = nc_fid.variables[surfaceVarName[m]][:]
   var = np.transpose(ncVar)
      
   if (m == 0):
      pI = nc_fid.variables['Psfc'][:]
      TI = nc_fid.variables['TSK'][:]
      T2I = nc_fid.variables['T2'][:]
      pS = np.transpose(pI)
      TS = np.transpose(TI)
      T2m = np.transpose(T2I)

     
      
   # Plot the variable's time history.
   if (plotSurface):
      fig = plt.figure()
      cs = plt.plot(t,var,'r-')
      cxl = plt.xlabel('t (hr)')
      cyl = plt.ylabel("%s (%s)" % (surfaceVarName[m],nc_fid.variables[surfaceVarName[m]].units))
   
   # Treat surface heat flux specially--compute surface temperature flux.  Here
   # Qs = hfx/(Cp*rho_s), where rho_s = p_s/(R*T_2m).  I used the 2m temperature
   # instead of the skin temperature.
   if (surfaceVarName[m] == 'HFX'):
       qS = np.zeros([nt])
       rhoS = np.zeros([nt])
       for i in range(nt):
           rhoS[i] = pS[i]/(R * T2m[i])
           qS[i] = var[i]/(Cp * rhoS[i])
    
       if (plotSurface):
          fig = plt.figure()
          cs = plt.plot(t,qS,'r-')
          cxl = plt.xlabel('t (hr)')
          cyl = plt.ylabel('Qs (K m/s)')
          



# T_S and T_2m are real temperature, but we also want these in potential temperature.
theta2m = np.zeros([nt])
thetaS = np.zeros([nt])
for i in range(nt):
   thetaS[i] = TS[i]*((p0/pS[i])**kappa)
   theta2m[i] = T2m[i]*((p0/pS[i])**kappa)

    
if (plotSurface):
   fig = plt.figure()
   cs = plt.plot(t,thetaS,'r-')
   cxl = plt.xlabel('t (hr)')
   cyl = plt.ylabel('theta_s (K)')

   fig = plt.figure()
   cs = plt.plot(t,theta2m,'r-')
   cxl = plt.xlabel('t (hr)')
   cyl = plt.ylabel('theta_2m (K)')


          
          
if (writeSurfaceValues):   
   # Skin real temperature       
   fid = open('surfaceSkinTemperatureTable','w')
   for n in range(nt):
      fid.write('             (' + str(t[n]*3600) + ' ' + str(TS[n]) + ')\n')   
   fid.close()
   
   # 2-m real temperature
   fid = open('surface2mTemperatureTable','w')
   for n in range(nt):
      fid.write('             (' + str(t[n]*3600) + ' ' + str(T2m[n]) + ')\n')   
   fid.close()  
   
   # Skin potential temperature       
   fid = open('surfaceSkinPotentialTemperatureTable','w')
   for n in range(nt):
      fid.write('             (' + str(t[n]*3600) + ' ' + str(thetaS[n]) + ')\n')   
   fid.close()
   
   # 2-m potential temperature
   fid = open('surface2mPotentialTemperatureTable','w')
   for n in range(nt):
      fid.write('             (' + str(t[n]*3600) + ' ' + str(theta2m[n]) + ')\n')   
   fid.close()
   
   # Surface temperature flux
   fid = open('surfaceTemperatureFluxTable','w')
   for n in range(nt):
      fid.write('             (' + str(t[n]*3600) + ' ' + str(-qS[n]) + ')\n')   
   fid.close()
          
          
          
       
       




def ncdump(nc_fid, verb=True):
    '''
    ncdump outputs dimensions, variables and their attribute information.
    The information is similar to that of NCAR's ncdump utility.
    ncdump requires a valid instance of Dataset.

    Parameters
    ----------
    nc_fid : netCDF4.Dataset
        A netCDF4 dateset object
    verb : Boolean
        whether or not nc_attrs, nc_dims, and nc_vars are printed

    Returns
    -------
    nc_attrs : list
        A Python list of the NetCDF file global attributes
    nc_dims : list
        A Python list of the NetCDF file dimensions
    nc_vars : list
        A Python list of the NetCDF file variables
    '''
    def print_ncattr(key):
        """
        Prints the NetCDF file attributes for a given key

        Parameters
        ----------
        key : unicode
            a valid netCDF4.Dataset.variables key
        """
        try:
            print "\t\ttype:", repr(nc_fid.variables[key].dtype)
            for ncattr in nc_fid.variables[key].ncattrs():
                print '\t\t%s:' % ncattr,\
                      repr(nc_fid.variables[key].getncattr(ncattr))
        except KeyError:
            print "\t\tWARNING: %s does not contain variable attributes" % key

    # NetCDF global attributes
    nc_attrs = nc_fid.ncattrs()
    if verb:
        print "NetCDF Global Attributes:"
        for nc_attr in nc_attrs:
            print '\t%s:' % nc_attr, repr(nc_fid.getncattr(nc_attr))
    nc_dims = [dim for dim in nc_fid.dimensions]  # list of nc dimensions
    # Dimension shape information.
    if verb:
        print "NetCDF dimension information:"
        for dim in nc_dims:
            print "\tName:", dim 
            print "\t\tsize:", len(nc_fid.dimensions[dim])
            print_ncattr(dim)
    # Variable information.
    nc_vars = [var for var in nc_fid.variables]  # list of nc variables
    if verb:
        print "NetCDF variable information:"
        for var in nc_vars:
            if var not in nc_dims:
                print '\tName:', var
                print "\t\tdimensions:", nc_fid.variables[var].dimensions
                print "\t\tsize:", nc_fid.variables[var].size
                print_ncattr(var)
    return nc_attrs, nc_dims, nc_vars