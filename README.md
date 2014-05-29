SOWFA
=====

All of these files compose the current version of the SOWFA (Simulator for 
Offshore Wind Farm Applications), created at the National Renewable Energy 
Laboratory (NREL).  The files are based on the OpenFOAM software and are 
either new files or modifications of files in the OpenFOAM source code 
distribution. Please see the included OpenFOAM readme file 
("README.OpenFOAM") and the GPL licence information ("COPYING"). Access
to and use of SOWFA imposes obligations on the user, as set forth in the 
NWTC Design Codes DATA USE DISCLAIMER AGREEMENT that can be found at
<http://wind.nrel.gov/designcodes/disclaimer.html>.


## Solvers and Codes Included

### Solvers
  * `ABLPisoSolver` - A large-eddy simulation (LES) solver for computing
    atmospheric boundary layer turbulent flow with the ability to
    specify surface roughness, stability, and wind speed and direction.
    It must be used with hexahedral meshes (like those created by
    OpenFOAM's `blockMesh` utility).
  * `windPlantPisoSolver` - A specialized version of `ABLPisoSolver` for
    performing LES of wind plant flow.  It includes the ability to
    include actuator line turbine models with local grid refinement
    around the turbine.
  * `windPlantPisoSolverFAST` - Like `windPlantPisoSolver`, but the actuator
    line turbine model is coupled to NREL's FAST aeroelastic and turbine
    system dynamics code.
    
NOTE:  These solvers are meant to be used for flow over flat terrain.
We hope to add the capability to handle non-flat terrain in the near
future.  Also, the way the sub-grid stresses are treated is different
than the standard OpenFOAM practice.  Therefore, the standard OpenFOAM
turbulence models are unavailable in these solvers--only the standard
Smagorinsky model is available.  We wish to add more capability in the
future.  It is important to remember, though, that you may couple the
actuator line models with any standard OpenFOAM solver, such as `pisoFoam`.

### Utilities
  * `setFieldsABL` - A utility to initialize the flow field for performing
    atmospheric boundary layer LES.  With the utility, you can specify
    an initial mean profile and perturbations that accelerate the
    development of turbulence will be superimposed.  You may also 
    specify the initial temperature profile and location and strength
    of the capping inversion.

### Libraries
  * `finiteVolume` - Contains a custom boundary condition for pressure
    called `buoyantPressureMod` that is compatible with the above solvers 
    but derived from OpenFOAM's standard `bouyantPressure` boundary
    condition.
  * `turbineModels` - Contains the actuator line turbine model similar
    to that outlined by Sorensen and Shen (2002).
  * `openfast` - A version of NREL's FAST code (see 
    http://wind.nrel.gov/designcodes/simulators/fast/) meant for
    coupling with the `windPlantPisoSolverFAST` solver.
  * `fastturb` - A version of the actuator line turbine model that
    is coupled with NREL's FAST aeroelastic and turbine system
    dynamics code.


## Compiling/Installation
The included codes work only with the OpenFOAM CFD Toolbox.  OpenFOAM has
not been distributed with the SOWFA package.  Please visit www.openfoam.com
to download and install OpenFOAM.  This release of SOWFA is known to work
with OpenFOAM-2.0.x.  It will probably need to be modified to run with
earlier versions, but may run as is with OpenFOAM-2.1.x.

Once OpenFOAM is installed, please follow these steps:

1.  Move the `user-2.0.x` to a desired location somewhere in your home
    directory and replace "user" with your username on your system.
2.  When you installed OpenFOAM, you modified your login script
    (`.bash_profile` or similar).  Make sure that the environment variable
    `WM_PROJECT_USER_DIR` points to where you put the renamed `user-2.0.x`
    directory.
3.  Source your login script by entering `source ~/.bash_profile` or 
    whatever is appropriate for your system.
3.  Move the `tutorialsSOWFA` directory to any desired location.
4.  Change directory to `WM_PROJECT_USER_DIR` by entering
    `cd $WM_PROJECT_USER_DIR`.
5.  Run the `Allwclean` script to remove any dependencies by entering
    `./Allwclean`.
6.  Compile the SOWFA codes by running the Allwmake script by entering
    `./Allwmake`.
7.  Make sure that no error messages appeared and that all libraries
    and applications are listed as "up to date."


## Running Tutorials
Tutorial example cases are provide for each solver. The tutorials are
as follows

  * `precursorABL` - An example case of how to perform an atmospheric
    boundary layer large-eddy simulation (without turbines).  This
    will generate turbulent fields that can be used in wind plant
    simulations.

  * `windPlant` - An example of how to use `windPlantPisoSolver` with
    a farm of NREL 5MW turbines.

  * `windPlantFAST` - Like the windPlant example but for use of the
    FAST-coupled `windPlantPisoSolverFAST`.

To run a tutorial, change to that tutorial directory and run the
`Allrun` script contained in the directory by entering `./Allrun`.  View
the `Allrun` script to understand the basic use of the code. To return
to the original state, run the Allclean script by entering `./Allclean`.

These are very basic tutorials meant to familiarize the user with the
general file structure of a case and the various input files.  They
are meant to run on a small amount of processors, but will not
generate very meaningful results as the grid resolution is extremely
coarse.  The turbine models use a Gaussian projection (see the
`epsilon` variable in the `constant/turbineArrayProperties` file) that
should be set to at least twice the local grid cell width.  For these
examples, `epsilon` is set to a realistic value for performing true
LES, but is much less than twice the local grid cell width of these
very coarse grids.  The `precursorABL` tutorial uses a periodic mesh,
which is what we do in running a "real" precursor simulation. The
wind plant tutorials also use a periodic mesh, but in actuality, we
feed data saved from the precursor into the wind plant domain and
have outflow boundaries elsewhere.  We did not show this in these
tutorials since the `precursorABL` simulation is too coarse to create
meaningful inflow data for the `windPlant` simulations.  We leave it to
the user to experiment with this.

