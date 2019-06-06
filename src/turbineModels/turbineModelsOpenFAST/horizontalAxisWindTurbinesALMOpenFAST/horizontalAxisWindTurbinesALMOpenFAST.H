/*---------------------------------------------------------------------------*\
This file was modified or created at the National Renewable Energy
Laboratory (NREL) on January 6, 2012 in creating the SOWFA (Simulator for
Offshore Wind Farm Applications) package of wind plant modeling tools that
are based on the OpenFOAM software. Access to and use of SOWFA imposes
obligations on the user, as set forth in the NWTC Design Codes DATA USE
DISCLAIMER AGREEMENT that can be found at
<http://wind.nrel.gov/designcodes/disclaimer.html>.
\*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011 OpenFOAM Foundation
     \\/     M anipulation  |
-------------------------------------------------------------------------------
License
    This file is part of OpenFOAM.

    OpenFOAM is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    OpenFOAM is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with OpenFOAM.  If not, see <http://www.gnu.org/licenses/>.

Namespace
    None

Class
    horizontalAxisWindTurbinesALMOpenFAST

Description
    Version of the older actuator line model that includes acutator tower and
    nacelle.  This will eventually become the standard version, but is relatively
    untested, so this is an ALPHA version.

    This is the horizontal-axis wind turbine array actuator line model class.
    It will set up an array of various kinds of turbines (currently blades 
    only) within a flow field.  The blade rotation rate is set or calculated
    based on a simple torque control model (not implemented yet), the blades
    are rotated at each time step, the turbine is yawed (not implemented yet),
    the blade forces are calculated, the actuator line body force information
    is passed back to the flow solver, and turbine information is written to
    files.

SourceFiles
    horizontalAxisWindTurbinesALMOpenFAST.C

\*---------------------------------------------------------------------------*/

#ifndef horizontalAxisWindTurbinesALMOpenFAST_H
#define horizontalAxisWindTurbinesALMOpenFAST_H

#include "HashPtrTable.H"
#include "IOdictionary.H"
#include "IFstream.H"
#include "OFstream.H"
#include "fvCFD.H"
#include "Random.H"
#include "scalarMatrices.H"
#include "OpenFAST.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace turbineModels
{

/*---------------------------------------------------------------------------*\
           Class horizontalAxisWindTurbinesALMOpenFAST declaration
\*---------------------------------------------------------------------------*/

class horizontalAxisWindTurbinesALMOpenFAST
{

private:
    // Private Data
        //- Runtime pointer.
        const Time& runTime_;

        //- Mesh pointer.
        const fvMesh& mesh_;

        //- Velocity field pointer.
        const volVectorField& U_;

        //- Degrees to radians conversion factor.
        const scalar degRad;

        //- Revolutions per second to radians per second conversion factor.
        const scalar rpsRadSec;

        //- Revolutions per minute to radians per second conversion factor.
        const scalar rpmRadSec;

        //- The processor number.
        const int p;

        //- Current time step size.
        scalar dt;
 
        //- Current simulation time.
        word time;
        scalar t;

        //- Simulation start and end time.
        scalar tStart;
        scalar tEnd;

        //- Boolean that is true past time step.
        bool pastFirstTimeStep;

        //- Velocity gradient used to linearly interpolate the velocity from the
        //  CFD gridto the actuator line.
        volTensorField gradU;

        //- Body force field applied to fluid by turbine.
        volVectorField bodyForce;
        volScalarField gBlade;

        //- Mark the cells that will be searched with integers.  0 for not a search cell, 
        //  1 for rotor cells, 2 for nacelle cells, 3 for tower cells.
        volScalarField searchCells;

        //- It sometimes is useful to have the velocity field relative to the rotor, rather
        //  than in the fixed frame.  This is the superposition of the actual flow speed
        //  and the rotor speed as a function of radius.  The radius is measured normal from
        //  the main shaft axis.
        volScalarField rFromShaft;
        volVectorField Urel;

        //- Declare an instance of the class that interfaces with FAST.
        fast::fastInputs fi ;
        autoPtr<fast::OpenFAST> FAST;

        //- Write every "outputInterval" time steps or seconds.  Options are
        //  "timeStep" or "runTime".  "runTime" writes out as closely to every
        //  "outputInterval" seconds as possible, but doesn't adjust the time
        //  step to write on the exact interval. 
        word outputControl;

        //- The inteveral over which to write out turbine data.
        scalar outputInterval;

        //- Last time when output written.
        scalar lastOutputTime;

        //- Last time step when output written.
        label outputIndex;

        //- Is this a FAST dry run?
        bool dryRun;

        //- Is this a FAST restart?
        bool restart;

        //- Is this case using the supercontroller?
        bool superControllerOn;

        //- Should this case have OpenFAST write out debug information for velocity/force along actuator node points?
        bool FASTwriteDebugInfo;

        //- The name of the supercontroller library.
        word superControllerLib;
 
        //- Number of supercontroller inputs/outputs.
        int numSuperControllerInputs;
        int numSuperControllerOutputs;

        //- Time of the beginning and end of the overall turbine simulation (not this sub-section of
        //  the simulation if having to restart.
        scalar timeSimulationStart;
        scalar timeSimulationEnd;

        //- Frequency in time steps to write a FAST restart file.
        int checkPointInterval;
 
        //- Value to perturb blade locations by to break ties when searching for control processor.
        scalar perturb;






        //- Turbine Array Level Data (all variables start with lower case letter).
            //- List of names of turbines in array.
            List<word> turbineName;

            //- Number of turbines in array.
            int numTurbines;

            //- List of names of turbine types in array.
            DynamicList<word> turbineType;

            //- FAST input file name (.fst).
            DynamicList<word> FASTInputFileName;

            //- FAST restart file name (.chkpt).
            DynamicList<word> FASTRestartFileName;

            //- Number of FAST time steps to take per one OpenFOAM time step.
            DynamicList<int> nFASTSubSteps;

            //- List of booleans for including nacelle and a global boolean that is
            //  true if at least one turbine nacelle is included.
            DynamicList<bool> includeNacelle;
            bool includeNacelleSomeTrue;

            //- List of booleans for including tower and a global boolean that is
            //  true if at least one turbine tower is included.
            DynamicList<bool> includeTower;
            bool includeTowerSomeTrue;
 
            //- List of locations of bases of turbines in array relative to origin (m).
            DynamicList<vector> baseLocation;

            //- List of number of actuator points on blades, tower, and nacelles
            //  of turbines in array. 
            DynamicList<int> numBladePoints; 
            DynamicList<int> numTowerPoints;
            DynamicList<int> numNacellePoints;

            //- List of number of blade and tower points in FAST.
            DynamicList<int> numBladeSamplePoints;
            DynamicList<int> numTowerSamplePoints;

            //- List of description of actuator line point distribution types
            //  for each turbine (set to uniform for now--here for future upgrades).
            DynamicList<word> bladePointDistType;
            DynamicList<word> towerPointDistType;
            DynamicList<word> nacellePointDistType;

            //- List of description of how velocity field is interpolated from the 
            //  CFD mesh to the actuator line points.  Options are "cellCenter", "linear",
            //  or "integral".  "cellCenter" uses the value at the cell center of the cell
            //  within which the actuator point lies.  "linear" uses linear
            //  interpolation from the cell within which the actuator point lies and
            //  from neighboring cells. "integral" computes the actuator point velocity as
            //  the integral of the local velocity and the force distribution function following
            //  Spalart's formulation.
            DynamicList<word> bladeActuatorPointInterpType;
            DynamicList<word> nacelleActuatorPointInterpType;
            DynamicList<word> towerActuatorPointInterpType;

            //- List of the method to define blade related search cells.
            DynamicList<word> bladeSearchCellMethod;

            //- List of how the blades are updated in time.  "oldPosition" means that for
            //  computing the body force due to the blades for t^(n+1), the velocity is
            //  sampled from the location of the blades at time t^n.  The blades are advanced
            //  to their t^(n+1) position.  Then the blade force is computed at the updated  
            //  blade location and projected from there onto the flow field.  "newPosition"
            //  means that the blades are first advanced to thier t^(n+1) location, and then
            //  velocity is sampled there, blade forces are computed there, and body forces
            //  are projected.   
            DynamicList<word> actuatorUpdateType;

            //- The body force projection type to be used. Currently, the options are:
            //  "uniformGaussian" - Projects force at a point uniformly in all three directions.
            //                      Enter the spreading value epsilon as the first entry into
            //                      the epsilon vector, the remaining two entries will be disregarded.
            //  "diskGaussian"    - Projects force at a point constantly over a disk of some radius,
            //                      but dies off with a Gaussian at the end of the disk.  It also dies
            //                      off as a Gaussian in the direction normal to the disk.  Useful for
            //                      the tower and nacelle.  For the tower, the disk normal is along
            //                      the tower direction and the disk takes the radius of the tower.
            //                      For the nacelle, the disk normal is along the drive shaft direction
            //                      and the radius is the effective radius (based on frontal area) of
            //                      the nacelle. Epsilon for the radial die off of force is the first
            //                      entry into the epsilon vector and that of the axial die off is
            //                      the second entry (see below).
            DynamicList<word> bladeForceProjectionType;
            DynamicList<word> nacelleForceProjectionType;
            DynamicList<word> towerForceProjectionType;

            //- There are two options for how the direction of lift and drag of the blades is set.
            //  In the "sampleVelocityAligned" option, the lift component of the body force is everywhere
            //  perpendicular to the sampled velocity vector and the blade span direction and the drag 
            //  is parallel to the sampled velocity vector.  In the "localVelocityAligned" option, the 
            //  lift component of the body force is everywhere perpendicular to the local velocity and the
            //  spanwise direction, and the drage is pareallel to the local velocity vector.  The second
            //  option follows Spalart's formulation.
            DynamicList<word> bladeForceProjectionDirection;

            //- List of body force normalization parameter for each turbine (m). This controls
            //  the width of the Gaussian projection.  It should be tied to grid width.
            //  A value below 1 times the local grid cell length will yield inaccurate
            //  projection of the forces to the grid (i.e., if you integrate the projected
            //  force, it will be significantly smaller than the force that was projected
            //  in the first place.
            DynamicList<vector> bladeEpsilon;
            DynamicList<vector> nacelleEpsilon;
            DynamicList<vector> towerEpsilon;

            //- The velocity used to calculate tower and nacelle forces must be sampled
            //  somewhere upstream of the actuator points.  These variables describe how
            //  far upstream the sampling occurs.
            DynamicList<scalar> nacelleSampleDistance;
            DynamicList<scalar> towerSampleDistance;

            //- The projection of actuator forces to body forces uses a Gaussian (or some
            //  other smooth normalization could be used).  At some distance away from the
            //  actuator point from which the force is being projected, this normalization
            //  dies to 0.1% of its peak value.  Beyond that distance, stop doing the projection
            //  to save computational effort.  This variable is that distance in (m).  It
            //  is based on epsilon values.  The larger epsilon, the wider the projection.
            DynamicList<scalar> bladeProjectionRadius;
            DynamicList<scalar> nacelleProjectionRadius;
            DynamicList<scalar> towerProjectionRadius;

            //- List of tip/root loss correction type for each turbine.  "none" applies
            //  no correction.  "Glauert" applies the Glauert tip loss correction.
            DynamicList<word> tipRootLossCorrType;

            //- List of correction type for the effect of drag on sampled velocity.  "none"
            //  applies no corretion.  "Martinez" applies the correction of Tony Martinez
            //  and Charles Meneveau.
            DynamicList<word> velocityDragCorrType;
        
            //- Rotor rotation direction as viewed from upwind.  Options are
            //  "cw" for clockwise and "ccw" for counter-clockwise.
            DynamicList<word> rotationDir;

            //- Initial or fixed rotor speed (rpm).  A positive value means
            //  clockwise rotation for a clockwise rotating turbine (see rotationDir
            //  above) or counter-clockwise rotation for a counter-clockwise
            //  rotating turbine.
            DynamicList<scalar> rotorSpeed;

            //- Filtered rotor speed that is fed to the controllers (rpm).
            DynamicList<scalar> rotorSpeedF;

            //- Speed error between reference low speed shaft and filtered low speed
            //  shaft speed for use in blade pitch PID control (rad/s).
            DynamicList<scalar> speedError;

            //- Integrated speed error used in blade pitch PID control (rad/s).
            DynamicList<scalar> intSpeedError;

            //- Initial blade 1 rotorAzimuth angle (degrees) (looking from upwind to 
            //  downwind, a positive rotorAzimuth angle makes a clockwise movement if
            //  this is a clockwise rotating turbine (see rotationDir above) or
            //  or a counterclockwise movement if this is a counter-clockwise
            //  rotating turbine).
            DynamicList<scalar> rotorAzimuth;

            //- Initial generator torque on turbine (not density normalized).
            DynamicList<scalar> generatorTorque;

            //- Initial blade pitch (degrees) of all blades.
            DynamicList<scalar> bladePitch;

            //- Initial or fixed nacelle yaw angle.  Direction that the turbine
            //  is pointed in cardinal directions (i.e. 0 = north, 90 = east, 
            //  180 = south, 270 = west) (degrees).  This is converted to radians
            //  in the more standard mathematical convention of 0 degrees on the 
            //  + x axis and positive degrees in the counter-clockwise direction.
            DynamicList<scalar> nacYaw;

            //- Specify the fluid density (kg/m^3).  This turbine model is to be  
            //  used with an incompressible solver, so density divides out of the 
            //  momentum equations.  Therefore, turbine forces are given to the 
            //  solver asforce/density.  To get actual forces, torques, and power 
            //  written to file, provide a density by which to multiply.
            DynamicList<scalar> fluidDensity;

            //- Number of blades;
            DynamicList<int> numBl;

            //- Distance of nacelle fore to aft extent (m).
            DynamicList<scalar> nacelleLength;

            //- Nacelle frontal area (m^2).
            DynamicList<scalar> nacelleFrontalArea;

            //- Equivalent nacelle radius (m), based on frontal area.
            DynamicList<scalar> nacelleEquivalentRadius;

            //- Nacelle coefficient of drag.
            DynamicList<scalar> nacelleCd;




        //- Important Actuator Line Geometry Data.
            //- List of rotors, nacelles, and tower that this processor can forseeably control.
            DynamicList<label> bladesControlled;
            DynamicList<label> nacellesControlled;
            DynamicList<label> towersControlled;

            //- List of cell ID that contains a certain actuator line velocity sampling 
            //  point on theprocessor in control of that point.  If the value is -1,
            //  then this processor is not in control of this point.
            DynamicList<List<List<label> > > bladeMinDisCellID;
            DynamicList<label> nacelleMinDisCellID;
            DynamicList<List<label> > towerMinDisCellID;

            //- List of locations of the intersection of the tower axis and the shaft 
            //  centerline relative to the origin (m).
            DynamicList<vector> towerShaftIntersect;

            //- List of locations of the rotor apex relative to the origin (m).
            DynamicList<vector> rotorApex;
            DynamicList<vector> rotorApexBeforeSearch;

            //- List of list of labels of cells that turbine could lie within to narrow
            //  the search for cells for integral velocity sampling and for body force
            //  projection.  
            DynamicList<DynamicList<label> > bladeInfluenceCells;
            DynamicList<DynamicList<label> > towerInfluenceCells;
            DynamicList<DynamicList<label> > nacelleInfluenceCells;

            //- Actuator element width.
            DynamicList<DynamicList<scalar> > bladeDs;
            DynamicList<DynamicList<scalar> > towerDs;
            DynamicList<DynamicList<scalar> > nacelleDs;

            //- Actuator line point locations with respect to origin.
            DynamicList<List<List<vector> > > bladePoints;
            DynamicList<List<List<vector> > > bladePointsOld;
            DynamicList<List<List<vector> > > bladeSamplePoints;

            //- Actuator line point locations with respect to origin.
            DynamicList<List<vector> > towerPoints;
            DynamicList<List<vector> > towerSamplePoints;

            //- Actuator line point locations with respect to origin.
            DynamicList<List<vector> > nacellePoints;
            DynamicList<vector> nacelleSamplePoint;

            //- Random perturbation of sample points applied only when determining control
            //  processors to break ties.  This does not affect true location where
            //  velocity is sampled and forces are applied.
            DynamicList<List<List<vector> > > bladePointsPerturbVector;
            DynamicList<List<vector> > towerPointsPerturbVector;
            DynamicList<vector> nacellePointPerturbVector;

            //- Stores a list of coefficients for a periodic cosine function describing
            //  rotor distortion at each radial location.
            DynamicList<List<List<scalar> > > rotorSurfaceCoeffs;

            //- Total actuator line points in blade, nacelle, and tower arrays.
            int totBladePoints;
            int totBladeSamplePoints;
            int totTowerPoints;
            int totTowerSamplePoints;
            int totNacellePoints;

            //- Blade radius away from rotor apex.  Takes into account coning and bending.
            DynamicList<List<List<scalar> > > bladePointRadius;
            DynamicList<List<List<scalar> > > bladeSamplePointRadius;

            //- Blade chord, twist, thickness, user defined quantity, and airfoil type
            //  interpolated to the blade actuator elements from the blade definition.
            DynamicList<List<List<scalar> > > bladePointChord;
            
            //- Blade point orientation.
            DynamicList<List<List<tensor> > > bladePointOrientation;

            //- Tower height from ground.
            DynamicList<List<scalar> > towerPointHeight;

            //- Tower chord, twist, thickness, user defined quantity, and airfoil type
            //  interpolated to the tower actuator elements from the blade definition.
            DynamicList<List<scalar> > towerPointChord;

            //- Tower points orientation.
            DynamicList<List<tensor> > towerPointOrientation;

            //- Unit vector pointing along the rotor shaft (axis of blade rotation).
            DynamicList<vector> mainShaftOrientation;
            DynamicList<vector> mainShaftOrientationBeforeSearch;

            //- Three vectors for each blade of each turbine that define the local
            //  blade-aligned coordinate system.  Vector 2 is along the blade pointed
            //  from root to tip, vector 1 is in the tangential direction (direction
            //  of blade rotation) where positive points in the direction opposite
            //  rotation if the rotor turns clockwise as viewed from upstream, and 0
            //  points orthogonal to vector 1 and 2 and points toward downstream (but
            //  vector 0 is not perfectly aligned with downstream due to rotor coning
            //  and nacelle tilt).
            DynamicList<List<List<List<vector> > > > bladeAlignedVectors;
            DynamicList<List<List<List<vector> > > > bladeAlignedVectorsSample;

            // Wind vectors at each velocity sampling point in the Cartesian system.
            DynamicList<List<List<vector> > > bladeWindVectorsCartesian;

            // Wind vectors at each velocity sampling point in the turbine geometry-aligned system.
            DynamicList<List<List<vector> > > bladeWindVectors;
            DynamicList<List<vector> > towerWindVectors;
            DynamicList<vector> nacelleWindVector;            
            

            //- Blade force at each actuator point.
            DynamicList<List<List<vector> > > bladePointForce;

            //- Nacelle force at each actuator point.
            DynamicList<List<vector> > nacellePointForce;

            //- Tower force at each actuator point.
            DynamicList<List<vector> > towerPointForce;

            //- Wind vector at each blade, tower, or nacelle actuator point in
            //  geometry-aligned coordinate system (i.e., bladeWindVectors are in the
            //  Cartesian coordinate system).
            DynamicList<List<List<vector> > > bladeWindVectorsFAST;
            DynamicList<vector> nacelleWindVectorFAST;
            DynamicList<List<vector> > towerWindVectorsFAST;

            //- Change in yaw each time step.
            DynamicList<scalar> deltaNacYaw;

            //- Change in rotorAzimuth each time step.
            DynamicList<scalar> deltaAzimuth;



        //- Information critical to turbine performance that can be written to file
        //  every time step.

            //- Angle of attack at each actuator point.
            DynamicList<List<List<scalar> > > bladePointAlpha;
            DynamicList<List<scalar> > towerPointAlpha;

            //- Wind magnitude (not including radial wind) at each actuator point.
            DynamicList<List<List<scalar> > > bladePointVmag;
            DynamicList<List<scalar> > towerPointVmag;
            DynamicList<List<scalar> > nacellePointVmag;

            //- Coefficient of lift at each actuator point. 
            DynamicList<List<List<scalar> > > bladePointCl; 
            DynamicList<List<scalar> > towerPointCl; 

            //- Coefficient of drag at each actuator point. 
            DynamicList<List<List<scalar> > > bladePointCd; 
            DynamicList<List<scalar> > towerPointCd; 
            DynamicList<List<scalar> > nacellePointCd;

            //- Lift/density at each actuator point. 
            DynamicList<List<List<scalar> > > bladePointLift;
            DynamicList<List<scalar> > towerPointLift;

            //- Drag/density at each actuator point. 
            DynamicList<List<List<scalar> > > bladePointDrag;
            DynamicList<List<scalar> > towerPointDrag;
            DynamicList<List<scalar> > nacellePointDrag;

            //- Axial force/density at each actuator point (not pointed in blade-local
            //  axial, but rather along horizontal component of shaft). 
            DynamicList<List<List<scalar> > > bladePointAxialForce;
            DynamicList<List<scalar> > towerPointAxialForce;
            DynamicList<List<scalar> > nacellePointAxialForce;

            //- Torque/density at each actuator point.
            DynamicList<List<List<scalar> > > bladePointTorque;

            //- Horizontal force/density at each actuator point (horizontal and perpendicular
            //  to shaft axis).
            DynamicList<List<List<scalar> > > bladePointHorizontalForce;
            DynamicList<List<scalar> > towerPointHorizontalForce;
            DynamicList<List<scalar> > nacellePointHorizontalForce;


            //- Vertical force/density at each actuator point (horizontal and perpendicular
            //  to shaft axis).
            DynamicList<List<List<scalar> > > bladePointVerticalForce;
            DynamicList<List<scalar> > nacellePointVerticalForce;


            //- Axial force on turbine (along horizontal component of shaft axis).
            DynamicList<scalar> rotorAxialForce;
            DynamicList<scalar> nacelleAxialForce;
            DynamicList<scalar> towerAxialForce;

            //- Total rotor torque/density on turbine (about shaft axis).
            DynamicList<scalar> rotorTorque;

            //- Total horizontal force on turbine (horizontal and perpendicular to shaft axis).
            DynamicList<scalar> rotorHorizontalForce;
            DynamicList<scalar> nacelleHorizontalForce;
            DynamicList<scalar> towerHorizontalForce;

            //- Total vertical force on turbine (along tower axis).
            DynamicList<scalar> rotorVerticalForce;
            DynamicList<scalar> nacelleVerticalForce;

            //- Power of turbine rotor and generator.
            DynamicList<scalar> rotorPower;
            DynamicList<scalar> generatorPower;
             



        //- Output Data File Information.
            //- List of output files for blade points.
            OFstream* bladePointAlphaFile_;

            OFstream* bladePointVmagFile_;
            OFstream* bladePointVaxialFile_;
            OFstream* bladePointVtangentialFile_;
            OFstream* bladePointVradialFile_;

            OFstream* bladePointClFile_;
            OFstream* bladePointCdFile_;

            OFstream* bladePointLiftFile_;
            OFstream* bladePointDragFile_;

            OFstream* bladePointAxialForceFile_;
            OFstream* bladePointHorizontalForceFile_;
            OFstream* bladePointVerticalForceFile_;
            OFstream* bladePointTorqueFile_;

            OFstream* bladePointXFile_;
            OFstream* bladePointYFile_;
            OFstream* bladePointZFile_;



            //- List of output files for nacelle points.
            OFstream* nacellePointVmagFile_;
            OFstream* nacellePointVaxialFile_;
            OFstream* nacellePointVhorizontalFile_;
            OFstream* nacellePointVverticalFile_;

            OFstream* nacellePointDragFile_;

            OFstream* nacellePointAxialForceFile_;
            OFstream* nacellePointHorizontalForceFile_;
            OFstream* nacellePointVerticalForceFile_;


        
            //- List of output files for tower points.
            OFstream* towerPointAlphaFile_;

            OFstream* towerPointVmagFile_;
            OFstream* towerPointVaxialFile_;
            OFstream* towerPointVhorizontalFile_;
            OFstream* towerPointVverticalFile_;

            OFstream* towerPointClFile_;
            OFstream* towerPointCdFile_;

            OFstream* towerPointLiftFile_;
            OFstream* towerPointDragFile_;

            OFstream* towerPointAxialForceFile_;
            OFstream* towerPointHorizontalForceFile_;
            OFstream* towerPointVerticalForceFile_;



            //- List of output files for the rotor.
            OFstream* rotorTorqueFile_;
            OFstream* rotorAxialForceFile_;
            OFstream* rotorHorizontalForceFile_;
            OFstream* rotorVerticalForceFile_;
            OFstream* rotorPowerFile_;
            OFstream* generatorPowerFile_;
            OFstream* rotorSpeedFile_;
            OFstream* rotorSpeedFFile_;
            OFstream* rotorAzimuthFile_;



            //- List of output files for the nacelle.
            OFstream* nacelleAxialForceFile_;
            OFstream* nacelleHorizontalForceFile_;
            OFstream* nacelleVerticalForceFile_;
            OFstream* nacelleYawFile_;



            //- List of output files for the tower.
            OFstream* towerAxialForceFile_;
            OFstream* towerHorizontalForceFile_;



            //- List of output files for blade bladePitch angle.
            OFstream* bladePitchFile_;



            //- List of output files for the generator.
            OFstream* generatorTorqueFile_;

            

      
          
    
    // Private Member Functions

        void initialize();
        void readInput();
        void initializeArrays();

        //- Coarse search functions.
        void updateRotorSearchCells(int turbineNumber);
        void updateNacelleSearchCells(int turbineNumber);
        void updateTowerSearchCells(int turbineNumber);
        void updateTurbinesControlled();
        void updateBladePointControlProcNo();
        void updateNacellePointControlProcNo();
        void updateTowerPointControlProcNo();

        //- Velocity sampling functions.
        void updateRadius(int turbineNumber);
        void computeBladeAlignedVectors();
        void sampleBladePointWindVectors();
        void sampleNacellePointWindVectors();
        void sampleTowerPointWindVectors();
        void computeBladeAlignedVelocity();
        void computeBladePointRadius();

        //- FAST access functions.
        void sendInput();
        void getPositions();
        void sendVelocities();
        void getForces();
        void getNumBlades();
        void getNumBladePoints();
        void getNumTowerPoints();
        void getChordLengths();

        //- Body force projection.
        void updateBladeBodyForce(int turbineNumber);
        void updateNacelleBodyForce(int turbineNumber);
        void updateTowerBodyForce(int turbineNumber);

        //- Various projection functions.
        scalar computeBladeProjectionFunction(vector disVector, scalar r0, int turbineNumber, int bladeNumber, int elementNumber);
        scalar uniformGaussian3D(scalar epsilon, scalar d);
        scalar generalizedGaussian3D(vector epsilon, vector d, vector dir0, vector dir1, vector dir2);
        scalar generalizedGaussian2D(vector epsilon, vector d, vector dir0, vector dir1);
        scalar diskGaussian(scalar rEpsilon, scalar xEpsilon, vector u, scalar r0, vector d);
        scalar ringGaussian(scalar rEpsilon, scalar xEpsilon, vector u, scalar r0, vector d);
        scalar lineToDiskGaussian3D(vector epsilon, scalar r0, scalar n, vector d);
        scalar gaussian1D(scalar x, scalar x0, scalar epsilon, scalar coeff);

        //- If using the actuator disk projection function, the shape of the disk must be
        //  computed.  It is done by fitting a periodic function through the blade points.
        void computeRotorSurfaceCoeffs(int turbineNumber);
        scalar getPointOnRotorSurface(int turbineNumber, scalar r, scalar theta);

        //- Rotates a point/vector about a rotation axis and rotation point by the specified
        //  angle in radians.
        vector rotateVector(vector v, vector translation, vector axis, scalar angle);

        //- Transform a vector from one coordinate system to another.
        vector transformGlobalCartToLocalCart(vector v,  vector xP, vector yP, vector zP);
        vector transformLocalCartToGlobalCart(vector vP, vector xP, vector yP, vector zP);
        vector transformCartToCyl(vector v);
        vector transformCylToCart(vector c);
        vector transformGlobalCartToRotorLocalCyl(vector v, int turbineNumber);

        //- Perform interpolation.
        void findBoundingIndices(scalar x, DynamicList<scalar>& xList, label& indexM, label& indexP);
        scalar interpolate(scalar x, DynamicList<scalar>& xList, DynamicList<scalar>& yList);
        label  interpolate(scalar x, DynamicList<scalar>& xList, DynamicList<label>& yList);

        //- Change a degree angle measurement from compass to standard.
        scalar compassToStandard(scalar dir);

        //- Change a degree angle measurement from standard to compass.
        scalar standardToCompass(scalar dir);

        //- Open turbine data output files.
        void openOutputFiles();
           
        //- Write turbine information to file.
        void printOutputFiles();

        //- Print variables for debugging.
        void printDebug();


public:
    //- Constructor
    horizontalAxisWindTurbinesALMOpenFAST
    (
        const volVectorField& U
    );
    
      
    //- Destructor
    virtual ~horizontalAxisWindTurbinesALMOpenFAST()
    {};
      
      
    // Public Member Functions
    
       //- Update state of turbine.
       void update();
    
       //- Return force.
       volVectorField& force();
    
       //- End.
       void end();
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace turbineModels
} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //

