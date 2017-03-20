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
    horizontalAxisWindTurbinesALMfastv8

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
    horizontalAxisWindTurbinesALMfastv8.C

\*---------------------------------------------------------------------------*/

#ifndef horizontalAxisWindTurbinesALMfastv8_H
#define horizontalAxisWindTurbinesALMfastv8_H

#include "HashPtrTable.H"
#include "IOdictionary.H"
#include "IFstream.H"
#include "OFstream.H"
#include "fvCFD.H"
#include "Random.H"
#include "FAST_cInterface.h"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace turbineModels
{

/*---------------------------------------------------------------------------*\
                           Class horizontalAxisWindTurbinesALMfastv8 declaration
\*---------------------------------------------------------------------------*/

class horizontalAxisWindTurbinesALMfastv8
{

private:
    // Private Data
        //- The FAST interface
        FAST_cInterface FAST;
        
        //- Constants
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

            //- Number of distinct turbines in array.
            int numTurbinesDistinct;

            //- List of distinct names of turbine types in array.
            DynamicList<word> turbineTypeDistinct;

            //- ID label given to each distinct type of turbine in the array.
            DynamicList<label> turbineTypeID;



        //- Turbine Level Data (all variables start with a capital letter).

            //*** THE FOLLOWING VARIABLES MATCH FAST INPUT FILE ***
            //- Number of blades;
            DynamicList<int> NumBl;

            //- Distance from rotor apex to blade tip (m).
            DynamicList<scalar> TipRad;

            //- Distance from rotor apex to blade root (m).
            DynamicList<scalar> HubRad;

            //- Distance from teeter pin to rotor apex (m).
            DynamicList<scalar> UndSling;

            //- Distance from nacelle yaw axis to teeter pin or rotor apex (m).
            DynamicList<scalar> OverHang;

            //- Distance of nacelle fore to aft extent (m).
            DynamicList<scalar> NacelleLength;

            //- Nacelle frontal area (m^2).
            DynamicList<scalar> NacelleFrontalArea;

            //- Equivalent nacelle radius (m), based on frontal area.
            DynamicList<scalar> NacelleEquivalentRadius;

            //- Nacelle coefficient of drag.
            DynamicList<scalar> NacelleCd;

            //- Height of tower top above ground (m).
            DynamicList<scalar> TowerHt;

            //- Vertical distance from tower-top to rotor shaft centerline (m).
            DynamicList<scalar> Twr2Shft;

            //- Shaft tilt-up angle (degrees).
            DynamicList<scalar> ShftTilt;

            //- Coning angle of blades (degrees) (one for each blade).
            DynamicList<List<scalar> > PreCone;

            //- Gear-box ratio.
            DynamicList<scalar> GBRatio;

            //- Gear-box efficiency.
            DynamicList<scalar> GBEfficiency;

            //- Generator efficiency.
            DynamicList<scalar> GenEfficiency;

            //- Rated rotor speed (rpm).
            DynamicList<scalar> RatedRotSpeed;

            //- Moment of inertia of generator about high-speed shaft (kg-m^2).
            DynamicList<scalar> GenIner;

            //- Moment of inertia of hub about rotor shaft (kg-m^2).
            DynamicList<scalar> HubIner;
            //*** END OF FAST INPUT FILE VARIABLES ***

            //- Moment of inertia of a single blade about rotor shaft (kg-m^2).
            DynamicList<scalar> BladeIner;

            //- Moment of inertia of entire drivetrain (rotor blades, hub, and
            //  generator about the rotor shaft (kg-m^2).
            DynamicList<scalar> DriveTrainIner;

            //- Rotor speed controller type.  Options are "none" or "fiveRegion".
            //  "none" provides no torque control and the rotor rotates at a 
            //  constant rate specified by the variable "rotorSpeed".  "fiveRegion"
            //  controls rotor speed through generator torque in regions 1, 1-1/2,
            //  2, 2-1/2, and 3.  Torque control alone will not control speed in
            //  region 3, but if no pitch control is implemented, then rotor speed
            //  will be limited at rated regardless of generator and rotor torque.
            DynamicList<word> GenTorqueControllerType;

            //- Nacelle yaw controller type (NOT IMPLEMENTED, just remains at 
            //  specified nacYaw).
            DynamicList<word> NacYawControllerType;

            //- Pitch controller type (NOT IMPLEMENTED, just remains at 
            //  specified pitch).
            DynamicList<word> BladePitchControllerType;

            //- Engage a rotor speed limiter (do not let rotor speed exceed rated
            //  or become negative.
            DynamicList<bool> RotSpeedLimiter;

            //- Engage a generator torque rate limiter.
            DynamicList<bool> GenTorqueRateLimiter;

            //- Engage a yaw rate limiter.
            DynamicList<bool> NacYawRateLimiter;

            //- Engage a blade pitch rate limiter.
            DynamicList<bool> BladePitchRateLimiter;

            //- Parameter for low-pass speed filter for control system (Hz).
            DynamicList<scalar> SpeedFilterCornerFrequency;


            //- Generator torque control parameters.
                //- Limiter on rate of change of generator torque (N-m/s).
                DynamicList<scalar> RateLimitGenTorque;

                //- Cut-in generator speed (rpm).
                DynamicList<scalar> CutInGenSpeed;

                //- Generator speed at start of region 2 (rpm).
                DynamicList<scalar> Region2StartGenSpeed;

                //- Generator speed at end of region 2 (rpm).
                DynamicList<scalar> Region2EndGenSpeed;

                //- Cut-in generator torque (N-m).
                DynamicList<scalar> CutInGenTorque;

                //- Rated generator torque (N-m).
                DynamicList<scalar> RatedGenTorque;

                //- Torque controller constant of proportionality (N-m/rpm^2).
                //  T_gen = K_gen*omega^2.
                DynamicList<scalar> KGen;

                //- Generator speed versus generator torque table (rpm, N-m).
                DynamicList<List<List<scalar> > > SpeedTorqueTable;
                DynamicList<DynamicList<scalar> > SpeedGenProfile;
                DynamicList<DynamicList<scalar> > TorqueGenProfile;


            //- Pitch control parameters.
                //- Maximum rate of change of blade pitch (degree/s).
                DynamicList<scalar> RateLimitBladePitch;

                //- Minimum pitch (degrees).
                DynamicList<scalar> PitchMin;

                //- Maximum pitch (degrees).
                DynamicList<scalar> PitchMax;

                //- Blade pitch angle at which the sensitivity of power to
                //  changes in blade pitch has doubled from its value at the
                //  rated operating point (degrees).
                DynamicList<scalar> PitchK;

                //- Pitch control gains.
                DynamicList<scalar> PitchControlKP; // (s)
                DynamicList<scalar> PitchControlKI; // (unitless)
                DynamicList<scalar> PitchControlKD; // (s^2)
     
       
            //- Nacelle yaw control parameters
                //- Maximum rate of change of nacelle yaw (degrees/s).
                DynamicList<scalar> RateLimitNacYaw;



            //- List of airfoils that compose turbine blade;
            DynamicList<List<word> > AirfoilType;


            //- Lists of blade data for each turbine type.
                //- Overall blade data array.
                DynamicList<List<List<scalar> > > BladeData;

                //- Station along blade in which information is given (m).
                DynamicList<DynamicList<scalar> > BladeStation;

                //- Chord at this station (m).
                DynamicList<DynamicList<scalar> > BladeChord;

                //- Twist at this station (degrees).
                DynamicList<DynamicList<scalar> > BladeTwist;

                //- Thickness at this station (percentage of chord).
                DynamicList<DynamicList<scalar> > BladeThickness;

                //- Placeholder for some other user-defined property at this station.
                DynamicList<DynamicList<scalar> > BladeUserDef;

                //- Airfoil type ID at this station.
                DynamicList<DynamicList<label> > BladeAirfoilTypeID;


            //- Lists of tower data for each turbine type.
                //- Overall tower data array.
                DynamicList<List<List<scalar> > > TowerData;

                //- Station along tower in which information is given (m).
                DynamicList<DynamicList<scalar> > TowerStation;

                //- Chord at this height (m).
                DynamicList<DynamicList<scalar> > TowerChord;

                //- Twist at this height (degrees).
                DynamicList<DynamicList<scalar> > TowerTwist;

                //- Thickness at this height (percentage of chord).
                DynamicList<DynamicList<scalar> > TowerThickness;

                //- Placeholder for some other user-defined property at this station.
                DynamicList<DynamicList<scalar> > TowerUserDef;

                //- Airfoil type ID at this height.
                DynamicList<DynamicList<label> > TowerAirfoilTypeID;



        //- Airfoil Level Data (all variables start with a lower-case letter).
            //- Number of distinct airfoils being used in turbines in array.
            int numAirfoilsDistinct;

            //- List of distinct type of airfoils amongst all turbines in array.
            DynamicList<word> airfoilTypesDistinct;

            //- Overall airfoil data array.
            DynamicList<List<List<scalar> > > airfoilData;
            
            //- Angle-of-attack.
            DynamicList<DynamicList<scalar> > airfoilAlpha;

            //- Lift Coefficient.
            DynamicList<DynamicList<scalar> > airfoilCl;

            //- Drag Coefficient.
            DynamicList<DynamicList<scalar> > airfoilCd;



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

            //- Total actuator line points in blade, nacelle, and tower arrays.
            int totBladePoints;
            int totTowerPoints;
            int totNacellePoints;

            //- Blade radius away from rotor apex.  Must take into account coning.
            DynamicList<List<List<scalar> > > bladePointRadius;

            //- Blade chord, twist, thickness, user defined quantity, and airfoil type
            //  interpolated to the blade actuator elements from the blade definition.
            DynamicList<List<List<scalar> > > bladePointChord;
            DynamicList<List<List<scalar> > > bladePointTwist;
            DynamicList<List<List<scalar> > > bladePointThickness;
            DynamicList<List<List<scalar> > > bladePointUserDef;
            DynamicList<List<List<label> > > bladePointAirfoil;

            //- Tower height from ground.
            DynamicList<List<scalar> > towerPointHeight;

            //- Tower chord, twist, thickness, user defined quantity, and airfoil type
            //  interpolated to the tower actuator elements from the blade definition.
            DynamicList<List<scalar> > towerPointChord;
            DynamicList<List<scalar> > towerPointTwist;
            DynamicList<List<scalar> > towerPointThickness;
            DynamicList<List<scalar> > towerPointUserDef;
            DynamicList<List<label> > towerPointAirfoil;

            //- An indicator of shaft direction.  The convention is that when viewed
            //  from upwind, the rotor turns clockwise for positive rotation angles,
            //  regardless of if it is an upwind or downwind turbine.  uvShaft is
            //  found by subtracting the rotor apex location from the tower shaft
            //  intersection point.  This vector switches direciton depending on 
            //  if the turbine is upwind or downwind, so this uvShaftDir multiplier
            //  makes the vector consistent no matter what kind of turbine.
            DynamicList<scalar> uvShaftDir;

            //- Unit vector pointing along the rotor shaft (axis of blade rotation).
            DynamicList<vector> uvShaft;

            //- Unit vector pointing along the tower (axis of yaw).
            DynamicList<vector> uvTower;

            //- Blade force at each actuator point.
            DynamicList<List<List<vector> > > bladePointForce;

            //- Nacelle force at each actuator point.
            DynamicList<List<vector> > nacellePointForce;

            //- Tower force at each actuator point.
            DynamicList<List<vector> > towerPointForce;

            //- Three vectors for each blade of each turbine that define the local
            //  blade-aligned coordinate system.  Vector 2 is along the blade pointed
            //  from root to tip, vector 1 is in the tangential direction (direction
            //  of blade rotation) where positive points in the direction opposite 
            //  rotation if the rotor turns clockwise as viewed from upstream, and 0
            //  points orthogonal to vector 1 and 2 and points toward downstream (but 
            //  vector 0 is not perfectly aligned with downstream due to rotor coning  
            //  and nacelle tilt).
            DynamicList<List<List<vector> > > bladeAlignedVectors;

            //- Wind vector at each blade, tower, or nacelle actuator point in
            //  geometry-aligned coordinate system (i.e., bladeWindVectors are in the
            //  bladeAlignedVectors coordinate system).
            DynamicList<List<List<vector> > > bladeWindVectors;
            DynamicList<vector> nacelleWindVector;
            DynamicList<List<vector> > towerWindVectors;

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

        //- The FAST access functions.
        void getFASTpositions();
        void FASTupdate();
        void getFASTforces();


        //- Define the rotor, nacelle, and tower search cells.
        void findRotorSearchCells(int turbineNumber);
        void findNacelleSearchCells(int turbineNumber);
        void findTowerSearchCells(int turbineNumber);

        //- Update the list of turbines controlled by this processor.
        void updateTurbinesControlled();
        
        //- Rotate the blades.
        void rotateBlades();
        
        //- Yaw the nacelle.
        void yawNacelle();

        //- Computer the rotor speed.
        void computeRotSpeed();

        //- Filter the rotor speed with a low pass filter for control system.
        void filterRotSpeed();
        
        //- Calculate generator torque.
        void controlGenTorque();

        //- Calculate the nacelle yaw position.
        void controlNacYaw();

        //- Calculate the blade pitch.
        void controlBladePitch();

        //- Find out which processor zone each actuator line point lies within.
        //  (Which processor is controlling each actuator line point?)
        void findBladePointControlProcNo();
        void findNacellePointControlProcNo();
        void findTowerPointControlProcNo();

        //- Compute the radius of the blade reference cells from the main shaft
        //  axis.
        void updateRadius(int turbineNumber);
        
        //- Compute the wind vector at each actuator point.
        void computeBladePointWindVectors();
        void computeNacellePointWindVectors();
        void computeTowerPointWindVectors();

        //- Compute the vectors that define the blade orientation (this is on a
        //  per blade basis.  It should be updated in the future to be on a per
        //  blade element basis to account for blade curvature due to deformation
        //  or base geometry.
        void computeBladeAlignedVectors();

        //- Compute blade forces.
        void computeBladePointForce();
        void computeNacellePointForce();
        void computeTowerPointForce();

        //- Function to compute the projection function at a given mesh cell for
        //  the blade force.
        scalar computeBladeProjectionFunction(vector disVector, int turbineNumber, int bladeNumber, int elementNumber);
        
        //- Compute body forces.
        void computeBladeBodyForce();
        void computeNacelleBodyForce();
        void computeTowerBodyForce();

        //- Compute body force spreading/projection using different methods.
        scalar uniformGaussian3D(scalar epsilon, scalar d);
        scalar generalizedGaussian3D(vector epsilon, vector d, vector dir0, vector dir1, vector dir2);
        scalar generalizedGaussian2D(vector epsilon, vector d, vector dir0, vector dir1);
        scalar diskGaussian(scalar rEpsilon, scalar xEpsilon, vector u, scalar r0, vector d);
        scalar ringGaussian(scalar rEpsilon, scalar xEpsilon, vector u, scalar r0, vector d);
        scalar gaussian1D(scalar x, scalar x0, scalar epsilon, scalar coeff);

        //- Rotates a point/vector about a rotation axis and rotation point by the specified
        //  angle in radians.
        vector rotateVector(vector v, vector translation, vector axis, scalar angle);

        //- Transform a vector from one coordinate system to another.
        vector transformVectorCartToLocal(vector v,  vector xP, vector yP, vector zP);
        vector transformVectorLocalToCart(vector vP, vector xP, vector yP, vector zP);

        //- Perform interpolation.
        scalar interpolate(scalar xNew, DynamicList<scalar>& xOld, DynamicList<scalar>& yOld);
        label  interpolate(scalar xNew, DynamicList<scalar>& xOld, DynamicList<label>& yOld);

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
    horizontalAxisWindTurbinesALMfastv8
    (
        const volVectorField& U
    );
    
    
    //- Destructor
    virtual ~horizontalAxisWindTurbinesALMfastv8()
    {
        //delete FAST;
    }
    
    
    // Public Member Functions

        //- Update state of turbine.
        void update();

    //- Return force.
    volVectorField& force();
        
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace turbineModels
} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //

