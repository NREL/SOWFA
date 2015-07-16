/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 1991-2009 OpenCFD Ltd.
     \\/     M anipulation  |
-------------------------------------------------------------------------------
License
    This file is part of OpenFOAM.

    OpenFOAM is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 2 of the License, or (at your
    option) any later version.

    OpenFOAM is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with OpenFOAM; if not, write to the Free Software Foundation,
    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

Namespace
    None

Class
    horizontalAxisWindTurbinesFAST

Description
    This is the horizontal-axis wind turbine array actuator line model class.
    It will set up an array of various kinds of turbines (currently blades 
    only) within a flow field.  The blade rotation rate is set or calculated
    based on a simple torque control model (not implemented yet), the blades
    are rotated at each time step, the turbine is yawed (not implemented yet),
    the blade forces are calculated, the actuator line body force information
    is passed back to the flow solver, and turbine information is written to
    files.

SourceFiles
    horizontalAxisWindTurbinesFAST.C

\*---------------------------------------------------------------------------*/

#ifndef horizontalAxisWindTurbinesFAST_H
#define horizontalAxisWindTurbinesFAST_H

#include "HashPtrTable.H"
#include "IOdictionary.H"
#include "IFstream.H"
#include "OFstream.H"
#include "fvCFD.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace turbineModels
{

/*---------------------------------------------------------------------------*\
                           Class horizontalAxisWindTurbinesFAST declaration
\*---------------------------------------------------------------------------*/

class horizontalAxisWindTurbinesFAST
{

private:

    const Time& runTime_;

    //- Mesh pointer.
    const fvMesh& mesh_;

    //- Velocity field pointer.
    const volVectorField& U_;

    volTensorField gradU;

    volVectorField bodyForceFAST;

    List<word> turbineName;

    scalar rotorD;

    List<scalar > refx;

    List<scalar > refy;

    List<scalar > refz;

    List<scalar > hubz;

    scalar PI;

    scalar yawAngle;

    scalar epsilon;

    scalar smearRadius;

    scalar effectiveRadiusFactor;

    scalar pointInterpType; 

    List<DynamicList<label> > sphereCells;			

    List<List<label> > minDistCellID;

    List<List<List<vector> > > bladePoints;

    List<List<List<vector> > > bladeForce;

    void getBldPosForce(int iturb);

public:

    int turbNum;

    int bldNum;	

    int bldPts; 

    int totalBldpts;

/*
    float uin[3][200];
    float vin[3][200];
    float win[3][200];

    float bldptx[3][200];
    float bldpty[3][200];
    float bldptz[3][200];

    float bldfx[3][200];
    float bldfy[3][200];
    float bldfz[3][200];
*/

    float **uin;
    float **vin;
    float **win;

    float **bldptx;
    float **bldpty;
    float **bldptz;

    float **bldfx;
    float **bldfy;
    float **bldfz;


    //- Constructor
    horizontalAxisWindTurbinesFAST
    (
        const volVectorField& U
    );
    
    //- Destructor
    virtual ~horizontalAxisWindTurbinesFAST()
    {

      for(int i=0;i<turbNum;i++)
      {

        delete [] uin[i];
        delete [] vin[i];
        delete [] win[i]; 

        delete [] bldptx[i];
        delete [] bldpty[i];
        delete [] bldptz[i];

        delete [] bldfx[i];
        delete [] bldfy[i];
        delete [] bldfz[i];

      }

      delete [] uin;
      delete [] vin;
      delete [] win;

      delete [] bldptx;
      delete [] bldpty;
      delete [] bldptz;

      delete [] bldfx;
      delete [] bldfy;
      delete [] bldfz;
   
    }

    void getBldPos(int iturb);
    
    //~ get box cellIDs
    void getSphereCellID(int iturb);
   
    //- get proc # & cellID for each grd pts
    void getContProcNoCellID(int iturb);  
    
    //- get wind field
    void getWndVec(int iturb);
	
    //- get forces and positions
    void computeBodyForce(int iturb);

    //- Return force.
    volVectorField& force();

};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace turbineModels
} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //

