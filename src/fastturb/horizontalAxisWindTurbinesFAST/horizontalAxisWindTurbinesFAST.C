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

\*---------------------------------------------------------------------------*/

#include "horizontalAxisWindTurbinesFAST.H"

namespace Foam
{
namespace turbineModels
{

// * * * * * * * * * * * * * *  Constructor  * * * * * * * * * * * * * * * * //

horizontalAxisWindTurbinesFAST::horizontalAxisWindTurbinesFAST
(
    const volVectorField& U
)	      
:

    runTime_(U.time()),
	
    mesh_(U.mesh()),

    // Set the pointer to the velocity field
    U_(U),

    gradU
    (
        IOobject
        (
            "gradU",
            runTime_.timeName(),
            mesh_,
            IOobject::NO_READ,
            IOobject::NO_WRITE
        ),
        mesh_,
        dimensionedTensor("gradU",dimVelocity/dimLength,tensor::zero)
    ),

    bodyForceFAST
    (
        IOobject
        (
            "bodyForceFAST",
            runTime_.timeName(),
            mesh_,
            IOobject::NO_READ,
            IOobject::AUTO_WRITE
        ),
        mesh_,
        dimensionedVector("bodyForceFAST",dimForce/dimVolume/dimDensity,vector::zero)
    )


{

    IOdictionary turbineArrayProperties
    (
        IOobject
        (
            "turbineArrayPropertiesFAST",
            runTime_.constant(),
            mesh_,
            IOobject::MUST_READ,
            IOobject::NO_WRITE
        )
    );

    turbineName = turbineArrayProperties.toc();
    turbNum = turbineName.size() - 1;
    
    refx.setSize(turbNum);
    refy.setSize(turbNum);
    refz.setSize(turbNum);
    hubz.setSize(turbNum);

    // read turbineArray dictionary file  
    for(int i=0; i<turbNum;i++)
    {
      refx[i] = readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("refx"));
      refy[i] = readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("refy"));
      refz[i] = readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("refz"));
      hubz[i] = readScalar(turbineArrayProperties.subDict(turbineName[i]).lookup("hubz"));
    }

    yawAngle = readScalar(turbineArrayProperties.subDict(turbineName[turbNum]).lookup("yawAngle"));
    bldNum = readScalar(turbineArrayProperties.subDict(turbineName[turbNum]).lookup("numberofBld"));
    bldPts = readScalar(turbineArrayProperties.subDict(turbineName[turbNum]).lookup("numberofBldPts"));
    rotorD = readScalar(turbineArrayProperties.subDict(turbineName[turbNum]).lookup("rotorDiameter"));
    epsilon = readScalar(turbineArrayProperties.subDict(turbineName[turbNum]).lookup("epsilon"));
    smearRadius = readScalar(turbineArrayProperties.subDict(turbineName[turbNum]).lookup("smearRadius"));
    effectiveRadiusFactor = readScalar(turbineArrayProperties.subDict(turbineName[turbNum]).lookup("effectiveRadiusFactor"));
    pointInterpType = readScalar(turbineArrayProperties.subDict(turbineName[turbNum]).lookup("pointInterpType"));

    // set variable sizes
    totalBldpts = bldNum*bldPts;

    minDistCellID.setSize(turbNum, List<label>(totalBldpts,-1));

    bladePoints.setSize(turbNum, List<List<vector> >(bldNum, List<vector>(bldPts,vector::zero)));
    bladeForce.setSize(turbNum, List<List<vector> >(bldNum, List<vector>(bldPts,vector::zero)));

    sphereCells.setSize(turbNum);

    yawAngle = 2.0*asin(1.0)/180.0*yawAngle;

    // initialize data-messenger variables (may not be required on some systems) 

    uin = new float*[turbNum];
    vin = new float*[turbNum];
    win = new float*[turbNum];

    bldptx = new float*[turbNum];
    bldpty = new float*[turbNum];
    bldptz = new float*[turbNum];

    bldfx = new float*[turbNum];
    bldfy = new float*[turbNum];
    bldfz = new float*[turbNum];

    for(int i=0;i<turbNum;i++)
    {
   
      uin[i] = new float[totalBldpts];
      vin[i] = new float[totalBldpts];
      win[i] = new float[totalBldpts];

      bldptx[i] = new float[totalBldpts];
      bldpty[i] = new float[totalBldpts];
      bldptz[i] = new float[totalBldpts];

      bldfx[i] = new float[totalBldpts];
      bldfy[i] = new float[totalBldpts];
      bldfz[i] = new float[totalBldpts];

    }


    for(int i=0;i<turbNum;i++)
    {
      for(int j=0;j<totalBldpts;j++)
      {
        uin[i][j] = 0.0;
        vin[i][j] = 0.0;
        win[i][j] = 0.0;

        bldptx[i][j] = 0.0;
        bldpty[i][j] = 0.0;
        bldptz[i][j] = 0.0;

        bldfx[i][j] = 0.0;
        bldfy[i][j] = 0.0;
        bldfz[i][j] = 0.0;
      }
    }

    // set sphere surounding turbine
    for(int i=0;i<turbNum;i++)
    {
      getSphereCellID(i);
    }

} 


// * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * * * //
void horizontalAxisWindTurbinesFAST::getSphereCellID(int iturb) 
{

    DynamicList<label> sphereCellsI;
    vector hubLoc;
    scalar effectiveRadius;

    hubLoc.x() = refx[iturb];
    hubLoc.y() = refy[iturb];
    hubLoc.z() = refz[iturb] + hubz[iturb];  
    effectiveRadius = effectiveRadiusFactor*rotorD/2.0;

    forAll(U_.mesh().cells(),cellI)
    {
      if ( mag(U_.mesh().C()[cellI] - hubLoc) <= effectiveRadius ) 
      {
        sphereCellsI.append(cellI);
      }
    }
    sphereCells[iturb].append(sphereCellsI);
    sphereCellsI.clear();

}


void horizontalAxisWindTurbinesFAST::getBldPos(int iturb)
{

    List<vector> bldPtsLocal(totalBldpts,vector::zero);

    for(int j=0;j<bldNum; j++)
    {
      for(int k=0;k<bldPts; k++)
      {
        bldPtsLocal[k+bldPts*j].x() = bldptx[iturb][k+bldPts*j]*cos(yawAngle)
                                    - bldpty[iturb][k+bldPts*j]*sin(yawAngle)
                                    + refx[iturb];

        bldPtsLocal[k+bldPts*j].y() = bldptx[iturb][k+bldPts*j]*sin(yawAngle)
                                    + bldpty[iturb][k+bldPts*j]*cos(yawAngle)
                                    + refy[iturb];

        bldPtsLocal[k+bldPts*j].z() = bldptz[iturb][k+bldPts*j] + refz[iturb];
      }
    }

    Pstream::gather(bldPtsLocal,sumOp<List<vector> >());
    Pstream::scatter(bldPtsLocal);

    for(int j=0;j<bldNum; j++)
    {
      for(int k=0;k<bldPts; k++)
      {

        bladePoints[iturb][j][k].x() = bldPtsLocal[k+bldPts*j].x();
        bladePoints[iturb][j][k].y() = bldPtsLocal[k+bldPts*j].y();
        bladePoints[iturb][j][k].z() = bldPtsLocal[k+bldPts*j].z();

      }
    }

}


void horizontalAxisWindTurbinesFAST::getContProcNoCellID(int iturb)  
{

    List<scalar> minDisLocal(totalBldpts,1.0E30);
    List<scalar> minDisGlobal(totalBldpts,1.0E30);
    
    if (sphereCells[iturb].size() > 0)
    {
      for(int j=0;j<bldNum; j++)
      {
        for(int k=0;k<bldPts; k++)
        {
          label cellID = -1;
          scalar minDis = 1.0E10;
 
          forAll(sphereCells[iturb], m)
          {
            scalar dis = mag(mesh_.C()[sphereCells[iturb][m]] - bladePoints[iturb][j][k]);
            if(dis <= minDis)
            {
              cellID = sphereCells[iturb][m];
              minDis = mag(mesh_.C()[cellID] - bladePoints[iturb][j][k]);
            }
          }  
          minDisLocal[k+bldPts*j] = minDis;
          minDisGlobal[k+bldPts*j] = minDis;
          minDistCellID[iturb][k+bldPts*j] = cellID;
        }
      }
    }

    Pstream::gather(minDisGlobal, minOp<List<scalar> >());
    Pstream::scatter(minDisGlobal);

    // For each grid pts, find the control processor # and the associated cellID within the processor
    if (sphereCells[iturb].size() > 0)
    {
      for(int j=0;j<bldNum; j++)
      {
        for(int k=0;k<bldPts; k++)
        {
          if(minDisGlobal[k+bldPts*j] != minDisLocal[k+bldPts*j])
          {
             minDistCellID[iturb][k+bldPts*j] = -1;
          }
        }
      }
    }

}


void horizontalAxisWindTurbinesFAST::getWndVec(int iturb)
{

    List<vector> windVectorLocal(totalBldpts, vector::zero);

    gradU = fvc::grad(U_);


    getContProcNoCellID(iturb);  


    if (sphereCells[iturb].size() > 0)
    {
      for(int j=0;j<bldNum; j++)
      {
        for(int k=0;k<bldPts; k++)
        {
          if(minDistCellID[iturb][k+bldPts*j] != -1)
          {
            windVectorLocal[k+bldPts*j] = U_[minDistCellID[iturb][k + bldPts*j]];

            if (pointInterpType == 1)
            {
              vector dx = bladePoints[iturb][j][k] - mesh_.C()[minDistCellID[iturb][k + bldPts*j]];
              vector dU = dx & gradU[minDistCellID[iturb][k + bldPts*j]];
              windVectorLocal[k+bldPts*j] += dU;
            }

          }
        }
      } 
    }
 

    Pstream::gather(windVectorLocal, sumOp<List<vector> >());
    Pstream::scatter(windVectorLocal);

      for(int j=0;j<bldNum; j++)
      {
        for(int k=0;k<bldPts; k++)
        {
 
          uin[iturb][k+bldPts*j] = windVectorLocal[k+bldPts*j].x()*cos(yawAngle)
                                 + windVectorLocal[k+bldPts*j].y()*sin(yawAngle);

          vin[iturb][k+bldPts*j] = -windVectorLocal[k+bldPts*j].x()*sin(yawAngle)
                                 +  windVectorLocal[k+bldPts*j].y()*cos(yawAngle);

          win[iturb][k+bldPts*j] = windVectorLocal[k+bldPts*j].z();
          
        }
      }

}

 
void horizontalAxisWindTurbinesFAST::getBldPosForce(int iturb)
{

    List<vector> bldPtsLocal(totalBldpts,vector::zero);
    List<vector> bldForceLocal(totalBldpts,vector::zero);
 
    for(int j=0;j<bldNum; j++)
    {
      for(int k=0;k<bldPts; k++)
      {

        bldPtsLocal[k+bldPts*j].x() = double(bldptx[iturb][k+bldPts*j]);
        bldPtsLocal[k+bldPts*j].y() = double(bldpty[iturb][k+bldPts*j]);
        bldPtsLocal[k+bldPts*j].z() = double(bldptz[iturb][k+bldPts*j]);

        bldForceLocal[k+bldPts*j].x() = double(bldfx[iturb][k+bldPts*j]);
        bldForceLocal[k+bldPts*j].y() = double(bldfy[iturb][k+bldPts*j]);
        bldForceLocal[k+bldPts*j].z() = double(bldfz[iturb][k+bldPts*j]);

      }
    }
 
    Pstream::gather(bldPtsLocal, sumOp<List<vector> >());
    Pstream::gather(bldForceLocal, sumOp<List<vector> >());

    Pstream::scatter(bldPtsLocal);
    Pstream::scatter(bldForceLocal);


    for(int j=0;j<bldNum; j++)
    {
      for(int k=0;k<bldPts; k++)
      {

        bladePoints[iturb][j][k].x() = bldPtsLocal[k+bldPts*j].x()*cos(yawAngle)
                                     - bldPtsLocal[k+bldPts*j].y()*sin(yawAngle) 
                                     + refx[iturb];

        bladePoints[iturb][j][k].y() = bldPtsLocal[k+bldPts*j].x()*sin(yawAngle)
                                     + bldPtsLocal[k+bldPts*j].y()*cos(yawAngle) 
                                     + refy[iturb];

        bladePoints[iturb][j][k].z() = bldPtsLocal[k+bldPts*j].z() + refz[iturb];


        bladeForce[iturb][j][k].x() = -bldForceLocal[k+bldPts*j].x()*cos(yawAngle)
                                      +bldForceLocal[k+bldPts*j].y()*sin(yawAngle);

        bladeForce[iturb][j][k].y() = -bldForceLocal[k+bldPts*j].x()*sin(yawAngle)
                                      -bldForceLocal[k+bldPts*j].y()*cos(yawAngle);

        bladeForce[iturb][j][k].z() = -bldForceLocal[k+bldPts*j].z();

      }
    }
 
}


void horizontalAxisWindTurbinesFAST::computeBodyForce(int iturb)
{ 
    getBldPosForce(iturb);

    if (sphereCells[iturb].size() > 0)
    {
      forAll(sphereCells[iturb], m)
      {
        bodyForceFAST[sphereCells[iturb][m]] = vector::zero;
        // For each blade.
        forAll(bladeForce[iturb], j)
        {
          // For each blade point.
          forAll(bladeForce[iturb][j], k)
          {
            scalar dis = mag(mesh_.C()[sphereCells[iturb][m]] - bladePoints[iturb][j][k]);
            if (dis <= smearRadius)
            {
              bodyForceFAST[sphereCells[iturb][m]] += bladeForce[iturb][j][k] * (Foam::exp(-Foam::sqr(dis/epsilon))/(Foam::pow(epsilon,3)*Foam::pow(Foam::constant::mathematical::pi,1.5)));
            }
          }
        }  
      }
    }

}

volVectorField& horizontalAxisWindTurbinesFAST::force()
{
    // Return the body force field to the solver
    return bodyForceFAST;
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace turbineModels
} // End namespace Foam

// ************************************************************************* //

