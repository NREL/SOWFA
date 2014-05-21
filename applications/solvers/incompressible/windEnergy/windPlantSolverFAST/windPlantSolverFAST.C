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

Application
    buoyantBoussinesqPimpleFoam

Description
    Transient solver for buoyant, turbulent flow of incompressible fluids

    Uses the Boussinesq approximation:
    \f[
        rho_{k} = 1 - beta(T - T_{ref})
    \f]

    where:
        \f$ rho_{k} \f$ = the effective (driving) kinematic density
        beta = thermal expansion coefficient [1/K]
        T = temperature [K]
        \f$ T_{ref} \f$ = reference temperature [K]

    Valid when:
    \f[
        rho_{k} << 1
    \f]

\*---------------------------------------------------------------------------*/

#include "fvCFD.H"
#include "singlePhaseTransportModel.H"
#include "turbulenceModel.H"
#include "pimpleControl.H"
#include "IFstream.H"
#include "OFstream.H"
//#include "horizontalAxisWindTurbinesALM.H"
#include "horizontalAxisWindTurbinesFAST.H"


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //
extern "C"
{
  void fastinit_( float& , int& );
  void fastread_( float*, float*, float*);
  void fastrun_( );
  void fastgetbldpos_( float*, float*, float*);
  void fastgetbldforce_(float*, float*, float*);
  void fastend_( );
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

int main(int argc, char *argv[])
{
    #include "setRootCase.H"
    #include "createTime.H"
    #include "createMesh.H"
    #include "readGravitationalAcceleration.H"
    #include "createFields.H"
    #include "createDivSchemeBlendingField.H"
    #include "createGradPd.H"
    #include "readTimeControls.H"
    #include "CourantNo.H"
    #include "setInitialDeltaT.H"
    //#include "findVerticalCellLevels.H"
    //#include "findVerticalFaceLevels.H"
    #include "findWindHeight.H"
    //#include "openCellStatisticsFiles.H"
    //#include "openFaceStatisticsFiles.H"
    //#include "openABLStatisticsFiles.H"
    #include "createAverageFields.H"


    // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //
    // Initialize FAST for each turbine
    Info << "Number of Turbines = " << turbfast.turbNum << endl;

    float tstep = runTime.deltaT().value();
    for(int turbNo=0; turbNo<turbfast.turbNum; turbNo++)
    {
      if(Pstream::myProcNo() == turbNo)
      {
        fastinit_(tstep, turbNo);
        fastgetbldpos_(turbfast.bldptx[turbNo], turbfast.bldpty[turbNo], turbfast.bldptz[turbNo]);
      }
      turbfast.getBldPos(turbNo);
    }

    // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //


    pimpleControl pimple(mesh);

    // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

    Info << nl << "Starting time loop\n" << endl;

    turbulence->correct();
    U.correctBoundaryConditions();


    while (runTime.loop())
    {
        Info << "Time = " << runTime.timeName() << tab;
        Info << "Time Step = " << runTime.timeIndex() << endl;

        #include "readTimeControls.H"
        #include "CourantNo.H"
        #include "setDeltaT.H"
        #include "updateDivSchemeBlendingField.H"


        // --- Pressure-velocity PIMPLE corrector loop
        for (pimple.start(); pimple.loop(); pimple++)
        {
            if (pimple.nOuterCorr() != 1)
            {
                p_rgh.storePrevIter();
            }

            // compute body force from FAST
            for(int turbNo=0; turbNo<turbfast.turbNum; turbNo++)
            {

              turbfast.getWndVec(turbNo);
              if(Pstream::myProcNo() == turbNo)
              {
                fastread_(turbfast.uin[turbNo], turbfast.vin[turbNo], turbfast.win[turbNo]);
                fastrun_();
                fastgetbldpos_(turbfast.bldptx[turbNo], turbfast.bldpty[turbNo], turbfast.bldptz[turbNo]);
                fastgetbldforce_(turbfast.bldfx[turbNo], turbfast.bldfy[turbNo], turbfast.bldfz[turbNo]);
              }
              turbfast.computeBodyForce(turbNo);
            }


            #include "UEqn.H"
            #include "TEqn.H"

            // --- Pressure corrector loop
            for (int corr=0; corr<pimple.nCorr(); corr++)
            {
                #include "pEqn.H"
                #include "TEqn.H"
            }

            // --- Compute the velocity flux divergence
            #include "computeDivergence.H"

            // --- Update the driving pressure gradient
            #include "correctGradPd.H"

            // --- Update the turbulence fields
            if (pimple.turbCorr())
            {
                turbulence->correct();
            }

            // --- Update the turbine array
            //turbines.update();

            // --- Update the boundary momentum and
            //     temperature flux conditions
            Rwall.correctBoundaryConditions();
            qwall.correctBoundaryConditions();
        }   


        #include "computeAverageFields.H"
//      if (runTime.outputTime())
//      {
//          #include "averageFields.H"
//      }

//      #include "statisticsCell.H"
//      #include "statisticsFace.H"
//      #include "statisticsABL.H"

        runTime.write();
        #include "writeGradPd.H"

        Info << "ExecutionTime = " << runTime.elapsedCpuTime() << " s"
             << "  ClockTime = " << runTime.elapsedClockTime() << " s"
             << nl << endl;
    }

    // terminate FAST for each turbine
    for(int turbNo=0; turbNo<turbfast.turbNum; turbNo++)
    {

      if(Pstream::myProcNo() == turbNo)
      {
        fastend_();
      }
    }


    Info << "End" << endl;

    return 0;
}


// ************************************************************************* //
