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

Class
    Foam::buoyancyModel

Description
    Buoyancy model
    Currently only Boussinesq

SourceFiles
    buoyancyModel.C

\*---------------------------------------------------------------------------*/

#ifndef buoyancyModel_H
#define buoyancyModel_H

#include "fvCFD.H"
#include "IOdictionary.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// Forward declaration of classes

/*---------------------------------------------------------------------------*\
                           Class buoyancyModel Declaration
\*---------------------------------------------------------------------------*/

class buoyancyModel
{
    // Private data

        //- Constants
            //- Runtime pointer
            const Time& runTime_;;
    
            //- Mesh pointer
            const fvMesh& mesh_;
    
            //- Temperature field pointer
            const volScalarField& T_;

            //- Reference temperature [K]
            const dimensionedScalar TRef_;

        //- Gravitational acceleration field
        uniformDimensionedVectorField g_;

        //- Boussinesq density field
        volScalarField rhok_;

        //- Gravioty potential field
        volScalarField gh_;
        surfaceScalarField ghf_;

        //- Background pressure field
        volScalarField pBackground_;

        //- Buoyancy term
        surfaceScalarField buoyancyTerm_;

        //- Definition of the background hydrostatic pressure
        word backgroundPressureType_;


public:

    //- Declare name of the class and its debug switch
    ClassName("buoyancyModel");


    // Constructors
    buoyancyModel
    (
        const volScalarField& T,
        const dimensionedScalar TRef,
        const dimensionedVector hRef
    );


    // Destructor
    virtual ~buoyancyModel();


    // Public Member functions

        //- Update buoyancy term
        void updateBuoyancyTerm();

        //- Update background pressure
        void updateBackgroundPressure();

        //- Update density field 
        void updateDensityField();

        //- Return bakcground pressure
        volScalarField& backgroundPressure()
        {
            return pBackground_;
        }

        //- Return buoyancy term
        surfaceScalarField& buoyancyTerm()
        {
            return buoyancyTerm_;
        }


};

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
