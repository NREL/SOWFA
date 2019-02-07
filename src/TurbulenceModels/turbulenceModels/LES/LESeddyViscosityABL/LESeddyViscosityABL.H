/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2013-2016 OpenFOAM Foundation
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
    Foam::LESModels::LESeddyViscosityABL

Group
    grpLESTurbulence

Description
    Atmospheric-boundary-layer-specific eddy viscosity LES SGS model base class.
    It includes an atmospheric-stability-dependent length scale recommended by
    Deardorff 

    Reference:
    \verbatim
        Deardorff, J. W. (1980),
        Stratocumulus-capped mixed layers derived from a three-dimensional model,
        Boundary Layer Meteorology, 18, 495-527.
    \endverbatim

SourceFiles
    LESeddyViscosityABL.C

\*---------------------------------------------------------------------------*/

#ifndef LESeddyViscosityABL_H
#define LESeddyViscosityABL_H

#include "LESModel.H"
#include "eddyViscosity.H"
#include "uniformDimensionedFields.H"
#include "IOdictionary.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace LESModels
{

/*---------------------------------------------------------------------------*\
                      Class LESeddyViscosityABL Declaration
\*---------------------------------------------------------------------------*/

template<class BasicTurbulenceModel>
class LESeddyViscosityABL
:
    public eddyViscosity<LESModel<BasicTurbulenceModel>>
{
    // Private Member Functions

        //- No copy construct
        LESeddyViscosityABL(const LESeddyViscosityABL&) = delete;

        //- No copy assignment
        void operator=(const LESeddyViscosityABL&) = delete;


protected:

    // Protected data

        dimensionedScalar Ce_;

        volScalarField l_;

        word TName_;

        word kappatName_;

        const volScalarField& T_;

        const uniformDimensionedVectorField& g_;

        IOdictionary transportProperties_;

        dimensionedScalar TRef_;


public:

    typedef typename BasicTurbulenceModel::alphaField alphaField;
    typedef typename BasicTurbulenceModel::rhoField rhoField;
    typedef typename BasicTurbulenceModel::transportModel transportModel;


    // Constructors

        //- Construct from components
        LESeddyViscosityABL
        (
            const word& type,
            const alphaField& alpha,
            const rhoField& rho,
            const volVectorField& U,
            const surfaceScalarField& alphaRhoPhi,
            const surfaceScalarField& phi,
            const transportModel& transport,
            const word& propertiesName = turbulenceModel::propertiesName
        );


    //- Destructor
    virtual ~LESeddyViscosityABL()
    {}


    // Member Functions

        //- Read model coefficients if they have changed
        virtual bool read();

        //- Return sub-grid dissipation rate
        virtual tmp<volScalarField> epsilon() const;

        //- Compute the stability-dependent length scale
        virtual void computeLengthScale();
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace LESModels
} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#ifdef NoRepository
    #include "LESeddyViscosityABL.C"
#endif

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
