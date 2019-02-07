/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011-2016 OpenFOAM Foundation
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
    Foam::LESModels::kEqnABL

Group
    grpLESTurbulence

Description
    One equation eddy-viscosity model for atmospheric-boundary-layer LES
    given by Deardorff (1980) and Moeng (1984).

    Reference:
    \verbatim
        Moeng, C.-H. (1986).
        A large-eddy-simulation Model for the study of planetary boundary layer turbulence
        Journal of the Atmospheric Sciences, 41(13), 2052-2062.
    \endverbatim

    \verbatim
        Deardorff, J. W. (1980),
        Stratocumulus-capped mixed layers derived from a three-dimensional model,
        Boundary Layer Meteorology, 18, 495-527.
    \endverbatim

    The default model coefficients are:
       Ck = 0.1
       Ce = 0.19 + (0.51*l/delta), 3.9 at lowest grid level

SourceFiles
    kEqnABL.C

\*---------------------------------------------------------------------------*/

#ifndef kEqnABL_H
#define kEqnABL_H

#include "LESeddyViscosityABL.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace LESModels
{

/*---------------------------------------------------------------------------*\
                           Class kEqnABL Declaration
\*---------------------------------------------------------------------------*/

template<class BasicTurbulenceModel>
class kEqnABL
:
    public LESeddyViscosityABL<BasicTurbulenceModel>
{
protected:

    // Protected data

        // Fields
            // subgrid-scale turbulent kinetic energy.
            volScalarField k_;

        // Model constants
            // Ck
            dimensionedScalar Ck_;

        // Model parameters
            // Ce field
            volScalarField CeField_;


    // Protected Member Functions

        //- No copy construct
        kEqnABL(const kEqnABL&) = delete;

        //- No copy assignment
        void operator=(const kEqnABL&) = delete;


        virtual void correctNut();
        virtual tmp<fvScalarMatrix> kSource() const;


public:

    typedef typename BasicTurbulenceModel::alphaField alphaField;
    typedef typename BasicTurbulenceModel::rhoField rhoField;
    typedef typename BasicTurbulenceModel::transportModel transportModel;


    //- Runtime type information
    TypeName("kEqnABL");


    // Constructors

        //- Constructor from components
        kEqnABL
        (
            const alphaField& alpha,
            const rhoField& rho,
            const volVectorField& U,
            const surfaceScalarField& alphaRhoPhi,
            const surfaceScalarField& phi,
            const transportModel& transport,
            const word& propertiesName = turbulenceModel::propertiesName,
            const word& type = typeName
        );


    //- Destructor
    virtual ~kEqnABL()
    {}


    // Member Functions

        //- Read model coefficients if they have changed
        virtual bool read();

        //- Return subgrid-scale turbulent kinetic energy
        virtual tmp<volScalarField> k() const
        {
            return k_;
        }

        //- Return sub-grid dissipation rate
        virtual tmp<volScalarField> epsilon() const;

        //- Return the effective diffusivity for k
        tmp<volScalarField> DkEff() const
        {
            return tmp<volScalarField>
            (
                new volScalarField("DkEff", this->nut_ + this->nu())
            );
        }

        //- Correct eddy-viscosity and related properties
        virtual void correct();
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace LESModels
} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#ifdef NoRepository
    #include "kEqnABL.C"
#endif

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
