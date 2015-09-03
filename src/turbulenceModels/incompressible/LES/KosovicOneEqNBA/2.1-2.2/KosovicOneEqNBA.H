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
    Foam::incompressible::LESModels::KosovicOneEqNBA

Description
    Kosovic's one-equation nonlinear anisotropic backscatter SGS model.

    Kosovic, B. and Curry J., "A Large Eddy Simulation Study of a Quasi-Steady,
    Stably Stratified Atmospheric Boundary Layer," Journal of the Atmospheric
    Sciences, Vol 57, pp. 1052--1068, 2000.

SourceFiles
    KosovicOneEqNBA.C

\*---------------------------------------------------------------------------*/

#ifndef KosovicOneEqNBA_H
#define KosovicOneEqNBA_H

#include "LESModel.H"
#include "uniformDimensionedFields.H"
#include "IOdictionary.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace incompressible
{
namespace LESModels
{

/*---------------------------------------------------------------------------*\
                      Class KosovicOneEqNBA Declaration
\*---------------------------------------------------------------------------*/

class KosovicOneEqNBA
:
    public LESModel
{

protected:

    // Protected data

        // Model coefficients

            dimensionedScalar cb_;
            dimensionedScalar cs_;
            dimensionedScalar ce_;
            dimensionedScalar ceps_;
            dimensionedScalar Ske_;
            dimensionedScalar c1_;
            dimensionedScalar c2_;


        // Fields

            volScalarField k_;
            volScalarField nuSgs_;
            volSymmTensorField nonlinearStress_;
            volScalarField leps_;
            volScalarField ln_;
            volScalarField ls_;

      
        // Other model parameters or needed information.

            word TName_;
            word kappatName_;
            const volScalarField& T_;
            const uniformDimensionedVectorField& g_;
            IOdictionary transportDict_;
            dimensionedScalar TRef_;


public:

    //- Runtime type information
    TypeName("KosovicOneEqNBA");

    // Constructors

        //- Construct from components
        KosovicOneEqNBA
        (
            const volVectorField& U,
            const surfaceScalarField& phi,
            transportModel& transport,
            const word& turbulenceModelName = turbulenceModel::typeName,
            const word& modelName = typeName
        );


    //- Destructor
    virtual ~KosovicOneEqNBA()
    {}


    // Member Functions
    
        //- Return SGS kinetic energy
        virtual tmp<volScalarField> k() const
        {
            return k_;
        }

        //- Return sub-grid disipation rate
        virtual tmp<volScalarField> epsilon() const
        {
            return ceps_*k()*sqrt(k())/leps_;
        }

        //- Return the SGS viscosity
        virtual tmp<volScalarField> nuSgs() const
        {
            return nuSgs_;
        }

        //- Return the effective diffusivity for k
        tmp<volScalarField> DkEff() const
        {
            return tmp<volScalarField>
            (
                new volScalarField("DkEff", nuSgs_ + nu())
            );
        }

        //- Return the sub-grid stress tensor.
        virtual tmp<volSymmTensorField> B() const;

        //- Return the effective sub-grid turbulence deviatoric stress
        //  tensor including the laminar stress
        virtual tmp<volSymmTensorField> devBeff() const;

        //- Return the divergence of the deviatoric part of the effective
        //  sub-grid turbulence stress tensor including the laminar stress
        virtual tmp<fvVectorMatrix> divDevBeff(volVectorField& U) const;

        //- Correct the stability-dependent length scales
        virtual void computeLengthScales();

        //- Correct eddy-viscosity and related properties
        virtual void correct(const tmp<volTensorField>& gradU);

        //- Read LESProperties dictionary
        virtual bool read();
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace LESModels
} // End namespace incompressible
} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
