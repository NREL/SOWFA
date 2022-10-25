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

Class
    Foam::incompressible::LESModels::fixedHeatingRateFvPatchField

Description
    Wall boundary condition for temperature flux at a solid boundary
    to acheive a specified cooling/heating rate.  Can be used in either
    horizontal-average or local mode.

SourceFiles
    fixedHeatingRateFvPatchField.C

\*---------------------------------------------------------------------------*/

#ifndef fixedHeatingRateFvPatchField_H
#define fixedHeatingRateFvPatchField_H

#include "fixedValueFvPatchFields.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

/*---------------------------------------------------------------------------*\
     Class fixedHeatingRateFvPatchField Declaration
\*---------------------------------------------------------------------------*/

class fixedHeatingRateFvPatchField
:
    public fixedValueFvPatchVectorField
{

private:

    // Private data
        //- Heating/heating rate (cooling negative, heating positive)
        scalarField heatingRate_;

        //- Surface temperature
        scalarField TSurface_;

        //- Von Karman constant
        scalar kappa_;

        //- Roughness height
        scalarField z0_;

        //- Constant for unstable Monin-Obuhkov velocity scaling law
        scalar betaM_;

        // -Constant for stable Monin-Obuhkov velocity scaling law
        scalar gammaM_;

        //- Constant for unstable Monin-Obuhkov temperature scaling law
        scalar betaH_;

        // -Constant for stable Monin-Obuhkov temperature scaling law
        scalar gammaH_;

        // -Constant for Monin-Obuhkov temperature scalaing law
        scalar alphaH_;
        
        // Constants for Beljaars equation
        scalar a_;
        scalar b_;
        scalar c_;
        scalar d_;
	
	// Turn on nonlinear Beljaars stability functions
	bool nonlinear_;

        // -Type of application of Monin-Obuhkov scaling, local or nonlocal
        word averageType_;

        // -Last time the surface temperature was modified
        scalar tLast_;

    // Private member functions
       
        // -Function to transform vectors from Cartesian into terrain-local
        //  coordinates
        vector transformVectorCartToLocal
        (
             vector v,
             vector xP,
             vector yP,
             vector zP
        );

        // -Function to transform vectors from terrain-local to Cartesian
        //  coordinates
        vector transformVectorLocalToCart
        (
             vector vP,
             vector xP,
             vector yP,
             vector zP
        );

        // -Function to calculate friction velocity using Monin-Obuhkov
        //  similarity laws
        void qwEvaluate
        (
             scalar& qw,
             scalar& uStar,
             scalar& L,
             scalar& phiM,
             scalar& phiH,
             scalar U,
             scalar z1,
             scalar z0,
             scalar kappa,
             scalar gammaM,
             scalar betaM,
             scalar gammaH,
             scalar betaH,
             scalar alphaH,
             scalar g,
             scalar TRef,
             scalar deltaT,
             scalar eps,
             scalar tol,
             label iterMax
        );  


public:

    //- Runtime type information
    TypeName("fixedHeatingRate");


    // Constructors

        //- Construct from patch and internal field
        fixedHeatingRateFvPatchField
        (
            const fvPatch&,
            const DimensionedField<vector, volMesh>&
        );

        //- Construct from patch, internal field and dictionary
        fixedHeatingRateFvPatchField
        (
            const fvPatch&,
            const DimensionedField<vector, volMesh>&,
            const dictionary&
        );

        //- Construct by mapping given
        //  stressABLWallFunctionFvPatchScalarField
        //  onto a new patch
        fixedHeatingRateFvPatchField
        (
            const fixedHeatingRateFvPatchField&,
            const fvPatch&,
            const DimensionedField<vector, volMesh>&,
            const fvPatchFieldMapper&
        );

        //- Construct as copy
        fixedHeatingRateFvPatchField
        (
            const fixedHeatingRateFvPatchField&
        );

        //- Construct and return a clone
        virtual tmp<fvPatchVectorField> clone() const
        {
            return tmp<fvPatchVectorField>
            (
                new fixedHeatingRateFvPatchField(*this)
            );
        }

        //- Construct as copy setting internal field reference
        fixedHeatingRateFvPatchField
        (
            const fixedHeatingRateFvPatchField&,
            const DimensionedField<vector, volMesh>&
        );

        //- Construct and return a clone setting internal field reference
        virtual tmp<fvPatchVectorField> clone
        (
            const DimensionedField<vector, volMesh>& iF
        ) const
        {
            return tmp<fvPatchVectorField>
            (
                new fixedHeatingRateFvPatchField
                (
                    *this,
                    iF
                )
            );
        }


    // Member functions

        // Access functions
            //- Return z_0
            scalarField& z0()
            {
                return z0_;
            }

        // Evaluation functions

            //- Evaluate the patchField
            virtual void evaluate
            (
                const Pstream::commsTypes commsType=Pstream::blocking
            );


        // I-O

            //- Write
            void write(Ostream&) const;
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
