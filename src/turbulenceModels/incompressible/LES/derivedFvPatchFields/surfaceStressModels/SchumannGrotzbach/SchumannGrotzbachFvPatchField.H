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
    Foam::incompressible::LESModels::SchumannGrotzbachFvPatchField

Description
    Wall boundary condition for total shear stress at a solid boundary.  
    Stresses are computed using the Schumann/Grotzbach formulation.  Can be
    used in either horizontal-average or local mode.

SourceFiles
    SchumannGrotzbachFvPatchField.C

\*---------------------------------------------------------------------------*/

#ifndef SchumannGrotzbachFvPatchField_H
#define SchumannGrotzbachFvPatchField_H

#include "fixedValueFvPatchFields.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{
namespace incompressible
{
namespace LESModels
{

/*---------------------------------------------------------------------------*\
     Class SchumannGrotzbachFvPatchField Declaration
\*---------------------------------------------------------------------------*/

class SchumannGrotzbachFvPatchField
:
    public fixedValueFvPatchSymmTensorField
{

private:

    // Private data

        //- Von Karman constant
        scalar kappa_;

        //- Roughness height
        scalarField z0_;

        //- Constant for unstable Monin-Obuhkov scaling law
        scalar betaM_;

        // -Constant for stable Monin-Obuhkov scaling law
        scalar gammaM_;

        // -Type of application of Monin-Obuhkov scaling, local or nonlocal
        word averageType_;

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

        // -Function to transform symmetric tensors from terrain-local to
        //  Cartesian coordinates
        symmTensor transformSymmTensorLocalToCart
        (
             symmTensor SP,
             vector xP,
             vector yP,
             vector zP
        );

        // -Function to calculate friction velocity using Monin-Obuhkov
        //  similarity laws
        void uStarEvaluate
        (
             scalar &uStar,
             scalar& L,
             scalar& phiM,
             scalar U,
             scalar z1,
             scalar z0,
             scalar kappa,
             scalar gammaM,
             scalar betaM,
             scalar g,
             scalar TRef,
             scalar qw,
             scalar eps,
             scalar tol,
             label iterMax
        );  


public:

    //- Runtime type information
    TypeName("SchumannGrotzbach");


    // Constructors

        //- Construct from patch and internal field
        SchumannGrotzbachFvPatchField
        (
            const fvPatch&,
            const DimensionedField<symmTensor, volMesh>&
        );

        //- Construct from patch, internal field and dictionary
        SchumannGrotzbachFvPatchField
        (
            const fvPatch&,
            const DimensionedField<symmTensor, volMesh>&,
            const dictionary&
        );

        //- Construct by mapping given
        //  stressABLWallFunctionFvPatchScalarField
        //  onto a new patch
        SchumannGrotzbachFvPatchField
        (
            const SchumannGrotzbachFvPatchField&,
            const fvPatch&,
            const DimensionedField<symmTensor, volMesh>&,
            const fvPatchFieldMapper&
        );

        //- Construct as copy
        SchumannGrotzbachFvPatchField
        (
            const SchumannGrotzbachFvPatchField&
        );

        //- Construct and return a clone
        virtual tmp<fvPatchSymmTensorField> clone() const
        {
            return tmp<fvPatchSymmTensorField>
            (
                new SchumannGrotzbachFvPatchField(*this)
            );
        }

        //- Construct as copy setting internal field reference
        SchumannGrotzbachFvPatchField
        (
            const SchumannGrotzbachFvPatchField&,
            const DimensionedField<symmTensor, volMesh>&
        );

        //- Construct and return a clone setting internal field reference
        virtual tmp<fvPatchSymmTensorField> clone
        (
            const DimensionedField<symmTensor, volMesh>& iF
        ) const
        {
            return tmp<fvPatchSymmTensorField>
            (
                new SchumannGrotzbachFvPatchField
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

} // End namespace LESModels
} // End namespace incompressible
} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
