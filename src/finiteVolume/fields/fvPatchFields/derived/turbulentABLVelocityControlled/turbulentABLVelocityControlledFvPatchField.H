/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2012 OpenFOAM Foundation
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
    Foam::turbulentABLVelocityControlledFvPatchField

Group
    grpGenericBoundaryConditions

Description
    Inflow velocity boundary condition for atmospheric flows.  The condition
    applies a logarithmic profile up to a certain height (typically, the height
    of the inversion).  Velocity is sampled at a specified point in the flow,
    and the log profile is scaled to try to achieve that desired velocity
    at that point (for example if trying to match mean speed at a met mast).
    Random fluctuations can also be added to try to initiate turbulence.

        type            turbulentABLVelocityControlled;
        print           0;
        zConst          700;					Height above which velocity is constant
        z0              0.1;					Surface roughness
        refLoc          $refLoc0;				Velocity sampling location
        refU            $refU0;					Desired wind speed at sampling location
        avgWindow       1;					Size of backwards averaging window for velocity sampling
        alphaTurbulent  0.25;					Relaxation factor on fluctuation application each new time step
        alphaMean       0.1;					Relaxation factor on log profile scaling to match desired speed at refLoc
        fluctScale      (0.0 0.0 0.0);				Scale of fluctuations in each direction relative to velocity at particular patch face
        fluctPeakZ      100.0;					Height of maximum fluctuation amplitude
        uStar           0.5;					Initial friction velocity
        kappa           0.4;					von Karmann constant
        windDir         190;					Direction of wind that flows into domain
        turbField       uniform (0 0 0);			Initial turbulence field
        value           uniform $U0;				Initial velocity field


SeeAlso
    Foam::fixedValueFvPatchField

SourceFiles
    turbulentABLVelocityControlledFvPatchField.C

\*---------------------------------------------------------------------------*/

#ifndef turbulentABLVelocityControlledFvPatchField_H
#define turbulentABLVelocityControlledFvPatchField_H

#include "fixedValueFvPatchFields.H"
#include "Random.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

/*---------------------------------------------------------------------------*\
                  Class turbulentABLVelocityControlledFvPatchField Declaration
\*---------------------------------------------------------------------------*/

class turbulentABLVelocityControlledFvPatchField
:
    public fixedValueFvPatchVectorField
{

protected:

    // Protected data

        //- Print information
        bool printOn_;

        scalar zConst_;
        scalar z0_;

        vector refLoc_;
        scalar refU_;

        label avgWindow_;

        scalar alphaTurbulent_;
        scalar alphaMean_;

        vector fluctScale_;

        scalar zPeak_;
     
        scalar uStar_;
        scalar uStarOld_;
       
        scalar kappa_;
   
        scalar windDir_;
        scalar windAng_;

        //- Current time index
        label curTimeIndex_;
        label oldTimeIndex_;

        scalar minDis;
        label minDisCellID;

        label refLocControl_;
  
        label counter_;

        scalar uHistSum;

        DynamicList<scalar> uHist;

        Field<vector> turbField_;
        Field<vector> turbFieldOld_;

        Random ranGen_;



public:

    //- Runtime type information
    TypeName("turbulentABLVelocityControlled");


    // Constructors

        //- Construct from patch and internal field
        turbulentABLVelocityControlledFvPatchField
        (
            const fvPatch&,
            const DimensionedField<vector, volMesh>&
        );

        //- Construct from patch, internal field and dictionary
        turbulentABLVelocityControlledFvPatchField
        (
            const fvPatch&,
            const DimensionedField<vector, volMesh>&,
            const dictionary&
        );

        //- Construct by mapping given turbulentABLVelocityControlledFvPatchField
        //  onto a new patch
        turbulentABLVelocityControlledFvPatchField
        (
            const turbulentABLVelocityControlledFvPatchField&,
            const fvPatch&,
            const DimensionedField<vector, volMesh>&,
            const fvPatchFieldMapper&
        );

        //- Construct as copy
        turbulentABLVelocityControlledFvPatchField
        (
            const turbulentABLVelocityControlledFvPatchField&
        );

        //- Construct and return a clone
        virtual tmp<fvPatchVectorField> clone() const
        {
            return tmp<fvPatchVectorField>
            (
                new turbulentABLVelocityControlledFvPatchField(*this)
            );
        }

        //- Construct as copy setting internal field reference
        turbulentABLVelocityControlledFvPatchField
        (
            const turbulentABLVelocityControlledFvPatchField&,
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
                new turbulentABLVelocityControlledFvPatchField(*this, iF)
            );
        }


    // Member functions

        // Evaluation functions

            //- Update the coefficients associated with the patch field
            virtual void updateCoeffs();

        //- Write
        void write(Ostream&) const;
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
