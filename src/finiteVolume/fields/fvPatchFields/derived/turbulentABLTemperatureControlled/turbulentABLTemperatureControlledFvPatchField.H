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
    Foam::turbulentABLTemperatureControlledFvPatchField

Group
    grpGenericBoundaryConditions

Description
    An inflow boundary condition for temperature.  It imposes a temperature
    profile with a capping inversion.  The entire profile below the inversion
    can heat up at a certain rate.  Also, temperature fluctuations can be
    applied to aid in the spin-up of turbulence.


        type              turbulentABLTemperatureControlled;
        print             false;
        zInversion        700.0;                        Inversion height
        widthInversion    100.0;                        Inversion width
        TInvBelow         300.0;                        T just below inversion
        TInvAbove         308.0;                        T just above inversion
        TSurface          300.5;                        T at first cell above surface
        TSurfRate         2.7778E-4;                    Rate at which the entire profile below the inversion changes
        TGradBelow        0.0;                          Gradient of temperature below inversion
        TGradAbove        0.003;			Gradient of temperature above inversion
        alphaTurbulent    0.25;				Relaxation factor on application of temperature fluctuations
        fluctScale        0.003;			Scale of fluctuations relative to mean temperature at particular patch face
        fluctPeakZ        60.0;				Height (m) above surface of peak fluctuations
        turbField         uniform 0;			Initial turbulent field
        value             uniform $T0;			Initial T field


SeeAlso
    Foam::fixedValueFvPatchField

SourceFiles
    turbulentABLTemperatureControlledFvPatchField.C

\*---------------------------------------------------------------------------*/

#ifndef turbulentABLTemperatureControlledFvPatchField_H
#define turbulentABLTemperatureControlledFvPatchField_H

#include "fixedValueFvPatchFields.H"
#include "Random.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

/*---------------------------------------------------------------------------*\
                  Class turbulentABLTemperatureControlledFvPatchField Declaration
\*---------------------------------------------------------------------------*/

class turbulentABLTemperatureControlledFvPatchField
:
    public fixedValueFvPatchScalarField
{

protected:

    // Protected data

        bool printOn_;
        scalar zInversion_;
        scalar widthInversion_;
        scalar TInvBelow_;
        scalar TInvAbove_;
        scalar TSurface_;
        scalar TSurfRate_;
        scalar TGradBelow_;
        scalar TGradAbove_;
        scalar alphaTurbulent_;
        scalar fluctScale_;
        scalar zPeak_;
        label curTimeIndex_;
        label counter_;
        Field<scalar> turbField_;
        Field<scalar> turbFieldOld_;
        Random ranGen_;
        scalar zInvBottom_;
        scalar zInvTop_;
        scalar TGradInv_;
        scalar TSurfaceOld_;

public:

    //- Runtime type information
    TypeName("turbulentABLTemperatureControlled");


    // Constructors

        //- Construct from patch and internal field
        turbulentABLTemperatureControlledFvPatchField
        (
            const fvPatch&,
            const DimensionedField<scalar, volMesh>&
        );

        //- Construct from patch, internal field and dictionary
        turbulentABLTemperatureControlledFvPatchField
        (
            const fvPatch&,
            const DimensionedField<scalar, volMesh>&,
            const dictionary&
        );

        //- Construct by mapping given turbulentABLTemperatureControlledFvPatchField
        //  onto a new patch
        turbulentABLTemperatureControlledFvPatchField
        (
            const turbulentABLTemperatureControlledFvPatchField&,
            const fvPatch&,
            const DimensionedField<scalar, volMesh>&,
            const fvPatchFieldMapper&
        );

        //- Construct as copy
        turbulentABLTemperatureControlledFvPatchField
        (
            const turbulentABLTemperatureControlledFvPatchField&
        );

        //- Construct and return a clone
        virtual tmp<fvPatchScalarField> clone() const
        {
            return tmp<fvPatchScalarField>
            (
                new turbulentABLTemperatureControlledFvPatchField(*this)
            );
        }

        //- Construct as copy setting internal field reference
        turbulentABLTemperatureControlledFvPatchField
        (
            const turbulentABLTemperatureControlledFvPatchField&,
            const DimensionedField<scalar, volMesh>&
        );

        //- Construct and return a clone setting internal field reference
        virtual tmp<fvPatchScalarField> clone
        (
            const DimensionedField<scalar, volMesh>& iF
        ) const
        {
            return tmp<fvPatchScalarField>
            (
                new turbulentABLTemperatureControlledFvPatchField(*this, iF)
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
