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
    Foam::timeVaryingMappedFluctuatingFixedValueFvPatchField

Description
    Foam::timeVaryingMappedFluctuatingFixedValueFvPatchField

    This is a modification of the standardTimeVaryingMappedFixedValue boundary
    condition that applies random fluctuations to the inflow field.  It applies
    the fluctuations to groups of cells, rather than each cell individually to
    give some spatial correlation.  The fluctuation is held constant over a
    specified time duration to give a temporal correlation.

    Interpolates from a set of supplied points in space and time. Supplied
    data in constant/boundaryData/<patchname>:
        - points : pointField with locations
        - ddd    : supplied values at time ddd
    Points need to be more or less on a plane since get triangulated in 2D.

    At startup this bc does the triangulation and determines linear
    interpolation (triangle it is in and weights to the 3 vertices)
    for every face centre. Interpolates linearly inbetween times.

    This modification to the standard timeVary

    @verbatim
        inlet
        {
            type            timeVaryingMappedFluctuatingFixedValue;

            // Maintain average to that of the supplied values
            setAverage             false;

            // Apply a rotation of direction of inflow
            rotateInflow           false;
            rotationAxis           (0 0 1);
            rotationAngle          15.0;

            // Optional: change perturbation (default 1E-5) to avoid any ties
            // in triangulating regular geometries.
            //perturb              0.0;
            
            // Period of time before fluctuations update
            fluctUpdatePeriod      5.0;

            // Fluctuation magnitude (fluctuations will be applied as 
            // +/-0.5*fluctMag
            fluctMag               1.0;

            // Fluctuation horizontal and vertical resolution.  Individual
            // fluctuations are applied over multiple cells to create groups
            // of cells with the same fluctuation level
            fluctResHoriz          30.0;
            fluctResVert           30.0;

            // The fluctuations may decay with height following a function.
            // The choices are:
            //     "constant"    - fluctuations are applied without decay up to 
            //                     the fluctVertDecayHeight level
            //     "exp"         - fluctuations are applied with the form 
            //                     1.65 * z/zPeak * exp(-0.5*(z/zPeak)^2)
            //                     with zPeak at fluctVertDecayHeight
            //     "expConstant" - constant up to fluctVertDecayHeight, then
            //                     follows the exponential decay of above
            fluctVertDecayType     "constant";

            // The height parameter for the vertical decay function
            fluctVertDecayHeight   100.0;

            // Optional: the last time at which the fluctuations were updated
            //fluctUpdateTimeLast  0.0;
            
            // Optional: the fluctuation field
            //fluctField           list of values for every patach face

            // Optional: use name instead of patchname for location of data
            //fieldTableName       samples;
        }
    @endverbatim

    Switch on debug flag to have it dump the triangulation (in transformed
    space) and transform face centres.

SourceFiles
    timeVaryingMappedFluctuatingFixedValueFvPatchField.C

\*---------------------------------------------------------------------------*/

#ifndef timeVaryingMappedFluctuatingFixedValueFvPatchField_H
#define timeVaryingMappedFluctuatingFixedValueFvPatchField_H

#include "fixedValueFvPatchFields.H"
#include "coordinateSystem.H"
#include "FixedList.H"
#include "instantList.H"
#include "Random.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

/*---------------------------------------------------------------------------*\
              Class timeVaryingMappedFluctuatingFixedValueFvPatch Declaration
\*---------------------------------------------------------------------------*/

template<class Type>
class timeVaryingMappedFluctuatingFixedValueFvPatchField
:
    public fixedValueFvPatchField<Type>
{
    // Private data

        //- Name of the field data table, defaults to the name of the field
        word fieldTableName_;

        //- If true adjust the mapped field to maintain average value
        bool setAverage_;

        //- Fraction of perturbation of geometry (fraction of bounding box) to add
        scalar perturb_;



        //- Additions to allow for rotation of inflow field. - MJC - 10 Sept 2014

        //- If true, rotate the inflow angle
        bool rotateInflow_;

        //- If rotating the inflow, give the axis about which rotatio is applied
        vector rotationAxis_;

        //- If rotating the inflow, give the rotation angle
        scalar rotationAngle_;




        //- Additions to add fluctuations to inflow field. - MJC - 3 June 2014

        //- Fluctuation time scale (how long before a new fluctuation is added)
        scalar fluctUpdatePeriod_;

        //- Type of vertical decay function for fluctuations
        //    Choice are:
        //       "constant" - constant up to fluctVertDecayHeight
        //       "exp" - of the form 1.65 * z/zPeak * exp(-0.5*(z/zPeak)^2) where zPeak is at fluctVertDecayHeight
        //       "expConst" - constant up to fluctVertCecayHeight, then follows exponential form of above
        word fluctVertDecayType_;

        //- Height parameter for vertical decay function of fluctuations
        scalar fluctVertDecayHeight_;

        //- Fluctuation magnitude (field will have fluctuations +/- 0.5*fluctMag)
        Type fluctMag_;

        //- Fluctuation space scale (how wide and tall are pockets of constant fluctuation)
        scalar fluctResVert_;
        scalar fluctResHoriz_;

        //- Time of last update of fluctuations
        scalar fluctUpdateTimeLast_;

        //- Fluctuation field
        Field<Type> fluctField_;

        //- Random number generator
        Random fluctRanGen_;






        //- Coordinate system
        autoPtr<coordinateSystem> referenceCS_;

        //- Current interpolation addressing to face centres of underlying
        //  patch
        List<FixedList<label, 3> > nearestVertex_;

        //- Current interpolation factors to face centres of underlying
        //  patch
        List<FixedList<scalar, 3> > nearestVertexWeight_;

        //- List of boundaryData time directories
        instantList sampleTimes_;

        //- Current starting index in sampleTimes
        label startSampleTime_;

        //- Interpolated values from startSampleTime
        Field<Type> startSampledValues_;

        //- If setAverage: starting average value
        Type startAverage_;

        //- Current end index in sampleTimes
        label endSampleTime_;

        //- Interpolated values from endSampleTime
        Field<Type> endSampledValues_;

        //- If setAverage: end average value
        Type endAverage_;


    // Private Member Functions

        //- Get names of times
        static wordList timeNames(const instantList&);

        //- Find times around current time
        void findTime
        (
            const fileName& instance,
            const fileName& local,
            const scalar timeVal,
            label& lo,
            label& hi
        ) const;


        //- Read boundary points and determine interpolation weights to patch
        //  faceCentres
        void readSamplePoints();

        //- Do actual interpolation using current weights
        tmp<Field<Type> > interpolate(const Field<Type>&) const;


public:

    //- Runtime type information
    TypeName("timeVaryingMappedFluctuatingFixedValue");


    // Constructors

        //- Construct from patch and internal field
        timeVaryingMappedFluctuatingFixedValueFvPatchField
        (
            const fvPatch&,
            const DimensionedField<Type, volMesh>&
        );

        //- Construct from patch, internal field and dictionary
        timeVaryingMappedFluctuatingFixedValueFvPatchField
        (
            const fvPatch&,
            const DimensionedField<Type, volMesh>&,
            const dictionary&
        );

        //- Construct by mapping given timeVaryingMappedFluctuatingFixedValueFvPatchField
        //  onto a new patch
        timeVaryingMappedFluctuatingFixedValueFvPatchField
        (
            const timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>&,
            const fvPatch&,
            const DimensionedField<Type, volMesh>&,
            const fvPatchFieldMapper&
        );

        //- Construct as copy
        timeVaryingMappedFluctuatingFixedValueFvPatchField
        (
            const timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>&
        );

        //- Construct and return a clone
        virtual tmp<fvPatchField<Type> > clone() const
        {
            return tmp<fvPatchField<Type> >
            (
                new timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>(*this)
            );
        }

        //- Construct as copy setting internal field reference
        timeVaryingMappedFluctuatingFixedValueFvPatchField
        (
            const timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>&,
            const DimensionedField<Type, volMesh>&
        );

        //- Construct and return a clone setting internal field reference
        virtual tmp<fvPatchField<Type> > clone
        (
            const DimensionedField<Type, volMesh>& iF
        ) const
        {
            return tmp<fvPatchField<Type> >
            (
                new timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>(*this, iF)
            );
        }


    // Member functions

        // Access

            //- Return the coordinateSystem
            const coordinateSystem& referenceCS() const
            {
                return referenceCS_;
            }

            //- Return startSampledValues
            const Field<Type> startSampledValues()
            {
                 return startSampledValues_;
            }


        // Mapping functions

            //- Map (and resize as needed) from self given a mapping object
            virtual void autoMap
            (
                const fvPatchFieldMapper&
            );

            //- Reverse map the given fvPatchField onto this fvPatchField
            virtual void rmap
            (
                const fvPatchField<Type>&,
                const labelList&
            );


        // Utility functions

            //- Find boundary data inbetween current time and interpolate
            void checkTable();


        // Evaluation functions

            //- Update the coefficients associated with the patch field
            virtual void updateCoeffs();


        //- Write
        virtual void write(Ostream&) const;
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#ifdef NoRepository
#   include "timeVaryingMappedFluctuatingFixedValueFvPatchField.C"
#endif

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
