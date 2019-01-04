/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011-2015 OpenFOAM Foundation
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
    Foam::timeVaryingMappedInletOutletFvPatchField

Group
    grpInletBoundaryConditions grpCoupledBoundaryConditions

Description
    This boundary condition is like inletOutlet in which if the flow is
    outflow, it is set to zero gradient; if the flow is inflow, the
    value is set.  Rather than set it to a single value as with inletOutlet,
    the value is set as timeVaryingMappedFixedValue would set the value
    using a saved time history of spatially varying data.  This is of 
    use in atmospheric simulations in which some boundaries may have
    inflow and outflow faces, and the inflow comes from pregenerated,
    saved turbulent flow.

    As with timeVaryingMappedFixedValue, on inflow faces, this boundary
    condition interpolates the values from a set of supplied
    points in space and time.  Supplied data should be specified in
    constant/boundaryData/\<patchname\> where:
    - points : pointField with locations
    - ddd    : supplied values at time ddd
    The default mode of operation (mapMethod planarInterpolation) is
    to project the points onto a plane (constructed from the first threee
    points) and construct a 2D triangulation and finds for the face centres
    the triangle it is in and the weights to the 3 vertices.

    The optional mapMethod nearest will avoid all projection and
    triangulation and just use the value at the nearest vertex.

    Inflow values are interpolated linearly between times.

    \heading Patch usage

    \table
        Property     | Description             | Required    | Default value
        setAverage   | flag to activate setting of average value | yes |
        perturb      | perturb points for regular geometries | no | 1e-5
        fieldTableName | alternative field name to sample | no| this field name
        mapMethod    | type of mapping | no | planarInterpolation
        offset   | for applying offset to mapped values  | no | constant 0.0
    \endtable

    \verbatim
    myPatch
    {
        type            timeVaryingMappedInletOutlet;
        setAverage      false;
        //perturb       0.0;
        //fieldTableName samples;
        //offset    constant 0.2;
    }
    \endverbatim

SeeAlso
    Foam::fixedValueFvPatchField

SourceFiles
    timeVaryingMappedInletOutletFvPatchField.C

\*---------------------------------------------------------------------------*/

#ifndef timeVaryingMappedInletOutletFvPatchField_H
#define timeVaryingMappedInletOutletFvPatchField_H

//#include "fixedValueFvPatchFields.H"
#include "mixedFvPatchFields.H"
#include "FixedList.H"
#include "instantList.H"
#include "pointToPointPlanarInterpolation.H"
#include "DataEntry.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

/*---------------------------------------------------------------------------*\
          Class timeVaryingMappedInletOutletFvPatchField Declaration
\*---------------------------------------------------------------------------*/

template<class Type>
class timeVaryingMappedInletOutletFvPatchField
:
  //public fixedValueFvPatchField<Type>
    public mixedFvPatchField<Type>
{

protected:

    // Protected data

        //- Name of flux field
        word phiName_;


private:
    // Private data

        //- Name of the field data table, defaults to the name of the field
        word fieldTableName_;

        //- If true adjust the mapped field to maintain average value
        bool setAverage_;

        //- Fraction of perturbation (fraction of bounding box) to add
        scalar perturb_;

	//- Does this boundary fix values so that they can't be adjusted by adjustPhi
	bool fixesValue_;

        //- Interpolation scheme to use
        word mapMethod_;

        //- 2D interpolation (for 'planarInterpolation' mapMethod)
        autoPtr<pointToPointPlanarInterpolation> mapperPtr_;

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

        //- Time varying offset values to interpolated data
        autoPtr<DataEntry<Type> > offset_;


public:

    //- Runtime type information
    TypeName("timeVaryingMappedInletOutlet");


    // Constructors

        //- Construct from patch and internal field
        timeVaryingMappedInletOutletFvPatchField
        (
            const fvPatch&,
            const DimensionedField<Type, volMesh>&
        );

        //- Construct from patch, internal field and dictionary
        timeVaryingMappedInletOutletFvPatchField
        (
            const fvPatch&,
            const DimensionedField<Type, volMesh>&,
            const dictionary&
        );

        //- Construct by mapping given timeVaryingMappedInletOutletFvPatchField
        //  onto a new patch
        timeVaryingMappedInletOutletFvPatchField
        (
            const timeVaryingMappedInletOutletFvPatchField<Type>&,
            const fvPatch&,
            const DimensionedField<Type, volMesh>&,
            const fvPatchFieldMapper&
        );

        //- Construct as copy
        timeVaryingMappedInletOutletFvPatchField
        (
            const timeVaryingMappedInletOutletFvPatchField<Type>&
        );

        //- Construct and return a clone
        virtual tmp<fvPatchField<Type> > clone() const
        {
            return tmp<fvPatchField<Type> >
            (
                new timeVaryingMappedInletOutletFvPatchField<Type>(*this)
            );
        }

        //- Construct as copy setting internal field reference
        timeVaryingMappedInletOutletFvPatchField
        (
            const timeVaryingMappedInletOutletFvPatchField<Type>&,
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
                new timeVaryingMappedInletOutletFvPatchField<Type>(*this, iF)
            );
        }


    // Member functions

        // Access

	    //- Returns true if this patch fixes a value.  This is used for
	    //  setting a reference value for a Poisson solve (pressure),
	    //  and for determining if adjustPhi affects the flux here.
	    virtual bool fixesValue() const
            {
	        return fixesValue_;
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


    // Member operators
    virtual void operator=(const fvPatchField<Type>& pvf);
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#ifdef NoRepository
#   include "timeVaryingMappedInletOutletFvPatchField.C"
#endif

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
