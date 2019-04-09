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
    Foam::DrivingForce<Type>

Description
    Driving force

SourceFiles
    DrivingForce.C

\*---------------------------------------------------------------------------*/

#ifndef DrivingForce_H
#define DrivingForce_H

#include "fvCFD.H"
#include "IOdictionary.H"
#include "meshPlanes.H"
#include "interpolate2D.H"
#include "windRoseToCartesian.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// Forward declaration of classes

/*---------------------------------------------------------------------------*\
                           Class drivingForce Declaration
\*---------------------------------------------------------------------------*/

template<class Type>
class DrivingForce
{
    // Private data


        typedef GeometricField<Type, fvPatchField, volMesh> volFieldType;


        //- Constants
        
            //- Name
            const word name_;

            //- Runtime pointer
            const Time& runTime_;
    
            //- Mesh pointer
            const fvMesh& mesh_;
    
            //- Field pointer
            const volFieldType& field_;


        //- Vertical mesh planes
        meshPlanes zPlanes_;

        //- Driving force field
        volFieldType bodyForce_;

        //- Height level indices for forcing at one height
        label hLevels1I;
        label hLevels2I;

        //- Height levels for forcing at one height
        scalar hLevels1;
        scalar hLevels2;
        
        //- Output file pointer
        autoPtr<OFstream> sourceHistoryFile_;

        //- Type of source
        word sourceType_;

        //- Velocity input type (only used when name_ == "momentum")
        word velocityInputType_;

        //- Heights at which the sources are given
        List<scalar> sourceHeightsSpecified_;

        //- Times at which the sources are given
        List<scalar> sourceTimesSpecified_;

        //- Source interpolation table
        List<List<Type> > sourceSpecified_;

        //- Relaxation coefficient
        scalar alpha_;

        //- Gather/write statistics?
        bool statisticsOn_;

        //- Statistics gathering/writing frequency?
        int statisticsFreq_;


    // Private Member Functions


        //- Return zero object of type for initialisation
        Type zeroTensor_()
        {
            return Type::zero;
        }


        //- Read input data from ABLProperties dictionary
        void readInputData_();


        //- Read source tables (componentwise)
        void readSourceTables_
        (
            IOdictionary& ABLProperties,
            label& nSourceHeights
        );
            

        //- Check size of source table
        void checkSourceTableSize_
        (
            word& sourceTableName,
            List<List<scalar> >& sourceTable,
            label& nSourceHeights
        );

        //- Find grid levels closest to specified height
        void findSingleForcingHeight_();

        //- Open file streams
        void openFiles_();

        //- Update given time-dependent source term
        void updateGivenTimeDepSource_();

        //- Update given time-height-dependent source term
        void updateGivenTimeHeightDepSource_();

        //- Update computed time-dependent source term
        void updateComputedTimeDepSource_();

        //- Update computed time-height-dependent source term
        void updateComputedTimeHeightDepSource_();

        //- Initialize controller
        void initializeController_(label nSourceHeights);

        //- Update controller
        List<Type> updateController_(List<Type>& error);

        //- Write source history to file (single value or column of values)
        void writeSourceHistory_(Type& source);
        void writeSourceHistory_(List<Type>& source);

        //- Convert speedAndDirection input to components (only for Type vector)
        Type speedDirToComp_
        (
            Type desiredField
        );

public:

    //- Runtime type information
    ClassName("DrivingForce");


    // Constructors
    DrivingForce
    (
        const word& name,
        const volFieldType& field
    );


    // Destructor
    virtual ~DrivingForce();


    // Public Member functions

        //- Update source term
        void update();

        //- Return bodyforce
        volFieldType& force()
        {
            return bodyForce_;
        }

};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

// specialization for scalar driving force
template<>
scalar DrivingForce<scalar>::zeroTensor_()
{
    return 0.0;
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#ifdef NoRepository
#   include "DrivingForce.C"
#   include "DrivingForceController.C"
#endif

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
