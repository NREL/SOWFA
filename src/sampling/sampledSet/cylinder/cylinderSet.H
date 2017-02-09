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
    Foam::cylinderSet

Description
    Samples a regular array of points on a cylinder

SourceFiles
    cylinderSet.C

\*---------------------------------------------------------------------------*/

#ifndef cylinderSet_H
#define cylinderSet_H

#include "sampledSet.H"
#include "DynamicList.H"
#include "coordinateSystem.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// Forward declaration of classes
class passiveParticle;
template<class Type> class particle;

/*---------------------------------------------------------------------------*\
                           Class cylinderSet Declaration
\*---------------------------------------------------------------------------*/

class cylinderSet
:
    public sampledSet
{
    // Private data

        //- Origin (x, y, z) in global cartesian co-ordinates
        point origin1_;
        point origin2_;

        //- Radius of cylinder
        scalar radius_;

        //- Number of axial and azimuthal sampling points
        scalar dAxial_;
        scalar dAzimuthal_;


    // Private Member Functions

        //- Samples all points in sampleCoords.
        void calcSamples
        (
            DynamicList<point>& samplingPts,
            DynamicList<label>& samplingCells,
            DynamicList<label>& samplingFaces,
            DynamicList<label>& samplingSegments,
            DynamicList<scalar>& samplingCurveDist
        ) const;

        //- Uses calcSamples to obtain samples. Copies them into *this.
        void genSamples();


public:

    //- Runtime type information
    TypeName("cylinder");


    // Constructors

        //- Construct from components
        cylinderSet
        (
            const word& name,
            const polyMesh& mesh,
            const meshSearch& searchEngine,
            const word& axis,
            const point& origin1,
            const point& origin2,
            const scalar& radius,
            const scalar& dAxial,
            const scalar& dAzimuthal
        );

        //- Construct from dictionary
        cylinderSet
        (
            const word& name,
            const polyMesh& mesh,
            const meshSearch& searchEngine,
            const dictionary& dict
        );


    //- Destructor
    virtual ~cylinderSet();
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
