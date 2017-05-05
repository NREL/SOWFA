/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011-2013 OpenFOAM Foundation
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

\*---------------------------------------------------------------------------*/

#include "cylinderSet.H"
#include "sampledSet.H"
#include "meshSearch.H"
#include "DynamicList.H"
#include "polyMesh.H"
#include "addToRunTimeSelectionTable.H"
#include "word.H"
#include "transform.H"

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

namespace Foam
{
    defineTypeNameAndDebug(cylinderSet, 0);
    addToRunTimeSelectionTable(sampledSet, cylinderSet, word);
}


// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //

void Foam::cylinderSet::calcSamples
(
    DynamicList<point>& samplingPts,
    DynamicList<label>& samplingCells,
    DynamicList<label>& samplingFaces,
    DynamicList<label>& samplingSegments,
    DynamicList<scalar>& samplingCurveDist
) const
{
    const meshSearch& queryMesh = searchEngine();

    const label nAxial = ceil((mag(origin2_ - origin1_))/dAxial_) + 1;
    const label nAzimuthal = ceil((2.0*Foam::constant::mathematical::pi*radius_)/dAzimuthal_) + 1;
    const scalar dTheta = 2.0*Foam::constant::mathematical::pi/(nAzimuthal - 1);

    label nTotalSamples(nAxial*nAzimuthal);

    List<point> sampleCoords(nTotalSamples);

    vector circleAxis = origin2_ - origin1_;
    circleAxis /= mag(circleAxis);

    vector radialAxis = vector::zero;
    radialAxis.x() =  circleAxis.y();
    radialAxis.y() = -circleAxis.x();
    radialAxis /= mag(radialAxis);

    Info << "nTotalSamples = " << nTotalSamples << endl;
    Info << "circleAxis = " << circleAxis << endl;
    Info << "radialAxis = " << radialAxis << endl;
    Info << "nAxial = " << nAxial << endl;
    Info << "nAzimuthal = " << nAzimuthal << endl;

    label p(0);
    vector origin = origin1_;
    for (label j = 0; j < nAxial; j++)
    {
        vector r = radialAxis;
        origin = origin1_ + (j*dAxial_)*circleAxis;
        for (label i = 0; i < nAzimuthal; i++)
        {
            r = r * cos(dTheta) + (r^circleAxis)*sin(dTheta);
            r /= mag(r);

            sampleCoords[p] = origin + radius_*r;
            
            p++;
        }
    }

    forAll(sampleCoords, sampleI)
    {
        label cellI = queryMesh.findCell(sampleCoords[sampleI]);

        if (cellI != -1)
        {
            samplingPts.append(sampleCoords[sampleI]);
            samplingCells.append(cellI);
            samplingFaces.append(-1);
            samplingSegments.append(0);
            samplingCurveDist.append(1.0 * sampleI);
        }
    }
}


void Foam::cylinderSet::genSamples()
{
    // Storage for sample points
    DynamicList<point> samplingPts;
    DynamicList<label> samplingCells;
    DynamicList<label> samplingFaces;
    DynamicList<label> samplingSegments;
    DynamicList<scalar> samplingCurveDist;

    calcSamples
    (
        samplingPts,
        samplingCells,
        samplingFaces,
        samplingSegments,
        samplingCurveDist
    );

    samplingPts.shrink();
    samplingCells.shrink();
    samplingFaces.shrink();
    samplingSegments.shrink();
    samplingCurveDist.shrink();

    setSamples
    (
        samplingPts,
        samplingCells,
        samplingFaces,
        samplingSegments,
        samplingCurveDist
    );
}


// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

Foam::cylinderSet::cylinderSet
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
)
:
    sampledSet(name, mesh, searchEngine, axis),
    origin1_(origin1),
    origin2_(origin2),
    radius_(radius),
    dAxial_(dAxial),
    dAzimuthal_(dAzimuthal)
{
    genSamples();

    if (debug)
    {
        write(Info);
    }
}


Foam::cylinderSet::cylinderSet
(
    const word& name,
    const polyMesh& mesh,
    const meshSearch& searchEngine,
    const dictionary& dict
)
:
    sampledSet(name, mesh, searchEngine, dict),
    origin1_(dict.lookup("origin1")),
    origin2_(dict.lookup("origin2")),
    radius_(readScalar(dict.lookup("radius"))),
    dAxial_(readScalar(dict.lookup("dAxial"))),
    dAzimuthal_(readScalar(dict.lookup("dAzimuthal")))
{
    genSamples();

    if (debug)
    {
        write(Info);
    }
}


// * * * * * * * * * * * * * * * * Destructor  * * * * * * * * * * * * * * * //

Foam::cylinderSet::~cylinderSet()
{}


// ************************************************************************* //
