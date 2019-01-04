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

\*---------------------------------------------------------------------------*/

#include "timeVaryingMappedFluctuatingFixedValueFvPatchField.H"
#include "Time.H"
#include "triSurfaceTools.H"
#include "triSurface.H"
#include "vector2D.H"
#include "OFstream.H"
#include "AverageIOField.H"
#include "transformGeometricField.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

template<class Type>
timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::
timeVaryingMappedFluctuatingFixedValueFvPatchField
(
    const fvPatch& p,
    const DimensionedField<Type, volMesh>& iF
)
:
    fixedValueFvPatchField<Type>(p, iF),
    fieldTableName_(iF.name()),
    setAverage_(false),
    perturb_(0),
    rotateInflow_(false),
    rotationAxis_(vector::zero),
    rotationAngle_(0.0),
    fluctUpdatePeriod_(10.0),
    fluctVertDecayType_("constant"),
    fluctVertDecayHeight_(100.0),
    fluctMag_(pTraits<Type>::zero),
    fluctResVert_(30.0),
    fluctResHoriz_(30.0),
    fluctUpdateTimeLast_(-1),
    fluctField_(p.size()),
    fluctRanGen_(label(0)),
    referenceCS_(NULL),
    nearestVertex_(0),
    nearestVertexWeight_(0),
    sampleTimes_(0),
    startSampleTime_(-1),
    startSampledValues_(0),
    startAverage_(pTraits<Type>::zero),
    endSampleTime_(-1),
    endSampledValues_(0),
    endAverage_(pTraits<Type>::zero)
{}


template<class Type>
timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::
timeVaryingMappedFluctuatingFixedValueFvPatchField
(
    const timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>& ptf,
    const fvPatch& p,
    const DimensionedField<Type, volMesh>& iF,
    const fvPatchFieldMapper& mapper
)
:
    fixedValueFvPatchField<Type>(ptf, p, iF, mapper),
    fieldTableName_(ptf.fieldTableName_),
    setAverage_(ptf.setAverage_),
    perturb_(ptf.perturb_),
    rotateInflow_(ptf.rotateInflow_),
    rotationAxis_(ptf.rotationAxis_),
    rotationAngle_(ptf.rotationAngle_),
    fluctUpdatePeriod_(ptf.fluctUpdatePeriod_),
    fluctVertDecayType_(ptf.fluctVertDecayType_),
    fluctVertDecayHeight_(ptf.fluctVertDecayHeight_),
    fluctMag_(ptf.fluctMag_),
    fluctResVert_(ptf.fluctResVert_),
    fluctResHoriz_(ptf.fluctResHoriz_),
    fluctUpdateTimeLast_(ptf.fluctUpdateTimeLast_),
    fluctField_(ptf.fluctField_,mapper),
    fluctRanGen_(label(0)),
    referenceCS_(NULL),
    nearestVertex_(0),
    nearestVertexWeight_(0),
    sampleTimes_(0),
    startSampleTime_(-1),
    startSampledValues_(0),
    startAverage_(pTraits<Type>::zero),
    endSampleTime_(-1),
    endSampledValues_(0),
    endAverage_(pTraits<Type>::zero)
{}


template<class Type>
timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::
timeVaryingMappedFluctuatingFixedValueFvPatchField
(
    const fvPatch& p,
    const DimensionedField<Type, volMesh>& iF,
    const dictionary& dict
)
:
    fixedValueFvPatchField<Type>(p, iF),
    fieldTableName_(iF.name()),
    setAverage_(readBool(dict.lookup("setAverage"))),
    perturb_(dict.lookupOrDefault("perturb", 1E-5)),
    rotateInflow_(readBool(dict.lookup("rotateInflow"))),
    rotationAxis_(vector(dict.lookup("rotationAxis"))),
    rotationAngle_(readScalar(dict.lookup("rotationAngle"))),
    fluctUpdatePeriod_(readScalar(dict.lookup("fluctUpdatePeriod"))),
    fluctVertDecayType_(dict.lookup("fluctVertDecayType")),
    fluctVertDecayHeight_(readScalar(dict.lookup("fluctVertDecayHeight"))),
    fluctMag_(pTraits<Type>(dict.lookup("fluctMag"))),
    fluctResVert_(readScalar(dict.lookup("fluctResVert"))),
    fluctResHoriz_(readScalar(dict.lookup("fluctResHoriz"))),
    fluctUpdateTimeLast_(dict.lookupOrDefault<scalar>("fluctUpdateTimeLast", -1)),
    fluctField_("fluctField", dict, p.size()),
    fluctRanGen_(label(0)),
    referenceCS_(NULL),
    nearestVertex_(0),
    nearestVertexWeight_(0),
    sampleTimes_(0),
    startSampleTime_(-1),
    startSampledValues_(0),
    startAverage_(pTraits<Type>::zero),
    endSampleTime_(-1),
    endSampledValues_(0),
    endAverage_(pTraits<Type>::zero)
{
    dict.readIfPresent("fieldTableName", fieldTableName_);

    if (dict.found("value"))
    {
        fvPatchField<Type>::operator==(Field<Type>("value", dict, p.size()));
    }
    else
    {
        updateCoeffs();
    }
}


template<class Type>
timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::
timeVaryingMappedFluctuatingFixedValueFvPatchField
(
    const timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>& ptf
)
:
    fixedValueFvPatchField<Type>(ptf),
    fieldTableName_(ptf.fieldTableName_),
    setAverage_(ptf.setAverage_),
    perturb_(ptf.perturb_),
    rotateInflow_(ptf.rotateInflow_),
    rotationAxis_(ptf.rotationAxis_),
    rotationAngle_(ptf.rotationAngle_),
    fluctUpdatePeriod_(ptf.fluctUpdatePeriod_),
    fluctVertDecayType_(ptf.fluctVertDecayType_),
    fluctVertDecayHeight_(ptf.fluctVertDecayHeight_),
    fluctMag_(ptf.fluctMag_),
    fluctResVert_(ptf.fluctResVert_),
    fluctResHoriz_(ptf.fluctResHoriz_),
    fluctUpdateTimeLast_(ptf.fluctUpdateTimeLast_),
    fluctField_(ptf.fluctField_),
    fluctRanGen_(ptf.fluctRanGen_),
    referenceCS_(ptf.referenceCS_),
    nearestVertex_(ptf.nearestVertex_),
    nearestVertexWeight_(ptf.nearestVertexWeight_),
    sampleTimes_(ptf.sampleTimes_),
    startSampleTime_(ptf.startSampleTime_),
    startSampledValues_(ptf.startSampledValues_),
    startAverage_(ptf.startAverage_),
    endSampleTime_(ptf.endSampleTime_),
    endSampledValues_(ptf.endSampledValues_),
    endAverage_(ptf.endAverage_)
{}



template<class Type>
timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::
timeVaryingMappedFluctuatingFixedValueFvPatchField
(
    const timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>& ptf,
    const DimensionedField<Type, volMesh>& iF
)
:
    fixedValueFvPatchField<Type>(ptf, iF),
    fieldTableName_(ptf.fieldTableName_),
    setAverage_(ptf.setAverage_),
    perturb_(ptf.perturb_),
    rotateInflow_(ptf.rotateInflow_),
    rotationAxis_(ptf.rotationAxis_),
    rotationAngle_(ptf.rotationAngle_),
    fluctUpdatePeriod_(ptf.fluctUpdatePeriod_),
    fluctVertDecayType_(ptf.fluctVertDecayType_),
    fluctVertDecayHeight_(ptf.fluctVertDecayHeight_),
    fluctMag_(ptf.fluctMag_),
    fluctResVert_(ptf.fluctResVert_),
    fluctResHoriz_(ptf.fluctResHoriz_),
    fluctUpdateTimeLast_(ptf.fluctUpdateTimeLast_),
    fluctField_(ptf.fluctField_),
    fluctRanGen_(ptf.fluctRanGen_),
    referenceCS_(ptf.referenceCS_),
    nearestVertex_(ptf.nearestVertex_),
    nearestVertexWeight_(ptf.nearestVertexWeight_),
    sampleTimes_(ptf.sampleTimes_),
    startSampleTime_(ptf.startSampleTime_),
    startSampledValues_(ptf.startSampledValues_),
    startAverage_(ptf.startAverage_),
    endSampleTime_(ptf.endSampleTime_),
    endSampledValues_(ptf.endSampledValues_),
    endAverage_(ptf.endAverage_)
{}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

template<class Type>
void timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::autoMap
(
    const fvPatchFieldMapper& m
)
{
    fixedValueFvPatchField<Type>::autoMap(m);
    if (startSampledValues_.size())
    {
        startSampledValues_.autoMap(m);
        endSampledValues_.autoMap(m);
    }
}


template<class Type>
void timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::rmap
(
    const fvPatchField<Type>& ptf,
    const labelList& addr
)
{
    fixedValueFvPatchField<Type>::rmap(ptf, addr);

    const timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>& tiptf =
        refCast<const timeVaryingMappedFluctuatingFixedValueFvPatchField<Type> >(ptf);

    startSampledValues_.rmap(tiptf.startSampledValues_, addr);
    endSampledValues_.rmap(tiptf.endSampledValues_, addr);
}


template<class Type>
void timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::readSamplePoints()
{
    // Read the sample points

    pointIOField samplePoints
    (
        IOobject
        (
            "points",
            this->db().time().constant(),
            "boundaryData"/this->patch().name(),
            this->db(),
            IOobject::MUST_READ,
            IOobject::AUTO_WRITE,
            false
        )
    );

    const fileName samplePointsFile = samplePoints.filePath();

    if (debug)
    {
        Info<< "timeVaryingMappedFluctuatingFixedValueFvPatchField :"
            << " Read " << samplePoints.size() << " sample points from "
            << samplePointsFile << endl;
    }

    // Determine coordinate system from samplePoints

    if (samplePoints.size() < 3)
    {
        FatalErrorIn
        (
            "timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::readSamplePoints()"
        )   << "Only " << samplePoints.size() << " points read from file "
            << samplePoints.objectPath() << nl
            << "Need at least three non-colinear samplePoints"
            << " to be able to interpolate."
            << "\n    on patch " << this->patch().name()
            << " of points " << samplePoints.name()
            << " in file " << samplePoints.objectPath()
            << exit(FatalError);
    }

    const point& p0 = samplePoints[0];

    // Find furthest away point
    vector e1;
    label index1 = -1;
    scalar maxDist = -GREAT;

    for (label i = 1; i < samplePoints.size(); i++)
    {
        const vector d = samplePoints[i] - p0;
        scalar magD = mag(d);

        if (magD > maxDist)
        {
            e1 = d/magD;
            index1 = i;
            maxDist = magD;
        }
    }
    // Find point that is furthest away from line p0-p1
    const point& p1 = samplePoints[index1];

    label index2 = -1;
    maxDist = -GREAT;
    for (label i = 1; i < samplePoints.size(); i++)
    {
        if (i != index1)
        {
            const point& p2 = samplePoints[i];
            vector e2(p2 - p0);
            e2 -= (e2&e1)*e1;
            scalar magE2 = mag(e2);

            if (magE2 > maxDist)
            {
                index2 = i;
                maxDist = magE2;
            }
        }
    }
    if (index2 == -1)
    {
        FatalErrorIn
        (
            "timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::readSamplePoints()"
        )   << "Cannot find points that make valid normal." << nl
            << "Have so far points " << p0 << " and " << p1
            << "Need at least three sample points which are not in a line."
            << "\n    on patch " << this->patch().name()
            << " of points " << samplePoints.name()
            << " in file " << samplePoints.objectPath()
            << exit(FatalError);
    }

    vector n = e1^(samplePoints[index2]-p0);
    n /= mag(n);

    if (debug)
    {
        Info<< "timeVaryingMappedFluctuatingFixedValueFvPatchField :"
            << " Used points " << p0 << ' ' << samplePoints[index1]
            << ' ' << samplePoints[index2]
            << " to define coordinate system with normal " << n << endl;
    }

    referenceCS_.reset
    (
        new coordinateSystem
        (
            "reference",
            p0,  // origin
            n,   // normal
            e1   // 0-axis
        )
    );

    tmp<vectorField> tlocalVertices
    (
        referenceCS().localPosition(samplePoints)
    );
    vectorField& localVertices = tlocalVertices();

    const boundBox bb(localVertices, true);
    const point bbMid(bb.midpoint());

    if (debug)
    {
        Info<< "timeVaryingMappedFluctuatingFixedValueFvPatchField :"
            << " Perturbing points with " << perturb_
            << " fraction of a random position inside " << bb
            << " to break any ties on regular meshes."
            << nl << endl;
    }

    Random rndGen(123456);
    forAll(localVertices, i)
    {
        localVertices[i] +=
            perturb_
           *(rndGen.position(bb.min(), bb.max())-bbMid);
    }

    // Determine triangulation
    List<vector2D> localVertices2D(localVertices.size());
    forAll(localVertices, i)
    {
        localVertices2D[i][0] = localVertices[i][0];
        localVertices2D[i][1] = localVertices[i][1];
    }

    triSurface s(triSurfaceTools::delaunay2D(localVertices2D));

    tmp<pointField> tlocalFaceCentres
    (
        referenceCS().localPosition
        (
            this->patch().patch().faceCentres()
        )
    );
    const pointField& localFaceCentres = tlocalFaceCentres();

    if (debug)
    {
        Pout<< "readSamplePoints :"
            <<" Dumping triangulated surface to triangulation.stl" << endl;
        s.write(this->db().time().path()/"triangulation.stl");

        OFstream str(this->db().time().path()/"localFaceCentres.obj");
        Pout<< "readSamplePoints :"
            << " Dumping face centres to " << str.name() << endl;

        forAll(localFaceCentres, i)
        {
            const point& p = localFaceCentres[i];
            str<< "v " << p.x() << ' ' << p.y() << ' ' << p.z() << nl;
        }
    }

    // Determine interpolation onto face centres.
    triSurfaceTools::calcInterpolationWeights
    (
        s,
        localFaceCentres,   // points to interpolate to
        nearestVertex_,
        nearestVertexWeight_
    );



    // Read the times for which data is available

    const fileName samplePointsDir = samplePointsFile.path();

    sampleTimes_ = Time::findTimes(samplePointsDir);

    if (debug)
    {
        Info<< "timeVaryingMappedFluctuatingFixedValueFvPatchField : In directory "
            << samplePointsDir << " found times " << timeNames(sampleTimes_)
            << endl;
    }
}


template<class Type>
wordList timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::timeNames
(
    const instantList& times
)
{
    wordList names(times.size());

    forAll(times, i)
    {
        names[i] = times[i].name();
    }
    return names;
}


template<class Type>
void timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::findTime
(
    const fileName& instance,
    const fileName& local,
    const scalar timeVal,
    label& lo,
    label& hi
) const
{
    lo = startSampleTime_;
    hi = -1;

    for (label i = startSampleTime_+1; i < sampleTimes_.size(); i++)
    {
        if (sampleTimes_[i].value() > timeVal)
        {
            break;
        }
        else
        {
            lo = i;
        }
    }

    if (lo == -1)
    {
        FatalErrorIn("findTime")
            << "Cannot find starting sampling values for current time "
            << timeVal << nl
            << "Have sampling values for times "
            << timeNames(sampleTimes_) << nl
            << "In directory "
            <<  this->db().time().constant()/"boundaryData"/this->patch().name()
            << "\n    on patch " << this->patch().name()
            << " of field " << fieldTableName_
            << exit(FatalError);
    }

    if (lo < sampleTimes_.size()-1)
    {
        hi = lo+1;
    }


    if (debug)
    {
        if (hi == -1)
        {
            Pout<< "findTime : Found time " << timeVal << " after"
                << " index:" << lo << " time:" << sampleTimes_[lo].value()
                << endl;
        }
        else
        {
            Pout<< "findTime : Found time " << timeVal << " inbetween"
                << " index:" << lo << " time:" << sampleTimes_[lo].value()
                << " and index:" << hi << " time:" << sampleTimes_[hi].value()
                << endl;
        }
    }
}


template<class Type>
void timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::checkTable()
{
    // Initialise
    if (startSampleTime_ == -1 && endSampleTime_ == -1)
    {
        readSamplePoints();
    }

    // Find current time in sampleTimes
    label lo = -1;
    label hi = -1;

    findTime
    (
        this->db().time().constant(),
        "boundaryData"/this->patch().name(),
        this->db().time().value(),
        lo,
        hi
    );

    // Update sampled data fields.

    if (lo != startSampleTime_)
    {
        startSampleTime_ = lo;

        if (startSampleTime_ == endSampleTime_)
        {
            // No need to reread since are end values
            if (debug)
            {
                Pout<< "checkTable : Setting startValues to (already read) "
                    <<   "boundaryData"
                        /this->patch().name()
                        /sampleTimes_[startSampleTime_].name()
                    << endl;
            }
            startSampledValues_ = endSampledValues_;
            startAverage_ = endAverage_;
        }
        else
        {
            if (debug)
            {
                Pout<< "checkTable : Reading startValues from "
                    <<   "boundaryData"
                        /this->patch().name()
                        /sampleTimes_[lo].name()
                    << endl;
            }


            // Reread values and interpolate
            AverageIOField<Type> vals
            (
                IOobject
                (
                    fieldTableName_,
                    this->db().time().constant(),
                    "boundaryData"
                   /this->patch().name()
                   /sampleTimes_[startSampleTime_].name(),
                    this->db(),
                    IOobject::MUST_READ,
                    IOobject::AUTO_WRITE,
                    false
                )
            );

            startAverage_ = vals.average();
            startSampledValues_ = interpolate(vals);
        }
    }

    if (hi != endSampleTime_)
    {
        endSampleTime_ = hi;

        if (endSampleTime_ == -1)
        {
            // endTime no longer valid. Might as well clear endValues.
            if (debug)
            {
                Pout<< "checkTable : Clearing endValues" << endl;
            }
            endSampledValues_.clear();
        }
        else
        {
            if (debug)
            {
                Pout<< "checkTable : Reading endValues from "
                    <<   "boundaryData"
                        /this->patch().name()
                        /sampleTimes_[endSampleTime_].name()
                    << endl;
            }
            // Reread values and interpolate
            AverageIOField<Type> vals
            (
                IOobject
                (
                    fieldTableName_,
                    this->db().time().constant(),
                    "boundaryData"
                   /this->patch().name()
                   /sampleTimes_[endSampleTime_].name(),
                    this->db(),
                    IOobject::MUST_READ,
                    IOobject::AUTO_WRITE,
                    false
                )
            );
            endAverage_ = vals.average();
            endSampledValues_ = interpolate(vals);
        }
    }
}


template<class Type>
tmp<Field<Type> > timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::interpolate
(
    const Field<Type>& sourceFld
) const
{
    tmp<Field<Type> > tfld(new Field<Type>(nearestVertex_.size()));
    Field<Type>& fld = tfld();

    forAll(fld, i)
    {
        const FixedList<label, 3>& verts = nearestVertex_[i];
        const FixedList<scalar, 3>& w = nearestVertexWeight_[i];

        if (verts[2] == -1)
        {
            if (verts[1] == -1)
            {
                // Use vertex0 only
                fld[i] = sourceFld[verts[0]];
            }
            else
            {
                // Use vertex 0,1
                fld[i] =
                    w[0]*sourceFld[verts[0]]
                  + w[1]*sourceFld[verts[1]];
            }
        }
        else
        {
            fld[i] =
                w[0]*sourceFld[verts[0]]
              + w[1]*sourceFld[verts[1]]
              + w[2]*sourceFld[verts[2]];
        }
    }
    return tfld;
}


template<class Type>
void timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::updateCoeffs()
{
    if (this->updated())
    {
        return;
    }

    // If this is the first time the boundary condition i called, and 
    // fluctUpdateTimeLast_ was not set by the user or was set to a value further
    // into the past than the fluctuation update period, then set the 
    // fluctUpdateTimeLast_ variable to match the simulation start time.
    if ((fluctUpdateTimeLast_ == -1) ||
        (this->db().time().startTime().value() - fluctUpdateTimeLast_ > fluctUpdatePeriod_))
    {
        fluctUpdateTimeLast_ = this->db().time().startTime().value();
    }

    checkTable();

    // Interpolate between the sampled data

    Type wantedAverage;

    if (endSampleTime_ == -1)
    {
        // only start value
        if (debug)
        {
            Pout<< "updateCoeffs : Sampled, non-interpolated values"
                << " from start time:"
                << sampleTimes_[startSampleTime_].name() << nl;
        }

        this->operator==(startSampledValues_);
        wantedAverage = startAverage_;
    }
    else
    {
        scalar start = sampleTimes_[startSampleTime_].value();
        scalar end = sampleTimes_[endSampleTime_].value();

        scalar s = (this->db().time().value()-start)/(end-start);

        if (debug)
        {
            Pout<< "updateCoeffs : Sampled, interpolated values"
                << " between start time:"
                << sampleTimes_[startSampleTime_].name()
                << " and end time:" << sampleTimes_[endSampleTime_].name()
                << " with weight:" << s << endl;
        }

        this->operator==((1-s)*startSampledValues_ + s*endSampledValues_);
        wantedAverage = (1-s)*startAverage_ + s*endAverage_;
    }









    // Add fluctuations to the field -- MJC -- 3 June 2014
    // Create new fluctuations if a period of time greater than the fluctuation
    // update period has elapsed.
  //Info << "fluctUpdateTimeLast: " << fluctUpdateTimeLast_ << endl;

    if ((this->db().time().value() - fluctUpdateTimeLast_ >= fluctUpdatePeriod_) &&
        (Foam::mag(fluctMag_) > 0.0))
    {
        // Find the bounding box of the patch so that the horizontal and
        // vertical extents can be found.
        boundBox bb(this->patch().patch().localPoints(), false);
        scalar bbMinX = bb.min().x();
        scalar bbMinY = bb.min().y();
        scalar bbMinZ = bb.min().z();
        scalar bbMaxX = bb.max().x();
        scalar bbMaxY = bb.max().y();
        scalar bbMaxZ = bb.max().z();
        reduce(bbMinX,minOp<scalar>());
        reduce(bbMinY,minOp<scalar>());
        reduce(bbMinZ,minOp<scalar>());
        reduce(bbMaxX,maxOp<scalar>());
        reduce(bbMaxY,maxOp<scalar>());
        reduce(bbMaxZ,maxOp<scalar>());

        scalar extentHoriz = sqrt(sqr(bbMaxX - bbMinX) + sqr(bbMaxY - bbMinY));
        scalar extentVert = bbMaxZ - bbMinZ;

      //Info << "bb = (" << bbMinX << " " << bbMinY << " " << bbMinZ << ") (" << bbMaxX << " " << bbMaxY << " " << bbMaxZ << ")" << endl;    
      //Info << "extentHoriz = " << extentHoriz << endl;
      //Info << "extentVert = " << extentVert << endl;

        
        // Compute how many fluctuation "cells" there will be in the
        // horizontal and vertical directions.
        label nFluctHoriz = ceil(extentHoriz/fluctResHoriz_);
        label nFluctVert = ceil(extentVert/fluctResVert_);

      //Info << "nFluctHoriz = " << nFluctHoriz << tab << "nFluctVert = " << nFluctVert << endl;        


        // Create the fluctuation array.
        DynamicList<List<Type> > randomField;
        for (int i = 0; i < nFluctHoriz; i++)
        {
           DynamicList<Type> randomFieldI;
           for (int j = 0; j < nFluctVert; j++)
           {
              Type fluctVal;
              fluctRanGen_.randomise(fluctVal);
              randomFieldI.append(cmptMultiply(fluctMag_,(fluctVal - 0.5*pTraits<Type>::one)));
           }
           randomField.append(randomFieldI);
        }
        
      //Info << "randomField size: " << randomField.size() << endl;
      //Info << randomField << endl;


        // Parallel communicate the master fluctuation list so that all 
        // processors have the same list.
        Pstream::scatter(randomField);


        // Apply the random field to the patch faces.
        forAll(fluctField_,i)
        {
           vector faceCentre = this->patch().patch().faceCentres()[i];
           scalar distHoriz = sqrt(sqr(faceCentre.x() - bbMinX) + sqr(faceCentre.y() - bbMinY));
           scalar distVert = faceCentre.z();
           label ii = ceil(distHoriz / fluctResHoriz_) - 1;
           label jj = ceil(distVert / fluctResVert_) - 1;
           
           // Compute how the fluctuations decay with height
           scalar decayFunction = 0.0;

           // - decay function is 1 up to a certain height, then 0.
           if (fluctVertDecayType_ == "constant")
           {
               if (distVert <= fluctVertDecayHeight_)
               {
                   decayFunction = 1.0;
               }
           }

           // - decay function follows a function of the form 1.65 * z/zPeak * exp(-0.5*(z/zPeak)^2)
           else if (fluctVertDecayType_ == "exp")
           {
               decayFunction = 1.65 * (distVert/fluctVertDecayHeight_) * Foam::exp(-0.5*Foam::pow((distVert/fluctVertDecayHeight_),2));
           }

           // - decay function is 1 up to a certain height then follows the exponential decay function above.
           else if (fluctVertDecayType_ == "expConstant")
           {
               if (distVert <= fluctVertDecayHeight_)
               {
                   decayFunction = 1.0;
               }
               else
               {
                   decayFunction = 1.65 * (distVert/fluctVertDecayHeight_) * Foam::exp(-0.5*Foam::pow((distVert/fluctVertDecayHeight_),2));
               }
           }

           // Multiply the decay function by the random number corresponding to this "fluctuation cell."
           fluctField_[i] = decayFunction * randomField[ii][jj];     
        }
       

        // Update the last time the fluctuations were updated.   
        fluctUpdateTimeLast_ += fluctUpdatePeriod_;
    }

    // Add the fluctuations to the base field.
    const Field<Type>& fld = *this;
    this->operator==(fld + fluctField_);




    //- Apply rotation of inflow field.  MJC - 10 Sept 2014
    if (rotateInflow_)
    {
        // Get access to inflow field.
        const Field<Type>& fld = *this;
        Field<Type> fldNew = fld;

        // Create rotation matrix.
        vector u = rotationAxis_/mag(rotationAxis_);
        tensor R = tensor::zero;
        scalar phi = rotationAngle_ * Foam::constant::mathematical::pi / 180.0;

        // from http://en.wikipedia.org/wiki/Rotation_matrix -- "Rotation matrix from axis and angle"
        R.xx() = Foam::cos(phi) + u.x()*u.x()*(1.0 - Foam::cos(phi));
        R.xy() = u.x()*u.y()*(1.0 - Foam::cos(phi)) - u.z()*Foam::sin(phi);
        R.xz() = u.x()*u.z()*(1.0 - Foam::cos(phi)) + u.y()*Foam::sin(phi);
        R.yx() = u.y()*u.x()*(1.0 - Foam::cos(phi)) + u.z()*Foam::sin(phi);
        R.yy() = Foam::cos(phi) + u.y()*u.y()*(1.0 - Foam::cos(phi));
        R.yz() = u.y()*u.z()*(1.0 - Foam::cos(phi)) - u.x()*Foam::sin(phi);
        R.zx() = u.z()*u.x()*(1.0 - Foam::cos(phi)) - u.y()*Foam::sin(phi);
        R.zy() = u.z()*u.y()*(1.0 - Foam::cos(phi)) + u.x()*Foam::sin(phi);
        R.zz() = Foam::cos(phi) + u.z()*u.z()*(1.0 - Foam::cos(phi));

        forAll(fld, i)
        {
           fldNew[i] = transform(R,fld[i]);
        }

        this->operator==(fldNew);
    }





    // Enforce average. Either by scaling (if scaling factor > 0.5) or by
    // offsetting.
    if (setAverage_)
    {
        const Field<Type>& fld = *this;

        Type averagePsi =
            gSum(this->patch().magSf()*fld)
           /gSum(this->patch().magSf());

        if (debug)
        {
            Pout<< "updateCoeffs :"
                << " actual average:" << averagePsi
                << " wanted average:" << wantedAverage
                << endl;
        }

        if (mag(averagePsi) < VSMALL)
        {
            // Field too small to scale. Offset instead.
            const Type offset = wantedAverage - averagePsi;
            if (debug)
            {
                Pout<< "updateCoeffs :"
                    << " offsetting with:" << offset << endl;
            }
            this->operator==(fld+offset);
        }
        else
        {
            const scalar scale = mag(wantedAverage)/mag(averagePsi);

            if (debug)
            {
                Pout<< "updateCoeffs :"
                    << " scaling with:" << scale << endl;
            }
            this->operator==(scale*fld);
        }
    }



    if (debug)
    {
        Pout<< "updateCoeffs : set fixedValue to min:" << gMin(*this)
            << " max:" << gMax(*this) << endl;
    }

    fixedValueFvPatchField<Type>::updateCoeffs();
}


template<class Type>
void timeVaryingMappedFluctuatingFixedValueFvPatchField<Type>::write(Ostream& os) const
{
    fvPatchField<Type>::write(os);
    os.writeKeyword("setAverage") << setAverage_ << token::END_STATEMENT << nl;
    os.writeKeyword("perturb") << perturb_ << token::END_STATEMENT << nl;
    os.writeKeyword("rotateInflow") << rotateInflow_ << token::END_STATEMENT << nl;
    os.writeKeyword("rotationAxis") << rotationAxis_ << token::END_STATEMENT << nl;
    os.writeKeyword("rotationAngle") << rotationAngle_ << token::END_STATEMENT << nl;
    os.writeKeyword("fluctUpdatePeriod") << fluctUpdatePeriod_ << token::END_STATEMENT << nl;
    os.writeKeyword("fluctVertDecayType") << fluctVertDecayType_ << token::END_STATEMENT << nl;
    os.writeKeyword("fluctVertDecayHeight") << fluctVertDecayHeight_ << token::END_STATEMENT << nl;
    os.writeKeyword("fluctMag") << fluctMag_ << token::END_STATEMENT << nl;
    os.writeKeyword("fluctResVert") << fluctResVert_ << token::END_STATEMENT << nl;
    os.writeKeyword("fluctResHoriz") << fluctResHoriz_ << token::END_STATEMENT << nl;
    os.writeKeyword("fluctUpdateTimeLast") << fluctUpdateTimeLast_ << token::END_STATEMENT << nl;
    fluctField_.writeEntry("fluctField", os);

    if (fieldTableName_ != this->dimensionedInternalField().name())
    {
        os.writeKeyword("fieldTableName") << fieldTableName_
            << token::END_STATEMENT << nl;
    }

    this->writeEntry("value", os);
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// ************************************************************************* //
