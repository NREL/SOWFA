/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     | Website:  https://openfoam.org
    \\  /    A nd           | Copyright (C) 2011-2018 OpenFOAM Foundation
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

#include "temporalAveragingItem.H"

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

const Foam::word Foam::functionObjects::temporalAveragingItem::EXT_MEAN
(
    "Mean"
);

const Foam::word Foam::functionObjects::temporalAveragingItem::EXT_PRIME2MEAN
(
    "Prime2Mean"
);

const Foam::word Foam::functionObjects::temporalAveragingItem::EXT_PRIMEUPRIMEMEAN
(
    "PrimeUPrimeMean"
);

template<>
const char* Foam::NamedEnum
<
    Foam::functionObjects::temporalAveragingItem::baseType,
    2
>::names[] = { "iteration", "time"};

const Foam::NamedEnum
<
    Foam::functionObjects::temporalAveragingItem::baseType,
    2
> Foam::functionObjects::temporalAveragingItem::baseTypeNames_;


// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

Foam::functionObjects::temporalAveragingItem::temporalAveragingItem()
:
    fieldName_("unknown"),
    mean_(0),
    meanFieldName_("unknown"),
    prime2Mean_(0),
    prime2MeanFieldName_("unknown"),
    primeUPrimeMean_(0),
    primeUPrimeMeanFieldName_("unknown"),
    base_(baseType::iter),
    window_(-1.0),
    windowName_("")
{}


Foam::functionObjects::temporalAveragingItem::temporalAveragingItem
(
    const temporalAveragingItem& faItem
)
:
    fieldName_(faItem.fieldName_),
    mean_(faItem.mean_),
    meanFieldName_(faItem.meanFieldName_),
    prime2Mean_(faItem.prime2Mean_),
    prime2MeanFieldName_(faItem.prime2MeanFieldName_),
    primeUPrimeMean_(faItem.primeUPrimeMean_),
    primeUPrimeMeanFieldName_(faItem.primeUPrimeMeanFieldName_),
    base_(faItem.base_),
    window_(faItem.window_),
    windowName_(faItem.windowName_)
{}


// * * * * * * * * * * * * * * * * Destructor  * * * * * * * * * * * * * * * //

Foam::functionObjects::temporalAveragingItem::~temporalAveragingItem()
{}


// * * * * * * * * * * * * * * * Member Operators  * * * * * * * * * * * * * //

void Foam::functionObjects::temporalAveragingItem::operator=
(
    const temporalAveragingItem& rhs
)
{
    // Check for assignment to self
    if (this == &rhs)
    {
        FatalErrorInFunction
            << "Attempted assignment to self" << nl
            << abort(FatalError);
    }

    // Set updated values
    fieldName_ = rhs.fieldName_;
    mean_ = rhs.mean_;
    meanFieldName_ = rhs.meanFieldName_;
    prime2Mean_ = rhs.prime2Mean_;
    prime2MeanFieldName_ = rhs.prime2MeanFieldName_;
    primeUPrimeMean_ = rhs.primeUPrimeMean_;
    primeUPrimeMeanFieldName_ = rhs.primeUPrimeMeanFieldName_;
    base_ = rhs.base_;
    window_ = rhs.window_;
    windowName_ = rhs.windowName_;
}


// ************************************************************************* //
