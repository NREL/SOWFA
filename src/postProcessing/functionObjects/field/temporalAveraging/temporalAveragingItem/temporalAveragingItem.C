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

#include "temporalAveragingItem.H"

// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

namespace Foam
{
    const word temporalAveragingItem::EXT_MEAN = "Mean";
    const word temporalAveragingItem::EXT_PRIME2MEAN = "Prime2Mean";

    template<>
    const char* Foam::NamedEnum
    <
        Foam::temporalAveragingItem::baseType,
        2
    >::names[] =
    {
        "iteration",
        "time"
    };
}


const Foam::NamedEnum<Foam::temporalAveragingItem::baseType, 2>
    Foam::temporalAveragingItem::baseTypeNames_;


// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

Foam::temporalAveragingItem::temporalAveragingItem()
:
    active_(false),
    fieldName_("unknown"),
    mean_(0),
    meanFieldName_("unknown"),
    prime2Mean_(0),
    prime2MeanFieldName_("unknown"),
    base_(ITER),
    window_(-1.0),
    windowName_("")
{}


Foam::temporalAveragingItem::temporalAveragingItem(const temporalAveragingItem& faItem)
:
    active_(faItem.active_),
    fieldName_(faItem.fieldName_),
    mean_(faItem.mean_),
    meanFieldName_(faItem.meanFieldName_),
    prime2Mean_(faItem.prime2Mean_),
    prime2MeanFieldName_(faItem.prime2MeanFieldName_),
    base_(faItem.base_),
    window_(faItem.window_),
    windowName_(faItem.windowName_)
{}


// * * * * * * * * * * * * * * * * Destructor  * * * * * * * * * * * * * * * //

Foam::temporalAveragingItem::~temporalAveragingItem()
{}


// * * * * * * * * * * * * * * * Member Operators  * * * * * * * * * * * * * //

void Foam::temporalAveragingItem::operator=(const temporalAveragingItem& rhs)
{
    // Check for assignment to self
    if (this == &rhs)
    {
        FatalErrorIn
        (
            "Foam::temporalAveragingItem::operator=(const Foam::temporalAveragingItem&)"
        )   << "Attempted assignment to self" << nl
            << abort(FatalError);
    }

    // Set updated values
    active_ = rhs.active_;
    fieldName_ = rhs.fieldName_;
    mean_ = rhs.mean_;
    meanFieldName_ = rhs.meanFieldName_;
    prime2Mean_ = rhs.prime2Mean_;
    prime2MeanFieldName_ = rhs.prime2MeanFieldName_;
    base_ = rhs.base_;
    window_ = rhs.window_;
    windowName_ = rhs.windowName_;
}


// ************************************************************************* //
