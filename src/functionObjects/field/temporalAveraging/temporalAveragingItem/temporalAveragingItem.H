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

Class
    Foam::functionObjects::temporalAveragingItem

Description
    Helper class to describe what form of averaging to apply.  A set will be
    applied to each base field in Foam::temporalAveraging, of the form:

    \verbatim
    {
        mean            on;
        prime2Mean      on;
        primeUprimeMean on;
        base            time; // iteration
        window          200;  // optional averaging window
        windowName      w1;   // optional window name (default = "")
    }
    \endverbatim

    The averaging window corresponds to the averaging interval (iters or time)
    If not specified, the averaging is over 'all iters/time'

SourceFiles
    temporalAveragingItem.C
    temporalAveragingItemIO.C

\*---------------------------------------------------------------------------*/

#ifndef temporalAveragingItem_H
#define temporalAveragingItem_H

#include "NamedEnum.H"
#include "Switch.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// Forward declaration of classes
class Istream;
class Ostream;

namespace functionObjects
{

// Forward declaration of friend functions and operators
class temporalAveragingItem;
Istream& operator>>(Istream&, temporalAveragingItem&);
Ostream& operator<<(Ostream&, const temporalAveragingItem&);

/*---------------------------------------------------------------------------*\
                      Class temporalAveragingItem Declaration
\*---------------------------------------------------------------------------*/

class temporalAveragingItem
{
public:

    // Public data

        // File and field name extensions

            //- Mean average
            static const word EXT_MEAN;

            //- Prime-squared average
            static const word EXT_PRIME2MEAN;

            //- Prime-Uprime average
            static const word EXT_PRIMEUPRIMEMEAN;

        //- Enumeration defining the averaging base type
        enum class baseType
        {
            iter,
            time
        };


private:

    // Private data

        //- Field name
        word fieldName_;

        //- Compute mean flag
        Switch mean_;

        //- Name of mean field
        word meanFieldName_;

        //- Compute prime-squared mean flag
        Switch prime2Mean_;

        //- Name of prime-squared mean field
        word prime2MeanFieldName_;

        //- Compute prime-Uprime mean flag
        Switch primeUPrimeMean_;

        //- Name of prime-Uprime mean field
        word primeUPrimeMeanFieldName_;

        //- Averaging base type names
        static const NamedEnum<baseType, 2> baseTypeNames_;

        //- Averaging base type
        baseType base_;

        //- Averaging window - defaults to -1 for 'all iters/time'
        scalar window_;

        //- Averaging window name - defaults to 'window'
        word windowName_;


public:

    // Constructors

        //- Construct null
        temporalAveragingItem();

        //- Construct from Istream
        temporalAveragingItem(Istream&);

        //- Construct as copy
        temporalAveragingItem(const temporalAveragingItem&);


    //- Destructor
    ~temporalAveragingItem();


    // Member Functions

        // Access

            //- Return const access to the field name
            const word& fieldName() const
            {
                return fieldName_;
            }

            //- Return const access to the mean flag
            const Switch& mean() const
            {
                return mean_;
            }

            //- Return non-const access to the mean flag
            Switch& mean()
            {
                return mean_;
            }

            //- Return const access to the mean field name
            const word& meanFieldName() const
            {
                return meanFieldName_;
            }

            //- Return const access to the prime-squared mean flag
            const Switch& prime2Mean() const
            {
                return prime2Mean_;
            }

            //- Return non-const access to the prime-squared mean flag
            Switch& prime2Mean()
            {
                return prime2Mean_;
            }

            //- Return const access to the prime-squared mean field name
            const word& prime2MeanFieldName() const
            {
                return prime2MeanFieldName_;
            }

            //- Return const access to the prime-Uprime mean flag
            const Switch& primeUPrimeMean() const
            {
                return primeUPrimeMean_;
            }

            //- Return non-const access to the prime-Uprime mean flag
            Switch& primeUPrimeMean()
            {
                return primeUPrimeMean_;
            }

            //- Return const access to the prime-Uprime mean field name
            const word& primeUPrimeMeanFieldName() const
            {
                return primeUPrimeMeanFieldName_;
            }

            //- Return averaging base type name
            const word base() const
            {
                return baseTypeNames_[base_];
            }

            //- Return true if base is iter
            Switch iterBase() const
            {
                return base_ == baseType::iter;
            }

            //- Return true if base is time
            Switch timeBase() const
            {
                return base_ == baseType::time;
            }

            scalar window() const
            {
                return window_;
            }

            const word& windowName() const
            {
                return windowName_;
            }


    // Member Operators

        void operator=(const temporalAveragingItem&);


    // Friend Operators

        friend bool operator==
        (
            const temporalAveragingItem& a,
            const temporalAveragingItem& b
        )
        {
            return
                a.fieldName_ == b.fieldName_
             && a.mean_ == b.mean_
             && a.meanFieldName_ == b.meanFieldName_
             && a.prime2Mean_ == b.prime2Mean_
             && a.prime2MeanFieldName_ == b.prime2MeanFieldName_
             && a.primeUPrimeMean_ == b.primeUPrimeMean_
             && a.primeUPrimeMeanFieldName_ == b.primeUPrimeMeanFieldName_
             && a.base_ == b.base_
             && a.window_ == b.window_
             && a.windowName_ == b.windowName_;
        }

        friend bool operator!=
        (
            const temporalAveragingItem& a,
            const temporalAveragingItem& b
        )
        {
            return !(a == b);
        }


    // IOstream Operators

        friend Istream& operator>>(Istream&, temporalAveragingItem&);
        friend Ostream& operator<<(Ostream&, const temporalAveragingItem&);
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace functionObjects
} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
