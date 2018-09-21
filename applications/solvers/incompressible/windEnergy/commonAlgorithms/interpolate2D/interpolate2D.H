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

InNamespace
    Foam

Description
    Interpolates function values from a surface to a given point on the 
    surface using bilinear interpolation.

SourceFiles
    interpolate2D.C

\*---------------------------------------------------------------------------*/

#ifndef interpolate2D_H
#define interpolate2D_H

#include "scalar.H"
#include "primitiveFieldsFwd.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

template<class Type>
List<List<Type> > interpolate2D
(
    const List<scalar>& xi,
    const List<scalar>& yi,
    const List<scalar>& x,
    const List<scalar>& y,
    const List<List<Type> >& f
);

template<class Type>
List<Type> interpolate2D
(
    const scalar& xi,
    const List<scalar>& yi,
    const List<scalar>& x,
    const List<scalar>& y,
    const List<List<Type> >& f
);

template<class Type>
List<Type> interpolate2D
(
    const List<scalar>& xi,
    const scalar& yi,
    const List<scalar>& x,
    const List<scalar>& y,
    const List<List<Type> >& f
);

template<class Type>
Type interpolate2D
(
    const scalar xi,
    const scalar yi,
    const List<scalar>& x,
    const List<scalar>& y,
    const List<List<Type> >& f
);


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#ifdef NoRepository
#   include "interpolate2D.C"
#endif

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
