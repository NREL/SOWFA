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

#include "windRoseToCartesian.H"
#include "primitiveFields.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

List<List<vector> > windRoseToCartesian
(
    const List<List<scalar> >& speed,
    const List<List<scalar> >& direction
)
{
    // Get size of interpolation point lists.
    label ni = speed.size();
    label nj = speed[0].size();

    // Interpolate element by element.
    List<List<vector> > u(ni,List<vector>(nj));
    forAll(speed, i)
    {
        forAll(speed[i], j)
        {
            u[i][j] = windRoseToCartesian(speed[i][j],direction[i][j]);
        }
    }
    return u;
}


List<vector> windRoseToCartesian
(
    const List<scalar>& speed,
    const List<scalar>& direction
)
{
    // Get size of interpolation point lists.
    label ni = speed.size();

    // Interpolate element by element.
    List<vector> u(ni);
    forAll(u, i)
    {
        u[i] = windRoseToCartesian(speed[i],direction[i]);
    }
    return u;
}


vector windRoseToCartesian
(
    const scalar speed,
    const scalar direction
)
{
    scalar dir = 1.0*direction;

    if (dir > 180.0)
    {
       dir -= 180.0;
    }
    else
    {
       dir += 180.0;
    }
    dir = 90.0 - dir;
    if (dir < 0.0)
    {
       dir += 360.0;
    }
    dir *= ((Foam::constant::mathematical::pi)/180.0);

    vector u;
    u.x() = speed * Foam::cos(dir);
    u.y() = speed * Foam::sin(dir);
    u.z() = 0.0;

    return u;
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// ************************************************************************* //
