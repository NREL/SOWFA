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

#include "interpolate2D.H"
#include "primitiveFields.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //


template<class Type>
Type interpolate2D
(
    const scalar xI,
    const scalar yI,
    label& i,
    label& j,
    const List<scalar>& x,
    const List<scalar>& y,
    const List<List<Type> >& f
)
{
    // Get interpolation data size.
    label nx = x.size();
    label ny = y.size();
    label nxf = f.size();
    label nyf = f[0].size();

    // Check to make sure data sizes all match up.  Does the size of x
    // match the x-index size of f, and same with y.  Give error message
    // and exit if not.
    if ((nx != nxf) || (ny != nyf))
    {
        FatalErrorIn
        (
            "interpolate2d"
        ) << "Sizes of input x and y vectors do not match size of f array:" << endl <<
             "x size: " << nx << endl <<
             "y size: " << ny << endl <<
             "f(x,y) size: " << nxf << ", " << nyf <<
             abort(FatalError);
    }


  
    bool flag;

    // Find the bounding indices in x, starting with the guess value.
    flag = 0;
    label iOriginal = i;
    i--;
    while ((i < nx-1) && (flag == 0))
    {
    i++;
        if ((xI > x[i]) && (xI <= x[i+1]))
        {
            flag = 1;
        }
    }
    // Loop back around if the guess was too high.
    if (flag == 0)
    {
        i = -1;
        while ((i < iOriginal) && (flag == 0))
        {
            i++;
            if ((xI > x[i]) && (xI <= x[i+1]))
            {
                flag = 1;
            }
       }
    }
 
    // If the xI is out of the bounds of the data table, then
    // extrapolate.
    if ((flag == 0) && (xI > x[nx-1]))
    {
        i = nx-2;
        flag = 1;
    }
    if ((flag == 0) && (xI < x[0]))
    {
        i = 0;
        flag = 1;
    }



    // Find the bounding indices in y, starting with the guess value.
    flag = 0;
    label jOriginal = j;
    j--;
    while ((j < ny-1) && (flag == 0))
    {
    j++;
        if ((yI > y[j]) && (yI <= y[j+1]))
        {
            flag = 1;
        }
    }
    // Loop back around if the guess was too high.
    if (flag == 0)
    {
        j = -1;
        while ((j < jOriginal) && (flag == 0))
        {
            j++;
            if ((yI > y[j]) && (yI <= y[j+1]))
            {
                flag = 1;
            }
       }
    }
    // If the yI is out of the bounds of the data table, then
    // extrapolate.
    if ((flag == 0) && (yI > y[ny-1]))
    {
        j = ny-2;
        flag = 1;
    }
    if ((flag == 0) && (yI < y[0]))
    {
        j = 0;
        flag = 1;
    }


    // Calculate the actual bilinear interpolation.
    scalar dx = 0.0;
    scalar dy = 0.0;
    scalar wx = 0.0;
    scalar wy = 0.0;
    Type f11 = 0.0;
    Type f12 = 0.0;
    Type f21 = 0.0;
    Type f22 = 0.0;

    if (nx > 1)
    {
       dx = x[i+1] - x[i];
       wx = (xI - x[i]) / dx;
    }

    if (ny > 1)
    {
       dy = y[j+1] - y[i];
       wy = (yI - y[j]) / dy;
    }

    scalar a0 = 1.0 - wx - wy + (wx * wy);
    scalar a1 = wy - (wx * wy);
    scalar a2 = wx - (wx * wy);
    scalar a3 = wx * wy;

    f11 = f[i][j];
    if (ny > 1)
    {
       f12 = f[i][j+1];
    }
    if (nx > 1)
    {
       f21 = f[i+1][j];
    }
    if ((nx > 1) && (ny > 1))
    {
       f22 = f[i+1][j+1];
    }

    Type fI = a0*f11 + a1*f12 + a2*f21 + a3*f22;

    return fI;
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// ************************************************************************* //
