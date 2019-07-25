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

#include "drivingForce.H"

// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //


template<class Type>
void Foam::drivingForce<Type>::initializeController_()
{
    label Nz = zPlanes_.numberOfPlanes();

    // Initialize integrated error
    errorInt_ = List<Type>(Nz,zeroTensor_());

    //Initialize weighted regression
    if (regSmoothing_)
    {
        // Matrix X
        scalarRectangularMatrix X(Nz,Nreg_+1);
        forAllPlanes(zPlanes_,planeI)
        {
            for (label i = 0; i < Nreg_+1; i++)
            {
                X[planeI][i] = Foam::pow(zPlanes_.planeLocationValues()[planeI]/max(zPlanes_.planeLocationValues()),i);
            }
        }
    
        scalarDiagonalMatrix W(Nz,0.0);
        forAllPlanes(zPlanes_,i)
        {
            W[i] = weights_[i];
        }
    
        //Compute regression matrix
        //- XtWX = X.T W X
        scalarRectangularMatrix XtWX(Nreg_+1,Nreg_+1);
        multiply(XtWX,X.T(),W,X);
    
        //- compute inverse of XtWX by
        //  converting to symmetricSquareMatrix.
        //  SVDinv could also be used but is less accurate
        //  scalarRectangularMatrix XtWXinv = SVDinv(XtWX);
        scalarSymmetricSquareMatrix Z(Nreg_+1);
        for (label i = 0; i < Nreg_+1; i++)
        {
            for (label j=0; j < Nreg_+1; j++)
            {
                Z[i][j] = XtWX[i][j];
            }
        }
        scalarSymmetricSquareMatrix Zinv=inv(Z);
        scalarRectangularMatrix XtWXinv(Nreg_+1,Nreg_+1);
        for (label i = 0; i < Nreg_+1; i++)
        {
            for (label j=0; j < Nreg_+1; j++)
            {
                XtWXinv[i][j] = Zinv[i][j];
            }
        }
        
        //- Areg = (X.T W X)^-1 X.T
        multiply(Areg_,XtWXinv,X.T());
    }
}


template<class Type>
List<Type> Foam::drivingForce<Type>::updateController_
(
    List<Type>& error
)
{
    // Get the current time step size.
    scalar dt = runTime_.deltaT().value();

    // Initialize auxiliary arrays
    List<Type> smoothError(error.size(),zeroTensor_());
    List<Type> effectiveError(error.size(),zeroTensor_());
    List<Type> source(error.size(),zeroTensor_());

    // Apply regression smoothing
    if (regSmoothing_)
    {
        scalarRectangularMatrix beta = computeRegressionCoeff_(error);
        smoothError = constructRegressionCurve_(beta);
    }
    else
    {
        smoothError = error;
    }

    // PI control
    errorInt_ = Foam::exp(-dt/timeWindow_) * errorInt_ + dt/timeWindow_*smoothError;
    effectiveError  = alpha_*smoothError + (1.0-alpha_) * errorInt_;

    source = gain_ * effectiveError;

    return source;
}


template<class Type>
scalarRectangularMatrix Foam::drivingForce<Type>::computeRegressionCoeff_
(
    List<Type>& y
)
{
    label Nz = zPlanes_.numberOfPlanes();

    scalarRectangularMatrix Wy(Nz,Type::nComponents);
    for (label i = 0; i < Nz; i++)
    {
        for (label j = 0; j < Type::nComponents; j++)
        {
            Wy[i][j] = weights_[i] * y[i][j];
        }
    }

    //Compute coefficients
    scalarRectangularMatrix beta(Nreg_+1,Type::nComponents);
    multiply(beta,Areg_,Wy);
    return beta;
}


template<class Type>
List<Type> Foam::drivingForce<Type>::constructRegressionCurve_
(
    scalarRectangularMatrix& beta
)
{
    label Nz = zPlanes_.numberOfPlanes();

    //Compute regression line
    List<Type> y(Nz,zeroTensor_());
    for (label i = 0; i < Nz; i++)
    {
        for (label j = 0; j < Nreg_+1; j++)
        {
            for (label k = 0; k < Type::nComponents; k++)
            {
                y[i][k] += beta[j][k] * Foam::pow(
                    zPlanes_.planeLocationValues()[i]/max(zPlanes_.planeLocationValues()),j);
            }
        }
    }
    return y;
}


// Specialization for scalar
// Type scalar does not have nComponent nor componentNames
namespace Foam
{
    template<>
    scalarRectangularMatrix drivingForce<scalar>::computeRegressionCoeff_
    (
        List<scalar>& y
    )
    {
        label Nz = zPlanes_.numberOfPlanes();
    
        scalarRectangularMatrix Wy(Nz,1);
        for (label i = 0; i < Nz; i++)
        {
            Wy[i][0] = weights_[i] * y[i];
        }
    
        //Compute coefficients
        scalarRectangularMatrix beta(Nreg_+1,1);
        multiply(beta,Areg_,Wy);
        return beta;
    }

    template<>
    List<scalar> drivingForce<scalar>::constructRegressionCurve_
    (
        scalarRectangularMatrix& beta
    )
    {
        label Nz = zPlanes_.numberOfPlanes();

        //Compute regression line
        List<scalar> y(Nz,zeroTensor_());
        for (label i = 0; i < Nz; i++)
        {
            for (label j = 0; j < Nreg_+1; j++)
            {
                y[i] += beta[j][0] * pow(
                    zPlanes_.planeLocationValues()[i]/max(zPlanes_.planeLocationValues()),j);
            }
        }
        return y;
    }
}


// ************************************************************************* //
