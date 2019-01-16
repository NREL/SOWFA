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

#include "DrivingForce.H"

// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //


template<class Type>
void Foam::DrivingForce<Type>::initializeController_
(
    label nSourceHeights
)
{
    errorInt_ = List<Type>(nSourceHeights,zeroTensor_());

    //Initialize weighted regression
    
    label Nz = zPlanes_.numberOfPlanes();
    // Regression order
    Nreg_ = 3;
    // Matrix X
    scalarRectangularMatrix X(Nz,Nreg_+1);
    forAllPlanes(zPlanes_,planeI)
    {
        for (label i = 0; i < Nreg_+1; i++)
        {
            X[planeI][i] = pow(zPlanes_.planeLocationValues()[planeI]/max(zPlanes_.planeLocationValues()),i);
        }
    }
    // Weights
    forAllPlanes(zPlanes_,planeI)
    {
        weights_.append(
                min( 1.0,2.0*zPlanes_.planeLocationValues()[planeI]/max(zPlanes_.planeLocationValues()) )
                        );
    }

    scalarDiagonalMatrix W(Nz);
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


template<class Type>
List<Type> Foam::DrivingForce<Type>::updateController_
(
    List<Type>& error
)
{
    // Get the current time step size.
    scalar dt = runTime_.deltaT().value();
    
    // Compute controller action
    List<Type> source(error.size(),zeroTensor_());
    //List<Type> KpError = deadBand_(error);
    //List<Type> KpError = error;
    List<Type> KpError = weightedRegression_(error);

    for (label i = 0; i < error.size(); i++)
    {
        //errorInt_[i] += KpError[i] * dt/Ti_;
        //errorInt_[i] += error[i] * dt/Ti_;

        //source[i] = alpha_/dt * ( KpError[i] + errorInt_[i] );
        source[i] = alpha_/dt * KpError[i];
    }
    return source;
}


template<class Type>
List<Type> Foam::DrivingForce<Type>::weightedRegression_
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


    //Compute regression line
    List<Type> yRegression(Nz);
    for (label i = 0; i < Nz; i++)
    {
        for (label j = 0; j < Nreg_; j++)
        {
            for (label k = 0; k < Type::nComponents; k++)
            {
                yRegression[i][k] = beta[j][k] * pow(
                    zPlanes_.planeLocationValues()[i]/max(zPlanes_.planeLocationValues()),j);
            }
        }
    }

    return yRegression;

}


template<class Type>
List<Type> Foam::DrivingForce<Type>::deadBand_
(
    List<Type>& x
)
{
    List<Type> x1(x.size(),zeroTensor_());
    List<Type> x2(x.size(),zeroTensor_());
    List<Type> deadBand(x.size(),zeroTensor_());

    x1 = (x+deadBandWidth2_/2.*unitTensor_())/((deadBandWidth2_ - deadBandWidth1_)/2.);
    x2 = (x-deadBandWidth2_/2.*unitTensor_())/((deadBandWidth2_ - deadBandWidth1_)/2.) + unitTensor_();

    deadBand = smoothStep_(x2) - smoothStep_(x1) + unitTensor_();

    List<Type> KpError(x.size(),Type::zero);

    for (label i = 0; i < x.size(); i++)
    {
        for (label j = 0; j < Type::nComponents; j++)
        {
            KpError[i][j] = deadBand[i][j] * x[i][j];
        }
    }
    return KpError;
}


template<class Type>
List<Type> Foam::DrivingForce<Type>::smoothStep_
(
    List<Type>& x
)
{
    List<Type> step(x.size(),zeroTensor_());
    for (label i = 0; i < x.size(); i++)
    {
        Type s(Type::zero);
        for (label j = 0; j < Type::nComponents; j++)
        {
            //s[j] = (x<=0.0) * 0.0;
            s[j]  = ((x[i][j]>0.0) && (x[i][j]<1.0)) * ( 0.5 + 0.5*sin( constant::mathematical::pi*(x[i][j]-0.5) ) );
            s[j] += (x[i][j]>=1.0) * 1.0;
        }
        step[i] = s;
    }
    return step;
}


// Specialization for scalar
// Type scalar does not have nComponent nor componentNames
namespace Foam
{
    template<>
    List<scalar> DrivingForce<scalar>::weightedRegression_
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
    
    
        //Compute regression line
        List<scalar> yRegression(Nz);
        for (label i = 0; i < Nz; i++)
        {
            for (label j = 0; j < Nreg_; j++)
            {
                yRegression[i] = beta[j][0] * pow(
                    zPlanes_.planeLocationValues()[i]/max(zPlanes_.planeLocationValues()),j);
            }
        }
    
        return yRegression;
    
    }


    template<>
    List<scalar> DrivingForce<scalar>::deadBand_
    (
        List<scalar>& x
    )
    {
        List<scalar> x1(x.size(),zeroTensor_());
        List<scalar> x2(x.size(),zeroTensor_());
        List<scalar> deadBand(x.size(),zeroTensor_());
    
        x1 = (x+deadBandWidth2_/2.*unitTensor_())/((deadBandWidth2_ - deadBandWidth1_)/2.);
        x2 = (x-deadBandWidth2_/2.*unitTensor_())/((deadBandWidth2_ - deadBandWidth1_)/2.) + unitTensor_();
    
        deadBand = smoothStep_(x2) - smoothStep_(x1) + unitTensor_();
    
        List<scalar> KpError(x.size(),zeroTensor_());
    
        for (label i = 0; i < x.size(); i++)
        {
            KpError[i] = deadBand[i] * x[i];
        }
        return KpError;
    }


    template<>
    List<scalar> DrivingForce<scalar>::smoothStep_
    (
        List<scalar>& x
    )
    {
        List<scalar> step(x.size(),0.0);
        for (label i = 0; i < x.size(); i++)
        {
            //step[i] = (x[i]<=0.0) * 0.0;
            step[i]  = ((x[i]>0.0) && (x[i]<1.0)) * ( 0.5 + 0.5*sin( constant::mathematical::pi*(x[i]-0.5) ) );
            step[i] += (x[i]>=1.0) * 1.0;
        }
        return step;
    }
}


// ************************************************************************* //
