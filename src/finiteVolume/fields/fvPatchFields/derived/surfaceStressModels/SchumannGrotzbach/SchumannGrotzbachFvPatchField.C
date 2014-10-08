/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 1991-2009 OpenCFD Ltd.
     \\/     M anipulation  |
-------------------------------------------------------------------------------
License
    This file is part of OpenFOAM.

    OpenFOAM is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 2 of the License, or (at your
    option) any later version.

    OpenFOAM is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with OpenFOAM; if not, write to the Free Software Foundation,
    Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

\*---------------------------------------------------------------------------*/

#include "SchumannGrotzbachFvPatchField.H"
#include "singlePhaseTransportModel.H"
#include "fvPatchFieldMapper.H"
#include "volFields.H"
#include "addToRunTimeSelectionTable.H"
#include "uniformDimensionedFields.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

SchumannGrotzbachFvPatchField::
SchumannGrotzbachFvPatchField
(
    const fvPatch& p,
    const DimensionedField<symmTensor, volMesh>& iF
)
:
    fixedValueFvPatchSymmTensorField(p, iF),
    kappa_(0.40),
    z0_(p.size(), 0.01),
    betaM_(15.0),
    gammaM_(4.7),
    averageType_("local")
{}


SchumannGrotzbachFvPatchField::
SchumannGrotzbachFvPatchField
(
    const SchumannGrotzbachFvPatchField& ptf,
    const fvPatch& p,
    const DimensionedField<symmTensor, volMesh>& iF,
    const fvPatchFieldMapper& mapper
)
:
    fixedValueFvPatchSymmTensorField(ptf, p, iF, mapper),
    kappa_(ptf.kappa_),
    z0_(ptf.z0_, mapper),
    betaM_(ptf.betaM_),
    gammaM_(ptf.gammaM_),
    averageType_(ptf.averageType_)
{}


SchumannGrotzbachFvPatchField::
SchumannGrotzbachFvPatchField
(
    const fvPatch& p,
    const DimensionedField<symmTensor, volMesh>& iF,
    const dictionary& dict
)
:
    fixedValueFvPatchSymmTensorField(p, iF, dict),
    kappa_(readScalar(dict.lookup("kappa"))),
    z0_("z0", dict, p.size()),
    betaM_(readScalar(dict.lookup("betaM"))),
    gammaM_(readScalar(dict.lookup("gammaM"))),
    averageType_(dict.lookupOrDefault<word>("averageType","local"))
{}


SchumannGrotzbachFvPatchField::
SchumannGrotzbachFvPatchField
(
    const SchumannGrotzbachFvPatchField& wfpf
)
:
    fixedValueFvPatchSymmTensorField(wfpf),
    kappa_(wfpf.kappa_),
    z0_(wfpf.z0_),
    betaM_(wfpf.betaM_),
    gammaM_(wfpf.gammaM_),
    averageType_(wfpf.averageType_)
{}


SchumannGrotzbachFvPatchField::
SchumannGrotzbachFvPatchField
(
    const SchumannGrotzbachFvPatchField& wfpf,
    const DimensionedField<symmTensor, volMesh>& iF
)
:
    fixedValueFvPatchSymmTensorField(wfpf, iF),
    kappa_(wfpf.kappa_),
    z0_(wfpf.z0_),
    betaM_(wfpf.betaM_),
    gammaM_(wfpf.gammaM_),
    averageType_(wfpf.averageType_)
{}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

void SchumannGrotzbachFvPatchField::evaluate
(
    const Pstream::commsTypes
)
{

    // ---Get preliminary information
    //    Get face normal vectors
    const vectorField normal = patch().nf();

    //    Get face areas (individual and global sum) (note that "gSum" is used as opposed
    //    to "sum" because "gSum" is parallel-aware --- it gathers sums from each processor
    //    to which this patch belongs and makes the global sum)
    const scalarField area = patch().magSf();
    const scalar areaTotal = gSum(area);

    //    Get the gravity vector
    const uniformDimensionedVectorField& gVec = db().lookupObject<uniformDimensionedVectorField>("g");
    const scalar g = mag(gVec.value());
    //Info << "gVec = " << gVec << tab
    //     << "g    = " << g    << endl;

    //    Get perpendicular distance from cell center to boundary
    const scalarField z1 = 1.0/patch().deltaCoeffs();
    scalar z1Mean = gSum(z1 * area)/areaTotal;

    //    Get the average surface roughness height
    scalar z0Mean = gSum(z0_ * area)/areaTotal;

    //    Get the reference temperature
    const singlePhaseTransportModel& laminarTransport = db().lookupObject<singlePhaseTransportModel>("transportProperties");
    const dimensionedScalar& TRefDim = laminarTransport.lookup("TRef");
    scalar TRef = TRefDim.value();
    //Info << "TRef = " << TRef << endl;


    vectorField loc = patch().Cf();



    // ---Get the velocity parallel to the boundary using,
    //
    //     U_|| = U_1 - ((U_1 dot n_f) * n_f), where U_||,
    //
    //    where U_|| is the parallel velocity vector, U_1 is the cell center velocity, and
    //    n_f is the surface face normal unit vector.
    //    Get the velocity in the cells adjacent to the boundary
    const fvPatchVectorField& UPatch = patch().lookupPatchField<volVectorField, vector>("U");
    vectorField UParallel = UPatch.patchInternalField();
    UParallel = UParallel - ((UParallel & normal) * normal);
    vector UParallelMean = gSum(UParallel * area) / areaTotal;
    scalar UParallelMeanMag = mag(UParallelMean);

    // ---Get the velocity parallel to the boundary in terrain-local coordinates
    vectorField UParallelP = UParallel;
    forAll(UParallelP, facei)
    {
        // Transform from the Cartesian (x,y,z) system into the local (x',y',z')
        // system

        // z' is equal to the surface normal pointing inward (negative because
        // OpenFOAM normal is outward)
        vector zP;
        zP = -normal[facei];

        // x' is pointed in the direction of the parallel resolved velocity at this cell
        vector xP;
        xP = UParallel[facei];

        // y' is orthogonal to x' and z', so it can be found with cross product
        vector yP;
        yP = zP ^ xP;

        // Transform the velocity from Cartesian to local
        UParallelP[facei] = transformVectorCartToLocal(UParallel[facei], xP, yP, zP);

        //Info << "facei = " << facei << tab
        //     << "UParallel  = " << UParallel[facei] << tab
        //     << "UParallelP = " << UParallelP << tab;

    }

    //    Get magnitudes and means of the terrain-local velocity
    scalarField UParallelPMag = mag(UParallelP);
    scalar UParallelPMagMean = gSum(UParallelPMag * area) / areaTotal;
    vector UParallelPMean = gSum(UParallelP * area) / areaTotal;
    scalar UParallelPMeanMag = mag(UParallelPMean);






    // ---Get the boundary temperature flux
    const fvPatchField<vector>& qwVec = patch().lookupPatchField<volVectorField, vector>("qwall");
    scalarField qw = qwVec & normal;
    scalar qwMean = gSum(qw * area)/areaTotal;
    //Info << "qw = " << qw << tab
    //     << "qwMean = " << qwMean << endl;






    // ---Compute the friction velocity, Obuhkov length, and non-dimensional shear
    //    Define friction velocity
    scalarField uStar(patch().size(),0.0);

    //    Define Obhukov length
    scalarField L(patch().size(),0.0);
 
    //    Define non-dimensional shear
    scalarField phiM(patch().size(),0.0);

    forAll(uStar, facei)
    {
        if(averageType_ == "planarAverage")
        {
//            Info << "planarAverage" << endl;
            if(facei == 0)
            {
                uStarEvaluate
                (
                    uStar[facei],
                    L[facei],
                    phiM[facei],
                  //UParallelPMagMean,
                    UParallelMeanMag,
                    z1Mean,
                    z0Mean,
                    kappa_,
                    gammaM_,
                    betaM_,
                    g,
                    TRef,
                    qwMean,
                    1.0E-1,
                    1.0E-8,
                    1000
                );
            }
            else
            {
                uStar[facei] = uStar[0];
                L[facei] = L[0];
                phiM[facei] = phiM[0];
            }
       
        }
        else if (averageType_ == "local")
        {
    //        Info << "local -- no average" << endl;
            uStarEvaluate
            (
                uStar[facei],
                L[facei],
                phiM[facei],
                UParallelPMag[facei],
                z1[facei],
                z0_[facei],
                kappa_,
                gammaM_,
                betaM_,
                g,
                TRef,
                qw[facei],
                1.0E-1,
                1.0E-8,
                1000
            );

        }

    }
   
    //    Get average friction velocity
    scalar uStarMean = gSum(uStar * area) / areaTotal;
    scalar LMean = gSum(L * area) / areaTotal;
    scalar phiMMean = gSum(phiM * area) / areaTotal;
    Info << "uStarMean = " << uStarMean << tab
         << "LMean = " << LMean << tab
         << "phiMMean = " << phiMMean << endl;
    Info << "UParallelMeanMag = " << UParallelMeanMag << tab
         << "UParallelPMeanMag = " << UParallelPMeanMag << endl;






    // ---Specify surface shear stresses (Schumann formulation)
    symmTensorField& Rw = *this;
    scalarField RwMag(patch().size(),0.0);

    forAll(Rw, facei)
    {
        // Form the wall shear stress tensor in terrain-local coordinates
        symmTensor RwP(symmTensor::zero);
        vector xP(vector::zero);
        vector yP(vector::zero);
        vector zP(vector::zero);
        if (averageType_ == "planarAverage")
        {
//            Info << "planarAverage" << endl;
            Rw[facei].xx() = 0.0;
	    Rw[facei].xy() = 0.0;
	  //RwP.xz() = -sqr(uStar[facei]) * (UParallelP[facei].x() / UParallelPMagMean);
	    Rw[facei].xz() = -sqr(uStar[facei]) * (UParallel[facei].x() / max(UParallelMeanMag, 1.0E-5));
	    Rw[facei].yy() = 0.0;
	  //RwP.yz() = -sqr(uStar[facei]) * (UParallelP[facei].y() / UParallelPMagMean);
	    Rw[facei].yz() = -sqr(uStar[facei]) * (UParallel[facei].y() / max(UParallelMeanMag, 1.0E-5));
	    Rw[facei].zz() = 0.0;
            
            // Get the magnitude of the surface stress vector
            RwMag[facei] = Foam::sqrt(Foam::sqr(Rw[facei].xz()) + Foam::sqr(Rw[facei].yz()));
        }
        else if (averageType_ == "local")
        {
  //          Info << "local" << endl;
            RwP.xx() = 0.0;
            RwP.xy() = 0.0;
            RwP.xz() = -sqr(uStar[facei]) * (UParallelP[facei].x() / max(UParallelPMag[facei], 1.0E-5));
            RwP.yy() = 0.0;
            RwP.yz() = -sqr(uStar[facei]) * (UParallelP[facei].y() / max(UParallelPMag[facei], 1.0E-5));
            RwP.zz() = 0.0;
        

            // Transform from the terrain-local into the Cartesian system

            // z' is equal to the surface normal pointing inward (negative because
            // OpenFOAM normal is outward)
            zP = -normal[facei];
 
            // x' is pointed in the direction of the parallel resolved velocity at this cell
            xP = UParallel[facei];

            // y' is orthogonal to x' and z', so it can be found with cross product
            yP = zP ^ xP;

            // Perform the transformation
            Rw[facei] = transformSymmTensorLocalToCart(RwP, xP, yP, zP);

            // Get the magnitude of the surface stress vector
            RwMag[facei] = Foam::sqrt(Foam::sqr(RwP.xz()) + Foam::sqr(RwP.yz()));
        }

        //Info << "facei = " << facei << tab
        //     << "Rw  = " << Rw[facei] << endl;
        //scalar degToRad = Foam::constant::mathematical::pi/180;
        //scalar angle = 30.0;
        //xP.x() = Foam::cos(angle*degToRad);
        //xP.y() = 0;
        //xP.z() = Foam::sin(angle*degToRad);
        //yP.x() = 0;
        //yP.y() = 1;
        //yP.z() = 0;
        //zP.x() = -Foam::sin(angle*degToRad);
        //zP.y() = 0;
        //zP.z() = Foam::cos(angle*degToRad);
        //symmTensor Sp(symmTensor::zero);
        //Sp.xz() = -1;
        //Sp.yz() = -0.1;
        //symmTensor S;
        //S = transformSymmTensorLocalToCart(Sp, xP, yP, zP);
        //if (facei == 0)
        //{
        //   Info << "Sp = " << Sp << endl;
        //   Info << "S = " << S << endl; 
        //}

        //if ((loc[facei].x() < 1501 && loc[facei].x() > 1499) &&
        //    (loc[facei].y() < 1501 && loc[facei].y() > 1499))
        //{
        //    Pout << "loc = " << loc[facei] << endl;
        //    Pout << "UParallel " << UParallel[facei] << endl;
        //    Pout << "UParallelP " << UParallelP[facei] << endl;
        //    Pout << "xP, yP, zP " << xP << tab << yP << tab << zP << endl;
        //    Pout << "RwP " << RwP << endl;
        //    Pout << "Rw " << Rw[facei] << endl;
        //}
    }
    symmTensor RwMean = gSum(Rw * area) / areaTotal;
    scalar RwMeanMag = Foam::sqrt(Foam::sqr(RwMean.xz()) + Foam::sqr(RwMean.yz()));
    scalar RwMagMean = gSum(RwMag * area) / areaTotal;

    Info << "RwMagMean = " << RwMagMean << tab
         << "RwMeanMag = " << RwMeanMag << tab
         << "sqrt(RwMagMean) = " << Foam::sqrt(RwMagMean) << tab
         << "sqrt(RwMeanMag) = " << Foam::sqrt(RwMeanMag) << tab
         << "uStarMean = " << uStarMean << endl;
}

vector SchumannGrotzbachFvPatchField::transformVectorCartToLocal
(
    vector v, 
    vector xP, 
    vector yP, 
    vector zP
)
{
    // Transform from the Cartesian (x,y,z) system into the local (x',y',z')
    // system
    //
    //    x' is aligned with the flow
    //    y' is the cross product of z' and x'
    //    z' is in the boundary face normal direction
    //
    // These vectors are unit vectors.  The vectors make up the rows of
    // the rotation matrix T', which rotates from (x,y,z) to (x',y',z').

    // z' is equal to the surface normal pointing inward (negative because
    // OpenFOAM normal is outward)
    scalar zPMag;
    zPMag = mag(zP);
    zP = zP/zPMag;

    // x' is pointed in the direction of the parallel resolved velocity at this cell
    scalar xPMag;
    xPMag = mag(xP);
    xP = xP/xPMag;

    // y' is orthogonal to x' and z', so it can be found with cross product
    scalar yPMag;
    yPMag = mag(yP);
    yP = yP/yPMag;

    // Create T'
    tensor TP;
    TP.xx() = xP.x();
    TP.xy() = xP.y();
    TP.xz() = xP.z();
    TP.yx() = yP.x();
    TP.yy() = yP.y();
    TP.yz() = yP.z();
    TP.zx() = zP.x();
    TP.zy() = zP.y();
    TP.zz() = zP.z();

    // Transform the vector from Cartesian to local
    vector vP = TP & v; 

    //Info << "xP = " << xP << tab
    //     << "yP = " << yP << tab
    //     << "zP = " << zP << tab
    //     << "v  = " << v  << tab
    //     << "vP = " << vP << endl;

    return vP;
}

symmTensor SchumannGrotzbachFvPatchField::transformSymmTensorLocalToCart
(
    symmTensor SP,
    vector xP,
    vector yP,
    vector zP
)
{
    // Transform from the local (x',y',z') system into the Cartesian (x,y,z)
    // system
    //
    //    x' is aligned with the flow
    //    y' is the cross product of z' and x'
    //    z' is in the boundary face normal direction
    //
    // These vectors are unit vectors.  The vectors make up the rows of
    // the rotation matrix, T', which rotates from (x,y,z) to (x',y',z').
    // T'^-1 = T transforms from (x',y',z') to (x,y,z).  Note that T' is 
    // such that the (T')^-1 = transpose(T') because it is made up of
    // orthogonal basis vectors.  A tensor can be transformed from (x',y',z')
    // to (x,y,z) using S_ij = T * S_ij' * T'.

    // z' is equal to the surface normal pointing inward (negative because
    // OpenFOAM normal is outward)
    scalar zPMag;
    zPMag = mag(zP);
    zP = zP/zPMag;

    // x' is pointed in the direction of the parallel resolved velocity at this cell
    scalar xPMag;
    xPMag = mag(xP);
    xP = xP/xPMag;

    // y' is orthogonal to x' and z', so it can be found with cross product
    scalar yPMag;
    yPMag = mag(yP);
    yP = yP/yPMag;

    // Create T'
    tensor TP;
    TP.xx() = xP.x();
    TP.xy() = xP.y();
    TP.xz() = xP.z();
    TP.yx() = yP.x();
    TP.yy() = yP.y();
    TP.yz() = yP.z();
    TP.zx() = zP.x();
    TP.zy() = zP.y();
    TP.zz() = zP.z();

    // Create T by transposing T' to get T'^-1
    tensor T;
    T = TP.T();

    // Transform the symmetric tensor by doing S_ij = T * S_ij' * T'
    tensor Ss;
    Ss = T & SP & TP;

    // OpenFOAM will not allow a tensor & tensor operation resulting in a symmTensor,
    // so the work around was to make Ss a tensor, even though it ends up being a 
    // symmetric one, and then copy the relevant components into symmTensor S
    symmTensor S(symmTensor::zero);
    S.xx() = Ss.xx();
    S.xy() = Ss.xy();
    S.xz() = Ss.xz();
    S.yy() = Ss.yy();
    S.yz() = Ss.yz();
    S.zz() = Ss.zz();

    //Info << "SP = " << SP << endl
    //     << "S  = " << S  << endl;

    return S;
}

void  SchumannGrotzbachFvPatchField::uStarEvaluate
(
    scalar& uStar, 
    scalar& L, 
    scalar& phiM, 
    scalar U, 
    scalar z1, 
    scalar z0, 
    scalar kappa, 
    scalar gammaM, 
    scalar betaM, 
    scalar g, 
    scalar TRef, 
    scalar qw, 
    scalar eps, 
    scalar tol, 
    label iterMax
)
{
            // To solve for u*, the Monin-Obuhkov similarity laws are used.  For neutral
            // conditions, this is the simple log law, and u* can be found explicitly.
            // For unstable or stable conditions, u* must be found iteratively, and below
            // is a nonlinear Newton solver for finding u*

            // Set iterator to zero
            label iter = 0;

            // Set initial guesses at u*
            scalar uStar0 = (kappa * U) / Foam::log(z1 / z0);
            scalar uStar1 = (1.0 + eps) * uStar0;

            // Limit u* to always be zero or positive
            uStar0 = max(0.0, uStar0);
            uStar1 = max(0.0, uStar1);

            // neutral
            if (qw == 0.0)
            {
            //  Info << "Neutral" << endl;

                uStar = uStar0;
                L = 1.0E30;
                phiM = 1.0;
            }

            // --stable
            // see book "Modelling of Atmospheric Flow Field", editors D. Lalas and C. Ratto
            // chapter 2--Modelling the Vertical ABL Structure, D. Etling, pp. 56--57
            else if (qw < 0.0)
            {
                label procPrint = -1;
                if (Pstream::myProcNo() == procPrint)
                {
                Pout << "Stable" << endl;
                Pout << "U = " << U << endl;
                }

                scalar f0 = 1E30;
                scalar f1 = 1E30;

                do
                {
                    iter++;


                    if (Pstream::myProcNo() == procPrint)
                    {
                    Pout << "iter = " << iter << endl;
                    Pout << "uStar0 = " << uStar0 << endl;
                    Pout << "uStar1 = " << uStar1 << endl;
                    Pout << "qw = " << qw << endl;
                    }

                    // set initial guesses at L
                    scalar L0 = -(Foam::pow(uStar0,3.0)) / (kappa * (g/TRef) * qw);
                    scalar L1 = -(Foam::pow(uStar1,3.0)) / (kappa * (g/TRef) * qw);

                    // limit L to always be positive and finite
                    L0 = max(1.0E-10,L0);
                    L1 = max(1.0E-10,L1);

                    if (Pstream::myProcNo() == procPrint)
                    {
                    Pout << "L0 = " << L0 << endl;
                    Pout << "L1 = " << L1 << endl;
                    }

                    // form the "zeta" variable, which is z/L
                    scalar zeta0 = z1/L0;
                    scalar zeta1 = z1/L1;

                    if (Pstream::myProcNo() == procPrint)
                    {
                    Pout << "zeta0 = " << zeta0 << endl;
                    Pout << "zeta1 = " << zeta1 << endl;
                    }

                    // form psiM
                    scalar psiM0 = -gammaM * zeta0;
                    scalar psiM1 = -gammaM * zeta1;

                    if (Pstream::myProcNo() == procPrint)
                    {
                    Pout << "psiM0 = " << psiM0 << endl;
                    Pout << "psiM1 = " << psiM1 << endl;
                    }

                    // form the function that we are driving to zero
                    //f0 = U - (uStar0/kappa)*(Foam::log(z1/z0) - psiM0);
                    //f1 = U - (uStar1/kappa)*(Foam::log(z1/z0) - psiM1);
                    scalar denom0 = Foam::log(z1/z0) - psiM0;
                    scalar denom1 = Foam::log(z1/z0) - psiM1;
                    if (denom0 == 0.0)
                    {
                        denom0 = 1.0E-10;
                    }
                    if (denom1 == 0.0)
                    {
                        denom1 = 1.0E-10;
                    }
                    f0 = uStar0 - ((U * kappa) / denom0);
                    f1 = uStar1 - ((U * kappa) / denom1);

                    if (Pstream::myProcNo() == procPrint)
                    {
                    Pout << "f0 = " << f0 << endl;
                    Pout << "f1 = " << f1 << endl;
                    }

                    // update uStar

                    scalar slope = (f1 - f0) / (uStar1 - uStar0);
                    if (slope == 0.0)
                    {
                        slope = 1.0E-10;
                    }
                    if (Pstream::myProcNo() == procPrint)
                    {
                    Pout << "slope = " << slope << endl;
                    }

                    scalar uStar1Old = uStar1;
                    uStar1 = max(0.0, uStar0 - (f0 / slope));

                    if (Pstream::myProcNo() == procPrint)
                    {
                    Pout << "uStar1 = " << uStar1 << endl;
                    }

                    uStar0 = uStar1Old;
                    if (Pstream::myProcNo() == procPrint)
                    {
                    Pout << "uStar0 = " << uStar0 << endl;
                    }

                    // in case this is converged, calculate uStar, L, and phiM
                    uStar = uStar1;
                    L = L1;
                    phiM = 1.0 + (gammaM * zeta1);

                    

                } while ((mag(f1) > tol) && (uStar0 != uStar1) && (iter < iterMax));

                if (iter >= iterMax-1)
                {
                    Info << "Max uStar iterations reached!!!" << endl;
                }

            }

            // --unstable
            // see book "Modelling of Atmospheric Flow Field", editors D. Lalas and C. Ratto
            // chapter 2--Modelling the Vertical ABL Structure, D. Etling, pp. 56--57 and
            // article "The Mathematical Representation of Wind Speed and Temperature Profiles
            // in the Unstable Atmospheric Surface Layer", C. Paulson, Journal of Applied
            // Meteorology, Vol 9, 1970, pp. 857--861.
            else if (qw > 0.0)
            {
                //Info << "Unstable" << endl;

                scalar f0 = 0;
                scalar f1 = 0;

                do
                {
                    iter++;

                    // set initial guesses at L
                    scalar L0 = -(Foam::pow(uStar0,3.0)) / (kappa * (g/TRef) * qw);
                    scalar L1 = -(Foam::pow(uStar1,3.0)) / (kappa * (g/TRef) * qw);

                    // limit L to always be negative and finite
                    L0 = min(-1.0E-10,L0);
                    L1 = min(-1.0E-10,L1);

                    // form the "zeta" variable, which is z/L
                    scalar zeta0 = z1/L0;
                    scalar zeta1 = z1/L1;

                    // form the "x" variable in Paulson's paper
                    scalar x0 = Foam::pow((1.0 - (betaM * zeta0)),0.25);
                    scalar x1 = Foam::pow((1.0 - (betaM * zeta1)),0.25);

                    // form psiM
                    scalar psiM0 = 2.0*Foam::log((1.0+x0)/2.0) + Foam::log((1.0 + Foam::pow(x0,2.0))/2.0) - 2.0*Foam::atan(x0) + Foam::constant::mathematical::pi/2.0;
                    scalar psiM1 = 2.0*Foam::log((1.0+x1)/2.0) + Foam::log((1.0 + Foam::pow(x1,2.0))/2.0) - 2.0*Foam::atan(x1) + Foam::constant::mathematical::pi/2.0;

                    // form the function that we are driving to zero
                    //f0 = U - (uStar0/kappa)*(Foam::log(z1/z0) - psiM0);
                    //f1 = U - (uStar1/kappa)*(Foam::log(z1/z0) - psiM1);
                    scalar denom0 = Foam::log(z1/z0) - psiM0;
                    scalar denom1 = Foam::log(z1/z0) - psiM1;
                    if (denom0 == 0.0)
                    {
                        denom0 = 1.0E-10;
                    }
                    if (denom1 == 0.0)
                    {
                        denom1 = 1.0E-10;
                    }
                    f0 = uStar0 - ((U * kappa) / denom0);
                    f1 = uStar1 - ((U * kappa) / denom1);


                    // update uStar
                    scalar slope = (f1 - f0) / (uStar1 - uStar0);
                    if (slope == 0.0)
                    {
                        slope = 1.0E-10;
                    }
                    scalar uStar1Old = uStar1;
                    uStar1 = max(0.0, uStar0 - (f0 / slope));
                    uStar0 = uStar1Old;

                    // in case this is converged, calculate uStar, L, and phiM
                    uStar = uStar1;
                    L = L1;
                    phiM = Foam::pow((1.0 - (betaM*zeta1)),-0.25);

                } while ((mag(f1) > tol) && (uStar0 != uStar1) && (iter < iterMax));


                if (iter >= iterMax-1)
                {
                    Info << "Max uStar iterations reached!!!" << endl;
                }

            }

            //Info << "iter = " << iter << endl;
            
}

void SchumannGrotzbachFvPatchField::write(Ostream& os) const
{
    fvPatchField<symmTensor>::write(os);
    os.writeKeyword("kappa") << kappa_ << token::END_STATEMENT << nl;
    z0_.writeEntry("z0", os);
    os.writeKeyword("betaM") << betaM_ << token::END_STATEMENT << nl;
    os.writeKeyword("gammaM") << gammaM_ << token::END_STATEMENT << nl;
    os.writeKeyword("averageType") << averageType_ << token::END_STATEMENT << nl;
    writeEntry("value", os);
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

makePatchTypeField
(
    fvPatchSymmTensorField,
    SchumannGrotzbachFvPatchField
);

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// ************************************************************************* //
