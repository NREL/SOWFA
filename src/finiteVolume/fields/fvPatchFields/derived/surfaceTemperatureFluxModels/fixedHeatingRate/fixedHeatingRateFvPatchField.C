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

#include "fixedHeatingRateFvPatchField.H"
#include "singlePhaseTransportModel.H"
#include "fvPatchFieldMapper.H"
#include "volFields.H"
#include "addToRunTimeSelectionTable.H"
#include "uniformDimensionedFields.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

fixedHeatingRateFvPatchField::
fixedHeatingRateFvPatchField
(
    const fvPatch& p,
    const DimensionedField<vector, volMesh>& iF
)
:
    fixedValueFvPatchVectorField(p, iF),
    heatingRate_(p.size(), 0.00),
    TSurface_(p.size(), 300.0),
    kappa_(0.35),
    z0_(p.size(), 0.01),
    betaM_(15.0),
    gammaM_(4.7),
    betaH_(9.0),
    gammaH_(4.7),
    alphaH_(0.74),
    averageType_("local"),
    tLast_(db().time().timeOutputValue())
{}


fixedHeatingRateFvPatchField::
fixedHeatingRateFvPatchField
(
    const fixedHeatingRateFvPatchField& ptf,
    const fvPatch& p,
    const DimensionedField<vector, volMesh>& iF,
    const fvPatchFieldMapper& mapper
)
:
    fixedValueFvPatchVectorField(ptf, p, iF, mapper),
    heatingRate_(ptf.heatingRate_,mapper),
    TSurface_(ptf.TSurface_,mapper),
    kappa_(ptf.kappa_),
    z0_(ptf.z0_, mapper),
    betaM_(ptf.betaM_),
    gammaM_(ptf.gammaM_),
    betaH_(ptf.betaH_),
    gammaH_(ptf.gammaH_),
    alphaH_(ptf.alphaH_),
    averageType_(ptf.averageType_),
    tLast_(ptf.tLast_)
{}


fixedHeatingRateFvPatchField::
fixedHeatingRateFvPatchField
(
    const fvPatch& p,
    const DimensionedField<vector, volMesh>& iF,
    const dictionary& dict
)
:
    fixedValueFvPatchVectorField(p, iF, dict),
    heatingRate_("heatingRate", dict, p.size()),
    TSurface_("TSurface", dict, p.size()),
    kappa_(readScalar(dict.lookup("kappa"))),
    z0_("z0", dict, p.size()),
    betaM_(readScalar(dict.lookup("betaM"))),
    gammaM_(readScalar(dict.lookup("gammaM"))),
    betaH_(readScalar(dict.lookup("betaH"))),
    gammaH_(readScalar(dict.lookup("gammaH"))),
    alphaH_(readScalar(dict.lookup("alphaH"))),
    averageType_(dict.lookupOrDefault<word>("averageType","local")),
    tLast_(db().time().timeOutputValue())
{}


fixedHeatingRateFvPatchField::
fixedHeatingRateFvPatchField
(
    const fixedHeatingRateFvPatchField& wfpf
)
:
    fixedValueFvPatchVectorField(wfpf),
    heatingRate_(wfpf.heatingRate_),
    TSurface_(wfpf.TSurface_),
    kappa_(wfpf.kappa_),
    z0_(wfpf.z0_),
    betaM_(wfpf.betaM_),
    gammaM_(wfpf.gammaM_),
    betaH_(wfpf.betaH_),
    gammaH_(wfpf.gammaH_),
    alphaH_(wfpf.alphaH_),
    averageType_(wfpf.averageType_),
    tLast_(wfpf.tLast_)
{}


fixedHeatingRateFvPatchField::
fixedHeatingRateFvPatchField
(
    const fixedHeatingRateFvPatchField& wfpf,
    const DimensionedField<vector, volMesh>& iF
)
:
    fixedValueFvPatchVectorField(wfpf, iF),
    heatingRate_(wfpf.heatingRate_),
    TSurface_(wfpf.TSurface_),
    kappa_(wfpf.kappa_),
    z0_(wfpf.z0_),
    betaM_(wfpf.betaM_),
    gammaM_(wfpf.gammaM_),
    betaH_(wfpf.betaH_),
    gammaH_(wfpf.gammaH_),
    alphaH_(wfpf.alphaH_),
    averageType_(wfpf.averageType_),
    tLast_(wfpf.tLast_)
{}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

void fixedHeatingRateFvPatchField::evaluate
(
    const Pstream::commsTypes
)
{

    // ---Get preliminary information
    scalar t = db().time().timeOutputValue();
    scalar dt = t - tLast_;



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

    Info << "z1Mean = " << z1Mean << endl;

    //    Get the average surface roughness height
    scalar z0Mean = gSum(z0_ * area)/areaTotal;

    //    Get the reference temperature
    const singlePhaseTransportModel& laminarTransport = db().lookupObject<singlePhaseTransportModel>("transportProperties");
    const dimensionedScalar& TRefDim = laminarTransport.lookup("TRef");
    scalar TRef = TRefDim.value();
    //Info << "TRef = " << TRef << endl;


    vectorField loc = patch().Cf();




    // ---Get the temperature adjacent to the boundary
    const volScalarField& T = db().objectRegistry::lookupObject<volScalarField>("T");
    const fvPatchScalarField& TPatch = T.boundaryField()[patch().index()]; 
    scalarField TAdjacent = TPatch.patchInternalField();
    scalar TAdjacentMean = gSum(TAdjacent * area) / areaTotal;

    // ---If in a new time step, update the surface temperature
    if (t != tLast_)
    {
        Info << "Updating surface temperature" << tab
             << "t = " << t << tab 
             << "tLast = " << tLast_ << tab
             << "dt = " << dt << endl;
        TSurface_ += heatingRate_*dt;
        tLast_ = t;
    }

    // ---Get new average surface temperature
    scalar TSurfaceMean = gSum(TSurface_ * area) / areaTotal;
    
    // ---Find temperature difference between surface and interior
    scalarField deltaT = TAdjacent - TSurface_;
    scalar deltaTMean = gSum(deltaT * area) / areaTotal;
    Info << "TSurfaceMean = " << TSurfaceMean << tab
         << "TAdjacentMean = " << TAdjacentMean << tab
         << "deltaTMean = " << deltaTMean << endl;




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




    // ---Compute the friction velocity, Obuhkov length, non-dimensional shear, and surface temperature flux magnitude.
    //    Define friction velocity
    scalarField uStar(patch().size(),0.0);

    //    Define Obhukov length
    scalarField L(patch().size(),0.0);
 
    //    Define non-dimensional velocity shear
    scalarField phiM(patch().size(),0.0);

    //    Define non-dimensional temperature shear
    scalarField phiH(patch().size(),0.0);
    
    //    Define the surface temperature flux magnitude
    scalarField qw(patch().size(),0.0);

    forAll(uStar, facei)
    {
        if(averageType_ == "planarAverage")
        {
            //Info << "planarAverage" << endl;
            if(facei == 0)
            {
                qwEvaluate
                (
                    qw[facei],
                    uStar[facei],
                    L[facei],
                    phiM[facei],
                    phiH[facei],
                    //UParallelPMagMean,
                    UParallelMeanMag,
                    z1Mean,
                    z0Mean,
                    kappa_,
                    gammaM_,
                    betaM_,
                    gammaH_,
                    betaH_,
                    alphaH_,
                    g,
                    TRef,
                    deltaTMean,
                    1.0E-1,
                    1.0E-8,
                    1000
                );
            }
            else
            {
                qw[facei] = qw[0];
                uStar[facei] = uStar[0];
                L[facei] = L[0];
                phiM[facei] = phiM[0];
                phiH[facei] = phiH[0];
            }
       
        }
        else if (averageType_ == "local")
        {
            //Info << "local -- no average" << endl;
            qwEvaluate
            (
                qw[facei],
                uStar[facei],
                L[facei],
                phiM[facei],
                phiH[facei],
                UParallelPMag[facei],
                z1[facei],
                z0_[facei],
                kappa_,
                gammaM_,
                betaM_,
                gammaH_,
                betaH_,
                alphaH_,
                g,
                TRef,
                deltaT[facei],
                1.0E-1,
                1.0E-8,
                1000
            );

        }

    }
   
    //    Get average values
    scalar uStarMean = gSum(uStar * area) / areaTotal;
    scalar LMean = gSum(L * area) / areaTotal;
    scalar phiMMean = gSum(phiM * area) / areaTotal;
    scalar phiHMean = gSum(phiH * area) / areaTotal;
    scalar qwMean = gSum(qw * area) / areaTotal;
    Info << "uStarMean = " << uStarMean << tab
         << "qwMean = " << qwMean << tab
         << "LMean = " << LMean << tab
         << "phiMMean = " << phiMMean << tab
         << "phiHMean = " << phiHMean << endl;






    // ---Form the surface temperature flux vector that has the proper
    //    magnitude normal to the surface
    vectorField& qwVec = *this;
    scalarField qwMag(patch().size(),0.0);

    forAll(qwVec, facei)
    {
        // Form the temperature flux vector in terrain-local coordinates.
        // It needs to be negative because the OpenFOAM surface normal points
        // outward (temperature flux out of the boundary is seen as positive)
        vector qwVecP(vector::zero);
        qwVecP.x() = 0.0;
        qwVecP.y() = 0.0;
        qwVecP.z() = -qw[facei];

        // Transform from the terrain-local into the Cartesian system

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

        // Transform from local to Cartesian coordinates
        qwVec[facei] = transformVectorLocalToCart(qwVecP, xP, yP, zP);

        // Perform the transformation
//        Rw[facei] = transformSymmTensorLocalToCart(RwP, xP, yP, zP);

        // Get the magnitude of the surface stress vector
//        RwMag[facei] = Foam::sqrt(Foam::sqr(RwP.xz()) + Foam::sqr(RwP.yz()));

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

        if ((loc[facei].x() < 201 && loc[facei].x() > 199) &&
            (loc[facei].y() < 201 && loc[facei].y() > 199))
        {
            Pout << "loc = " << loc[facei] << endl;
            Pout << "UParallel " << UParallel[facei] << endl;
            Pout << "UParallelP " << UParallelP[facei] << endl;
            Pout << "xP, yP, zP " << xP << tab << yP << tab << zP << endl;
            Pout << "qwP " << qwVecP << endl;
            Pout << "qw " << qwVec[facei] << endl;
        }
    }

//    scalar RwMagMean = gSum(RwMag * area) / areaTotal;

 //   Info << "RwMagMean = " << RwMagMean << tab
//         << "sqrt(RwMagMean) = " << Foam::sqrt(RwMagMean) << tab
//         << "uStarMean = " << uStarMean << endl;
}

vector fixedHeatingRateFvPatchField::transformVectorCartToLocal
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

vector fixedHeatingRateFvPatchField::transformVectorLocalToCart
(
    vector vP,
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

    // Create T by transposing T' to get T'^-1
    tensor T;
    T = TP.T();

    // Transform the vector from Cartesian to local
    vector v = T & vP;

    //Info << "xP = " << xP << tab
    //     << "yP = " << yP << tab
    //     << "zP = " << zP << tab
    //     << "v  = " << v  << tab
    //     << "vP = " << vP << endl;

    return v;
}

void  fixedHeatingRateFvPatchField::qwEvaluate
(
    scalar& qw,
    scalar& uStar, 
    scalar& L, 
    scalar& phiM, 
    scalar& phiH,
    scalar U, 
    scalar z1, 
    scalar z0, 
    scalar kappa, 
    scalar gammaM, 
    scalar betaM, 
    scalar gammaH,
    scalar betaH,
    scalar alphaH,
    scalar g, 
    scalar TRef, 
    scalar deltaT, 
    scalar eps, 
    scalar tol, 
    label iterMax
)
{
            // To solve for u* and qw, the Monin-Obuhkov similarity laws are used.  For neutral
            // conditions, this is the simple log law, and u* can be found explicitly.
            // For unstable or stable conditions, u* must be found iteratively, and below
            // is a nonlinear iterative solver suggested in article "An Inconvenient 'Truth'
            // About Using Sensible Heat Flux as the Suface Boundary Conditions in Models
            // Under Stably Stratified Regimes," S. Basu, A. Holtslag, B. Van De Wiel, 
            // A. Moene, G.-J. Steeneveld, Acta Geophysica, Vol. 56, No. 1, pp. 88--99.  
            // This is algorithm 2 in the article.

            // Set iterator to zero
            label iter = 0;

            //Info << "U" << U << endl;

            // Set initial guesse at u*
            scalar uStar0 = (kappa * U) / Foam::log(z1 / z0);

            // Limit u* to always be zero or positive
            uStar0 = max(0.0, uStar0);

            // Set initial guess at qw
            scalar qw0 = (-deltaT * uStar0 * kappa) / (alphaH * Foam::log(z1 / z0));

            //Info << "qw0 = " << qw0 << tab <<
            //        "uStar0 = " << uStar0 << endl;



            qw = qw0;
            uStar = uStar0;
            L = -(Foam::pow(uStar,3.0)) / (kappa * (g/TRef) * qw);
            phiM = 1.0 + (gammaM * z1/L);


            // neutral
            if (deltaT == 0.0)
            {
                //Info << "Neutral" << endl;
                qw = qw0;
                uStar = uStar0;
                L = 1.0E30;
                phiM = 1.0;
            }

            // --stable
            // see book "Modelling of Atmospheric Flow Field", editors D. Lalas and C. Ratto
            // chapter 2--Modelling the Vertical ABL Structure, D. Etling, pp. 56--57
            else if (deltaT > 100000000.0)
            //else if (deltaT > 0.0)
            {
                Info << "Stable" << endl;
                label procPrint = -1;

                scalar uStarDiff = 1E30;
                scalar qwDiff = 1E30;

                do
                {
                    iter++;

                    if (Pstream::myProcNo() == procPrint)
                    {
                    Pout << "iter = " << iter << endl;
                    Pout << "qw0 = " << qw0 << endl;
                    Pout << "uStar0 = " << uStar0 << endl;
                    Pout << "U = " << U << endl;
                    Pout << "g = " << g << endl;
                    Pout << "kappa = " << kappa << endl;
                    Pout << "TRef = " << TRef << endl;
                    Pout << "z1 = " << z1 << endl;
                    Pout << "z0 = " << z0 << endl;
                    Pout << "gammaM = " << gammaM << endl;
                    Pout << "gammaH = " << gammaH << endl;
                    Pout << "betaM = " << betaM << endl;
                    Pout << "betaH = " << betaH << endl;
                    Pout << "alphaH = " << alphaH << endl;
                    }

                    // set initial guesses at L
                    scalar L0 = -(Foam::pow(uStar0,3.0)) / (kappa * (g/TRef) * qw0);

                    // limit L to always be positive and finite
                    L0 = max(1.0E-10,L0);

                    // form the "zeta" variable, which is z/L
                    scalar zeta0 = z1/L0;

                    // form psiM
                    scalar psiM0 = -gammaM * zeta0;

                    // form psiH
                    scalar psiH0 = -gammaH * zeta0;

                    // update u*
                    scalar uStarOld = uStar0;
                    uStar0 = (kappa * U) / (Foam::log(z1/z0) - psiM0);

                    // update qw
                    scalar qwOld = qw0;
                    qw0 = -(kappa * uStar0 * deltaT) / (alphaH * Foam::log(z1/z0) - psiH0);

                    // compute change in u* and qw to see if converged
                    uStarDiff = uStar0 - uStarOld;
                    qwDiff = qw0 - qwOld;

                    // in case this is converged, calculate uStar, L, and phiM
                    qw = qw0;
                    uStar = uStar0;
                    L = L0;
                    phiM = 1.0 + (gammaM * zeta0);
                    phiH = alphaH + (gammaH * zeta0);

                } while (((mag(uStarDiff) > tol) || (mag(qwDiff) > tol)) && (iter < iterMax-1));

                if (iter >= iterMax)
                {
                    Info << "Max qw, uStar iterations reached!!!" << endl;
                }

            }

            // --unstable
            // see book "Modelling of Atmospheric Flow Field", editors D. Lalas and C. Ratto
            // chapter 2--Modelling the Vertical ABL Structure, D. Etling, pp. 56--57 and
            // article "The Mathematical Representation of Wind Speed and Temperature Profiles
            // in the Unstable Atmospheric Surface Layer", C. Paulson, Journal of Applied
            // Meteorology, Vol 9, 1970, pp. 857--861.
            else if (deltaT < -100000000.0)
//            else if (deltaT < 0.0)
            {
                Info << "Unstable" << endl;

                scalar uStarDiff = 1E30;
                scalar qwDiff = 1E30;

                do
                {
                    iter++;
                    Info << "iter = " << iter << endl;
                    Info << "uStar0 = " << uStar0 << endl;
                    Info << "kappa = " << kappa << endl;
                    Info << "TRef = " << TRef << endl;
                    Info << "qw = " << qw0 << endl;

                    // set initial guesses at L
                    scalar L0 = -(Foam::pow(uStar0,3.0)) / (kappa * (g/TRef) * qw0);

                    // limit L to always be negative and finite
                    L0 = min(-1.0E-10,L0);
                    Info << "L0 = " << L0 << endl;

                    // form the "zeta" variable, which is z/L
                    scalar zeta0 = z1/L0;
                    Info << "zeta0 = " << zeta0 << endl;

                    // form the "x" variable in Paulson's paper
                    scalar x0 = Foam::pow((1.0 - (betaM * zeta0)),0.25);
                    Info << "x0 = " << x0 << endl;

                    // form the "y" variable in Etling's text
                    scalar y0 = Foam::pow((1.0 - (betaH * zeta0)),0.5);
                    Info << "y0 = " << y0 << endl;

                    // form psiM
                    scalar psiM0 = 2.0*Foam::log((1.0+x0)/2.0) + Foam::log((1.0 + Foam::pow(x0,2.0))/2.0) - 2.0*Foam::atan(x0) + Foam::constant::mathematical::pi/2.0;
                    Info << "psiM0 = " << psiM0 << endl;

                    // form psiH
                    scalar psiH0 =     Foam::log((1.0+y0)/2.0);
                    Info << "psiH0 = " << psiH0 << endl;

                    // update u*
                    scalar uStarOld = uStar0;
                    uStar0 = (kappa * U) / (Foam::log(z1/z0) - psiM0);
                    Info << "uStar0 = " << uStar0 << endl;

                    // update qw
                    scalar qwOld = qw0;
                    qw0 = -(kappa * uStar0 * deltaT) / (alphaH * Foam::log(z1/z0) - psiH0);
                    Info << "qw0 = " << qw0 << endl;

                    // compute change in u* and qw to see if converged
                    uStarDiff = uStar0 - uStarOld;
                    qwDiff = qw0 - qwOld;

                    // in case this is converged, calculate uStar, L, and phiM
                    qw = qw0;
                    uStar = uStar0;
                    L = L0;
                    phiM = Foam::pow((1.0 - (betaM*zeta0)),-0.25);
                    phiH = Foam::pow((1.0 - (betaH*zeta0)),-0.5);
                    Info << "end" << endl;

                } while (((mag(uStarDiff) > tol) || (mag(qwDiff) > tol)) && (iter < iterMax-1));

                if (iter >= iterMax)
                {
                    Info << "Max qw, uStar iterations reached!!!" << endl;
                }

            }

            //Info << "iter = " << iter << endl;
            
}

void fixedHeatingRateFvPatchField::write(Ostream& os) const
{
    fvPatchField<vector>::write(os);
    heatingRate_.writeEntry("heatingRate", os);
    TSurface_.writeEntry("TSurface", os);
    os.writeKeyword("kappa") << kappa_ << token::END_STATEMENT << nl;
    z0_.writeEntry("z0", os);
    os.writeKeyword("betaM") << betaM_ << token::END_STATEMENT << nl;
    os.writeKeyword("gammaM") << gammaM_ << token::END_STATEMENT << nl;
    os.writeKeyword("betaH") << betaH_ << token::END_STATEMENT << nl;
    os.writeKeyword("gammaH") << gammaH_ << token::END_STATEMENT << nl;
    os.writeKeyword("alphaH") << alphaH_ << token::END_STATEMENT << nl;
    os.writeKeyword("averageType") << averageType_ << token::END_STATEMENT << nl;
    writeEntry("value", os);
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

makePatchTypeField
(
    fvPatchVectorField,
    fixedHeatingRateFvPatchField
);

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// ************************************************************************* //
