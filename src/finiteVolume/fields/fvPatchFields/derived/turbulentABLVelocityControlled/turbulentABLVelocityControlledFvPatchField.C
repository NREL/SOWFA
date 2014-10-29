/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2012 OpenFOAM Foundation
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

#include "turbulentABLVelocityControlledFvPatchField.H"
#include "volFields.H"
#include "fvCFD.H"
#include "addToRunTimeSelectionTable.H"


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

turbulentABLVelocityControlledFvPatchField::turbulentABLVelocityControlledFvPatchField
(
    const fvPatch& p,
    const DimensionedField<vector, volMesh>& iF
)
:
    fixedValueFvPatchVectorField(p, iF),
    printOn_(false),
    zConst_(700.0),
    z0_(0.1),
    refLoc_(vector::zero),
    refU_(10.0),
    avgWindow_(60),
    alphaTurbulent_(0.1),
    alphaMean_(0.1),
    fluctScale_(vector::zero),
    zPeak_(60.0),
    uStar_(0.5),
    uStarOld_(0.5),
    kappa_(0.4),
    windDir_(270.0),
    windAng_(0.0),
    curTimeIndex_(-1),
    minDis(1.0E30),
    minDisCellID(-1),
    refLocControl_(0),
    counter_(0),
    uHistSum(0.0),
    uHist(avgWindow_),
    turbField_(p.size()),
    turbFieldOld_(p.size()),
    ranGen_(label(0))
{}


turbulentABLVelocityControlledFvPatchField::turbulentABLVelocityControlledFvPatchField
(
    const turbulentABLVelocityControlledFvPatchField& ptf,
    const fvPatch& p,
    const DimensionedField<vector, volMesh>& iF,
    const fvPatchFieldMapper& mapper
)
:
    fixedValueFvPatchVectorField(ptf, p, iF, mapper),
    printOn_(ptf.printOn_),
    zConst_(ptf.zConst_),
    z0_(ptf.z0_),
    refLoc_(ptf.refLoc_),
    refU_(ptf.refU_),
    avgWindow_(ptf.avgWindow_),
    alphaTurbulent_(ptf.alphaTurbulent_),
    alphaMean_(ptf.alphaMean_),
    fluctScale_(ptf.fluctScale_),
    zPeak_(ptf.zPeak_),
    uStar_(ptf.uStar_),
    uStarOld_(0.5),
    kappa_(ptf.kappa_),
    windDir_(ptf.windDir_),
    windAng_(0.0),
    curTimeIndex_(-1),
    oldTimeIndex_(-1),
    minDis(1.0E30),
    minDisCellID(-1),
    refLocControl_(0),
    counter_(0),
    uHistSum(0.0),
    uHist(avgWindow_),
    turbField_(ptf.turbField_, mapper),
    turbFieldOld_(ptf.turbFieldOld_,mapper),
    ranGen_(label(0))
{}


turbulentABLVelocityControlledFvPatchField::turbulentABLVelocityControlledFvPatchField
(
    const fvPatch& p,
    const DimensionedField<vector, volMesh>& iF,
    const dictionary& dict
)
:
    fixedValueFvPatchVectorField(p, iF, dict),
    printOn_(dict.lookupOrDefault<bool>("print", false)),
    zConst_(readScalar(dict.lookup("zConst"))),
    z0_(readScalar(dict.lookup("z0"))),
    refLoc_(dict.lookup("refLoc")),
    refU_(readScalar(dict.lookup("refU"))),
    avgWindow_(readScalar(dict.lookup("avgWindow"))),
    alphaTurbulent_(readScalar(dict.lookup("alphaTurbulent"))),
    alphaMean_(readScalar(dict.lookup("alphaMean"))),
    fluctScale_(dict.lookup("fluctScale")),
    zPeak_(readScalar(dict.lookup("fluctPeakZ"))),
    uStar_(readScalar(dict.lookup("uStar"))),
    uStarOld_(uStar_),
    kappa_(readScalar(dict.lookup("kappa"))),
    windDir_(readScalar(dict.lookup("windDir"))),
    windAng_(0.0),
    curTimeIndex_(-1),
    oldTimeIndex_(-1),
    minDis(1.0E30),
    minDisCellID(-1),
    refLocControl_(0),
    counter_(0),
    uHistSum(0.0),
    uHist(avgWindow_),
    turbField_("turbField", dict, p.size()),
    turbFieldOld_(p.size()),
    ranGen_(label(0))
{}


turbulentABLVelocityControlledFvPatchField::turbulentABLVelocityControlledFvPatchField
(
    const turbulentABLVelocityControlledFvPatchField& ptf
)
:
    fixedValueFvPatchVectorField(ptf),
    printOn_(ptf.printOn_),
    zConst_(ptf.zConst_),
    z0_(ptf.z0_),
    refLoc_(ptf.refLoc_),
    refU_(ptf.refU_),
    avgWindow_(ptf.avgWindow_),
    alphaTurbulent_(ptf.alphaTurbulent_),
    alphaMean_(ptf.alphaMean_),
    fluctScale_(ptf.fluctScale_),
    zPeak_(ptf.zPeak_),
    uStar_(ptf.uStar_),
    uStarOld_(0.5),
    kappa_(ptf.kappa_),
    windDir_(ptf.windDir_),
    windAng_(0.0),
    curTimeIndex_(-1),
    oldTimeIndex_(-1),
    minDis(1.0E30),
    minDisCellID(-1),
    refLocControl_(0),
    counter_(0),
    uHistSum(0.0),
    uHist(avgWindow_),
    turbField_(ptf.turbField_),
    turbFieldOld_(ptf.turbFieldOld_),
    ranGen_(ptf.ranGen_)
{}


turbulentABLVelocityControlledFvPatchField::turbulentABLVelocityControlledFvPatchField
(
    const turbulentABLVelocityControlledFvPatchField& ptf,
    const DimensionedField<vector, volMesh>& iF
)
:
    fixedValueFvPatchVectorField(ptf, iF),
    printOn_(ptf.printOn_),
    zConst_(ptf.zConst_),
    z0_(ptf.z0_),
    refLoc_(ptf.refLoc_),
    refU_(ptf.refU_),
    avgWindow_(ptf.avgWindow_),
    alphaTurbulent_(ptf.alphaTurbulent_),
    alphaMean_(ptf.alphaMean_),
    fluctScale_(ptf.fluctScale_),
    zPeak_(ptf.zPeak_),
    uStar_(ptf.uStar_),
    uStarOld_(0.5),
    kappa_(ptf.kappa_),
    windDir_(ptf.windDir_),
    windAng_(0.0),
    curTimeIndex_(-1),
    oldTimeIndex_(-1),
    minDis(1.0E30),
    minDisCellID(-1),
    refLocControl_(0),
    counter_(0),
    uHistSum(0.0),
    uHist(avgWindow_),
    turbField_(ptf.turbField_),
    turbFieldOld_(ptf.turbFieldOld_),
    ranGen_(ptf.ranGen_)
{}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

void turbulentABLVelocityControlledFvPatchField::updateCoeffs()
{
    if (this->updated())
    {
        return;
    }

    scalar pi_ = Foam::constant::mathematical::pi;

//  Set up access to the internal velocity field.
    const volVectorField& U_ = db().objectRegistry::lookupObject<volVectorField>("U");
    
//  Set up access to the mesh
    const fvMesh& mesh_ = U_.mesh();

//  Find the cell at which to monitor the flow.
    if (curTimeIndex_ == -1)
    {
       uStarOld_ = uStar_;

       turbFieldOld_ = turbField_;

       windAng_ = windDir_ + 180.0;
       if (windAng_ >= 360.0)
       {
          windAng_ -= 360.0;
       }
       windAng_ = 90.0 - windAng_;
       if (windAng_ < 0.0)
       {
          windAng_ = windAng_ + 360.0;
       }

       forAll(mesh_.C(),i)
       {
          scalar dis = mag(mesh_.C()[i] - refLoc_);
          if (dis < minDis)
          {
             minDis = dis;
             minDisCellID = i;
          }
       }
       
       scalar minDisGlobal = minDis;
       reduce(minDisGlobal,minOp<scalar>());

       if (minDis == minDisGlobal)
       {
          refLocControl_ = 1;
       }
    }

    
    if (curTimeIndex_ != this->db().time().timeIndex())
    {
       oldTimeIndex_ = curTimeIndex_;
       uStarOld_ = uStar_;
       counter_++;
    }

//  Sample the velocity at the sample point.
    scalar Umeas = 0.0;
    scalar UmeasAvg = 0.0;
//  volTensorField gradU = fvc::grad(U_);
//  vector dx = refLoc_ - mesh_.C()[minDisCellID];
//  vector dU = dx & gradU[minDisCellID];
//  vector Uvec = U_[minDisCellID] + dU;
    vector Uvec = U_[minDisCellID];
    if (refLocControl_ == 1)
    {
       Umeas = mag(Uvec);
    }
    else
    {
       Umeas = 0.0;
    }
    reduce(Umeas,sumOp<scalar>());

    if (counter_ <= avgWindow_)
    {
       if (curTimeIndex_ != this->db().time().timeIndex())
       {
           for (int i = counter_ - 1; i > 0; i--)
           {
               uHist(i) = uHist(i-1);
           }
           uHist(0) = Umeas;
           uHistSum += Umeas;
           UmeasAvg = uHistSum / counter_;
       }
       else
       {
           uHistSum -= uHist(0);
           uHist(0) = Umeas;
           uHistSum += uHist(0);
           UmeasAvg = uHistSum / counter_;
       }
    }
    else
    {
       counter_ = avgWindow_;
       if (curTimeIndex_ != this->db().time().timeIndex())
       {
           uHistSum -= uHist(avgWindow_ - 1);
           for(int i = avgWindow_ - 1; i > 0; i--)
           {
               uHist(i) = uHist(i - 1);
           }
           uHist(0) = Umeas;
           uHistSum += uHist(0);
           UmeasAvg = uHistSum / avgWindow_;
       }
       else
       {
           uHistSum -= uHist(0);
           uHist(0) = Umeas;
           uHistSum += uHist(0);
           UmeasAvg = uHistSum / avgWindow_;
       }
    }

//  UmeasAvg = 0.0;
//  for (int i = 0; i < counter_; i++)
//  {
//     UmeasAvg += uHist(i);
//  }
//  UmeasAvg /= counter_;

//  Info << counter_ << tab << avgWindow_ << tab << "Umeas = " << Umeas << tab << "UmeasAvg = " << UmeasAvg << endl;
//  Info << uHist << endl;


//  Compute the new inflow velocity.
    if ((curTimeIndex_ != -1) && (curTimeIndex_ != this->db().time().timeIndex()))
    {
        uStar_ = uStar_ * (refU_/UmeasAvg);
      //scalar uStarDesired = uStar_;
        uStar_ = alphaMean_*uStar_ + (1.0 - alphaMean_)*uStarOld_;
    }
//  Info << curTimeIndex_ << tab << uStar_ << tab << windDir_ << endl;
    Info << counter_ << tab << avgWindow_ << tab << "Umeas = " << Umeas << tab << "UmeasAvg = " << UmeasAvg << tab << "uStar = " << uStar_ << endl;



    vectorField ULocal(patch().size(),vector::zero);

    forAll(patch(), faceI)
    {
         scalar z = patch().Cf()[faceI].z();
         scalar u = 0.0;
         if (z < zConst_)
         {
            u = (uStar_ / kappa_) * Foam::log(z/z0_);
         }
         else
         {
            u = (uStar_ / kappa_) * Foam::log(zConst_/z0_);
         }
         ULocal[faceI].z() = 0.0;
         ULocal[faceI].x() = u * Foam::cos(windAng_*pi_/180.0);
         ULocal[faceI].y() = u * Foam::sin(windAng_*pi_/180.0);
    }

    if (curTimeIndex_ != this->db().time().timeIndex())
    {

        Field<vector> randomField(this->size());
        Field<scalar> mask(this->size());
        Field<vector> turbFieldNew(this->size());
       
        turbFieldOld_ = turbField_;

        forAll(patch(), faceI)
        {
             ranGen_.randomise(randomField[faceI]);
             scalar z = patch().Cf()[faceI].z();
             mask[faceI] = 1.65 * (z/zPeak_) * Foam::exp(-0.5*Foam::pow((z/zPeak_),2));
        }

        scalar rmsCorr = sqrt(12.0*(2.0*alphaTurbulent_ - sqr(alphaTurbulent_)))/alphaTurbulent_;
       
        turbFieldNew = mask * (rmsCorr * cmptMultiply(randomField - 0.5 * pTraits<vector>::one,fluctScale_) * mag(ULocal));
        
        turbField_ = (1 - alphaTurbulent_) * turbFieldOld_ + (alphaTurbulent_) * turbFieldNew;
        
        ULocal += turbField_;

//      if (this->size() > 0)
//      {
//         Pout << "turbFieldOld_= " << turbFieldOld_[0] << endl
//              << "turbFieldNew_= " << turbFieldNew[0] << endl
//              << "turbField_= " << turbField_[0] << endl
//              << "alphaT_= " << alphaTurbulent_ << endl;
//      }
    }



    
//  Info << "UstarDesired = " << uStarDesired << tab << "Ustar = " << uStar_ << tab << "UstarOld = " << uStarOld_ << endl;
//  Info << "curTimeIndx = " << curTimeIndex_ << tab << "oldTimeIndex = " << oldTimeIndex_ << endl;

    if (printOn_)
    {
    }   

    this->operator==(ULocal);

    fixedValueFvPatchVectorField::updateCoeffs();

    curTimeIndex_ = this->db().time().timeIndex();

}


void turbulentABLVelocityControlledFvPatchField::write(Ostream& os) const
{
    fvPatchField<vector>::write(os);
    os.writeKeyword("print")     << printOn_   << token::END_STATEMENT << nl;
    os.writeKeyword("zConst")    << zConst_    << token::END_STATEMENT << nl;
    os.writeKeyword("z0")        << z0_        << token::END_STATEMENT << nl;
    os.writeKeyword("refLoc")    << refLoc_    << token::END_STATEMENT << nl;
    os.writeKeyword("refU")      << refU_      << token::END_STATEMENT << nl;
    os.writeKeyword("avgWindow") << avgWindow_ << token::END_STATEMENT << nl;
    os.writeKeyword("alphaTurbulent") << alphaTurbulent_ << token::END_STATEMENT << nl;
    os.writeKeyword("alphaMean") << alphaMean_ << token::END_STATEMENT << nl;
    os.writeKeyword("fluctScale") << fluctScale_ << token::END_STATEMENT << nl;
    os.writeKeyword("fluctPeakZ") << zPeak_ << token::END_STATEMENT << nl;
    os.writeKeyword("uStar")     << uStar_     << token::END_STATEMENT << nl;
    os.writeKeyword("kappa")     << kappa_    << token::END_STATEMENT << nl;
    os.writeKeyword("windDir")   << windDir_   << token::END_STATEMENT << nl;
    turbField_.writeEntry("turbField", os);
    this->writeEntry("value", os);
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

makePatchTypeField
(
   fvPatchVectorField,
   turbulentABLVelocityControlledFvPatchField
);

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// ************************************************************************* //
