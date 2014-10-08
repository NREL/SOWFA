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

#include "turbulentABLTemperatureControlledFvPatchField.H"
#include "volFields.H"
#include "fvCFD.H"
#include "addToRunTimeSelectionTable.H"


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

turbulentABLTemperatureControlledFvPatchField::turbulentABLTemperatureControlledFvPatchField
(
    const fvPatch& p,
    const DimensionedField<scalar, volMesh>& iF
)
:
    fixedValueFvPatchScalarField(p, iF),
    printOn_(false),
    zInversion_(700.0),
    widthInversion_(100.0),
    TInvBelow_(300.0),
    TInvAbove_(308.0),
    TSurface_(300.4),
    TSurfRate_(8.4E-5),
    TGradBelow_(0.0),
    TGradAbove_(0.003),
    alphaTurbulent_(0.1),
    fluctScale_(0.0003),
    zPeak_(100.0),
    curTimeIndex_(-1),
    counter_(0),
    turbField_(p.size()),
    turbFieldOld_(p.size()),
    ranGen_(label(0)),
    zInvBottom_(650.0),
    zInvTop_(750.0),
    TGradInv_(0.08),
    TSurfaceOld_(TSurface_)
{}


turbulentABLTemperatureControlledFvPatchField::turbulentABLTemperatureControlledFvPatchField
(
    const turbulentABLTemperatureControlledFvPatchField& ptf,
    const fvPatch& p,
    const DimensionedField<scalar, volMesh>& iF,
    const fvPatchFieldMapper& mapper
)
:
    fixedValueFvPatchScalarField(ptf, p, iF, mapper),
    printOn_(ptf.printOn_),
    zInversion_(ptf.zInversion_),
    widthInversion_(ptf.widthInversion_),
    TInvBelow_(ptf.TInvBelow_),
    TInvAbove_(ptf.TInvAbove_),
    TSurface_(ptf.TSurface_),
    TSurfRate_(ptf.TSurfRate_),
    TGradBelow_(ptf.TGradBelow_),
    TGradAbove_(ptf.TGradAbove_),
    alphaTurbulent_(ptf.alphaTurbulent_),
    fluctScale_(ptf.fluctScale_),
    zPeak_(ptf.zPeak_),
    curTimeIndex_(-1),
    counter_(0),
    turbField_(ptf.turbField_,mapper),
    turbFieldOld_(ptf.turbFieldOld_,mapper),
    ranGen_(label(0)),
    zInvBottom_(650.0),
    zInvTop_(750.0),
    TGradInv_(0.08),
    TSurfaceOld_(TSurface_)
{}


turbulentABLTemperatureControlledFvPatchField::turbulentABLTemperatureControlledFvPatchField
(
    const fvPatch& p,
    const DimensionedField<scalar, volMesh>& iF,
    const dictionary& dict
)
:
    fixedValueFvPatchScalarField(p, iF, dict),
    printOn_(dict.lookupOrDefault<bool>("print", false)),
    zInversion_(readScalar(dict.lookup("zInversion"))),
    widthInversion_(readScalar(dict.lookup("widthInversion"))),
    TInvBelow_(readScalar(dict.lookup("TInvBelow"))),
    TInvAbove_(readScalar(dict.lookup("TInvAbove"))),
    TSurface_(readScalar(dict.lookup("TSurface"))),
    TSurfRate_(readScalar(dict.lookup("TSurfRate"))),
    TGradBelow_(readScalar(dict.lookup("TGradBelow"))),
    TGradAbove_(readScalar(dict.lookup("TGradAbove"))),
    alphaTurbulent_(readScalar(dict.lookup("alphaTurbulent"))),
    fluctScale_(readScalar(dict.lookup("fluctScale"))),
    zPeak_(readScalar(dict.lookup("fluctPeakZ"))),
    curTimeIndex_(-1),
    counter_(0),
    turbField_("turbField", dict, p.size()),
    turbFieldOld_(p.size()),
    ranGen_(label(0)),
    zInvBottom_(zInversion_-0.5*widthInversion_),
    zInvTop_(zInversion_+0.5*widthInversion_),
    TGradInv_((TInvAbove_-TInvBelow_)/widthInversion_),
    TSurfaceOld_(TSurface_)
{}


turbulentABLTemperatureControlledFvPatchField::turbulentABLTemperatureControlledFvPatchField
(
    const turbulentABLTemperatureControlledFvPatchField& ptf
)
:
    fixedValueFvPatchScalarField(ptf),
    printOn_(ptf.printOn_),
    zInversion_(ptf.zInversion_),
    widthInversion_(ptf.widthInversion_),
    TInvBelow_(ptf.TInvBelow_),
    TInvAbove_(ptf.TInvAbove_),
    TSurface_(ptf.TSurface_),
    TSurfRate_(ptf.TSurfRate_),
    TGradBelow_(ptf.TGradBelow_),
    TGradAbove_(ptf.TGradAbove_),
    alphaTurbulent_(ptf.alphaTurbulent_),
    fluctScale_(ptf.fluctScale_),
    zPeak_(ptf.zPeak_),
    curTimeIndex_(-1),
    counter_(0),
    turbField_(ptf.turbField_),
    turbFieldOld_(ptf.turbFieldOld_),
    ranGen_(ptf.ranGen_),
    zInvBottom_(ptf.zInvBottom_),
    zInvTop_(ptf.zInvTop_),
    TGradInv_(ptf.TGradInv_),
    TSurfaceOld_(ptf.TSurfaceOld_)
{}


turbulentABLTemperatureControlledFvPatchField::turbulentABLTemperatureControlledFvPatchField
(
    const turbulentABLTemperatureControlledFvPatchField& ptf,
    const DimensionedField<scalar, volMesh>& iF
)
:
    fixedValueFvPatchScalarField(ptf, iF),
    printOn_(ptf.printOn_),
    zInversion_(ptf.zInversion_),
    widthInversion_(ptf.widthInversion_),
    TInvBelow_(ptf.TInvBelow_),
    TInvAbove_(ptf.TInvAbove_),
    TSurface_(ptf.TSurface_),
    TSurfRate_(ptf.TSurfRate_),
    TGradBelow_(ptf.TGradBelow_),
    TGradAbove_(ptf.TGradAbove_),
    alphaTurbulent_(ptf.alphaTurbulent_),
    fluctScale_(ptf.fluctScale_),
    zPeak_(ptf.zPeak_),
    curTimeIndex_(-1),
    counter_(0),
    turbField_(ptf.turbField_),
    turbFieldOld_(ptf.turbFieldOld_),
    ranGen_(ptf.ranGen_),
    zInvBottom_(ptf.zInvBottom_),
    zInvTop_(ptf.zInvTop_),
    TGradInv_(ptf.TGradInv_),
    TSurfaceOld_(ptf.TSurfaceOld_)
{}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //

void turbulentABLTemperatureControlledFvPatchField::updateCoeffs()
{
    if (this->updated())
    {
        return;
    }

    scalar pi_ = Foam::constant::mathematical::pi;



//  Compute the new inflow velocity.
    if (curTimeIndex_ != this->db().time().timeIndex())
    {
 
        scalar dt = this->db().time().deltaT().value();

        scalarField TLocal(patch().size(),0.0);

        zInvBottom_ += dt*TSurfRate_/TGradInv_;

        forAll(patch(), faceI)
        {
            scalar z = patch().Cf()[faceI].z();
            scalar T = 0.0;

            if (z < zInvBottom_)
            {
                T = TInvBelow_ + TGradBelow_ * (z - zInvBottom_);
            }
            else if ( (z >= zInvBottom_) && 
                      (z <= zInvTop_) )
            {
                T = TInvAbove_ + TGradInv_ * (z - zInvTop_);
            }
            else if (z > zInvTop_)
            {
                T = TInvAbove_ + TGradAbove_ * (z - zInvTop_);
            }

            scalar Tdiff = TSurface_ - T;
            scalar blend = Foam::exp(-30.0*(z/zInvTop_));
            T += Tdiff * blend;

            TLocal[faceI] = T;
        }

        TSurfaceOld_ = TSurface_;
        TSurface_ += dt*TSurfRate_;
        TInvBelow_ += dt*TSurfRate_;
        //Info << "TSurfaceOld = " << TSurfaceOld_ << endl <<
        //        "TSurface    = " << TSurface_ << endl;


        Field<scalar> randomField(this->size());
        Field<scalar> mask(this->size());
        Field<scalar> turbFieldNew(this->size());

        forAll(patch(), faceI)
        {
             ranGen_.randomise(randomField[faceI]);
             scalar z = patch().Cf()[faceI].z();
             mask[faceI] = 1.65 * (z/zPeak_) * Foam::exp(-0.5*Foam::pow((z/zPeak_),2));
        }

        scalar rmsCorr = sqrt(12.0*(2.0*alphaTurbulent_ - sqr(alphaTurbulent_)))/alphaTurbulent_;

        turbFieldOld_ = turbField_;

        turbFieldNew = mask * rmsCorr * (randomField - 0.5) * fluctScale_ * TLocal;

        turbField_ = (1 - alphaTurbulent_) * turbFieldOld_ + (alphaTurbulent_) * turbFieldNew;

        TLocal += turbField_;
    
        this->operator==(TLocal);
 
        fixedValueFvPatchScalarField::updateCoeffs();
    
        curTimeIndex_ = this->db().time().timeIndex();
    }

    if (printOn_)
    {
    }   


}


void turbulentABLTemperatureControlledFvPatchField::write(Ostream& os) const
{
    fvPatchField<scalar>::write(os);
    os.writeKeyword("print")     << printOn_   << token::END_STATEMENT << nl;
    os.writeKeyword("zInversion") << zInversion_ << token::END_STATEMENT << nl;
    os.writeKeyword("widthInversion") << widthInversion_ << token::END_STATEMENT << nl;
    os.writeKeyword("TInvBelow") << TInvBelow_ << token::END_STATEMENT << nl;
    os.writeKeyword("TInvAbove") << TInvAbove_ << token::END_STATEMENT << nl;
    os.writeKeyword("TSurface") << TSurfaceOld_ << token::END_STATEMENT << nl;
    os.writeKeyword("TSurfRate") << TSurfRate_ << token::END_STATEMENT << nl;
    os.writeKeyword("TGradBelow") << TGradBelow_ << token::END_STATEMENT << nl;
    os.writeKeyword("TGradAbove") << TGradAbove_ << token::END_STATEMENT << nl;
    os.writeKeyword("alphaTurbulent") << alphaTurbulent_ << token::END_STATEMENT << nl;
    os.writeKeyword("fluctScale") << fluctScale_ << token::END_STATEMENT << nl;
    os.writeKeyword("fluctPeakZ") << zPeak_ << token::END_STATEMENT << nl;
    turbField_.writeEntry("turbField", os);
    this->writeEntry("value", os);
}


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

makePatchTypeField
(
   fvPatchScalarField,
   turbulentABLTemperatureControlledFvPatchField
);

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// ************************************************************************* //
