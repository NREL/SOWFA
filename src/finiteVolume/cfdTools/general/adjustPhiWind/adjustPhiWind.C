/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011-2015 OpenFOAM Foundation
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

#include "adjustPhiWind.H"
#include "volFields.H"
#include "surfaceFields.H"
#include "inletOutletFvPatchFields.H"

// * * * * * * * * * * * * * * * Global Functions  * * * * * * * * * * * * * //

bool Foam::adjustPhiWind
(
    surfaceScalarField& phi,
    const volVectorField& U,
    volScalarField& p
)
{
    if (p.needReference())
    {
        scalar massIn = 0.0;
        scalar fixedMassOut = 0.0;
        scalar adjustableMassOut = 0.0;

        surfaceScalarField::GeometricBoundaryField& bphi = phi.boundaryField();

        // For flux monitoring
//      List<scalar> adjustablePatchOutflow(bphi.size(), 0.0);
//      List<scalar> fixedPatchInflow(bphi.size(), 0.0);
//      List<scalar> fixedPatchOutflow(bphi.size(), 0.0);

        forAll(bphi, patchi)
        {
            const fvPatchVectorField& Up = U.boundaryField()[patchi];
            const fvsPatchScalarField& phip = bphi[patchi];

            if (!phip.coupled())
            {
                if
                (
                    (Up.fixesValue() && !isA<inletOutletFvPatchVectorField>(Up))
                 || (Up.fixesValue() && Up.type() == "timeVaryingMappedInletOutlet") // need extra conditional here because timeVaryingMappedInletOutlet is derived from inletOutletFvPatch
                )
                {
                    forAll(phip, i)
                    {
                        Info<< " " << phip[i];
                        if (phip[i] < 0.0)
                        {
                            massIn -= phip[i];
//                          fixedPatchInflow[patchi] -= phip[i];
                        }
                        else
                        {
                            fixedMassOut += phip[i];
//                          fixedPatchOutflow[patchi] += phip[i];
                        }
                        Info<< endl;
                    }
                }
                else
                {
                    forAll(phip, i)
                    {
                        Info<< " ~ " << phip[i];
                        if (phip[i] < 0.0)
                        {
                            massIn -= phip[i];
//                          fixedPatchInflow[patchi] -= phip[i];
                        }
                        else
                        {
                            adjustableMassOut += phip[i];
//                          adjustablePatchOutflow[patchi] += phip[i];
                        }
                        Info<< endl;
                    }
                }
            }
        }

        // Calculate the total flux in the domain, used for normalisation
        scalar totalFlux = VSMALL + sum(mag(phi)).value();

        reduce(massIn, sumOp<scalar>());
        reduce(fixedMassOut, sumOp<scalar>());
        reduce(adjustableMassOut, sumOp<scalar>());

        // Monitor fluxes
//      forAll(bphi, patchi)
//      {
//          const fvPatchVectorField& Up = U.boundaryField()[patchi];
//          if(Up.patch().type() != "processor")
//          {
//              scalar totFixedPatchInflow(fixedPatchInflow[patchi]);
//              scalar totFixedPatchOutflow(fixedPatchOutflow[patchi]);
//              scalar totAdjustablePatchOutflow(adjustablePatchOutflow[patchi]);
//              reduce(totFixedPatchInflow, sumOp<scalar>());
//              reduce(totFixedPatchOutflow, sumOp<scalar>());
//              reduce(totAdjustablePatchOutflow, sumOp<scalar>());
//              Info<< "Patch \"" << Up.patch().name() << "\""
//                  << " cumulative fixed mass in, fixed mass out, adjustable mass out :"
//                  << " " << totFixedPatchInflow
//                  << " " << totFixedPatchOutflow
//                  << " " << totAdjustablePatchOutflow
//                  << endl;
//          }
//      }

        scalar massCorr = 1.0;
        scalar magAdjustableMassOut = mag(adjustableMassOut);

        if
        (
            magAdjustableMassOut > VSMALL
         && magAdjustableMassOut/totalFlux > SMALL
        )
        {
            massCorr = (massIn - fixedMassOut)/adjustableMassOut;
//          Info<< "Applying massflow correction factor = " << massCorr << endl;
        }
        else if (mag(fixedMassOut - massIn)/totalFlux > 1e-8)
        {
            FatalErrorIn
            (
                "adjustPhiWind"
                "("
                    "surfaceScalarField&, "
                    "const volVectorField&,"
                    "volScalarField&"
                ")"
            )   << "Continuity error cannot be removed by adjusting the"
                   " outflow.\nPlease check the velocity boundary conditions"
                   " and/or run potentialFoam to initialise the outflow." << nl
                << "Total flux              : " << totalFlux << nl
                << "Specified mass inflow   : " << massIn << nl
                << "Specified mass outflow  : " << fixedMassOut << nl
                << "Adjustable mass outflow : " << adjustableMassOut << nl
                << exit(FatalError);
        }

        forAll(bphi, patchi)
        {
            const fvPatchVectorField& Up = U.boundaryField()[patchi];
            fvsPatchScalarField& phip = bphi[patchi];

            if (!phip.coupled())
            {
                if
                (
                    !Up.fixesValue()
                 || (isA<inletOutletFvPatchVectorField>(Up) // need extra conditional here because timeVaryingMappedInletOutlet is derived from inletOutletFvPatch
                     && Up.type() != "timeVaryingMappedInletOutlet")
                )
                {
                    forAll(phip, i)
                    {
                        if (phip[i] > 0.0)
                        {
                            phip[i] *= massCorr;
                        }
                    }
                }
            }
        }

        return mag(massIn)/totalFlux < SMALL
            && mag(fixedMassOut)/totalFlux < SMALL
            && mag(adjustableMassOut)/totalFlux < SMALL;
    } // if p.needReference()
    else
    {
        return false;
    }
}


// ************************************************************************* //
