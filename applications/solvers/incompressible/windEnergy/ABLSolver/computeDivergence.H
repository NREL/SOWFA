// Calculate divergence of velocity flux and display
{

    // Compute divergence cell by cell and report statistics.
    volScalarField divPhi = fvc::div(phi);
    volScalarField divPhiMag  = pow(pow(divPhi,2),0.5);
    scalar minLocalPhiContErr = min(divPhiMag).value();
    reduce(minLocalPhiContErr, minOp<scalar>());
    scalar maxLocalPhiContErr = max(divPhiMag).value();
    reduce(maxLocalPhiContErr, maxOp<scalar>());
    scalar avgLocalPhiContErr = divPhiMag.weightedAverage(mesh.V()).value();
    Info << "Local Flux Continuity Error:  Min " << minLocalPhiContErr << tab
         <<                               "Max " << maxLocalPhiContErr << tab
         <<                     "Weighted Mean " << avgLocalPhiContErr << endl;


    // Compute global divergence over domain boundaries and report statistics.
    Info << "Boundary Flux Report:" << endl;

    scalar globalSumPhiBoundary = 0.0;
    scalar globalSumAreaBoundary = 0.0;
    forAll(phi.boundaryField(), patchi)
    {
        if ( !mesh.boundary()[patchi].coupled() )
        {

            const fvPatchVectorField& Up = U.boundaryField()[patchi];

            word boundaryName = mesh.boundary()[patchi].name();
            scalar sumPhiBoundary = 0.0;
            scalar sumAreaBoundary = 0.0;
            const fvsPatchScalarField& phip = phi.boundaryField()[patchi];
            forAll(phip,i)
            {
                sumPhiBoundary += phip[i];
                sumAreaBoundary += mesh.boundary()[patchi].magSf()[i];
            }

            reduce(sumPhiBoundary, sumOp<scalar>());
            reduce(sumAreaBoundary, sumOp<scalar>());

            globalSumPhiBoundary += sumPhiBoundary;
            globalSumAreaBoundary += sumAreaBoundary;

            Info << "    " << boundaryName << " Flux: " << sumPhiBoundary << tab << "/ Area: " << sumAreaBoundary << endl;
        }
    }

    reduce(globalSumPhiBoundary, sumOp<scalar>());
    reduce(globalSumAreaBoundary, sumOp<scalar>());
    Info << "Total Boundary Flux: " << globalSumPhiBoundary << endl;
    Info << "Total Boundary Area: " << globalSumAreaBoundary << endl;
}
