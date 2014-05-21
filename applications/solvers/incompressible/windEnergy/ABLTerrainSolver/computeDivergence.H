// Calculate divergence of velocity flux and display
{
    volScalarField divPhi = fvc::div(phi);
    f = divPhi;
    volScalarField divPhiMag  = pow(pow(divPhi,2),0.5);
    scalar minLocalPhiContErr = min(divPhiMag).value();
    reduce(minLocalPhiContErr, minOp<scalar>());
    scalar maxLocalPhiContErr = max(divPhiMag).value();
    reduce(maxLocalPhiContErr, maxOp<scalar>());
    scalar avgLocalPhiContErr = divPhiMag.weightedAverage(mesh.V()).value();
    Info << "Local Flux Continuity Error:  Min " << minLocalPhiContErr << tab
         <<                               "Max " << maxLocalPhiContErr << tab
         <<                     "Weighted Mean " << avgLocalPhiContErr << endl;

    scalar globalSumPhiBoundary = 0.0;
    scalar globalSumAreaBoundary = 0.0;
    forAll(phi.boundaryField(), patchi)
    {
        scalar sumPhiBoundary = 0.0;
        scalar sumAreaBoundary = 0.0;
        const fvsPatchScalarField& phip = phi.boundaryField()[patchi];
        forAll(phip,i)
        {
           sumPhiBoundary += phip[i];
           sumAreaBoundary += mesh.boundary()[patchi].magSf()[i];
        }
        globalSumPhiBoundary += sumPhiBoundary;
        globalSumAreaBoundary += sumAreaBoundary;
    }

    reduce(globalSumPhiBoundary, sumOp<scalar>());
    Info << "Total Boundary Flux: " << globalSumPhiBoundary << endl;
    Info << "Total Boundary Area: " << globalSumAreaBoundary << endl;
}
