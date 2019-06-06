// Calculate divergence of velocity flux and display
if (pimple.finalPimpleIter())
{

    // Compute divergence cell by cell and report statistics.
    volScalarField divPhi = fvc::div(phi);
    volScalarField divPhiMag  = pow(pow(divPhi,2),0.5);
    scalar minLocalPhiContErr = min(divPhiMag).value();
    reduce(minLocalPhiContErr, minOp<scalar>());
    scalar maxLocalPhiContErr = max(divPhiMag).value();
    reduce(maxLocalPhiContErr, maxOp<scalar>());
    scalar avgLocalPhiContErr = divPhiMag.weightedAverage(mesh.V()).value();
    Info << "Continuity Report:" << endl;
    Info << "   -Local Cell Continuity Error:" << endl;
    Info << "       minimum: " << minLocalPhiContErr << endl;
    Info << "       maximum: " << maxLocalPhiContErr << endl;
    Info << "       weighted mean: " << avgLocalPhiContErr << endl;


    // Compute global divergence over domain boundaries and report statistics.
    Info << "   -Boundary Flux:" << endl;

    scalar globalSumPhiBoundary = 0.0;
    scalar globalSumAreaBoundary = 0.0;
    forAll(phi.boundaryField(), patchi)
    {
        if ( !mesh.boundary()[patchi].coupled() )
        {
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

            Info << "       " << boundaryName << " - flux: " << sumPhiBoundary << tab << "/ area: " << sumAreaBoundary << endl;
        }
    }

    Info << "       total - flux: " << globalSumPhiBoundary << tab << "/ area: " << globalSumAreaBoundary << endl;
}
