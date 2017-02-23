{
        volScalarField divPhi = fvc::div(phi);
        volScalarField divPhiMag  = Foam::sqr(divPhi);
        scalar minLocalPhiContErr = min(divPhi).value();
        reduce(minLocalPhiContErr, minOp<scalar>());
        scalar maxLocalPhiContErr = max(divPhi).value();
        reduce(maxLocalPhiContErr, maxOp<scalar>());
        scalar rmsLocalPhiContErr = Foam::sqrt(divPhiMag.weightedAverage(mesh.V()).value());
        Info << "Local Flux Continuity Error:  Min " << minLocalPhiContErr << tab;
        Info <<                               "Max " << maxLocalPhiContErr << tab;
        Info <<                               "RMS " << rmsLocalPhiContErr << endl;

        scalar globalSumPhiBoundary = 0.0;
        forAll(phi.boundaryField(), patchi)
        {
            scalar sumPhiBoundary = 0.0;
            const fvsPatchScalarField& phip = phi.boundaryField()[patchi];
            forAll(phip,i)
            {
                sumPhiBoundary += phip[i];
            }
            globalSumPhiBoundary += sumPhiBoundary;
            Pout << "Boundary " << mesh.boundaryMesh()[patchi].name() << " Flux:  " << sumPhiBoundary << endl;
        }

        reduce(globalSumPhiBoundary, sumOp<scalar>());
        Info << "Total Boundary Flux: " << globalSumPhiBoundary << endl;
}
