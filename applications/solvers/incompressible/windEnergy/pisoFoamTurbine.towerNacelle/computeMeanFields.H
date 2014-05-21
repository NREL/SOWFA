{
    volVectorField uPrime = U;
    volScalarField pPrime = p;

    // Compute the running time averages of the fields and then get the fluctuation.
    if (runTime.value() >= startTimeMean)
    {
        UMean = (UMean * sumTimeMean) + (runTime.deltaT().value() * U);
        pMean = (pMean * sumTimeMean) + (runTime.deltaT().value() * p);
        RMean = (RMean * sumTimeMean) + (runTime.deltaT().value() * turbulence->R());
        kMean = (kMean * sumTimeMean) + (runTime.deltaT().value() * turbulence->k());

        sumTimeMean += runTime.deltaT().value();

        UMean = UMean / sumTimeMean;
        pMean = pMean / sumTimeMean;
        RMean = RMean / sumTimeMean;
        kMean = kMean / sumTimeMean;
    }

    // Compute the running time averages of the correlation fields.
    if (runTime.value() >= startTimeCorr)
    {
        uPrime = U - UMean;
        pPrime = p - pMean;

        uuMean = (uuMean * sumTimeCorr) + (runTime.deltaT().value() * Foam::sqr(uPrime));
        ppMean = (ppMean * sumTimeCorr) + (runTime.deltaT().value() * Foam::sqr(pPrime));
        upMean = (upMean * sumTimeCorr) + (runTime.deltaT().value() * pPrime * uPrime);

        sumTimeCorr += runTime.deltaT().value();

        uuMean = uuMean / sumTimeCorr;
        ppMean = ppMean / sumTimeCorr;
        upMean = upMean / sumTimeCorr;

        uuRTotal = uuMean + RMean;
    }
}
