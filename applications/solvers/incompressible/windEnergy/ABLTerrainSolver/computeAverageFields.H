{
    volVectorField uPrime = U;
    volScalarField TPrime = T;

    volSymmTensorField R = turbulence->R();
    volVectorField q = -kappat * fvc::grad(T);
    volScalarField nuSGS = turbulence->nut();
    kSGS = turbulence->k();
    volScalarField epsilonSGS = turbulence->epsilon();


    // Compute the running time averages of the fields and then get the fluctuation.
    if (runTime.value() >= avgStartTime)
    {
        UAvg = (UAvg * avgTimeSum) + (runTime.deltaT().value() * U);
        TAvg = (TAvg * avgTimeSum) + (runTime.deltaT().value() * T);
        p_rghAvg = (p_rghAvg * avgTimeSum) + (runTime.deltaT().value() * p_rgh);
        Rmean = (Rmean * avgTimeSum) + (runTime.deltaT().value() * R);
        qmean = (qmean * avgTimeSum) + (runTime.deltaT().value() * q);
        nuSGSmean = (nuSGSmean * avgTimeSum) + (runTime.deltaT().value() * nuSGS);
        kSGSmean = (kSGSmean * avgTimeSum) + (runTime.deltaT().value() * kSGS);
        epsilonSGSmean = (epsilonSGSmean * avgTimeSum) + (runTime.deltaT().value() * epsilonSGS);

        avgTimeSum += runTime.deltaT().value();
        Info << "avgTimeSum = " << avgTimeSum << endl;

        UAvg = UAvg / avgTimeSum;
        TAvg = TAvg / avgTimeSum;
        p_rghAvg = p_rghAvg / avgTimeSum;
        Rmean = Rmean / avgTimeSum;
        qmean = qmean / avgTimeSum;
        nuSGSmean = nuSGSmean / avgTimeSum;
        kSGSmean = kSGSmean / avgTimeSum;
        epsilonSGSmean = epsilonSGSmean / avgTimeSum;
    }

    // Compute the running time averages of the correlation fields.
    if (runTime.value() >= corrStartTime)
    {
        uPrime = U - UAvg;
        TPrime = T - TAvg;

        uuPrime2 = (uuPrime2 * corrTimeSum) + (runTime.deltaT().value() * Foam::sqr(uPrime));
        uTPrime2 = (uTPrime2 * corrTimeSum) + (runTime.deltaT().value() * TPrime * uPrime);
        TTPrime2 = (TTPrime2 * corrTimeSum) + (runTime.deltaT().value() * Foam::sqr(TPrime));

        corrTimeSum += runTime.deltaT().value();
        Info << "corrTimeSum = " << corrTimeSum << endl;

        uuPrime2 = uuPrime2 / corrTimeSum;
        uTPrime2 = uTPrime2 / corrTimeSum;
        TTPrime2 = TTPrime2 / corrTimeSum;

        forAll(uRMS,cellI)
        {
            uRMS[cellI].x() = Foam::sqrt(uuPrime2[cellI].xx());
            uRMS[cellI].y() = Foam::sqrt(uuPrime2[cellI].yy());
            uRMS[cellI].z() = Foam::sqrt(uuPrime2[cellI].zz());
        }
        TRMS = Foam::sqrt(TTPrime2);

        forAll(kResolved,cellI)
        {
            kResolved[cellI] = 0.5 * (uuPrime2[cellI].xx() + uuPrime2[cellI].yy() + uuPrime2[cellI].zz());
        }
    }
}
