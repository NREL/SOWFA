{
    //volVectorField uPrime = U;
    //volScalarField TPrime = T;
    volSymmTensorField R = 1.0*turbulence->R();
    volVectorField q = -kappat * fvc::grad(T);
    volScalarField nuSGS = 1.0*turbulence->nut();
    kSGS = 1.0*turbulence->k();
    volScalarField epsilonSGS = 1.0*turbulence->epsilon();


    // Compute the running time averages of the fields
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
        UUAvg = (UUAvg * avgTimeSum) + (runTime.deltaT().value() * Foam::sqr(U));
        TTAvg = (TTAvg * avgTimeSum) + (runTime.deltaT().value() * Foam::sqr(T));
        UTAvg = (UTAvg * avgTimeSum) + (runTime.deltaT().value() * T*U);

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
        UUAvg = UUAvg / avgTimeSum;
        TTAvg = TTAvg / avgTimeSum;
        UTAvg = UTAvg / avgTimeSum;

        uuPrime2 = UUAvg - Foam::sqr(UAvg);       // calculate time-averaged u'u' from instantaneous UUAvg & UAvg, without needing long-term UAvg
        uTPrime2 = UTAvg - (TAvg * UAvg);
        TTPrime2 = TTAvg - Foam::sqr(TAvg);

        forAll(uRMS,cellI)
        {
            uRMS[cellI].x() = Foam::sqrt(Foam::mag(uuPrime2[cellI].xx())); // Foam::mag ensures won't crash with sqrt(-value) floating point exception
            uRMS[cellI].y() = Foam::sqrt(Foam::mag(uuPrime2[cellI].yy()));
            uRMS[cellI].z() = Foam::sqrt(Foam::mag(uuPrime2[cellI].zz()));
        }
        TRMS = Foam::sqrt(Foam::mag(TTPrime2));

        forAll(kResolved,cellI)
        {
            kResolved[cellI] = 0.5 * (uuPrime2[cellI].xx() + uuPrime2[cellI].yy() + uuPrime2[cellI].zz());
        }
    }

/*  corrStartTime, corrTimeSum no longer used
    // Compute the running time history of uPrime, Tprime
    if (runTime.value() >= corrStartTime)
    {
        //uPrime = U - UAvg; // user should calculate themselves from long-term UAvg in post-processing
        //TPrime = T - TAvg; // user should calculate themselves from long-term TAvg in post-processing

        //uuPrime2 = (uuPrime2 * corrTimeSum) + (runTime.deltaT().value() * Foam::sqr(uPrime)); // previous version of uuPrime2
        //uTPrime2 = (uTPrime2 * corrTimeSum) + (runTime.deltaT().value() * TPrime * uPrime);   // previous version of uTPrime2
        //TTPrime2 = (TTPrime2 * corrTimeSum) + (runTime.deltaT().value() * Foam::sqr(TPrime)); // previous version of TTPrime2

        corrTimeSum += runTime.deltaT().value();
        Info << "corrTimeSum = " << corrTimeSum << endl;

        //uuPrime2 = uuPrime2 / corrTimeSum; // finalizing previous versions of uuPrime2, uTPrime2, TTPrime2
        //uTPrime2 = uTPrime2 / corrTimeSum;
        //TTPrime2 = TTPrime2 / corrTimeSum;
    }
*/
}
