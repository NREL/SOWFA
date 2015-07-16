// Update the SGS fields
{

   qSgs = -(turbulence->nu()/Pr + kappat)* fvc::grad(T);
   RdevSgs =  1.0*turbulence->devReff();
   kSgs = 1.0*turbulence->k();

}
