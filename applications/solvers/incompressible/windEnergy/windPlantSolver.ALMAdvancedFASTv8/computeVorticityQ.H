Q = 0.5*(sqr(tr(fvc::grad(U))) - tr(((fvc::grad(U)) & (fvc::grad(U)))));
omega = fvc::curl(U);
omegaAvg = fvc::curl(UAvg);
