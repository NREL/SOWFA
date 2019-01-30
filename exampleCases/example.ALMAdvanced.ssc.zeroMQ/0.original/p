/*--------------------------------*- C++ -*----------------------------------*\
| =========                 |                                                 |
| \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox           |
|  \\    /   O peration     | Version:  1.6                                   |
|   \\  /    A nd           | Web:      http://www.OpenFOAM.org               |
|    \\/     M anipulation  |                                                 |
\*---------------------------------------------------------------------------*/
FoamFile
{
    version     2.0;
    format      ascii;
    class       volScalarField;
    location    "0000";
    object      p;
}
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#include        "../setUp"

 
dimensions      [0 2 -2 0 0 0 0];
 
internalField   uniform 0;

boundaryField
{
    lower
    {
        type            zeroGradient;
        value           uniform 0;
    }
    upper
    {
        type            zeroGradient;
        value           uniform 0;
    }
    east
    {
        type            fixedValue;
        value           uniform 0;
    }
    west
    {
        type            zeroGradient;
        value           uniform 0;
    }
    south
    {
        type            zeroGradient;
        value           uniform 0;
    }
    north
    {
        type            zeroGradient;
        value           uniform 0;
    }
}


// ************************************************************************* //
