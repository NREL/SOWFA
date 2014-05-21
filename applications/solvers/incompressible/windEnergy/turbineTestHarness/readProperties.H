// Get access to the input dictionary.
IOdictionary setFieldsABLDict
(
   IOobject
   (
       "windProperties",
       runTime.time().system(),
       runTime,
       IOobject::MUST_READ,
       IOobject::NO_WRITE
   )
);


// Change the table profiles from scalar lists to scalar fields
List<List<scalar> > windTable(setFieldsABLDict.lookup("windTable"));

scalarField tProfile(windTable.size(),0.0);
scalarField UProfile(windTable.size(),0.0);
scalarField VProfile(windTable.size(),0.0);
scalarField WProfile(windTable.size(),0.0);

forAll(tProfile,i)
{
   tProfile[i] = windTable[i][0];
   UProfile[i] = windTable[i][1];
   VProfile[i] = windTable[i][2];
   WProfile[i] = windTable[i][3];
}
