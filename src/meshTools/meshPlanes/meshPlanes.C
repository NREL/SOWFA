/*---------------------------------------------------------------------------*\
  =========                 |
  \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox
   \\    /   O peration     |
    \\  /    A nd           | Copyright (C) 2011-2013 OpenFOAM Foundation
     \\/     M anipulation  |
-------------------------------------------------------------------------------
License
    This file is part of OpenFOAM.

    OpenFOAM is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    OpenFOAM is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with OpenFOAM.  If not, see <http://www.gnu.org/licenses/>.

\*---------------------------------------------------------------------------*/

#include "meshPlanes.H"


// * * * * * * * * * * * * * * Static Data Members * * * * * * * * * * * * * //

namespace Foam
{
    defineTypeNameAndDebug(meshPlanes, 0);
    
    scalar meshPlanes::tol_ = 1.0E-8;
}


// * * * * * * * * * * * * * Private Member Functions  * * * * * * * * * * * //

void Foam::meshPlanes::findPlanes()
{

    // On each processor, do a search through all cells and make a list of 
    // all distinct plane locations

    DynamicList<scalar> planeLocationValuesP(0);
    numberOfPlanes_ = 0;

    forAll(mesh_.cells(),cellI)
    {
	    const vector& cellCenter = mesh_.C()[cellI];
    	int planesFlag = 0;
        if(cellI == 0)
	    {
	        planeLocationValuesP.append(cellCenter[normalIndex_]);
	        numberOfPlanes_++;
	    }
	    else
	    {
	        forAll(planeLocationValuesP,planeI)
	        {
	            const scalar loc = planeLocationValuesP[planeI];
		        if(mag(cellCenter[normalIndex_] - loc) < tol_)
		        {
                    planesFlag = 1;
		        }
	        }
	        if(planesFlag == 0)
	        {
	            planeLocationValuesP.append(cellCenter[normalIndex_]);
		        numberOfPlanes_++;
	        }
	    }
    }
    reduce(numberOfPlanes_,sumOp<label>());


    // Now combine the plane location lists from each processor on the master
    List<List<scalar> > planeLocationValuesM(Pstream::nProcs());
    planeLocationValuesM[Pstream::myProcNo()] = planeLocationValuesP;
    Pstream::gatherList(planeLocationValuesM);
    Pstream::scatterList(planeLocationValuesM);



    // Remove duplicate values in the list of plane locations
    DynamicList<scalar> planeLocationValues_Unsrt;
    {
        label I = 0;
        numberOfPlanes_ = 0;
        forAll(planeLocationValuesM,ListI)
        {
            forAll(planeLocationValuesM[ListI],ListJ)
            {
	            label planesFlag = 0;
	            if(I == 0)
	            {
	                planeLocationValues_Unsrt.append(planeLocationValuesM[ListI][ListJ]);
		            numberOfPlanes_++;
	            }
	            else
	            {
	                for(label J = 0; J < numberOfPlanes_; J++)
	                {
	                    const scalar loc = planeLocationValues_Unsrt[J];
	                    if(mag(planeLocationValuesM[ListI][ListJ] - loc) < tol_)
	  	                {
		                    planesFlag = 1;
		                }
	                }
	                if(planesFlag == 0)
	                {
	                    planeLocationValues_Unsrt.append(planeLocationValuesM[ListI][ListJ]);
		                numberOfPlanes_++;
	                }
                }
                I++;
           }
        }
    }
    planeLocationValues_ = planeLocationValues_Unsrt;
    //Info << endl << "Plane Location Values: " << planeLocationValues_ << endl;


    // Sort the height list to be in ascending order
    forAll(planeLocationValues_,planeI)
    {
	    scalar planeLocationValuesMin = 1.0E20;
    	scalar locValue = 0.0;
	    label locLabelJ = 0;
	    for(int planeJ = planeI; planeJ < numberOfPlanes_; planeJ++)
	    {
            if(planeLocationValues_[planeJ] < planeLocationValuesMin)
	        {
	            locValue = planeLocationValues_[planeJ];
		        locLabelJ = planeJ;
		        planeLocationValuesMin = locValue;
	        }
	    }
    	planeLocationValues_[locLabelJ] = planeLocationValues_[planeI];
	    planeLocationValues_[planeI] = locValue;
    }


    // Make a list of lists of cell ID labels.  Each list within the list contains
    // the cell ID labels corresponding to a specific plane on a processor.  The overall list
    // should contain as many cell ID labels lists as there are distinct planes.
    // Also sum up the volume of cells in each plane on each processor.
    List<scalar> totVolPerPlaneP(numberOfPlanes_);
   
    forAll(planeLocationValues_,planeI)
    {
	    numCellPerPlane_.append(0);
	    totVolPerPlaneP[planeI] = 0.0;
        forAll(mesh_.cells(),cellI)
        {
            const vector& cellCenter = mesh_.C()[cellI];
	        if(mag(cellCenter[normalIndex_] - planeLocationValues_[planeI]) < tol_)
	        {
	            numCellPerPlane_[planeI]++;
		        totVolPerPlaneP[planeI] += mesh_.V()[cellI];
	        }
	    }
    }

    forAll(planeLocationValues_,planeI)
    {
        //planesCellList_.append(List<label>(max(numCellPerPlane_)));
        planesCellList_.append(List<label>(numCellPerPlane_[planeI]));
	    int i = 0;
        forAll(mesh_.cells(),cellI)
	    {
	        const vector& cellCenter = mesh_.C()[cellI];
	        if(mag(cellCenter[normalIndex_] - planeLocationValues_[planeI]) < tol_)
	        {
        		planesCellList_[planeI][i] = cellI;
		        i++;
    	    }
	    }
    }

    // Gather the volumes per plane to get a global value
    List<List<scalar> > totVolPerPlaneM(Pstream::nProcs());
    totVolPerPlaneM[Pstream::myProcNo()] = totVolPerPlaneP;
    Pstream::gatherList(totVolPerPlaneM);
    Pstream::scatterList(totVolPerPlaneM);
    
    forAll(planeLocationValues_,planeI)
    {
        totVolPerPlane_.append(0.0);
    	for (int n = 0; n < Pstream::nProcs(); n++)
	    {
            totVolPerPlane_[planeI] += totVolPerPlaneM[n][planeI];
        }
    }


//    forAll(numCellPerPlane_,i)
//    {
//        label numCellPerPlaneGlobal = numCellPerPlane_[i];
//        reduce(numCellPerPlaneGlobal,sumOp<label>());
//    }

}


// * * * * * * * * * * * * * * * * Constructors  * * * * * * * * * * * * * * //

Foam::meshPlanes::meshPlanes
(
    const fvMesh& mesh,
    const word& normal
)
:
    mesh_(mesh)
{

    if (normal == "x")
    {
        normalIndex_ = 0;
    }
    else if (normal == "y")
    {
        normalIndex_ = 1;
    }
    else if (normal == "z")
    {
        normalIndex_ = 2;
    }


    findPlanes();

    Info << "number of meshPlanes: " << numberOfPlanes_ << endl;
    Info << totVolPerPlane_ << endl;

}


Foam::meshPlanes::meshPlanes
(
    const fvMesh& mesh
)
:
    mesh_(mesh)
{
    // Default to z planes
    normalIndex_ = 2;

    findPlanes();
}


// * * * * * * * * * * * * * * * * Destructor  * * * * * * * * * * * * * * * //

Foam::meshPlanes::~meshPlanes()
{}


// * * * * * * * * * * * * * * * Member Functions  * * * * * * * * * * * * * //


template<class Type>
List<Type> Foam::meshPlanes::average
(
    const GeometricField<Type, fvPatchField, volMesh>& vField
)
{
    List<Type> vFieldMean(numberOfPlanes_,Type::zero);

    forAll(planeLocationValues_,planeI)
    {
        for (label i = 0; i < numCellPerPlane_[planeI]; i++)
        {
            label cellI = planesCellList_[planeI][i];
            vFieldMean[planeI] += vField[cellI] * mesh_.V()[cellI];
        }
    }

    reduce(vFieldMean,sumOp<List<Type> >());

    forAll(planeLocationValues_,planeI)
    {
        vFieldMean[planeI] /= totVolPerPlane_[planeI];
    }

    return vFieldMean;
}


template<class Type>
Type Foam::meshPlanes::average
(
    const GeometricField<Type, fvPatchField, volMesh>& vField,
    label planeI
)
{
    Type vFieldMean(Type::zero);

    for (label i = 0; i < numCellPerPlane_[planeI]; i++)
    {
        label cellI = planesCellList_[planeI][i];
        vFieldMean += vField[cellI] * mesh_.V()[cellI];
    }

    reduce(vFieldMean,sumOp<Type>());

    vFieldMean /= totVolPerPlane_[planeI];

    return vFieldMean;
}


// Explicit instantiation of template function average<Type>
template vector Foam::meshPlanes::average<vector>(const volVectorField&, label);
template List<vector> Foam::meshPlanes::average<vector>(const volVectorField&);
template symmTensor Foam::meshPlanes::average<symmTensor>(const volSymmTensorField&, label);
template List<symmTensor> Foam::meshPlanes::average<symmTensor>(const volSymmTensorField&);

// Specialization for average<scalar>
// Reason: scalar::zero does not exist, so initialization must use 0.0 instead
// Note that template specialization must be within Foam namespace,
// and providing the namespace as part of the type does not work in this case.
namespace Foam
{
    template<>
    List<scalar> meshPlanes::average<scalar>
    (
        const GeometricField<scalar, fvPatchField, volMesh>& vField
    )
    {
        List<scalar> vFieldMean(numberOfPlanes_,0.0);
    
        forAll(planeLocationValues_,planeI)
        {
            for (label i = 0; i < numCellPerPlane_[planeI]; i++)
            {
                label cellI = planesCellList_[planeI][i];
                vFieldMean[planeI] += vField[cellI] * mesh_.V()[cellI];
            }
        }
    
        reduce(vFieldMean,sumOp<List<scalar> >());
    
        forAll(planeLocationValues_,planeI)
        {
            vFieldMean[planeI] /= totVolPerPlane_[planeI];
        }
    
        return vFieldMean;
    }


    template<>
    scalar meshPlanes::average<scalar>
    (
        const GeometricField<scalar, fvPatchField, volMesh>& vField,
        label planeI
    )
    {
        scalar vFieldMean(0.0);
    
        for (label i = 0; i < numCellPerPlane_[planeI]; i++)
        {
            label cellI = planesCellList_[planeI][i];
            vFieldMean += vField[cellI] * mesh_.V()[cellI];
        }
    
        reduce(vFieldMean,sumOp<scalar>());
    
        vFieldMean /= totVolPerPlane_[planeI];
    
        return vFieldMean;
    }
}


// ************************************************************************* //
