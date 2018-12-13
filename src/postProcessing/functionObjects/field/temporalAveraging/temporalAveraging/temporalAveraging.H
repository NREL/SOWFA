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

Class
    Foam::temporalAveraging

Group
    grpFieldFunctionObjects

Description
    This function object is an extension of the standard OpenFOAM fieldAverage
    class. The main functionality of fieldAverage is retained, i.e., this
    function object can be used to calculate average quantities for a
    user-specified selection of volumetric and surface fields. The extended
    functionality allows to compute the correlation between the field and the
    velocity field.
    
    
    Fields are entered as a list
    of sub-dictionaries, which indicate the type of averages to perform, and
    can be updated during the calculation.  The current options include:
    - \c mean: arithmetic mean:
        \f[
            \overline{x} = \frac{1}{N}\displaystyle\sum\limits_{i=0}^N x_i
        \f]
    - \c prime2Mean: prime-squared mean
        \f[
            \overline{x'}^2 = \frac{1}{N}\displaystyle\sum\limits_{i=0}^N
            (x_i - \overline{x})^2
        \f]
    - \c corrUPrimeMean: mean of the correlation with velocity fluctuations
        \f[
            \overline{x'u'} = \frac{1}{N}\displaystyle\sum\limits_{i=0}^N
            (x_i - \overline{x})(u_i - \overline{u})
        \f]
    - base: average over 'time', or 'iteration' (\f$N\f$ in the above)
    - window: optional averaging window, specified in 'base' units

    Average field names are constructed by concatenating the base field with
    the averaging type, e.g. when averaging field 'U', the resultant fields
    are:
    - arithmetic mean field, UMean
    - prime-squared field, UPrime2Mean
    Correlation names are constructed in a similar way, e.g., when computing
    the correlation between the field 'T' and 'U', the result is:
    - correlation field, TPrimeUPrimeMean

    Information regarding the number of averaging steps, and total averaging
    time are written on a per-field basis to the
    \c temporalAveragingProperties dictionary, located in \<time\>/uniform

    When restarting form a previous calculation, the averaging is continuous.
    However, the averaging process can be restarted using the \c resetOnRestart
    option.

    To restart the averaging process after each calculation output time, use
    the \c resetOnOutput option.

    Example of function object specification:
    \verbatim
    temporalAveraging1
    {
        type temporalAveraging;
        functionObjectLibs ("libfieldFunctionObjects.so");
        ...
        resetOnRestart true;
        resetOnOutput false;
        fields
        (
            U
            {
                mean            on;
                prime2Mean      on;
                corrUPrimeMean  off;
                base            time;
                window          10.0;
                windowName      w1;
            }
            p
            {
                mean            on;
                prime2Mean      on;
                corrUPrimeMean  on;
                base            time;
            }
        );
    }
    \endverbatim

    \heading Function object usage
    \table
        Property     | Description             | Required    | Default value
        type         | type name: temporalAveraging | yes         |
        resetOnRestart | flag to reset the averaging on restart | yes  |
        resetOnOutput| flag to reset the averaging on output | yes |
        fields       | list of fields and averaging options | yes |
    \endtable


Note
    To employ the \c prime2Mean option, the \c mean option must be selected.
    To employ the \c corrUPrimeMean option, the \c mean option must be selected
        for both the baseField and for U. Moreover, the corrUPrimeMean option is
        ignored for the 'U' field as this would be identical to the prime2Mean option

SeeAlso
    Foam::functionObject
    Foam::OutputFilterFunctionObject

SourceFiles
    temporalAveraging.C
    temporalAveragingTemplates.C
    temporalAveragingItem.C

\*---------------------------------------------------------------------------*/

#ifndef temporalAveraging_H
#define temporalAveraging_H

#include "volFieldsFwd.H"
#include "Switch.H"

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

namespace Foam
{

// Forward declaration of classes
class objectRegistry;
class dictionary;
class temporalAveragingItem;
template<class Type>
class List;
class polyMesh;
class mapPolyMesh;

/*---------------------------------------------------------------------------*\
                        Class temporalAveraging Declaration
\*---------------------------------------------------------------------------*/

class temporalAveraging
{
protected:

    // Protected data

        //- Name of this set of field averages.
        word name_;

        //- Database this class is registered to
        const objectRegistry& obr_;

        //- On/off switch
        bool active_;

        //- Time at last call, prevents repeated averaging
        label prevTimeIndex_;

        //- Reset the averaging process on restart flag
        Switch resetOnRestart_;

        //- Reset the averaging process on output flag
        Switch resetOnOutput_;

        //- Initialised flag
        bool initialised_;

        //- List of field average items, describing what averages to be
        //  calculated and output
        List<temporalAveragingItem> faItems_;

        // Counters

            //- Iteration steps counter
            List<label> totalIter_;

            //- Total time counter
            List<scalar> totalTime_;


    // Private Member Functions

        // Initialisation routines

            //- Checkout fields (causes deletion) from the database
            //  and reset lists
            void resetFields();

            //- Reset lists (clear existing values) and initialize averaging.
            //  Check requested field averages are valid, populate field lists
            void initialize();

            //- Add mean average field to database
            template<class Type>
            void addMeanFieldType(const label fieldI);

            //- Add mean average field to database
            template<class Type>
            void addMeanField(const label fieldI);

            //- Add prime-squared average field to database
            template<class Type1, class Type2>
            void addPrime2MeanFieldType(const label fieldI);

            //- Add prime-squared average field to database
            template<class Type1, class Type2>
            void addPrime2MeanField(const label fieldI);


        // Calculation functions

            //- Main calculation routine
            virtual void calcAverages();

            //- Calculate mean average fields
            template<class Type>
            void calculateMeanFieldType(const label fieldI) const;

            //- Calculate mean average fields
            template<class Type>
            void calculateMeanFields() const;

            //- Calculate prime-squared average fields
            template<class Type1, class Type2>
            void calculatePrime2MeanFieldType(const label fieldI) const;

            //- Calculate prime-squared average fields
            template<class Type1, class Type2>
            void calculatePrime2MeanFields() const;

            //- Add mean-squared field value to prime-squared mean field
            template<class Type1, class Type2>
            void addMeanSqrToPrime2MeanType(const label fieldI) const;

            //- Add mean-squared field value to prime-squared mean field
            template<class Type1, class Type2>
            void addMeanSqrToPrime2Mean() const;


        // I-O

            //- Write averages
            virtual void writeAverages() const;

            //- Write fields
            template<class Type>
            void writeFieldType(const word& fieldName) const;

            //- Write fields
            template<class Type>
            void writeFields() const;

            //- Write averaging properties - steps and time
            void writeAveragingProperties() const;

            //- Read averaging properties - steps and time
            void readAveragingProperties();


        //- Disallow default bitwise copy construct
        temporalAveraging(const temporalAveraging&);

        //- Disallow default bitwise assignment
        void operator=(const temporalAveraging&);


public:

    //- Runtime type information
    TypeName("temporalAveraging");


    // Constructors

        //- Construct for given objectRegistry and dictionary.
        //  Allow the possibility to load fields from files
        temporalAveraging
        (
            const word& name,
            const objectRegistry&,
            const dictionary&,
            const bool loadFromFiles = false
        );


    //- Destructor
    virtual ~temporalAveraging();


    // Member Functions

        //- Return name of the set of field averages
        virtual const word& name() const
        {
            return name_;
        }

        //- Read the field average data
        virtual void read(const dictionary&);

        //- Execute the averaging
        virtual void execute();

        //- Execute the averaging at the final time-loop, currently does nothing
        virtual void end();

        //- Called when time was set at the end of the Time::operator++
        virtual void timeSet();

        //- Calculate the field average data and write
        virtual void write();

        //- Update mesh
        virtual void updateMesh(const mapPolyMesh&);

        //- Move points
        virtual void movePoints(const polyMesh&);
};


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

} // End namespace Foam

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#ifdef NoRepository
#   include "temporalAveragingTemplates.C"
#endif

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //

#endif

// ************************************************************************* //
