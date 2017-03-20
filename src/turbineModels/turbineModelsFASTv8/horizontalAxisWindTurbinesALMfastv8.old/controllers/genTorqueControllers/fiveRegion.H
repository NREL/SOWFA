            // Five region controller torque controller based on Jonkman et al.'s implementation in the
            // NREL 5 MW Reference turbine model.      


            scalar RatedGenSpeed = (RatedRotSpeed[j]/rpmRadSec)*GBRatio[j];


            // Region 1.
            if (genSpeedF < CutInGenSpeed[j])
            {
                //Info << "Region 1" << endl;
                generatorTorqueCommanded = CutInGenTorque[j];
            }
            // Region 1-1/2.
            else if ((genSpeedF >= CutInGenSpeed[j]) && (genSpeedF < Region2StartGenSpeed[j]))
            {
                //Info << "Region 1-1/2" << endl;
                scalar dGenSpeed = genSpeedF - CutInGenSpeed[j];
                scalar Region2StartGenTorque = KGen[j] * Region2StartGenSpeed[j] * Region2StartGenSpeed[j];
                scalar torqueSlope = (Region2StartGenTorque - CutInGenTorque[j]) / ( Region2StartGenSpeed[j] - CutInGenSpeed[j] );
                generatorTorqueCommanded = CutInGenTorque[j] + torqueSlope*dGenSpeed;
            }
            // Region 2.
            else if ((genSpeedF >= Region2StartGenSpeed[j]) && (genSpeedF < Region2EndGenSpeed[j]))
            {
                //Info << "Region 2" << endl;
                generatorTorqueCommanded = KGen[j] * genSpeedF * genSpeedF;
            }
            // Region 2-1/2.
            else if ((genSpeedF >= Region2EndGenSpeed[j]) && (genSpeedF < RatedGenSpeed))
            {
                //Info << "Region 2-1/2" << endl;
                scalar dGenSpeed = genSpeedF - Region2EndGenSpeed[j];
                scalar Region2EndGenTorque = KGen[j] * Region2EndGenSpeed[j] * Region2EndGenSpeed[j];
                scalar torqueSlope = (RatedGenTorque[j] - Region2EndGenTorque) / ( RatedGenSpeed - Region2EndGenSpeed[j] );
                generatorTorqueCommanded = Region2EndGenTorque + torqueSlope*dGenSpeed;
            }
            // Region 3.
            else if (genSpeedF >= RatedGenSpeed)
            {
                //Info << "Region 3" << endl;
              //generatorTorqueCommanded = RatedGenTorque[j];
                generatorTorqueCommanded = (RatedGenTorque[j] * RatedGenSpeed) / genSpeedF;
            }
