            // Use a rotor speed vs. generator torque look-up table.
            
            generatorTorqueCommanded = interpolate(genSpeedF,SpeedGenProfile[j],TorqueGenProfile[j]);
            generatorTorqueCommanded = min(max(0.0,generatorTorqueCommanded),max(TorqueGenProfile[j]));
