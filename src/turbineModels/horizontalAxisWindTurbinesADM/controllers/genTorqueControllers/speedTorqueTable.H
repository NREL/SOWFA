            // Use a rotor speed vs. generator torque look-up table.
            
            torqueGenCommanded = interpolate(genSpeedF,SpeedGenProfile[j],TorqueGenProfile[j]);
            torqueGenCommanded = min(max(0.0,torqueGenCommanded),max(TorqueGenProfile[j]));
