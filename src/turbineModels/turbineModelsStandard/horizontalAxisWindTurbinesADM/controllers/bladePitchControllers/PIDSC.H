            //_SSC_ version of PID where min pitch is set by super controller

            // If past first call to controller, apply control.
            if (pastFirstTimeStep)
            {
                // Set the gain scheduling variable.
                GK = 1.0 / (1.0 + (pitch[i]*degRad)/PitchK[j]);

                // Store the old value of speed error.
                scalar speedErrorLast = speedError[i];

                // Compute the low speed shaft speed error.
                speedError[i] = rotSpeedF[i] - RatedRotSpeed[j];

                // Numerically integrate the speed error over time.
                intSpeedError[i] += speedError[i] * dt;

                // Numerically take the deriviative of speed error w.r.t time.
                scalar derivSpeedError = (speedError[i] - speedErrorLast) / dt;

				// SSC: Convert superInfoFromSSC blade pitch angles from degrees to radians
				float lbPitch = superInfoFromSSC[i*nOutputsFromSSC+1]; // Lower bound on the pitch angle
				lbPitch = lbPitch * degRad; // Degrees to radians
	
                // Saturate the integrated speed error based on pitch saturation.
                intSpeedError[i] = min(max(intSpeedError[i], lbPitch/(GK*PitchControlKI[j])), PitchMax[j]/(GK*PitchControlKI[j]));

                // Compute the pitch components from the proportional, integral,
                // and derivative parts and sum them.
                scalar pitchP = GK * PitchControlKP[j] * speedError[i];
                scalar pitchI = GK * PitchControlKI[j] * intSpeedError[i];
                scalar pitchD = GK * PitchControlKD[j] * derivSpeedError;
                pitchCommanded = pitchP + pitchI + pitchD;

                // Saturate the pitch based on the pitch limits of the pitch
                // actuator.
                pitchCommanded = min(max(pitchCommanded, superInfoFromSSC[i*nOutputsFromSSC+1]), PitchMax[j]);
            }
            // If first call to controller, initialize some values.
            else
            {
                // set integrated speed error such that if there is no speed error
                // on the first real control step (see the else section below), there
                // will not be a change in pitch.
                GK = 1.0 / (1.0 + (pitch[i]*degRad)/PitchK[j]);
                intSpeedError[i] = (pitch[i]*degRad) / (GK * PitchControlKI[j]);
            }

