            // Limit the change in blade pitch.

            scalar pitchRateCommanded = (pitchCommanded - (pitch[i]*degRad)) / dt;
            pitchRateCommanded = min(max(pitchRateCommanded, -(RateLimitBladePitch[j]*degRad)), (RateLimitBladePitch[j]*degRad));
            pitchCommanded = (pitch[i]*degRad) + (pitchRateCommanded * dt);
