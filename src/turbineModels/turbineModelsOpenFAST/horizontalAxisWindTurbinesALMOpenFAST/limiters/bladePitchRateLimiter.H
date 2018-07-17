            // Limit the change in blade pitch.

            scalar pitchRateCommanded = (bladePitchCommanded - (bladePitch[i]*degRad)) / dt;
            pitchRateCommanded = min(max(pitchRateCommanded, -(RateLimitBladePitch[j]*degRad)), (RateLimitBladePitch[j]*degRad));
            bladePitchCommanded = (bladePitch[i]*degRad) + (pitchRateCommanded * dt);
