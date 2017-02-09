            // Limit the change in generator torque.
            //
            scalar torqueGenRateCommanded = (torqueGenCommanded - torqueGen[i]) / dt;
            torqueGenRateCommanded = min(max(torqueGenRateCommanded, -RateLimitGenTorque[j]), RateLimitGenTorque[j]);
            torqueGenCommanded = torqueGen[i] + (torqueGenRateCommanded * dt);
