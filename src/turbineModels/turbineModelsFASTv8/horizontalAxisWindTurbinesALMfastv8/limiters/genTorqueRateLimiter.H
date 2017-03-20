            // Limit the change in generator torque.
            //
            scalar torqueGenRateCommanded = (generatorTorqueCommanded - generatorTorque[i]) / dt;
            torqueGenRateCommanded = min(max(torqueGenRateCommanded, -RateLimitGenTorque[j]), RateLimitGenTorque[j]);
            generatorTorqueCommanded = generatorTorque[i] + (torqueGenRateCommanded * dt);
