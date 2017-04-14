            // Limit the change in nacelle yaw.

            scalar nacYawRateCommanded = deltaNacYaw[i] / dt;
            nacYawRateCommanded = min(max(nacYawRateCommanded, -(RateLimitNacYaw[j]*degRad)), (RateLimitNacYaw[j]*degRad));
            deltaNacYaw[i] = nacYawRateCommanded * dt;
            nacYawCommanded = nacYaw[i] + deltaNacYaw[i];
