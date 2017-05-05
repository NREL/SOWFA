            // Use a time vs. yaw angle look-up table.
            
            nacYawCommanded = interpolate(t,TimeYawProfile[j],YawYawProfile[j]);
            nacYawCommanded = compassToStandard(nacYawCommanded) * degRad;
