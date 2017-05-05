            // Limit the rotor speed to be positive and such that the generator does not turn
            // faster than rated.

            rotorSpeed[i] = min(max(0.0,rotorSpeed[i]),RatedRotSpeed[j]);
