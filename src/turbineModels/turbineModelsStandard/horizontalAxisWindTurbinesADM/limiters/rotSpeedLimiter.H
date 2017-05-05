            // Limit the rotor speed to be positive and such that the generator does not turn
            // faster than rated.

            rotSpeed[i] = min(max(0.0,rotSpeed[i]),RatedRotSpeed[j]);
