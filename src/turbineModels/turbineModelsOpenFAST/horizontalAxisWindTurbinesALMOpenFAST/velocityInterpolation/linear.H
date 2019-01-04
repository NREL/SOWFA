// Use trilinear interpolation using nearest cell center and cell-center
// gradient information.
velocity = U_[cellID];
vector dx = point - mesh_.C()[cellID];
vector dU = dx & gradU[cellID];
velocity += dU;
