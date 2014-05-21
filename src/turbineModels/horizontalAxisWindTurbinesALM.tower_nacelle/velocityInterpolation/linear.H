// Use trilinear interpolation using nearest cell center and cell-center
// gradient information.
vector dx = point - mesh_.C()[cellID];
vector dU = dx & gradU[cellID];
velocity += dU;
