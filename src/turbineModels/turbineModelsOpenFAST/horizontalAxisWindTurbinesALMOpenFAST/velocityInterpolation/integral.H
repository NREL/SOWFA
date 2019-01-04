// Use trilinear interpolation using nearest cell center and cell-center
// gradient information.
forAll(bladeInfluenceCells[i], m)
{
    vector disVector = (mesh_.C()[bladeInfluenceCells[i][m]] - point);
    scalar dis = mag(disVector);
    if (dis <= bladeProjectionRadius[i])
    {
        // Compute the body force projection.
        scalar spreading = computeBladeProjectionFunction(disVector,i,j,k);

        // Sum up this mesh cell's contribution to the integrated velocity, 
        // weighted by the projection function.
        velocity += U_[bladeInfluenceCells[i][m]] * spreading * mesh_.V()[bladeInfluenceCells[i][m]];
    }
}
