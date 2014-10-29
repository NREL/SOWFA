// find all cells that lie within height range.

DynamicList<label> windHeightCell;
scalar windHeightCellVolumeTotal = 0.0;

forAll(mesh.cells(),celli)
{
    const vector& cellCenter = mesh.C()[celli];

    if ((cellCenter.z() >= hWind.value() - 0.5 * hWindWidth.value()) &&
        (cellCenter.z() <= hWind.value() + 0.5 * hWindWidth.value()))
    {
        windHeightCell.append(celli);
        windHeightCellVolumeTotal += mesh.V()[celli];
    }
}

reduce(windHeightCellVolumeTotal,sumOp<scalar>());
