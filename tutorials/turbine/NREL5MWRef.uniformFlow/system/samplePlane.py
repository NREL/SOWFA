D = 126.0
downstreamPosition = [-1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0];
planeDir = '../planeData'
time = 1050.0






try: paraview.simple
except: from paraview.simple import *
paraview.simple._DisableFirstRenderCameraReset()

controlDict_foam = OpenFOAMReader( FileName='/scratch/mchurchf/ALM_comparison_JHU/run.Smag_Cs08_eps10/system/controlDict.foam', TimestepValues=[1800.0], CaseType='Decomposed Case', MeshRegions='internalMesh',CellArrays=['UMean','uuMean'])

controlDict_foam.CellArrays = ['cellDist', 'flm', 'fmm', 'nuSgs', 'p', 'U']
controlDict_foam.MeshRegions = ['internalMesh']

AnimationScene2 = GetAnimationScene()
controlDict_foam.CellArrays = []
controlDict_foam.CaseType = 'Decomposed Case'
controlDict_foam.MeshRegions = ['internalMesh']

AnimationScene2.AnimationTime = time

controlDict_foam.CellArrays = ['bodyForce', 'flm_0', 'fmm_0', 'kMean', 'omega', 'pMean', 'ppMean', 'Q', 'RMean', 'U_0', 'UMean', 'upMean', 'uuMean', 'uuRTotal']
controlDict_foam.MeshRegions = ['internalMesh']

AnimationScene2.AnimationTime = time
AnimationScene2.PlayMode = 'Snap To TimeSteps'
AnimationScene2.EndTime = time

RenderView2 = GetRenderView()
DataRepresentation1 = Show()
DataRepresentation1.ScalarOpacityUnitDistance = 11.382656425624814
DataRepresentation1.Representation = 'Outline'
DataRepresentation1.EdgeColor = [0.0, 0.0, 0.5000076295109483]

RenderView2.CenterOfRotation = [1323.0, 0.0, 0.0]

AnimationScene2.AnimationTime = time

RenderView2.CameraPosition = [1323.0, 0.0, 6889.069671254006]
RenderView2.ViewTime = time
RenderView2.CameraFocalPoint = [1323.0, 0.0, 0.0]
RenderView2.CameraClippingRange = [6067.958974541466, 7939.295716322816]
RenderView2.CameraParallelScale = 1783.0224339586982

controlDict_foam.CellArrays = ['UMean', 'uuMean']
controlDict_foam.MeshRegions = ['internalMesh']

Slice1 = Slice( SliceType="Plane" )
for i in range(len(downstreamPosition)):
    print 'Processing y-z plane ' + str(i+1) + ' of ' + str(len(downstreamPosition)) + '...'


#   Slice1 = Slice( SliceType="Plane" )

    Slice1.SliceOffsetValues = [0.0]
    Slice1.SliceType.Origin = [downstreamPosition[i]*D, 0.0, 0.0]
    Slice1.SliceType = "Plane"
    Slice1.SliceType.Normal = [1.0, 0.0, 0.0]

    DataRepresentation2 = Show()
    DataRepresentation2.EdgeColor = [0.0, 0.0, 0.5000076295109483]

    RenderView2.CameraClippingRange = [5908.096896294395, 8140.529136427796]

    RenderView2.CameraClippingRange = [5908.096896294395, 8140.529136427796]

    Render()

    fileName = planeDir + '/plane_yz_' + str(int(downstreamPosition[i])) + '.csv'

    writer = CreateWriter(fileName, Slice1)
    writer.FieldAssociation = "Points"
    writer.UpdatePipeline()

    del writer
 #  del Slice1
    del DataRepresentation2







print 'Processing x-y plane...'
#lice1 = Slice( SliceType="Plane" )

Slice1.SliceOffsetValues = [0.0]
Slice1.SliceType.Origin = [0.0, 0.0, 0.0]
Slice1.SliceType = "Plane"
Slice1.SliceType.Normal = [0.0, 0.0, 1.0]

DataRepresentation2 = Show()
DataRepresentation2.EdgeColor = [0.0, 0.0, 0.5000076295109483]

RenderView2.CameraClippingRange = [5908.096896294395, 8140.529136427796]

RenderView2.CameraClippingRange = [5908.096896294395, 8140.529136427796]

Render()

fileName = planeDir + '/plane_xy_' + str(int(downstreamPosition[i])) + '.csv'

writer = CreateWriter(fileName, Slice1)
writer.FieldAssociation = "Points"
writer.UpdatePipeline()

del writer
#el Slice1
del DataRepresentation2






print 'Processing x-z plane...'
#lice1 = Slice( SliceType="Plane" )

Slice1.SliceOffsetValues = [0.0]
Slice1.SliceType.Origin = [0.0, 0.0, 0.0]
Slice1.SliceType = "Plane"
Slice1.SliceType.Normal = [0.0, 1.0, 0.0]

DataRepresentation2 = Show()
DataRepresentation2.EdgeColor = [0.0, 0.0, 0.5000076295109483]

RenderView2.CameraClippingRange = [5908.096896294395, 8140.529136427796]

RenderView2.CameraClippingRange = [5908.096896294395, 8140.529136427796]

Render()

fileName = planeDir + '/plane_xz_' + str(int(downstreamPosition[i])) + '.csv'

writer = CreateWriter(fileName, Slice1)
writer.FieldAssociation = "Points"
writer.UpdatePipeline()

del writer
#el Slice1
del DataRepresentation2
