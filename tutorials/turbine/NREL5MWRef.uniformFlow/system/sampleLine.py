try: paraview.simple
except: from paraview.simple import *
paraview.simple._DisableFirstRenderCameraReset()




D = 126.0
downstreamPosition = [-1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0];
lineDir = '../lineData'
time = 1050.0






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


for i in range(len(downstreamPosition)):
    for j in range(2):
        SetActiveSource(controlDict_foam)
        PlotOverLine1 = PlotOverLine( Source="High Resolution Line Source" )
        PlotOverLine1.Source.Resolution = 600
        if (j == 1):
            print 'Processing line ' + str(i+1) + 'y of ' + str(len(downstreamPosition)) + '...'
            PlotOverLine1.Source.Point1 = [downstreamPosition[i]*D, -378.0, 0.0]
            PlotOverLine1.Source.Point2 = [downstreamPosition[i]*D,  378.0, 0.0]
        else:
            print 'Processing line ' + str(i+1) + 'z of ' + str(len(downstreamPosition)) + '...'
            PlotOverLine1.Source.Point1 = [downstreamPosition[i]*D, 0.0, -378.0]
            PlotOverLine1.Source.Point2 = [downstreamPosition[i]*D, 0.0,  378.0]

        XYChartView1 = CreateXYPlotView()
        XYChartView1.ViewTime = time


        DataRepresentation2 = Show()
        DataRepresentation2.XArrayName = 'arc_length'
        DataRepresentation2.SeriesVisibility = ['UMean (0)', '0', 'UMean (1)', '0', 'UMean (2)', '0', 'uuMean (0)', '0', 'uuMean (1)', '0', 'uuMean (2)', '0', 'uuMean (3)', '0', 'uuMean (4)', '0', 'uuMean (5)', '0', 'vtkValidPointMask', '0', 'arc_length', '0', 'Points (0)', '0', 'Points (1)', '0', 'Points (2)', '0', 'Points (Magnitude)', '0', 'vtkOriginalIndices', '0']
        DataRepresentation2.UseIndexForXAxis = 0

        AnimationScene2.ViewModules = [ RenderView2, XYChartView1 ]

        RenderView2.CameraClippingRange = [6067.958974541466, 7939.295716322816]

        DataRepresentation2.SeriesColor = ['uuMean (Magnitude)', '0.976471', '0.513725', '0.141176']
        DataRepresentation2.SeriesVisibility = ['UMean (0)', '0', 'UMean (1)', '0', 'UMean (2)', '0', 'uuMean (0)', '0', 'uuMean (1)', '0', 'uuMean (2)', '0', 'uuMean (3)', '0', 'uuMean (4)', '0', 'uuMean (5)', '0', 'vtkValidPointMask', '0', 'arc_length', '0', 'Points (0)', '0', 'Points (1)', '0', 'Points (2)', '0', 'Points (Magnitude)', '0', 'vtkOriginalIndices', '0', 'uuMean (Magnitude)', '1']

        Render()
    
        if (j == 1):
            fileName = lineDir + '/line_y_' + str(int(downstreamPosition[i])) + '.csv'
        else:
            fileName = lineDir + '/line_z_' + str(int(downstreamPosition[i])) + '.csv'

        writer = CreateWriter(fileName, PlotOverLine1)
        writer.FieldAssociation = "Points"
        writer.UpdatePipeline()

        del writer
        del PlotOverLine1
        del DataRepresentation2
