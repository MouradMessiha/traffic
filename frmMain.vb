Imports System.Threading

Public Class frmMain

   Private Shared mblnExitThread As Boolean = False
   Private mobjFormBitmap As Bitmap
   Private Shared mobjBitmapGraphics As Graphics
   Private Shared mintFormWidth As Integer
   Private Shared mintFormHeight As Integer

   Private Shared mintXCenter As Int16
   Private Shared mintYCenter As Int16
   Private Shared mintRoadWidth As Int16

   Private mobjLightsThread As Thread

   Private Shared mobjIntersectionLights As IntersectionLights

   Private Shared mobjVehicles As List(Of Vehicle)

   Private mobjRandom As Random

   Private Class Vehicle

      Public mintRoad As Int16
      Public mintXposition As Int16
      Public mintYposition As Int16
      Private mintDistanceToCenter As Int16
      Public mdblSpeed As Double
      Private mdblMaxSpeed As Double = 200

      Private Sub TranslateDistanceToCoordinates()

         Select Case mintRoad
            Case 1
               mintXposition = mintXCenter + mintDistanceToCenter
               mintYposition = mintYCenter - (mintRoadWidth / 4)
            Case 2
               mintXposition = mintXCenter - mintDistanceToCenter
               mintYposition = mintYCenter + (mintRoadWidth / 4)
            Case 3
               mintXposition = mintXCenter - (mintRoadWidth / 4)
               mintYposition = mintYCenter - mintDistanceToCenter
            Case Else
               mintXposition = mintXCenter + (mintRoadWidth / 4)
               mintYposition = mintYCenter + mintDistanceToCenter
         End Select

      End Sub

      Private Function AtRedLight() As Boolean

         If mobjIntersectionLights.Light(mintRoad) = IntersectionLights.TrafficLightColor.Red And mintDistanceToCenter >= 10 + (mintRoadWidth / 2) And mintDistanceToCenter <= 10 + (mintRoadWidth / 2) + (2 * mdblMaxSpeed / 100) Then
            Return True
         Else
            Return False
         End If

      End Function

      Private Function BehindStoppedVehicle() As Boolean

         Dim intVehicleCount As Int16
         Dim objVehicle As Vehicle

         intVehicleCount = 0
         Do While intVehicleCount <= mobjVehicles.Count - 1
            objVehicle = mobjVehicles.Item(intVehicleCount)
            If Not objVehicle Is Me Then
               If objVehicle.mintRoad = mintRoad Then
                  If objVehicle.mdblSpeed = 0 Then
                     Select Case mintDistanceToCenter - objVehicle.mintDistanceToCenter
                        Case 0 To 30
                           Return True
                     End Select
                  End If
               End If
            End If

            intVehicleCount += 1
         Loop

         Return False

      End Function

      Public Sub Start()

         Select Case mintRoad
            Case 1, 2
               mintDistanceToCenter = mintFormWidth / 2
            Case Else
               mintDistanceToCenter = mintFormHeight / 2
         End Select
         TranslateDistanceToCoordinates()

         Do While Not mblnExitThread
            If AtRedLight() Then
               mdblSpeed = 0
            Else
               If BehindStoppedVehicle Then
                  mdblSpeed = 0
               Else
                  mdblSpeed = mdblMaxSpeed
               End If
            End If

            mintDistanceToCenter -= (mdblSpeed / 100)
            TranslateDistanceToCoordinates()
            Thread.Sleep(10)
         Loop

      End Sub

   End Class

   Private Class IntersectionLights

      Public Enum TrafficLightColor As Integer
         Red = 1
         Amber = 2
         Green = 3
      End Enum

      Public mintLight(4) As TrafficLightColor

      Public mintTimer As Int16

      Public ReadOnly Property Light(ByVal pintIndex As Int16) As TrafficLightColor
         Get
            Return mintLight(pintIndex)
         End Get
      End Property

      Public Sub Start()

         Do While Not mblnExitThread
            mintTimer = 0
            Do While Not mblnExitThread And mintTimer < 20
               mintTimer += 1
               mintLight(1) = TrafficLightColor.Green
               mintLight(2) = TrafficLightColor.Green
               mintLight(3) = TrafficLightColor.Red
               mintLight(4) = TrafficLightColor.Red
               Thread.Sleep(1000)
            Loop

            Do While Not mblnExitThread And mintTimer < 23
               mintTimer += 1
               mintLight(1) = TrafficLightColor.Amber
               mintLight(2) = TrafficLightColor.Amber
               mintLight(3) = TrafficLightColor.Red
               mintLight(4) = TrafficLightColor.Red
               Thread.Sleep(1000)
            Loop

            Do While Not mblnExitThread And mintTimer < 27
               mintTimer += 1
               mintLight(1) = TrafficLightColor.Red
               mintLight(2) = TrafficLightColor.Red
               mintLight(3) = TrafficLightColor.Red
               mintLight(4) = TrafficLightColor.Red
               Thread.Sleep(1000)
            Loop

            Do While Not mblnExitThread And mintTimer < 48
               mintTimer += 1
               mintLight(1) = TrafficLightColor.Red
               mintLight(2) = TrafficLightColor.Red
               mintLight(3) = TrafficLightColor.Green
               mintLight(4) = TrafficLightColor.Green
               Thread.Sleep(1000)
            Loop

            Do While Not mblnExitThread And mintTimer < 51
               mintTimer += 1
               mintLight(1) = TrafficLightColor.Red
               mintLight(2) = TrafficLightColor.Red
               mintLight(3) = TrafficLightColor.Amber
               mintLight(4) = TrafficLightColor.Amber
               Thread.Sleep(1000)
            Loop

            Do While Not mblnExitThread And mintTimer < 55
               mintTimer += 1
               mintLight(1) = TrafficLightColor.Red
               mintLight(2) = TrafficLightColor.Red
               mintLight(3) = TrafficLightColor.Red
               mintLight(4) = TrafficLightColor.Red
               Thread.Sleep(1000)
            Loop

         Loop

      End Sub

   End Class

   Private Sub frmMain_Activated(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Activated

      Static blnDoneOnce As Boolean = False

      If Not blnDoneOnce Then
         blnDoneOnce = True
         mintFormWidth = Me.Width
         mintFormHeight = Me.Height
         mobjFormBitmap = New Bitmap(mintFormWidth, mintFormHeight, Me.CreateGraphics())
         mobjBitmapGraphics = Graphics.FromImage(mobjFormBitmap)
         mobjBitmapGraphics.FillRectangle(Brushes.White, 0, 0, mintFormWidth, mintFormHeight)
         mobjRandom = New Random
         mobjVehicles = New List(Of Vehicle)

      End If

   End Sub

   Private Sub frmMain_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed

      mblnExitThread = True

   End Sub

   Private Sub frmMain_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown

      Dim blnStarted As Boolean = False

      Select Case e.KeyCode
         Case Keys.Enter
            If Not blnStarted Then
               blnStarted = True
               InitializeRoadData()
               StartSimulation()
            End If

      End Select

   End Sub

   Private Sub frmMain_Paint(ByVal sender As Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles Me.Paint

      e.Graphics.DrawImage(mobjFormBitmap, 0, 0)

   End Sub

   Protected Overrides Sub OnPaintBackground(ByVal pevent As System.Windows.Forms.PaintEventArgs)

      ' to remove the flickering

   End Sub

   Private Sub InitializeRoadData()

      mintXCenter = mintFormWidth / 2
      mintYCenter = mintFormHeight / 2
      mintRoadWidth = 60

   End Sub

   Private Function GetColorFromEnum(ByVal pintColor As IntersectionLights.TrafficLightColor) As Brush

      Select Case pintColor
         Case IntersectionLights.TrafficLightColor.Amber
            Return Brushes.Yellow
         Case IntersectionLights.TrafficLightColor.Green
            Return Brushes.Green
         Case IntersectionLights.TrafficLightColor.Red
            Return Brushes.Red
         Case Else
            Return Brushes.Red
      End Select

   End Function


   Private Sub StartSimulation()

      Dim objPen As Pen
      Dim objVehicle As Vehicle
      Dim objVehicleThread As Thread
      Dim intVehicleCount As Int16
      Dim objIcon(4) As Icon

      mobjIntersectionLights = New IntersectionLights
      mobjLightsThread = New Thread(AddressOf mobjIntersectionLights.Start)
      mobjLightsThread.Start()

      objPen = New Pen(Color.Black)
      For intIconCount As Int16 = 1 To 4
         objIcon(intIconCount) = New Icon(My.Application.Info.DirectoryPath + "\car" & intIconCount & ".ico")
      Next

      Do While Not mblnExitThread
         mobjBitmapGraphics.FillRectangle(Brushes.White, 0, 0, mintFormWidth, mintFormHeight)

         ' vertical road
         mobjBitmapGraphics.FillRectangle(Brushes.DarkGray, CType(mintXCenter - (mintRoadWidth / 2), Integer), 0, mintRoadWidth, mintFormHeight)
         ' horizontal road
         mobjBitmapGraphics.FillRectangle(Brushes.DarkGray, 0, CType(mintYCenter - (mintRoadWidth / 2), Integer), mintFormWidth, mintRoadWidth)

         ' vertical road divider
         mobjBitmapGraphics.FillRectangle(Brushes.Yellow, mintXCenter, 0, 1, mintFormHeight)
         ' horizontal road divider
         mobjBitmapGraphics.FillRectangle(Brushes.Yellow, 0, mintYCenter, mintFormWidth, 1)

         ' traffic 1 light
         mobjBitmapGraphics.DrawLine(Pens.Black, CType(mintXCenter - (mintRoadWidth / 2), Integer), CType(mintYCenter - (mintRoadWidth / 4) - 4, Integer), CType(mintXCenter - (mintRoadWidth / 2), Integer), CType(mintYCenter - (mintRoadWidth / 4) + 4, Integer))
         mobjBitmapGraphics.FillRectangle(GetColorFromEnum(mobjIntersectionLights.Light(1)), CType(mintXCenter - (mintRoadWidth / 2) + 1, Integer), CType(mintYCenter - (mintRoadWidth / 4) - 4, Integer), 3, 9)

         ' traffic 2 light
         mobjBitmapGraphics.DrawLine(Pens.Black, CType(mintXCenter + (mintRoadWidth / 2), Integer), CType(mintYCenter + (mintRoadWidth / 4) - 4, Integer), CType(mintXCenter + (mintRoadWidth / 2), Integer), CType(mintYCenter + (mintRoadWidth / 4) + 4, Integer))
         mobjBitmapGraphics.FillRectangle(GetColorFromEnum(mobjIntersectionLights.Light(2)), CType(mintXCenter + (mintRoadWidth / 2) - 3, Integer), CType(mintYCenter + (mintRoadWidth / 4) - 4, Integer), 3, 9)

         ' traffic 3 light
         mobjBitmapGraphics.DrawLine(Pens.Black, CType(mintXCenter - (mintRoadWidth / 4) - 4, Integer), CType(mintYCenter + (mintRoadWidth / 2), Integer), CType(mintXCenter - (mintRoadWidth / 4) + 4, Integer), CType(mintYCenter + (mintRoadWidth / 2), Integer))
         mobjBitmapGraphics.FillRectangle(GetColorFromEnum(mobjIntersectionLights.Light(3)), CType(mintXCenter - (mintRoadWidth / 4) - 4, Integer), CType(mintYCenter + (mintRoadWidth / 2) - 3, Integer), 9, 3)

         ' traffic 4 light
         mobjBitmapGraphics.DrawLine(Pens.Black, CType(mintXCenter + (mintRoadWidth / 4) - 4, Integer), CType(mintYCenter - (mintRoadWidth / 2), Integer), CType(mintXCenter + (mintRoadWidth / 4) + 4, Integer), CType(mintYCenter - (mintRoadWidth / 2), Integer))
         mobjBitmapGraphics.FillRectangle(GetColorFromEnum(mobjIntersectionLights.Light(4)), CType(mintXCenter + (mintRoadWidth / 4) - 4, Integer), CType(mintYCenter - (mintRoadWidth / 2) + 1, Integer), 9, 3)

         ' bring more vehicles
         If mobjRandom.NextDouble < 0.02 Then
            objVehicle = New Vehicle
            Select Case mobjRandom.NextDouble
               Case 0 To 0.25
                  objVehicle.mintRoad = 1
               Case 0.25 To 0.5
                  objVehicle.mintRoad = 2
               Case 0.5 To 0.75
                  objVehicle.mintRoad = 3
               Case Else
                  objVehicle.mintRoad = 4
            End Select
            objVehicleThread = New Thread(AddressOf objVehicle.Start)
            objVehicleThread.Start()
            mobjVehicles.Add(objVehicle)
         End If

         Thread.Sleep(30)

         intVehicleCount = 0
         Do While intVehicleCount <= mobjVehicles.Count - 1
            ' draw vehicle
            objVehicle = mobjVehicles.Item(intVehicleCount)
            mobjBitmapGraphics.DrawIcon(objIcon(objVehicle.mintRoad), objVehicle.mintXposition - 16, objVehicle.mintYposition - 16)
            If objVehicle.mintXposition < -500 Or objVehicle.mintXposition > mintFormWidth + 500 Or objVehicle.mintYposition < -500 Or objVehicle.mintYposition > mintFormHeight + 500 Then
               mobjVehicles.Remove(objVehicle)
            Else
               intVehicleCount += 1
            End If
         Loop

         Me.Invalidate()
         Application.DoEvents()
      Loop

   End Sub

End Class




