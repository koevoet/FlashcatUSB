Public Class MemControl

#Region "Dynamic Range Box"

    Private CurrentBase As UInt32
    Private CurrentSize As UInt32
    Private CurrentMax As UInt32

    Private BaseTxt As New Windows.Forms.TextBox
    Private LenTxt As New Windows.Forms.TextBox

    Private Function ShowRangeBox(ByRef BaseAddress As UInt32, ByRef Size As UInt32, ByVal MaxData As UInt32) As Boolean
        If Size > MaxData Then Size = MaxData
        Dim InputSelectionForm As New Form With {.Width = 172, .Height = 80}
        InputSelectionForm.FormBorderStyle = Windows.Forms.FormBorderStyle.FixedToolWindow
        InputSelectionForm.ShowInTaskbar = False
        InputSelectionForm.ShowIcon = False
        InputSelectionForm.ControlBox = False
        Dim BtnOK As New Windows.Forms.Button With {.Text = RM.GetString("fcusb_okbutton"), .Width = 60, .Height = 20, .Left = 90, .Top = 50}
        Dim BtnCAN As New Windows.Forms.Button With {.Text = RM.GetString("fcusb_cancel"), .Width = 60, .Height = 20, .Left = 20, .Top = 50}
        Dim Lbl1 As New Windows.Forms.Label With {.Text = RM.GetString("fcusb_baseaddress"), .Left = 10, .Top = 5}
        Dim Lbl2 As New Windows.Forms.Label With {.Text = RM.GetString("fcusb_length"), .Left = 105, .Top = 5}
        BaseTxt = New Windows.Forms.TextBox With {.Text = "0x" & Hex(BaseAddress), .Width = 70, .Top = 20, .Left = 10}
        LenTxt = New Windows.Forms.TextBox With {.Text = Size.ToString, .Width = 70, .Top = 20, .Left = 90}
        InputSelectionForm.Controls.Add(BtnOK)
        InputSelectionForm.Controls.Add(BtnCAN)
        InputSelectionForm.Controls.Add(BaseTxt)
        InputSelectionForm.Controls.Add(LenTxt)
        InputSelectionForm.Controls.Add(Lbl2)
        InputSelectionForm.Controls.Add(Lbl1)

        AddHandler BtnCAN.Click, AddressOf Dyn_CancelClick
        AddHandler BtnOK.Click, AddressOf Dyn_OkClick
        AddHandler InputSelectionForm.MouseDown, AddressOf Dyn_MouseDown
        AddHandler InputSelectionForm.MouseUp, AddressOf Dyn_MouseUp
        AddHandler InputSelectionForm.MouseMove, AddressOf Dyn_MouseMove
        AddHandler Lbl2.MouseDown, AddressOf Dyn_MouseDown
        AddHandler Lbl2.MouseUp, AddressOf Dyn_MouseUp
        AddHandler Lbl2.MouseMove, AddressOf DynLabel_MouseMove
        AddHandler Lbl1.MouseDown, AddressOf Dyn_MouseDown
        AddHandler Lbl1.MouseUp, AddressOf Dyn_MouseUp
        AddHandler Lbl1.MouseMove, AddressOf DynLabel_MouseMove
        AddHandler InputSelectionForm.Load, AddressOf DynForm_Load
        AddHandler LenTxt.KeyDown, AddressOf DynForm_Keydown
        AddHandler LenTxt.LostFocus, AddressOf DynFormLength_LostFocus
        AddHandler BaseTxt.KeyDown, AddressOf DynForm_Keydown
        AddHandler BaseTxt.LostFocus, AddressOf DynFormBase_LostFocus

        BtnOK.Select()
        CurrentBase = BaseAddress
        CurrentSize = Size
        CurrentMax = MaxData
        If InputSelectionForm.ShowDialog() = Windows.Forms.DialogResult.OK Then
            BaseAddress = CurrentBase
            Size = CurrentSize
            Return True
        Else
            Return False
        End If
    End Function

    Private Sub DynFormLength_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim t As TextBox = DirectCast(sender, TextBox)
        Try
            If IsNumeric(t.Text) Then
                CurrentSize = CUInt(t.Text)
            ElseIf isHex(t.Text) AndAlso t.Text.Length < 9 Then
                CurrentSize = HexToUint(t.Text)
            End If
        Finally
            If CurrentSize > CurrentMax Then CurrentSize = CurrentMax
            If CurrentSize < 1 Then CurrentSize = 1
        End Try
        t.Text = CurrentSize
        If CurrentBase + CurrentSize > CurrentMax Then
            CurrentBase = CurrentMax - CurrentSize
            BaseTxt.Text = "0x" & Hex(CurrentBase)
        End If
    End Sub

    Private Sub DynFormBase_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim t As TextBox = DirectCast(sender, TextBox)
        Try
            If IsNumeric(t.Text) AndAlso (CLng(t.Text) < (CurrentMax + 1)) Then
                CurrentBase = CUInt(t.Text)
            ElseIf isHex(t.Text) AndAlso t.Text.Length < 9 Then
                CurrentBase = HexToUint(t.Text)
            End If
        Finally
            If CurrentBase > (CurrentMax + 1) Then CurrentBase = CurrentMax - 1
        End Try
        t.Text = "0x" & Hex(CurrentBase)
        If CurrentBase + CurrentSize > CurrentMax Then
            CurrentSize = CurrentMax - CurrentBase
            LenTxt.Text = CurrentSize.ToString
        End If
    End Sub

    Private Sub DynForm_Keydown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs)
        If e.KeyCode = 13 Then 'Enter pressed
            Dim Btn As Windows.Forms.TextBox = CType(sender, TextBox)
            Dim SendFrm As Form = Btn.FindForm
            SendFrm.DialogResult = Windows.Forms.DialogResult.OK
        End If
    End Sub
    'Always centers the dynamic input form on top of the original form
    Private Sub DynForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim frm As Form = CType(sender, Form)
        frm.Top = CInt(GuiForm.Top + ((GuiForm.Height / 2) - (frm.Height / 2)))
        frm.Left = CInt(GuiForm.Left + ((GuiForm.Width / 2) - (frm.Width / 2)))
    End Sub
    'Handles the dynamic form for a click
    Private Sub Dyn_OkClick(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim Btn As Windows.Forms.Button = CType(sender, Button)
        Dim SendFrm As Form = Btn.FindForm
        SendFrm.DialogResult = Windows.Forms.DialogResult.OK
    End Sub

    Private Sub Dyn_CancelClick(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim Btn As Windows.Forms.Button = CType(sender, Button)
        Dim SendFrm As Form = Btn.FindForm
        SendFrm.DialogResult = Windows.Forms.DialogResult.Cancel
    End Sub

    'Allows you to click and move the form around
    Private MouseDownOnForm As Boolean = False
    Private ClickPoint As Point

    Private Sub Dyn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        MouseDownOnForm = True
        ClickPoint = New Point(Windows.Forms.Cursor.Position.X, Windows.Forms.Cursor.Position.Y)
    End Sub

    Private Sub Dyn_MouseUp(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        MouseDownOnForm = False
    End Sub

    Private Sub Dyn_MouseMove(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        If MouseDownOnForm Then
            Dim newPoint As New Point(Windows.Forms.Cursor.Position.X, Windows.Forms.Cursor.Position.Y)
            Dim Form1 As Form = CType(sender, Form)
            Form1.Top = Form1.Top + (newPoint.Y - ClickPoint.Y)
            Form1.Left = Form1.Left + (newPoint.X - ClickPoint.X)
            ClickPoint = newPoint
        End If
    End Sub
    'Hanldes the move if a label is being dragged
    Private Sub DynLabel_MouseMove(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs)
        If MouseDownOnForm Then
            Dim newPoint As New Point(Windows.Forms.Cursor.Position.X, Windows.Forms.Cursor.Position.Y)
            Dim Btn As Windows.Forms.Label = CType(sender, Label)
            Dim Form1 As Form = Btn.FindForm
            Form1.Top = Form1.Top + (newPoint.Y - ClickPoint.Y)
            Form1.Left = Form1.Left + (newPoint.X - ClickPoint.X)
            ClickPoint = newPoint
        End If
    End Sub

#End Region

    'contains the data and info the memory loops require
    Private Structure data_params
        Public FileName As String 'Contains the shortname of the file being opened or written to
        Public DataBuffer() As Byte 'Holds the data to be written or data to be read
        Public Size As UInt32 'Number of bytes of our data to write or to read
        Public TargetOffset As UInt32 'The offset we are going to write the data to
    End Structure

    'We just wrap the same events from the hex editor to our calling object
    Public Event StreamUpdate(ByVal addr As UInt32, ByVal b As Byte) 'Updates the data source with a single byte (only in stream mode)
    Public Event ReadStream(ByVal addr As UInt32, ByRef data() As Byte) 'Requests data to be filled
    Public Event SetStatus(ByVal msg As String) 'Must be handled
    Public Event WriteData(ByVal addr As UInt32, ByVal data() As Byte) 'Must be handled
    Public Event ReadData(ByVal addr As UInt32, ByRef data() As Byte) 'Must be handled
    Public Event StopOperation() 'When user clicks the "STOP" button

    Private MemoryThread As Threading.Thread 'Run process on thread as not to lock up gui
    Private Abort_MemoryLoop As Boolean 'Set to true to stop calling object from doing its thing (reading/writing)

    Delegate Sub SetSpeedText(ByVal value As String)
    Delegate Sub VisableAddrBox(ByVal value As Boolean)
    Delegate Sub AddrUpdate(ByVal Address As String)
    Delegate Sub CbSetProgress(ByVal value As Integer)
    Delegate Sub CbSetupGui()

    Private CurrentGuiAddress As UInt32 = 0

    Private MemDeviceName As String = RM.GetString("fcusb_memdev") 'Changes via SetDeviceName

    Sub New()
        ' This call is required by the Windows Form Designer.
        InitializeComponent()
        txtAddress.Text = "0x0"
        cmdMemWrite.Text = RM.GetString("fcusb_Write")
        cmdMemRead.Text = RM.GetString("fcusb_Read")
        AddrBox.Text = RM.GetString("fcusb_mem_address")
        FlashTransfer.Text = RM.GetString("fcusb_xferspeed")
    End Sub

    Public Sub SetDeviceName(ByVal GroupBoxName As String, ByVal DeviceName As String)
        gbMemory.Text = GroupBoxName
        lblMemName.Text = DeviceName
        MemDeviceName = DeviceName
    End Sub

    Private Sub MemEditor_ReadStream(ByVal location As UInteger, ByRef Data() As Byte) Handles MemHexEditor.ReadStream
        RaiseEvent ReadStream(location, Data)
    End Sub

    Private Sub MemEditor_StreamUpdate(ByVal Location As UInt32, ByVal B As Byte) Handles MemHexEditor.StreamUpdate
        RaiseEvent StreamUpdate(Location, B)
    End Sub

    Private Sub MemEditor_AddressUpdate(ByVal Address As UInt32) Handles MemHexEditor.AddressUpdate
        If txtAddress.InvokeRequired Then
            Dim d As New AddrUpdate(AddressOf MemEditor_AddressUpdate)
            Me.Invoke(d, New Object() {Address})
        Else
            CurrentGuiAddress = Address
            txtAddress.Text = "0x" & UCase(Hex(Address))
            txtAddress.SelectionStart = txtAddress.Text.Length
        End If
    End Sub

    Private Sub txtAddress_Enter(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtAddress.Enter
        txtAddress.Clear()
    End Sub

    Private Sub txtAddress_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles txtAddress.KeyPress
        If Asc(e.KeyChar) = Keys.Enter Then
            MemHexEditor.Focus() 'Makes this control loose focus and trigger the other event (lostfocus)
        End If
    End Sub

    Private Sub txtAddress_LostFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles txtAddress.LostFocus
        Dim inputVal As String = Trim(txtAddress.Text.Replace(" ", ""))
        If IsNumeric(inputVal) Then
            CurrentGuiAddress = CUInt(inputVal)
            txtAddress.Text = "0x" & Hex(CurrentGuiAddress)
        ElseIf isHex(txtAddress.Text) Then
            CurrentGuiAddress = HexToUint(inputVal)
            txtAddress.Text = "0x" & Hex(CurrentGuiAddress)
        Else 'Erroneous input
            txtAddress.Text = "0x" & Hex(CurrentGuiAddress)
            Exit Sub
        End If
        MemHexEditor.GotoAddress(HexToUint(txtAddress.Text))
        'If MemHexEditor.isDrawn Then MemHexEditor.GotoHexAdr(txtAddress.Text)
    End Sub

    Public Sub SetAddressVisable(ByVal Value As Boolean)
        If Me.AddrBox.InvokeRequired Then
            Dim d As New VisableAddrBox(AddressOf SetAddressVisable)
            Me.Invoke(d, New Object() {Value})
        Else
            AddrBox.Visible = Value
            FlashTransfer.Visible = Not Value
            Application.DoEvents()
        End If
    End Sub

    Public Sub SetProgress(ByVal P As Integer)
        Try
            If Me.InvokeRequired Then
                Dim d As New CbSetProgress(AddressOf SetProgress)
                Me.Invoke(d, New Object() {P})
            Else
                pbar.Value = P
            End If
        Catch ex As Exception
        End Try
    End Sub

    Public Sub LoadData(ByVal EditorSize As UInt32, ByVal aType As HexEditor.AccessType, ByVal aMode As HexEditor.AccessMode, ByVal data() As Byte)
        MemHexEditor.CreateHexViewer(EditorSize, 0, aType, aMode)
        MemHexEditor.BeginHexViewer(data)
    End Sub

    Public Sub Loaded()
        MemHexEditor.IsLoaded = True
    End Sub

    Private Sub cmdMemRead_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdMemRead.Click
        If cmdMemRead.Text = RM.GetString("fcusb_stop").ToUpper.Trim Then
            Abort_MemoryLoop = True
            RaiseEvent StopOperation()
            cmdMemRead.Enabled = False
            Exit Sub
        End If
        Dim BaseAddress As UInt32 = 0 'The starting address to read the from data
        Dim NumberToWrite As UInt32 = MemHexEditor.DataSize 'The total number of bytes to write
        RaiseEvent SetStatus(String.Format(RM.GetString("fcusb_mem_readfrom"), MemDeviceName))
        If Not ShowRangeBox(BaseAddress, NumberToWrite, MemHexEditor.DataSize) Then
            RaiseEvent SetStatus(RM.GetString("fcusb_mem_readcanceled"))
            Exit Sub
        End If
        RaiseEvent SetStatus(RM.GetString("fcusb_mem_readstart"))
        If NumberToWrite = 0 Then Exit Sub
        Dim dp As New data_params
        ReDim dp.DataBuffer(NumberToWrite)
        dp.Size = NumberToWrite
        dp.TargetOffset = BaseAddress
        MemoryThread = New Threading.Thread(AddressOf ReadMemoryLoop)
        MemoryThread.SetApartmentState(Threading.ApartmentState.STA)
        MemoryThread.IsBackground = True
        MemoryThread.Start(dp)
    End Sub

    Private Sub cmdMemWrite_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdMemWrite.Click
        If cmdMemWrite.Text = RM.GetString("fcusb_stop").ToUpper.Trim Then
            Abort_MemoryLoop = True
            RaiseEvent StopOperation()
            cmdMemWrite.Enabled = False
            Exit Sub
        End If
        Dim fn As String = ""
        If Not OpenFileToWrite(fn) Then Exit Sub
        Dim FileIntelHexFormat As Boolean = False
        Dim FileNFO As New IO.FileInfo(fn)
        If Not FileNFO.Exists OrElse FileNFO.Length = 0 Then
            RaiseEvent SetStatus(RM.GetString("fcusb_mem_err1"))
            Exit Sub
        End If
        RaiseEvent SetStatus(RM.GetString("fcusb_mem_writestart"))
        Dim FileDATA() As Byte = ReadBytes(FileNFO.FullName)
        If FileNFO.Extension.ToUpper.EndsWith(".HEX") Then
            If IsIntelHex(FileDATA) Then FileIntelHexFormat = True
        End If
        If FileIntelHexFormat Then
            FileDATA = IntelHexToBin(FileDATA)   'Convert HEX to bin file
            WriteConsole(String.Format(RM.GetString("fcusb_mem_opened_intel"), FileNFO.Name, Format(FileDATA.Length, "#,###")))
        Else
            WriteConsole(String.Format(RM.GetString("fcusb_mem_opened_bin"), FileNFO.Name, Format(FileDATA.Length, "#,###")))
        End If
        Dim BaseAddress As UInt32 = 0 'The starting address to write the data
        Dim NumberToWrite As UInt32 = FileDATA.Length 'The total number of bytes to write
        RaiseEvent SetStatus(String.Format(RM.GetString("fcusb_mem_selectwriterange"), MemDeviceName))
        If Not ShowRangeBox(BaseAddress, NumberToWrite, MemHexEditor.DataSize) Then
            RaiseEvent SetStatus(RM.GetString("fcusb_mem_writecanceled"))
            Exit Sub
        End If
        If NumberToWrite = 0 Then Exit Sub
        RaiseEvent SetStatus(String.Format(RM.GetString("fcusb_mem_writing"), FileNFO.Name, MemDeviceName, Format(NumberToWrite, "#,###")))
        Dim dp As New data_params
        dp.FileName = FileNFO.Name
        dp.Size = NumberToWrite
        dp.TargetOffset = BaseAddress
        dp.DataBuffer = FileDATA
        MemoryThread = New Threading.Thread(AddressOf WriteMemoryLoop)
        MemoryThread.SetApartmentState(Threading.ApartmentState.STA)
        MemoryThread.IsBackground = True
        MemoryThread.Start(dp)
    End Sub

    Private Sub WriteMemoryLoop(ByVal dparams As data_params)
        Abort_MemoryLoop = False
        bcLedBlink()
        ReDim Preserve dparams.DataBuffer(dparams.Size - 1)
        RaiseEvent WriteData(dparams.TargetOffset, dparams.DataBuffer)
        If Abort_MemoryLoop Then
            RaiseEvent SetStatus(RM.GetString("fcusb_mem_cancelled"))
        Else
            RaiseEvent SetStatus(String.Format(RM.GetString("fcusb_mem_writecomplete"), Format(dparams.Size, "#,###")))
        End If
        bcLedOn()
        UpdateEditor()
    End Sub
    'Refreshes the hex editor (gets new data if in stream mode)
    Public Sub UpdateEditor()
        MemHexEditor.UpdateScreen()
    End Sub

    Private Sub ReadMemoryLoop(ByVal dparams As data_params)
        Abort_MemoryLoop = False
        bcLedBlink()
        RaiseEvent ReadData(dparams.TargetOffset, dparams.DataBuffer)
        If Abort_MemoryLoop Then
            RaiseEvent SetStatus(RM.GetString("fcusb_mem_usercanceledread"))
        ElseIf dparams.DataBuffer Is Nothing Then
            RaiseEvent SetStatus(RM.GetString("fcusb_mem_err2"))
        Else
            RaiseEvent SetStatus(RM.GetString("fcusb_mem_readcomplete"))
            Dim TargetFilename As String = MemDeviceName.Replace(" ", "_") & "_" & IntToHex(dparams.TargetOffset) & "-" & IntToHex(dparams.TargetOffset + dparams.DataBuffer.Length - 2)
            SaveFileFromRead(dparams.DataBuffer, TargetFilename)
        End If
        bcLedOn()
    End Sub

    Public Sub SetSpeedInfoMemory(ByVal Msg As String)
        If Me.lblSpeed.InvokeRequired Then
            Dim d As New SetSpeedText(AddressOf SetSpeedInfoMemory)
            Me.Invoke(d, New Object() {[Msg]})
        Else
            lblSpeed.Text = Msg
        End If
    End Sub

    Private Function OpenFileToWrite(ByRef Filename As String) As Boolean
        Dim OpenMe As New OpenFileDialog
        OpenMe.AddExtension = True
        OpenMe.InitialDirectory = Application.StartupPath
        OpenMe.Title = String.Format(RM.GetString("fcusb_mem_choosefile"), MemDeviceName)
        OpenMe.CheckPathExists = True
        OpenMe.Filter = BinHexFiles 'Bin Files, Hex Files, All Files
        If OpenMe.ShowDialog = Windows.Forms.DialogResult.OK Then
            Filename = OpenMe.FileName
            RaiseEvent SetStatus(String.Format(RM.GetString("fcusb_mem_filechosen"), MemDeviceName))
            Return True
        Else
            RaiseEvent SetStatus(String.Format(RM.GetString("fcusb_mem_usercanwrite"), MemDeviceName))
            Return False
        End If
    End Function

    Public Sub SetupForReadOperation()
        If cmdMemRead.InvokeRequired Then
            Dim d As New CbSetupGui(AddressOf SetupForReadOperation)
            Me.Invoke(d)
        Else
            SetProgress(0)
            SetAddressVisable(False)
            cmdMemRead.Text = RM.GetString("fcusb_stop")
            cmdMemWrite.Enabled = False
        End If
    End Sub

    Public Sub SetupForWriteOperation()
        If cmdMemRead.InvokeRequired Then
            Dim d As New CbSetupGui(AddressOf SetupForWriteOperation)
            Me.Invoke(d)
        Else
            SetProgress(0)
            SetAddressVisable(False)
            cmdMemWrite.Text = RM.GetString("fcusb_stop")
            cmdMemRead.Enabled = False
        End If
    End Sub

    Public Sub OperationComplete()
        If cmdMemRead.InvokeRequired Then
            Dim d As New CbSetupGui(AddressOf OperationComplete)
            Me.Invoke(d)
        Else
            cmdMemWrite.Text = RM.GetString("fcusb_Write")
            cmdMemRead.Text = RM.GetString("fcusb_Read")
            cmdMemWrite.Enabled = True
            cmdMemRead.Enabled = True
            SetAddressVisable(True)
            SetProgress(0) 'Set the forms status bar to 0
            SetSpeedInfoMemory("")
        End If
    End Sub

End Class
