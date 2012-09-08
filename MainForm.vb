'COPYRIGHT EMBEDDEDCOMPUTERS.NET 2012 - ALL RIGHTS RESERVED
'CONTACT EMAIL: contact@embeddedcomputers.net
'ANY USE OF THIS CODE MUST ADHERE TO THE LICENSE FILE INCLUDED WITH THIS SDK

Public Class MainForm

#Region "Script Tab Control"
    'Removes all of the tabs except for Status and Console
    Public Sub RemoveAllTabs()
        Dim i As Integer
        Dim list As New ArrayList
        Dim tP As TabPage
        For Each tP In MyTabs.Controls
            If tP Is TabStatus Then
            ElseIf tP Is TabConsole Then
            Else
                list.Add(tP)
            End If
        Next
        For i = 0 To list.Count - 1
            MyTabs.Controls.Remove(CType(list(i), Control))
        Next
    End Sub

    Public Function GetTabObjectText(ByVal ControlName As String, ByVal TabIndex As Integer) As String
        Dim MyObj As String = "IND:" & CStr(TabIndex)
        Dim tP As TabPage
        For Each tP In MyTabs.Controls
            If tP.Name = MyObj Then
                Dim Ct As Control
                For Each Ct In tP.Controls
                    If UCase(Ct.Name) = UCase(ControlName) Then
                        Return Ct.Text
                    End If
                Next
                Return "" 'not found
            End If
        Next
        Return ""
    End Function

    Public Function GetUserTab(ByVal ind As Integer) As TabPage
        Dim MyObj As String = "IND:" & CStr(ind)
        Dim tP As TabPage
        For Each tP In MyTabs.Controls
            If tP.Name = MyObj Then Return tP
        Next
        Return Nothing
    End Function

    Public Sub SetControlText(ByVal usertabind As Integer, ByVal UserControl As String, ByVal NewText As String)
        If Me.MyTabs.InvokeRequired Then
            Dim d As New cbSetControlText(AddressOf SetControlText)
            Me.Invoke(d, New Object() {usertabind, UserControl, NewText})
        Else
            Dim usertab As TabPage = GetUserTab(usertabind)
            If usertab Is Nothing Then Exit Sub
            Dim C As Control
            For Each C In usertab.Controls
                If UCase(C.Name) = UCase(UserControl) Then
                    C.Text = NewText
                    If C.GetType Is GetType(Windows.Forms.TextBox) Then
                        Dim t As TextBox = CType(C, TextBox)
                        t.SelectionStart = 0
                    End If
                    Exit Sub
                End If
            Next
        End If
    End Sub

    Public Sub CreateFormTab(ByVal TabIndex As Integer, ByVal TabName As String)
        If Me.MyTabs.InvokeRequired Then
            Dim d As New cbCreateFormTab(AddressOf CreateFormTab)
            Me.Invoke(d, New Object() {TabIndex, [TabName]})
        Else
            Dim newTab As New TabPage(TabName)
            newTab.Name = "IND:" & CStr(TabIndex)
            Me.MyTabs.Controls.Add(newTab)
        End If
    End Sub

    Public Sub AddTab(ByVal tb As TabPage)
        If Me.MyTabs.InvokeRequired Then
            Dim d As New cbAddTab(AddressOf AddTab)
            Me.Invoke(d, New Object() {tb})
        Else
            MyTabs.Controls.Add(tb)
        End If
    End Sub

    Public Sub AddToTab(ByVal usertabind As Integer, ByVal obj As Object)
        If Me.MyTabs.InvokeRequired Then
            Dim d As New cbAddToTab(AddressOf AddToTab)
            Me.Invoke(d, New Object() {usertabind, [obj]})
        Else
            Dim usertab As TabPage = GetUserTab(usertabind)
            If usertab Is Nothing Then Exit Sub
            Dim c As Control = CType(obj, Control)
            usertab.Controls.Add(c)
            c.BringToFront()
        End If
    End Sub

    Public Sub HandleButtons(ByVal usertabind As Integer, ByVal Enabled As Boolean, ByVal BtnName As String)
        Dim usertab As TabPage = GetUserTab(usertabind)
        If usertab Is Nothing Then Exit Sub
        Dim C As Control
        For Each C In usertab.Controls
            If C.GetType Is GetType(Windows.Forms.Button) Then
                If UCase(C.Name) = UCase(BtnName) Or BtnName = "" Then
                    If Enabled Then
                        EnableButton(CType(C, Button))
                    Else
                        DisableButton(CType(C, Button))
                    End If
                End If
            End If
        Next
    End Sub

    Public Sub DisableButton(ByVal b As Button)
        If b.InvokeRequired Then
            Dim d As New SetBtnCallback(AddressOf DisableButton)
            Me.Invoke(d, New Object() {b})
        Else
            b.Enabled = False
        End If
    End Sub

    Public Sub EnableButton(ByVal b As Button)
        If b.InvokeRequired Then
            Dim d As New SetBtnCallback(AddressOf EnableButton)
            Me.Invoke(d, New Object() {b})
        Else
            b.Enabled = True
        End If
    End Sub

#End Region

#Region "Scripts"

    Private Sub LoadScripts(ByVal CPU As UInteger)
        CurrentScript_MI.DropDownItems.Clear()
        WriteConsole(RM.GetString("fcusb_script_check"))
        Dim MyScripts(,) As String = GetCompatibleScripts(CPU)
        Dim DefaultScript As String = Reg_GetPref_DefaultScript(Hex(CPU))
        Dim SelectScript As Integer = 0
        Dim i As Integer
        If MyScripts Is Nothing Then
            WriteConsole(RM.GetString("fcusb_script_none"))
        ElseIf (MyScripts.Length / 2) = 1 Then
            WriteConsole(String.Format(RM.GetString("fcusb_script_loading"), MyScripts(0, 0)))
            ScriptEngine.LoadScriptFile(New IO.FileInfo(ScriptPath & MyScripts(0, 0)))
            UpdateStatusMessage(RM.GetString("cnts_script"), MyScripts(0, 1))
            CurrentScript_MI.Enabled = True
            LoadScript_MI.Enabled = True
            UnloadScript_MI.Enabled = True
            Dim tsi As ToolStripMenuItem = CurrentScript_MI.DropDownItems.Add(MyScripts(0, 0))
            tsi.Tag = MyScripts(0, 0)
            AddHandler tsi.Click, AddressOf LoadSelectedScript
            tsi.Checked = True
        Else 'Multiple scripts (choose preferrence)
            CurrentScript_MI.Enabled = True
            LoadScript_MI.Enabled = True
            UnloadScript_MI.Enabled = True
            If Not DefaultScript = "" Then 'No prefference
                For i = 0 To CInt((MyScripts.Length / 2) - 1)
                    If UCase(MyScripts(i, 0)) = UCase(DefaultScript) Then
                        SelectScript = i
                        Exit For
                    End If
                Next
            End If
            For i = 0 To CInt((MyScripts.Length / 2) - 1)
                Dim tsi As ToolStripMenuItem = CurrentScript_MI.DropDownItems.Add(MyScripts(i, 1))
                tsi.Tag = MyScripts(i, 0)
                AddHandler tsi.Click, AddressOf LoadSelectedScript
                If SelectScript = i Then
                    tsi.Checked = True
                End If
            Next
            UpdateStatusMessage(RM.GetString("cnts_script"), MyScripts(SelectScript, 0))
            Dim df As New IO.FileInfo(ScriptPath & MyScripts(SelectScript, 0))
            ScriptEngine.LoadScriptFile(df)
        End If
    End Sub
    'Opens a script from the file 
    Private Sub LoadScript_MI_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles LoadScript_MI.Click
        Dim OpenMe As New OpenFileDialog
        OpenMe.AddExtension = True
        OpenMe.InitialDirectory = Application.StartupPath & "\Scripts\"
        OpenMe.Title = RM.GetString("fcusb_script_open")
        OpenMe.CheckPathExists = True
        OpenMe.Filter = BlackcatScript
        If OpenMe.ShowDialog = Windows.Forms.DialogResult.OK Then
            LoadScriptFile(OpenMe.FileName)
        End If
    End Sub
    'Unloads a script from the engine
    Private Sub UnloadScript_MI_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles UnloadScript_MI.Click
        Dim CurrentScriptName As String = ScriptEngine.ScriptName
        If Not CurrentScriptName = "" Then
            ScriptEngine.UnloadDeviceScript()
            RemoveScriptChecks()
            RemoveStatusMessage(RM.GetString("cnts_script"))
            SetStatus(String.Format(RM.GetString("fcusb_script_unload"), CurrentScriptName))
        End If
    End Sub

    Private Sub LoadSelectedScript(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim tsi As ToolStripMenuItem = sender
        If Not tsi.Checked Then
            Dim ScriptName As String = tsi.Tag
            LoadScriptFile(Application.StartupPath & "\Scripts\" & ScriptName)
            RemoveScriptChecks()
            tsi.Checked = True
            Reg_SavePref_DefaultScript(Hex(EJ.TargetDevice.IDCODE), ScriptName)
        End If
    End Sub

    Private Sub RemoveScriptChecks()
        Dim tsi As ToolStripMenuItem
        For Each tsi In CurrentScript_MI.DropDownItems
            tsi.Checked = False
        Next
    End Sub

    Private Sub LoadScriptFile(ByVal scriptName As String)
        Dim f As New IO.FileInfo(scriptName)
        If f.Exists Then
            If OperationMode = AvrMode.SPI Then
                Dim WasUnloaded As Boolean = ScriptEngine.UnloadDeviceScript() 'Unload it here, so our load will not remove flash tab
                If WasUnloaded Then
                    SPI.ScanForDevice()
                    Do While SPI.Scanning
                        Application.DoEvents()
                        Sleep(200)
                    Loop
                End If
            End If
            UpdateStatusMessage(RM.GetString("cnts_script"), f.Name)
            ScriptEngine.LoadScriptFile(f)
            SetStatus(String.Format(RM.GetString("fcusb_script_loaded"), f.Name))
        Else
            SetStatus(RM.GetString("fcusb_script_notexist"))
        End If
    End Sub

    Private Sub miDetectDevice_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles miDetectDevice.Click
        Disconnect(True)
    End Sub

    Private Sub tsiBootloader_Click(sender As System.Object, e As System.EventArgs) Handles tsiBootloader.Click
        EJ.StartBootloader()
    End Sub

#End Region

#Region "SPI Form Settings"

    Private Sub cbUseEnWS_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbUseEnWS.CheckedChanged
        SPI.CustomDevice.Definition.SEND_EWSR = cbUseEnWS.Checked
    End Sub

    Private Sub txtEnWS_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtEnWS.LostFocus
        Try
            Dim inputval As String = txtEnWS.Text.Trim
            Dim lngValue As Long
            If IsNumeric(inputval) Then
                lngValue = CLng(inputval)
            ElseIf isHex(inputval) Then
                lngValue = HexToLng(inputval)
            Else 'Error
                SetStatus(RM.GetString("fcusb_err5"))
                GoTo ExitSub
            End If
            If lngValue > 255 Then
                SetStatus(RM.GetString("fcusb_err6"))
            Else
                SPI.CustomDevice.Definition.EWSR = CByte(lngValue)
            End If
        Catch ex As Exception
        End Try
ExitSub:
        txtEnWS.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.EWSR)
    End Sub

    Private Sub cbSpiProgMode_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbSpiProgMode.SelectedIndexChanged
        SPI.CustomDevice.Definition.PROGMODE = CType(cbSpiProgMode.SelectedIndex, SPI_API.ProgramMode)
    End Sub

    Private Sub txtEraseSize_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtEraseSize.LostFocus
        Try
            Dim inputval As String = txtEraseSize.Text.Trim
            Dim lngValue As Long
            If IsNumeric(inputval) Then
                lngValue = CLng(inputval)
            ElseIf isHex(inputval) Then
                lngValue = HexToLng(inputval)
            Else 'Error
                SetStatus(RM.GetString("fcusb_err5"))
                GoTo ExitSub
            End If
            If lngValue > 2147483647 Then
                SetStatus(RM.GetString("fcusb_err7"))
            Else
                SPI.CustomDevice.Definition.EraseSize = CInt(lngValue)
            End If
        Catch ex As Exception
        End Try
ExitSub:
        txtEraseSize.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.EraseSize)
    End Sub

    Private Sub txtChipSize_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtChipSize.LostFocus
        Try
            Dim inputval As String = txtChipSize.Text.Trim
            Dim lngValue As Long
            If IsNumeric(inputval) Then
                lngValue = CLng(inputval)
            ElseIf isHex(inputval) Then
                lngValue = HexToLng(inputval)
            Else 'Error
                SetStatus(RM.GetString("fcusb_err5"))
                GoTo ExitSub
            End If
            If lngValue > 2147483647 Then
                SetStatus(RM.GetString("fcusb_err7"))
            ElseIf lngValue < 131072 Then
                SetStatus(RM.GetString("fcusb_spi_err3"))
            ElseIf Not (lngValue Mod 131072 = 0) Then
                SetStatus(RM.GetString("fcusb_spi_err4"))
            Else
                SPI.CustomDevice.FlashSize = CInt(lngValue)
            End If
        Catch ex As Exception
        End Try
ExitSub:
        txtChipSize.Text = "0x" & IntToHex(SPI.CustomDevice.FlashSize)
    End Sub

    Private Sub txtWriteStatus_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtWriteStatus.LostFocus
        Try
            Dim inputval As String = txtWriteStatus.Text.Trim
            Dim lngValue As Long
            If IsNumeric(inputval) Then
                lngValue = CLng(inputval)
            ElseIf isHex(inputval) Then
                lngValue = HexToLng(inputval)
            Else 'Error
                SetStatus(RM.GetString("fcusb_err5"))
                GoTo ExitSub
            End If
            If lngValue > 255 Then
                SetStatus(RM.GetString("fcusb_err6"))
            Else
                SPI.CustomDevice.Definition.WRSR = CByte(lngValue)
            End If
        Catch ex As Exception
        End Try
ExitSub:
        txtWriteStatus.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.WRSR)
    End Sub

    Private Sub txtPageProgram_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtPageProgram.LostFocus
        Try
            Dim inputval As String = txtPageProgram.Text.Trim
            Dim lngValue As Long
            If IsNumeric(inputval) Then
                lngValue = CLng(inputval)
            ElseIf isHex(inputval) Then
                lngValue = HexToLng(inputval)
            Else 'Error
                SetStatus(RM.GetString("fcusb_err5"))
                GoTo ExitSub
            End If
            If lngValue > 255 Then
                SetStatus(RM.GetString("fcusb_err6"))
            Else
                SPI.CustomDevice.Definition.PROGRAM = CByte(lngValue)
            End If
        Catch ex As Exception
        End Try
ExitSub:
        txtPageProgram.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.PROGRAM)
    End Sub

    Private Sub txtWriteEnable_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtWriteEnable.LostFocus
        Try
            Dim inputval As String = txtWriteEnable.Text.Trim
            Dim lngValue As Long
            If IsNumeric(inputval) Then
                lngValue = CLng(inputval)
            ElseIf isHex(inputval) Then
                lngValue = HexToLng(inputval)
            Else 'Error
                SetStatus(RM.GetString("fcusb_err5"))
                GoTo ExitSub
            End If
            If lngValue > 255 Then
                SetStatus(RM.GetString("fcusb_err6"))
            Else
                SPI.CustomDevice.Definition.WREN = CByte(lngValue)
            End If
        Catch ex As Exception
        End Try
ExitSub:
        txtWriteEnable.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.WREN)
    End Sub

    Private Sub txtReadStatus_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtReadStatus.LostFocus
        Try
            Dim inputval As String = txtReadStatus.Text.Trim
            Dim lngValue As Long
            If IsNumeric(inputval) Then
                lngValue = CLng(inputval)
            ElseIf isHex(inputval) Then
                lngValue = HexToLng(inputval)
            Else 'Error
                SetStatus(RM.GetString("fcusb_err5"))
                GoTo ExitSub
            End If
            If lngValue > 255 Then
                SetStatus(RM.GetString("fcusb_err6"))
            Else
                SPI.CustomDevice.Definition.RDSR = CByte(lngValue)
            End If
        Catch ex As Exception
        End Try
ExitSub:
        txtReadStatus.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.RDSR)
    End Sub

    Private Sub txtRead_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtRead.LostFocus
        Try
            Dim inputval As String = txtRead.Text.Trim
            Dim lngValue As Long
            If IsNumeric(inputval) Then
                lngValue = CLng(inputval)
            ElseIf isHex(inputval) Then
                lngValue = HexToLng(inputval)
            Else 'Error
                SetStatus(RM.GetString("fcusb_err5"))
                GoTo ExitSub
            End If
            If lngValue > 255 Then
                SetStatus(RM.GetString("fcusb_err6"))
            Else
                SPI.CustomDevice.Definition.READ = CByte(lngValue)
            End If
        Catch ex As Exception
        End Try
ExitSub:
        txtRead.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.READ)
    End Sub

    Private Sub txtSectorErase_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtSectorErase.LostFocus
        Try
            Dim inputval As String = txtSectorErase.Text.Trim
            Dim lngValue As Long
            If IsNumeric(inputval) Then
                lngValue = CLng(inputval)
            ElseIf isHex(inputval) Then
                lngValue = HexToLng(inputval)
            Else 'Error
                SetStatus(RM.GetString("fcusb_err5"))
                GoTo ExitSub
            End If
            If lngValue > 255 Then
                SetStatus(RM.GetString("fcusb_err6"))
            Else
                SPI.CustomDevice.Definition.SECTORERASE = CByte(lngValue)
            End If
        Catch ex As Exception
        End Try
ExitSub:
        txtSectorErase.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.SECTORERASE)
    End Sub

    Private Sub txtChipErase_LostFocus(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtChipErase.LostFocus
        Try
            Dim inputval As String = txtChipErase.Text.Trim
            Dim lngValue As Long
            If IsNumeric(inputval) Then
                lngValue = CLng(inputval)
            ElseIf isHex(inputval) Then
                lngValue = HexToLng(inputval)
            Else 'Error
                SetStatus(RM.GetString("fcusb_err5"))
                GoTo ExitSub
            End If
            If lngValue > 255 Then
                SetStatus(RM.GetString("fcusb_err6"))
            Else
                SPI.CustomDevice.Definition.CHIPERASE = CByte(lngValue)
            End If
        Catch ex As Exception
        End Try
ExitSub:
        txtChipErase.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.CHIPERASE)
    End Sub

    Private Sub RadioUseSpiSettings_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioUseSpiSettings.CheckedChanged
        If RadioUseSpiSettings.Checked Then
            cbSPI.Enabled = True
            SPI.UseCustom = True
        End If
    End Sub

    Private Sub RadioUseSpiAuto_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RadioUseSpiAuto.CheckedChanged
        If RadioUseSpiAuto.Checked Then
            cbSPI.Enabled = False
            SPI.UseCustom = False
            SPI.SpecifyClock(-1)
            SPI.SpecifyOrder("")
            SPI.SpecifyMode(-1)
        End If
    End Sub

    Private Sub LoadSpiTabSettings()
        cbSPI.Enabled = False
        txtChipSize.Text = "0x" & IntToHex(SPI.CustomDevice.FlashSize)
        txtEraseSize.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.EraseSize)
        txtWriteStatus.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.WRSR)
        txtPageProgram.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.PROGRAM)
        txtWriteEnable.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.WREN)
        txtReadStatus.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.RDSR)
        txtRead.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.READ)
        txtSectorErase.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.SECTORERASE)
        txtChipErase.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.CHIPERASE)
        txtEnWS.Text = "0x" & IntToHex(SPI.CustomDevice.Definition.EWSR)
        cbUseEnWS.Checked = SPI.CustomDevice.Definition.SEND_EWSR
        cbBitOrder.Items.Add("MSB")
        cbBitOrder.Items.Add("LSB")
        cbSpiClock.Items.Add("2 (8 MHz)")
        cbSpiClock.Items.Add("4 (4 MHz)")
        cbSpiClock.Items.Add("8 (2 MHz)")
        cbSpiClock.Items.Add("16 (1 MHz)")
        cbSpiClock.Items.Add("32 (500 KHz)")
        cbSpiClock.Items.Add("64 (250 KHz)")
        cbSpiClock.Items.Add("128 (125 KHz)")
        cbSpiMode.Items.Add("Mode 0")
        cbSpiMode.Items.Add("Mode 1")
        cbSpiMode.Items.Add("Mode 2")
        cbSpiMode.Items.Add("Mode 3")
        cbSpiProgMode.Items.Add("Page")
        cbSpiProgMode.Items.Add("AAI (Byte)")
        cbSpiProgMode.Items.Add("AAI (Word)")
        cbSpiProgMode.Items.Add("Atmel (buffer)")
        cbSpiProgMode.SelectedIndex = 0
        cbSpiClock.SelectedIndex = 0
        cbSpiMode.SelectedIndex = 0
        cbBitOrder.SelectedIndex = 0
    End Sub

    Private Sub cbSpiMode_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbSpiMode.SelectedIndexChanged
        If cbSPI.Enabled Then
            SPI.SpecifyMode(cbSpiMode.SelectedIndex)
        End If
    End Sub

    Private Sub cbSpiClock_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbSpiClock.SelectedIndexChanged
        If cbSPI.Enabled Then
            Dim cspeed As String = CStr(cbSpiClock.SelectedItem)
            cspeed = Mid(cspeed, 1, InStr(cspeed, " ") - 1)
            SPI.SpecifyClock(CInt(cspeed))
        End If
    End Sub

    Private Sub cbBitOrder_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbBitOrder.SelectedIndexChanged
        If cbSPI.Enabled Then
            SPI.SpecifyOrder("""" & cbBitOrder.Text & """")
        End If
    End Sub

    Public Sub ShowSpiSettings(ByVal ShowTab As Boolean)
        If Me.InvokeRequired Then
            Dim d As New cbShowSpiSettings(AddressOf ShowSpiSettings)
            Me.Invoke(d, New Object() {ShowTab})
        Else
            Try
                If ShowTab Then
                    If Not MyTabs.TabPages.Contains(SpiTab) Then
                        MyTabs.TabPages.Add(SpiTab)
                    End If
                Else
                    If MyTabs.TabPages.Contains(SpiTab) Then
                        MyTabs.TabPages.Remove(SpiTab)
                    End If
                End If
            Catch ex As Exception
            End Try
        End If
    End Sub

#End Region

#Region "AVR Form Tab"
    Private FwHexName As String
    Private FwHexFile() As String = Nothing
    Private FwHexBin() As Byte = Nothing
    Private HexFileSize As Integer = 0
    Private CommandThread As Threading.Thread
    Private ScriptCommand As String
    'Called once at startup to setup the tab
    Private Sub AvrTabInit()
        DelHexFileInfo()
        cmdAvrProg.Enabled = False
        cmdAvrStart.Enabled = False
    End Sub
    'Called when the app connects to the DFU bootloader
    Private Sub AvrDFUconnect()
        cmdAvrLoad.Enabled = True
        If Not FwHexBin Is Nothing Then
            cmdAvrProg.Enabled = True
            cmdAvrStart.Enabled = True
            cmdAvrProg.BringToFront()
            cmdAvrProg.Select()
        End If
    End Sub
    'Deletes all of the hex file in memory and clears gui labels
    Private Sub DelHexFileInfo()
        lblAvrFn.Text = RM.GetString("fcusb_hex_nofile")
        lblAvrRange.Text = RM.GetString("fcusb_range") & " 0x0000 - 0x0000"
        lblAvrCrc.Text = "CRC: 0x000000"
        FwHexBin = Nothing
        HexFileSize = 0
        FwHexName = ""
        'AvrEditor.UnloadData()
    End Sub

    Private Sub txtInput_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles txtInput.KeyPress
        If Asc(e.KeyChar) = 13 Then 'Enter key was pressed
            ScriptCommand = txtInput.Text
            txtInput.Text = ""
            Application.DoEvents()
            CommandThread = New Threading.Thread(AddressOf CmdThreadExec)
            CommandThread.IsBackground = True
            CommandThread.Name = "ScriptExecThread"
            CommandThread.SetApartmentState(Threading.ApartmentState.STA)
            CommandThread.Start()
        End If
    End Sub
    'This is so that the console command does not tie up the Form or input boxes etc
    Private Sub CmdThreadExec()
        ScriptEngine.ExecuteCommand(ScriptCommand)
    End Sub

    Private Sub cmdAvrLoad_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdAvrLoad.Click
        Dim retFile As String = ""
        If OpenHexFile(retFile) Then
            Dim finfo As New IO.FileInfo(retFile)
            Dim FileData() As Byte = ReadBytes(finfo.FullName)
            If IsIntelHex(FileData) Then
                FwHexBin = IntelHexToBin(FileData)
                FwHexName = finfo.Name
                HexFileSize = finfo.Length
                LoadHexFileInfo()
            Else
                SetStatus(RM.GetString("fcusb_err8")) 'Error: file is corrupt or not a AVR Hex file
            End If
        End If
    End Sub

    Private Sub cmdAvrProg_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdAvrProg.Click
        Dim Res As Boolean = False
        cmdAvrProg.Enabled = False 'Prevents user from double clicking program button
        cmdAvrStart.Enabled = False
        Dim DfuSize As Integer = DFU.GetFlashSize
        If FwHexBin.Length > DfuSize Then
            SetStatus(RM.GetString("fcusb_avr_err1")) 'Error: The hex file data is larger than the size of the DFU memory
            GoTo ExitAvrProg
        End If
        UpdateDfuStatusBar(0)
        SetStatus(RM.GetString("fcusb_avr_err2"))
        WriteConsole(RM.GetString("fcusb_avr_err3"))
        Res = DFU.EraseFlash()
        If Not Res Then
            WriteConsole(RM.GetString("fcusb_avr_err4"))
            SetStatus(RM.GetString("fcusb_avr_err4"))
            GoTo ExitAvrProg
        Else
            WriteConsole(RM.GetString("fcusb_avr_sucess"))
        End If
        Application.DoEvents()
        Threading.Thread.Sleep(250)
        SetStatus(RM.GetString("fcusb_newavrfw"))
        WriteConsole(String.Format(RM.GetString("fcusb_avrwrite"), FwHexBin.Length)) 'Beginning AVR flash write ({0} bytes)
        Application.DoEvents()
        Threading.Thread.Sleep(250)
        Res = DFU.WriteFlash(FwHexBin)
        If Not Res Then
            WriteConsole(RM.GetString("fcusb_avr_err5"))
            SetStatus(RM.GetString("fcusb_avr_err5"))
            GoTo ExitAvrProg
        End If
        WriteConsole(RM.GetString("fcusb_avr_writedone"))
        SetStatus(RM.GetString("fcusb_avr_writedonegui"))
        Application.DoEvents()
        Threading.Thread.Sleep(250)
ExitAvrProg:
        cmdAvrStart.Enabled = True
        cmdAvrProg.Enabled = True
        UpdateDfuStatusBar(0)
    End Sub

    Private Sub cmdAvrStart_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdAvrStart.Click
        cmdAvrLoad.Enabled = False
        cmdAvrProg.Enabled = False
        cmdAvrStart.Enabled = False
        DFU.RunApp() 'Start application (hardware reset)
        Sleep(100)
        MainApp.Disconnect()
    End Sub
    'Loads the gui information and loads up the hex editor
    Public Sub LoadHexFileInfo()
        cmdAvrProg.Enabled = True
        cmdAvrStart.Enabled = True
        lblAvrFn.Text = String.Format(RM.GetString("fcusb_file"), FwHexName)
        lblAvrRange.Text = RM.GetString("fcusb_range") & " 0x0000 - 0x" & Hex(FwHexBin.Length - 1).PadLeft(4, CChar("0"))
        Dim crc As Int32
        Dim i As Integer
        For i = 0 To FwHexBin.Length - 1
            crc += FwHexBin(0)
        Next
        crc = crc Xor &HFFFFFF
        crc = crc + 1
        lblAvrCrc.Text = "CRC: 0x" & Hex(crc And &HFFFFFF)
        AvrEditor.CreateHexViewer(FwHexBin.Length, 0, HexEditor.AccessType._ReadOnly, HexEditor.AccessMode._Cached)
        AvrEditor.BeginHexViewer(FwHexBin)
    End Sub

    Public Sub UpdateDfuStatusBar(ByVal Perc As Integer)
        If Me.InvokeRequired Then
            Dim d As New cbUpdateDfuStatusBar(AddressOf UpdateDfuStatusBar)
            Me.Invoke(d, New Object() {Perc})
        Else
            DfuPbBar.Value = Perc
        End If
    End Sub

#End Region

#Region "Delegates"
    Delegate Sub cbSetConnectionStatus(ByVal Connected As Boolean)
    Delegate Sub cbClearStatusMessage()
    Delegate Sub cbUpdateStatusMessage(ByVal Label As String, ByVal Msg As String)
    Delegate Sub cbRemoveStatusMessage(ByVal Label As String)
    Delegate Sub cbShowSpiSettings(ByVal Show As Boolean)
    Delegate Sub cbUpdateDfuStatusBar(ByVal Value As Integer)
    Delegate Sub cbSetControlText(ByVal usertabind As Integer, ByVal Value As String, ByVal NewText As String)
    Delegate Sub SetBtnCallback(ByVal Value As Windows.Forms.Button)
    Delegate Sub cbAddToTab(ByVal usertab As Integer, ByVal Value As Object)
    Delegate Sub cbAddTab(ByVal tb As TabPage)
    Delegate Sub cbCreateFormTab(ByVal Index As Integer, ByVal Name As String)
    Delegate Sub cbPrintConsole(ByVal msg As String)
    Delegate Sub cbSetStatus(ByVal msg As String)
    Delegate Sub cbOnDeviceDisconnected()
    Delegate Sub cbOnDeviceConnected()
#End Region

    Public FormIsLoaded As Boolean = False

    Private Sub MainForm_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        SetupText()
        Me.MinimumSize = Me.Size
        ReDim StatusMessageControls(6)
        StatusMessageControls(0) = sm1
        StatusMessageControls(1) = sm2
        StatusMessageControls(2) = sm3
        StatusMessageControls(3) = sm4
        StatusMessageControls(4) = sm5
        StatusMessageControls(5) = sm6
        StatusMessageControls(6) = sm7
        ClearStatusMessage()
        Me.Text = "FlashcatUSB (Build " & Build & ")"
        Dim libdll As New IO.FileInfo(Application.StartupPath & "\LibUsbDotNet.dll")
        If Not libdll.Exists Then
            MsgBox("Unable to load LibUsbDotNet.dll", MsgBoxStyle.Critical, "Error starting application")
            Me.Close() : Exit Sub
        End If
        Dim libVer As String = System.Reflection.AssemblyName.GetAssemblyName("LibUsbDotNet.dll").Version.ToString
        WriteConsole(String.Format(RM.GetString("fcusb_libusb_ver"), libVer))
        LoadSpiTabSettings()
        MyTabs.Controls.Remove(AvrTab)
        AvrTabInit()
        ScriptEngine.PrintInformation() 'Script Engine
        miDetectDevice.Enabled = False
        VerifyMenuItem.Checked = VerifyData
        EnableJTAGVccPinToolStripMenuItem.Checked = EnableJtagVcc
        SPINRF24LE1ModeToolStripMenuItem.Checked = SpiNordicMode
        CurrentScript_MI.Enabled = False
        LoadScript_MI.Enabled = False
        UnloadScript_MI.Enabled = False
        PrintConsole(String.Format(RM.GetString("fcusb_welcome"), Build))
        PrintConsole(String.Format(RM.GetString("fcusb_running"), Platform))
        FormIsLoaded = True
    End Sub

    Private Sub MainForm_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        AppIsClosing = True
        bcLedOff() 'Send LED off anyways
        SetRegistryValue("Language", LanguageName)
        SaveRegistry()
    End Sub

    Private StatusMessageControls() As Control 'Holds the label that the form displays

    Private Sub SetupText()
        SetStatus(RM.GetString("fcusb_welcome_status")) 'Welcome to FlashcatUSB!
        SetConnectionStatus(False)
        MainToolStripMenuItem.Text = RM.GetString("fcusb_menu_main")
        miDetectDevice.Text = RM.GetString("fcusb_menu_detectdevice")
        ExitToolStripMenuItem.Text = RM.GetString("fcusb_menu_exit")
        SettingsToolStripMenuItem.Text = RM.GetString("fcusb_menu_settings")
        ScriptToolStripMenuItem.Text = RM.GetString("fcusb_menu_script")
        CurrentScript_MI.Text = RM.GetString("fcusb_menu_cscript")
        LoadScript_MI.Text = RM.GetString("fcusb_menu_loadscript")
        UnloadScript_MI.Text = RM.GetString("fcusb_menu_unloadscript")
        LanguageToolStripMenuItem.Text = RM.GetString("fcusb_menu_language")
        TabStatus.Text = RM.GetString("fcusb_menu_status")
        TabConsole.Text = RM.GetString("fcusb_menu_console")
        SpiTab.Text = RM.GetString("fcusb_menu_spisettings")
        VerifyMenuItem.Text = RM.GetString("fcusb_menu_verify")
        'SPI Tab
        RadioUseSpiAuto.Text = RM.GetString("fcusb_menu_spi_auto")
        RadioUseSpiSettings.Text = RM.GetString("fcusb_menu_spi_usesettings")
        cbSPI.Text = RM.GetString("fcusb_menu_spi_devcmds")
        lblSpiChipSize.Text = RM.GetString("fcusb_menu_spi_chipsize")
        lblSpiPageProgram.Text = RM.GetString("fcusb_menu_spi_pp")
        lblSpiRead.Text = RM.GetString("fcusb_menu_spi_read")
        lblSpiMode.Text = RM.GetString("fcusb_menu_spi_spimode")
        lblSpiEraseSize.Text = RM.GetString("fcusb_menu_spi_erasesize")
        lblSpiWriteEn.Text = RM.GetString("fcusb_menu_spi_we")
        lblSpiSectorErase.Text = RM.GetString("fcusb_menu_spi_serase")
        lblSpiClockDiv.Text = RM.GetString("fcusb_menu_spi_clodiv")
        lblSpiWriteStatus.Text = RM.GetString("fcusb_menu_spi_wstatus")
        lblSpiReadStatus.Text = RM.GetString("fcusb_menu_spi_rstatus")
        lblSpiChipErase.Text = RM.GetString("fcusb_menu_spi_chiperase")
        lblSpiBitOrder.Text = RM.GetString("fcusb_menu_spi_bitorder")
        lblSpiEnWrStatus.Text = RM.GetString("fcusb_menu_spi_enwrstatus")
        cbUseEnWS.Text = RM.GetString("fcusb_menu_spi_usewren")
        lblSpiProgMode.Text = RM.GetString("fcusb_menu_spi_pmode")
        lblSpiInfo.Text = RM.GetString("fcusb_menu_spi_info")
        'DFU Tab
        AvrTab.Text = RM.GetString("fcusb_menu_avrfw")
        lblAvrFn.Text = RM.GetString("fcusb_dfu_file_default")
        cmdAvrLoad.Text = RM.GetString("fcusb_dfu_loadfile")
        cmdAvrProg.Text = RM.GetString("fcusb_dfu_progfile")
        cmdAvrStart.Text = RM.GetString("fcusb_dfu_startprog")
    End Sub

    Public Sub OnDeviceConnected()
        If Me.InvokeRequired Then
            Dim d As New cbOnDeviceConnected(AddressOf OnDeviceConnected)
            Me.Invoke(d)
        Else
            miDetectDevice.Enabled = True
            SetConnectionStatus(True)
            ClearStatusMessage()
            bcLedOn()
            Try
                Select Case OperationMode
                    Case AvrMode.JTAG
                        WriteConsole(RM.GetString("fcusb_initjtag"))
                        Dim FirmVer As String = EJ.GetFirmwareVersion
                        If FirmVer Is Nothing OrElse FirmVer = "" Then
                            SetStatus(RM.GetString("fcusb_err1"))
                            WriteConsole(RM.GetString("fcusb_err1"))
                            Exit Sub
                        End If
                        Dim FirmSng As Single = StrToSingle(FirmVer)
                        UpdateStatusMessage(RM.GetString("cnts_mode"), "Enhanced JTAG")
                        UpdateStatusMessage(RM.GetString("cnts_avrver"), FirmSng.ToString)
                        If FirmSng < StrToSingle(FirmVer) Then
                            SetStatus(String.Format(RM.GetString("fcusb_err2"), Format(MinBoardRev, "0.00")))
                            WriteConsole(RM.GetString("fcusb_err3"))
                            Exit Sub
                        End If
                        WriteConsole(String.Format(RM.GetString("fcusb_jtag_conn"), FirmSng))
                        If EJ.Init Then
                            WriteConsole(RM.GetString("fcusb_jtagsetup"))
                        Else
                            SetStatus(RM.GetString("fcusb_err4"))
                            UpdateStatusMessage(RM.GetString("cnts_device"), RM.GetString("fcusb_jtag_err1"))
                            WriteConsole(RM.GetString("fcusb_jtag_err1"))
                            Exit Sub
                        End If
                        If EJ.TargetDevice.IDCODE = 0 Then 'Not connected
                            UpdateStatusMessage(RM.GetString("cnts_device"), RM.GetString("fcusb_jtag_err2"))
                            Exit Sub
                        End If
                        UpdateStatusMessage(RM.GetString("cnts_device"), GetManu(EJ.TargetDevice.MANUID) & " " & Hex(EJ.TargetDevice.PARTNU))
                        WriteConsole("Detected CPU ID: 0x" & Hex(EJ.TargetDevice.IDCODE) & " IMP CODE: 0x" & Hex(EJ.TargetDevice.IMPCODE))
                        WriteConsole("Manufacturer ID: 0x" & Hex(EJ.TargetDevice.MANUID) & " Part ID: 0x" & Hex(EJ.TargetDevice.PARTNU))
                        WriteConsole("EJTAG Version support: " & EJ.TargetDevice.IMPVER)
                        If EJ.TargetDevice.NoDMA Then
                            WriteConsole(RM.GetString("fcusb_jtag_nodma"))
                        Else
                            WriteConsole(RM.GetString("fcusb_jtag_dma"))
                        End If
                        LoadScripts(EJ.TargetDevice.IDCODE)
                        LoadScript_MI.Enabled = True
                        SetStatus(RM.GetString("fcusb_jtag_ready"))
                    Case AvrMode.SPI
                        UpdateStatusMessage(RM.GetString("cnts_mode"), "Serial Programmable Interface (SPI)")
                        WriteConsole(String.Format(RM.GetString("fcusb_spi_ver"), SPI.GetAvrVersion))
                        UpdateStatusMessage(RM.GetString("cnts_avrver"), SPI.GetAvrVersion)
                        UpdateStatusMessage(RM.GetString("cnts_device"), RM.GetString("fcusb_spi_scanning"))
                        SPI.ScanForDevice()
                        Do While SPI.Scanning
                            If AppIsClosing Then Exit Select
                            Application.DoEvents()
                            Sleep(200)
                        Loop
                        Select Case SPI.FlashStatus
                            Case SPI_API.Status.NotDetected
                                UpdateStatusMessage(RM.GetString("cnts_device"), RM.GetString("fcusb_noflash"))
                                ShowSpiSettings(True)
                                SetStatus(RM.GetString("fcusb_spi_err1"))
                                Exit Sub
                            Case SPI_API.Status.NotSupported
                                UpdateStatusMessage(RM.GetString("cnts_device"), SPI.GetFlashName)
                                ShowSpiSettings(True)
                                SetStatus(RM.GetString("fcusb_spi_err2"))
                                Exit Sub
                            Case SPI_API.Status.Supported
                                If SpiNordicMode Then
                                    UpdateStatusMessage(RM.GetString("cnts_device"), "SPI compatible device (Nordic SPI port)")
                                Else
                                    UpdateStatusMessage(RM.GetString("cnts_device"), "SPI compatible device (JEDEC ID: " & SPI.GetJedecName & ")")
                                End If
                                UpdateStatusMessage(RM.GetString("cnts_flashsize"), Format(SPI.GetFlashSize, "#,###") & " bytes")
                                AddMemoryDevice(DeviceTypes.SPI, 0, SPI.GetFlashSize, Nothing, SPI.GetFlashName)
                                LoadScript_MI.Enabled = True
                                SetStatus(RM.GetString("fcusb_spi_ready"))
                        End Select
                    Case AvrMode.NAND
                        Dim FwVersion As String = NAND.GetFwVersion
                        WriteConsole(RM.GetString("fcusb_nand_conn"))
                        UpdateStatusMessage(RM.GetString("cnts_mode"), "NAND over SPI")
                        UpdateStatusMessage(RM.GetString("cnts_avrver"), FwVersion)
                        If NAND.FlashInit Then
                            UpdateStatusMessage(RM.GetString("cnts_device"), NAND.GetFlashName)
                        Else
                            UpdateStatusMessage(RM.GetString("cnts_device"), RM.GetString("fcusb_noflash"))
                            Exit Sub
                        End If
                        UpdateStatusMessage(RM.GetString("cnts_flashsize"), Format(NAND.GetFlashSize, "#,###") & " bytes")
                        LoadScript_MI.Enabled = True
                        AddMemoryDevice(DeviceTypes.NAND, 0, NAND.GetFlashSize, Nothing, NAND.GetFlashName)
                        SetStatus(RM.GetString("fcusb_nand_ready"))
                    Case AvrMode.DFU
                        Dim FlashSizeTot As Integer = DFU.GetFlashSize + DFU.GetBootloaderSize
                        WriteConsole(RM.GetString("fcusb_dfu_conn"))
                        UpdateStatusMessage(RM.GetString("cnts_mode"), "Device Firmware Upgrade (DFU)")
                        UpdateStatusMessage(RM.GetString("cnts_device"), DFU.GetAtmelPart)
                        UpdateStatusMessage(RM.GetString("cnts_flashtype"), "ATMEL AVR Flash")
                        UpdateStatusMessage(RM.GetString("cnts_flashsize"), Format(FlashSizeTot, "#,###") & " bytes")
                        If Not MyTabs.Controls.Contains(AvrTab) Then
                            MyTabs.Controls.Add(AvrTab) 'If not added, Add it
                        End If
                        AvrDFUconnect()
                        SetStatus(RM.GetString("fcusb_dfu_ready"))
                End Select
            Catch ex As Exception
                OnDeviceDisconnected()
            End Try
        End If
    End Sub

    Public Sub OnDeviceDisconnected()
        If Me.InvokeRequired Then
            Dim d As New cbOnDeviceDisconnected(AddressOf OnDeviceDisconnected)
            Me.Invoke(d)
        Else
            SetConnectionStatus(False)
            SetStatus(RM.GetString("fcusb_waitingforusb"))
            ScriptEngine.Abort()
            ClearStatusMessage()
            CurrentScript_MI.Enabled = False
            LoadScript_MI.Enabled = False
            UnloadScript_MI.Enabled = False
            miDetectDevice.Enabled = False
            ScriptEngine.UnloadDeviceScript() 'Unloads the device script and any objects/tabs
            RemoveAllTabs()
        End If
    End Sub

    Public Sub PrintConsole(ByVal Msg As String)
        If Me.InvokeRequired Then
            Dim d As New cbPrintConsole(AddressOf PrintConsole)
            Me.Invoke(d, New Object() {[Msg]})
        Else
            ConsoleBox.BeginUpdate()
            ConsoleBox.Items.Add(Msg)
            If ConsoleBox.Items.Count > 750 Then
                Dim i As Integer
                For i = 0 To 249
                    ConsoleBox.Items.RemoveAt(0)
                Next
            End If
            ConsoleBox.SelectedIndex = ConsoleBox.Items.Count - 1
            ConsoleBox.EndUpdate()
        End If
    End Sub

    Public Sub SetStatus(ByVal Msg As String)
        If Me.InvokeRequired Then
            Dim d As New cbSetStatus(AddressOf SetStatus)
            Me.Invoke(d, New Object() {[Msg]})
        Else
            Me.Status.Text = Msg
        End If
    End Sub

    Public Sub SetConnectionStatus(ByVal Connected As Boolean)
        If Me.InvokeRequired Then
            Dim d As New cbSetConnectionStatus(AddressOf SetConnectionStatus)
            Me.Invoke(d, New Object() {Connected})
        Else
            If Connected Then
                Me.lblStatus.Text = RM.GetString("fcusb_status_connected")
            Else
                Me.lblStatus.Text = RM.GetString("fcusb_status_disconnect")
            End If
        End If
    End Sub

    Public Sub UpdateStatusMessage(ByVal Label As String, ByVal Msg As String)
        If Me.InvokeRequired Then
            Dim d As New cbUpdateStatusMessage(AddressOf UpdateStatusMessage)
            Me.Invoke(d, New Object() {Label, Msg})
        Else
            For i = 0 To StatusMessageControls.Length - 1
                Dim o As Object = DirectCast(StatusMessageControls(i), Label).Tag
                If o IsNot Nothing AndAlso CStr(o).ToUpper = Label.ToUpper Then
                    DirectCast(StatusMessageControls(i), Label).Text = Label & ": " & Msg
                    Exit Sub
                End If
            Next
            For i = 0 To StatusMessageControls.Length - 1
                Dim o As Object = DirectCast(StatusMessageControls(i), Label).Tag
                If o Is Nothing OrElse CStr(o) = "" Then
                    DirectCast(StatusMessageControls(i), Label).Tag = Label
                    DirectCast(StatusMessageControls(i), Label).Text = Label & ": " & Msg
                    Exit Sub
                End If
            Next
        End If
    End Sub

    Public Sub RemoveStatusMessage(ByVal Label As String)
        If Me.InvokeRequired Then
            Dim d As New cbRemoveStatusMessage(AddressOf RemoveStatusMessage)
            Me.Invoke(d, New Object() {Label})
        Else
            Dim LabelCollector As New ArrayList
            For i = 0 To StatusMessageControls.Length - 1
                Dim o As Object = DirectCast(StatusMessageControls(i), Label).Tag
                If o IsNot Nothing AndAlso Not CStr(o).ToUpper = Label.ToUpper Then
                    Dim n As New Label With {.Tag = StatusMessageControls(i).Tag, .Text = StatusMessageControls(i).Text}
                    LabelCollector.Add(n)
                End If
            Next
            ClearStatusMessage()
            For i = 0 To LabelCollector.Count - 1
                DirectCast(StatusMessageControls(i), Label).Tag = DirectCast(LabelCollector(i), Label).Tag
                DirectCast(StatusMessageControls(i), Label).Text = DirectCast(LabelCollector(i), Label).Text
            Next
        End If
    End Sub
    'Removes all of the text of the status messages
    Public Sub ClearStatusMessage()
        If Me.InvokeRequired Then
            Dim d As New cbClearStatusMessage(AddressOf ClearStatusMessage)
            Me.Invoke(d)
        Else
            For i = 0 To StatusMessageControls.Length - 1
                DirectCast(StatusMessageControls(i), Label).Text = ""
                DirectCast(StatusMessageControls(i), Label).Tag = Nothing
            Next
        End If
    End Sub

    Private Sub ExitToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExitToolStripMenuItem.Click
        Me.Close()
    End Sub
    'Saves the console log to text
    Private Sub cmdSaveLog_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cmdSaveLog.Click
        If ConsoleBox.Items.Count = 0 Then Exit Sub
        Dim fDiag As New SaveFileDialog
        fDiag.Filter = "Text files (*.txt)|*.txt|All files (*.*)|*.*"
        fDiag.Title = RM.GetString("fcusb_console_save")
        fDiag.FileName = "BcUSB.console.log.txt"
        If fDiag.ShowDialog = Windows.Forms.DialogResult.OK Then
            Dim logfile(ConsoleBox.Items.Count - 1) As String
            Dim i As Integer
            For i = 0 To logfile.Length - 1
                logfile(i) = ConsoleBox.Items.Item(i).ToString
            Next
            Try
                WriteFile(logfile, fDiag.FileName)
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub VerifyMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles VerifyMenuItem.Click
        Dim NewValue As Boolean = Not VerifyData
        VerifyData = NewValue
        VerifyMenuItem.Checked = NewValue
    End Sub

    Private Sub EnableJTAGVccPinToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles EnableJTAGVccPinToolStripMenuItem.Click
        Dim NewValue As Boolean = Not EnableJtagVcc
        EnableJtagVcc = NewValue
        EnableJTAGVccPinToolStripMenuItem.Checked = NewValue
    End Sub

    Private Sub EnglishToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles EnglishToolStripMenuItem.Click
        RM = My.Resources.English.ResourceManager : LanguageName = "English"
        ConsoleBox.Items.Clear()
        SetupText()
        Disconnect(True)
    End Sub

    Private Sub GermanToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles GermanToolStripMenuItem.Click
        RM = My.Resources.German.ResourceManager : LanguageName = "German"
        ConsoleBox.Items.Clear()
        SetupText()
        Disconnect(True)
    End Sub

    Private Sub SpanishToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles SpanishToolStripMenuItem.Click
        RM = My.Resources.Spanish.ResourceManager : LanguageName = "Spanish"
        ConsoleBox.Items.Clear()
        SetupText()
        Disconnect(True)
    End Sub

    Private Sub FrenchToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles FrenchToolStripMenuItem.Click
        RM = My.Resources.French.ResourceManager : LanguageName = "French"
        ConsoleBox.Items.Clear()
        SetupText()
        Disconnect(True)
    End Sub

    Private Sub PortugueseToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles PortugueseToolStripMenuItem.Click
        RM = My.Resources.Portuguese.ResourceManager : LanguageName = "Portuguese"
        ConsoleBox.Items.Clear()
        SetupText()
        Disconnect(True)
    End Sub

    Private Sub ChineseToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles ChineseToolStripMenuItem.Click
        RM = My.Resources.Chinese.ResourceManager : LanguageName = "Chinese"
        ConsoleBox.Items.Clear()
        SetupText()
        Disconnect(True)
    End Sub

    Private Sub SPINRF24LE1ModeToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SPINRF24LE1ModeToolStripMenuItem.Click
        Dim NewValue As Boolean = Not SpiNordicMode
        SpiNordicMode = NewValue
        SPINRF24LE1ModeToolStripMenuItem.Checked = NewValue
    End Sub

End Class
