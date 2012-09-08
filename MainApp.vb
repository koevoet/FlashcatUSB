'COPYRIGHT EMBEDDEDCOMPUTERS.NET 2012 - ALL RIGHTS RESERVED
'CONTACT EMAIL: contact@embeddedcomputers.net
'ANY USE OF THIS CODE MUST ADHERE TO THE LICENSE FILE INCLUDED WITH THIS SDK
'INFO: This is the main module that is loaded first. 

Module MainApp
    Public RM As Resources.ResourceManager = My.Resources.English.ResourceManager
    Public LanguageName As String

    Private vdataflag As Boolean = False 'Holds the verify data flag
    Private jtagvccflag As Boolean = False 'Holds the jtag vcc flag
    Private spinordflag As Boolean = False 'Holds the spi nordic flag

    Public Const Build As Integer = 330
    Public Platform As String
    Public GuiForm As MainForm
    Public ScriptPath As String = Application.StartupPath & "\Scripts\" 'Holds the full directory name of where scripts are located
    Public OperationMode As AvrMode = AvrMode.NotConnected
    Public MinBoardRev As Single = 7.0 'Min revision allowed for software
    Public AppIsClosing As Boolean = False

    Public WithEvents ScriptEngine As New Script
    Public WithEvents LegacyNonVol As New BcmNonVol
    Public WithEvents EJ As New EJTAG
    Public WithEvents DFU As New DFU_API
    Public WithEvents SPI As New SPI_API
    Public WithEvents NAND As New NAND_API
    'Entry point for the software
    Sub Main(ByVal Args() As String)
        LoadRegistry()
        LoadLanguageSettings()
        Threading.Thread.CurrentThread.Name = "rootApp"
        Platform = My.Computer.Info.OSFullName & " (" & GetOsBitsString() & ")"

        If Args IsNot Nothing AndAlso Args.Count > 0 Then 'We are running as CONSOLE
            RunConsoleMode(Args)
        Else 'We are running normal GUI
            GuiForm = New MainForm
            Dim t As New Threading.Thread(AddressOf BeginUsbCheckLoop)
            t.Start()
            Application.Run(GuiForm)
        End If

        AppClosing()
    End Sub

#Region "Console mode"
    Declare Function AllocConsole Lib "kernel32" () As Integer
    Declare Function FreeConsole Lib "kernel32" () As Integer

    Private Sub RunConsoleMode(ByVal Args() As String)
        If Convert.ToBoolean(AllocConsole()) Then
            Console.WriteLine("FlashcatUSB Build " & Build)
            Console.WriteLine(String.Format(RM.GetString("fcusb_welcome"), Build))
            Console.WriteLine(String.Format(RM.GetString("fcusb_running"), Platform))
            Dim task As ConsoleTask = ConsoleTask.NoTask
            Dim ShowStatus As Boolean = False
            Dim addr_offset As UInt32 = 0
            Dim data_len As UInt32 = 0
            Dim data_out() As Byte = Nothing
            Dim file_opt As String = ""
            Dim file_io As IO.FileInfo = Nothing
            Dim flash_index As Integer = 0
            Dim autoexit As Boolean = False
            Dim skipauto As Boolean = False
            For i = 0 To Args.Length - 1
                If Args(i).ToUpper = "-H" OrElse Args(i) = "-?" Then
                    Console_DisplayHelp() : GoTo ExitConsoleMode
                ElseIf Args(i).ToUpper = "-READ" Then
                    task = ConsoleTask.ReadMemory
                ElseIf Args(i).ToUpper = "-WRITE" Then
                    task = ConsoleTask.WriteMemory
                ElseIf Args(i).ToUpper = "-EXECUTE" Then
                    task = ConsoleTask.ExecuteScript
                ElseIf Args(i).ToUpper = "-SKIPAUTOINIT" Then
                    skipauto = True
                ElseIf Args(i).ToUpper = "-VERIFY_ON" Then
                    VerifyData = True
                ElseIf Args(i).ToUpper = "-VERIFY_OFF" Then
                    VerifyData = False
                ElseIf Args(i).ToUpper = "-AUTOEXIT" Then
                    autoexit = True
                ElseIf Args(i).ToUpper = "-INDEX" AndAlso Not i = Args.Length - 1 AndAlso IsNumeric(Args(i + 1)) Then
                    flash_index = CUInt(Args(i + 1))
                    i += 1
                ElseIf Args(i).ToUpper = "-OFFSET" AndAlso Not i = Args.Length - 1 AndAlso IsNumeric(Args(i + 1)) Then
                    addr_offset = CUInt(Args(i + 1))
                    i += 1
                ElseIf Args(i).ToUpper = "-LENGTH" AndAlso Not i = Args.Length - 1 AndAlso IsNumeric(Args(i + 1)) Then
                    data_len = Args(i + 1)
                    i += 1
                ElseIf Args(i).ToUpper = "-FILE" AndAlso Not i = Args.Length - 1 Then
                    file_opt = Args(i + 1)
                    i += 1
                End If
            Next
            If task = ConsoleTask.NoTask Then
                Console.WriteLine("No mode specified. Use -H to list all available modes.") : GoTo ExitConsoleMode
            ElseIf task = ConsoleTask.ReadMemory Then
                If file_opt = "" Then
                    Console.WriteLine("ReadMemory requires option -FILE to specify where to save to.") : GoTo ExitConsoleMode
                End If
                file_io = New IO.FileInfo(file_opt)
            ElseIf task = ConsoleTask.WriteMemory Then
                If file_opt = "" Then
                    Console.WriteLine("WriteMemory requires option -FILE to specify where to save to.") : GoTo ExitConsoleMode
                End If
                file_io = New IO.FileInfo(file_opt)
                If Not file_io.Exists Then
                    Console.WriteLine("Error: file not found: " & file_io.FullName) : GoTo ExitConsoleMode
                End If
            ElseIf task = ConsoleTask.ExecuteScript Then
                If file_opt = "" Then
                    Console.WriteLine("ExecuteScript requires option -FILE to specify which script to run.") : GoTo ExitConsoleMode
                End If
                file_io = New IO.FileInfo(Application.StartupPath & "\Scripts\" & file_opt)
                If Not file_io.Exists Then
                    file_io = New IO.FileInfo(file_opt)
                    If Not file_io.Exists Then
                        Console.WriteLine("Error: file not found: " & file_io.FullName) : GoTo ExitConsoleMode
                    End If
                End If
            End If
            OperationMode = AvrMode.NotConnected
            CheckStatus()
            Select Case OperationMode
                Case AvrMode.JTAG
                    If Not JtagSetup(skipauto) Then GoTo ExitConsoleMode
                Case AvrMode.SPI
                    Console.WriteLine(RM.GetString("cnts_mode") & ": Serial Programmable Interface (SPI)")
                    Console.WriteLine(RM.GetString("cnts_avrver") & ": " & SPI.GetAvrVersion)
                    SPI.ScanForDevice()
                    Do While SPI.Scanning
                        Application.DoEvents()
                        Sleep(200)
                    Loop
                    Select Case SPI.FlashStatus
                        Case SPI_API.Status.NotDetected
                            Console.WriteLine(RM.GetString("cnts_device") & ": " & RM.GetString("fcusb_noflash"))
                            Console.WriteLine(RM.GetString("fcusb_spi_err1"))
                            GoTo ExitConsoleMode
                        Case SPI_API.Status.NotSupported
                            Console.WriteLine(RM.GetString("cnts_device"), SPI.GetFlashName)
                            Console.WriteLine(RM.GetString("fcusb_spi_err2"))
                            GoTo ExitConsoleMode
                        Case SPI_API.Status.Supported
                            If SpiNordicMode Then
                                Console.WriteLine(RM.GetString("cnts_device") & ": SPI compatible device (Nordic SPI port)")
                            Else
                                Console.WriteLine(RM.GetString("cnts_device") & ": SPI compatible device (JEDEC ID: " & SPI.GetJedecName & ")")
                            End If
                            AddMemoryDevice(DeviceTypes.SPI, 0, SPI.GetFlashSize, Nothing, SPI.GetFlashName)
                    End Select
                Case AvrMode.NAND
                    Console.WriteLine("Board is currently in NAND mode.") : GoTo ExitConsoleMode
                Case AvrMode.DFU
                    Console.WriteLine("Board is currently in DFU mode.") : GoTo ExitConsoleMode
                Case AvrMode.NotConnected
                    Console.WriteLine("Status: not connected to FlashcatUSB board.") : GoTo ExitConsoleMode
            End Select
            ConsoleProgressReset = True
            If Not task = ConsoleTask.ExecuteScript Then
                If MyMemDevices.Count = 0 Then
                    Console.WriteLine("Unable to perform any actions because there are no detected Flash devices.") : GoTo ExitConsoleMode
                End If
                If flash_index > MyMemDevices.Count - 1 Then
                    Console.WriteLine("Flash index specified is higher than actual flash count.") : GoTo ExitConsoleMode
                End If
                If data_len = 0 Then
                    data_len = MyMemDevices(flash_index).MemSize
                End If
                Dim CurrentMax As Integer = MyMemDevices(flash_index).MemSize
                If addr_offset + data_len > CurrentMax Then
                    Console.WriteLine("Offset is to high, resetting to 0.")
                    addr_offset = 0
                End If
                If addr_offset + data_len > CurrentMax Then
                    Console.WriteLine("Length is larger than memory, resetting to default.")
                    data_len = CurrentMax
                End If
                AddHandler MyMemDevices(flash_index).SetProgress, AddressOf SetConsoleProgress
                AddHandler MyMemDevices(flash_index).SetSpeedInfoMemory, AddressOf SetConsoleSpeed
            End If
            If task = ConsoleTask.ReadMemory Then
                Dim b() As Byte = MyMemDevices(flash_index).ReadBytes(addr_offset, data_len)
                If b Is Nothing OrElse b.Length = 0 Then
                    Console.WriteLine("Error: ReadMemory was not successful because there is no data to save!") : GoTo ExitConsoleMode
                End If
                WriteBytes(b, file_io.FullName)
                Console.WriteLine("Saved the data to: " & file_io.FullName)
            ElseIf task = ConsoleTask.WriteMemory Then
                data_out = ReadBytes(file_io.FullName)
                If data_out Is Nothing OrElse data_out.Length = 0 Then
                    Console.WriteLine("Error: WriteMemory was not successful because there is no data to write!") : GoTo ExitConsoleMode
                End If
                If data_len > 0 AndAlso data_len < data_out.Length Then ReDim Preserve data_out(data_len - 1)
                'MyMemDevices(flash_index).WriteBytes(addr_offset, data_out)


                bcFlashWrite(addr_offset, data_out, flash_index, False)

            ElseIf task = ConsoleTask.ExecuteScript Then
                Console.WriteLine("Executing FlashcatUSB script file: " & file_io.Name)
                ScriptEngine.LoadScriptFile(file_io)
            End If
ExitConsoleMode:
            If Not autoexit Then
                Console.WriteLine("--------------------------------------------")
                Console.WriteLine("Press any key to close")
                Console.ReadKey()
            End If
            FreeConsole()
        End If
    End Sub

    Private Function JtagSetup(ByVal skipauto As Boolean) As Boolean
        Console.WriteLine(RM.GetString("fcusb_initjtag"))
        Dim FirmVer As String = EJ.GetFirmwareVersion
        If FirmVer Is Nothing OrElse FirmVer = "" Then
            SetStatus(RM.GetString("fcusb_err1"))
            WriteConsole(RM.GetString("fcusb_err1"))
            Return False
        End If
        Dim FirmSng As Single = StrToSingle(FirmVer)
        Console.WriteLine(RM.GetString("cnts_mode") & ": Enhanced JTAG")
        Console.WriteLine(RM.GetString("cnts_avrver") & ": " & FirmSng.ToString)
        If EJ.Init Then
            Console.WriteLine(RM.GetString("fcusb_jtagsetup"))
        Else
            Console.WriteLine(RM.GetString("fcusb_jtag_err1")) : Return False
        End If
        If EJ.TargetDevice.IDCODE = 0 Then 'Not connected
            Console.WriteLine(RM.GetString("fcusb_jtag_err2")) : Return False
        End If
        Console.WriteLine("Detected CPU ID: 0x" & Hex(EJ.TargetDevice.IDCODE) & " IMP CODE: 0x" & Hex(EJ.TargetDevice.IMPCODE))
        Console.WriteLine("Manufacturer ID: 0x" & Hex(EJ.TargetDevice.MANUID) & " Part ID: 0x" & Hex(EJ.TargetDevice.PARTNU))
        Console.WriteLine("EJTAG Version support: " & EJ.TargetDevice.IMPVER)
        If EJ.TargetDevice.NoDMA Then
            Console.WriteLine(RM.GetString("fcusb_jtag_nodma"))
        Else
            Console.WriteLine(RM.GetString("fcusb_jtag_dma"))
        End If
        If Not skipauto Then
            Dim MyScripts(,) As String = GetCompatibleScripts(EJ.TargetDevice.IDCODE)
            Dim DefaultScript As String = Reg_GetPref_DefaultScript(Hex(EJ.TargetDevice.IDCODE))
            Dim SelectScript As Integer = 0
            If MyScripts IsNot Nothing AndAlso (MyScripts.Length / 2) = 1 Then
                Console.WriteLine(String.Format(RM.GetString("fcusb_script_loading"), MyScripts(0, 0)))
                ScriptEngine.LoadScriptFile(New IO.FileInfo(ScriptPath & MyScripts(0, 0)))
            ElseIf MyScripts IsNot Nothing Then
                If Not DefaultScript = "" Then
                    For i = 0 To CInt((MyScripts.Length / 2) - 1)
                        If UCase(MyScripts(i, 0)) = UCase(DefaultScript) Then
                            SelectScript = i
                            Exit For
                        End If
                    Next
                End If
                Console.WriteLine(String.Format(RM.GetString("fcusb_script_loading"), MyScripts(SelectScript, 0)))
                Dim df As New IO.FileInfo(ScriptPath & MyScripts(SelectScript, 0))
                ScriptEngine.LoadScriptFile(df)
            End If
        End If
    End Function

    Private Enum ConsoleTask
        NoTask
        ReadMemory
        WriteMemory
        ExecuteScript
    End Enum

    Private ConsoleProgressReset As Boolean

    Private Sub SetConsoleProgress(ByVal p As Integer)
        If ConsoleProgressReset Then
            Console.WriteLine("")
            ConsoleProgressReset = False
        End If
        Console.SetCursorPosition(0, Console.CursorTop - 1)
        Console.Write("[" & p.ToString.PadLeft(3, " ") & "% complete]")
        Console.SetCursorPosition(0, Console.CursorTop + 1)
        'If p = 100 Then Console.Write(vbCrLf)
    End Sub

    Private Sub SetConsoleSpeed(ByVal speedinfo As String)
        If ConsoleProgressReset Then
            Console.WriteLine("")
            ConsoleProgressReset = False
        End If
        Console.SetCursorPosition(15, Console.CursorTop - 1)
        Console.Write(" [" & speedinfo & "]          ")
        Console.SetCursorPosition(0, Console.CursorTop + 1)
    End Sub

    Private Sub Console_DisplayHelp()
        Console.WriteLine("--------------------------------------------")
        Console.WriteLine("Syntax: FlashcatUSB.exe [mode] (options)")
        Console.WriteLine("")
        Console.WriteLine("Modes:")
        Console.WriteLine("-read            Will perform a flash memory read operation")
        Console.WriteLine("-write           Will perform a flash memory write operation")
        Console.WriteLine("-execute         Allows you to execute a Flashcat script file (*.bcs)")
        Console.WriteLine("")
        Console.WriteLine("Options:")
        Console.WriteLine("-File            Specifies the file to use for read/write/execute")
        Console.WriteLine("-Length          Specifies the number of bytes to read/write")
        Console.WriteLine("-Verify_On       Turns on data verification for flash write operations")
        Console.WriteLine("-Verify_Off      Turns off data verification for flash write operations")
        Console.WriteLine("-Index           To specify the flash index (default is 0) for read/write")
        Console.WriteLine("-AutoExit        Automatically close the console window when complete")
        Console.WriteLine("-SkipAutoInit    In JTAG mode, do not load the Autorun.ini settings")
    End Sub

#End Region

    Private Sub LoadLanguageSettings()
        Dim key As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(REGKEY)
        If key Is Nothing Then
            LoadDefaultLanguage()
        Else
            Dim o As Object = key.GetValue("Language")
            If o IsNot Nothing Then
                Dim SelLangauge As String = CStr(o)
                Select Case SelLangauge.ToUpper
                    Case "ENGLISH"
                        RM = My.Resources.English.ResourceManager : LanguageName = "English"
                    Case "SPANISH"
                        RM = My.Resources.Spanish.ResourceManager : LanguageName = "Spanish"
                    Case "FRENCH"
                        RM = My.Resources.French.ResourceManager : LanguageName = "French"
                    Case "GERMAN"
                        RM = My.Resources.German.ResourceManager : LanguageName = "German"
                    Case "PORTUGUESE"
                        RM = My.Resources.Portuguese.ResourceManager : LanguageName = "Portuguese"
                    Case "CHINESE"
                        RM = My.Resources.Chinese.ResourceManager : LanguageName = "Chinese"
                    Case Else
                        RM = My.Resources.English.ResourceManager : LanguageName = "English"
                End Select
            Else
                LoadDefaultLanguage()
            End If
        End If
    End Sub

    Private Sub LoadDefaultLanguage()
        'http://www1.cs.columbia.edu/~lok/csharp/refdocs/System.Globalization/types/CultureInfo.html
        Dim strLanguage As String = System.Globalization.CultureInfo.CurrentCulture.ToString.ToUpper.Substring(0, 2)
        Select Case strLanguage
            Case "EN"
                RM = My.Resources.English.ResourceManager : LanguageName = "English"
            Case "ES"
                RM = My.Resources.Spanish.ResourceManager : LanguageName = "Spanish"
            Case "PT"
                RM = My.Resources.Portuguese.ResourceManager : LanguageName = "Portuguese"
            Case "FR"
                RM = My.Resources.Spanish.ResourceManager : LanguageName = "French"
                'Case "IT"
            Case "DE"
                RM = My.Resources.German.ResourceManager : LanguageName = "German"
                'Case "RU"
                'Case "JA"
            Case "ZH"
                RM = My.Resources.Chinese.ResourceManager : LanguageName = "Chinese"
            Case Else
                RM = My.Resources.English.ResourceManager : LanguageName = "English"
        End Select
    End Sub

    Private Sub BeginUsbCheckLoop()
        'Wait to begin checking

        Threading.Thread.CurrentThread.Name = "usbCheckLoop"

        If GuiForm IsNot Nothing Then
            Do Until GuiForm.FormIsLoaded
                Threading.Thread.Sleep(50)
            Loop
        End If
        Do Until AppIsClosing
            Application.DoEvents()
            CheckStatus()
            Threading.Thread.Sleep(500)
        Loop
    End Sub

    Private Function CheckStatus() As Boolean
        If OperationMode = AvrMode.NotConnected Then
            If EJ.IsConnected() Then
                If GuiForm IsNot Nothing AndAlso Not AppIsClosing Then GuiForm.ShowSpiSettings(False)
                If EJ.Connect() Then
                    OperationMode = AvrMode.JTAG
                    If GuiForm IsNot Nothing AndAlso Not AppIsClosing Then GuiForm.OnDeviceConnected()
                End If
            ElseIf DFU.IsConnected Then
                If GuiForm IsNot Nothing AndAlso Not AppIsClosing Then GuiForm.ShowSpiSettings(False)
                If DFU.Connect() Then
                    OperationMode = AvrMode.DFU
                    If GuiForm IsNot Nothing AndAlso Not AppIsClosing Then GuiForm.OnDeviceConnected()
                End If
            ElseIf SPI.IsConnected Then
                If GuiForm IsNot Nothing AndAlso Not AppIsClosing Then GuiForm.ShowSpiSettings(False)
                If SPI.Connect() Then
                    OperationMode = AvrMode.SPI
                    If GuiForm IsNot Nothing AndAlso Not AppIsClosing Then GuiForm.OnDeviceConnected()
                End If
            ElseIf NAND.IsConnected Then
                If GuiForm IsNot Nothing AndAlso Not AppIsClosing Then GuiForm.ShowSpiSettings(False)
                If NAND.Connect() Then
                    OperationMode = AvrMode.NAND
                    If GuiForm IsNot Nothing AndAlso Not AppIsClosing Then GuiForm.OnDeviceConnected()
                End If
            Else
                Return False 'Not connected
            End If
        ElseIf OperationMode = AvrMode.JTAG Then
            'If Not EJTAG.CheckConnection = 0 Then GoTo Disconnected
            If Not EJ.IsConnected Then GoTo Disconnected
        ElseIf OperationMode = AvrMode.DFU Then
            If Not DFU.IsConnected Then GoTo Disconnected
        ElseIf OperationMode = AvrMode.SPI Then
            If Not SPI.IsConnected Then SPI.Disconnect() : GoTo Disconnected
        ElseIf OperationMode = AvrMode.NAND Then
            If Not NAND.IsConnected Then NAND.Disconnect() : GoTo Disconnected
        End If
        Return True 'We are still connected
Disconnected:
        Disconnect()
        Return False
    End Function

    Public Sub Disconnect(Optional SkipMsg As Boolean = False)
        OperationMode = AvrMode.NotConnected
        MainFlashLoaded = False
        DFU.Clear()
        MyMemDevices.Clear() 'Removes all of our devices
        MemoryInterface.FlashCounter = -1
        If GuiForm IsNot Nothing Then
            GuiForm.OnDeviceDisconnected()
            GuiForm.ShowSpiSettings(True)
        End If
        If Not SkipMsg Then WriteConsole(RM.GetString("fcusb_disconnected"))
    End Sub
    'Called whent the device is closing
    Public Sub AppClosing()
        AppIsClosing = True
        For Each memdev In MyMemDevices
            memdev.QuitOperation()
            Do While memdev.IsBusy
                Threading.Thread.Sleep(100)
            Loop
        Next
        If OperationMode = AvrMode.JTAG Then
            EJ.CloseDevice()
        ElseIf OperationMode = AvrMode.SPI Then
            SPI.Disconnect()
        ElseIf OperationMode = AvrMode.NAND Then
            NAND.Disconnect()
        End If
    End Sub

    Private MyLock As New Object

    Public Sub OnDfuStatusUpdate(ByVal percent As Integer) Handles DFU.OnStatus
        If GuiForm IsNot Nothing Then GuiForm.UpdateDfuStatusBar(percent)
    End Sub

    Private Sub OnScriptPrint(ByVal Msg As String) Handles ScriptEngine.printf
        WriteConsole(Msg)
    End Sub

    Private Sub OnNonVolPrint(ByVal Msg As String) Handles LegacyNonVol.Printf
        WriteConsole(Msg)
    End Sub

    Public Sub WriteConsole(ByVal Msg As String)
        If AppIsClosing Then Exit Sub
        Threading.Monitor.Enter(MyLock)
        Try
            If GuiForm IsNot Nothing Then
                GuiForm.PrintConsole(Msg)
            Else 'We are writing to console
                Console.WriteLine(Msg)
                ConsoleProgressReset = True
            End If
        Catch ex As Exception
        Finally
            Threading.Monitor.Exit(MyLock)
        End Try
    End Sub

    Public Sub SetStatus(ByVal Msg As String)
        If Not GuiForm Is Nothing Then GuiForm.SetStatus(Msg)
    End Sub

#Region "Windows Registry"

    Private Const REGKEY As String = "Software\EmbComputers\FlashcatUSB\"

    Public Function GetRegistryValue(ByVal Name As String, ByVal DefaultValue As String) As String
        Dim key As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(REGKEY)
        If key Is Nothing Then Return DefaultValue
        Dim o As Object = key.GetValue(Name)
        If o Is Nothing Then Return DefaultValue
        Return CStr(o)
    End Function

    Public Function SetRegistryValue(ByVal Name As String, ByVal Value As String) As Boolean
        Try
            Dim key As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(REGKEY, True)
            key.SetValue(Name, Value)
            Return True
        Catch ex As Exception
            Return False
        End Try
    End Function

    Public Function GetRegistryValueBool(ByVal Name As String, ByVal DefaultValue As Boolean) As Boolean
        Try
            Dim key As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(REGKEY)
            If key Is Nothing Then Return DefaultValue
            Dim o As Object = key.GetValue(Name)
            If o Is Nothing Then Return DefaultValue
            Return CBool(o)
        Catch ex As Exception
            Return False
        End Try
    End Function

    Public Function SetRegistryValueBool(ByVal Name As String, ByVal Value As Boolean) As Boolean
        Try
            Dim key As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(REGKEY, True)
            If key Is Nothing Then
                Microsoft.Win32.Registry.CurrentUser.CreateSubKey(REGKEY)
                key = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(REGKEY, True)
            End If
            key.SetValue(Name, Value)
            Return True
        Catch ex As Exception
            Return False
        End Try
    End Function
    'Loads values from registry into memory
    Public Sub LoadRegistry()
        Try
            VerifyData = GetRegistryValueBool("VerifyData", True)
            EnableJtagVcc = GetRegistryValueBool("VccEnable", False)
            SpiNordicMode = GetRegistryValueBool("SpiNordicMode", False)
        Catch ex As Exception
            VerifyData = True
        End Try
    End Sub

    Public Sub SaveRegistry()
        SetRegistryValueBool("VerifyData", VerifyData)
        SetRegistryValueBool("VccEnable", EnableJtagVcc)
        SetRegistryValueBool("SpiNordicMode", SpiNordicMode)
    End Sub

    Public Function Reg_GetPref_DefaultScript(ByVal CPUID As String) As String
        Try
            Dim key As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(REGKEY & "Default")
            If key Is Nothing Then Return ""
            Dim o As Object = key.GetValue(CPUID)
            If o Is Nothing Then Return ""
            Return CStr(o)
        Catch ex As Exception
            Return ""
        End Try
    End Function

    Public Sub Reg_SavePref_DefaultScript(ByVal CPUID As String, ByVal Script As String)
        Try
            Dim key As Microsoft.Win32.RegistryKey = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(REGKEY & "Default", True)
            If key Is Nothing Then
                Microsoft.Win32.Registry.CurrentUser.CreateSubKey(REGKEY & "Default")
                key = Microsoft.Win32.Registry.CurrentUser.OpenSubKey(REGKEY & "Default", True)
            End If
            key.SetValue(CPUID, Script)
        Catch ex As Exception
        End Try
    End Sub

#End Region

#Region "Software DLL Wrapper Calls"

    Public Sub bcLedOff()
        If OperationMode = AvrMode.JTAG Then
            EJ.LEDOff()
        ElseIf OperationMode = AvrMode.SPI Then
            SPI.LEDOff()
        ElseIf OperationMode = AvrMode.NAND Then
            NAND.LedOff()
        End If
    End Sub

    Public Sub bcLedOn()
        If OperationMode = AvrMode.JTAG Then
            EJ.LEDOn()
        ElseIf OperationMode = AvrMode.SPI Then
            SPI.LEDOn()
        ElseIf OperationMode = AvrMode.NAND Then
            NAND.LedOff()
        End If
    End Sub

    Public Sub bcLedBlink()
        Dim datRet() As Byte = Nothing
        If OperationMode = AvrMode.JTAG Then
            EJ.LEDBlink(200)
        ElseIf OperationMode = AvrMode.SPI Then
            SPI.LEDBlink()
        ElseIf OperationMode = AvrMode.NAND Then
            'NAND.LedBlink()
        End If
    End Sub

    Public Sub bcClosing()
        If OperationMode = AvrMode.JTAG Then
            'nothing to do for JTAG
        ElseIf OperationMode = AvrMode.SPI Then
            SPI.Disconnect()
        ElseIf OperationMode = AvrMode.NAND Then
            NAND.Disconnect()
        End If
    End Sub

#End Region

    Public Function GetCompatibleScripts(ByVal CPUID As UInteger) As String(,)
        Dim DefaultFile As New IO.FileInfo(ScriptPath & "default.bcs")
        Dim Autorun As New IO.FileInfo(ScriptPath & "autorun.ini")
        If Autorun.Exists Then
            Dim autoscripts(,) As String = Nothing
            If ProcessAutorun(Autorun, CPUID, autoscripts) Then
                Return autoscripts
            End If
        End If
        If DefaultFile.Exists Then
            Return New String(,) {{"default.bcs", "Default"}}
        End If
        Return Nothing
    End Function

    Private Function ProcessAutorun(ByVal Autorun As IO.FileInfo, ByVal ID As UInteger, ByRef scripts(,) As String) As Boolean
        Try
            Dim f() As String = ReadFile(Autorun.FullName)
            Dim autoline() As String
            Dim sline As String
            Dim MyCode As UInteger
            Dim out As New ArrayList 'Holds str()
            For Each sline In f
                sline = Trim(RemoveComment(sline))
                If Not sline = "" Then
                    autoline = sline.Split(CChar(":"))
                    If autoline.Length = 3 Then
                        MyCode = HexToUint(autoline(0))
                        If MyCode = ID Then
                            out.Add(New String() {autoline(1), autoline(2)})
                        End If
                    End If
                End If
            Next
            If out.Count > 0 Then
                Dim ret(out.Count - 1, 1) As String
                Dim i As Integer
                Dim s() As String
                For i = 0 To out.Count - 1
                    s = CType(out(i), String())
                    ret(i, 0) = s(0)
                    ret(i, 1) = s(1)
                Next
                scripts = ret
                Return True 'Scripts are available
            End If
        Catch ex As Exception
            WriteConsole("Error processing Autorun.ini")
        End Try
        Return False
    End Function

    Public Function bcGetPreferredBlockSize() As Integer
        If OperationMode = AvrMode.JTAG Then
            If EJ.TargetDevice.NoDMA Then
                Return 512 'non dma is REALLY slow
            Else
                'Return 1024 '78KB on 3349
                'Return 2048 '101.5KB on 3349
                Return 4096 '113KB on 3349
            End If
        ElseIf OperationMode = AvrMode.SPI Then
            'Return 1024 '60KB
            'Return 4096 '128KB
            'Return 8192 155KB
            'Return 16384 '170KB
            Return 32768 '180KB
            'Return 65536
        ElseIf OperationMode = AvrMode.NAND Then
            Return 16896
        End If
        Return 1024
    End Function

    Public Function bcGetFlashSize(ByVal index As Integer) As Integer
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(index)
        If memDev Is Nothing Then Return 0 'Not found
        Return memDev.MemSize
    End Function

    Public Function bcReadMode(ByVal index As Integer) As Integer
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(index)
        If memDev Is Nothing Then Return 0 'Not found
        memDev.ReadMode()
        Return 1
    End Function

    Public Function bcFlashWrite(ByVal addr32 As UInteger, ByRef data() As Byte, ByVal index As Integer, ByVal silent As Boolean) As Boolean
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(index)
        If memDev Is Nothing Then Return False 'Not found
        Dim FlashBase As UInt32 = memDev.BaseAddress
        Dim FlashSize As Integer = memDev.MemSize
        Dim LastAddress As UInt32 = CUInt(FlashBase + addr32 + data.Length - 1)
        Dim TopAddr As UInt32 = CUInt(FlashBase + FlashSize - 1)
        If LastAddress > TopAddr Then
            WriteConsole(RM.GetString("fcusb_flasherr1"))
            Return False
        End If
        memDev.Silent = silent
        Return memDev.WriteBytes(addr32, data)
    End Function

    Public Function bcFlashSectorSize(ByVal sector As Integer, ByVal index As Integer) As Integer
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(index)
        If memDev Is Nothing Then Return 0 'Not found
        Return memDev.GetSectorSize(sector)
    End Function

    Public Function bcReadFlashByte(ByVal address As UInteger, ByVal index As Integer) As Byte
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(index)
        If memDev Is Nothing Then Return 0 'Not found
        Return memDev.ReadByte(address)
    End Function

    Public Function bcFlashSectorCount(ByVal index As Integer) As Integer
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(index)
        If memDev Is Nothing Then Return 0 'Not found
        Return memDev.GetFlashSectors
    End Function

    Public Sub bcFlashEraseSector(ByVal sector As Integer, ByVal memIndex As Integer)
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(memIndex)
        If memDev Is Nothing Then Exit Sub 'Not found
        memDev.EraseSector(sector)
        memDev.GuiControl.UpdateEditor()
    End Sub

    Public Function bcFlashEraseSection(ByVal address As Integer, ByVal bytecount As Integer, ByVal memIndex As Integer) As Boolean
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(memIndex)
        If memDev Is Nothing Then Return False 'Not found
        Dim SectorCounter As Integer = GetSectorFromAddress(memDev, address)
        If SectorCounter = -1 Then Return False
        Dim BytesErased As Integer = 0
        Do Until BytesErased >= bytecount
            BytesErased += memDev.GetSectorSize(SectorCounter)
            memDev.EraseSector(SectorCounter)
            SectorCounter = SectorCounter + 1
        Loop
        Return True
    End Function

    Public Sub bcFlashEraseBulk(ByVal memIndex As Integer)
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(memIndex)
        If memDev Is Nothing Then Exit Sub 'Not found
        memDev.EraseBulk()
    End Sub
    'On error or busy, return nothing
    Public Function bcReadMemory(ByVal Addr As UInt32, ByVal Len As Integer, ByVal memIndex As Integer, ByVal silent As Boolean) As Byte()
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(memIndex)
        If memDev Is Nothing Then Return Nothing 'Not found
        memDev.Silent = silent
        Return memDev.ReadBytes(Addr, Len)
    End Function
    'Attempts to write blindly to a CFI compatible flash device, this is used for hacking techniques to fix devices
    Public Function bcFlashBlindly(ByVal address As UInt32, ByVal data() As Byte, ByVal index As Integer) As Boolean
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(index)
        If memDev Is Nothing Then Return False 'Not found
        Return memDev.WriteBytes(address, data)
        Return True
    End Function

    Public Function bcMemoryUpdate(ByVal memIndex As Integer) As Boolean
        Dim memDev As MemoryDeviceInstance = MemoryInterface.GetDeviceInstance(memIndex)
        If memDev Is Nothing Then Return False 'Not found
        memDev.GuiControl.UpdateEditor()
        Return True
    End Function

    Public Property VerifyData() As Boolean
        Get
            Return vdataflag
        End Get
        Set(ByVal value As Boolean)
            vdataflag = value
        End Set
    End Property

    Public Property EnableJtagVcc() As Boolean
        Get
            Return jtagvccflag
        End Get
        Set(ByVal value As Boolean)
            jtagvccflag = value
            If OperationMode = AvrMode.JTAG Then EJ.EnableVccPin(jtagvccflag)
        End Set
    End Property

    Public Property SpiNordicMode() As Boolean
        Get
            Return spinordflag
        End Get
        Set(ByVal value As Boolean)
            spinordflag = value
        End Set
    End Property


End Module
