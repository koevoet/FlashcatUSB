'COPYRIGHT EMBEDDEDCOMPUTERS.NET 2012 - ALL RIGHTS RESERVED
'CONTACT EMAIL: contact@embeddedcomputers.net
'ANY USE OF THIS CODE MUST ADHERE TO THE LICENSE FILE INCLUDED WITH THIS SDK
'INFO: this class creates a flash memory interface that is used by the main program

Module MemoryInterface
    'Used by MemoryDeviceInstance to specify what type of memory device is attached
    Public Enum DeviceTypes
        NotSpecified = 0
        Dram = 1
        CFI = 2
        SPI = 3
        NAND = 4
    End Enum

    'Contains all of the memory devices (dram, cfi flash, spi flash) - resets at disconnect
    Public MyMemDevices As New List(Of MemoryDeviceInstance)
    Public MainFlashLoaded As Boolean = False
    Public FlashCounter As Integer = -1
    Public FlashWatch As New Stopwatch

    Public Function GetDeviceInstance(ByVal IndexName As Integer) As MemoryDeviceInstance
        If MyMemDevices.Count = 0 Then Return Nothing
        Dim i As Integer
        Dim memDev As MemoryDeviceInstance
        For i = 0 To MyMemDevices.Count - 1
            memDev = CType(MyMemDevices(i), MemoryDeviceInstance)
            If memDev.Index = IndexName Then Return memDev
        Next
        Return Nothing
    End Function
    'Returns true if the memory interface at the specified index exists
    Public Function HasMemoryAttached(ByVal IndexName As Integer) As Boolean
        If MyMemDevices.Count = 0 Then Return False
        Dim i As Integer
        Dim memDev As MemoryDeviceInstance
        For i = 0 To MyMemDevices.Count - 1
            memDev = CType(MyMemDevices(i), MemoryDeviceInstance)
            If memDev.Index = IndexName Then Return True
        Next
        Return False
    End Function

    Public Function AddMemoryDevice(ByVal TypeStr As DeviceTypes, ByVal BaseAddress As UInt32, ByVal Size As Integer, ByRef IndexCreated As Integer, Optional ByVal Name As String = "(Not specified)") As Boolean
        If OperationMode = AvrMode.JTAG Then
            If EJ.TargetDevice.NoDMA Then
                If Not EJ.SUPPORT_PRACC Then
                    WriteConsole(RM.GetString("fcusb_memif_nopraccsupport"))
                    Return False
                End If
            Else
                If Not EJ.SUPPORT_DMA Then
                    WriteConsole(RM.GetString("fcusb_memif_nopraccsupport"))
                    Return False
                End If
            End If
        End If
        Dim TabString As String = ""
        Select Case TypeStr
            Case DeviceTypes.CFI
                TabString = "CFI Flash"
            Case DeviceTypes.Dram
                TabString = "Memory"
            Case DeviceTypes.SPI
                TabString = "SPI Flash"
            Case DeviceTypes.NAND
                TabString = "NAND Flash"
        End Select
        Dim memDev As New MemoryDeviceInstance(TypeStr)
        memDev.MemSize = Size
        memDev.BaseAddress = BaseAddress
        memDev.PartName = Name
        AddHandler memDev.PrintConsole, AddressOf WriteConsole
        If GuiForm IsNot Nothing Then AddHandler memDev.SetStatus, AddressOf GuiForm.SetStatus
        If memDev.Init() Then 'Must be called after events are setup
            FlashCounter += 1
            IndexCreated = FlashCounter
            memDev.Index = FlashCounter
            If GuiForm IsNot Nothing Then
                Dim newTab As New TabPage(TabString)
                memDev.GuiControl.Width = newTab.Width
                memDev.GuiControl.Height = newTab.Height
                memDev.GuiControl.Anchor = AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Top Or AnchorStyles.Bottom
                memDev.GuiControl.Loaded()
                If Not TypeStr = DeviceTypes.Dram Then
                    GuiForm.UpdateStatusMessage(RM.GetString("cnts_flashname"), memDev.PartName)
                    GuiForm.UpdateStatusMessage(RM.GetString("cnts_flashsize"), Format(memDev.MemSize, "#,###") & " bytes")
                End If
                newTab.Controls.Add(memDev.GuiControl)
                GuiForm.AddTab(newTab)
            End If
            MyMemDevices.Add(memDev)
            Return True
        Else
            WriteConsole(RM.GetString("fcusb_memif_err1"))
            Return False
        End If
    End Function

    Public Sub RemoveMemoryDevice(ByVal IndexName As Integer)
        Dim counter As Integer = 0
        For Each memDev In MyMemDevices
            If memDev.Index = IndexName Then
                MyMemDevices.RemoveAt(counter)
                Exit Sub
            End If
            counter += 1
        Next
    End Sub

    Public Function GetSectorFromAddress(ByRef memDev As MemoryDeviceInstance, ByVal AddressToFind As Integer) As Integer
        Dim x As Integer = memDev.GetFlashSectors
        For i = 0 To x - 1
            Dim a As Integer = CInt(memDev.FindSectorBase(i))
            Dim s As Integer = memDev.GetSectorSize(i)
            If (a = AddressToFind) Or ((a + s) > AddressToFind) Then
                Return i
            End If
        Next
        Return -1
    End Function

    Public Class MemoryDeviceInstance
        Public WithEvents GuiControl As MemControl
        Public DeviceType As DeviceTypes = DeviceTypes.NotSpecified
        Private BytesWritten As Integer = 0
        Private ReadOnlyMode As Boolean = False 'Set to true to stop user from writing to device
        Private GuiIsUsing As Boolean = False 'Form sets this when it is using
        Private m_quit As Boolean = False
        Private m_in_operation As Boolean = False
        Public PartName As String
        Public Index As Integer 'Contains a unque id
        Public MemSize As Integer 'Number of bytes of the memory device
        Public BaseAddress As UInt32 = 0 'Only changes for JTAG devices
        Public Silent As Boolean = False 'Set to True to disable progress, status and speed info updates

        Public Event PrintConsole(ByVal msg As String) 'Prints text to the console
        Public Event SetStatus(ByVal statusMsg As String) 'Sets the status text at the bottom of the software
        Public Event SetProgress(ByVal p As Integer)
        Public Event SetSpeedInfoMemory(ByVal SpeedInfo As String)


        Private WithEvents CFIFlash As CFI = Nothing

        Sub New(ByVal devType As DeviceTypes)
            GuiControl = New MemControl
            AddHandler Me.SetProgress, AddressOf GuiControl.SetProgress
            AddHandler Me.SetSpeedInfoMemory, AddressOf GuiControl.SetSpeedInfoMemory

            DeviceType = devType
        End Sub

        Public Sub QuitOperation()
            m_quit = True
        End Sub

        Public ReadOnly Property IsBusy
            Get
                Return m_in_operation
            End Get
        End Property

        Public Function Init() As Boolean
            If DeviceType = DeviceTypes.CFI Then
                CFIFlash = New CFI
                AddHandler CFIFlash.PrintConsole, AddressOf CfiConsole
                If CFIFlash.DetectFlash(BaseAddress) Then
                    MemSize = CFIFlash.FlashSize
                    PartName = CFIFlash.FlashName
                Else
                    Return False
                End If
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                If Not EJ.SPI_API_LOADED Then
                    RaiseEvent PrintConsole(RM.GetString("fcusb_memif_err2")) 'Can not detect SPI flash (API not loaded for current MCU)
                    Return False
                End If
                If EJ.SPI_Detect Then
                    MemSize = EJ.SPI_Definition.FlashSize
                    PartName = EJ.SPI_Definition.Name
                Else
                    Return False
                End If
            ElseIf DeviceType = DeviceTypes.Dram Then
                PartName = "Generic DRAM"
            End If
            If DeviceType = DeviceTypes.Dram Then
                GuiControl.SetDeviceName("Memory Device", PartName)
                GuiControl.LoadData(MemSize, HexEditor.AccessType._ReadWrite, HexEditor.AccessMode._StreamNoCache, Nothing)
            ElseIf DeviceType = DeviceTypes.CFI Then
                GuiControl.SetDeviceName("CFI Flash Device", PartName)
                GuiControl.LoadData(MemSize, HexEditor.AccessType._ReadOnly, HexEditor.AccessMode._Stream, Nothing)
                If Not MainFlashLoaded Then MainFlashLoaded = True
            ElseIf DeviceType = DeviceTypes.SPI Then
                GuiControl.SetDeviceName("SPI Flash device", PartName)
                GuiControl.LoadData(MemSize, HexEditor.AccessType._ReadOnly, HexEditor.AccessMode._Stream, Nothing)
            ElseIf DeviceType = DeviceTypes.NAND Then
                GuiControl.SetDeviceName("NAND Flash device", PartName)
                GuiControl.LoadData(MemSize, HexEditor.AccessType._ReadOnly, HexEditor.AccessMode._Stream, Nothing)
            End If
            Return True
        End Function

        Public Function GetTypeString() As String
            Select Case DeviceType
                Case DeviceTypes.CFI
                    Return "CFI Flash"
                Case DeviceTypes.Dram
                    Return "DRAM"
                Case DeviceTypes.SPI
                    Return "SPI Flash"
                Case DeviceTypes.NAND
                    Return "NAND Flash"
            End Select
            Return ""
        End Function

        Private Sub CfiConsole(ByVal msg As String)
            RaiseEvent PrintConsole(msg)
        End Sub

        Public Property IsReadOnly() As Boolean
            Get
                Return ReadOnlyMode
            End Get
            Set(ByVal value As Boolean)
                ReadOnlyMode = value
            End Set
        End Property
        'User hit the "READ" button on the form control
        Private Sub OnReadRequest(ByVal addr As UInt32, ByRef data() As Byte) Handles GuiControl.ReadData
            GuiIsUsing = True
            Silent = False
            Try
                data = ReadBytes(addr, data.Length)
            Catch ex As Exception
                GuiControl.SetProgress(0)
                RaiseEvent SetStatus("")
            End Try
            GuiIsUsing = False
        End Sub
        'User hit the "WRITE" button on the form control
        Private Sub OnWriteRequest(ByVal addr As UInt32, ByVal data() As Byte) Handles GuiControl.WriteData
            Dim result As Boolean = False
            GuiIsUsing = True
            Silent = False
            Try
                result = WriteBytes(addr, data)
            Catch ex As Exception
                GuiControl.SetProgress(0)
                RaiseEvent SetStatus("")
            End Try
            If Not result Then data = Nothing
            GuiIsUsing = False
        End Sub

        Private Sub OnStopRequest() Handles GuiControl.StopOperation
            m_quit = True
        End Sub

        Private Sub OnStatusRequest(ByVal msg As String) Handles GuiControl.SetStatus
            RaiseEvent SetStatus(msg)
        End Sub
        'Mainstream version (pre-status)
        Public Function ReadBytes(ByVal Address As UInt32, ByVal ByteCount As Integer) As Byte()
            m_quit = False
            GuiControl.SetupForReadOperation()
            If Not Silent Then RaiseEvent PrintConsole(String.Format(RM.GetString("fcusb_memif_beginread"), GetTypeString))
            If Not Silent Then RaiseEvent PrintConsole(String.Format(RM.GetString("fcusb_memif_startaddr"), Address, "0x" & IntToHex(Address), ByteCount))
            Dim BlockSize As Integer = bcGetPreferredBlockSize()
            Dim i As Integer 'Temp Loop counter index
            If Not Silent Then RaiseEvent SetProgress(0)
            Dim Loops As Integer = CInt(Math.Ceiling(ByteCount / BlockSize)) 'Calcuates iterations
            Dim b() As Byte 'Temp Byte buffer
            Dim Loc As Integer = 0 'location of FILE not stream
            Dim sVal As Single 'Needs to be single to have decimals
            Dim StatusLoc, StatusPrec, StatusSpeeed As String 'Temp String space
            StatusSpeeed = ""
            Dim Sw As New Stopwatch 'Used to calculate time
            Sw.Start() 'Start Stopwach
            Dim BytesRemaining As Integer = ByteCount
            Dim fileout(ByteCount - 1) As Byte
            m_in_operation = True
            For i = 1 To Loops
                If m_quit Then GoTo ExitReadBytesWithNothing
                Dim BytesCountToRead As Integer = BytesRemaining
                If BytesCountToRead > BlockSize Then BytesCountToRead = BlockSize
                ReDim b(BytesCountToRead - 1) 'Erase block data
                b = ReadMemoryBlock(CUInt(Address + Loc), BytesCountToRead)
                Array.Copy(b, 0, fileout, Loc, b.Length)  'Copies block buffer into file
                Loc = Loc + BytesCountToRead 'Increment location address
                sVal = CSng((i / Loops) * 100) 'Calulate % done
                If i Mod 10 = 0 Or i = Loops Then
                    If Not Silent Then RaiseEvent SetProgress(CInt(sVal)) 'Set Progress bar (if there is one)
                    StatusLoc = Format(Loc, "#,###") & " of " & Format(ByteCount, "#,###") & " Bytes " 'Format Status
                    StatusPrec = "(" & Math.Round(sVal, 0) & "%) " 'Format Status
                    StatusSpeeed = Format(Math.Round(Loc / (Sw.ElapsedMilliseconds / 1000)), "#,###") & " Bytes/s"  'Format Status
                    If Not Silent Then RaiseEvent SetStatus("Reading " & GetTypeString() & " " & StatusLoc & StatusPrec) '& StatusSpeeed 'Print Status
                    RaiseEvent SetSpeedInfoMemory(StatusSpeeed)
                    Application.DoEvents()
                End If
                BytesRemaining -= BytesCountToRead
            Next
            Sw.Stop()
            m_in_operation = False
            GuiControl.OperationComplete()
            If Not Silent Then RaiseEvent PrintConsole(RM.GetString("fcusb_memif_readdone"))
            If Not Silent Then RaiseEvent PrintConsole("Read " & ByteCount.ToString & " bytes in " & (Sw.ElapsedMilliseconds / 1000) & " seconds, " & StatusSpeeed)
            Return fileout
ExitReadBytesWithNothing:
            m_in_operation = False
            GuiControl.OperationComplete()
            Return Nothing
        End Function
        'Mainstream version (pre-status)
        Public Function WriteBytes(ByVal Address As UInteger, ByRef Data() As Byte) As Boolean
            If ReadOnlyMode Then
                If Not Silent Then RaiseEvent PrintConsole(RM.GetString("fcusb_memif_write_err1"))
                Return False
            End If
            m_quit = False
            FlashWatch.Stop()
            FlashWatch.Reset()
            GuiControl.SetupForReadOperation()
            m_in_operation = True
            Dim WriteResult As Boolean
            If DeviceType = DeviceTypes.Dram Then
                WriteResult = WriteBytes_LoopForVol(Address, Data)
            Else 'Non-Volatile memory
                WriteResult = WriteBytes_LoopForNonVol(Address, Data)
            End If
            m_in_operation = False
            GuiControl.OperationComplete()
            If WriteResult Then
                If Not Silent Then
                    Dim Speed As String = CStr(Format(Math.Round(Data.Length / (FlashWatch.ElapsedMilliseconds / 1000)), "#,###"))
                    RaiseEvent SetStatus(String.Format(RM.GetString("fcusb_memif_writedone"), Format(Data.Length, "#,###")))
                    RaiseEvent PrintConsole(RM.GetString("fcusb_memif_writecomplete"))
                    RaiseEvent PrintConsole(String.Format(RM.GetString("fcusb_memif_writespeed"), Format(Data.Length, "#,###"), (FlashWatch.ElapsedMilliseconds / 1000), Speed))
                End If
                ReadMode()
                Return True
            Else
                If Not Silent Then GuiControl.SetProgress(0)
                ReadMode()
                Return False
            End If
        End Function

        Private Function WriteBytes_LoopForVol(ByVal Address As UInt32, ByVal data() As Byte) As Boolean
            FlashWatch.Start()
            Dim BlockSize As Integer = 4096
            If EJ.TargetDevice.NoDMA Then BlockSize = 1024
            Dim BytesLeft As Integer = data.Length
            Dim BytesWritten As Integer = 0
            While BytesLeft > 0
                If m_quit Then Return False
                Dim BytesThisPacket As Integer = BytesLeft
                If BytesThisPacket > BlockSize Then BytesThisPacket = BlockSize
                Dim DataOut(BytesThisPacket - 1) As Byte
                Array.Copy(data, BytesWritten, DataOut, 0, BytesThisPacket)
                Dim result As Boolean = EJ.Memory_Write_Bulk(Address + BytesWritten, DataOut)
                If Not result Then Return False
                If GuiIsUsing And Not Silent Then RaiseEvent SetSpeedInfoMemory(Format(Math.Round(BytesWritten / (FlashWatch.ElapsedMilliseconds / 1000)), "#,###") & " Bytes/s")
                BytesWritten += BytesThisPacket
                BytesLeft -= BytesThisPacket
            End While
            FlashWatch.Stop()
            Return True
        End Function

        Private Function WriteBytes_LoopForNonVol(ByVal Address As UInt32, ByVal data() As Byte) As Boolean
            AddHandleForSpeedUpdate()
            Dim TotalSectors As Integer = GetFlashSectors()
            BytesWritten = 0
            Dim SectorLength As Integer
            Dim SectorBase As UInteger
            Dim TempBuffer(), PreBuffer() As Byte
            Dim OrgByteCount As Integer = data.Length
            Dim i As Integer
            WaitUntilReady() 'Some flash devices requires us to wait before sending data
            For i = 0 To TotalSectors - 1
                Dim percent As Single = CSng((i / TotalSectors) * 100)
                Dim DataLength As Integer = data.Length
                If (Not Silent) Then RaiseEvent SetProgress(CInt(((OrgByteCount - DataLength) / OrgByteCount) * 100))
                If m_quit Then GoTo ExitWithFalse
                SectorBase = FindSectorBase(i)
                SectorLength = GetSectorSize(i)
                ReDim TempBuffer(SectorLength - 1)
                If Address = SectorBase Then 'Common
                    If data.Length > TempBuffer.Length Then
                        Array.Copy(data, 0, TempBuffer, 0, TempBuffer.Length)
                        ReDim PreBuffer(data.Length - TempBuffer.Length - 1)
                        Array.Copy(data, TempBuffer.Length, PreBuffer, 0, PreBuffer.Length)
                        data = PreBuffer
                    ElseIf data.Length = TempBuffer.Length Then
                        TempBuffer = data
                        data = Nothing
                    Else 'Data has less data, read in from flash to fill buffer
                        Dim More As Integer = SectorLength - data.Length
                        Dim b() As Byte = ReadMemoryBlock(CUInt(Address + data.Length), More)
                        If b Is Nothing Then GoTo ExitWithFalse
                        Array.Copy(data, 0, TempBuffer, 0, data.Length) 'Copy over the data to write
                        Array.Copy(b, 0, TempBuffer, data.Length, b.Length) 'Copy over the read data
                        data = Nothing
                    End If
                    WriteDataSub(i, Address, TempBuffer, Math.Round(percent, 0)) 'Writes data
                    BytesWritten += TempBuffer.Length
                    If (Not Silent) Then RaiseEvent SetSpeedInfoMemory(Format(Math.Round(BytesWritten / (FlashWatch.ElapsedMilliseconds / 1000)), "#,###") & " Bytes/s")
                    If data Is Nothing Then Exit For 'No more data to write
                    Address = CUInt(Address + TempBuffer.Length) 'Increment the address we are writing
                ElseIf Address > SectorBase And Address < (SectorBase + SectorLength) Then 'Address is greater than base address
                    Dim DestBase As UInteger = CUInt(Address - SectorBase) 'Where the data is going (array)
                    Dim BytesToCopy As Integer = CInt(SectorLength - DestBase) 'How many bytes to copy from data array
                    Dim b() As Byte = ReadMemoryBlock(CUInt(SectorBase), CInt(DestBase)) 'Read in what we are not writing
                    If b Is Nothing Then GoTo ExitWithFalse
                    Address = SectorBase 'push the address pointer back to the begining of the sector
                    Array.Copy(b, 0, TempBuffer, 0, b.Length) 'TempBuffer has data before Address
                    If data.Length > BytesToCopy Then 'We have more to write
                        Array.Copy(data, 0, TempBuffer, DestBase, BytesToCopy) 'TempBuffer = all data to write
                        ReDim PreBuffer(data.Length - BytesToCopy - 1)
                        Array.Copy(data, SectorLength - DestBase, PreBuffer, 0, PreBuffer.Length)
                        data = PreBuffer 'Data has bytes left to write
                    ElseIf data.Length = BytesToCopy Then 'We wrote everything (excactly) probably rare
                        Array.Copy(data, 0, TempBuffer, DestBase, BytesToCopy) 'TempBuffer = all data to write
                        data = Nothing
                    Else 'We need to fill the end of the array
                        Dim BytesFilled As Integer = CInt(Address + DestBase + data.Length)
                        b = ReadMemoryBlock(CUInt(BytesFilled), CInt(SectorLength - (DestBase + data.Length)))
                        If b Is Nothing Then GoTo ExitWithFalse
                        Array.Copy(b, 0, TempBuffer, (DestBase + data.Length), b.Length) 'Fills end of array with data from flash
                        Array.Copy(data, 0, TempBuffer, DestBase, data.Length) 'Finally, copy the bytes we want to write into the dest array
                        data = Nothing
                    End If
                    WriteDataSub(i, Address, TempBuffer, Math.Round(percent, 0)) 'Writes data
                    BytesWritten += TempBuffer.Length
                    If (Not Silent) Then RaiseEvent SetSpeedInfoMemory(Format(Math.Round(BytesWritten / (FlashWatch.ElapsedMilliseconds / 1000)), "#,###") & " Bytes/s")
                    If data Is Nothing Then Exit For 'No more data to write
                    Address = CUInt(SectorBase + SectorLength)
                End If
            Next
            RemoveHandleForSpeedUpdate()
            Return True 'Operation was successful
ExitWithFalse:
            RemoveHandleForSpeedUpdate()
            Return False
        End Function

        Private Sub AddHandleForSpeedUpdate()
            If OperationMode = AvrMode.JTAG Then
                AddHandler EJ.UpdateProgress, AddressOf UpdateSpeedInfo
            End If
        End Sub

        Private Sub RemoveHandleForSpeedUpdate()
            If OperationMode = AvrMode.JTAG Then
                RemoveHandler EJ.UpdateProgress, AddressOf UpdateSpeedInfo
            End If
        End Sub

        Private Sub UpdateSpeedInfo(ByVal CurrentBytesWritten As Integer)
            Dim Tally As Integer = BytesWritten + CurrentBytesWritten
            If GuiIsUsing And (Not Silent) Then
                GuiControl.SetSpeedInfoMemory(Format(Math.Round(Tally / (FlashWatch.ElapsedMilliseconds / 1000)), "#,###") & " Bytes/s")
            End If
        End Sub
        'Does the actual erase sector and program functions
        Private Sub WriteDataSub(ByRef sector As Integer, ByRef address As UInteger, ByVal Data() As Byte, ByVal Perc As Integer)
            Dim RetryCount As Integer = 0
            Dim ReadResult As Boolean
            Do
                If m_quit Then Exit Sub
                If IsEraseRequired() Then EraseSector(sector)
                If Not Silent Then RaiseEvent SetStatus(String.Format(RM.GetString("fcusb_memif_writeaddr"), "0x" & Hex(address), Format(Data.Length, "#,###"), Perc.ToString & "%"))
                FlashWatch.Start()
                WriteSector(sector, CType(Data.Clone, Byte()))
                FlashWatch.Stop()
                If m_quit Then Exit Sub
                If VerifyData Then 'Verify is enabled
                    If Not Silent Then RaiseEvent SetStatus(String.Format(RM.GetString("fcusb_memif_verify"), "0x" & Hex(address)))
                    ReadResult = VerifyDataSub(address, Data)
                    If ReadResult Then
                        RetryCount = 0
                        If Not Silent Then RaiseEvent SetStatus(RM.GetString("fcusb_memif_verifyokay"))
                        Application.DoEvents()
                    Else
                        RetryCount = RetryCount + 1
                        If RetryCount = 3 Then
                            If Not Silent Then RaiseEvent PrintConsole(String.Format(RM.GetString("fcusb_memif_verifyfailed"), "0x" & Hex(address)))
                            If Not Silent Then RaiseEvent SetStatus(String.Format(RM.GetString("fcusb_memif_verifyfailed"), "0x" & Hex(address)))
                            Exit Sub
                        End If
                        Sleep(500)
                    End If
                Else
                End If
            Loop Until ReadResult Or (Not VerifyData)
        End Sub

        Private Function VerifyDataSub(ByVal BaseAddress As UInteger, ByVal Data() As Byte) As Boolean
            Dim i As Integer
            Dim BlockSize As Integer = Data.Length
            Dim Verify() As Byte 'The data to check against
            Dim Miscount As Integer = 0
            Dim FirstWrongByte As Byte = Nothing
            Dim FirstWrongAddr As Integer = 0
            WaitUntilReady()
            Verify = ReadMemoryBlock(BaseAddress, BlockSize)
            If Verify Is Nothing OrElse (Not Verify.Length = Data.Length) Then
                RaiseEvent PrintConsole(RM.GetString("fcusb_memif_verifyerr1"))
                Return False
            End If
            For i = 0 To Data.Length - 1
                If Not Data(i) = Verify(i) Then
                    If Miscount = 0 Then
                        FirstWrongByte = Verify(i)
                        FirstWrongAddr = CInt(BaseAddress + i)
                    End If
                    Miscount = Miscount + 1
                End If
            Next
            If Miscount = 0 Then 'Verification successful
                Return True
            Else 'Error!
                Dim DataShouldBe As Byte = Data(CInt(FirstWrongAddr - BaseAddress))
                RaiseEvent PrintConsole(String.Format(RM.GetString("fcusb_memif_verifyerr2"), IntToHexStr(FirstWrongAddr), IntToHexStr(DataShouldBe), IntToHexStr(FirstWrongByte), Miscount))
                Return False 'Error!
            End If
        End Function

        Public Function ReadMemoryBlock(ByVal Address As UInt32, ByVal Count As Integer) As Byte()
            If DeviceType = DeviceTypes.Dram Then
                Return EJ.Memory_Read_Bulk(BaseAddress + Address, Count)
            ElseIf DeviceType = DeviceTypes.CFI Then
                Return CFIFlash.ReadData(Address, Count)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                Return EJ.SPI_ReadBulk(BaseAddress + Address, Count)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.SPI Then
                If Not SPI.FlashStatus = SPI_API.Status.Supported Then Return Nothing
                Return SPI.ReadData(Address, Count)
            ElseIf DeviceType = DeviceTypes.NAND Then
                Return NAND.ReadFlash(Address, Count)
            End If
            Return Nothing
        End Function

        Public Function WriteBytesBlock(ByVal Address As UInt32, ByVal data() As Byte) As Boolean
            If DeviceType = DeviceTypes.Dram Then
                EJ.Memory_Write_Bulk(Address, data)
            ElseIf DeviceType = DeviceTypes.CFI Then
                CFIFlash.WriteData(Address, data)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                EJ.SPI_WriteData(Address, data)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.SPI Then
                SPI.WriteData(Address, data)
            ElseIf DeviceType = DeviceTypes.NAND Then
                NAND.WriteFlash(Address, data)
            End If
            Return True
        End Function

        Private Sub WaitUntilReady()
            If DeviceType = DeviceTypes.CFI Then
                CFIFlash.WaitUntilReady()
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                EJ.SPI_Wait()
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.SPI Then
                SPI.WaitUntilReady()
            ElseIf DeviceType = DeviceTypes.NAND Then
                Sleep(100)
            End If
        End Sub

        Public Sub EraseSector(ByVal SectorNum As Integer)
            If DeviceType = DeviceTypes.CFI Then
                CFIFlash.EraseSector(SectorNum)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                EJ.SPI_EraseSector(SectorNum)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.SPI Then
                SPI.EraseSector(SectorNum)
            ElseIf DeviceType = DeviceTypes.NAND Then
                NAND.EraseSector(SectorNum)
            End If
        End Sub

        Public Function GetFlashSectors() As Integer
            If DeviceType = DeviceTypes.CFI Then
                Return CFIFlash.GetFlashSectors()
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                Return EJ.SPI_GetFlashSectors()
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.SPI Then
                Return SPI.GetFlashSectors()
            ElseIf DeviceType = DeviceTypes.NAND Then
                Return NAND.GetBlockCount()
            Else
                Return -1
            End If
        End Function

        Public Function IsEraseRequired() As Boolean
            If DeviceType = DeviceTypes.CFI Then
                Return CFIFlash.EraseRequired()
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                Return EJ.SPI_EraseRequired()
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.SPI Then
                Return SPI.EraseRequired()
            ElseIf DeviceType = DeviceTypes.NAND Then
                Return NAND.EraseRequired()
            Else
                Return False
            End If
        End Function

        Private Sub WriteSector(ByRef sector As Integer, ByVal Data() As Byte)
            FlashWatch.Start()
            If DeviceType = DeviceTypes.CFI Then
                CFIFlash.WriteSector(sector, Data)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                EJ.SPI_WriteSector(sector, Data)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.SPI Then
                SPI.WriteSector(sector, Data)
            ElseIf DeviceType = DeviceTypes.NAND Then
                NAND.WriteBlock(sector, Data)
            End If
            FlashWatch.Stop()
        End Sub

        Public Function FindSectorBase(ByVal sectorInt As Integer) As UInteger
            If DeviceType = DeviceTypes.CFI Then
                Return CFIFlash.FindSectorBase(sectorInt)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                Return EJ.SPI_FindSectorBase(sectorInt)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.SPI Then
                Return SPI.FindSectorBase(sectorInt)
            ElseIf DeviceType = DeviceTypes.NAND Then
                Return NAND.FindBlockBase(sectorInt)
            Else
                Return 0
            End If
        End Function

        Public Function GetSectorSize(ByVal sector As Integer) As Integer
            If DeviceType = DeviceTypes.CFI Then
                Return CFIFlash.GetSectorSize(sector)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                Return EJ.SPI_GetSectorSize()
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.SPI Then
                Return SPI.GetSectorSize() 'all sizes are the same size
            ElseIf DeviceType = DeviceTypes.NAND Then
                Return NAND.GetBlockSize() 'all sizes are the same size
            Else
                Return -1
            End If
        End Function

        Public Sub ReadMode()
            If DeviceType = DeviceTypes.CFI Then
                CFIFlash.Read_Mode()
            End If
        End Sub

        Public Function EraseBulk() As Boolean
            m_in_operation = True
            If DeviceType = DeviceTypes.Dram Then
                RaiseEvent PrintConsole(RM.GetString("fcusb_memif_dram_notsup"))
            ElseIf DeviceType = DeviceTypes.CFI Then
                CFIFlash.EraseBulk()
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                EJ.SPI_EraseBulk()
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.SPI Then
                SPI.BulkErase()
            ElseIf OperationMode = AvrMode.NAND Then
                NAND.BulkErase()
            End If
            GuiControl.UpdateEditor()
            m_in_operation = False
            Return True
        End Function
        'Reads a single byte from the memory device
        Public Function ReadByte(ByVal address As UInt32) As Byte
            If DeviceType = DeviceTypes.Dram Then
                Dim ReadAddr As UInteger = CUInt(Math.Floor(address / 4) * 4)   'JTAG reads only words correctly
                Dim ret As UInt32 = EJ.Memory_Read_W(ReadAddr)
                Dim d As UInt32 = CUInt((24 - ((address - ReadAddr) * 8)))
                Return CByte(((ret >> CInt(d)) And &HFF))
            ElseIf DeviceType = DeviceTypes.CFI Then
                Return CFIFlash.ReadByte(address)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.JTAG Then
                Dim d() As Byte = EJ.SPI_ReadBulk(address, 1)
                If Not d Is Nothing Then Return d(0)
            ElseIf DeviceType = DeviceTypes.SPI And OperationMode = AvrMode.SPI Then
                Dim d() As Byte = SPI.ReadData(address, 1)
                If Not d Is Nothing Then Return d(0)
            ElseIf OperationMode = AvrMode.NAND Then
                Dim d() As Byte = NAND.ReadFlash(address, 1)
                If Not d Is Nothing Then Return d(0)
            End If
            Return Nothing
        End Function
        'Updates a single byte (from the hex editor with enableediting = true)
        Private Sub onStreamUpdate(ByVal addr As UInt32, ByVal b As Byte) Handles GuiControl.StreamUpdate
            Select Case DeviceType
                Case DeviceTypes.Dram 'Only DRAM can update a single byte
                    EJ.Memory_Write_B(addr, b)
            End Select
        End Sub
        'Reads a lot of bytes from the stream for the hex editor
        Private Sub onReadStream(ByVal addr As UInt32, ByRef Data() As Byte) Handles GuiControl.ReadStream
            Dim DataIn() As Byte = Me.ReadMemoryBlock(addr, Data.Length)
            If DataIn Is Nothing Then 'Device is busy doing somethign else
                ReDim Data(Data.Length - 1) 'Fill array with Blank data to draw
            Else
                Data = DataIn
            End If
        End Sub

        Private Sub OnStatusSet(ByVal msg As String) Handles GuiControl.SetStatus
            RaiseEvent SetStatus(msg)
        End Sub

    End Class

End Module
