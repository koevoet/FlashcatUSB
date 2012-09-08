'COPYRIGHT EMBEDDEDCOMPUTERS.NET 2012 - ALL RIGHTS RESERVED
'CONTACT EMAIL: contact@embeddedcomputers.net
'ANY USE OF THIS CODE MUST ADHERE TO THE LICENSE FILE INCLUDED WITH THIS SDK
'INFO: This class interfaces the NAND over SPI functionality to the FlashcatUSB hardware/firmware
'ACKNOWLEDGEMENT: USB driver functionality provided by LibUsbDotNet (sourceforge.net/projects/libusbdotnet) 

Imports LibUsbDotNet
Imports LibUsbDotNet.Info
Imports LibUsbDotNet.Main

Public Class NAND_API
    Private ConfigFlashID As UInt32
    Private NandBlockCount As Integer

    Sub New()
        'FlashInit()


    End Sub

#Region "Public API"

    Public ReadOnly Property EraseRequired As Boolean
        Get
            Return False
        End Get
    End Property

    Public Function FlashInit() As Boolean
        DataDeinit()

        LedOff()
        ConfigFlashID = 0
        Dim s As UInt32 = GetStatus()
        ConfigFlashID = DataInit()
        If ConfigFlashID = 0 Then Return False
        Select Case ConfigFlashID
            Case &H1198010 'Org 16MB
                NandBlockCount = 1024
            Case &H23010 'Jasper 16MB
                NandBlockCount = 1024
            Case &HA3020 'Jasper 256MB
                NandBlockCount = 16384
            Case &HA8A3020 'Jasper 256MB (just added)
                NandBlockCount = 16384
            Case &HAA3020 ' Jasper 512MB
                NandBlockCount = 32768
            Case Else
                WriteConsole("Detect returned: 0x" & Hex(ConfigFlashID).PadLeft(8, "0"))
                Return False
        End Select
        WriteConsole("NAND Device detected, config: 0x" & Hex(ConfigFlashID).PadLeft(8, "0"))
        LedOn()
        Return True
    End Function

    Public Function ReadFlash(ByVal Address As Integer, ByVal Count As Integer) As Byte()
        Dim SectorSize As Integer = GetSectorSize()
        If Count <= SectorSize Then
            Return ReadFlash_BySector(Address, Count)
        Else
            Return ReadFlash_ByBlock(Address, Count)
        End If
    End Function

    Public Sub WriteFlash(ByVal Address As Integer, ByVal Data() As Byte)
        Dim SectorSize As Integer = GetSectorSize()
        Dim BlockSize As Integer = GetBlockSize()
        Dim StartSectorAddress As Integer = Math.Floor(Address \ SectorSize)
        Dim BytesLeft As Integer = Data.Length
        Dim calc As Single = CSng(BytesLeft) / CSng(BlockSize)
        Dim BlocksToWrite As Integer = Math.Ceiling(calc)
        For i = 0 To BlocksToWrite - 1
            Dim BytesWritting As Integer
            If BytesLeft > BlockSize Then BytesWritting = BlockSize Else BytesWritting = BytesLeft
            Dim DataOut(BytesWritting - 1) As Byte
            Array.Copy(Data, i * BlockSize, DataOut, 0, DataOut.Length)
            WriteFlashBlock(StartSectorAddress + (i * 32), DataOut)
            BytesLeft -= BytesWritting
        Next
    End Sub

    Public Sub WriteBlock(ByVal BlockNum As Integer, ByVal data() As Byte)
        Dim secNum As Integer = BlockNum * 32
        Dim Addr As UInteger = FindSectorBase(secNum)
        WriteFlash(Addr, data)
    End Sub

    Private Sub WriteSector(ByVal secNum As Integer, ByVal data() As Byte)
        Dim Addr As UInteger = FindSectorBase(secNum)
        WriteFlash(Addr, data)
    End Sub

    Public Function FindBlockBase(ByVal BlockNum As Integer) As UInteger
        Return CUInt(GetSectorSize() * (BlockNum * 32))
    End Function

    Private Function FindSectorBase(ByVal sectorInt As Integer) As UInteger
        Return CUInt(GetSectorSize() * sectorInt)
    End Function
    'Erases all of the data on the target device
    Public Sub BulkErase()
        Dim count As Integer = GetSectorCount()
        Dim blockcount As Integer = count
        For x = 0 To blockcount - 1
            EraseBlock(x)
        Next
    End Sub

    Public Sub EraseSector(ByVal SectorNum As Integer)
        EraseBlock(SectorNum \ 32)
    End Sub

    Public Sub EraseBlock(ByVal BlockNum As Integer)
        Dim RetCount As Integer = 0
        Dim SectAddr() As Byte = UintToBytes(BlockNum * 16896)
        Array.Reverse(SectAddr)
        Dim DirectionFlag As Byte = CByte(UsbCtrlFlags.RequestType_Vendor)
        Dim usbSetupPacket As New UsbSetupPacket(DirectionFlag, CByte(NandRequests.REQ_DATAERASE), 0, 0, 0)
        fcusb.ControlTransfer(usbSetupPacket, SectAddr, 0, RetCount)
        GetStatus()
    End Sub

    Public Function GetFlashSize() As Integer
        Return GetSectorCount() * GetSectorSize() 'Returns 17301504
    End Function

    Public Function GetFlashName() As String
        Return "NAND Flash (0x" & Hex(ConfigFlashID).PadLeft(8, "0") & ")"
    End Function

    Public Function GetBlockCount() As Integer
        Return NandBlockCount
    End Function

    Private Function GetSectorCount() As Integer
        Return GetBlockCount() * 32 'Total number of sectors
    End Function

    Private Function GetSectorSize() As Integer
        Return 528 'number of bytes in each sector
    End Function

    Public Function GetBlockSize() As Integer
        Return 16896 'Size of each block in bytes
    End Function

#End Region

    Enum NandRequests As Byte
        REQ_LED = &H20
        REQ_FIRMWARE = &H21
        REQ_ECHO = &H22
        REQ_DATAREAD = &H23
        REQ_DATAWRITE = &H24
        REQ_DATAINIT = &H25
        REQ_DATADEINIT = &H26
        REQ_DATASTATUS = &H27
        REQ_DATAERASE = &H28
        REQ_POWERUP = &H29
        REQ_SHUTDOWN = &H2A
    End Enum

    Private Function ReadFlash_BySector(ByVal Address As Integer, ByVal Count As Integer) As Byte()
        Dim SectorSize As Integer = GetSectorSize()
        Dim SectorStart As Integer = Math.Floor(Address \ SectorSize)
        Dim SectorEnd As Integer = Math.Floor((Address + (Count - 1)) \ SectorSize)
        Dim Offset As Integer = (Address - (SectorStart * SectorSize))
        Dim SectorsToRead As Integer = (SectorEnd - SectorStart) + 1
        Dim TotalReadBuff((SectorsToRead * SectorSize) - 1) As Byte
        For i = 0 To SectorsToRead - 1
            Dim b() As Byte = FlashReadBlock(SectorStart + i, SectorSize)
            If b Is Nothing Then Return Nothing 'Error
            Array.Copy(b, 0, TotalReadBuff, i * SectorSize, SectorSize)
        Next
        Dim RetOut(Count - 1) As Byte
        Array.Copy(TotalReadBuff, Offset, RetOut, 0, RetOut.Length)
        Return RetOut
    End Function

    Private Function ReadFlash_ByBlock(ByVal Address As Integer, ByVal Count As Integer) As Byte()
        Dim BlockSize As Integer = GetBlockSize()
        Dim BlockStart As Integer = Math.Floor(Address \ BlockSize)
        Dim BlockEnd As Integer = Math.Floor((Address + (Count - 1)) \ BlockSize)
        Dim Offset As Integer = (Address - (BlockStart * BlockSize))
        Dim BlocksToRead As Integer = (BlockEnd - BlockStart) + 1
        Dim TotalReadBuff((BlocksToRead * BlockSize) - 1) As Byte
        For i = 0 To BlocksToRead - 1
            Dim b() As Byte = FlashReadBlock((BlockStart + i) * 32, BlockSize)
            If b Is Nothing Then Return Nothing 'Error
            Array.Copy(b, 0, TotalReadBuff, i * BlockSize, BlockSize)
        Next
        Dim RetOut(Count - 1) As Byte
        Array.Copy(TotalReadBuff, Offset, RetOut, 0, RetOut.Length)
        Return RetOut
    End Function

    Private Function FlashReadBlock(ByVal Sector As UInteger, ByVal Count As UShort) As Byte()
        Dim DirectionFlag As Byte = CByte(UsbCtrlFlags.RequestType_Vendor)
        Dim wIndex As UShort = 0
        Dim usbSetupPacket As New UsbSetupPacket(DirectionFlag, CByte(NandRequests.REQ_DATAREAD), Count, wIndex, CShort(4))
        Dim RetCount As Integer = 0
        Dim SectAddr() As Byte = UintToBytes(Sector)
        Array.Reverse(SectAddr)
        If fcusb.ControlTransfer(usbSetupPacket, SectAddr, 4, RetCount) Then
            Dim Data() As Byte = ReadEndPoint(ReadEndpointID.Ep02, Count) 'ep2 = 130 0x82
            Dim res As UInt32 = GetStatus()
            Return Data
        End If
        Return Nothing
    End Function

    Private Function WriteFlashBlock(ByVal SectorNum As UInteger, ByVal Data() As Byte) As Boolean
        Dim DirectionFlag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor)
        Dim Count As UShort = Data.Length
        Dim wIndex As UShort = 0
        Dim usbSetupPacket As New UsbSetupPacket(DirectionFlag, CByte(NandRequests.REQ_DATAWRITE), Count, wIndex, CShort(4))
        Dim RetCount As Integer = 0
        Dim SectAddr() As Byte = UintToBytes(SectorNum)
        Array.Reverse(SectAddr)
        If fcusb.ControlTransfer(usbSetupPacket, SectAddr, 4, RetCount) Then
            Dim w As UsbEndpointWriter = fcusb.OpenEndpointWriter(WriteEndpointID.Ep01, EndpointType.Bulk)
            Dim tlen As Integer = 0
            Dim ec As ErrorCode = w.Write(Data, 0, Count, 1000, tlen)
            If ec <> ErrorCode.None Then
                Return False
            Else
                Return True
            End If
        Else
            Return False
        End If
    End Function

    Private Sub DataDeinit()
        Dim DirectionFlag As Byte = CByte(UsbCtrlFlags.RequestType_Vendor)
        Dim usbSetupPacket As New UsbSetupPacket(DirectionFlag, CByte(NandRequests.REQ_DATADEINIT), 0, 0, 0)
        Dim ret As Integer = 0
        fcusb.ControlTransfer(usbSetupPacket, 0, 0, ret)
    End Sub

    Private Function DataInit() As UInt32
        Dim DirectionFlag As Byte = CByte(UsbCtrlFlags.Direction_In Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor)
        Dim usbSetupPacket As New UsbSetupPacket(DirectionFlag, CByte(NandRequests.REQ_DATAINIT), 0, 0, 4)
        Dim ret As Integer = 0
        Dim buff(3) As Byte
        If fcusb.ControlTransfer(usbSetupPacket, buff, 4, ret) Then
            Array.Reverse(buff)
            Return BytesToUint32(buff)
        End If
        Return 0
    End Function

    Private Function GetStatus() As UInt16
        Dim DirectionFlag As Byte = CByte(UsbCtrlFlags.Direction_In Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor)
        Dim usbSetupPacket As New UsbSetupPacket(DirectionFlag, CByte(NandRequests.REQ_DATASTATUS), 0, 0, 2)
        Dim ret As Integer = 0
        Dim buff(1) As Byte
        If fcusb.ControlTransfer(usbSetupPacket, buff, 2, ret) Then
            Return (CUShort(buff(0)) << 8) + buff(1)
        Else
        End If
        Return &HFFFF
    End Function

    Private Function ReadStatus() As UInt32
        Dim DirectionFlag As Byte = CByte(UsbCtrlFlags.RequestType_Vendor)
        Dim usbSetupPacket As New UsbSetupPacket(DirectionFlag, CByte(NandRequests.REQ_DATASTATUS), 0, 0, 0)
        Dim ret As Integer = 0
        If fcusb.ControlTransfer(usbSetupPacket, 0, 0, ret) Then
            Dim Status() As Byte = ReadEndPoint(ReadEndpointID.Ep02, 4)
            If Status IsNot Nothing AndAlso Status.Length = 4 Then Return BytesToUint32(Status)
        Else
        End If
        Return 0
    End Function

    '#################################################
    '######        LIBUSBDOTNET CALLS
    '#################################################

#Region "Enums"

    Enum LED_STATE As Byte
        LED_STATE_OFF = &H0
        LED_STATE_ON = &H2
    End Enum

    Enum LED_BLINK As Byte
        LED_BLINK_OFF = &H0
        LED_BLINK_ON = &H1
    End Enum

#End Region

    Private fcusb As UsbDevice
    Private usbFinder As UsbDeviceFinder = New UsbDeviceFinder(&H16C0, &H5DF)

    Public Function IsConnected() As Boolean
        If UsbDevice.AllDevices.Find(usbFinder) Is Nothing Then Return False
        Return True
    End Function

    Public Function Connect() As Boolean
        If IsConnected() Then
            If OpenDevice() Then
                If EchoTest() Then
                    Return True
                Else
                    CloseDevice()
                End If
            End If
        End If
        Return False
    End Function

    Public Function OpenDevice() As Boolean
        CloseDevice()
        If Not IsConnected() Then Return False
        fcusb = UsbDevice.OpenUsbDevice(usbFinder)
        If fcusb IsNot Nothing Then
            Dim wholeUsbDevice As IUsbDevice = TryCast(fcusb, IUsbDevice)
            If wholeUsbDevice IsNot Nothing Then
                wholeUsbDevice.SetConfiguration(1)
                wholeUsbDevice.ClaimInterface(0)
            End If
            'SetDeviceConfig(SPI_CLOCK.SPI_CLOCK_FOSC_2, SPI_MODE.SPI_MODE_0)
            Return True
        End If
        Return False
    End Function

    Public Sub CloseDevice()
        If IsOpened() Then
            Dim wholeUsbDevice As IUsbDevice = TryCast(fcusb, IUsbDevice)
            If Not ReferenceEquals(wholeUsbDevice, Nothing) Then
                wholeUsbDevice.ReleaseInterface(0)
            End If
            fcusb.Close()
            fcusb = Nothing
        End If
    End Sub

    Public Function IsOpened() As Boolean
        If fcusb Is Nothing Then Return False
        If fcusb.IsOpen Then Return True
        Return False
    End Function

    Public Sub Disconnect()
        'FlashLoaded = False
        'MyFlashDevice = Nothing
        Application.DoEvents()
        'Add other disconnect stuff here
        CloseDevice()
    End Sub

    Public Sub LedOn()
        If Not IsOpened() Then Exit Sub
        Dim ledConf As Short = CShort(2) 'SOLID
        Dim DirectionFlag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor)
        Dim usbSetupPacket As New UsbSetupPacket(DirectionFlag, CByte(NandRequests.REQ_LED), ledConf, 0, 0)
        Dim ret As Integer
        fcusb.ControlTransfer(usbSetupPacket, Nothing, 0, ret)
    End Sub

    Public Sub LedOff()
        If Not IsOpened() Then Exit Sub
        Dim ledConf As Short = CShort(0) 'OFF
        Dim usbSetupPacket As New UsbSetupPacket(CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor), CByte(NandRequests.REQ_LED), ledConf, 0, 0)
        Dim ret As Integer
        fcusb.ControlTransfer(usbSetupPacket, Nothing, 0, ret)
    End Sub

    Public Sub LedBlink()
        If Not IsOpened() Then Exit Sub
        Dim ledConf As Short = CShort(1) 'Blink
        Dim usbSetupPacket As New UsbSetupPacket(CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor), CByte(NandRequests.REQ_LED), ledConf, 0, 0)
        Dim ret As Integer
        fcusb.ControlTransfer(usbSetupPacket, Nothing, 0, ret)
    End Sub

    Public Function GetFwVersion() As String
        If Not IsOpened() Then Return Nothing
        Dim usbSetupPacket As New UsbSetupPacket(CByte(UsbCtrlFlags.Direction_In Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor), CByte(NandRequests.REQ_FIRMWARE), 0, 0, 4)
        Dim ret As Integer
        Dim buff As Byte() = New Byte(4) {}
        If fcusb.ControlTransfer(usbSetupPacket, buff, 4, ret) Then
            buff(4) = buff(3)
            buff(3) = buff(2)
            buff(2) = Asc(".")
        End If
        Dim fwstr As String = BytesToString(buff)
        Return StrToSingle(fwstr).ToString
    End Function

    Public Function EchoTest() As Boolean
        If Not IsOpened() Then Return False 'Returns UsbDevice.IsOpen
        Dim buff As Byte() = New Byte(7) {}
        Dim DirFlag As Byte = CByte(UsbCtrlFlags.Direction_In Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor)
        Dim usbSetupPacket As New UsbSetupPacket(DirFlag, CByte(NandRequests.REQ_ECHO), &H1234, &H5678, 8)
        Dim ret As Integer
        If fcusb.ControlTransfer(usbSetupPacket, buff, 8, ret) Then
            If buff(0) <> CByte(UsbCtrlFlags.Direction_In Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor) Then
                Return False
            End If
            If buff(1) <> CByte(NandRequests.REQ_ECHO) Then Return False
            If buff(2) <> &H34 Then Return False
            If buff(3) <> &H12 Then Return False
            If buff(4) <> &H78 Then Return False
            If buff(5) <> &H56 Then Return False
            If buff(6) <> &H8 Then Return False
            If buff(7) <> &H0 Then Return False
            Return True
        End If
        Return False
    End Function

    Private Function ReadEndPoint(ByVal EPNUM As ReadEndpointID, ByVal ByteCount As Integer) As Byte()
        Dim buffer(ByteCount - 1) As Byte
        Dim TranCount As Integer = 0
        'Dim reader As UsbEndpointReader = fcusb.OpenEndpointReader(EPNUM, ByteCount, EndpointType.Bulk)
        Dim reader As UsbEndpointReader = fcusb.OpenEndpointReader(EPNUM)
        reader.Read(buffer, 0, ByteCount, 1000, TranCount)
        Return buffer
    End Function

    Private Function WriteEndPoint(ByVal EPNUM As WriteEndpointID, ByVal Data() As Byte) As Boolean
        Dim xferlen As Integer = 0
        Dim writer As UsbEndpointWriter = fcusb.OpenEndpointWriter(EPNUM)
        Dim ec As ErrorCode = writer.Write(Data, 0, Data.Length, 2000, xferlen)
        If ec <> ErrorCode.None Then Return False
        Return True
    End Function

    Private Function TargetPowerOn() As Boolean
        Dim DirectionFlag As Byte = CByte(UsbCtrlFlags.RequestType_Vendor)
        Dim usbSetupPacket As New UsbSetupPacket(DirectionFlag, CByte(NandRequests.REQ_POWERUP), 0, 0, 0)
        Dim ret As Integer = 0
        If fcusb.ControlTransfer(usbSetupPacket, 0, 0, ret) Then
            Return True
        Else
            Return False
        End If
    End Function

    Private Function TargetPowerOff() As Boolean
        Dim DirectionFlag As Byte = CByte(UsbCtrlFlags.RequestType_Vendor)
        Dim usbSetupPacket As New UsbSetupPacket(DirectionFlag, CByte(NandRequests.REQ_SHUTDOWN), 0, 0, 0)
        Dim ret As Integer = 0
        If fcusb.ControlTransfer(usbSetupPacket, 0, 0, ret) Then
            Return True
        Else
            Return False
        End If
    End Function

End Class
