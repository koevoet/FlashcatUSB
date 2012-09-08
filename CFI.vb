﻿Imports System.Threading

'COPYRIGHT EMBEDDEDCOMPUTERS.NET 2012 - ALL RIGHTS RESERVED
'CONTACT EMAIL: contact@embeddedcomputers.net
'ANY USE OF THIS CODE MUST ADHERE TO THE LICENSE FILE INCLUDED WITH THIS SDK
'INFO: This class interfaces the CFI flashes (over JTAG) via FlashcatUSB hardware/firmware

Public Class CFI

#Region "Sector / Addresses"
    Private Flash_BlockCount As UShort 'Number of blocks
    Private Flash_EraseBlock() As Integer 'Number of erase sectors per block
    Private Flash_EraseSize() As Integer 'Size of sectors per block
    Private Flash_Address() As Integer 'Addresses of all sectors
    Private Flash_Supported As Boolean 'Indicates that we support the device for writing

    Private Sub InitSectorAddresses()
        Dim i As Integer
        Dim AllSects() As Integer = GetAllSectors()
        Dim SectorInt As Integer = AllSects.Length
        Dim SecAdd As Integer = 0
        ReDim Flash_Address(SectorInt - 1)
        For i = 0 To SectorInt - 1
            Flash_Address(i) = SecAdd
            SecAdd += AllSects(i)
        Next
    End Sub
    'Returns the base address given the sector
    Public Function FindSectorBase(ByVal sector As Integer) As UInt32
        Try
            Return CUInt(Flash_Address(sector))
        Catch ex As Exception
            Return 0
        End Try
    End Function
    'Returns the sector that contains the offset address (verified)
    Public Function FindSectorOffset(ByVal Offset As Integer) As Integer
        Dim allSectors() As Integer = GetAllSectors()
        Dim i As Integer
        Dim MinAddress As Int32 = 0
        Dim MaxAddress As Int32
        For i = 0 To allSectors.Length - 1
            MaxAddress += allSectors(i) - 1
            If Offset >= MinAddress And Offset <= MaxAddress Then Return i 'Found it
            MinAddress = MaxAddress + 1
        Next
        Return -1 'Did not find it
    End Function
    'Returns the size (in bytes) of a sector
    Public Function GetSectorSize(ByVal Sector As Integer) As Integer
        Dim sectors() As Integer = GetAllSectors()
        If Sector > sectors.Length Then Return 0
        Return sectors(Sector)
    End Function
    'Returns all of the sectors (as their byte sizes)
    Private Function GetAllSectors() As Integer()
        Dim i As Integer
        Dim x As Integer
        Dim list As New ArrayList
        Dim numSectors As Integer
        For i = 0 To Flash_BlockCount - 1
            numSectors = Flash_EraseBlock(i)
            For x = 0 To numSectors - 1
                list.Add(Flash_EraseSize(i))
            Next
        Next
        Return DirectCast(list.ToArray(GetType(Integer)), Integer())
    End Function
    'Returns the total number of sectors
    Public Function GetFlashSectors() As Integer
        Dim i As Integer
        Dim TotalSectors As Integer = 0
        For i = 0 To Flash_BlockCount - 1
            TotalSectors += Flash_EraseBlock(i)
        Next
        Return TotalSectors
    End Function
    'Erases a sector on the flash device (byte mode only)
    Public Sub EraseSector(ByVal Sector As Integer)
        Monitor.Enter(EJ.ClientLock)
        Try
            Dim SA As Integer = CInt(FindSectorBase(Sector)) 'Sector Address
            If MyDeviceMode = DeviceAlgorithm.Intel Or MyDeviceMode = DeviceAlgorithm.Intel_Sharp Then
                write_command(SA, &H50) 'clear register
                write_command(SA, &H60) 'Unlock block (just in case)
                write_command(SA, &HD0) 'Confirm Command
                Threading.Thread.Sleep(50)
                write_command(SA, &H20)
                write_command(SA, &HD0)
                WaitUntilReady()
            ElseIf MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu Or MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu_Extended Then
                write_command(&HAAA, &HAA) 'AAA = 0xAA
                write_command(&H555, &H55) '555 = 0x55
                write_command(&HAAA, &H80) 'AAA = 0x80
                write_command(&HAAA, &HAA) 'AAA = 0xAA
                write_command(&H555, &H55) '555 = 0x55
                write_command(SA, &H30) 'SA  = 0x30
                write_command(0, &HF0) 'amd reset cmd
                amd_erasewait(SA)
            ElseIf MyDeviceMode = DeviceAlgorithm.SST Then
                write_command(&HAAAA, &HAA)
                write_command(&H5554, &H55)
                write_command(&HAAAA, &H80)
                write_command(&HAAAA, &HAA)
                write_command(&H5554, &H55)
                write_command(SA, &H30) 'SA  = 0x30
                amd_erasewait(SA)
            ElseIf MyDeviceMode = DeviceAlgorithm.AMD_NoBypass Then
                write_command(&HAAA, &HAA) 'AAA = 0xAA
                write_command(&H555, &H55) '555 = 0x55
                write_command(&HAAA, &H80) 'AAA = 0x80
                write_command(&HAAA, &HAA) 'AAA = 0xAA
                write_command(&H555, &H55) '555 = 0x55
                write_command(SA, &H30) 'SA  = 0x30
                write_command(0, &HF0) 'amd reset cmd
                amd_erasewait(SA)
            End If
        Finally
            Monitor.Exit(EJ.ClientLock)
        End Try
    End Sub
    'Writes data to a given sector and also swaps bytes (endian for words/halfwords)
    Public Sub WriteSector(ByVal Sector As Integer, ByVal data() As Byte)
        Dim Addr32 As UInteger = FindSectorBase(Sector)
        WriteData(Addr32, data)
    End Sub
    'Waits until a sector is blank (using the AMD read sector method)
    Private Sub amd_erasewait(ByVal SectorOffset As Integer, Optional AllowTimeout As Boolean = True)
        Try
            Sleep(500) 'Incase the data is already blank
            Dim Counter As UInt32 = 0
            Dim mydata As UInt32 = 0
            Do Until mydata = &HFFFFFFFFL
                Sleep(100)
                mydata = EJ.Memory_Read_W(CUInt(MyDeviceBase + SectorOffset))
                If AllowTimeout Then
                    Counter += 1
                    If Counter = 20 Then Exit Do
                End If
            Loop
        Catch ex As Exception
        End Try
    End Sub

#End Region

    Public Event PrintConsole(ByVal Msg As String) 'Prints device information to console

    Public UseBulkRead As Boolean = True  'Set to false to manually read each word

    Private FlashDatabase As New ArrayList 'Contains the flash device database
    Private MyDeviceMode As DeviceAlgorithm
    Private MyDeviceBus As DeviceBus
    Private MyDeviceInterface As DeviceInterface 'Loaded via CFI
    Private MyDeviceID As ChipID
    Private MyDeviceSize As Integer 'Size in number of bytes
    Private MyDeviceName As String 'Contains the ascii name of the flash IC
    Private MyDeviceBase As UInt32 'Address of the device

    Sub New()
        AddDeviceDefinition("Spansion S29GL256M", &H1, &H7E1201)
        AddDeviceDefinition("Spansion S29GL128M", &H1, &H7E1200)
        AddDeviceDefinition("Spansion S29GL064M", &H1, &H7E1300)
        AddDeviceDefinition("Spansion S29GL064M", &H1, &H7E0C01)
        AddDeviceDefinition("Spansion S29GL064M", &H1, &H7E1000) 'Bottom boot
        AddDeviceDefinition("Spansion S29GL064M", &H1, &H7E1001) 'Top boot
        AddDeviceDefinition("Spansion S29GL064M", &H1, &H7E1301)
        AddDeviceDefinition("Spansion S29GL032M", &H1, &H7E1C00)
        AddDeviceDefinition("Spansion S29GL032M", &H1, &H7E1D00)
        AddDeviceDefinition("Spansion S29GL032M", &H1, &H7E1A00) 'Bottom boot
        AddDeviceDefinition("Spansion S29GL032M", &H1, &H7E1A01) 'Top Boot
        AddDeviceDefinition("Spansion S70GL02G", &H1, &H7E4801)
        AddDeviceDefinition("Spansion S29GL01G", &H1, &H7E2801)
        AddDeviceDefinition("Spansion S29GL512", &H1, &H7E2301)
        AddDeviceDefinition("Spansion S29GL256", &H1, &H7E2201)
        AddDeviceDefinition("Spansion S29GL128", &H1, &H7E2101)
        AddDeviceDefinition("AMD 28F400BT", &H1, &H2223)
        AddDeviceDefinition("AMD 29DL322GB", &H1, &H2256)
        AddDeviceDefinition("AMD 29DL322GT", &H1, &H2255)
        AddDeviceDefinition("AMD 29DL323GB", &H1, &H2253)
        AddDeviceDefinition("AMD 29DL323GT", &H1, &H2250)
        AddDeviceDefinition("AMD 29DL324GB", &H1, &H225F)
        AddDeviceDefinition("AMD 29DL324GT", &H1, &H225C)
        AddDeviceDefinition("AMD 29LV160DB", &H1, &H2249)
        AddDeviceDefinition("AMD 29LV160DT", &H1, &H22C4)
        AddDeviceDefinition("AMD 29LV320DB", &H1, &H22F9)
        AddDeviceDefinition("AMD 29LV320DT", &H1, &H22F6)
        AddDeviceDefinition("AMD 29LV320MB", &H1, &H2200)
        AddDeviceDefinition("AMD 29LV320MT", &H1, &H2201)
        AddDeviceDefinition("AMD 29LV400BB", &H1, &H22BA)
        AddDeviceDefinition("AMD 29LV800BB", &H1, &H225B)
        AddDeviceDefinition("ATMEL AT49BV/LV16X", &H1F, &HC0)
        AddDeviceDefinition("ATMEL AT49BV/LV16XT", &H1F, &HC2)
        AddDeviceDefinition("HYHYNIX HY29F400TT", &HAD, &H2223)
        AddDeviceDefinition("HYHYNIX HY29LV1600T", &HAD, &H22C4)
        AddDeviceDefinition("Intel 28F160B3", &H89, &H8891)
        AddDeviceDefinition("Intel 28F160B3", &H89, &H8890)
        AddDeviceDefinition("Intel 28F800B3", &H89, &H8893)
        AddDeviceDefinition("Intel 28F320B3", &H89, &H8896)
        AddDeviceDefinition("Intel 28F320B3", &H89, &H8897)
        AddDeviceDefinition("Intel 28F640B3", &H89, &H8898)
        AddDeviceDefinition("Intel 28F640B3", &H89, &H8899)
        AddDeviceDefinition("Intel TE28F800C3T", &H89, &H88C0)
        AddDeviceDefinition("Intel TE28F800C3B", &H89, &H88C1)
        AddDeviceDefinition("Intel TE28F160C3T", &H89, &H88C2)
        AddDeviceDefinition("Intel TE28F160C3B", &H89, &H88C3)
        AddDeviceDefinition("Intel TE28F320C3T", &H89, &H88C4)
        AddDeviceDefinition("Intel TE28F320C3B", &H89, &H88C5)
        AddDeviceDefinition("Intel TE28F640C3T", &H89, &H88CC)
        AddDeviceDefinition("Intel TE28F640C3B", &H89, &H88CD)
        AddDeviceDefinition("Intel 28F320J5", &H89, &H14)
        AddDeviceDefinition("Intel 28F640J5", &H89, &H15)
        AddDeviceDefinition("Intel 28F320J3", &H89, &H16)
        AddDeviceDefinition("Intel 28F640J3", &H89, &H17)
        AddDeviceDefinition("Intel 28F128J3", &H89, &H18)
        AddDeviceDefinition("Samsung K8D1716UB", &HEC, &H2277)
        AddDeviceDefinition("Samsung K8D1716UT", &HEC, &H2275)
        AddDeviceDefinition("Samsung K8D3216UB", &HEC, &H22A2)
        AddDeviceDefinition("Samsung K8D3216UT", &HEC, &H22A0)
        AddDeviceDefinition("ST M28W160CB", &H20, &H88CF)
        AddDeviceDefinition("ST M29D323DB", &H20, &H225F)
        AddDeviceDefinition("FUJITSU 29DL323GB", &H4, &H2253)
        AddDeviceDefinition("FUJITSU 29DL323TE", &H4, &H225C)
        AddDeviceDefinition("FUJITSU 29LV160B", &H4, &H2249)
        AddDeviceDefinition("FUJITSU 29LV160T", &H4, &H22C4)
        AddDeviceDefinition("FUJITSU 29LV320BE", &H4, &H22F9)
        AddDeviceDefinition("FUJITSU 29LV320TE", &H4, &H22F6)
        AddDeviceDefinition("FUJITSU 29LV800B", &H4, &H225B)
        AddDeviceDefinition("Micron 28F160C34B", &H2C, &H4493)
        AddDeviceDefinition("Micron 28F160C34T", &H2C, &H4492)
        AddDeviceDefinition("Micron 28F322P3", &H2C, &H4495)
        AddDeviceDefinition("MXIC 25FL0165A", &HC2, &H20)
        AddDeviceDefinition("MXIC 29LV800T", &HC2, &H22DA) 'NON-CFI
        AddDeviceDefinition("MXIC 29LV800B", &HC2, &H225B) 'NON-CFI
        AddDeviceDefinition("MXIC 29LV161T", &HC2, &H22C4) 'NON-CFI
        AddDeviceDefinition("MXIC 29LV161B", &HC2, &H2249) 'NON-CFI
        AddDeviceDefinition("MXIC 29LV320B", &HC2, &HA8)
        AddDeviceDefinition("MXIC 29LV320B", &HC2, &H22A8)
        AddDeviceDefinition("MXIC 29LV320T", &HC2, &H22A7)
        AddDeviceDefinition("MXIC 29LV320T", &HC2, &HA7)
        AddDeviceDefinition("MXIC 29LV800BMC", &HC2, &H225B)
        AddDeviceDefinition("SHARP 28F320BJE", &HB0, &HE3)
        AddDeviceDefinition("SHARP LH28F160BJHG", &HB0, &HE9)
        AddDeviceDefinition("SHARP 28F160S3", &HB0, &HD0)
        AddDeviceDefinition("SHARP 28F320S3", &HB0, &HD4)
        AddDeviceDefinition("SST 39VF1600", &HBF, &H2782)
        AddDeviceDefinition("SST 39VF1601", &HBF, &H234B)
        AddDeviceDefinition("SST 39VF3201", &HBF, &H235B)
        AddDeviceDefinition("SST 39VF800", &HBF, &H2781)
        AddDeviceDefinition("ST MT28W320", &H20, &H88BB)
        AddDeviceDefinition("ST MT28W320", &H20, &H88BC)
        AddDeviceDefinition("ST 29W320DB", &H20, &H22CB)
        AddDeviceDefinition("ST 29W320DT", &H20, &H22CA)
        AddDeviceDefinition("ST M29W160EB", &H20, &H2249)
        AddDeviceDefinition("ST M29W160ET", &H20, &H22C4)
        AddDeviceDefinition("ST M58LW064D", &H20, &H17)
        AddDeviceDefinition("ST M29W800AB", &H20, &H5B)
        AddDeviceDefinition("TOSHIBA TC58FVB160", &H98, &H43)
        AddDeviceDefinition("TOSHIBA TC58FVB321", &H98, &H9C)
        AddDeviceDefinition("TOSHIBA TC58FVT160", &H98, &HC2)
        AddDeviceDefinition("TOSHIBA TC58FVT160B", &H98, &H43)
        AddDeviceDefinition("TOSHIBA TC58FVT321", &H98, &H9A)
    End Sub
    'Returns true if the flash device is detected
    Public Function DetectFlash(ByVal BaseAddress As UInt32) As Boolean
        Flash_Supported = False
        MyDeviceBase = BaseAddress
        Read_Mode()
        If Enable_CFI_Mode(DeviceBus.X8) OrElse Enable_CFI_Mode(DeviceBus.X16) OrElse Enable_CFI_Mode(DeviceBus.X32) Then
            Load_CFI_Data()
        ElseIf Enable_CFI_Mode_ForSST() Then
            Load_CFI_Data()
        End If
        Read_Mode() 'Puts the flash back into read mode
        If Enable_JEDEC_Mode() Then
            MyDeviceID = New ChipID With {.ManufactureID = 0, .PartNumber = 0}
            If MyDeviceMode = DeviceAlgorithm.NotDefined Then 'Possible non-cfi device
                Dim FirstWord As UInt32 = EJ.Memory_Read_W(MyDeviceBase)
                FirstWord = EJ.Memory_Read_W(MyDeviceBase) 'Read this twice for some unknown reason
                MyDeviceID.ManufactureID = CByte(FirstWord And &HFF)
                MyDeviceID.PartNumber = (FirstWord And &HFFFF0000) >> 16
                If Not Detect_NonCFI_Device(MyDeviceID.ManufactureID, MyDeviceID.PartNumber) Then Return False
                EJ.SetParameter(5, BaseAddress) 'Legacy devices require flash commands, this sets the flash base address
            Else
                MyDeviceID.ManufactureID = CByte(EJ.Memory_Read_H(MyDeviceBase) And &HFF)
                If MyDeviceMode = DeviceAlgorithm.Intel Or MyDeviceMode = DeviceAlgorithm.Intel_Sharp Then
                    MyDeviceID.PartNumber = EJ.Memory_Read_H(CUInt(MyDeviceBase + &H2))
                ElseIf MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu Or MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu_Extended Then
                    MyDeviceID.PartNumber = EJ.Memory_Read_H(CUInt(MyDeviceBase + &H22))
                ElseIf MyDeviceMode = DeviceAlgorithm.SST Then
                    MyDeviceID.PartNumber = EJ.Memory_Read_H(CUInt(MyDeviceBase + &H2))
                    EJ.SetParameter(5, BaseAddress) 'SST flashes require flash commands, this sets the flash base address
                End If
                If MyDeviceID.ManufactureID = 1 And MyDeviceID.PartNumber = &H227E Then 'Updates the full PartNumber for SPANSION devices
                    Dim cycle_one As Byte = CByte(&HFF And EJ.Memory_Read_H(CUInt(MyDeviceBase + +&H2)))
                    Dim cycle_two As Byte = CByte(&HFF And EJ.Memory_Read_H(CUInt(MyDeviceBase + +&H1C)))
                    Dim cycle_thr As Byte = CByte(&HFF And EJ.Memory_Read_H(CUInt(MyDeviceBase + +&H1E)))
                    MyDeviceID.PartNumber = (CUInt(cycle_one) << 16) + (CUInt(cycle_two) << 8) + CUInt(cycle_thr)
                End If
                Read_Mode() 'Puts the flash back into read mode
                Dim BaseStr As String = "0x" & Hex(BaseAddress).PadLeft(8, "0")
                RaiseEvent PrintConsole(String.Format(RM.GetString("fcusb_cfi_founddev"), BaseStr)) '"Loaded CFI compatible flash device at: {0}"
            End If
            LoadFlashName()
            RaiseEvent PrintConsole(String.Format(RM.GetString("fcusb_cfi_jedecid"), MyDeviceID.ToString))
            RaiseEvent PrintConsole(String.Format(RM.GetString("fcusb_cfi_base"), "0x" & Hex(MyDeviceBase).PadLeft(8, "0")))
            RaiseEvent PrintConsole(String.Format(RM.GetString("fcusb_cfi_desc"), CStr(MyDeviceName & " (" & Format(MyDeviceSize, "#,###") & " bytes)")))
            PrintProgrammingMode() '"Programming mode: etc"
        Else
            RaiseEvent PrintConsole(RM.GetString("fcusb_cfi_err1"))
            Return False
        End If
        Return True
    End Function

    Private Function Detect_NonCFI_Device(ByVal ManufactureID As Byte, ByVal PartNumber As UShort) As Boolean
        If ManufactureID = &HC2 And PartNumber = &H22C4 Then 'MX29LV161T
            MyDeviceSize = &H200000
            Flash_BlockCount = 4
            ReDim Flash_EraseBlock(Flash_BlockCount - 1)
            ReDim Flash_EraseSize(Flash_BlockCount - 1)
            Flash_EraseSize(0) = &H10000 '64KB
            Flash_EraseBlock(0) = 31
            Flash_EraseSize(1) = &H8000 '32KB
            Flash_EraseBlock(1) = 1
            Flash_EraseSize(2) = &H2000 '8KB
            Flash_EraseBlock(2) = 2
            Flash_EraseSize(3) = &H4000 '16KB
            Flash_EraseBlock(3) = 1
            InitSectorAddresses()
            MyDeviceMode = DeviceAlgorithm.AMD_NoBypass
        ElseIf ManufactureID = &HC2 And PartNumber = &H2249 Then 'MX29LV161B
            MyDeviceSize = &H200000
            Flash_BlockCount = 4
            ReDim Flash_EraseBlock(Flash_BlockCount - 1)
            ReDim Flash_EraseSize(Flash_BlockCount - 1)
            Flash_EraseSize(0) = &H4000 '16KB
            Flash_EraseBlock(0) = 1
            Flash_EraseSize(1) = &H2000 '8KB
            Flash_EraseBlock(1) = 2
            Flash_EraseSize(2) = &H8000 '32KB
            Flash_EraseBlock(2) = 1
            Flash_EraseSize(3) = &H10000 '64KB
            Flash_EraseBlock(3) = 31
            InitSectorAddresses()
            MyDeviceMode = DeviceAlgorithm.AMD_NoBypass
        ElseIf ManufactureID = &HC2 And PartNumber = &H22DA Then 'MXIC 29LV800T
            MyDeviceSize = &H100000
            Flash_BlockCount = 4
            ReDim Flash_EraseBlock(Flash_BlockCount - 1)
            ReDim Flash_EraseSize(Flash_BlockCount - 1)
            Flash_EraseSize(0) = &H10000 '64KB
            Flash_EraseBlock(0) = 15
            Flash_EraseSize(1) = &H8000 '32KB
            Flash_EraseBlock(1) = 1
            Flash_EraseSize(2) = &H2000 '8KB
            Flash_EraseBlock(2) = 2
            Flash_EraseSize(3) = &H4000 '16KB
            Flash_EraseBlock(3) = 1
            InitSectorAddresses()
            MyDeviceMode = DeviceAlgorithm.AMD_NoBypass
        ElseIf ManufactureID = &HC2 And PartNumber = &H22DA Then 'MXIC 29LV800B
            MyDeviceSize = &H100000
            Flash_BlockCount = 4
            ReDim Flash_EraseBlock(Flash_BlockCount - 1)
            ReDim Flash_EraseSize(Flash_BlockCount - 1)
            Flash_EraseSize(0) = &H4000 '16KB
            Flash_EraseBlock(0) = 1
            Flash_EraseSize(1) = &H2000 '8KB
            Flash_EraseBlock(1) = 2
            Flash_EraseSize(2) = &H8000 '32KB
            Flash_EraseBlock(2) = 1
            Flash_EraseSize(3) = &H10000 '64KB
            Flash_EraseBlock(3) = 15
            InitSectorAddresses()
            MyDeviceMode = DeviceAlgorithm.AMD_NoBypass
        Else
            Read_Mode()
            Return False
        End If
        Read_Mode()
        Return True
    End Function

    Public ReadOnly Property FlashName() As String
        Get
            Return MyDeviceName
        End Get
    End Property

    Public ReadOnly Property FlashSize() As Integer
        Get
            Return MyDeviceSize
        End Get
    End Property

    Private Structure DeviceDefinition
        Dim Name As String 'Combination of Manu and part ID
        Dim Manufacturer As Byte
        Dim PartID As UInt32
    End Structure

    Public Structure ChipID
        Dim ManufactureID As Byte
        Dim PartNumber As UInt32 'Contains 2 or 3 bytes for the part number
        Public Function IsValid() As Boolean
            If ManufactureID = 0 Then Return False
            If ManufactureID = 255 Then Return False
            If PartNumber = 0 Then Return False
            If PartNumber = &HFFFFFFFF Then Return False
            Return True
        End Function
        Public Overrides Function ToString() As String
            Return Hex(ManufactureID).PadLeft(2, CChar("0")) & " " & Hex(PartNumber).PadLeft(4, CChar("0"))
        End Function
    End Structure
    'The device bus width used to accept commands
    Public Enum DeviceBus
        X8 = 0
        X16 = 1
        X32 = 2
    End Enum
    'The device specific programming / algorithm (set by CFI, 0x26+0x28)
    Public Enum DeviceAlgorithm As UShort
        NotDefined = 0
        Intel_Sharp = &H100
        SST = &H107
        AMD_Fujitsu = &H200
        Intel = &H300
        AMD_Fujitsu_Extended = &H400
        AMD_NoBypass = &H1001 'We created/specified this mode type
    End Enum

    Public Enum DeviceInterface
        x8_only = 0
        x16_only = 1
        x8_and_x16 = 2 'via BYTE#
        x32 = 3
    End Enum
    'Required by our general memory device API
    Public ReadOnly Property EraseRequired As Boolean
        Get
            Return True
        End Get
    End Property
    'If our device can be programmed by this code
    Public ReadOnly Property WriteAllowed() As Boolean
        Get
            Return Flash_Supported
        End Get
    End Property

    Public Sub AddDeviceDefinition(ByVal FlashName As String, ByVal ManuID As Byte, ByVal PartID As UInt32)
        Dim nDevice As New DeviceDefinition
        nDevice.Name = FlashName
        nDevice.Manufacturer = ManuID
        nDevice.PartID = PartID
        For i = 0 To FlashDatabase.Count - 1
            Dim dummy As DeviceDefinition = DirectCast(FlashDatabase.Item(i), DeviceDefinition)
            If (dummy.Manufacturer = ManuID) And (dummy.PartID = PartID) Then
                RaiseEvent PrintConsole(String.Format(RM.GetString("fcusb_cfi_info1"), FlashName)) 'Changing parameter for existing part: {0}
                FlashDatabase(i) = nDevice
                Exit Sub
            End If
        Next
        FlashDatabase.Add(nDevice) 'Adding new entry
    End Sub
    'Loads the device name (if we have it in our database)
    Private Sub LoadFlashName()
        MyDeviceName = RM.GetString("fcusb_cfi_unknown")
        For i = 0 To FlashDatabase.Count - 1
            Dim dev_def As DeviceDefinition = DirectCast(FlashDatabase.Item(i), DeviceDefinition)
            If dev_def.Manufacturer = MyDeviceID.ManufactureID Then
                If dev_def.PartID = MyDeviceID.PartNumber Then
                    MyDeviceName = dev_def.Name
                    Exit Sub
                End If
            End If
        Next
        MyDeviceName = GetDeviceManufacture(MyDeviceID.ManufactureID) & "_(0x" & Hex(MyDeviceID.ManufactureID) & ")"
    End Sub

    Private Sub PrintProgrammingMode()
        Dim BusWidthString As String = ""
        Dim AlgStr As String = ""
        Dim InterfaceStr As String = ""
        Select Case MyDeviceBus
            Case DeviceBus.X8
                BusWidthString = String.Format(RM.GetString("fcusb_cfi_bitbus"), 8)
            Case DeviceBus.X16
                BusWidthString = String.Format(RM.GetString("fcusb_cfi_bitbus"), 16)
            Case DeviceBus.X32
                BusWidthString = String.Format(RM.GetString("fcusb_cfi_bitbus"), 32)
            Case Else
                Exit Sub
        End Select
        Select Case MyDeviceMode
            Case DeviceAlgorithm.AMD_Fujitsu
                AlgStr = "AMD/Fujitsu"
            Case DeviceAlgorithm.AMD_Fujitsu_Extended
                AlgStr = "AMD/Fujitsu (extended)"
            Case DeviceAlgorithm.Intel
                AlgStr = "Intel"
            Case DeviceAlgorithm.Intel_Sharp
                AlgStr = "Intel/Sharp"
            Case DeviceAlgorithm.SST
                AlgStr = "SST"
            Case Else
                Exit Sub
        End Select
        Select Case MyDeviceInterface
            Case DeviceInterface.x8_only
                InterfaceStr = "x8 " & RM.GetString("fcusb_cfi_interface")
            Case DeviceInterface.x8_and_x16
                InterfaceStr = "x8/x16 " & RM.GetString("fcusb_cfi_interface")
            Case DeviceInterface.x16_only
                InterfaceStr = "x16 " & RM.GetString("fcusb_cfi_interface")
            Case DeviceInterface.x32
                InterfaceStr = "x32 " & RM.GetString("fcusb_cfi_interface")
        End Select
        RaiseEvent PrintConsole(RM.GetString("fcusb_cfi_pmode") & ": " & AlgStr & " " & InterfaceStr & " " & BusWidthString)
    End Sub

    Private Sub write_command(ByVal offset As UInt32, ByVal data As UInt32)
        Select Case MyDeviceBus
            Case DeviceBus.X8
                EJ.Memory_Write_B(CUInt(MyDeviceBase + offset), data)
            Case DeviceBus.X16
                EJ.Memory_Write_H(CUInt(MyDeviceBase + offset), data)
            Case DeviceBus.X32
                EJ.Memory_Write_W(CUInt(MyDeviceBase + offset), data)
        End Select
    End Sub
    'Attempts to put the device into CFI mode
    Private Function Enable_CFI_Mode(ByVal BusMode As DeviceBus) As Boolean
        Select Case BusMode
            Case DeviceBus.X8
                EJ.Memory_Write_B(CUInt(MyDeviceBase + &HAA), &H98) 'CFI Mode Command
            Case DeviceBus.X16
                EJ.Memory_Write_H(CUInt(MyDeviceBase + &HAA), &H98) 'CFI Mode Command
            Case DeviceBus.X32
                EJ.Memory_Write_W(CUInt(MyDeviceBase + &HAA), &H98) 'CFI Mode Command 
        End Select
        Sleep(50) 'If the command succeded, we need to wait for the device to switch modes
        Dim ReadBack As UInt32 = CUInt(EJ.Memory_Read_H(MyDeviceBase + &H20UI))
        ReadBack = CUInt((ReadBack << 8) + EJ.Memory_Read_H(MyDeviceBase + &H22UI))
        ReadBack = CUInt((ReadBack << 8) + EJ.Memory_Read_H(MyDeviceBase + &H24UI))
        If ReadBack = &H515259 Then '"QRY"
            'Flash Device Interface description (refer to CFI publication 100)
            MyDeviceBus = BusMode
            Return True
        End If
        Read_Mode()
        Return False
    End Function
    'Attempts to put the device into JEDEC mode
    'Attempts to put the device into JEDEC mode
    Private Function Enable_JEDEC_Mode() As Boolean
        If MyDeviceMode = DeviceAlgorithm.NotDefined Then
            write_command(&HAAA, &HAA)
            write_command(&H555, &H55)
            write_command(&HAAA, &H90)
        ElseIf MyDeviceMode = DeviceAlgorithm.Intel Or MyDeviceMode = DeviceAlgorithm.Intel_Sharp Then
            'write_command(&H555, &H90)
            write_command(0, &H90)
        ElseIf MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu Or MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu_Extended Then
            write_command(&HAAA, &HAA)
            write_command(&H555, &H55)
            write_command(&HAAA, &H90)
        ElseIf MyDeviceMode = DeviceAlgorithm.AMD_NoBypass Then
            write_command(&HAAA, &HAA)
            write_command(&H555, &H55)
            write_command(&HAAA, &H90)
        ElseIf MyDeviceMode = DeviceAlgorithm.SST Then
            write_command(&HAAAA, &HAA)
            write_command(&H5554, &H55)
            write_command(&HAAAA, &H90)
        Else
            Return False
        End If
        Sleep(50)
        Return True
    End Function
    'Puts the device back into READ mode
    Private Function Enable_CFI_Mode_ForSST()
        EJ.Memory_Write_B(MyDeviceBase + &HAAAA, &HAA)
        EJ.Memory_Write_B(MyDeviceBase + &H5554, &H55)
        EJ.Memory_Write_B(MyDeviceBase + &HAAAA, &H98)
        Sleep(50) 'If the command succeeded, we need to wait for the device to switch modes
        Dim ReadBack As UInt32 = CUInt(EJ.Memory_Read_H(MyDeviceBase + &H20UI))
        ReadBack = CUInt((ReadBack << 8) + EJ.Memory_Read_H(MyDeviceBase + &H22UI))
        ReadBack = CUInt((ReadBack << 8) + EJ.Memory_Read_H(MyDeviceBase + &H24UI))
        If ReadBack = &H515259 Then '"QRY"
            MyDeviceBus = DeviceBus.X8 'Flash Device Interface description (refer to CFI publication 100)
            Return True
        End If
        Read_Mode()
        Return False
    End Function

    Private Sub Load_CFI_Data()
        MyDeviceSize = CInt(2 ^ EJ.Memory_Read_H(CUInt(MyDeviceBase + &H4E)))
        Dim DeviceCommandSet As UShort = CUShort(&HFF And EJ.Memory_Read_H(CUInt(MyDeviceBase + &H26))) << 8
        DeviceCommandSet += CByte(&HFF And EJ.Memory_Read_H(CUInt(MyDeviceBase + &H28)))
        MyDeviceMode = DeviceCommandSet
        MyDeviceInterface = CInt(EJ.Memory_Read_H(CUInt(MyDeviceBase + &H50)))
        Flash_BlockCount = EJ.Memory_Read_H(CUInt(MyDeviceBase + &H58))
        Dim BootFlag As UInt32 = EJ.Memory_Read_H(CUInt(MyDeviceBase + &H9E))
        ReDim Flash_EraseBlock(Flash_BlockCount - 1)
        ReDim Flash_EraseSize(Flash_BlockCount - 1)
        Dim BlockAddress As UInt32 = &H5A 'Start address of block 1 information
        For i = 1 To Flash_BlockCount
            Flash_EraseBlock(i - 1) = ((EJ.Memory_Read_H(CUInt(MyDeviceBase + BlockAddress + 2)) << 8) + EJ.Memory_Read_H(CUInt(MyDeviceBase + BlockAddress))) + 1
            Flash_EraseSize(i - 1) = ((EJ.Memory_Read_H(CUInt(MyDeviceBase + BlockAddress + 6)) << 8) + EJ.Memory_Read_H(CUInt(MyDeviceBase + BlockAddress + 4))) * 256
            BlockAddress += 8 'Increase address by 8
        Next
        If BootFlag = 3 Then 'warning: might only be designed for TC58FVT160
            Array.Reverse(Flash_EraseBlock)
            Array.Reverse(Flash_EraseSize)
        End If
        InitSectorAddresses() 'Creates the map of the addresses of all sectors
    End Sub

    Public Sub Read_Mode()
        If MyDeviceMode = DeviceAlgorithm.NotDefined Then
            EJ.Memory_Write_B(MyDeviceBase, &HFF) 'For Intel / Sharp
            EJ.Memory_Write_B(MyDeviceBase, &H50)
            EJ.Memory_Write_B(MyDeviceBase + &HAAA, &HAA) 'For AMD
            EJ.Memory_Write_B(MyDeviceBase + &H555, &H55)
            EJ.Memory_Write_B(MyDeviceBase + &HAAAA, &HAA) 'For SST
            EJ.Memory_Write_B(MyDeviceBase + &H5554, &H55)
            EJ.Memory_Write_B(MyDeviceBase + &HAAAA, &HF0)
            EJ.Memory_Write_B(MyDeviceBase, &HF0) 'For LEGACY
        ElseIf MyDeviceMode = DeviceAlgorithm.Intel Or MyDeviceMode = DeviceAlgorithm.Intel_Sharp Then
            write_command(0, &HFF)
            write_command(0, &H50)
        ElseIf MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu Or MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu_Extended Then
            write_command(&HAAA, &HAA)
            write_command(&H555, &H55)
            write_command(0, &HF0)
        ElseIf MyDeviceMode = DeviceAlgorithm.AMD_NoBypass Then
            write_command(0, &HF0)
        ElseIf MyDeviceMode = DeviceAlgorithm.SST Then
            write_command(&HAAAA, &HAA)
            write_command(&H5554, &H55)
            write_command(&HAAAA, &HF0)
        End If
        Sleep(50)
    End Sub

    Public Sub WaitUntilReady()
        Dim counter As Integer = 0
        Dim sr As UShort
        If MyDeviceMode = DeviceAlgorithm.Intel Or MyDeviceMode = DeviceAlgorithm.Intel_Sharp Then
            Do
                If counter = 100 Then Exit Sub
                counter += 1
                Sleep(25)
                write_command(0, &H70) 'READ SW
                sr = EJ.Memory_Read_H(MyDeviceBase)
            Loop While (Not ((sr >> 7) = 1))
            Read_Mode()
        ElseIf MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu Or MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu_Extended Then
            Sleep(100)
        ElseIf MyDeviceMode = DeviceAlgorithm.SST Then
            Sleep(100)
        End If
    End Sub
    'Erases all blocks on the CFI device
    Public Function EraseBulk() As Boolean
        RaiseEvent PrintConsole(RM.GetString("fcusb_cfi_erasing"))
        If MyDeviceMode = DeviceAlgorithm.Intel Or MyDeviceMode = DeviceAlgorithm.Intel_Sharp Then
            Dim secCount As Integer = GetFlashSectors()
            For i = 0 To secCount - 1
                EraseSector(i)
            Next
        ElseIf MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu Or MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu_Extended Then
            write_command(&HAAA, &HAA) 'AAA = 0xAA
            write_command(&H555, &H55) '555 = 0x55
            write_command(&HAAA, &H80) 'AAA = 0x80
            write_command(&HAAA, &HAA) 'AAA = 0xAA
            write_command(&H555, &H55) '555 = 0x55
            write_command(&HAAA, &H10) 'AAA = 0x10
            amd_erasewait(0, False) 'We may want to wait for a very long time (up to a minute)
        ElseIf MyDeviceMode = DeviceAlgorithm.SST Then
            write_command(&HAAAA, &HAA)
            write_command(&H5554, &H55)
            write_command(&HAAAA, &H80)
            write_command(&HAAAA, &HAA)
            write_command(&H5554, &H55)
            write_command(&HAAAA, &H10)
            amd_erasewait(0, False) 'We may want to wait for a very long time (up to a minute)
        ElseIf MyDeviceMode = DeviceAlgorithm.AMD_NoBypass Then
            write_command(&HAAA, &HAA) 'AAA = 0xAA
            write_command(&H555, &H55) '555 = 0x55
            write_command(&HAAA, &H80) 'AAA = 0x80
            write_command(&HAAA, &HAA) 'AAA = 0xAA
            write_command(&H555, &H55) '555 = 0x55
            write_command(&HAAA, &H10) 'AAA = 0x10
            amd_erasewait(0, False) 'We may want to wait for a very long time (up to a minute)
        End If
        Read_Mode()
        RaiseEvent PrintConsole(RM.GetString("fcusb_cfi_erasecomplete"))
        Return True
    End Function

    Public Function ReadData(ByVal Offset As UInt32, ByVal NumberOfBytes As UInt32) As Byte()
        Dim DataOut() As Byte = Nothing
        Monitor.Enter(EJ.ClientLock)
        Try
            If UseBulkRead Then 'This is significantly faster
                DataOut = EJ.Memory_Read_Bulk(MyDeviceBase + Offset, NumberOfBytes)
            Else
                Dim c As Integer = 0
                ReDim DataOut(NumberOfBytes - 1)
                For i = 0 To (NumberOfBytes / 4) - 1
                    Dim word As UInt32 = EJ.Memory_Read_W(MyDeviceBase + Offset + (i * 4))
                    DataOut(c + 3) = (word And &HFF000000) >> 24
                    DataOut(c + 2) = (word And &HFF0000) >> 16
                    DataOut(c + 1) = (word And &HFF00) >> 8
                    DataOut(c + 0) = (word And &HFF)
                    c = c + 4
                Next
            End If
        Finally
            Monitor.Exit(EJ.ClientLock)
        End Try
        Return DataOut
    End Function
    'Sector must be erased prior to writing data
    Public Sub WriteData(ByVal Offset As UInt32, ByVal Data() As Byte)
        If EJ.BigEndian Then ReverseByteEndian_16bit(Data)
        Monitor.Enter(EJ.ClientLock) 'Prevents multiple threads from accessing the bus while we are doing a sequence of commands
        Try
            If MyDeviceMode = DeviceAlgorithm.Intel Or MyDeviceMode = DeviceAlgorithm.Intel_Sharp Then
                If EJ.SUPPORT_INTELFLASH And (Not EJ.TargetDevice.NoDMA) Then 'Our fast method only works for DMA enabled targets
                    EJ.dmaWriteFlash(MyDeviceBase + Offset, Data, EJTAG.cfi_mode.Intel_16)
                Else
                    For i = 0 To (Data.Length - 1) Step 4 'We will write data 4 bytes at a time
                        EJ.Memory_Write_H(MyDeviceBase + Offset + i, &H40)
                        EJ.Memory_Write_H(MyDeviceBase + Offset + i, (CInt(Data(i + 1)) << 8) + Data(i + 0))
                        EJ.Memory_Write_H(MyDeviceBase + Offset + i + 2, &H40)
                        EJ.Memory_Write_H(MyDeviceBase + Offset + i + 2, (CInt(Data(i + 3)) << 8) + Data(i + 2))
                    Next
                End If
                Read_Mode()
            ElseIf MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu Or MyDeviceMode = DeviceAlgorithm.AMD_Fujitsu_Extended Then
                write_command(&HAAA, &HAA)
                write_command(&H555, &H55)
                write_command(&HAAA, &H20)
                If (EJ.SUPPORT_AMDFLASH And (Not EJ.TargetDevice.NoDMA)) Then 'Our fast method only works for DMA enabled targets
                    EJ.dmaWriteFlash(MyDeviceBase + Offset, Data, EJTAG.cfi_mode.AMD_16)
                Else
                    For i = 0 To (Data.Length - 1) Step 4 'We will write data 4 bytes at a time
                        EJ.Memory_Write_H(MyDeviceBase, &HA0)
                        EJ.Memory_Write_H(MyDeviceBase + Offset + i, (CInt(Data(i + 1)) << 8) + Data(i + 0))
                        EJ.Memory_Write_H(MyDeviceBase, &HA0)
                        EJ.Memory_Write_H(MyDeviceBase + Offset + i + 2, (CInt(Data(i + 3)) << 8) + Data(i + 2))
                    Next
                End If
                write_command(0, &H90)
                write_command(0, &H0)
            ElseIf MyDeviceMode = DeviceAlgorithm.AMD_NoBypass Then
                If EJ.SUPPORT_LEGACYFLASH And (Not EJ.TargetDevice.NoDMA) Then 'Our fast method only works for DMA enabled targets
                    EJ.dmaWriteFlash(MyDeviceBase + Offset, Data, EJTAG.cfi_mode.NoBypass)
                Else
                    For i = 0 To (Data.Length - 1) Step 4 'We will write data 4 bytes at a time
                        write_command(&HAAA, &HAA)
                        write_command(&H555, &H55)
                        write_command(&HAAA, &HA0)
                        EJ.Memory_Write_H(MyDeviceBase + Offset + i, (CInt(Data(i + 1)) << 8) + Data(i + 0))
                        write_command(&HAAA, &HAA)
                        write_command(&H555, &H55)
                        write_command(&HAAA, &HA0)
                        EJ.Memory_Write_H(MyDeviceBase + Offset + i + 2, (CInt(Data(i + 3)) << 8) + Data(i + 2))
                    Next
                End If
                'write_command(0, &HF0)
            ElseIf MyDeviceMode = DeviceAlgorithm.SST Then
                If EJ.SUPPORT_LEGACYFLASH And (Not EJ.TargetDevice.NoDMA) Then
                    EJ.dmaWriteFlash(MyDeviceBase + Offset, Data, EJTAG.cfi_mode.SST)
                Else
                    For i = 0 To (Data.Length - 1) Step 4 'We will write data 4 bytes at a time
                        write_command(&HAAAA, &HAA)
                        write_command(&H5554, &H55)
                        write_command(&HAAAA, &HA0)
                        EJ.Memory_Write_H(MyDeviceBase + Offset + i, (CInt(Data(i + 1)) << 8) + Data(i + 0))
                        write_command(&HAAAA, &HAA)
                        write_command(&H5554, &H55)
                        write_command(&HAAAA, &HA0)
                        EJ.Memory_Write_H(MyDeviceBase + Offset + i + 2, (CInt(Data(i + 3)) << 8) + Data(i + 2))
                    Next
                End If
            End If
        Finally
            Monitor.Exit(EJ.ClientLock)
        End Try
    End Sub

    Public Function ReadByte(ByVal offset As UInt32) As Byte
        Return EJ.Memory_Read_B(MyDeviceBase + offset)
    End Function

End Class
