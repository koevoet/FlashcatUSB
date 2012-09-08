'COPYRIGHT EMBEDDEDCOMPUTERS.NET 2012 - ALL RIGHTS RESERVED
'CONTACT EMAIL: contact@embeddedcomputers.net
'ANY USE OF THIS CODE MUST ADHERE TO THE LICENSE FILE INCLUDED WITH THIS SDK
'INFO: This class interfaces the SPI functionality to the FlashcatUSB hardware/firmware
'ACKNOWLEDGEMENT: USB driver functionality provided by LibUsbDotNet (sourceforge.net/projects/libusbdotnet) 
'Compatible with AVR firmware BCUSB.3.xx.SPI.hex

Imports LibUsbDotNet
Imports LibUsbDotNet.Info
Imports LibUsbDotNet.Main

Public Class SPI_API
    Private SwapDataFlag As Boolean = False
    Private fcusb As UsbDevice
    Private usbFinder As UsbDeviceFinder = New UsbDeviceFinder(&H16C0, &H5DE)
    Private SPI_Clock_Divider As Integer = 2
    Private MyFlashDevice As SpiFlashDevice
    Private MyFlashStatus As Status = Status.NotDetected
    Private FlashDB As New ArrayList 'Holds SpiFlashDevice

    'Contains SPI definition of address size and opcodes
    Public Class SpiDef
        Public PROGMODE As ProgramMode = ProgramMode.PageMode
        Public AddressSize As UInt32 'Number of bits the address space takes up (16/24/32)
        Public PageSize As UInt32
        Public EraseSize As UInt32 'Number of bytes an erase command wipes
        Public PageOffset As Integer 'Used by legacy atmel devices
        Public SEND_EWSR As Boolean = False 'Will send Enable Write Status Register prior to sending WRSR
        Public SEND_CMD As Boolean = False 'If true, the device will send the init cmd after read id

        Sub New(ByVal addrsize As UInt32, ByVal page_size As UInt32, ByVal EraseCMD As Byte, ByVal erase_size As UInt32)
            AddressSize = addrsize
            PageSize = page_size
            EraseSize = erase_size
            SECTORERASE = EraseCMD
        End Sub

        Public READ As Byte = &H3 'Read-data
        Public WREN As Byte = &H6 'Write-Enable
        Public WRDI As Byte = &H4 'Write-Disable
        Public RDSR As Byte = &H5 'Read Status Register
        Public EWSR As Byte = &H50 'Enable Write Status Register (used by SST/PCT chips)
        Public WRSR As Byte = &H1 'Write Status Register
        Public PROGRAM As Byte = &H2 'Page Program or word program (AAI) command
        Public SECTORERASE As Byte 'Set by sub new
        Public CHIPERASE As Byte = &HC7 'Erases the entire memory device (was 0x60)
        Public WR_TOBUFFER1 As Byte = &H84 'Command to write data into SRAM buffer 1 (used by Atmel)
        Public WR_FRMBUFFER1 As Byte = &H88 'Command to write data from SRAM buffer 1 into page (used by Atmel)
        Public INITCMD As Byte = 0 'Command to write after Read ID (if SEND_CMD is true)
        ''Number of bytes reserved for the address
        Public ReadOnly Property AddressBytes() As Integer
            Get
                Return CInt(Math.Ceiling(AddressSize / 8))
            End Get
        End Property

    End Class

    Public Class SpiFlashDevice
        Public Name As String
        Public Manu As Byte
        Public PartNum As UInt16
        Public FlashSize As UInt32
        Public Definition As SpiDef

        Sub New(ByVal FlashName As String, ByVal flash_size As UInt32, ByVal ManuID As Byte, ByVal ManuPart As UInt16)
            Name = FlashName
            Manu = ManuID
            PartNum = ManuPart
            FlashSize = flash_size
        End Sub

        Sub New()

        End Sub

    End Class

    Sub New()
        InitDefinitions()
        AddDevice("Generic 1Mbit", MB001, &H10, &H1010, Generic) 'Verified (build 320)
        AddDevice("Atmel AT25DF641", MB064, &H1F, &H4800, SPI_64KB) 'Verified (build 320)
        AddDevice("Atmel AT25DF321", MB032, &H1F, &H4700, SPI_64KB)
        AddDevice("Atmel AT25DF161", MB016, &H1F, &H4602, SPI_64KB)
        AddDevice("Atmel AT25DF081", MB008, &H1F, &H4502, SPI_4KB)
        AddDevice("Atmel AT25DF021", MB002, &H1F, &H4300, SPI_4KB)
        AddDevice("Atmel AT26DF081A", MB008, &H1F, &H4501, SPI_4KB)
        AddDevice("Atmel AT26DF161", MB016, &H1F, &H4600, SPI_64KB)
        AddDevice("Atmel AT26DF161A", MB016, &H1F, &H4601, SPI_64KB)
        AddDevice("Atmel AT26DF321", MB032, &H1F, &H4700, SPI_64KB)
        AddDevice("Spansion S25FL256S", MB256, &H1, &H219, SPI_HDSP) 'Verified (build 320)
        AddDevice("Spansion S25FL128S", MB128, &H1, &H218, SPI_64KB)
        AddDevice("Spansion S25FL128P", MB128, &H1, &H2018, SPI_64KB) 'Testing
        AddDevice("Spansion S25FL064", MB064, &H1, &H216, SPI_64KB)
        AddDevice("Spansion S25FL032", MB032, &H1, &H215, SPI_64KB)
        AddDevice("Spansion S25FL016", MB016, &H1, &H214, SPI_64KB)
        AddDevice("Spansion S25FL008", MB008, &H1, &H213, SPI_64KB)
        AddDevice("Micron M25PE10", MB001, &H20, &H8011, SPI_64KB)
        AddDevice("Micron M25PE20", MB002, &H20, &H8012, SPI_64KB)
        AddDevice("Micron M25PE40", MB004, &H20, &H8013, SPI_64KB)
        AddDevice("Micron M25PE80", MB008, &H20, &H8014, SPI_64KB)
        AddDevice("Micron M25PE16", MB016, &H20, &H8015, SPI_64KB)
        AddDevice("Micron M25P05", 65536, &H20, &H2010, Generic) 'Verified (build 326)
        AddDevice("Micron M25P10", MB001, &H20, &H2011, Generic)
        AddDevice("Micron M25P20", MB002, &H20, &H2012, SPI_32KB)
        AddDevice("Micron M25P40", MB004, &H20, &H2013, SPI_32KB)
        AddDevice("Micron M25P80", MB008, &H20, &H2014, SPI_64KB)
        AddDevice("Micron M25P16", MB016, &H20, &H2015, SPI_64KB)
        AddDevice("Micron M25P32", MB032, &H20, &H2016, SPI_64KB)
        AddDevice("Micron M25P64", MB064, &H20, &H2017, SPI_64KB) 'Verified (build 320)
        AddDevice("Micron M25P128", MB128, &H20, &H2018, SPI_2MBT) 'Verified (build 325)
        AddDevice("Micron N25Q064", MB064, &H20, &HBB17, SPI_64KB) 'Verified (Numonyx version)
        AddDevice("Micron N25Q064A", MB128, &H20, &HBA17, SPI_64KB) 'Test pending
        AddDevice("Micron N25Q128", MB128, &H20, &HBA18, SPI_64KB) 'Test pending
        AddDevice("Micron N25Q256", MB256, &H20, &HBA19, SPI_HD64) 'Test pending
        AddDevice("Micron N25Q512", MB512, &H20, &HBA20, SPI_HD64) 'Test pending
        AddDevice("Micron N25Q00A", MB1Gb, &H20, &HBA21, SPI_HD64) 'Test pending
        AddDevice("Micron M25PX64", MB064, &H20, &H7117, SPI_64KB) 'Test pending
        AddDevice("Windbond W25X40", MB004, &HEF, &H3013, SPI_32KB)
        AddDevice("Windbond W25X80", MB008, &HEF, &H3014, SPI_4KB)
        AddDevice("Windbond W25X16", MB016, &HEF, &H3015, SPI_4KB)
        AddDevice("Windbond W25X32", MB032, &HEF, &H3016, SPI_4KB)
        AddDevice("Windbond W25X64", MB064, &HEF, &H3017, SPI_4KB)
        AddDevice("Windbond W25Q80", MB008, &HEF, &H4014, SPI_32KB) 'Verified (build 320)
        AddDevice("Windbond W25Q16", MB016, &HEF, &H4015, SPI_32KB) 'Verified (build 320)    'Should work for BV/FV variants
        AddDevice("Windbond W25Q32", MB032, &HEF, &H4016, SPI_32KB) 'Should work for BV/FV variants
        AddDevice("Windbond W25Q64", MB064, &HEF, &H4017, SPI_32KB) 'Verified (build 325) - Requires #HOLD 'Should work for BV/FV variants
        AddDevice("MXIC MX25L10", MB001, &HC2, &H2011, SPI_64KB)
        AddDevice("MXIC MX25L20", MB002, &HC2, &H2012, SPI_4KB)
        AddDevice("MXIC MX25L40", MB004, &HC2, &H2013, SPI_64KB)
        AddDevice("MXIC MX25L4006", MB004, &HC2, &H13, SPI_4KB)
        AddDevice("MXIC MX25L80", MB008, &HC2, &H2014, SPI_64KB)
        AddDevice("MXIC MX25L160", MB016, &HC2, &H2015, SPI_64KB)
        AddDevice("MXIC MX25L320", MB032, &HC2, &H2016, SPI_64KB)
        AddDevice("MXIC MX25L640", MB064, &HC2, &H2017, SPI_64KB)
        AddDevice("MXIC MX25L128", MB128, &HC2, &H2018, SPI_64KB)
        AddDevice("MXIC MX25L256", MB256, &HC2, &H2019, SPI_HD64) 'In progress
        AddDevice("EON EN25F20", MB002, &H1C, &H3112, SPI_4KB)
        AddDevice("EON EN25F40", MB004, &H1C, &H3113, SPI_4KB) 'recommend fosc(4)
        AddDevice("EON EN25F80", MB008, &H1C, &H3114, SPI_4KB)
        AddDevice("EON EN25P16", MB016, &H1C, &H2015, SPI_32KB)
        AddDevice("EON EN25P32", MB032, &H1C, &H2016, SPI_32KB)
        AddDevice("EON EN25P64", MB064, &H1C, &H2017, SPI_32KB)
        AddDevice("EON EN25Q64", MB064, &H1C, &H3017, SPI_64KB)
        AddDevice("SST 25VF010A", MB001, &HBF, &H49BF, SST_4KB) 'Verified (build 320)
        AddDevice("SST 25VF020A", MB002, &HBF, &H43BF, SST_4KB) 'Verified (build 320)
        AddDevice("SST 25WF040", MB004, &HBF, &H2504, SST_64KB)
        AddDevice("SST 25VF040B", MB004, &HBF, &H258D, SST_64KB)
        AddDevice("SST 25VF080", MB008, &HBF, &H80BF, SST_64KB)
        AddDevice("SST 25VF080B", MB008, &HBF, &H258E, SST_64KB) 'Verified (build 320)
        AddDevice("SST 26VF016", MB016, &HBF, &H2601, SST_64KB)
        AddDevice("SST 25VF016B", MB016, &HBF, &H2541, SST_64KB) 'Verified (build 320)
        AddDevice("SST 25VF032", MB032, &HBF, &H254A, SST_64KB) 'Verified (build 320)
        AddDevice("SST 25VF032B", MB032, &HBF, &H2542, SST_64KB)
        AddDevice("SST 26VF032", MB032, &HBF, &H2602, SST_64KB)
        AddDevice("SST 26VF064", MB064, &HBF, &H2603, SST_64KB)
        AddDevice("SST 25VF064B", MB064, &HBF, &H2543, SST_64KB)
        AddDevice("SST 25VF064C", MB064, &HBF, &H254B, SST_64KB)
        AddDevice("SST 25VF128B", MB128, &HBF, &H2544, SST_64KB)
        AddDevice("PMC PM25LV512", 65536, &H9D, &H7B7F, PMC_4KB)
        AddDevice("PMC PM25LV010", MB001, &H9D, &H7C7F, PMC_4KB)
        AddDevice("PMC PM25LV020", MB002, &H9D, &H7D7F, PMC_4KB)
        AddDevice("PMC PM25LV040", MB004, &H9D, &H7E7F, PMC_4KB)
        AddDevice("PMC PM25LV080B", MB008, &H7F, &H9D13, PMC_4KB)
        AddDevice("PMC PM25LV016B", MB016, &H7F, &H9D14, PMC_4KB)
        AddDevice("Atmel AT45DB011", 135168, &H1F, &H2200, AT45DB_PS264)
        AddDevice("Atmel AT45DB021", 270336, &H1F, &H2300, AT45DB_PS264)
        AddDevice("Atmel AT45DB041", 540672, &H1F, &H2400, AT45DB_PS264)
        AddDevice("Atmel AT45DB081", 1081344, &H1F, &H2500, AT45DB_PS264) 'Verified (build 320) / AT45DB081D
        AddDevice("Atmel AT45DB161", 2162688, &H1F, &H2600, AT45DB_PS528) 'Verified (build 320) / AT45DB161D
        AddDevice("Atmel AT45DB321", 4325376, &H1F, &H2700, AT45DB_PS528)
        AddDevice("Atmel AT45DB642", 8650752, &H1F, &H2800, AT45DB_PS1056)
        AddDevice("PCT 25VF512A", 65536, &HBF, &H48BF, PCT_4KB_BYTE) '512kbit
        AddDevice("PCT 25VF010A", MB001, &HBF, &H49BF, PCT_4KB_BYTE)
        AddDevice("PCT 25VF020B", MB002, &HBF, &H258C, PCT_4KB_WORD)
        AddDevice("PCT 25VF040B", MB004, &HBF, &H258D, PCT_4KB_WORD)
        AddDevice("PCT 25VF080B", MB008, &HBF, &H258E, PCT_4KB_WORD)
        AddDevice("PCT 25VF016B", MB016, &HBF, &H2541, PCT_32KB)
        AddDevice("PCT 25VF032B", MB032, &HBF, &H254A, PCT_32KB)
        AddDevice("PCT 25VF064C", MB064, &HBF, &H254B, PCT_64KB) 'Uses Page-Program, not AAI!
        AddDevice("PCT 26VF016", MB016, &HBF, &H2601, SPI_4KB)
        AddDevice("PCT 26VF032", MB032, &HBF, &H2602, SPI_4KB)
    End Sub

    Private Sub InitDefinitions()
        SPI_4KB = New SpiDef(24, 256, &H20, &H1000)
        SPI_32KB = New SpiDef(24, 256, &H52, &H8000)
        SPI_64KB = New SpiDef(24, 256, &HD8, &H10000)
        SPI_2MBT = New SpiDef(24, 256, &HD8, &H40000) 'Yes, the ST M25P128 uses sectors of 2mbit each

        PCT_4KB_BYTE = New SpiDef(24, 256, &H20, &H1000) : PCT_4KB_BYTE.SEND_EWSR = True : PCT_4KB_BYTE.PROGMODE = ProgramMode.AAI_Byte : PCT_4KB_BYTE.PROGRAM = &HAF
        PCT_4KB_WORD = New SpiDef(24, 256, &H20, &H1000) : PCT_4KB_WORD.SEND_EWSR = True : PCT_4KB_WORD.PROGMODE = ProgramMode.AAI_Word : PCT_4KB_WORD.PROGRAM = &HAD
        PCT_32KB = New SpiDef(24, 256, &H52, &H8000) : PCT_32KB.SEND_EWSR = True : PCT_32KB.PROGMODE = ProgramMode.AAI_Word : PCT_32KB.PROGRAM = &HAD
        PCT_64KB = New SpiDef(24, 256, &HD8, &H10000) : PCT_64KB.SEND_EWSR = True

        Generic = New SpiDef(24, 256, &HD8, &H8000) : Generic.CHIPERASE = &HC7 'Tested on ST M25P10-A
        PMC_4KB = New SpiDef(24, 256, &HD7, &H1000) : PMC_4KB.CHIPERASE = &HC7
        SST_64KB = New SpiDef(24, 256, &HD8, &H10000) : SST_64KB.PROGMODE = ProgramMode.AAI_Word : SST_64KB.PROGRAM = &HAD : SST_64KB.SEND_EWSR = True
        SST_4KB = New SpiDef(24, 256, &H20, &H1000) : SST_4KB.PROGMODE = ProgramMode.AAI_Byte : SST_4KB.PROGRAM = &HAF : SST_4KB.SEND_EWSR = True
        SPI_HDSP = New SpiDef(32, 256, &HD8, &H10000) : SPI_HDSP.PROGRAM = &H12 : SPI_HDSP.SECTORERASE = &HDC : SPI_HDSP.READ = &H13 '(enables 32-bit address commands)
        SPI_HD64 = New SpiDef(32, 256, &HD8, &H10000) : SPI_HD64.SEND_CMD = True : SPI_HD64.INITCMD = &HB7

        AT45DB_PS264 = New SpiDef(24, 264, &H50, 2112)
        AT45DB_PS264.PROGMODE = ProgramMode.AtmelLegacy
        AT45DB_PS264.RDSR = &HD7
        AT45DB_PS264.READ = &HE8
        AT45DB_PS264.PageOffset = 9

        AT45DB_PS528 = New SpiDef(24, 528, &H50, 4224)
        AT45DB_PS528.PROGMODE = ProgramMode.AtmelLegacy
        AT45DB_PS528.RDSR = &HD7
        AT45DB_PS528.READ = &HE8
        AT45DB_PS528.PageOffset = 10

        AT45DB_PS1056 = New SpiDef(24, 1056, &H50, 8448)
        AT45DB_PS1056.PROGMODE = ProgramMode.AtmelLegacy
        AT45DB_PS1056.RDSR = &HD7
        AT45DB_PS1056.READ = &HE8
        AT45DB_PS1056.PageOffset = 11

        CustomDevice = New SpiFlashDevice With {.Name = "SPI_SPECIFIC", .PartNum = 0}
        CustomDevice.Definition = New SpiDef(24, 256, &HD8, &H10000)
        CustomDevice.FlashSize = MB001

    End Sub

    Public Sub AddDevice(ByRef name As String, ByRef size As UInt32, ByRef manuid As Byte, ByRef part As UInt16, ByVal OPCODES As SpiDef)
        Dim spi_dev As New SpiFlashDevice(name, size, manuid, part)
        spi_dev.Definition = OPCODES
        FlashDB.Add(spi_dev)
    End Sub

    Public Function DetectFlash() As Boolean
        Dim Found As Boolean = False
        Dim MFG As Byte = 0
        Dim PART As Integer = 0
        If UseCustom Then 'No need to detect, we are going to use the gui settings
            MyFlashDevice = CustomDevice
        Else 'Search for it
            Dim ID As Integer
            If Not ReadDeviceID(ID) Then
                WriteConsole(RM.GetString("fcusb_spi_err5")) 'Unable to connect to compatible SPI device
                MyFlashStatus = Status.NotDetected
                Return False
            End If
            MFG = CByte((ID And &HFF0000) >> 16)
            PART = (ID And &HFFFF)
            Found = GetFlashDefinition(MFG, PART, MyFlashDevice)
        End If
        Dim Jedec As String = Hex(MFG).PadLeft(2, CChar("0")) & " " & Hex(PART).PadLeft(4, CChar("0"))
        WriteConsole(RM.GetString("fcusb_spi_connflash") & " (JEDEC: " & Jedec & ")")
        If UseCustom Then
            WriteConsole(RM.GetString("fcusb_spi_usingcustom"))
        ElseIf Not Found Then
            WriteConsole(RM.GetString("fcusb_spi_email"))
            MyFlashDevice = New SpiFlashDevice()
            MyFlashDevice.Manu = MFG
            MyFlashDevice.PartNum = PART
            MyFlashStatus = Status.NotSupported
            Return False
        End If
        MyFlashStatus = Status.Supported
        If MyFlashDevice.Definition.SEND_CMD Then
            SPIBUS_WriteRead({MyFlashDevice.Definition.INITCMD}, Nothing)
        End If
        SPIBUS_WriteEnable() 'Some devices such as AT25DF641 require the WREN and the status reg cleared before we can write data
        WriteStatusReg(0)
        Dim SpiFlashSize As Integer = GetFlashSize()
        WriteConsole(String.Format(RM.GetString("fcusb_spi_flashdetected"), GetFlashName, Format(SpiFlashSize, "#,###")))
        WriteConsole(RM.GetString("fcusb_spi_progmode"))
        Return True
    End Function
    'Returns true if device ID was read
    Public Function ReadDeviceID(ByRef ReturnID As Integer) As Boolean
        Dim order As SPI_ORDER
        Dim mode As SPI_MODE
        Dim clock As SPI_CLOCK
        Dim SupportedModes() As SPI_CLOCK = CType(GetSpiModes(), SPI_CLOCK())
        Dim SupportedClocks() As SPI_MODE = CType(GetSpiClocks(), SPI_MODE())
        Dim SupportedOrders() As SPI_ORDER = GetSpiOrders()
        Dim TotalModes As Integer = SupportedModes.Length * SupportedClocks.Length * SupportedOrders.Length
        Dim i As Integer = 0
        For Each order In SupportedOrders
            For Each clock In SupportedClocks
                For Each mode In SupportedModes
                    i = i + 1
                    SetDeviceConfig(clock, mode, order)
                    Application.DoEvents()
                    Sleep(100)
                    ReturnID = DeviceReadId()
                    If Not ReturnID = &HFFFFFF Then
                        WriteConsole(RM.GetString("fcusb_spi_openinmode") & " " & ModeToStr(mode) & " (Fosc/" & ClockToStr(clock) & " " & OrderToStr(order) & ")")
                        Return True
                    End If
                Next
            Next
        Next
        Return False
    End Function
    'Sends various READ ID commands to get a response from the chip (9F 90 AB)
    Public Function DeviceReadId() As Integer
        If Not IsOpened() Then Return NO_ID_READ
        Dim res As Integer = NO_ID_READ
        Dim buff() As Byte
        If Not res = &HFFFFFF Then Return res
        ReDim buff(2)
        If SPIBUS_WriteRead({&H9F}, buff) = 4 Then 'READ JEDEC ID (most common)
            res = (buff(0) * 65536) + (buff(1) * 256) + buff(2)
        End If
        If Not res = &HFFFFFF Then Return res
        If SPIBUS_WriteRead({&H90}, buff) = 4 Then 'RDID
            res = (buff(0) * 65536) + (buff(1) * 256) + buff(2)
        End If
        If Not res = &HFFFFFF Then Return res
        ReDim buff(2)
        If SPIBUS_WriteRead({&HAB, 0, 0, 0}, buff) = 7 Then 'READ ID (PMC / ST M25P10)
            res = (buff(0) * 65536) + (buff(1) * 256) + buff(2)
        End If
        If Not res = &HFFFFFF Then Return res
        Return res
    End Function

    Public Function ReadData(ByVal Offset As UInt32, ByVal ByteCount As UInt32) As Byte()
        Dim count_upper As UShort = (ByteCount And &HFFFF0000) >> 16
        Dim count_lower As UShort = (ByteCount And &HFFFF)
        Dim ReadArraySetup() As Byte
        If MyFlashDevice.Definition.PROGMODE = ProgramMode.AtmelLegacy Then
            Dim PageAddr As Integer = Math.Floor(Offset / MyFlashDevice.Definition.PageSize)
            Dim PageOffset As Integer = Offset - (PageAddr * MyFlashDevice.Definition.PageSize)
            Dim u() As Byte = IntTo3Bytes((PageAddr << MyFlashDevice.Definition.PageOffset) + PageOffset)
            ReadArraySetup = {MyFlashDevice.Definition.READ, u(0), u(1), u(2), 0, 0, 0, 0}
        Else
            ReadArraySetup = GetArrayWithCmdAndAddr(MyFlashDevice.Definition.READ, Offset)
        End If
        'Writes the command to put the device into read-array mode
        Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
        Dim usbpacket1 As New UsbSetupPacket(usbflag, SPIREQ_READBULK, count_upper, count_lower, CShort(ReadArraySetup.Length))
        fcusb.ControlTransfer(usbpacket1, ReadArraySetup, ReadArraySetup.Length, Nothing)
        'Open up our endpoint reader to do mass-reading from the device
        Dim reader As UsbEndpointReader = fcusb.OpenEndpointReader(ReadEndpointID.Ep01, ByteCount, EndpointType.Bulk)
        Dim dataout(ByteCount - 1) As Byte
        reader.Read(dataout, 0, CInt(ByteCount), 500, Nothing)
        If SwapDataFlag Then SwapEndian_8bit(dataout)
        Return dataout
    End Function

    Public Function WriteData(ByVal Offset As UInt32, ByVal Data() As Byte) As Boolean
        If SwapDataFlag Then SwapEndian_8bit(Data)
        Select Case MyFlashDevice.Definition.PROGMODE
            Case ProgramMode.PageMode
                Return WriteData_PerPage(Offset, Data)
            Case ProgramMode.AAI_Byte
                Return WriteData_AAI_Byte(Offset, Data)
            Case ProgramMode.AAI_Word
                Return WriteData_AAI_Word(Offset, Data)
            Case ProgramMode.AtmelLegacy
                Return WriteData_ATMEL(Offset, Data)
            Case ProgramMode.Nordic
                Return WriteData_Nordic(Offset, Data)
            Case Else
                Return False
        End Select
    End Function

    Public Function WriteData_PerPage(ByVal Offset As UInt32, ByVal Data() As Byte) As Boolean
        Dim psize As UInt32 = MyFlashDevice.Definition.PageSize
        Dim ret As Integer = 0
        Dim count_upper As UShort = (Data.Length And &HFFFF0000) >> 16
        Dim count_lower As UShort = (Data.Length And &HFFFF)
        Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
        Dim buffer(9) As Byte
        buffer(0) = MyFlashDevice.Definition.PROGRAM
        buffer(1) = MyFlashDevice.Definition.WREN
        buffer(2) = MyFlashDevice.Definition.RDSR
        buffer(3) = CByte(MyFlashDevice.Definition.AddressBytes)
        buffer(4) = CByte((psize And &HFF00) >> 8)
        buffer(5) = CByte(psize And &HFF)
        buffer(6) = CByte((Offset And &HFF000000) >> 24)
        buffer(7) = CByte((Offset And &HFF0000) >> 16)
        buffer(8) = CByte((Offset And &HFF00) >> 8)
        buffer(9) = CByte(Offset And &HFF)
        Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_WRITEBULK, count_upper, count_lower, CShort(buffer.Length))
        Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, buffer, buffer.Length, ret)
        Dim writer As UsbEndpointWriter = fcusb.OpenEndpointWriter(WriteEndpointID.Ep02, EndpointType.Bulk)
        Dim ec As ErrorCode = writer.Write(Data, 0, Data.Length, 5000, ret)
        If ec = ErrorCode.None Then Return True
        Return False
    End Function
    'Writes the data two bytes at a time using AAI
    Public Function WriteData_AAI_Word(ByVal offset As UInteger, ByVal data() As Byte) As Boolean
        SPIBUS_WriteEnable()
        Dim BytesToWrite As UInt32 = data.Length - 1
        Dim count_upper As UShort = (BytesToWrite And &HFFFF0000) >> 16
        Dim count_lower As UShort = (BytesToWrite And &HFFFF)
        Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
        Dim buffer(8) As Byte
        buffer(0) = MyFlashDevice.Definition.PROGRAM
        buffer(1) = MyFlashDevice.Definition.RDSR
        buffer(2) = CByte(MyFlashDevice.Definition.AddressBytes)
        buffer(3) = CByte((offset And &HFF000000) >> 24)
        buffer(4) = CByte((offset And &HFF0000) >> 16)
        buffer(5) = CByte((offset And &HFF00) >> 8)
        buffer(6) = CByte(offset And &HFF)
        buffer(7) = data(0) 'We write the first and second byte with the setup packet
        buffer(8) = data(1)
        Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_WRITEBULK_AAIWORD, count_upper, count_lower, CShort(buffer.Length))
        Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, buffer, buffer.Length, Nothing)
        Dim writer As UsbEndpointWriter = fcusb.OpenEndpointWriter(WriteEndpointID.Ep02, EndpointType.Bulk)
        Dim ec As ErrorCode = writer.Write(data, 2, data.Length - 1, 2500, Nothing)
        SPIBUS_WriteDisable()
        WaitUntilReady()
        If ec = ErrorCode.None Then Return True
        Return False
    End Function
    'Writes the data one byte at a time using AAI
    Public Function WriteData_AAI_Byte(ByVal offset As UInteger, ByVal data() As Byte) As Boolean
        SPIBUS_WriteEnable()
        Dim BytesToWrite As UInt32 = data.Length - 1
        Dim count_upper As UShort = (BytesToWrite And &HFFFF0000) >> 16
        Dim count_lower As UShort = (BytesToWrite And &HFFFF)
        Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
        Dim buffer(7) As Byte
        buffer(0) = MyFlashDevice.Definition.PROGRAM
        buffer(1) = MyFlashDevice.Definition.RDSR
        buffer(2) = CByte(MyFlashDevice.Definition.AddressBytes)
        buffer(3) = CByte((offset And &HFF000000) >> 24)
        buffer(4) = CByte((offset And &HFF0000) >> 16)
        buffer(5) = CByte((offset And &HFF00) >> 8)
        buffer(6) = CByte(offset And &HFF)
        buffer(7) = data(0) 'We write the first byte with the setup packet
        Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_WRITEBULK_AAIBYTE, count_upper, count_lower, CShort(buffer.Length))
        Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, buffer, buffer.Length, Nothing)
        Dim writer As UsbEndpointWriter = fcusb.OpenEndpointWriter(WriteEndpointID.Ep02, EndpointType.Bulk)
        Dim ec As ErrorCode = writer.Write(data, 1, data.Length - 1, 2500, Nothing)
        SPIBUS_WriteDisable()
        WaitUntilReady()
        If ec = ErrorCode.None Then Return True
        Return False
    End Function
    'Uses an internal sram buffer to transfer data from the board to the flash (used by Atmel AT45DBxxx)
    Public Function WriteData_ATMEL(ByVal offset As UInteger, ByVal total_data() As Byte) As Boolean
        Dim BytesLeft As Integer = total_data.Length
        Do Until BytesLeft = 0
            Dim BytesToWrite As Integer = BytesLeft
            If BytesToWrite > MyFlashDevice.Definition.PageSize Then BytesToWrite = MyFlashDevice.Definition.PageSize
            Dim DataToBuffer(BytesToWrite + 3) As Byte
            DataToBuffer(0) = MyFlashDevice.Definition.WR_TOBUFFER1
            Dim src_ind As Integer = total_data.Length - BytesLeft
            Array.Copy(total_data, src_ind, DataToBuffer, 4, BytesToWrite)
            Dim count_upper As UShort = (DataToBuffer.Length And &HFFFF0000) >> 16
            Dim count_lower As UShort = (DataToBuffer.Length And &HFFFF)
            Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
            Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_WRITEBULK_RAW, count_upper, count_lower, CShort(0))
            Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, Nothing, 0, Nothing) 'Setup command for buffer write
            Dim writer As UsbEndpointWriter = fcusb.OpenEndpointWriter(WriteEndpointID.Ep02, EndpointType.Bulk)
            Dim ec As ErrorCode = writer.Write(DataToBuffer, 0, DataToBuffer.Length, 2500, Nothing)
            WaitUntilReady()
            Dim PageAddr As Integer = Math.Floor(offset / MyFlashDevice.Definition.PageSize)
            Dim PageCmd() As Byte = IntTo3Bytes(PageAddr << MyFlashDevice.Definition.PageOffset)
            Dim Cmd2() As Byte = {MyFlashDevice.Definition.WR_FRMBUFFER1, PageCmd(0), PageCmd(1), PageCmd(2)}
            SPIBUS_WriteRead(Cmd2, Nothing)
            WaitUntilReady()
            offset += BytesToWrite
            BytesLeft -= BytesToWrite
        Loop
        Return True
    End Function
    'Used by the Nordic MCU device
    Public Function WriteData_Nordic(ByVal Offset As UInt32, ByVal Data() As Byte) As Boolean
        'We can write up to 1024 '2 pages
        Dim dataout(Data.Length + 2) As Byte
        dataout(0) = MyFlashDevice.Definition.PROGRAM
        dataout(1) = CByte((Offset >> 8) And &HFF)
        dataout(2) = CByte(Offset And &HFF)
        Array.Copy(Data, 0, dataout, 3, Data.Length)
        SPIBUS_WriteEnable()
        Dim count_upper As UShort = (dataout.Length And &HFFFF0000) >> 16
        Dim count_lower As UShort = (dataout.Length And &HFFFF)
        Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
        Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_WRITEBULK_RAW, count_upper, count_lower, CShort(0))
        Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, Nothing, 0, Nothing) 'Setup command for buffer write
        Dim writer As UsbEndpointWriter = fcusb.OpenEndpointWriter(WriteEndpointID.Ep02, EndpointType.Bulk)
        Dim ec As ErrorCode = writer.Write(dataout, 0, dataout.Length, 2500, Nothing)
        WaitUntilReady()
        Return True
    End Function
    'Writes data to a given sector and also swaps bytes (endian for words/halfwords)
    Public Function WriteSector(ByVal Sector As Integer, ByVal data() As Byte) As Boolean
        Dim Addr32 As UInteger = FindSectorBase(Sector)
        Return WriteData(Addr32, data)
    End Function

    Public Function BulkErase() As Boolean
        Dim res As Integer
        Dim readbackcount As Integer
        If MyFlashDevice.Definition.PROGMODE = ProgramMode.AtmelLegacy Then
            readbackcount = 4
            res = SPIBUS_WriteRead({&HC7, &H94, &H80, &H9A}, Nothing)
        ElseIf MyFlashDevice.Definition.PROGMODE = ProgramMode.Nordic Then 'We do NOT want to use bulk erase, since that erases NV data and IP page!
            Dim t As New Stopwatch : t.Start()
            WriteConsole(String.Format(RM.GetString("fcusb_spi_erasingbulk"), Format(GetFlashSize, "#,###")))
            Dim TotalPages As Integer = MyFlashDevice.FlashSize / MyFlashDevice.Definition.PageSize
            For i = 0 To TotalPages - 1
                SPIBUS_WriteEnable() : Sleep(50)
                SPIBUS_WriteRead({MyFlashDevice.Definition.CHIPERASE, i}, Nothing)
                WaitUntilReady()
            Next
            WriteConsole(String.Format(RM.GetString("fcusb_spi_erasecomplete"), Format(t.ElapsedMilliseconds / 1000, "#.##")))
            Return True
        Else
            SPIBUS_WriteEnable()
            readbackcount = 1
            res = SPIBUS_WriteRead({MyFlashDevice.Definition.CHIPERASE}, Nothing)
        End If
        If res = readbackcount Then
            Dim t As New Stopwatch : t.Start()
            WriteConsole(String.Format(RM.GetString("fcusb_spi_erasingbulk"), Format(GetFlashSize, "#,###")))
            WaitUntilReady()
            WriteConsole(String.Format(RM.GetString("fcusb_spi_erasecomplete"), Format(t.ElapsedMilliseconds / 1000, "#.##")))
            Return True
        Else
            Return False
        End If
    End Function

    Public Sub EraseSector(ByVal Sector As UInt32)
        Dim offset As UInteger = FindSectorBase(Sector)
        If MyFlashDevice.Definition.PROGMODE = ProgramMode.AtmelLegacy Then
            Dim blocknum As Integer = Math.Floor(offset / MyFlashDevice.Definition.EraseSize)
            Dim addrbytes() As Byte = IntTo3Bytes(blocknum << (MyFlashDevice.Definition.PageOffset + 3))
            SPIBUS_WriteRead({MyFlashDevice.Definition.SectorErase, addrbytes(0), addrbytes(1), addrbytes(2)}, Nothing)
        ElseIf MyFlashDevice.Definition.PROGMODE = ProgramMode.Nordic Then
            SPIBUS_WriteEnable() : Sleep(50)
            Dim PageNum As Byte = Math.Floor(offset / 512)
            SPIBUS_WriteRead({MyFlashDevice.Definition.SectorErase, PageNum}, Nothing)
        Else
            SPIBUS_WriteEnable()
            Dim DataToWrite() As Byte = GetArrayWithCmdAndAddr(MyFlashDevice.Definition.SECTORERASE, offset)
            SPIBUS_WriteRead(DataToWrite, Nothing)
        End If
        WaitUntilReady()
    End Sub

    Public Sub WaitUntilReady()
        Dim Status As UInt32
        If MyFlashDevice.Definition.PROGMODE = ProgramMode.AtmelLegacy Then
            Do
                Status = ReadStatusReg()
                If Not ((Status And &H80) > 0) Then Sleep(50)
            Loop While Not ((Status And &H80) > 0)
        Else
            Do
                Status = ReadStatusReg()
                If (Status And 1) Then Sleep(50)
            Loop While (Status And 1)
            If MyFlashDevice.Definition.PROGMODE = ProgramMode.Nordic Then
                Sleep(50)
            End If
        End If
    End Sub

    Public Function SPIBUS_WriteEnable() As Boolean
        If SPIBUS_WriteRead({MyFlashDevice.Definition.WREN}, Nothing) = 1 Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Function SPIBUS_WriteDisable() As Boolean
        If SPIBUS_WriteRead({MyFlashDevice.Definition.WRDI}, Nothing) = 1 Then
            Return True
        Else
            Return False
        End If
    End Function

    Public Function GetFlashName() As String
        If MyFlashStatus = Status.Supported Then
            Return MyFlashDevice.Name
        Else
            Return GetJedecName()
        End If
    End Function

    Public Function GetFlashSize() As UInt32
        If MyFlashStatus = Status.Supported Then
            Return MyFlashDevice.FlashSize
        Else
            Return 0
        End If
    End Function

    Public Function GetSectorSize() As UInt32
        If MyFlashStatus = Status.Supported Then
            'Return MyFlashDevice.Definition.PageSize
            Return MyFlashDevice.Definition.EraseSize
        Else
            Return 0
        End If
    End Function
    'Returns the total number of sectors (actually number of flash pages)
    Public Function GetFlashSectors() As UInt32
        If MyFlashStatus = Status.Supported Then
            'Dim PageSize As UInt32 = GetSectorSize()
            Dim EraseSize As UInt32 = MyFlashDevice.Definition.EraseSize
            Dim FlashSize As UInt32 = GetFlashSize()
            Return CInt(FlashSize / EraseSize)
        Else
            Return 0
        End If
    End Function

    Public Function GetJedecName() As String
        If SpiNordicMode Then
            Return "Nordic"
        ElseIf MyFlashStatus = Status.Supported Then
            Return Hex(MyFlashDevice.Manu).PadLeft(2, CChar("0")) & " " & Hex(MyFlashDevice.PartNum).PadLeft(4, CChar("0"))
        Else
            Return "Not supported"
        End If
    End Function
    'Returns the 8-bits from the status register
    Public Function ReadStatusReg() As Byte
        Dim status_reg(0) As Byte
        SPIBUS_WriteRead({MyFlashDevice.Definition.RDSR}, status_reg)
        Return status_reg(0)
    End Function

    Public Function WriteStatusReg(ByVal RegValue As Byte) As Boolean
        If MyFlashDevice.Definition.SEND_EWSR Then
            SPIBUS_WriteRead({MyFlashDevice.Definition.EWSR}, Nothing)
            Threading.Thread.Sleep(20)
        End If
        If Not SPIBUS_WriteRead({MyFlashDevice.Definition.WRSR, RegValue}, Nothing) = 2 Then Return False
        Return True
    End Function

    Public Function FindSectorBase(ByVal SectorIndex As UInt32) As UInt32
        Return GetSectorSize() * SectorIndex
    End Function

    Private ScanInProgress As Boolean = False
    Private ScanThread As Threading.Thread

    Public Sub ScanForDevice()
        If SpiNordicMode Then
            CreateNordicDevice("Main flash block", 16384)
        Else
            ScanInProgress = True
            ScanThread = New Threading.Thread(AddressOf ScanningThread)
            ScanThread.Name = "SpiDetect"
            ScanThread.IsBackground = True
            ScanThread.Start()
        End If
    End Sub

    Private Sub ScanningThread()
        DetectFlash()
        ScanInProgress = False
    End Sub
    'Call this to make the SPI program work with Nordic devices (such as nRF24LE1)
    Public Sub CreateNordicDevice(ByVal name As String, ByVal size As Integer)
        EnableProgMode()
        SetDeviceConfig(SPI_CLOCK.SPI_CLOCK_FOSC_2, SPI_MODE.SPI_MODE_0, SPI_ORDER.SPI_ORDER_MSB_FIRST)
        MyFlashDevice = New SpiFlashDevice(name, size, 0, 0)
        MyFlashDevice.Definition = New SpiDef(16, 512, &H52, 512)
        MyFlashStatus = Status.Supported
        MyFlashDevice.Definition.PROGMODE = ProgramMode.Nordic
    End Sub

    Public Function GetFlashDefinition(ByVal MFG As Byte, ByVal PART As Integer, ByRef RetDev As SpiFlashDevice) As Boolean
        Dim item As SpiFlashDevice
        For Each item In FlashDB
            If item.Manu = MFG And item.PartNum = PART Then
                RetDev = item
                Return True
            End If
        Next
        Return False
    End Function

#Region "Public Properties"

    Public ReadOnly Property Scanning
        Get
            Return ScanInProgress
        End Get
    End Property

    Public ReadOnly Property FlashStatus As Status
        Get
            Return MyFlashStatus
        End Get
    End Property

    Public ReadOnly Property EraseRequired As Boolean
        Get
            Return True
        End Get
    End Property

    Public Property ReverseEndian As Boolean
        Get
            Return SwapDataFlag
        End Get
        Set(value As Boolean)
            SwapDataFlag = value
        End Set
    End Property

#End Region

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

    Public Sub Disconnect()
        MyFlashStatus = Status.NotDetected
        MyFlashDevice = Nothing
        Application.DoEvents()
        CloseDevice()
    End Sub

    Public Function OpenDevice() As Boolean
        CloseDevice()
        If Not IsConnected() Then Return False
        fcusb = UsbDevice.OpenUsbDevice(usbFinder)
        If fcusb IsNot Nothing Then
            Dim wholeUsbDevice As IUsbDevice = TryCast(fcusb, IUsbDevice)
            If wholeUsbDevice IsNot Nothing Then 'Libusb only
                wholeUsbDevice.SetConfiguration(1)
                wholeUsbDevice.ClaimInterface(0)
            End If
            SetDeviceConfig(SPI_CLOCK.SPI_CLOCK_FOSC_2, SPI_MODE.SPI_MODE_0)
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

    Private Sub UpdateClockDevider(ByVal clock As SPI_CLOCK)
        Select Case clock
            Case SPI_CLOCK.SPI_CLOCK_FOSC_2
                SPI_Clock_Divider = 2
            Case SPI_CLOCK.SPI_CLOCK_FOSC_4
                SPI_Clock_Divider = 4
            Case SPI_CLOCK.SPI_CLOCK_FOSC_8
                SPI_Clock_Divider = 8
            Case SPI_CLOCK.SPI_CLOCK_FOSC_16
                SPI_Clock_Divider = 16
            Case SPI_CLOCK.SPI_CLOCK_FOSC_32
                SPI_Clock_Divider = 32
            Case SPI_CLOCK.SPI_CLOCK_FOSC_64
                SPI_Clock_Divider = 64
            Case SPI_CLOCK.SPI_CLOCK_FOSC_128
                SPI_Clock_Divider = 128
            Case Else
                SPI_Clock_Divider = 2
        End Select
    End Sub

    Private Function GetArrayWithCmdAndAddr(ByVal cmd As Byte, ByVal addr_offset As UInt32) As Byte()
        Dim addr_data() As Byte = BitConverter.GetBytes(addr_offset)
        ReDim Preserve addr_data(MyFlashDevice.Definition.AddressBytes - 1)
        Array.Reverse(addr_data)
        Dim data_out(MyFlashDevice.Definition.AddressBytes) As Byte
        data_out(0) = cmd
        For i = 1 To data_out.Length - 1
            data_out(i) = addr_data(i - 1)
        Next
        Return data_out
    End Function

#Region "Custom device"
    Public CustomDevice As SpiFlashDevice
    Public UseCustom As Boolean = False
    Private Hardcode_Clock As SPI_CLOCK = SPI_CLOCK.SPI_UNSPECIFIED
    Private Hardcode_Mode As SPI_MODE = SPI_MODE.SPI_UNSPECIFIED
    Private Hardcode_Order As SPI_ORDER = SPI_ORDER.SPI_UNSPECIFIED

    Public Sub SpecifyClock(ByVal Divider As Integer)
        Select Case Divider
            Case 2
                Hardcode_Clock = SPI_CLOCK.SPI_CLOCK_FOSC_2
                WriteConsole("SPI clock mode set to Fosc/2 (8 MHz)")
            Case 4
                Hardcode_Clock = SPI_CLOCK.SPI_CLOCK_FOSC_4
                WriteConsole("SPI clock mode set to Fosc/4 (4 MHz)")
            Case 8
                Hardcode_Clock = SPI_CLOCK.SPI_CLOCK_FOSC_8
                WriteConsole("SPI clock mode set to Fosc/8 (2 MHz)")
            Case 16
                Hardcode_Clock = SPI_CLOCK.SPI_CLOCK_FOSC_16
                WriteConsole("SPI clock mode set to Fosc/16 (1 MHz)")
            Case 32
                Hardcode_Clock = SPI_CLOCK.SPI_CLOCK_FOSC_32
                WriteConsole("SPI clock mode set to Fosc/32 (500 KHz)")
            Case 64
                Hardcode_Clock = SPI_CLOCK.SPI_CLOCK_FOSC_64
                WriteConsole("SPI clock mode set to Fosc/64 (250 KHz)")
            Case 128
                Hardcode_Clock = SPI_CLOCK.SPI_CLOCK_FOSC_128
                WriteConsole("SPI clock mode set to Fosc/128 (125 KHz)")
            Case 0 'Removes hardcode
                Hardcode_Clock = SPI_CLOCK.SPI_UNSPECIFIED
                WriteConsole("SPI clock set to auto configure")
            Case -1 'Removes hardcode
                Hardcode_Clock = SPI_CLOCK.SPI_UNSPECIFIED
                WriteConsole("SPI clock set to auto configure")
            Case Else
                WriteConsole("Error setting SPI clock, speed divider unknown: " & CStr(Divider))
        End Select
    End Sub

    Public Sub SpecifyOrder(ByVal Order As String)
        Select Case Order
            Case """MSB"""
                Hardcode_Order = SPI_ORDER.SPI_ORDER_MSB_FIRST
                WriteConsole("SPI bit order set to MSB")
            Case """LSB"""
                Hardcode_Order = SPI_ORDER.SPI_ORDER_LSB_FIRST
                WriteConsole("SPI bit order set to LSB")
            Case "" 'Remove
                Hardcode_Order = SPI_ORDER.SPI_UNSPECIFIED
                WriteConsole("SPI bit order set to auto configure")
            Case Else 'Error
                WriteConsole("Error setting SPI bit order: must be MSB or LSB")
        End Select
    End Sub

    Public Sub SpecifyMode(ByVal Mode As Integer)
        Select Case Mode
            Case 0
                Hardcode_Mode = SPI_MODE.SPI_MODE_0
                WriteConsole(String.Format(RM.GetString("fcusb_spi_modesetto"), 0))
            Case 1
                Hardcode_Mode = SPI_MODE.SPI_MODE_1
                WriteConsole(String.Format(RM.GetString("fcusb_spi_modesetto"), 1))
            Case 2
                Hardcode_Mode = SPI_MODE.SPI_MODE_2
                WriteConsole(String.Format(RM.GetString("fcusb_spi_modesetto"), 2))
            Case 3
                Hardcode_Mode = SPI_MODE.SPI_MODE_3
                WriteConsole(String.Format(RM.GetString("fcusb_spi_modesetto"), 3))
            Case -1 'Remove
                Hardcode_Mode = SPI_MODE.SPI_UNSPECIFIED
                WriteConsole(RM.GetString("fcusb_spi_modesetauto"))
            Case Else
                WriteConsole(String.Format(RM.GetString("fcusb_spi_modenotsupported"), Mode))
        End Select
    End Sub

#End Region

#Region "Constants"

    Private Const SPIREQ_ECHO As Byte = &H80
    Private Const SPIREQ_LEDON As Byte = &H81
    Private Const SPIREQ_LEDOFF As Byte = &H82
    Private Const SPIREQ_LEDBLINK As Byte = &H83
    Private Const SPIREQ_SETCFG As Byte = &H84
    Private Const SPIREQ_ENPROGIF As Byte = &H85
    Private Const SPIREQ_VERSION As Byte = &H86
    Private Const SPIREQ_WRITEDATA As Byte = &H87
    Private Const SPIREQ_READDATA As Byte = &H88
    Private Const SPIREQ_SS_HIGH As Byte = &H89
    Private Const SPIREQ_SS_LOW As Byte = &H8A
    Private Const SPIREQ_READBULK As Byte = &H8B
    Private Const SPIREQ_WRITEBULK As Byte = &H8C
    Private Const SPIREQ_WRITEBULK_AAIBYTE As Byte = &H8D
    Private Const SPIREQ_WRITEBULK_AAIWORD As Byte = &H8E
    Private Const SPIREQ_WRITEBULK_RAW As Byte = &H8F

    Private Const MB001 As UInt32 = 131072
    Private Const MB002 As UInt32 = 262144
    Private Const MB004 As UInt32 = 524288
    Private Const MB008 As UInt32 = 1048576
    Private Const MB016 As UInt32 = 2097152
    Private Const MB032 As UInt32 = 4194304
    Private Const MB064 As UInt32 = 8388608
    Private Const MB128 As UInt32 = 16777216
    Private Const MB256 As UInt32 = 33554432
    Private Const MB512 As UInt32 = 67108864
    Private Const MB1Gb As UInt32 = 134217728


    Private Const NO_ID_READ As Integer = &HFFFFFF

    Private PCT_4KB_BYTE As SpiDef
    Private PCT_4KB_WORD As SpiDef
    Private PCT_32KB As SpiDef
    Private PCT_64KB As SpiDef
    Private SPI_4KB As SpiDef
    Private SPI_32KB As SpiDef
    Private SPI_64KB As SpiDef
    Private SPI_2MBT As SpiDef '2mbit per sector
    Private SPI_HDSP As SpiDef 'High density
    Private SPI_HD64 As SpiDef 'High density (for MX/Micron ST)
    Private Generic As SpiDef
    Private SST_64KB As SpiDef
    Private PMC_4KB As SpiDef
    Private SST_4KB As SpiDef
    Private AT45DB_PS264 As SpiDef
    Private AT45DB_PS528 As SpiDef
    Private AT45DB_PS1056 As SpiDef

#End Region

#Region "Enumerators"

    Enum SPI_CLOCK As Byte
        SPI_CLOCK_FOSC_2 = &H80
        SPI_CLOCK_FOSC_4 = &H0
        SPI_CLOCK_FOSC_8 = &H81
        SPI_CLOCK_FOSC_16 = &H1
        SPI_CLOCK_FOSC_32 = &H82
        SPI_CLOCK_FOSC_64 = &H2 'or 0x83
        SPI_CLOCK_FOSC_128 = &H3
        SPI_UNSPECIFIED = &HFF
    End Enum

    Enum SPI_MODE As Byte
        SPI_MODE_0 = &H0
        SPI_MODE_1 = &H4
        SPI_MODE_2 = &H8
        SPI_MODE_3 = &HC
        SPI_UNSPECIFIED = &HFF
    End Enum

    Enum SPI_ORDER As Byte
        SPI_ORDER_MSB_FIRST = &H0
        SPI_ORDER_LSB_FIRST = &H20
        SPI_UNSPECIFIED = &HFF
    End Enum

    Enum ProgramMode
        PageMode = 0
        AAI_Byte = 1
        AAI_Word = 2
        AtmelLegacy = 3
        Nordic = 4
    End Enum

    Enum Status
        NotDetected = 0
        Supported = 1
        NotSupported = 2
    End Enum

    Private Function GetSpiOrders() As SPI_ORDER()
        If Hardcode_Order = SPI_ORDER.SPI_UNSPECIFIED Then
            Dim ret(1) As SPI_ORDER
            ret(0) = SPI_ORDER.SPI_ORDER_MSB_FIRST
            ret(1) = SPI_ORDER.SPI_ORDER_LSB_FIRST
            Return ret
        Else
            Dim ret(0) As SPI_ORDER
            ret(0) = Hardcode_Order
            Return ret
        End If
    End Function

    Private Function GetSpiClocks() As SPI_CLOCK()
        If Hardcode_Clock = SPI_CLOCK.SPI_UNSPECIFIED Then
            Dim ret(3) As SPI_CLOCK
            ret(0) = SPI_CLOCK.SPI_CLOCK_FOSC_2
            ret(1) = SPI_CLOCK.SPI_CLOCK_FOSC_4
            ret(2) = SPI_CLOCK.SPI_CLOCK_FOSC_8
            ret(3) = SPI_CLOCK.SPI_CLOCK_FOSC_16
            'ret(4) = SPI_CLOCK.SPI_CLOCK_FOSC_32
            'ret(5) = SPI_CLOCK.SPI_CLOCK_FOSC_64
            'ret(6) = SPI_CLOCK.SPI_CLOCK_FOSC_128
            Return ret
        Else
            Dim ret(0) As SPI_CLOCK
            ret(0) = Hardcode_Clock
            Return ret
        End If
    End Function

    Private Function GetSpiModes() As SPI_MODE()
        If Hardcode_Mode = SPI_MODE.SPI_UNSPECIFIED Then
            Dim ret(3) As SPI_MODE
            ret(0) = SPI_MODE.SPI_MODE_0
            ret(1) = SPI_MODE.SPI_MODE_1
            ret(2) = SPI_MODE.SPI_MODE_2
            ret(3) = SPI_MODE.SPI_MODE_3
            Return ret
        Else
            Dim ret(0) As SPI_MODE
            ret(0) = Hardcode_Mode
            Return ret
        End If
    End Function

    Private Function ModeToStr(ByVal Mode As SPI_MODE) As String
        Select Case Mode
            Case SPI_MODE.SPI_MODE_0
                Return "0"
            Case SPI_MODE.SPI_MODE_1
                Return "1"
            Case SPI_MODE.SPI_MODE_2
                Return "2"
            Case SPI_MODE.SPI_MODE_3
                Return "3"
            Case Else
                Return "0"
        End Select
    End Function

    Private Function ClockToStr(ByVal Clk As SPI_CLOCK) As String
        Select Case Clk
            Case SPI_CLOCK.SPI_CLOCK_FOSC_2
                Return "2"
            Case SPI_CLOCK.SPI_CLOCK_FOSC_4
                Return "4"
            Case SPI_CLOCK.SPI_CLOCK_FOSC_8
                Return "8"
            Case SPI_CLOCK.SPI_CLOCK_FOSC_16
                Return "16"
            Case SPI_CLOCK.SPI_CLOCK_FOSC_32
                Return "32"
            Case SPI_CLOCK.SPI_CLOCK_FOSC_64
                Return "64"
            Case SPI_CLOCK.SPI_CLOCK_FOSC_128
                Return "128"
            Case Else
                Return "2"
        End Select
    End Function

    Private Function OrderToStr(ByVal Order As SPI_ORDER) As String
        Select Case Order
            Case SPI_ORDER.SPI_ORDER_MSB_FIRST
                Return "MSB"
            Case SPI_ORDER.SPI_ORDER_LSB_FIRST
                Return "LSB"
            Case Else
                Return "MSB"
        End Select
    End Function

#End Region

#Region "LIBUSBDOTNET Calls"

    Private Function EchoTest() As Boolean
        Try
            If Not IsOpened() Then Return False
            Dim buff As Byte() = New Byte(7) {}
            Dim usbSetupPacket As New UsbSetupPacket(CByte(UsbCtrlFlags.Direction_In Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor), CByte(SPIREQ_ECHO), &H1234, &H5678, 8)
            Dim ret As Integer
            If fcusb.ControlTransfer(usbSetupPacket, buff, 8, ret) Then
                If buff(0) <> CByte(UsbCtrlFlags.Direction_In Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor) Then
                    Return False
                End If
                If buff(1) <> CByte(SPIREQ_ECHO) Then Return False
                If buff(2) <> &H34 Then Return False
                If buff(3) <> &H12 Then Return False
                If buff(4) <> &H78 Then Return False
                If buff(5) <> &H56 Then Return False
                If buff(6) <> &H8 Then Return False
                If buff(7) <> &H0 Then Return False
                Return True
            End If
            Return False
        Catch ex As Exception
            Return False
        End Try
    End Function

    Public Function SetDeviceConfig(ByVal clock As SPI_CLOCK, ByVal mode As SPI_MODE, Optional ByVal order As SPI_ORDER = SPI_ORDER.SPI_ORDER_MSB_FIRST) As Boolean
        If Not IsOpened() Then Return False
        Dim spiConf As Short = CShort(CByte(clock) Or CByte(mode) Or CByte(order))
        Dim usbSetupPacket As New UsbSetupPacket(CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor), SPIREQ_SETCFG, spiConf, 0, 0)
        Dim ret As Integer
        If fcusb.ControlTransfer(usbSetupPacket, Nothing, 0, ret) Then
            UpdateClockDevider(clock)
            Return True
        End If
        Return False
    End Function
    'Pulses the RESET pin to enable device program mode for SPI slave (nRF24LE1)
    Public Sub EnableProgMode()
        If Not IsOpened() Then Exit Sub
        Dim DirectionFlag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor)
        Dim usbSetupPacket As New UsbSetupPacket(DirectionFlag, SPIREQ_ENPROGIF, 0, 0, 0)
        Dim ret As Integer
        fcusb.ControlTransfer(usbSetupPacket, Nothing, 0, ret)
    End Sub

    Public Function GetAvrVersion() As String
        If Not IsOpened() Then Return Nothing
        Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_In Or UsbCtrlFlags.Recipient_Device Or UsbCtrlFlags.RequestType_Vendor)
        Dim usbSetupPacket As New UsbSetupPacket(usbflag, SPIREQ_VERSION, 0, 0, 4)
        Dim ret As Integer
        Dim buff(4) As Byte
        If fcusb.ControlTransfer(usbSetupPacket, buff, 4, ret) Then
            buff(4) = buff(3)
            buff(3) = buff(2)
            buff(2) = Asc(".")
        End If
        Dim fwstr As String = BytesToString(buff)
        Return StrToSingle(fwstr).ToString
    End Function
    'Controls the SPI line by writing the data in buffer1 and reading the data into buffer2. Returns total bytes write and read
    Public Function SPIBUS_WriteRead(ByVal WriteBuffer() As Byte, ByRef ReadBuffer() As Byte) As UInt32
        If WriteBuffer Is Nothing And ReadBuffer Is Nothing Then Return 0
        If WriteBuffer IsNot Nothing AndAlso WriteBuffer.Length > 64 Then Return 0
        If ReadBuffer IsNot Nothing AndAlso ReadBuffer.Length > 64 Then Return 0
        Dim TotalBytesTransfered As UInt32 = 0
        If WriteBuffer IsNot Nothing Then
            Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
            Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_WRITEDATA, 0, 0, CShort(WriteBuffer.Length))
            Dim ret As Integer = 0
            Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, WriteBuffer, WriteBuffer.Length, ret)
            If res Then TotalBytesTransfered += ret
        Else
            SlaveSelect_Enable()
        End If
        If ReadBuffer IsNot Nothing Then
            Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_In Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
            Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_READDATA, 0, 0, CShort(ReadBuffer.Length))
            Dim ret As Integer = 0
            Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, ReadBuffer, ReadBuffer.Length, ret)
            If res Then TotalBytesTransfered += ret
        ElseIf WriteBuffer IsNot Nothing Then
            SlaveSelect_Disable()
        End If
        Return TotalBytesTransfered
    End Function
    'Makes the SS pin go low
    Public Sub SlaveSelect_Enable()
        Dim ret As Integer = 0
        Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
        Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_SS_HIGH, 0, 0, 0)
        Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, Nothing, 0, ret)
    End Sub
    'Releases the SS pin
    Public Sub SlaveSelect_Disable()
        Dim ret As Integer = 0
        Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
        Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_SS_LOW, 0, 0, 0)
        Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, Nothing, 0, ret)
    End Sub

    Public Sub LEDOn()
        Dim ret As Integer = 0
        Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
        Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_LEDON, 0, 0, 0)
        Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, Nothing, 0, ret)
    End Sub

    Public Sub LEDOff()
        Dim ret As Integer = 0
        Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
        Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_LEDOFF, 0, 0, 0)
        Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, Nothing, 0, ret)
    End Sub

    Public Sub LEDBlink()
        Dim ret As Integer = 0
        Dim usbflag As Byte = CByte(UsbCtrlFlags.Direction_Out Or UsbCtrlFlags.RequestType_Vendor Or UsbCtrlFlags.Recipient_Device)
        Dim usbPacket2 As New UsbSetupPacket(usbflag, SPIREQ_LEDBLINK, 0, 0, 0)
        Dim res As Boolean = fcusb.ControlTransfer(usbPacket2, Nothing, 0, ret)
    End Sub

#End Region


End Class
