'COPYRIGHT EMBEDDEDCOMPUTERS.NET 2012 - ALL RIGHTS RESERVED
'CONTACT EMAIL: contact@embeddedcomputers.net
'ANY USE OF THIS CODE MUST ADHERE TO THE LICENSE FILE INCLUDED WITH THIS SDK
'INFO: This module contains commonly used basis functions

Module Common

    Public Enum AvrMode
        NotConnected = 0
        JTAG = 1
        SPI = 2
        DFU = 3
        NAND = 4
    End Enum

#Region "File Type Constants"
    Public Const AllF As String = "All files (*.*)|*.*"
    Public Const sym As String = "Sym File (*.sym)|*.sym"
    Public Const Prj As String = "Project File (*.prj)|*.prj"
    Public Const asm As String = "Asm File (*.asm)|*.asm"
    Public Const cbl As String = "Compressed Bootloader (*.cbl)|*.cbl"
    Public Const TxtF As String = "Text Files (*.txt)|*.txt"
    Public Const FbF As String = "FB Patch (*.ptx)|*.ptx"
    Public Const ftHex As String = "Intel Hex Format (*.hex)|*.hex"
    Public Const BinFile As String = "Binary Files (*.bin)|*.bin"
    Public Const FirmFile As String = "Firmware Files (*.hex.bin)|*.hex.bin"
    Public Const P7Type As String = "P7 Files (*.p7)|*.p7"
    Public Const StFile As String = "ST Files (*.st)|*.st"
    Public Const BcFname As String = "Blackcat Scripts (*.bcs)|*.bcs"
    Public Const ptxFile As String = "PTX Files (*.ptx)|*.ptx"
    Public Const FBPRJ As String = "FB Project (*.prj)|*.prj"
    Public Const stype As String = "S File (*.s)|*.s"
    Public Const FwTypes As String = ftHex & "|" & AllF
    Public Const CommonTypes As String = FbF & "|" & AllF
    Public Const BootRoms As String = StFile & "|" & ftHex & "|" & AllF
    Public Const FirmwareFilter As String = FirmFile & "|" & P7Type & "|" & AllF
    Public Const BlackcatScript As String = BcFname & "|" & AllF
    Public Const BootloaderFilter As String = cbl & "|" & BinFile & "|" & AllF
    Public Const SaveFromRamFilter As String = BinFile & "|" & ftHex & "|" & AllF
    Public Const SecCert As String = "Certificate (*.cert)|*.cert" & "|" & AllF
    Public Const BinFiles As String = BinFile & "|" & AllF
    Public Const BinHexFiles As String = BinFile & "|" & ftHex & "|" & AllF
#End Region
    'Converts a integer up to 24 bits to a byte array
    Public Function IntTo3Bytes(ByVal input As Integer) As Byte()
        Dim ret(2) As Byte
        ret(0) = CByte((input And &HFF0000) >> 16)
        ret(1) = CByte((input And &HFF00) >> 8)
        ret(2) = CByte(input And &HFF)
        Return ret
    End Function

    Public Function BytesToUint32(ByVal data() As Byte, Optional ByVal Offset As UInt32 = 0, Optional ByVal Reverse As Boolean = False) As UInt32
        If data Is Nothing OrElse data.Length < 4 Then Return 0
        Dim sum As UInt32
        If Reverse Then
            sum = CUInt(data(Offset + 3)) << 24
            sum += CUInt(data(Offset + 2)) << 16
            sum += CUInt(data(Offset + 1)) << 8
            sum += CUInt(data(Offset + 0))
        Else
            sum = CUInt(data(Offset + 0)) << 24
            sum += CUInt(data(Offset + 1)) << 16
            sum += CUInt(data(Offset + 2)) << 8
            sum += CUInt(data(Offset + 3))
        End If
        Return sum
    End Function
    'Converts a string that contains a number to single (all region compatible)
    Public Function StrToSingle(ByVal input As String) As Single
        Return Convert.ToSingle(input, New System.Globalization.CultureInfo("en-US"))
    End Function
    'Adds two U32 and overflows
    Public Sub AddWord(ByRef Dest As UInt32, ByVal ToAdd As UInt32)
        Dim ultra As UInt64 = Dest 'Larger than 32 bits to hold the overflow
        ultra = ultra + ToAdd 'This overflows the value
        Dest = CUInt((CLng(ultra) And &HFFFFFFFF))
    End Sub

    Public Function SectionChecksum(ByVal section() As Byte) As UInt32
        Dim i As Integer
        Dim sum As UInt32 = 0L
        Dim s As UInt32
        Dim WORDTOT As Integer = CInt((Math.Floor(section.Length / 4)))
        For i = 0 To WORDTOT - 1
            s = CUInt((section(i * 4) * 16777216L))
            s = CUInt(s + (section((i * 4) + 1) * 65536))
            s = CUInt(s + (section((i * 4) + 2) * 256))
            s = s + (section((i * 4) + 3))
            AddWord(sum, s)
        Next
        Dim TOP As Integer = (WORDTOT * 4)
        Dim LEFT As Integer = section.Length - TOP 'Number of bytes left
        Dim TWOBYTE As UInt32 = 0
        Dim ONEBYTE As UInt32 = 0
        If LEFT = 3 Then
            TWOBYTE = CUInt((section(TOP) * 256) + (section(TOP + 1)))
            ONEBYTE = (section(TOP + 2))
        ElseIf LEFT = 2 Then
            TWOBYTE = CUInt((section(TOP) * 256) + (section(TOP + 1)))
        ElseIf LEFT = 1 Then
            ONEBYTE = section(TOP)
        End If
        TWOBYTE = (TWOBYTE << 8)
        TWOBYTE = (TWOBYTE Or ONEBYTE)
        TWOBYTE = (TWOBYTE << 8)
        AddWord(sum, TWOBYTE)
        Return CUInt((sum Xor &HFFFFFFFFL))
    End Function

    Public Sub SaveFileFromRead(ByVal data() As Byte, ByVal DefaultName As String)
        Dim Saveme As New SaveFileDialog
        Saveme.AddExtension = True
        Saveme.InitialDirectory = Application.StartupPath
        Saveme.Title = RM.GetString("fcusb_filesave_type")
        Saveme.CheckPathExists = True
        Saveme.FileName = DefaultName
        Saveme.Filter = BinHexFiles
        If Saveme.ShowDialog = Windows.Forms.DialogResult.OK Then
            If Saveme.FileName.ToUpper.EndsWith(".HEX") Then
                data = BinToIntelHex(data)
                WriteConsole(RM.GetString("fcusb_filesave_tohex"))
            End If
            WriteBytes(data, Saveme.FileName) 'Writes buffer of bytes to Disk
            SetStatus(String.Format(RM.GetString("fcusb_filesave_sucess"), DefaultName))
        Else
            SetStatus(RM.GetString("fcusb_filesave_canceled"))
        End If
    End Sub

    Public Sub WriteBytes(ByVal bytes() As Byte, ByVal FileName As String)
        If Not Dir(FileName).Length = 0 Then Kill(FileName)
        Dim bLength As Integer = UBound(bytes)
        Dim i As Single
        Dim Buffer As Integer = 2048
        Dim offset As Integer = 0
        Dim n As Integer = CInt(Math.Ceiling(bLength / Buffer) - 1)
        Dim FS As System.IO.FileStream
        FS = New IO.FileStream(FileName, IO.FileMode.Create)
        For i = 0 To n
            If offset + Buffer > bytes.Length Then
                FS.Write(bytes, offset, bytes.Length - offset)
            Else
                FS.Write(bytes, offset, Buffer)
            End If
            offset = offset + Buffer
            Application.DoEvents()
        Next
        FS.Flush()
        FS.Close()
    End Sub

    Public Function ReadBytes(ByVal fileName As String) As Byte()
        Dim bytesOut() As Byte
        Dim BR As New IO.BinaryReader(IO.File.OpenRead(fileName))
        Dim FileLength As Integer = CInt(BR.BaseStream.Length - 1)
        ReDim bytesOut(FileLength)
        For i = 0 To FileLength
            bytesOut(CInt(i)) = BR.ReadByte
        Next
        BR.Close()
        Return bytesOut
    End Function

    Public Function ReadFile(ByVal fileName As String) As String()
        Dim keyFile As String
        Dim fileLine As String
        Dim counter As Integer = 0
        Dim fileContents() As String
        keyFile = fileName
        ReDim fileContents(0)
        If Dir(keyFile).Length = 0 Then Return Nothing
        Dim rFile As IO.StreamReader = IO.File.OpenText(keyFile)
        fileLine = (rFile.ReadLine)
        Do
            ReDim Preserve fileContents(counter)
            fileContents(counter) = fileLine
            counter = counter + 1
            fileLine = (rFile.ReadLine)
        Loop Until rFile.Peek = -1
        ReDim Preserve fileContents(counter)
        fileContents(counter) = fileLine
        rFile.Close()
        Return fileContents
    End Function

    Public Sub WriteFile(ByRef FileOut() As String, ByVal fileName As String)
        If FileOut Is Nothing Then Exit Sub
        Dim destFile As New IO.FileInfo(fileName)
        If destFile.Exists Then destFile.Delete()
        Dim destDir As New IO.DirectoryInfo(destFile.DirectoryName)
        If Not destDir.Exists Then destDir.Create()
        Dim wFile As IO.StreamWriter = IO.File.CreateText(destFile.FullName)
        Dim line As String
        For Each line In FileOut
            If line.Length = 0 Then wFile.WriteLine() Else wFile.WriteLine(line)
        Next
        wFile.Close()
    End Sub

    Public Function GetStringArray_Bytes(ByVal b() As Byte) As String()
        Dim fileout As New ArrayList
        Dim s As New IO.MemoryStream(b)
        Dim rFile As New IO.StreamReader(s)
        Do
            fileout.Add(rFile.ReadLine)
        Loop Until rFile.Peek = -1
        Return DirectCast(fileout.ToArray(GetType(String)), String())
    End Function

    Public Function HexToInt(ByVal value As String) As Int32
        Try
            If UCase(value).StartsWith("0X") Then value = Mid(value, 3)
            If value = "" Then Return 0
            Return Convert.ToInt32(value, 16)
        Catch
            Return 0
        End Try
    End Function

    Public Function HexToLng(ByVal value As String) As Long
        Try
            If UCase(value).StartsWith("0X") Then value = Mid(value, 3)
            If value = "" Then Return 0
            Return Convert.ToInt64(value, 16)
        Catch
            Return 0
        End Try
    End Function

    Public Function BytestoHex(ByVal d() As Byte) As String
        Dim i As Integer
        Dim Out As String = ""
        For i = 0 To d.Length - 1
            Out = Out & Hex(d(i)).PadLeft(2, CChar("0"))
        Next
        Return Out
    End Function

    Public Function HexToUint(ByVal input As String) As UInt32
        Try
            If UCase(input).StartsWith("0X") Then input = Mid(input, 3)
            If input = "" Then Return 0
            Return Convert.ToUInt32(input, 16)
        Catch ex As Exception
            Return 0
        End Try
    End Function

    Public Function isHex(ByVal input As String) As Boolean
        If UCase(input).StartsWith("0X") Then input = Mid(input, 3)
        Dim i As Integer
        For i = 1 To input.Length
            Dim ine As String = Mid(input, i, 1)
            If Not IsNumeric(ine) Then
                Select Case UCase(ine)
                    Case "A"
                    Case "B"
                    Case "C"
                    Case "D"
                    Case "E"
                    Case "F"
                    Case Else
                        Return False
                End Select
            End If
        Next
        Return True
    End Function
    'Converts a data string (0x80;0x81 etc) to a byte array
    Public Function ConvertDataString(ByVal dataline As String) As Byte()
        If dataline.StartsWith(";") Then dataline = Mid(dataline, 2)
        If dataline.EndsWith(";") Then dataline = Mid(dataline, 1, dataline.Length - 1)
        If InStr(dataline, ";") = 0 Then Return Nothing
        Dim p() As String = dataline.Split(CChar(";"))
        Dim data(p.Length - 1) As Byte
        Dim i As Integer
        Dim Val As Integer
        For i = 0 To p.Length - 1
            If IsNumeric(p(i)) Then
                Val = CInt(p(i))
            ElseIf isHex(p(i)) Then
                Val = HexToInt(p(i))
            Else
                Return Nothing
            End If
            If Not Val < 256 Then Return Nothing
            data(i) = CByte(Val)
        Next
        Return data
    End Function
    'Checks to see if the data string is a byte array: 0x80;0x81 etc
    Public Function IsData(ByVal data As String) As Boolean
        If InStr(data, CChar(";")) = 0 Then Return False
        Dim p() As String = data.Split(CChar(";"))
        Dim i As Integer
        For i = 0 To p.Length - 1
            If IsNumeric(p(i)) Then
            ElseIf isHex(p(i)) Then
            Else
                Return False
            End If
        Next
        Return True
    End Function

    Public Function isString(ByVal input As String) As Boolean
        If input.StartsWith("""") And input.EndsWith("""") Then Return True
        Return False
    End Function

    Public Function IntToHex(ByVal value As Long) As String
        Dim nValue As String = Hex(value)
        If nValue.Length = 1 Then nValue = "0" & nValue
        Return nValue
    End Function

    Public Function IntToHexStr(ByVal value As Long) As String
        Return "0x" & Hex(value)
    End Function

    Public Function GetManu(ByVal ManuID As Integer) As String
        Select Case ManuID
            Case 1
                Return "Spansion"
            Case 4
                Return "Fujitsu"
            Case 7
                Return "Hitachi"
            Case 9
                Return "Intel"
            Case 21
                Return "Philips"
            Case 31
                Return "Atmel"
            Case 32
                Return "ST"
            Case 52
                Return "Cypress"
            Case 53
                Return "DEC"
            Case 73
                Return "Xilinx"
            Case 110
                Return "Altera"
            Case 112 '0x70
                Return "QUALCOMM"
            Case 191 '0xBF
                Return "Broadcom"
            Case 194
                Return "MXIC"
            Case 239
                Return "Winbond"
            Case 336
                Return "Signetics"
            Case Else
                Return Hex(ManuID) ' Not Found
        End Select
    End Function

    Public Function WordToUint32(ByVal i1 As Byte, ByVal i2 As Byte, ByVal i3 As Byte, ByVal i4 As Byte) As UInt32
        Return CUInt(i4 + (i3 * 256) + (i2 * 65536) + (i1 * 16777216))
    End Function

    Public Sub Uint32toWord(ByVal Word As UInt32, ByRef i1 As Byte, ByRef i2 As Byte, ByRef i3 As Byte, ByRef i4 As Byte)
        Dim b() As Byte = SwapEndian(BitConverter.GetBytes(Word))
        i1 = b(0)
        i2 = b(1)
        i3 = b(2)
        i4 = b(3)
    End Sub
    'Switches endian in a byte array
    Public Function SwapEndian(ByVal Input() As Byte) As Byte()
        Dim i As Integer
        Dim ByteLen As Integer = Input.Length - 1
        Dim Output(ByteLen) As Byte
        Dim counter As Integer = 0
        For i = 0 To ByteLen
            Output(counter) = Input(ByteLen - i)
            counter = counter + 1
        Next
        ReDim Preserve Output(counter - 1)
        Return Output
    End Function

    Public Function BytesToString(ByVal B() As Byte) As String
        Dim i As Integer
        If B Is Nothing Then Return ""
        Dim S As String = ""
        Dim y As Int16
        For i = 0 To B.Length - 1
            y = B(i)
            If y = 0 Then Return S
            S = S & Chr(y)
        Next
        Return S
    End Function

    Public Function HexStringToBytes(ByVal DataIN As String) As Byte()
        Dim i As Single
        Dim bytes() As Byte
        Dim TotalBytes As Long = CLng(Len(DataIN) / 2 - 1)
        ReDim Preserve bytes(CInt(TotalBytes))
        For i = 0 To TotalBytes
            bytes(CInt(i)) = CByte(HexToInt(Mid(DataIN, CInt(i * 2 + 1), 2)))
        Next
        Return bytes
    End Function

    Public Function BytesToHexString(ByVal Bytes() As Byte) As String
        If Bytes Is Nothing Then Return ""
        Dim StrOut As String = ""
        Dim b As Byte
        For Each b In Bytes
            StrOut = StrOut & IntToHex(b)
        Next
        Return StrOut
    End Function

    Public Function StringToBytes(ByVal textIn As String) As Byte()
        If textIn Is Nothing Then Return Nothing
        Dim UE As New System.Text.UnicodeEncoding
        Dim MessageBytes As Byte() = UE.GetBytes(textIn)
        Dim newArray As Byte() = Nothing
        Dim counter As Single = 0
        Dim i As Single
        For i = 0 To UBound(MessageBytes)
            If Not MessageBytes(CInt(i)) = 0 Then
                ReDim Preserve newArray(CInt(counter))
                newArray(CInt(counter)) = MessageBytes(CInt(i))
                counter = counter + 1
            End If
        Next
        Return newArray
    End Function

    Public Function ParseToMac(ByVal MacAddress As String) As String
        Dim i As Integer
        Dim sOut As String = ""
        For i = 0 To 5
            sOut &= Mid(MacAddress, (i * 2) + 1, 2) & ":"
        Next
        Return Mid(sOut, 1, sOut.Length - 1)
    End Function
    'Checks to see if input valid is Hex : 0xFF etc
    Public Function ConvertFromHex(ByRef Input As String) As Boolean
        Dim InputU As String = UCase(Input)
        If InputU.StartsWith("0X") Then
            Dim i As Integer
            Dim S As String
            InputU = Mid(InputU, 3)
            For i = 1 To InputU.Length
                S = Mid(InputU, i, 1)
                If Not IsNumeric(S) Then
                    Select Case S
                        Case "A"
                        Case "B"
                        Case "C"
                        Case "D"
                        Case "E"
                        Case "F"
                        Case Else
                            Return False
                    End Select
                End If
            Next
        Else
            Return False
        End If
        If InputU.Length > 8 Then
            'WriteConsole("Error: Argument is greater than 32 bits: " & Input)
            Return False
        Else
            Input = CStr(HexToInt(InputU))
            Return True
        End If
    End Function
    'Downloads a file from the internet and returns it as a byte array
    Public Function WebDownload(ByVal WebFile As String) As Byte()
        Try
            Dim wr As Net.HttpWebRequest = CType(Net.WebRequest.Create(WebFile), Net.HttpWebRequest)
            Dim ws As Net.HttpWebResponse = CType(wr.GetResponse(), Net.HttpWebResponse)
            Dim str As IO.Stream = ws.GetResponseStream()
            Dim inBuf(100000) As Byte
            Dim bytesToRead As Integer = CInt(inBuf.Length)
            Dim bytesRead As Integer = 0
            While bytesToRead > 0
                Dim n As Integer = str.Read(inBuf, bytesRead, bytesToRead)
                If n = 0 Then
                    Exit While
                End If
                bytesRead += n
                bytesToRead -= n
            End While
            ReDim Preserve inBuf(bytesRead - 1)
            Return inBuf
        Catch ex As Exception
            Return Nothing
        End Try
    End Function
    'Parses a byte and returns the 2 char hex str
    Public Function Hex2(ByVal b As Byte) As String
        Return Hex(b).PadLeft(2, CChar("0"))
    End Function

    Public Function IntToTwoBytes(ByVal i As Integer) As Byte()
        Dim b(1) As Byte
        Dim Hval As Integer = CInt(Math.Floor(i / 256))
        b(0) = CByte(Hval)
        i = i - (Hval * 256)
        b(1) = CByte(i)
        Return b
    End Function

    Public Function IntToFourBytes(ByVal i As UInt32) As Byte()
        Dim b(3) As Byte
        Dim Hval As Integer
        Hval = CInt(Math.Floor(i / 16777216))
        b(0) = CByte(Hval)
        i = CUInt(i - (Hval * 16777216))
        Hval = CInt(Math.Floor(i / 65536))
        b(1) = CByte(Hval)
        i = CUInt(i - (Hval * 65536))
        Hval = CInt(Math.Floor(i / 256))
        b(2) = CByte(Hval)
        i = CUInt(i - (Hval * 256))
        b(3) = CByte(i)
        Return b
    End Function

    Public Sub AddBytes(ByVal b() As Byte, ByRef byteList() As Byte)
        If byteList Is Nothing Then
            byteList = b
            Exit Sub
        End If
        Dim SourceIndex As Integer = byteList.Length
        ReDim Preserve byteList((SourceIndex + b.Length) - 1)
        Dim i As Integer
        For i = 0 To b.Length - 1
            byteList(SourceIndex + i) = b(i)
        Next
    End Sub
    'Returns TRUE if input is +,-,& (opperators)
    Public Function isOpper(ByVal InVal As String) As Boolean
        If InVal.Length = 1 Then
            Select Case InVal
                Case "+"
                    Return True
                Case "-"
                    Return True
                Case "&"
                    Return True
                Case Else
                    Return False
            End Select
        End If
        Return False
    End Function
    'Returns TRUE if input is a-z,A-Z
    Public Function isChar(ByVal InVal As String) As Boolean
        If InVal.Length = 1 Then
            InVal = UCase(InVal)
            If Asc(InVal) >= 65 And Asc(InVal) <= 90 Then Return True
        End If
        Return False
    End Function

    Public Function UintArrayToByteArray(ByVal words() As UInt32) As Byte()
        Dim ret((words.Length * 4) - 1) As Byte
        Dim i As Integer
        Dim counter As Integer = 0
        Dim q() As Byte
        For i = 0 To words.Length - 1
            q = UintToBytes(words(i))
            ret(counter) = q(0)
            ret(counter + 1) = q(1)
            ret(counter + 2) = q(2)
            ret(counter + 3) = q(3)
            counter += 4
        Next
        Return ret
    End Function

    Public Function UintToBytes(ByVal value As UInt32) As Byte()
        Dim ret(3) As Byte
        ret(0) = CByte(((value >> 24) And &HFF))
        ret(1) = CByte(((value >> 16) And &HFF))
        ret(2) = CByte(((value >> 8) And &HFF))
        ret(3) = CByte((value And &HFF))
        Return ret
    End Function
    'Converts a byte() into uint() padds the last element with 00s
    Public Function ByteArrayToUintArray(ByVal data() As Byte) As UInt32()
        Dim i As Integer
        Do Until (data.Length Mod 4) = 0
            ReDim Preserve data(data.Length)
        Loop
        Dim NumOfWords As Integer = CInt(data.Length / 4)
        Dim ret(NumOfWords - 1) As UInt32
        Dim sVal As UInt32
        Dim ival As UInt32
        For i = 0 To NumOfWords - 1
            Dim s As Integer = i * 4
            sVal = CUInt(data(s)) << 24
            ival = data(s + 1)
            sVal += (ival << 16)
            ival = data(s + 2)
            sVal += (ival << 8)
            sVal += data(s + 3)
            ret(i) = sVal
        Next
        Return ret
    End Function
    'Trims unicode strings (usefull after pulling strings from C++ DLLs)
    Public Function SpecialStrTrim(ByVal StrIn As String) As String
        Dim i As Integer
        Dim charcode As Integer
        Dim strOut As String = ""
        For i = 1 To StrIn.Length
            charcode = Asc(Mid(StrIn, i, 1))
            If charcode > 31 And charcode < 127 Then
                strOut = strOut & Chr(charcode)
            Else
                Return Trim(strOut)
            End If
        Next
        Return Trim(strOut)
    End Function
    'Removes a comment from a command line
    Public Function RemoveComment(ByVal input As String) As String
        Dim ret As String = ""
        Dim i As Integer
        Dim inQuote As Boolean = False
        For i = 1 To input.Length
            If inQuote Then
                If Mid(input, i, 1) = """" Then
                    inQuote = False
                End If
            Else
                If Mid(input, i, 1) = """" Then
                    inQuote = True
                ElseIf Mid(input, i, 1) = "#" Then 'We have comment
                    Return ret
                End If
            End If
            ret &= Mid(input, i, 1)
        Next
        Return ret
    End Function
    'Removes quotes from a string obj
    Public Function RemoveStr(ByVal input As String) As String
        If input Is Nothing Then Return Nothing
        If input = "" Then Return ""
        If input.StartsWith("""") And input.EndsWith("""") Then
            Return Mid(input, 2, input.Length - 2)
        Else
            Return input
        End If
    End Function
    'Splits a command line into command parts
    Public Function SplitCmd(ByVal cmdStr As String) As String()
        If cmdStr = "" Then Return Nothing
        Dim i As Integer
        Dim partCounter As Integer = -1
        Dim parts(31) As String 'Splits up to 32 parts
        Dim InStr As Boolean = False 'If we are in string 
        Dim InParam As Boolean = False 'If we are in parameter
        Dim MakeString As String = ""
        Dim sChr As Char
        For i = 0 To cmdStr.Length - 1
            sChr = CChar(Mid(cmdStr, i + 1, 1))
            If InStr Then
                If sChr = """" Then
                    MakeString &= """"
                    InStr = False
                Else
                    MakeString &= sChr
                End If
            ElseIf InParam Then
                If sChr = ")" Then
                    MakeString &= ")"
                    InParam = False
                Else
                    MakeString &= sChr
                End If
            Else
                If sChr = """" Then
                    MakeString &= """"
                    InStr = True
                ElseIf sChr = "(" Then
                    MakeString &= "("
                    InParam = True
                ElseIf sChr = " " Then
                    If Not MakeString = "" Then
                        partCounter = partCounter + 1
                        parts(partCounter) = MakeString
                        MakeString = ""
                    End If
                Else
                    MakeString &= sChr
                End If
            End If
            If i = cmdStr.Length - 1 Then 'last char
                If Not MakeString = "" Then
                    partCounter = partCounter + 1
                    parts(partCounter) = MakeString
                    MakeString = ""
                End If
            End If
        Next
        ReDim Preserve parts(partCounter)
        Return parts
    End Function
    'Returns the name of the flash device
    Public Function GetDeviceManufacture(ByVal ManuID As Byte) As String
        Select Case ManuID
            Case &H89
                Return "Intel"
            Case &H20
                Return "ST"
            Case &H2C
                Return "Micron"
            Case &H1
                Return "AMD / Spansion"
            Case &H98
                Return "TOSHIBA"
            Case &H4
                Return "FUJITSU"
            Case &HB0
                Return "SHARP"
            Case &HC2
                Return "MXIC"
            Case &H1F
                Return "ATMEL"
            Case &HAD
                Return "HYHYNIX"
            Case &HBF
                Return "SST" 'Silicon Storage
            Case &HEC
                Return "Samsung"
            Case Else
                Return "(Unknown)"
        End Select
    End Function

    Public Function GetOsBitsString() As String
        If IntPtr.Size = 8 Then
            Return "64 bit"
        ElseIf IntPtr.Size = 4 Then
            Return "32 bit"
        End If
        Return "32 bit"
    End Function

    Public Function hweight32(ByVal w As UInteger) As UInteger
        Dim res As UInteger = CUInt((w And &H55555555) + ((w >> 1) And &H55555555))
        res = CUInt((res And &H33333333) + ((res >> 2) And &H33333333))
        res = CUInt((res And &HF0F0F0F) + ((res >> 4) And &HF0F0F0F))
        res = CUInt((res And &HFF00FF) + ((res >> 8) And &HFF00FF))
        Return CUInt((res And &HFFFF) + ((res >> 16) And &HFFFF))
    End Function

    Public Sub Sleep(ByVal miliseconds As Integer)
        Threading.Thread.Sleep(miliseconds)
    End Sub
    'Swaps the endian inside a byte array
    Public Sub SwapEndian_8bit(ByRef Data() As Byte)
        Dim i As Integer
        Dim b As Byte
        Dim bo() As Boolean
        For i = 0 To Data.Length - 1
            b = Data(i)
            bo = ByteToBooleanArray(b)
            Array.Reverse(bo)
            b = BoolArrayToByte(bo)
            Data(i) = b
        Next
    End Sub

    Public Function ByteToBooleanArray(ByVal anyByteArray() As Byte) As Boolean()
        Dim returnedArray() As Boolean
        Dim truthList As New List(Of Boolean)
        If Not anyByteArray Is Nothing Then
            For index As Integer = 0 To anyByteArray.GetUpperBound(0)
                truthList.Add(Convert.ToBoolean(anyByteArray(index) And 128))
                truthList.Add(Convert.ToBoolean(anyByteArray(index) And 64))
                truthList.Add(Convert.ToBoolean(anyByteArray(index) And 32))
                truthList.Add(Convert.ToBoolean(anyByteArray(index) And 16))
                truthList.Add(Convert.ToBoolean(anyByteArray(index) And 8))
                truthList.Add(Convert.ToBoolean(anyByteArray(index) And 4))
                truthList.Add(Convert.ToBoolean(anyByteArray(index) And 2))
                truthList.Add(Convert.ToBoolean(anyByteArray(index) And 1))
            Next
        End If
        returnedArray = truthList.ToArray
        Return returnedArray
    End Function

    Public Function ByteToBooleanArray(ByVal anyByteArray As Byte) As Boolean()
        Dim returnedArray() As Boolean
        Dim truthList As New List(Of Boolean)
        truthList.Add(Convert.ToBoolean(anyByteArray And 128))
        truthList.Add(Convert.ToBoolean(anyByteArray And 64))
        truthList.Add(Convert.ToBoolean(anyByteArray And 32))
        truthList.Add(Convert.ToBoolean(anyByteArray And 16))
        truthList.Add(Convert.ToBoolean(anyByteArray And 8))
        truthList.Add(Convert.ToBoolean(anyByteArray And 4))
        truthList.Add(Convert.ToBoolean(anyByteArray And 2))
        truthList.Add(Convert.ToBoolean(anyByteArray And 1))
        returnedArray = truthList.ToArray
        Return returnedArray
    End Function

    Public Function BoolArrayToByte(ByVal bools() As Boolean) As Byte
        Dim res As Byte = 0
        If bools(0) Then res = CByte(128)
        If bools(1) Then res = CByte(res + 64)
        If bools(2) Then res = CByte(res + 32)
        If bools(3) Then res = CByte(res + 16)
        If bools(4) Then res = CByte(res + 8)
        If bools(5) Then res = CByte(res + 4)
        If bools(6) Then res = CByte(res + 2)
        If bools(7) Then res = CByte(res + 1)
        Return res
    End Function
    'Swaps every two bytes
    Public Sub ReverseByteEndian_16bit(ByRef Buffer() As Byte)
        Dim B1, B2 As Byte
        Dim i As Integer
        For i = 0 To CInt(Math.Floor(Buffer.Length / 2) - 1)
            B1 = Buffer((i * 2) + 1)
            B2 = Buffer((i * 2))
            Buffer((i * 2) + 1) = B2
            Buffer((i * 2)) = B1
        Next
    End Sub
    'Changes the endian in a byte array (word)
    Public Sub ReverseByteEndian_32bit(ByRef Buffer() As Byte)
        Dim B1, B2, B3, B4 As Byte
        Dim i As Integer
        For i = 0 To CInt(Math.Floor(Buffer.Length / 4) - 1)
            B1 = Buffer((i * 4) + 3)
            B2 = Buffer((i * 4) + 2)
            B3 = Buffer((i * 4) + 1)
            B4 = Buffer((i * 4))
            Buffer((i * 4) + 3) = B4
            Buffer((i * 4) + 2) = B3
            Buffer((i * 4) + 1) = B2
            Buffer((i * 4)) = B1
        Next
    End Sub

#Region "Intel HEX Tools"

    Private Structure IntelHexLine
        Public Hex_Size As Byte
        Public Hex_Addr As UInt32
        Public Hex_REC As Byte
        Public Hex_DATA() As Byte
        Public Hex_CRC As UInt16
    End Structure
    'Returns true or false if this data is a file in intel hex format
    Public Function IsIntelHex(ByVal Input() As Byte) As Boolean
        If Input Is Nothing Then Return False
        Dim byte_stream As New IO.MemoryStream(Input)
        Dim HexFile As New IO.StreamReader(byte_stream)
        Dim ReadAtLeastOneLine As Boolean = False
        Do
            Dim line As String = HexFile.ReadLine()
            If Not line.StartsWith(":") Then Return False
            ReadAtLeastOneLine = True
        Loop Until HexFile.Peek = -1
        Return ReadAtLeastOneLine
    End Function

    Public Function IntelHexToBin(ByVal Input() As Byte) As Byte()
        If Input Is Nothing Then Return Nothing
        Dim byte_stream As New IO.MemoryStream(Input)
        Dim HexFile As New IO.StreamReader(byte_stream)
        Dim HexCollector As New ArrayList
        Dim Upper16 As UInt32 = 0
        Dim ExtAddr As UInt32 = 0
        Do
            Dim line As String = HexFile.ReadLine()
            Dim hline As New IntelHexLine
            hline.Hex_Size = HexToInt(Mid(line, 2, 2))
            hline.Hex_Addr = Upper16 + ExtAddr + HexToInt(Mid(line, 4, 4))
            hline.Hex_REC = HexToInt(Mid(line, 8, 2))
            hline.Hex_CRC = HexToInt(Mid(line, (hline.Hex_Size * 2) + 10))
            hline.Hex_DATA = HexStringToBytes(Mid(line, 10, hline.Hex_Size * 2))
            If hline.Hex_REC = 0 Then
                HexCollector.Add(hline) 'Collect Record DATA
            ElseIf hline.Hex_REC = 1 Then 'End Of File record
                Exit Do
            ElseIf hline.Hex_REC = 2 Then 'Extended Segment Address Record
                ExtAddr = ((CUInt(hline.Hex_DATA(0)) << 8) + hline.Hex_DATA(1)) * 16
            ElseIf hline.Hex_REC = 4 Then 'Extended Linear Address Record
                Upper16 = ((CUInt(hline.Hex_DATA(0)) << 8) + hline.Hex_DATA(1)) << 16
            End If
        Loop Until HexFile.Peek = -1
        Dim HighestAddr As UInt32 = 0
        Dim HexLine As IntelHexLine
        For Each HexLine In HexCollector
            Dim ThisHighAddr As UInt32 = HexLine.Hex_Addr + HexLine.Hex_Size
            If ThisHighAddr > HighestAddr Then HighestAddr = ThisHighAddr
        Next
        Dim BinFile(HighestAddr - 1) As Byte
        For Each HexLine In HexCollector
            For i = 0 To HexLine.Hex_Size - 1
                BinFile(HexLine.Hex_Addr + i) = HexLine.Hex_DATA(i)
            Next
        Next
        Return BinFile
    End Function

    Public Function BinToIntelHex(ByVal Input() As Byte) As Byte()
        Dim NumLines As Integer = Math.Floor(Input.Length / 32)
        Dim Remainder As Integer = Input.Length - (NumLines * 32)
        If Remainder > 0 Then NumLines += 1
        Dim LineCollector As New ArrayList
        Dim Addr32 As UInt32 = 0
        Dim Upper32 As UShort = 0
        For i = 0 To NumLines - 1
            Dim BytesPerLine As Integer = 32
            If (Remainder > 0) AndAlso i = NumLines - 1 Then BytesPerLine = Remainder
            Dim CurrentUpper As UShort = CUShort((Addr32 And &HFFFF0000) >> 16)
            Dim CurrentLower As UShort = CUShort(Addr32 And &HFFFF)
            If Not CurrentUpper = Upper32 Then
                Dim ExtRecord As String = "02000004" & Hex(CurrentUpper).PadLeft(4, "0")
                Dim CrcHex As String = Hex(GetIntelHexCRC(HexStringToBytes(ExtRecord))).PadLeft(2, "0")
                LineCollector.Add(":" & ExtRecord & CrcHex)
                Upper32 = CurrentUpper
            End If
            Dim CurrentLine As String = ":" & Hex(BytesPerLine).PadLeft(2, "0") & Hex(CurrentLower).PadLeft(4, "0") & "00"
            Dim Data(BytesPerLine - 1) As Byte
            Array.Copy(Input, Addr32, Data, 0, BytesPerLine)
            CurrentLine &= BytesToHexString(Data)
            Dim CrcField() As Byte = HexStringToBytes(Mid(CurrentLine, 2))
            CurrentLine &= Hex(GetIntelHexCRC(CrcField)).PadLeft(2, "0")
            LineCollector.Add(CurrentLine)
            Addr32 += BytesPerLine
        Next
        LineCollector.Add(":00000001FF") 'Adds the end of file
        If Upper32 > 0 Then LineCollector.Insert(0, ":020000040000FA")
        Dim HexFile() As String = DirectCast(LineCollector.ToArray(GetType(String)), String())
        Dim ByteArrayCollector As New ArrayList
        For Each line In HexFile
            ByteArrayCollector.Add(System.Text.UnicodeEncoding.ASCII.GetBytes(line))
        Next
        Dim DestSize As Integer = 0
        Dim barray() As Byte
        For Each barray In ByteArrayCollector
            DestSize += barray.Length
        Next
        Dim BinaryOut((DestSize + ByteArrayCollector.Count) - 1) As Byte
        Dim DestInd As Integer = 0
        For Each barray In ByteArrayCollector
            Array.Copy(barray, 0, BinaryOut, DestInd, barray.Length)
            DestInd += barray.Length + 1
            BinaryOut(DestInd - 1) = 10 'Adds the CF
        Next
        Return BinaryOut
    End Function

    Private Function GetIntelHexCRC(ByRef data() As Byte) As Byte
        Dim value As Integer = 0
        For i = 0 To data.Length - 1
            value += data(i)
            value = value And 255
        Next
        Return CByte(((value Xor 255) + 1) And 255)
    End Function
    'Opens a hex file from disk
    Public Function OpenHexFile(ByRef Filename As String) As Boolean
        Dim OpenMe As New OpenFileDialog
        OpenMe.AddExtension = True
        OpenMe.InitialDirectory = Application.StartupPath & "\AVR Firmware"
        OpenMe.Title = RM.GetString("fcusb_common_avrtoprog") 'Choose AVR firmware to program
        OpenMe.CheckPathExists = True
        OpenMe.Filter = ftHex
        If OpenMe.ShowDialog = Windows.Forms.DialogResult.OK Then
            Filename = OpenMe.FileName
            Return True
        Else
            Return False
        End If
    End Function

#End Region

End Module
