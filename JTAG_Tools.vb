'COPYRIGHT EMBEDDEDCOMPUTERS.NET 2012 - ALL RIGHTS RESERVED
'CONTACT EMAIL: contact@embeddedcomputers.net
'ANY USE OF THIS CODE MUST ADHERE TO THE LICENSE FILE INCLUDED WITH THIS SDK
'INFO: This class implements the SVF / XSVF file format developed by Texas Instroments

Namespace JTAG

    Public Class SVF_Player
        Public Event Progress(ByVal percent As Integer)
        Public Event ShiftBits(ByVal BitCount As Integer, ByVal tdi_bits() As Byte, ByVal tms_bits() As Byte, ByRef tdo_bits() As Byte)
        Public Event SetFrequency(ByVal Hz As Integer)
        Public Event SetTRST(ByVal Enabled As Boolean)
        Public Event Printf(ByVal msg As String)

        Public Current_State As MachineState
        Public Current_Hertz As Integer = 1000000 'Default of 1 MHz

        Sub New()

        End Sub

        Private Sub Setup()
            RaiseEvent ShiftBits(5, {0}, {&HFF}, Nothing) 'Sets machine state to TestLogicReset
            RaiseEvent SetFrequency(Current_Hertz)
            Current_State = MachineState.TestLogicReset
        End Sub

        Public Function RunFile_XSVF(ByVal user_file() As Byte) As Boolean
            Setup()
            Dim xsvf_file() As xsvf_param = ConvertDataToProperFormat(user_file)
            Dim XENDIR As MachineState = MachineState.RunTestIdle 'Make sure this is correct
            Dim XENDDR As MachineState = MachineState.RunTestIdle 'Make sure this is correct
            Dim IR_TAIL As New svf_param
            Dim IR_HEAD As New svf_param
            Dim DR_TAIL As New svf_param
            Dim DR_HEAD As New svf_param
            Dim TDO_MASK As svf_data = Nothing
            Dim TDO_REPEAT As UInt32 = 16 'Number of times to retry
            Dim XRUNTEST As UInt32 = 0
            Dim XSDRSIZE As UInt32 = 0
            Dim line_counter As Integer = 0
            GotoState(MachineState.RunTestIdle)

            For Each line In xsvf_file
                line_counter += 1
                RaiseEvent Progress((line_counter / xsvf_file.Length) * 100)
                Select Case line.instruction
                    Case xsvf_instruction.XTDOMASK
                        TDO_MASK = line.value_data
                    Case xsvf_instruction.XREPEAT
                        TDO_REPEAT = line.value_uint
                    Case xsvf_instruction.XRUNTEST
                        XRUNTEST = line.value_uint
                    Case xsvf_instruction.XSIR
                        GotoState(MachineState.Shift_IR)
                        ShiftDataOut(line.value_data.bits, line.value_data.data, True)
                        Current_State = MachineState.Exit1_IR
                        If Not XRUNTEST = 0 Then
                            GotoState(MachineState.RunTestIdle)
                            DoXRunTest(line.value_uint) 'wait for the last specified XRUNTEST time. 
                        Else
                            GotoState(XENDIR)  'Otherwise, go to the last specified XENDIR state.
                        End If
                    Case xsvf_instruction.XSDR
                        Dim Counter As UInt32 = TDO_REPEAT
                        Dim Result As Boolean = False
                        Do
                            GotoState(MachineState.Shift_DR)
                            Dim TDO() As Byte = ShiftDataOut(line.value_data.bits, line.value_data.data, True)
                            Current_State = MachineState.Exit1_DR
                            Result = CompareResult(TDO, line.value_expected.data, TDO_MASK.data) 'compare TDO with line.value_expected (use TDOMask from last XTDOMASK)
                            If (Not Result) AndAlso (Counter = 0) Then
                                RaiseEvent Printf("Failed sending XSDR command")
                                Return False
                            End If
                            If Counter > 0 Then Counter -= 1
                        Loop Until Result
                        If Not XRUNTEST = 0 Then
                            GotoState(MachineState.RunTestIdle)
                            DoXRunTest(line.value_uint) 'wait for the last specified XRUNTEST time. 
                        Else
                            GotoState(XENDDR)  'Otherwise, go to the last specified XENDDR state.
                        End If
                    Case xsvf_instruction.XSDRSIZE
                        XSDRSIZE = line.value_uint    'Specifies the length of all XSDR/XSDRTDO records that follow.
                    Case xsvf_instruction.XSDRTDO
                        Dim Counter As UInt32 = TDO_REPEAT
                        Dim Result As Boolean = False
                        Do
                            GotoState(MachineState.Shift_DR)
                            Dim TDO() As Byte = ShiftDataOut(line.value_data.bits, line.value_data.data, True)
                            Current_State = MachineState.Exit1_DR
                            Result = CompareResult(TDO, line.value_expected.data, TDO_MASK.data) 'compare TDO with line.value_expected (use TDOMask from last XTDOMASK)
                            If (Not Result) AndAlso (Counter = 0) Then
                                RaiseEvent Printf("Failed sending XSDRTDO command")
                                Return False
                            End If
                            If Counter > 0 Then Counter -= 1
                        Loop Until Result
                        If Not XRUNTEST = 0 Then
                            GotoState(MachineState.RunTestIdle)
                            DoXRunTest(line.value_uint) 'wait for the last specified XRUNTEST time. 
                        Else
                            GotoState(XENDDR)  'Otherwise, go to the last specified XENDDR state.
                        End If
                    Case xsvf_instruction.XSDRB
                        GotoState(MachineState.Shift_DR)
                        ShiftDataOut(line.value_data.bits, line.value_data.data, False)
                    Case xsvf_instruction.XSDRC
                        ShiftDataOut(line.value_data.bits, line.value_data.data, False)
                    Case xsvf_instruction.XSDRE
                        ShiftDataOut(line.value_data.bits, line.value_data.data, False)
                        GotoState(XENDDR)
                    Case xsvf_instruction.XSDRTDOB
                        Dim Counter As UInt32 = TDO_REPEAT
                        Dim Result As Boolean = False
                        Do
                            GotoState(MachineState.Shift_DR)
                            Dim TDO() As Byte = ShiftDataOut(line.value_data.bits, line.value_data.data, False)
                            Result = CompareResult(TDO, line.value_expected.data, TDO_MASK.data) 'compare TDO with line.value_expected (use TDOMask from last XTDOMASK)
                            If (Not Result) AndAlso (Counter = 0) Then
                                RaiseEvent Printf("Failed sending XSDRTDOB command")
                                Return False
                            End If
                            If Counter > 0 Then Counter -= 1
                        Loop Until Result
                    Case xsvf_instruction.XSDRTDOC
                        Dim Counter As UInt32 = TDO_REPEAT
                        Dim Result As Boolean = False
                        Do
                            Dim TDO() As Byte = ShiftDataOut(line.value_data.bits, line.value_data.data, False)
                            Result = CompareResult(TDO, line.value_expected.data, TDO_MASK.data) 'compare TDO with line.value_expected (use TDOMask from last XTDOMASK)
                            If (Not Result) AndAlso (Counter = 0) Then
                                RaiseEvent Printf("Failed sending XSDRTDOC command")
                                Return False
                            End If
                            If Counter > 0 Then Counter -= 1
                        Loop Until Result
                    Case xsvf_instruction.XSDRTDOE
                        Dim Counter As UInt32 = TDO_REPEAT
                        Dim Result As Boolean = False
                        Do
                            Dim TDO() As Byte = ShiftDataOut(line.value_data.bits, line.value_data.data, False)
                            Result = CompareResult(TDO, line.value_expected.data, TDO_MASK.data) 'compare TDO with line.value_expected (use TDOMask from last XTDOMASK)
                            If (Not Result) AndAlso (Counter = 0) Then
                                RaiseEvent Printf("Failed sending XSDRTDOE command")
                                Return False
                            End If
                            If Counter > 0 Then Counter -= 1
                        Loop Until Result
                        GotoState(XENDDR)
                    Case xsvf_instruction.XSETSDRMASKS 'Obsolete
                    Case xsvf_instruction.XSDRINC 'Obsolete
                    Case xsvf_instruction.XCOMPLETE
                        Exit For
                    Case xsvf_instruction.XSTATE
                        If line.state = MachineState.TestLogicReset Then
                            RaiseEvent ShiftBits(5, {0}, {&HFF}, Nothing) 'Sets machine state to TestLogicReset
                            Current_State = MachineState.TestLogicReset
                        Else
                            GotoState(line.state)
                        End If
                    Case xsvf_instruction.XENDIR
                        XENDIR = line.state
                    Case xsvf_instruction.XENDDR
                        XENDDR = line.state
                    Case xsvf_instruction.XSIR2 'Same as XSIR (since we should all support 255 bit lengths or more)
                        GotoState(MachineState.Shift_IR)
                        ShiftDataOut(line.value_data.bits, line.value_data.data, True)
                        Current_State = MachineState.Exit1_IR
                        If Not XRUNTEST = 0 Then
                            GotoState(MachineState.RunTestIdle)
                            DoXRunTest(line.value_uint) 'wait for the last specified XRUNTEST time. 
                        Else
                            GotoState(XENDIR)  'Otherwise, go to the last specified XENDIR state.
                        End If
                    Case xsvf_instruction.XCOMMENT 'No need to display this
                    Case xsvf_instruction.XWAIT
                        GotoState(line.state)
                        Dim Sleep As Double = line.value_uint / 1000
                        Threading.Thread.Sleep(Sleep)
                        GotoState(line.state_end)
                End Select
            Next
            GotoState(MachineState.TestLogicReset)
            Return True
        End Function

        Private Function CompareResult(ByVal tdo() As Byte, ByVal expected() As Byte, ByVal mask() As Byte) As Boolean
            Try
                If mask IsNot Nothing Then
                    For i = 0 To tdo.Length - 1
                        Dim masked_tdo As Byte = tdo(i) And mask(i)
                        Dim masked_exp As Byte = expected(i) And mask(i)
                        If Not (masked_tdo = masked_exp) Then Return False
                    Next
                Else
                    For i = 0 To tdo.Length - 1
                        If Not tdo(i) = expected(i) Then Return False
                    Next
                End If
                Return True
            Catch ex As Exception
                Return False
            End Try
        End Function

        Public Function RunFile_SVF(ByVal user_file() As String) As Boolean
            Setup()
            Dim svf_file() As String = ConvertFileToProperFormat(user_file)
            Dim SIR_STATE As MachineState = MachineState.RunTestIdle
            Dim SDR_STATE As MachineState = MachineState.RunTestIdle
            Dim IR_TAIL As New svf_param
            Dim IR_HEAD As New svf_param
            Dim DR_TAIL As New svf_param
            Dim DR_HEAD As New svf_param
            GotoState(MachineState.RunTestIdle)
            For Each line In svf_file
                If line.ToUpper.StartsWith("ENDIR ") Then
                    SIR_STATE = GetState(Mid(line, 7).Trim)
                ElseIf line.ToUpper.StartsWith("ENDDR ") Then
                    SDR_STATE = GetState(Mid(line, 7).Trim)
                ElseIf line.ToUpper.StartsWith("TRST ") Then 'Disable Test Reset line
                    Dim s As String = Mid(line, 6).Trim.ToUpper
                    Dim EnableTrst As Boolean = False
                    If s = "ON" OrElse "YES" OrElse "TRUE" Then EnableTrst = True
                    RaiseEvent SetTRST(s)
                ElseIf line.ToUpper.StartsWith("FREQUENCY ") Then 'Sets the max freq of the device
                    Dim s As String = Mid(line, 11)
                    Current_Hertz = Integer.Parse(Mid(s, 1, InStr(s, " ") - 1), Globalization.NumberStyles.AllowExponent)
                    RaiseEvent SetFrequency(Current_Hertz)
                ElseIf line.ToUpper.StartsWith("RUNTEST ") Then
                    ProcessRuntest(Mid(line, 9).Trim)
                ElseIf line.ToUpper.StartsWith("STATE ") Then
                    Dim Desired_State As String = Mid(line, 7).Trim.ToUpper
                    If Desired_State = "RESET" Then
                        RaiseEvent ShiftBits(5, {0}, {&HFF}, Nothing) 'Sets machine state to TestLogicReset
                        Current_State = MachineState.TestLogicReset
                    Else
                        GotoState(GetState(Desired_State))
                    End If
                ElseIf line.ToUpper.StartsWith("SIR ") Then
                    GotoState(MachineState.Shift_IR)
                    Dim line_svf As New svf_param(line)
                    If IR_HEAD.LEN > 0 Then ShiftDataOut(IR_HEAD.LEN, IR_HEAD.TDI, False)
                    If IR_TAIL.LEN > 0 Then
                        ShiftDataOut(line_svf.LEN, line_svf.TDI, False)
                        ShiftDataOut(IR_TAIL.LEN, IR_TAIL.TDI, True) 'Check TDO data
                    Else
                        ShiftDataOut(line_svf.LEN, line_svf.TDI, True)
                    End If
                    Current_State = MachineState.Exit1_IR
                    GotoState(SIR_STATE)
                ElseIf line.ToUpper.StartsWith("SDR ") Then
                    Dim line_svf As New svf_param(line)
                    Dim TDO_OUT() As Byte = Nothing
                    GotoState(MachineState.Shift_DR)
                    If DR_HEAD.LEN > 0 Then ShiftDataOut(DR_HEAD.LEN, DR_HEAD.TDI, False)
                    If DR_TAIL.LEN > 0 Then
                        ShiftDataOut(line_svf.LEN, line_svf.TDI, False)
                        TDO_OUT = ShiftDataOut(DR_TAIL.LEN, DR_TAIL.TDI, True) 'Check TDO data
                    Else
                        TDO_OUT = ShiftDataOut(line_svf.LEN, line_svf.TDI, True)
                    End If
                    Current_State = MachineState.Exit1_DR
                    GotoState(SIR_STATE)
                    If line_svf.TDO IsNot Nothing Then 'We need to check output data (TDO) and MASK
                        If Not CompareResult(TDO_OUT, line_svf.TDO, line_svf.MASK) Then
                            RaiseEvent Printf("SDR command returned an unexpected result")
                            RaiseEvent Printf("Shifted in: " & BytesToHexString(line_svf.TDI) & " and received: " & BytesToHexString(TDO_OUT))
                        End If
                    End If
                ElseIf line.ToUpper.StartsWith("TIR ") Then
                    IR_TAIL.LoadParams(line)
                ElseIf line.ToUpper.StartsWith("HIR ") Then
                    IR_HEAD.LoadParams(line)
                ElseIf line.ToUpper.StartsWith("TDR ") Then
                    DR_TAIL.LoadParams(line)
                ElseIf line.ToUpper.StartsWith("HDR ") Then
                    DR_HEAD.LoadParams(line)
                End If
            Next
            GotoState(MachineState.TestLogicReset)
            Return True
        End Function

        Private Sub DoXRunTest(ByVal wait_amount As UInt32)
            Dim num_bytes As Integer = wait_amount / 8 'Possible problem
            Dim tdi(num_bytes - 1) As Byte
            Dim tdo(num_bytes - 1) As Byte
            Dim tms(num_bytes - 1) As Byte
            For i = 0 To tms.Length - 1
                tms(i) = 255
            Next
            RaiseEvent ShiftBits(wait_amount, tdi, tdo, tms)
            Dim Sleep As Double = wait_amount / 1000
            Threading.Thread.Sleep(Sleep)
        End Sub

        Private Sub ProcessRuntest(ByVal line As String)
            Dim next_item As String = Mid(line, 1, InStr(line, " ") - 1)
            Dim start_state As MachineState = MachineState.RunTestIdle
            If Not IsNumeric(next_item) Then
                start_state = GetState(next_item)
                line = Mid(line, InStr(line, " ") + 1) 'Feeds a word
            End If
            GotoState(start_state)
            Dim time_str As String = Mid(line, 1, InStr(line, " ") - 1)
            Dim min_time As Double = Double.Parse(time_str, Globalization.NumberStyles.AllowExponent)
            line = Mid(line, InStr(line, " ") + 1)
            Dim time_form As String = FeedWord(line)
            If time_form.ToUpper = "TCK" Then
                Dim sleep_int As Integer = (min_time / CDbl(Current_Hertz)) * 1000
                If sleep_int < 1 Then sleep_int = 20
                Threading.Thread.Sleep(sleep_int)
            ElseIf time_form.ToUpper = "SEC" Then
                Dim sleep_int As Integer = min_time * 1000
                If sleep_int < 1 Then sleep_int = 20
                Threading.Thread.Sleep(sleep_int)
            End If
            If line.ToUpper.StartsWith("ENDSTATE ") Then
                line = Mid(line, 10).Trim
                next_item = FeedWord(line)
                GotoState(GetState(next_item))
            End If
        End Sub
        'Returns TDO data
        Public Function ShiftDataOut(ByVal BitCount As Integer, ByVal TDI_IN() As Byte, ByVal ExitShift As Boolean) As Byte()
            Dim TDI(TDI_IN.Length - 1) As Byte
            For i = 0 To TDI_IN.Length - 1
                TDI(i) = TDI_IN(i)
            Next
            Dim TDO(Math.Ceiling(BitCount / 8) - 1) As Byte
            Dim TMS(TDO.Length - 1) As Byte
            If ExitShift Then 'The last tms bit must be high
                Dim offset As Integer = BitCount Mod 8
                If offset = 0 Then
                    TMS(0) = &H80
                Else
                    TMS(0) = (1 << (offset - 1))
                End If
            End If
            RaiseEvent ShiftBits(BitCount, TDI, TMS, TDO)
            Return TDO
        End Function
        'Shift out bits on tms to move the state machine to our desired state (this code was auto-generated for performance)
        Public Sub GotoState(ByVal state As MachineState)
            If Current_State = state Then Exit Sub
            Dim tms As UInt64 = 0
            Dim bits As Integer = 0
            Select Case Current_State
                Case MachineState.TestLogicReset
                    Select Case state
                        Case MachineState.RunTestIdle
                            tms = 0 : bits = 1 '0
                        Case MachineState.Select_DR
                            tms = 2 : bits = 2 '10
                        Case MachineState.Capture_DR
                            tms = 2 : bits = 3 '010
                        Case MachineState.Shift_DR
                            tms = 2 : bits = 4 '0010
                        Case MachineState.Exit1_DR
                            tms = 10 : bits = 4 '1010
                        Case MachineState.Pause_DR
                            tms = 10 : bits = 5 '01010
                        Case MachineState.Exit2_DR
                            tms = 42 : bits = 6 '101010
                        Case MachineState.Update_DR
                            tms = 26 : bits = 5 '11010
                        Case MachineState.Select_IR
                            tms = 6 : bits = 3 '110
                        Case MachineState.Capture_IR
                            tms = 6 : bits = 4 '0110
                        Case MachineState.Shift_IR
                            tms = 6 : bits = 5 '00110
                        Case MachineState.Exit1_IR
                            tms = 22 : bits = 5 '10110
                        Case MachineState.Pause_IR
                            tms = 22 : bits = 6 '010110
                        Case MachineState.Exit2_IR
                            tms = 86 : bits = 7 '1010110
                        Case MachineState.Update_IR
                            tms = 54 : bits = 6 '110110
                    End Select
                Case MachineState.RunTestIdle
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 7 : bits = 3 '111
                        Case MachineState.Select_DR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Capture_DR
                            tms = 1 : bits = 2 '01
                        Case MachineState.Shift_DR
                            tms = 1 : bits = 3 '001
                        Case MachineState.Exit1_DR
                            tms = 5 : bits = 3 '101
                        Case MachineState.Pause_DR
                            tms = 5 : bits = 4 '0101
                        Case MachineState.Exit2_DR
                            tms = 21 : bits = 5 '10101
                        Case MachineState.Update_DR
                            tms = 13 : bits = 4 '1101
                        Case MachineState.Select_IR
                            tms = 3 : bits = 2 '11
                        Case MachineState.Capture_IR
                            tms = 3 : bits = 3 '011
                        Case MachineState.Shift_IR
                            tms = 3 : bits = 4 '0011
                        Case MachineState.Exit1_IR
                            tms = 11 : bits = 4 '1011
                        Case MachineState.Pause_IR
                            tms = 11 : bits = 5 '01011
                        Case MachineState.Exit2_IR
                            tms = 43 : bits = 6 '101011
                        Case MachineState.Update_IR
                            tms = 27 : bits = 5 '11011
                    End Select
                Case MachineState.Select_DR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 3 : bits = 2 '11
                        Case MachineState.RunTestIdle
                            tms = 3 : bits = 3 '011
                        Case MachineState.Capture_DR
                            tms = 0 : bits = 1 '0
                        Case MachineState.Shift_DR
                            tms = 0 : bits = 2 '00
                        Case MachineState.Exit1_DR
                            tms = 2 : bits = 2 '10
                        Case MachineState.Pause_DR
                            tms = 2 : bits = 3 '010
                        Case MachineState.Exit2_DR
                            tms = 10 : bits = 4 '1010
                        Case MachineState.Update_DR
                            tms = 6 : bits = 3 '110
                        Case MachineState.Select_IR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Capture_IR
                            tms = 1 : bits = 2 '01
                        Case MachineState.Shift_IR
                            tms = 1 : bits = 3 '001
                        Case MachineState.Exit1_IR
                            tms = 5 : bits = 3 '101
                        Case MachineState.Pause_IR
                            tms = 5 : bits = 4 '0101
                        Case MachineState.Exit2_IR
                            tms = 21 : bits = 5 '10101
                        Case MachineState.Update_IR
                            tms = 13 : bits = 4 '1101
                    End Select
                Case MachineState.Capture_DR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 31 : bits = 5 '11111
                        Case MachineState.RunTestIdle
                            tms = 3 : bits = 3 '011
                        Case MachineState.Select_DR
                            tms = 7 : bits = 3 '111
                        Case MachineState.Shift_DR
                            tms = 0 : bits = 1 '0
                        Case MachineState.Exit1_DR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Pause_DR
                            tms = 1 : bits = 2 '01
                        Case MachineState.Exit2_DR
                            tms = 5 : bits = 3 '101
                        Case MachineState.Update_DR
                            tms = 3 : bits = 2 '11
                        Case MachineState.Select_IR
                            tms = 15 : bits = 4 '1111
                        Case MachineState.Capture_IR
                            tms = 15 : bits = 5 '01111
                        Case MachineState.Shift_IR
                            tms = 15 : bits = 6 '001111
                        Case MachineState.Exit1_IR
                            tms = 47 : bits = 6 '101111
                        Case MachineState.Pause_IR
                            tms = 47 : bits = 7 '0101111
                        Case MachineState.Exit2_IR
                            tms = 175 : bits = 8 '10101111
                        Case MachineState.Update_IR
                            tms = 111 : bits = 7 '1101111
                    End Select
                Case MachineState.Shift_DR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 31 : bits = 5 '11111
                        Case MachineState.RunTestIdle
                            tms = 3 : bits = 3 '011
                        Case MachineState.Select_DR
                            tms = 7 : bits = 3 '111
                        Case MachineState.Capture_DR
                            tms = 7 : bits = 4 '0111
                        Case MachineState.Exit1_DR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Pause_DR
                            tms = 1 : bits = 2 '01
                        Case MachineState.Exit2_DR
                            tms = 5 : bits = 3 '101
                        Case MachineState.Update_DR
                            tms = 3 : bits = 2 '11
                        Case MachineState.Select_IR
                            tms = 15 : bits = 4 '1111
                        Case MachineState.Capture_IR
                            tms = 15 : bits = 5 '01111
                        Case MachineState.Shift_IR
                            tms = 15 : bits = 6 '001111
                        Case MachineState.Exit1_IR
                            tms = 47 : bits = 6 '101111
                        Case MachineState.Pause_IR
                            tms = 47 : bits = 7 '0101111
                        Case MachineState.Exit2_IR
                            tms = 175 : bits = 8 '10101111
                        Case MachineState.Update_IR
                            tms = 111 : bits = 7 '1101111
                    End Select
                Case MachineState.Exit1_DR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 15 : bits = 4 '1111
                        Case MachineState.RunTestIdle
                            tms = 1 : bits = 2 '01
                        Case MachineState.Select_DR
                            tms = 3 : bits = 2 '11
                        Case MachineState.Capture_DR
                            tms = 3 : bits = 3 '011
                        Case MachineState.Shift_DR
                            tms = 2 : bits = 3 '010
                        Case MachineState.Pause_DR
                            tms = 0 : bits = 1 '0
                        Case MachineState.Exit2_DR
                            tms = 2 : bits = 2 '10
                        Case MachineState.Update_DR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Select_IR
                            tms = 7 : bits = 3 '111
                        Case MachineState.Capture_IR
                            tms = 7 : bits = 4 '0111
                        Case MachineState.Shift_IR
                            tms = 7 : bits = 5 '00111
                        Case MachineState.Exit1_IR
                            tms = 23 : bits = 5 '10111
                        Case MachineState.Pause_IR
                            tms = 23 : bits = 6 '010111
                        Case MachineState.Exit2_IR
                            tms = 87 : bits = 7 '1010111
                        Case MachineState.Update_IR
                            tms = 55 : bits = 6 '110111
                    End Select
                Case MachineState.Pause_DR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 31 : bits = 5 '11111
                        Case MachineState.RunTestIdle
                            tms = 3 : bits = 3 '011
                        Case MachineState.Select_DR
                            tms = 7 : bits = 3 '111
                        Case MachineState.Capture_DR
                            tms = 7 : bits = 4 '0111
                        Case MachineState.Shift_DR
                            tms = 1 : bits = 2 '01
                        Case MachineState.Exit1_DR
                            tms = 5 : bits = 3 '101
                        Case MachineState.Exit2_DR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Update_DR
                            tms = 3 : bits = 2 '11
                        Case MachineState.Select_IR
                            tms = 15 : bits = 4 '1111
                        Case MachineState.Capture_IR
                            tms = 15 : bits = 5 '01111
                        Case MachineState.Shift_IR
                            tms = 15 : bits = 6 '001111
                        Case MachineState.Exit1_IR
                            tms = 47 : bits = 6 '101111
                        Case MachineState.Pause_IR
                            tms = 47 : bits = 7 '0101111
                        Case MachineState.Exit2_IR
                            tms = 175 : bits = 8 '10101111
                        Case MachineState.Update_IR
                            tms = 111 : bits = 7 '1101111
                    End Select
                Case MachineState.Exit2_DR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 15 : bits = 4 '1111
                        Case MachineState.RunTestIdle
                            tms = 1 : bits = 2 '01
                        Case MachineState.Select_DR
                            tms = 3 : bits = 2 '11
                        Case MachineState.Capture_DR
                            tms = 3 : bits = 3 '011
                        Case MachineState.Shift_DR
                            tms = 0 : bits = 1 '0
                        Case MachineState.Exit1_DR
                            tms = 2 : bits = 2 '10
                        Case MachineState.Pause_DR
                            tms = 2 : bits = 3 '010
                        Case MachineState.Update_DR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Select_IR
                            tms = 7 : bits = 3 '111
                        Case MachineState.Capture_IR
                            tms = 7 : bits = 4 '0111
                        Case MachineState.Shift_IR
                            tms = 7 : bits = 5 '00111
                        Case MachineState.Exit1_IR
                            tms = 23 : bits = 5 '10111
                        Case MachineState.Pause_IR
                            tms = 23 : bits = 6 '010111
                        Case MachineState.Exit2_IR
                            tms = 87 : bits = 7 '1010111
                        Case MachineState.Update_IR
                            tms = 55 : bits = 6 '110111
                    End Select
                Case MachineState.Update_DR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 7 : bits = 3 '111
                        Case MachineState.RunTestIdle
                            tms = 0 : bits = 1 '0
                        Case MachineState.Select_DR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Capture_DR
                            tms = 1 : bits = 2 '01
                        Case MachineState.Shift_DR
                            tms = 1 : bits = 3 '001
                        Case MachineState.Exit1_DR
                            tms = 5 : bits = 3 '101
                        Case MachineState.Pause_DR
                            tms = 5 : bits = 4 '0101
                        Case MachineState.Exit2_DR
                            tms = 21 : bits = 5 '10101
                        Case MachineState.Select_IR
                            tms = 3 : bits = 2 '11
                        Case MachineState.Capture_IR
                            tms = 3 : bits = 3 '011
                        Case MachineState.Shift_IR
                            tms = 3 : bits = 4 '0011
                        Case MachineState.Exit1_IR
                            tms = 11 : bits = 4 '1011
                        Case MachineState.Pause_IR
                            tms = 11 : bits = 5 '01011
                        Case MachineState.Exit2_IR
                            tms = 43 : bits = 6 '101011
                        Case MachineState.Update_IR
                            tms = 27 : bits = 5 '11011
                    End Select
                Case MachineState.Select_IR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 1 : bits = 1 '1
                        Case MachineState.RunTestIdle
                            tms = 1 : bits = 2 '01
                        Case MachineState.Select_DR
                            tms = 5 : bits = 3 '101
                        Case MachineState.Capture_DR
                            tms = 5 : bits = 4 '0101
                        Case MachineState.Shift_DR
                            tms = 5 : bits = 5 '00101
                        Case MachineState.Exit1_DR
                            tms = 21 : bits = 5 '10101
                        Case MachineState.Pause_DR
                            tms = 21 : bits = 6 '010101
                        Case MachineState.Exit2_DR
                            tms = 85 : bits = 7 '1010101
                        Case MachineState.Update_DR
                            tms = 53 : bits = 6 '110101
                        Case MachineState.Capture_IR
                            tms = 0 : bits = 1 '0
                        Case MachineState.Shift_IR
                            tms = 0 : bits = 2 '00
                        Case MachineState.Exit1_IR
                            tms = 2 : bits = 2 '10
                        Case MachineState.Pause_IR
                            tms = 2 : bits = 3 '010
                        Case MachineState.Exit2_IR
                            tms = 10 : bits = 4 '1010
                        Case MachineState.Update_IR
                            tms = 6 : bits = 3 '110
                    End Select
                Case MachineState.Capture_IR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 31 : bits = 5 '11111
                        Case MachineState.RunTestIdle
                            tms = 3 : bits = 3 '011
                        Case MachineState.Select_DR
                            tms = 7 : bits = 3 '111
                        Case MachineState.Capture_DR
                            tms = 7 : bits = 4 '0111
                        Case MachineState.Shift_DR
                            tms = 7 : bits = 5 '00111
                        Case MachineState.Exit1_DR
                            tms = 23 : bits = 5 '10111
                        Case MachineState.Pause_DR
                            tms = 23 : bits = 6 '010111
                        Case MachineState.Exit2_DR
                            tms = 87 : bits = 7 '1010111
                        Case MachineState.Update_DR
                            tms = 55 : bits = 6 '110111
                        Case MachineState.Select_IR
                            tms = 15 : bits = 4 '1111
                        Case MachineState.Shift_IR
                            tms = 0 : bits = 1 '0
                        Case MachineState.Exit1_IR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Pause_IR
                            tms = 1 : bits = 2 '01
                        Case MachineState.Exit2_IR
                            tms = 5 : bits = 3 '101
                        Case MachineState.Update_IR
                            tms = 3 : bits = 2 '11
                    End Select
                Case MachineState.Shift_IR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 31 : bits = 5 '11111
                        Case MachineState.RunTestIdle
                            tms = 3 : bits = 3 '011
                        Case MachineState.Select_DR
                            tms = 7 : bits = 3 '111
                        Case MachineState.Capture_DR
                            tms = 7 : bits = 4 '0111
                        Case MachineState.Shift_DR
                            tms = 7 : bits = 5 '00111
                        Case MachineState.Exit1_DR
                            tms = 23 : bits = 5 '10111
                        Case MachineState.Pause_DR
                            tms = 23 : bits = 6 '010111
                        Case MachineState.Exit2_DR
                            tms = 87 : bits = 7 '1010111
                        Case MachineState.Update_DR
                            tms = 55 : bits = 6 '110111
                        Case MachineState.Select_IR
                            tms = 15 : bits = 4 '1111
                        Case MachineState.Capture_IR
                            tms = 15 : bits = 5 '01111
                        Case MachineState.Exit1_IR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Pause_IR
                            tms = 1 : bits = 2 '01
                        Case MachineState.Exit2_IR
                            tms = 5 : bits = 3 '101
                        Case MachineState.Update_IR
                            tms = 3 : bits = 2 '11
                    End Select
                Case MachineState.Exit1_IR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 15 : bits = 4 '1111
                        Case MachineState.RunTestIdle
                            tms = 1 : bits = 2 '01
                        Case MachineState.Select_DR
                            tms = 3 : bits = 2 '11
                        Case MachineState.Capture_DR
                            tms = 3 : bits = 3 '011
                        Case MachineState.Shift_DR
                            tms = 3 : bits = 4 '0011
                        Case MachineState.Exit1_DR
                            tms = 11 : bits = 4 '1011
                        Case MachineState.Pause_DR
                            tms = 11 : bits = 5 '01011
                        Case MachineState.Exit2_DR
                            tms = 43 : bits = 6 '101011
                        Case MachineState.Update_DR
                            tms = 27 : bits = 5 '11011
                        Case MachineState.Select_IR
                            tms = 7 : bits = 3 '111
                        Case MachineState.Capture_IR
                            tms = 7 : bits = 4 '0111
                        Case MachineState.Shift_IR
                            tms = 2 : bits = 3 '010
                        Case MachineState.Pause_IR
                            tms = 0 : bits = 1 '0
                        Case MachineState.Exit2_IR
                            tms = 2 : bits = 2 '10
                        Case MachineState.Update_IR
                            tms = 1 : bits = 1 '1
                    End Select
                Case MachineState.Pause_IR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 31 : bits = 5 '11111
                        Case MachineState.RunTestIdle
                            tms = 3 : bits = 3 '011
                        Case MachineState.Select_DR
                            tms = 7 : bits = 3 '111
                        Case MachineState.Capture_DR
                            tms = 7 : bits = 4 '0111
                        Case MachineState.Shift_DR
                            tms = 7 : bits = 5 '00111
                        Case MachineState.Exit1_DR
                            tms = 23 : bits = 5 '10111
                        Case MachineState.Pause_DR
                            tms = 23 : bits = 6 '010111
                        Case MachineState.Exit2_DR
                            tms = 87 : bits = 7 '1010111
                        Case MachineState.Update_DR
                            tms = 55 : bits = 6 '110111
                        Case MachineState.Select_IR
                            tms = 15 : bits = 4 '1111
                        Case MachineState.Capture_IR
                            tms = 15 : bits = 5 '01111
                        Case MachineState.Shift_IR
                            tms = 1 : bits = 2 '01
                        Case MachineState.Exit1_IR
                            tms = 5 : bits = 3 '101
                        Case MachineState.Exit2_IR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Update_IR
                            tms = 3 : bits = 2 '11
                    End Select
                Case MachineState.Exit2_IR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 15 : bits = 4 '1111
                        Case MachineState.RunTestIdle
                            tms = 1 : bits = 2 '01
                        Case MachineState.Select_DR
                            tms = 3 : bits = 2 '11
                        Case MachineState.Capture_DR
                            tms = 3 : bits = 3 '011
                        Case MachineState.Shift_DR
                            tms = 3 : bits = 4 '0011
                        Case MachineState.Exit1_DR
                            tms = 11 : bits = 4 '1011
                        Case MachineState.Pause_DR
                            tms = 11 : bits = 5 '01011
                        Case MachineState.Exit2_DR
                            tms = 43 : bits = 6 '101011
                        Case MachineState.Update_DR
                            tms = 27 : bits = 5 '11011
                        Case MachineState.Select_IR
                            tms = 7 : bits = 3 '111
                        Case MachineState.Capture_IR
                            tms = 7 : bits = 4 '0111
                        Case MachineState.Shift_IR
                            tms = 0 : bits = 1 '0
                        Case MachineState.Exit1_IR
                            tms = 2 : bits = 2 '10
                        Case MachineState.Pause_IR
                            tms = 2 : bits = 3 '010
                        Case MachineState.Update_IR
                            tms = 1 : bits = 1 '1
                    End Select
                Case MachineState.Update_IR
                    Select Case state
                        Case MachineState.TestLogicReset
                            tms = 7 : bits = 3 '111
                        Case MachineState.RunTestIdle
                            tms = 0 : bits = 1 '0
                        Case MachineState.Select_DR
                            tms = 1 : bits = 1 '1
                        Case MachineState.Capture_DR
                            tms = 1 : bits = 2 '01
                        Case MachineState.Shift_DR
                            tms = 1 : bits = 3 '001
                        Case MachineState.Exit1_DR
                            tms = 5 : bits = 3 '101
                        Case MachineState.Pause_DR
                            tms = 5 : bits = 4 '0101
                        Case MachineState.Exit2_DR
                            tms = 21 : bits = 5 '10101
                        Case MachineState.Update_DR
                            tms = 13 : bits = 4 '1101
                        Case MachineState.Select_IR
                            tms = 3 : bits = 2 '11
                        Case MachineState.Capture_IR
                            tms = 3 : bits = 3 '011
                        Case MachineState.Shift_IR
                            tms = 3 : bits = 4 '0011
                        Case MachineState.Exit1_IR
                            tms = 11 : bits = 4 '1011
                        Case MachineState.Pause_IR
                            tms = 11 : bits = 5 '01011
                        Case MachineState.Exit2_IR
                            tms = 43 : bits = 6 '101011
                    End Select
            End Select
            RaiseEvent ShiftBits(bits, GetBytes_FromUint(0, bits), GetBytes_FromUint(tms, bits), Nothing)
            Current_State = state
        End Sub

        Private Function GetState(ByVal input As String) As MachineState
            input = RemoveComment(input)
            If input.EndsWith(";") Then input = Mid(input, 1, input.Length - 1).Trim
            input = input.ToUpper
            Select Case input
                Case "IRPAUSE"
                    Return MachineState.Pause_IR
                Case "DRPAUSE"
                    Return MachineState.Pause_DR
                Case "IDLE"
                    Return MachineState.RunTestIdle
                Case Else
                    Return MachineState.RunTestIdle
            End Select
        End Function

        Private Function GetState(ByVal StateCMD As Byte) As MachineState
            Select Case StateCMD
                Case 0
                    Return MachineState.TestLogicReset
                Case 1
                    Return MachineState.RunTestIdle
                Case 2
                    Return MachineState.Select_DR
                Case 3
                    Return MachineState.Capture_DR
                Case 4
                    Return MachineState.Shift_DR
                Case 5
                    Return MachineState.Exit1_DR
                Case 6
                    Return MachineState.Pause_DR
                Case 7
                    Return MachineState.Exit2_DR
                Case 8
                    Return MachineState.Update_DR
                Case 9
                    Return MachineState.Select_IR
                Case 10
                    Return MachineState.Capture_IR
                Case 11
                    Return MachineState.Shift_IR
                Case 12
                    Return MachineState.Exit1_IR
                Case 13
                    Return MachineState.Pause_IR
                Case 14
                    Return MachineState.Exit2_IR
                Case 15
                    Return MachineState.Update_IR
                Case Else
                    Return MachineState.RunTestIdle
            End Select
        End Function

        Private Function ConvertFileToProperFormat(ByVal input() As String) As String()
            Dim c As New ArrayList
            For Each line In input
                line = RemoveComment(line).Replace(vbTab, " ").Trim
                If Not line = "" Then c.Add(line)
            Next
            Dim d As New ArrayList
            Dim WorkInProgress As String = ""
            For Each line In c
                WorkInProgress &= line.ToString.TrimStart
                If WorkInProgress.EndsWith(";") Then
                    WorkInProgress = Mid(WorkInProgress, 1, WorkInProgress.Length - 1)
                    d.Add(WorkInProgress)
                    WorkInProgress = ""
                Else
                    WorkInProgress &= " "
                End If
            Next
            Return DirectCast(d.ToArray(GetType(String)), String())
        End Function

        Private Function ConvertDataToProperFormat(ByVal data() As Byte) As xsvf_param()
            Dim pointer As Integer = 0
            Dim x As New ArrayList
            Dim XSDRSIZE As UInt32 = 8 'number of bits
            Do Until pointer = data.Length
                Dim n As New xsvf_param(data(pointer))
                Select Case n.instruction
                    Case xsvf_instruction.XTDOMASK
                        Load_TDI(data, pointer, n, XSDRSIZE)
                    Case xsvf_instruction.XREPEAT
                        n.value_uint = data(pointer + 1)
                        pointer += 2
                    Case xsvf_instruction.XRUNTEST
                        n.value_uint = Load_Uint32_Value(data, pointer)
                    Case xsvf_instruction.XSIR
                        Dim num_bytes As Integer = Math.Ceiling(data(pointer + 1) / 8)
                        Dim new_Data(num_bytes - 1) As Byte
                        Array.Copy(data, pointer + 2, new_Data, 0, num_bytes)
                        n.value_data = New svf_data With {.data = new_Data, .bits = data(pointer + 1)}
                        pointer += num_bytes + 2
                    Case xsvf_instruction.XSDR
                        Load_TDI(data, pointer, n, XSDRSIZE)
                    Case xsvf_instruction.XSDRSIZE
                        XSDRSIZE = Load_Uint32_Value(data, pointer)
                        n.value_uint = XSDRSIZE
                    Case xsvf_instruction.XSDRTDO
                        Dim num_bytes As Integer = Math.Ceiling(XSDRSIZE / 8)
                        Dim data1(num_bytes - 1) As Byte
                        Dim data2(num_bytes - 1) As Byte
                        Array.Copy(data, pointer + 1, data1, 0, num_bytes)
                        Array.Copy(data, pointer + 1 + num_bytes, data2, 0, num_bytes)
                        n.value_data = New svf_data With {.data = data1, .bits = XSDRSIZE}
                        n.value_expected = New svf_data With {.data = data2, .bits = XSDRSIZE}
                        pointer += num_bytes + num_bytes + 1
                    Case xsvf_instruction.XSDRB
                        Load_TDI(data, pointer, n, XSDRSIZE)
                    Case xsvf_instruction.XSDRC
                        Load_TDI(data, pointer, n, XSDRSIZE)
                    Case xsvf_instruction.XSDRE
                        Load_TDI(data, pointer, n, XSDRSIZE)
                    Case xsvf_instruction.XSDRTDOB
                        Load_TDI_Expected(data, pointer, n, XSDRSIZE)
                    Case xsvf_instruction.XSDRTDOC
                        Load_TDI_Expected(data, pointer, n, XSDRSIZE)
                    Case xsvf_instruction.XSDRTDOE
                        Load_TDI_Expected(data, pointer, n, XSDRSIZE)
                    Case xsvf_instruction.XSETSDRMASKS 'Obsolete
                    Case xsvf_instruction.XSDRINC 'Obsolete
                    Case xsvf_instruction.XCOMPLETE
                        Return DirectCast(x.ToArray(GetType(xsvf_param)), xsvf_param())
                    Case xsvf_instruction.XSTATE
                        n.value_uint = data(pointer + 1)
                        n.state = GetState(data(pointer + 1))
                        pointer += 2
                    Case xsvf_instruction.XENDIR
                        n.value_uint = data(pointer + 1)
                        Select Case n.value_uint
                            Case 0
                                n.state = MachineState.RunTestIdle
                            Case 1
                                n.state = MachineState.Pause_IR
                        End Select
                        pointer += 2
                    Case xsvf_instruction.XENDDR
                        n.value_uint = data(pointer + 1)
                        Select Case n.value_uint
                            Case 0
                                n.state = MachineState.RunTestIdle
                            Case 1
                                n.state = MachineState.Pause_DR
                        End Select
                        pointer += 2
                    Case xsvf_instruction.XSIR2 'Same as XSIR (since we should all support 255 bit lengths or more)
                        Dim num_bytes As Integer = Math.Ceiling(data(pointer + 1) / 8)
                        Dim new_Data(num_bytes - 1) As Byte
                        Array.Copy(data, pointer + 2, new_Data, 0, num_bytes)
                        n.value_data = New svf_data With {.data = new_Data, .bits = XSDRSIZE}
                        pointer += num_bytes + 2
                    Case xsvf_instruction.XCOMMENT
                        Do
                            pointer += 1
                            If pointer = data.Length Then Exit Select
                        Loop Until data(pointer) = 0
                        pointer += 1
                    Case xsvf_instruction.XWAIT
                        n.state = GetState(data(pointer + 1))
                        n.state_end = GetState(data(pointer + 2))
                        n.value_uint = CUInt(data(pointer + 3)) << 24
                        n.value_uint += CUInt(data(pointer + 4)) << 16
                        n.value_uint += CUInt(data(pointer + 5)) << 8
                        n.value_uint += CUInt(data(pointer + 6))
                        pointer += 7
                End Select
                x.Add(n)
            Loop
            Return DirectCast(x.ToArray(GetType(xsvf_param)), xsvf_param())
        End Function

        Private Sub Load_TDI(ByRef data() As Byte, ByRef pointer As Integer, ByRef line As xsvf_param, ByVal XSDRSIZE As Integer)
            Dim num_bytes As Integer = Math.Ceiling(XSDRSIZE / 8)
            Dim new_Data(num_bytes - 1) As Byte
            Array.Copy(data, pointer + 1, new_Data, 0, num_bytes)
            line.value_data = New svf_data With {.data = new_Data, .bits = XSDRSIZE}
            pointer += num_bytes + 1
        End Sub

        Private Sub Load_TDI_Expected(ByRef data() As Byte, ByRef pointer As Integer, ByRef line As xsvf_param, ByVal XSDRSIZE As Integer)
            Dim num_bytes As Integer = XSDRSIZE / 8 'Possible problem
            Dim data1(num_bytes - 1) As Byte
            Dim data2(num_bytes - 1) As Byte
            Array.Copy(data, pointer + 1, data1, 0, num_bytes)
            Array.Copy(data, pointer + 5, data2, 0, num_bytes)
            line.value_data = New svf_data With {.data = data1, .bits = XSDRSIZE}
            line.value_expected = New svf_data With {.data = data2, .bits = XSDRSIZE}
            pointer += 9
        End Sub

        Private Function Load_Uint32_Value(ByRef data() As Byte, ByRef pointer As Integer) As UInt32
            Dim ret As UInt32
            ret = CUInt(data(pointer + 1)) << 24
            ret += CUInt(data(pointer + 2)) << 16
            ret += CUInt(data(pointer + 3)) << 8
            ret += CUInt(data(pointer + 4))
            pointer += 5
            Return ret
        End Function

        Private Function GetBytes_FromUint(ByVal input As UInt32, ByVal MinBits As Integer) As Byte()
            Dim current(3) As Byte
            current(0) = (input And &HFF000000) >> 24
            current(1) = (input And &HFF0000) >> 16
            current(2) = (input And &HFF00) >> 8
            current(3) = (input And &HFF)
            Dim MaxSize As Integer = Math.Ceiling(MinBits / 8)
            Dim out(MaxSize - 1) As Byte
            For i = 0 To MaxSize - 1
                out(out.Length - (1 + i)) = current(current.Length - (1 + i))
            Next
            Return out
        End Function

    End Class

    Public Structure svf_data
        Dim data() As Byte
        Dim bits As Integer
    End Structure

    Public Class svf_param
        Public LEN As Integer = 0
        Public TDI As Byte()
        Public SMASK As Byte()
        Public TDO As Byte()
        Public MASK As Byte()

        Sub New()

        End Sub

        Sub New(ByVal input As String)
            LoadParams(input)
        End Sub

        Public Sub LoadParams(ByVal input As String)
            input = Mid(input, InStr(input, " ") + 1)
            LEN = Mid(input, 1, InStr(input, " ") - 1)
            input = Mid(input, InStr(input, " ") + 1)
            Do Until input = ""
                If input.ToUpper.StartsWith("TDI") Then
                    input = Mid(input, 4).Trim
                    TDI = GetBytes_Param(input)
                ElseIf input.ToUpper.StartsWith("TDO") Then
                    input = Mid(input, 4).Trim
                    TDO = GetBytes_Param(input)
                ElseIf input.ToUpper.StartsWith("SMASK") Then
                    input = Mid(input, 6).Trim
                    SMASK = GetBytes_Param(input)
                ElseIf input.ToUpper.StartsWith("MASK") Then
                    input = Mid(input, 5).Trim
                    MASK = GetBytes_Param(input)
                End If
            Loop
        End Sub

        Private Function GetBytes_Param(ByRef line As String) As Byte()
            Dim x1 As Integer = InStr(line, "(")
            Dim x2 As Integer = InStr(line, ")")
            Dim HexStr As String = line.Substring(x1, x2 - 2).Replace(" ", "")
            line = Mid(line, x2 + 1).Trim
            Return HexStringToBytes(HexStr)
        End Function

    End Class

    Public Class xsvf_param
        Public instruction As xsvf_instruction
        Public state As MachineState
        Public state_end As MachineState
        Public value_uint As UInt64
        Public value_data As svf_data
        Public value_expected As svf_data

        Sub New(ByVal code As Byte)
            instruction = code
        End Sub

        Public Overrides Function ToString() As String

            Return instruction.ToString()

        End Function

    End Class

    Public Enum MachineState As Byte
        TestLogicReset = 0
        RunTestIdle = 1
        Select_DR = 2
        Capture_DR = 3
        Shift_DR = 4
        Exit1_DR = 5
        Pause_DR = 6
        Exit2_DR = 7
        Update_DR = 8
        Select_IR = 9
        Capture_IR = 10
        Shift_IR = 11
        Exit1_IR = 12
        Pause_IR = 13
        Exit2_IR = 14
        Update_IR = 15
    End Enum

    Public Enum xsvf_instruction
        XCOMPLETE = &H0
        XTDOMASK = &H1
        XSIR = &H2
        XSDR = &H3
        XRUNTEST = &H4
        XREPEAT = &H7
        XSDRSIZE = &H8
        XSDRTDO = &H9
        XSETSDRMASKS = &HA
        XSDRINC = &HB
        XSDRB = &HC
        XSDRC = &HD
        XSDRE = &HE
        XSDRTDOB = &HF
        XSDRTDOC = &H10
        XSDRTDOE = &H11
        XSTATE = &H12
        XENDIR = &H13
        XENDDR = &H14
        XSIR2 = &H15
        XCOMMENT = &H16
        XWAIT = &H17
    End Enum

    Public Module Common
        'Removes ! and // lines
        Public Function RemoveComment(ByVal input As String) As String
            If input.Contains("!") Then
                input = Mid(input, 1, InStr(input, "!") - 1)
            End If
            If input.Contains("//") Then
                input = Mid(input, 1, InStr(input, "//") - 1)
            End If
            Return input
        End Function

        Public Function HexStringToBytes(ByVal hex_str As String) As Byte()
            Dim i As Single
            Dim bytes() As Byte
            Dim TotalBytes As Long = CLng(Len(hex_str) / 2 - 1)
            ReDim Preserve bytes(CInt(TotalBytes))
            For i = 0 To TotalBytes
                bytes(CInt(i)) = CByte(HexToInt(Mid(hex_str, CInt(i * 2 + 1), 2)))
            Next
            Return bytes
        End Function

        Public Function FeedWord(ByRef line As String) As String
            Dim word_out As String = ""
            If line.Contains(" ") Then
                word_out = Mid(line, 1, InStr(line, " ") - 1)
                line = Mid(line, InStr(line, " ") + 1) 'Feeds a word
            Else
                word_out = line
                line = ""
            End If
            Return word_out
        End Function

    End Module

End Namespace


