﻿'COPYRIGHT EMBEDDEDCOMPUTERS.NET 2012 - ALL RIGHTS RESERVED
'CONTACT EMAIL: contact@embeddedcomputers.net
'ANY USE OF THIS CODE MUST ADHERE TO THE LICENSE FILE INCLUDED WITH THIS SDK
'INFO: This class is the entire scripting engine which can control the software
'via user supplied text files. The langauge format is similar to BASIC.

Public Class Script
    Public ScriptBar As ProgressBar 'Our one and only progress bar

    Private Const Build As Integer = 202
    Private Delegate Function ScriptFunction(ByVal args() As String, ByVal Index As Integer, ByVal SubFun As String) As ScriptVariable
    Delegate Sub cbUpdatePbar(ByVal Percent As Integer)
    Private ScriptFunctions As New ArrayList 'Contains all of function delegates
    Private MyVariables() As ScriptVariable
    Private UserTabCount As Integer
    Private MyEventVariables As ArrayList 'holds variables specific to events
    Private Scripts As ArrayList 'Contains ScriptFile
    Private Quit As Boolean = False 'Change to true to abort current exection
    Public Event printf(ByVal Msg As String)
    Private MemoryError As String 'Contains the last error message
    Private ScriptRunning As Boolean 'Indicates if we are running a script
    Private StopScript As Boolean 'Set to true to halt script exeuction
    Private ReloadVariables As Boolean = False
    Public ScriptName As String
    Private OurFlashDevices As New ArrayList 'Contains index of the flash devices the current script has created

    Sub New()
        AddCmd("msgbox", New ScriptFunction(AddressOf msg))
        AddCmd("hex", New ScriptFunction(AddressOf hexcmd))
        AddCmd("verify", New ScriptFunction(AddressOf verify))
        AddCmd("writeline", New ScriptFunction(AddressOf writelinecmd))
        AddCmd("jtag", New ScriptFunction(AddressOf jtagcmd))
        AddCmd("setparam", New ScriptFunction(AddressOf setparam))
        AddCmd("ejctrl", New ScriptFunction(AddressOf ejctrl))
        AddCmd("pause", New ScriptFunction(AddressOf pausecmd))
        AddCmd("status", New ScriptFunction(AddressOf statuscmd))
        AddCmd("resize", New ScriptFunction(AddressOf resizecmd))
        AddCmd("tab", New ScriptFunction(AddressOf tabcmd))
        AddCmd("mode", New ScriptFunction(AddressOf mode))
        AddCmd("len", New ScriptFunction(AddressOf lencmd))
        AddCmd("openfile", New ScriptFunction(AddressOf openfilecmd))
        AddCmd("savefile", New ScriptFunction(AddressOf savefilecmd))
        AddCmd("hword", New ScriptFunction(AddressOf HWordCmd))
        AddCmd("word", New ScriptFunction(AddressOf WordCmd))
        AddCmd("byte", New ScriptFunction(AddressOf ByteCmd))
        AddCmd("ask", New ScriptFunction(AddressOf askcmd))
        AddCmd("status", New ScriptFunction(AddressOf statuscmd))
        AddCmd("value", New ScriptFunction(AddressOf valuecmd))
        AddCmd("legacy", New ScriptFunction(AddressOf legacycmd))
        AddCmd("tointeger", New ScriptFunction(AddressOf ToInteger))
        AddCmd("copy", New ScriptFunction(AddressOf CopyCmd))
        AddCmd("memory", New ScriptFunction(AddressOf MemCmd))
        AddCmd("bytes", New ScriptFunction(AddressOf bytescmd))
        AddCmd("spi", New ScriptFunction(AddressOf SpiCmd))



    End Sub

#Region "Structures and Enum"

    Structure CMD
        Dim RESULT As String 'Variable to hold output (optional)
        Dim OPPER As String 'How to handle output (=,+=,-=,&=)
        Dim ARGS() As ScriptVariable
        Dim CONNECTS() As String 'How to handle ARGS (-,+,&)
        Dim Type As DataType 'Type of command that needs to be commonly shared
    End Structure

    Enum DataType
        Null = 0
        Int = 1
        Str = 2
        Bytes = 3
        Bool = 4
        Err = 5
        Unknown = 6
        IntFunc = 7
        Variable = 8
        Hex = 9
        IntEvent = 10
        Any = 11
    End Enum

    Structure FUNC
        Dim NAME As String 'Name of the function
        Dim SUBF As String 'Name of the sub function (optional)
        Dim ARGS() As String 'Arguments to execute
        Dim Index As Integer 'The index of the function (usually 0)
    End Structure

    Private Enum LineType
        Normal = 0
        Condition = 1
        Location = 2
        Jump = 3
        ExitF = 4
        ReturnF = 5
    End Enum

    Private Structure Element
        Dim lineNumber As Integer
        Dim Type As LineType
        Dim LineStr As String
        Dim LineCond As Condition
        Dim Location As String
        Dim LocIndex As Integer 'The point in the script where a location points to
        Public Overrides Function ToString() As String
            Return LineStr
        End Function
    End Structure

    Private Class ElementReturn
        Public Type As ReturnType
        Public Var As ScriptVariable
        Public Jump As String
        Public ErrMsg As String
        Public Line As Integer
        Public ExitStr As String
    End Class

    Private Enum ReturnType
        null = 0
        scriptvar = 1
        jump = 2
        Err = 3
        ExitNow = 4
    End Enum

    Private Structure Condition
        Dim IfStatement As String '(True) or (False)
        Dim IfTrue() As Element
        Dim IfElse() As Element
        Dim DoOpposite As Boolean 'NOT or !()
    End Structure

    Class ScriptVariable
        Private intType As DataType 'Our internal type 
        Private intErrMsg As String 'Error message
        Private intBool As Boolean
        Private intBytes() As Byte
        Private intStr As String
        Private intInt As Long
        Private intName As String
        Private intFUNC As FUNC

        Sub New(ByVal Name As String, Optional ByVal SetType As DataType = DataType.Unknown)
            intName = Name
            intType = SetType
        End Sub

        Public Property MyType() As DataType
            Get
                Return intType
            End Get
            Set(ByVal value As DataType)
                intType = value
            End Set
        End Property

        Public ReadOnly Property IsNothing() As Boolean
            Get
                Select Case intType
                    Case DataType.Int
                        If intInt = 0 Then Return True
                    Case DataType.Bytes
                        If intBytes Is Nothing Then Return True
                    Case DataType.Str
                        If intStr = "" Then Return True
                End Select
                Return False
            End Get
        End Property

        Public Property Value() As Object
            Get
                Select Case intType
                    Case DataType.Int
                        Return intInt
                    Case DataType.Str
                        Return """" & intStr & """"
                    Case DataType.Bytes
                        Return intBytes
                    Case DataType.Bool
                        Return intBool
                    Case DataType.Err
                        Return intErrMsg
                    Case DataType.Unknown
                        Return Nothing
                    Case DataType.IntFunc
                        Return intFUNC
                    Case DataType.IntEvent
                        Return intFUNC
                    Case DataType.Variable
                        Return intName
                    Case Else
                        Return Nothing
                End Select
            End Get
            Set(ByVal NewValue As Object)
                If intType = DataType.Variable Then
                    intName = CStr(NewValue)
                Else
                    If NewValue Is Nothing Then
                        intType = DataType.Null
                        intBool = Nothing
                        intBytes = Nothing
                        intStr = Nothing
                        intInt = Nothing
                        intFUNC = Nothing
                    ElseIf NewValue.GetType Is GetType(String) Then
                        intType = DataType.Str
                        intStr = RemoveStr(CStr(NewValue))
                    ElseIf NewValue.GetType Is GetType(Boolean) Then
                        intType = DataType.Bool
                        intBool = CBool(NewValue)
                        If intBool Then
                            intInt = 1
                        Else
                            intInt = 0
                        End If
                    ElseIf NewValue.GetType Is GetType(FUNC) Then
                        intFUNC = CType(NewValue, FUNC)
                    ElseIf NewValue.GetType Is GetType(Integer) Then
                        intType = DataType.Int
                        intInt = CLng(NewValue)
                    ElseIf NewValue.GetType Is GetType(UInteger) Then
                        intType = DataType.Int
                        intInt = CLng(NewValue)
                    ElseIf NewValue.GetType Is GetType(Long) Then
                        intType = DataType.Int
                        intInt = CLng(NewValue)
                    ElseIf NewValue.GetType Is GetType(Double) Then
                        intType = DataType.Int
                        intInt = CLng(NewValue)
                    ElseIf NewValue.GetType Is GetType(Byte()) Then
                        intType = DataType.Bytes
                        intBytes = CType(NewValue, Byte())
                    End If
                End If
            End Set
        End Property

        Public Sub CreateError(ByVal ErrMsg As String)
            intErrMsg = ErrMsg
            intType = DataType.Err
        End Sub

        Public Property Name() As String
            Get
                Return intName
            End Get
            Set(ByVal value As String)
                intName = value
            End Set
        End Property

        Public Function GetString() As String
            Select Case intType
                Case DataType.Int
                    Return CStr(intInt)
                Case DataType.Str
                    Return """" & intStr & """"
                Case DataType.Bool
                    If intInt = 1 Then Return "True"
                    Return "False"
                Case DataType.Bytes
                    Return BytestoHex(intBytes)
                Case DataType.Variable
                    Return intName 'Return variable name
                Case DataType.Null
                    Return "Nothing"
                Case Else
                    Return ""
            End Select
        End Function

        Public Overrides Function ToString() As String
            Return intName
        End Function
    End Class

    Public Function CloneVariable(ByRef InputVar As ScriptVariable) As ScriptVariable
        Dim NewVar As New ScriptVariable(InputVar.Name, InputVar.MyType)
        NewVar.Value = InputVar.Value
        Return NewVar
    End Function

    Structure FunctionHolder
        Dim Name As String
        Dim Func As System.Delegate
    End Structure

    Private Class ScriptFile
        Public MinBuild As Integer = 0
        Public IsError As Boolean = False
        Public Name As String 'The filename of the script being loaded
        Public ErrDesc As String = "No error defined"
        Public LineCount As Integer
        Private MainCollector As New ArrayList  'Collects elements
        Private EventCollector As New ArrayList 'Collects elements
        Private IfCollector As ArrayList 'Collects elements
        Private CurrentEvent As String = "MAIN"
        Private EventNames As New ArrayList
        Private EventElements As New ArrayList
        Private OnCondition As Boolean = False

        Sub New(ByVal fnName As String, ByVal data() As String)
            Dim i As Integer = 0
            Dim uline, nline As String
            Dim WorkLine As String
            IsError = False
            Name = fnName
            LineCount = data.Length
            For i = 0 To data.Length - 1
                If Not data(i) Is Nothing Then
                    nline = Trim(RemoveComment(data(i).Replace(vbTab, " ")))
                    uline = UCase(nline)
                    If Not nline = "" Then
                        If uline.StartsWith("CREATEEVENT(") Then 'Creating new event
                            If uline.EndsWith(")") Then
                                nline = Mid(nline, 13)
                                CurrentEvent = Trim(Mid(nline, 1, nline.Length - 1))
                                EventCollector = New ArrayList
                            Else
                                ErrDesc = "CreateEvent command not compete" : IsError = True : Exit Sub
                            End If
                        ElseIf uline = "ENDEVENT" Then
                            AddEvent(EventCollector, CurrentEvent)
                            CurrentEvent = "MAIN"
                        ElseIf uline.StartsWith("REQUIRE(") And uline.EndsWith(")") Then 'Creating a subscript
                            nline = Mid(nline, 9)
                            nline = Trim(Mid(nline, 1, nline.Length - 1))
                            If IsNumeric(nline) Then MinBuild = CInt(nline)
                        ElseIf uline = "EXIT" Or uline.StartsWith("EXIT ") Then
                            WorkLine = Trim(Mid(nline, 5))
                            AddToCollector(GetExitElement(i, WorkLine))
                        ElseIf uline.StartsWith("RETURN ") Then
                            WorkLine = Trim(Mid(nline, 8))
                            AddToCollector(GetReturnElement(WorkLine, i))
                        ElseIf uline.StartsWith("IF ") Then 'Creating If statement
                            If OnCondition Then
                                ErrDesc = "Can not process a If statement inside a If statement" : IsError = True : Exit Sub
                            End If
                            WorkLine = Trim(Mid(nline, 3))
                            OnCondition = True
                            IfCollector = New ArrayList
                            IfCollector.Add(WorkLine)
                        ElseIf uline.StartsWith("ENDIF") Then
                            If Not OnCondition Then
                                ErrDesc = "EndIf found before If statement" : IsError = True : Exit Sub
                            End If
                            OnCondition = False
                            Dim retStr As String = ""
                            Dim e As Element = GetConditionElement(IfCollector, retStr)
                            If Not retStr = "" Then ErrDesc = "If command invalid" : IsError = True : Exit Sub
                            If e.Type = LineType.Condition Then AddToCollector(e)
                        ElseIf uline.EndsWith(":") Then 'Creating location point
                            WorkLine = Trim(Mid(nline, 1, nline.Length - 1))
                            AddToCollector(GetLocElement(WorkLine, i))
                        ElseIf uline.StartsWith("GOTO ") Then 'Creating jump point
                            WorkLine = Trim(Mid(nline, 5))
                            AddToCollector(GetJumpElement(WorkLine, i))
                        Else
                            AddToCollector(GetLineElement(nline, i))
                        End If
                    End If
                End If
            Next
            If OnCondition Then
                ErrDesc = "IF statement not closed with EndIf command" : IsError = True : Exit Sub
            ElseIf Not CurrentEvent = "MAIN" Then
                ErrDesc = "Current event not closed with EndEvent command" : IsError = True : Exit Sub
            End If
            AddEvent(MainCollector, "MAIN")
        End Sub

        Private Sub AddToCollector(ByVal e As Element)
            If OnCondition Then
                IfCollector.Add(e)
            Else
                If CurrentEvent = "MAIN" Then
                    MainCollector.Add(e)
                Else
                    EventCollector.Add(e)
                End If
            End If
        End Sub

        Private Function GetConditionElement(ByVal obj As ArrayList, ByVal ErrorRet As String) As Element
            If obj Is Nothing Then Return Nothing
            If obj.Count < 2 Then Return Nothing
            Dim e As Element
            Dim IfCol As New ArrayList 'Collects elements
            Dim ElseCol As New ArrayList 'Collects elements
            Dim CollectingElse As Boolean = False
            Dim i As Integer
            Dim IfStr As String = CStr(obj(0))
            For i = 1 To obj.Count - 1
                e = CType(obj(i), Global.FlashcatUSB.Script.Element)
                If UCase(e.LineStr) = "ELSE" Then
                    CollectingElse = True
                Else
                    If Not CollectingElse Then
                        IfCol.Add(e) 'colleting if
                    Else
                        ElseCol.Add(e) 'collecting else
                    End If
                End If
            Next
            Dim c As New Condition
            If IfCol.Count > 0 Then
                c.IfTrue = DirectCast(IfCol.ToArray(GetType(Element)), Element())
            End If
            If ElseCol.Count > 0 Then
                c.IfElse = DirectCast(ElseCol.ToArray(GetType(Element)), Element())
            End If
            If UCase(IfStr).StartsWith("NOT ") Then
                IfStr = Trim(Mid(IfStr, 4))
                c.DoOpposite = True
            ElseIf IfStr.StartsWith("!(") And IfStr.EndsWith(")") Then
                IfStr = Trim(Mid(IfStr, 2))
                c.DoOpposite = True
            End If
            If IfStr.StartsWith("(") And IfStr.EndsWith(")") Then
                IfStr = Mid(IfStr, 2, IfStr.Length - 2)
            End If
            c.IfStatement = IfStr
            Dim ret As New Element
            ret.Type = LineType.Condition
            ret.LineCond = c
            Return ret
        End Function

        Private Function GetLocElement(ByVal LineStr As String, ByVal LineNum As Integer) As Element
            Dim e As New Element
            e.Type = LineType.Location
            e.lineNumber = LineNum
            e.Location = LineStr
            If CurrentEvent = "MAIN" Then
                e.LocIndex = MainCollector.Count
            Else
                e.LocIndex = EventCollector.Count
            End If
            Return e
        End Function

        Private Function GetJumpElement(ByVal LineStr As String, ByVal LineNum As Integer) As Element
            Dim e As New Element
            e.Type = LineType.Jump
            e.lineNumber = LineNum
            e.Location = LineStr
            Return e
        End Function

        Private Function GetExitElement(ByVal LineNum As Integer, ByVal ExitStr As String) As Element
            Dim e As New Element
            e.Type = LineType.ExitF
            e.lineNumber = LineNum
            e.LineStr = ExitStr
            Return e
        End Function

        Private Function GetReturnElement(ByVal input As String, ByVal LineNum As Integer) As Element
            Dim e As New Element
            e.Type = LineType.ReturnF
            e.LineStr = input
            e.lineNumber = LineNum
            Return e
        End Function

        Private Function GetLineElement(ByVal LineStr As String, ByVal LineNum As Integer) As Element
            Dim e As New Element
            e.Type = LineType.Normal
            e.lineNumber = LineNum
            e.LineStr = LineStr
            Return e
        End Function

        Private Sub AddEvent(ByVal EventObj As ArrayList, ByVal Name As String)
            EventNames.Add(Name)
            EventElements.Add(EventObj)
        End Sub

        Public Function GetEvent(ByVal EventName As String) As Element()
            Dim i As Integer
            If EventNames.Count = 0 Then Return Nothing
            For i = 0 To EventNames.Count - 1
                If UCase(CStr(EventNames.Item(i))) = UCase(EventName) Then
                    Dim selArr As ArrayList = CType(EventElements(i), ArrayList)
                    Return DirectCast(selArr.ToArray(GetType(Element)), Element())
                End If
            Next
            Return Nothing
        End Function

        Public Function GetEventNames() As String()
            If EventNames.Count = 0 Then Return Nothing
            Return DirectCast(EventNames.ToArray(GetType(String)), String())
        End Function

        Public Function FindLocation(ByVal CurrentEvent As String, ByVal label As String) As Integer
            Dim i As Integer
            Dim uLabel As String = UCase(label)
            If EventNames.Count = 0 Then Return Nothing
            For i = 0 To EventNames.Count - 1
                If UCase(CStr(EventNames(i))) = UCase(CurrentEvent) Then
                    Dim selArr As ArrayList = CType(EventElements(i), ArrayList)
                    Dim es() As Element = DirectCast(selArr.ToArray(GetType(Element)), Element())
                    Dim e As Element
                    For Each e In es
                        If e.Type = LineType.Location Then
                            If UCase(e.Location) = uLabel Then
                                Return e.LocIndex
                            End If
                        End If
                    Next
                    Return -1
                End If
            Next
            Return -1 'Not found
        End Function

    End Class

    Structure SV_Holder
        Dim eventname As String
        Dim vars() As ScriptVariable
    End Structure

#End Region
    'Sends a error msg to the console or a error to a handler
    Private Sub OnError(ByVal Msg As String)
        StopScript = True
        MemoryError = Msg
        If Not ScriptRunning Then
            RaiseEvent printf(Msg)
        End If
    End Sub

    Public Sub Abort() 'Quits the current script
        Quit = True
    End Sub

    Public Sub PrintInformation()
        RaiseEvent printf("FlashcatUSB Script Engine build: " & Build)
    End Sub
    'Call this to load a script file into memory
    Public Sub LoadScriptFile(ByVal BcsFile As IO.FileInfo)
        If BcsFile.Exists Then 'Only load it if it exists
            Dim ScriptWasUnloaded As Boolean = UnloadDeviceScript() 'Will unload if a current script is loaded
            ScriptName = BcsFile.Name
            RaiseEvent printf("Loading script: " & BcsFile.Name)
            Dim ScrTxt() As String = ReadFile(BcsFile.FullName)
            If Not ScrTxt Is Nothing Then
                Dim ns As New ScriptFile(BcsFile.Name, ScrTxt)
                Dim minbuild As Integer = ns.MinBuild
                If ns.IsError Then
                    RaiseEvent printf("Error loading: " & ns.ErrDesc)
                Else
                    If minbuild > MainApp.Build Then
                        RaiseEvent printf("Can not execute script! Software build " & minbuild & " required")
                        Exit Sub
                    End If
                    If Scripts Is Nothing Then Scripts = New ArrayList
                    Scripts.Add(ns)
                End If
            Else
                RaiseEvent printf("Can not load because there is nothing to run")
            End If
            RunScriptFile()
        Else
            RaiseEvent printf("Unable to load script (file not found)")
        End If
    End Sub
    'Call this to run a script file that is loaded in memory
    Public Function RunScriptFile(Optional ByVal DefaultEvent As String = "MAIN") As ScriptVariable
        ScriptRunning = True
        Dim i As Integer
        Dim sf As ScriptFile = Nothing
        Dim Linenumber As Integer = 0
        If Not Scripts Is Nothing Then
            StopScript = False
            For Each sf In Scripts
                If StopScript Then ScriptRunning = False : Return Nothing 'We are quiting
                Dim e() As Element = sf.GetEvent(DefaultEvent)
                If e Is Nothing Then
                    RaiseEvent printf("Script error: event not found: " & DefaultEvent)
                    Return Nothing
                End If
                For i = 0 To e.Length - 1
                    If StopScript Then ScriptRunning = False : Return Nothing 'We are quiting
                    If Not UCase(DefaultEvent) = "MAIN" And ReloadVariables Then
                        ReloadVariables = False
                        LoadEventVariables(DefaultEvent)
                    End If
                    Linenumber = e(i).lineNumber + 1
                    Dim er As ElementReturn = ExecuteElement(e(i))
                    Select Case er.Type
                        Case ReturnType.Err
                            MemoryError = er.ErrMsg
                            Linenumber = er.Line
                            GoTo OnScriptError
                        Case ReturnType.ExitNow
                            Return Nothing
                        Case ReturnType.jump
                            i = sf.FindLocation(DefaultEvent, er.Jump)
                            If i = -1 Then
                                MemoryError = "location not found: " & er.Jump
                                GoTo OnScriptError
                            End If
                        Case ReturnType.null
                        Case ReturnType.scriptvar
                            Return er.Var
                    End Select
                Next
            Next
        Else
            RaiseEvent printf("No scripts to run")
        End If
        ScriptRunning = False : Return Nothing 'Not a return
OnScriptError:
        RaiseEvent printf("Error in script " & sf.Name & " on line " & CStr(Linenumber))
        RaiseEvent printf("Reason: " & MemoryError)
        MemoryError = "" 'Erase message
        ScriptRunning = False : Return Nothing
    End Function

    Private Function ExecuteElement(ByVal e As Element) As ElementReturn
        Dim er As New ElementReturn
        er.Type = ReturnType.null
        Dim Result As Boolean
        Dim x As Integer
        If e.Type = LineType.Normal Then
            Try
                Result = ExecuteCommand(e.LineStr)
            Catch ex As Exception
                er.Type = ReturnType.Err
                er.ErrMsg = "Unspecified Error"
                Return er
            End Try
            If Not Result Then
                er.Line = e.lineNumber
                er.Type = ReturnType.Err
                er.ErrMsg = MemoryError
                Return er
            End If
        ElseIf e.Type = LineType.ExitF Then
            If UCase(e.LineStr) = "SCRIPT" Then
                StopScript = True
            End If
            er.Type = ReturnType.ExitNow
            er.ExitStr = e.LineStr
        ElseIf e.Type = LineType.Jump Then
            er.Type = ReturnType.jump
            er.Jump = e.Location
        ElseIf e.Type = LineType.Condition Then
            Dim IFC As Condition = e.LineCond
            Dim res As Boolean = ProcessIf(IFC.IfStatement)
            Dim e2() As Element
            If (res And Not IFC.DoOpposite) Or Not res And IFC.DoOpposite Then
                e2 = IFC.IfTrue
            Else
                e2 = IFC.IfElse
            End If
            If Not e2 Is Nothing Then
                Dim imm As ElementReturn
                For x = 0 To e2.Length - 1
                    imm = ExecuteElement(e2(x))
                    If imm.Type = ReturnType.ExitNow Then
                        If UCase(imm.ExitStr) = "EVENT" Then
                            er.Type = ReturnType.ExitNow
                        ElseIf UCase(imm.ExitStr) = "SCRIPT" Then 'Exit the entire script
                            er.Type = ReturnType.ExitNow
                            StopScript = True
                        Else
                            er.Type = ReturnType.null 'Exit IF, but not Event or script
                        End If
                        Return er
                    ElseIf imm.Type = ReturnType.jump Then
                        Return imm
                    ElseIf imm.Type = ReturnType.scriptvar Then 'Return
                        Return imm
                    ElseIf imm.Type = ReturnType.Err Then
                        er.Type = ReturnType.Err
                        er.ErrMsg = imm.ErrMsg
                        er.Line = imm.Line
                    End If
                Next
            End If
        ElseIf e.Type = LineType.ReturnF Then
            Dim arg As String = e.LineStr
            If ProcessArgument(arg) Then
                er.Type = ReturnType.scriptvar
                er.Var = ConvertArgToSv(arg)
            Else 'Was error?
                er.Type = ReturnType.Err
                er.ErrMsg = "Unspecified Error"
            End If
        End If
        Return er
    End Function
    'Executes a event
    Public Function ExecuteEvent(ByVal obj As FUNC) As ScriptVariable
        Dim sv() As ScriptVariable = ArgsToVariables(obj.ARGS)
        SetEventVariables(obj.NAME, sv)
        ReloadVariables = True
        Dim ret As ScriptVariable = RunScriptFile(obj.NAME)
        ReloadVariables = True
        Return ret
    End Function

    Public Function ArgsToVariables(ByVal input() As String) As ScriptVariable()
        If input Is Nothing Then Return Nothing
        Dim sv(input.Length - 1) As ScriptVariable
        Dim i As Integer
        Dim counter As Integer = 1
        For i = 0 To sv.Length - 1
            sv(i) = ConvertArgToSv(input(i), i + 1)
        Next
        Return sv
    End Function
    'Returns TRUE if the name given is a user defined event
    Public Function IsEvent(ByVal Name As String) As Boolean
        Dim i As Integer = InStr(Name, "(")
        If i > 1 Then Name = Mid(Name, 1, i - 1)
        Dim uName As String = UCase(Name)
        If Scripts Is Nothing Then Return False
        Dim sf As ScriptFile
        For Each sf In Scripts
            Dim eventnames() As String = sf.GetEventNames
            Dim eventline As String
            For Each eventline In eventnames
                If UCase(eventline) = uName Then Return True
            Next
        Next
        Return False
    End Function
    'Called when USB is unplugged or from console
    Public Function UnloadDeviceScript() As Boolean
        If Scripts Is Nothing Then Return False
        If Scripts.Count = 0 Then Return False 'No scripts loaded
        Dim CurrentName As String = ScriptName
        If GuiForm IsNot Nothing Then GuiForm.RemoveAllTabs()
        Application.DoEvents()
        MemoryError = "" 'Erase error
        ResetScript()
        Quit = True
        Scripts = New ArrayList
        UserTabCount = 0
        Dim i As Integer
        For i = 0 To OurFlashDevices.Count - 1
            RemoveMemoryDevice(CInt(OurFlashDevices(i)))
        Next
        OurFlashDevices.Clear()
        RaiseEvent printf("Script file (" & CurrentName & ") unloaded")
        Return True 'Script successfully unloaded
    End Function
    'Resets the script (removes tabs) and also removes any variables
    Public Sub ResetScript()
        ReloadVariables = False
        MyVariables = Nothing
        MyEventVariables = New ArrayList
        ReloadVariables = False
        MemoryError = ""
        ScriptName = ""
    End Sub


    'Used to execute a console command 
    Public Function ExecuteCommand(ByVal Cmdline As String) As Boolean
        If Cmdline = "" Then Return True
        Cmdline = Cmdline.Trim.Replace(vbTab, "")
        If Cmdline = "" Then Return False
        Dim MyCmd As CMD = Nothing
        If Not ParseCmd(Cmdline, MyCmd) Then 'Convert the cmdline into its parts
            OnError("error parsing command: " & Cmdline)
            Return False
        End If
        'execute every function in the arguments
        If Not ExecuteCommand_ExeFunctions(MyCmd) Then Return False
        'execute every event in the arguments
        If Not ExecuteCommand_ExeEvents(MyCmd) Then Return False
        'Convert all Variables in arguments to their datatype
        If Not ExecuteCommand_ParseVariables(MyCmd) Then Return False
        'Now we need to compile the results into one value
        Dim NewVariable As ScriptVariable = Nothing
        If Not ExecuteCommand_CompileResult(MyCmd, NewVariable) Then Return False
        If NewVariable Is Nothing Then Return True 'Nothing to save and no errors
        If Not MyCmd.RESULT Is Nothing Then NewVariable.Name = MyCmd.RESULT
        Return SetVariable(NewVariable, MyCmd.OPPER)
    End Function

    Private Function ExecuteCommand_ParseVariables(ByRef MyCmd As CMD) As Boolean
        If MyCmd.ARGS Is Nothing Then Return True
        Dim i As Integer
        For i = 0 To MyCmd.ARGS.Length - 1
            If Not MyCmd.ARGS(i) Is Nothing Then
                If MyCmd.ARGS(i).MyType = DataType.Variable Then
                    Dim VarName As String = CStr(MyCmd.ARGS(i).Value)
                    If IsVariable(VarName) Then
                        Dim sv As ScriptVariable = GetVariable(VarName)
                        If Not sv.MyType = DataType.Bytes Then 'dont convert data var
                            MyCmd.ARGS(i) = sv 'Converts into argument
                        End If
                    Else
                        OnError("undefined function, event or variable: " & VarName)
                        Return False 'No such variable
                    End If
                End If
            End If
        Next
        Return True ' No errors
    End Function

    Private Function ExecuteCommand_ExeEvents(ByRef MyCmd As CMD) As Boolean
        If MyCmd.ARGS Is Nothing Then Return True
        Dim a As ScriptVariable
        Dim i As Integer = 0
        Dim sv As ScriptVariable
        For Each a In MyCmd.ARGS
            If Not a Is Nothing Then
                If a.MyType = DataType.IntEvent Then
                    sv = ExecuteEvent(CType(a.Value, FUNC))
                    If Not sv Is Nothing Then 'Function might return nothing
                        MyCmd.ARGS(i) = sv
                    Else 'Returns nothing
                        MyCmd.ARGS(i).MyType = DataType.Null 'Deleted
                    End If
                End If
            End If
            i = i + 1
        Next
        Return True
    End Function

    Private Function ExecuteCommand_ExeFunctions(ByRef MyCmd As CMD) As Boolean
        If MyCmd.ARGS Is Nothing Then Return True 'Nothing to execute
        Dim a As ScriptVariable
        Dim i As Integer = 0
        Dim sv As ScriptVariable
        For Each a In MyCmd.ARGS
            If Not a Is Nothing Then
                If a.MyType = DataType.IntFunc Then
                    sv = ExecuteFunction(CType(a.Value, FUNC))
                    If Not MemoryError = "" Then Return False 'Was a error
                    If Not sv Is Nothing Then 'Function might return nothing
                        MyCmd.ARGS(i) = sv
                    Else 'Returns nothing
                        MyCmd.ARGS(i) = Nothing 'Deleted
                    End If
                End If
            End If
            i = i + 1
        Next
        Return True
    End Function
    'Compiles a command and outputs it as a script variable
    Private Function ExecuteCommand_CompileResult(ByVal Input As CMD, ByRef NewVar As ScriptVariable) As Boolean
        Dim a() As ScriptVariable = Input.ARGS
        Dim svName As String = Input.RESULT
        If a.Length = 1 AndAlso a(0) IsNot Nothing Then 'Only one argument to convert 
            NewVar = CloneVariable(a(0)) 'So we do not overwrite our origional variable
        ElseIf a.Length > 1 Then 'Multiple arguments to process
            Dim i As Integer
            Dim BaseType As DataType = a(0).MyType
            If BaseType = DataType.Variable Then
                BaseType = GetVariableType(a(0).Name)
            End If
            Dim Sign As String = "+" 'We are going to add
            If BaseType = DataType.Str Or BaseType = DataType.Bytes Then Sign = "&"
            For i = 0 To a.Length - 1
                Dim ThisType As DataType = a(i).MyType
                If a(i).MyType = DataType.Variable Then ThisType = GetVariableType(a(i).Name)
                If (Not ThisType = BaseType) Then
                    If Not (BaseType = DataType.Str And a(0).MyType = DataType.Int) Then ' We can append INT to a STR
                        OnError("Error: data types in command are not the same") : Return False
                    End If
                End If
                If Not ComputeValue(NewVar, a(i), Sign) Then
                    OnError("Error: failed to compute command") : Return False
                End If
                If Not i = a.Length - 1 Then Sign = Input.CONNECTS(i) 'Not the last one 
            Next
        End If
        Return True 'No error
    End Function

    Public Function ProcessArgument(ByRef Cmdline As String) As Boolean
        Dim CmdParts() As String = SplitCmd(Cmdline)
        Dim resSV As ScriptVariable = Nothing
        Dim Result As CMD = ConvertPartsToCommands(CmdParts)
        If Not ExecuteCommand_ExeFunctions(Result) Then Return False
        If Not ExecuteCommand_ExeEvents(Result) Then Return False
        If Not ExecuteCommand_ParseVariables(Result) Then Return False
        If Not ExecuteCommand_CompileResult(Result, resSV) Then Return False
        If resSV Is Nothing Then
            Cmdline = Cmdline
        Else
            Cmdline = resSV.GetString
        End If
        Return True
    End Function

    Private Function ComputeValue(ByRef Current As ScriptVariable, ByVal ToCompute As ScriptVariable, ByVal Sign As String) As Boolean
        Dim Source As ScriptVariable
        If ToCompute.MyType = DataType.Variable Then
            Source = GetVariable(ToCompute.Name)
        Else
            Source = ToCompute
        End If
        If Current Is Nothing Then Current = New ScriptVariable(Source.Name, Source.MyType)
        If Current.MyType = DataType.Variable Then
            Current = GetVariable(Current.Name)
        Else
            Current = Current
        End If

        Select Case Current.MyType
            Case DataType.Int
                If Sign = "+" Then
                    Current.Value = CUInt(Current.Value) + CUInt(Source.Value)
                ElseIf Sign = "-" Then
                    Current.Value = CUInt(Current.Value) - CUInt(Source.Value)
                Else
                    Return False 'ERROR
                End If
            Case DataType.Str
                If Not Sign = "&" Then Return False
                Current.Value = """" & RemoveStr(CStr(Current.Value)) & RemoveStr(CStr(Source.Value)) & """"
            Case DataType.Bytes 'For copying bytes
                If Not Sign = "&" Then Return False
                Dim Bytes1() As Byte = CType(Current.Value, Byte())
                Dim Bytes2() As Byte = CType(Source.Value, Byte())
                If Bytes1 Is Nothing Then
                    Current.Value = Bytes2
                Else
                    Dim Res(Bytes1.Length + Bytes2.Length - 1) As Byte
                    Array.Copy(Bytes1, 0, Res, 0, Bytes1.Length)
                    Array.Copy(Bytes2, 0, Res, Bytes1.Length, Bytes2.Length)
                    Current.Value = Res
                End If
            Case DataType.Bool
                Current.Value = CBool(Source.Value)
        End Select
        Return True 'No Error
    End Function
    'Executes a script function (also processes argments)
    Private Function ExecuteFunction(ByVal obj As FUNC) As ScriptVariable
        Dim FncName As String = obj.NAME
        Dim fh As FunctionHolder
        For Each fh In ScriptFunctions
            If UCase(fh.Name) = UCase(FncName) Then
                Dim i As Integer
                If Not obj.ARGS Is Nothing Then
                    For i = 0 To obj.ARGS.Length - 1
                        If Not ProcessArgument(obj.ARGS(i)) Then
                            OnError("Error processing: " & obj.ARGS(i))
                            Return Nothing
                        End If
                    Next
                End If
                Dim o() As Object = {obj.ARGS, obj.Index, obj.SUBF}
                Dim sv As ScriptVariable = CType(fh.Func.DynamicInvoke(o), ScriptVariable) 'Returns script variable 
                Return sv
            End If
        Next
        OnError("Command not reconized: " & FncName)
        Return Nothing
    End Function
    'Processes a condition statement and returns TRUE or FALSE
    Private Function ProcessIf(ByVal IfCond As String) As Boolean
        Dim v1, v2 As ScriptVariable
        Dim CmdParts() As String = SplitCmd(IfCond)
        If CmdParts Is Nothing Then Return Nothing
        If CmdParts.Length = 1 Then
            If Not ProcessArgument(IfCond) Then Return False
            v1 = ConvertArgToSv(IfCond)
            If v1.MyType = DataType.Bool Then
                Return CBool(v1.Value)
            ElseIf v1.MyType = DataType.Int Then
                Return CBool(v1.Value)
            End If
            Return False
        End If
        Dim i As Integer
        Dim Sign As Integer = -1
        For i = 0 To CmdParts.Length - 1
            Select Case CmdParts(i)
                Case ">"
                Case "<"
                Case "="
                Case ">="
                Case "<="
                Case Else
                    GoTo SkipThis
            End Select
            If Not Sign = -1 Then
                OnError("Error in IF statement: mutliple opperands")
                Return False
            End If
            Sign = i
SkipThis:
        Next
        If Sign = -1 Then 'No sign, so evaluate as one
            If Not ProcessArgument(IfCond) Then Return False
            v1 = ConvertArgToSv(IfCond)
            If v1.MyType = DataType.Bool Then
                Return CBool(v1.Value)
            ElseIf v1.MyType = DataType.Int Then
                Return CBool(v1.Value)
            End If
            Return False
        End If
        Dim Part1 As String = ""
        Dim Part2 As String = ""
        For i = 0 To CmdParts.Length - 1
            If i < Sign Then
                Part1 = Trim(Part1 & " " & CmdParts(i))
            ElseIf i > Sign Then
                Part2 = Trim(Part2 & " " & CmdParts(i))
            End If
        Next
        'If Not ProcessArgument(Part1) Then Return False
        'If Not ProcessArgument(Part2) Then Return False
        ProcessArgument(Part1)
        ProcessArgument(Part2)
        v1 = ConvertArgToSv(Part1)
        v2 = ConvertArgToSv(Part2)

        If v1.MyType = DataType.Null Or v2.MyType = DataType.Null Then
        Else
            If Not v1.MyType = v2.MyType Then
                OnError("Error in IF statement: Type mismatch")
                Return False
            End If
        End If

        If v1.MyType = DataType.Str And CmdParts(Sign) = "=" Then
            If CStr(v1.Value) = CStr(v2.Value) Then Return True
            Return False
        ElseIf v1.MyType = DataType.Bool And CmdParts(Sign) = "=" Then
            If CBool(v1.Value) = CBool(v2.Value) Then Return True
            Return False
        ElseIf v1.MyType = DataType.Null And v2.MyType = DataType.Null And CmdParts(Sign) = "=" Then
            Return True 'both objects are null
        ElseIf v1.MyType = DataType.Null And CmdParts(Sign) = "=" Then
            If v2.MyType = DataType.Variable Then
                If GetVariable(CStr(v2.Value)) Is Nothing Then
                    Return True
                Else
                    Return False
                End If
            End If
        ElseIf v2.MyType = DataType.Null And CmdParts(Sign) = "=" Then
            If v1.MyType = DataType.Variable Then
                If GetVariable(CStr(v1.Value)) Is Nothing Then
                    Return True
                Else
                    Return False
                End If
            End If
        End If
        If Not v1.MyType = DataType.Int Then
            OnError("Error in IF statement: Type not supported")
            Return False
        End If
        If CmdParts(Sign) = ">" Then
            If CUInt(v1.Value) > CUInt(v2.Value) Then Return True
        ElseIf CmdParts(Sign) = "<" Then
            If CUInt(v1.Value) < CUInt(v2.Value) Then Return True
        ElseIf CmdParts(Sign) = ">=" Then
            If CUInt(v1.Value) >= CUInt(v2.Value) Then Return True
        ElseIf CmdParts(Sign) = "<=" Then
            If CUInt(v1.Value) <= CUInt(v2.Value) Then Return True
        ElseIf CmdParts(Sign) = "=" Then
            If CUInt(v1.Value) = CUInt(v2.Value) Then Return True
        End If
        Return False
    End Function

    Private Function IsFunction(ByVal fncName As String) As Boolean
        Dim i As Integer
        For i = 1 To fncName.Length
            If Mid(fncName, i, 1) = "." Then fncName = Mid(fncName, 1, i - 1) : Exit For
            If Mid(fncName, i, 1) = "(" Then fncName = Mid(fncName, 1, i - 1) : Exit For
        Next
        If i > 0 And fncName.EndsWith(")") Then fncName = Mid(fncName, 1, i - 1)
        Dim fh As FunctionHolder
        For Each fh In ScriptFunctions
            If UCase(fh.Name) = UCase(fncName) Then
                Return True
            End If
        Next
        Return False 'Not found
    End Function
    'Adds a command to our script engine
    Private Sub AddCmd(ByVal NameStr As String, ByVal f As System.Delegate)
        Dim fh As FunctionHolder
        fh.Func = f
        fh.Name = NameStr
        ScriptFunctions.Add(fh)
    End Sub

    Private Function GetErrorVariable(ByVal msg As String) As ScriptVariable
        Dim sv As New ScriptVariable("RNDERROR", DataType.Err)
        sv.CreateError(msg)
        Return sv
    End Function
    'Sets or creates a variable
    Private Function SetVariable(ByVal MyVariable As ScriptVariable, Optional ByVal Method As String = "=") As Boolean
        Dim VarName As String = MyVariable.Name
        If VarName = "" Then Return True
        Dim VarIndex As Integer = -1
        If VarName.Contains("(") And VarName.EndsWith(")") Then
            Dim p As String = Mid(VarName, InStr(VarName, "(") + 1)
            VarName = Mid(VarName, 1, InStr(VarName, "(") - 1)
            p = ConvertIfVariable(Mid(p, 1, p.Length - 1))
            If isHex(p) Then p = CStr(HexToInt(p))
            If IsNumeric(p) Then VarIndex = CInt(p)
        End If
        Dim i As Integer
        Dim VarCount As Integer = 0
        If MyVariables Is Nothing Then GoTo CreateNew
        VarCount = MyVariables.Length
        For i = 0 To MyVariables.Length - 1
            If UCase(MyVariables(i).Name) = UCase(VarName) Then
                Select Case Method
                    Case "=" 'Replace variable
                        If MyVariables(i).MyType = DataType.Bytes And Not VarIndex = -1 Then
                            Dim d() As Byte = CType(MyVariables(i).Value, Byte())
                            If VarIndex + 1 > d.Length Then
                                OnError("Error, unable to set byte variable (" & VarName & "): index out of bounds") : Return False
                            End If
                            If Not MyVariable.MyType = DataType.Int Then
                                OnError("Error, unable to set byte variable (" & VarName & "): value is not a byte") : Return False
                            End If
                            Dim valToSet As UInt32 = CUInt(MyVariable.Value)
                            If Not valToSet >= 0 And valToSet <= 255 Then
                                OnError("Error, unable to set byte variable (" & VarName & "): value is not a byte") : Return False
                            End If
                            d(VarIndex) = CByte(valToSet)
                            MyVariables(i).Value = d
                        Else
                            MyVariables(i) = MyVariable
                        End If
                    Case "+="
                        If MyVariables(i).MyType = DataType.Int And MyVariable.MyType = DataType.Int Then
                            MyVariables(i).Value = CInt(MyVariables(i).Value) + CInt(MyVariable.Value)
                        Else
                            OnError("Error, unable to add variable (" & VarName & "):  it is not a integer") : Return False
                        End If
                    Case "-="
                        If MyVariables(i).MyType = DataType.Int And MyVariable.MyType = DataType.Int Then
                            MyVariables(i).Value = CInt(MyVariables(i).Value) - CInt(MyVariable.Value)
                        Else
                            OnError("Error, unable to add variable (" & VarName & "): it is not a integer") : Return False
                        End If
                    Case "&="
                        If MyVariables(i).MyType = DataType.Str And MyVariable.MyType = DataType.Str Then
                            MyVariables(i).Value = CStr(MyVariables(i).Value) & CStr(MyVariable.Value)
                        Else
                            OnError("Error, unable to add variable (" & VarName & "):  it is not a string") : Return False
                        End If
                    Case Else
                End Select
                Return True
            End If
        Next
        'Not found, create a new one
CreateNew:
        ReDim Preserve MyVariables(VarCount)
        MyVariables(MyVariables.Length - 1) = MyVariable
        Return True
    End Function
    'Sets a group of variables that are used only for a specific event
    Private Function SetEventVariables(ByVal EventName As String, ByVal sv() As ScriptVariable) As Boolean
        Dim h As SV_Holder
        Dim i As Integer
        If MyEventVariables Is Nothing Then
            MyEventVariables = New ArrayList
        Else
            For i = 0 To MyEventVariables.Count - 1
                h = CType(MyEventVariables(i), SV_Holder)
                If UCase(h.eventname) = UCase(EventName) Then
                    h.vars = sv
                    MyEventVariables(i) = h
                    Return True
                End If
            Next
        End If
        h.eventname = EventName
        h.vars = sv
        MyEventVariables.Add(h)
        Return True
    End Function

    Private Function GetEventVariables(ByVal EventName As String) As ScriptVariable()
        If MyEventVariables Is Nothing Then Return Nothing
        Dim h As SV_Holder
        Dim i As Integer
        For i = 0 To MyEventVariables.Count - 1
            h = CType(MyEventVariables(i), SV_Holder)
            If UCase(h.eventname) = UCase(EventName) Then
                Return h.vars
            End If
        Next
        Return Nothing
    End Function
    'Loads all of the variables for a specific event (call at the beginning of a event or returning to a event)
    Private Function LoadEventVariables(ByVal EventName As String) As Boolean
        Dim SV() As ScriptVariable = GetEventVariables(EventName)
        If SV Is Nothing Then Return True
        Dim V As ScriptVariable
        For Each V In SV
            SetVariable(V)
        Next
        Return True
    End Function
    'Returns TRUE if variable name is found
    Private Function IsVariable(ByVal VarName As String) As Boolean
        Dim i As Integer
        Dim uVarName As String = UCase(VarName)
        If MyVariables Is Nothing Then Return False
        For i = 0 To MyVariables.Length - 1
            If UCase(MyVariables(i).Name) = uVarName Then Return True
        Next
        Return False
    End Function

    'Retreives a variable from our memory
    Private Function GetVariable(ByVal VarName As String) As ScriptVariable
        Dim i As Integer
        Dim uVarName As String = UCase(VarName)
        If MyVariables Is Nothing Then Return Nothing
        For i = 0 To MyVariables.Length - 1
            If UCase(MyVariables(i).Name) = uVarName Then Return MyVariables(i)
        Next
        Return Nothing 'Not found
    End Function
    'Checks to See if VAR is integer, and if it is, returns it. Otherwise returns -1
    Private Function IsVarInteger(ByRef VarName As String) As Boolean
        Dim SV As ScriptVariable = GetVariable(VarName)
        If SV Is Nothing Then Return False
        If SV.MyType = DataType.Int Then
            VarName = CStr(SV.Value)
            Return True
        End If
        Return False
    End Function

    Private Function IsVarBytes(ByVal VarName As String, ByRef VarBytes() As Byte) As Boolean
        Dim SV As ScriptVariable = GetVariable(VarName)
        If SV Is Nothing Then Return False
        If SV.MyType = DataType.Bytes Then
            VarBytes = CType(SV.Value, Byte())
            Return True
        End If
        Return False
    End Function

    Private Function GetVariableType(ByVal VarName As String) As DataType
        Dim i As Integer
        Dim uVarName As String = UCase(VarName)
        If MyVariables Is Nothing Then Return DataType.Unknown
        For i = 0 To MyVariables.Length - 1
            If UCase(MyVariables(i).Name) = uVarName Then Return MyVariables(i).MyType
        Next
        Return Nothing 'Not found
    End Function
    'Checks to See if VAR is string, and if it is, returns it. Otherwise returns nothing
    Private Function IsVarString(ByRef VarName As String) As Boolean
        Dim SV As ScriptVariable = GetVariable(VarName)
        If SV Is Nothing Then Return False
        If SV.MyType = DataType.Str Then
            VarName = CStr(SV.Value)
            Return True
        End If
        Return False
    End Function
    'Possibly converts the input if it is a variable of some kind
    Private Function ConvertIfVariable(ByVal input As String) As String
        If IsVarInteger(input) Then 'Converts if integer
        ElseIf IsVarString(input) Then 'Converts if string
        End If
        Return input
    End Function
    'Converts a datatype to its string form
    Private Function DataTypeToString(ByVal input As DataType) As String
        Select Case input
            Case DataType.Null
                Return "Null"
            Case DataType.Err
                Return "Error"
            Case DataType.Bool
                Return "Boolean"
            Case DataType.Bytes
                Return "Bytes"
            Case DataType.Hex
                Return "HEX"
            Case DataType.Int
                Return "Integer"
            Case DataType.IntFunc
                Return "Function"
            Case DataType.Str
                Return "String"
            Case DataType.Variable
                Return "Variable"
            Case Else
                Return "(Unknown)"
        End Select
    End Function
    'Returns the datatype of the given input
    Private Function GetInputsDataType(ByVal input As String) As DataType
        If IsNumeric(input) Then
            Return DataType.Int
        ElseIf isHex(input) Then
            Return DataType.Hex
        ElseIf isString(input) Then
            Return DataType.Str
        ElseIf IsFunction(input) Then
            Return DataType.IntFunc
        ElseIf IsVariable(input) Then
            Return DataType.Variable
        ElseIf UCase(input) = "TRUE" Or UCase(input) = "FALSE" Then
            Return DataType.Bool
        Else
            Return DataType.Unknown
        End If
    End Function
    'Checks to make sure arguments supplied match function
    Private Function ArgRequire(ByVal FncName As String, ByVal Number As Int32, ByVal Args() As String, ByVal Types() As DataType) As Boolean
        If Number = 0 Then
            If Not Args Is Nothing Then
                OnError("Error in " & FncName & ": function requires no arguments")
                Return False
            End If
            Return True 'No error
        End If
        If Args Is Nothing And Number > 0 Then
            OnError("Error in " & FncName & ": function requires " & Number & " argument(s)")
            Return False
        End If
        If Not Args.Length = Number Then
            OnError("Error in " & FncName & ": function requires " & Number & " argument(s)")
            Return False
        End If
        Dim i As Integer
        For i = 0 To Number - 1
            Dim MyType As DataType = GetInputsDataType(Args(i))
            If Types(i) = MyType Then
            ElseIf Types(i) = DataType.Any Then 'We will accept any input
            ElseIf Types(i) = DataType.Str And MyType = DataType.Int Then 'We can output integer as string
            ElseIf Types(i) = DataType.Str And MyType = DataType.Hex Then 'We can output hex as string
            ElseIf Types(i) = DataType.Str And MyType = DataType.Bool Then 'We can output bool as string
            ElseIf MyType = DataType.Variable Then
                Dim varType As DataType = GetVariableType(Args(i))
                If varType = Types(i) Then
                ElseIf Types(i) = DataType.Str And varType = DataType.Int Then 'We can output integer as string
                Else
                    OnError("Error in " & FncName & ": data type in argument " & CStr(i) & " is not " & DataTypeToString(Types(i)))
                    Return False
                End If
            Else
                OnError("Error in " & FncName & ": data type in argument " & CStr(i) & " is not " & DataTypeToString(Types(i)))
                Return False
            End If
        Next
        Return True 'No error, arguments verified
    End Function
    'Checks to make sure arguments supplied match function (same as require except no error)
    Private Function ArgAllow(ByVal FncName As String, ByVal Number As Int32, ByVal Args() As String, ByVal Types() As DataType) As Boolean
        If Number = 0 Then
            If Not Args Is Nothing Then Return False
            Return True 'No error
        End If
        If Args Is Nothing And Number > 0 Then Return False
        If Not Args.Length = Number Then Return False
        Dim i As Integer
        For i = 0 To Number - 1
            Dim MyType As DataType = GetInputsDataType(Args(i))
            If Types(i) = MyType Then
            ElseIf Types(i) = DataType.Str And MyType = DataType.Int Then 'We can output integer as string
            ElseIf Types(i) = DataType.Str And MyType = DataType.Hex Then 'We can output hex as string
            ElseIf MyType = DataType.Variable Then
                Dim varType As DataType = GetVariableType(Args(i))
                If varType = Types(i) Then
                ElseIf Types(i) = DataType.Str And varType = DataType.Int Then 'We can output integer as string
                Else
                    Return False
                End If
            Else
                Return False
            End If
        Next
        Return True 'No error, arguments verified
    End Function
    'Seperates a commandline into its various parts
    Public Function ParseCmd(ByVal CmdLine As String, ByRef Result As CMD) As Boolean
        CmdLine = Trim(CmdLine.Replace(vbTab, " ")) 'Do some basic formatting
        If CmdLine.StartsWith("=") Then Return False
        If CmdLine.StartsWith("+") Then Return False
        If CmdLine.StartsWith("-") Then Return False
        If CmdLine.StartsWith("&") Then Return False
        If CmdLine.StartsWith("(") Then Return False
        Dim CmdParts() As String = SplitCmd(CmdLine)
        If CmdParts Is Nothing Then Return False
        If CmdParts.Length = 1 Then
            ReDim Result.ARGS(0)
            Result.ARGS(0) = ConvertArgToSv(CmdParts(0))
        Else 'Mutli part command
            If CmdParts.Length < 3 Then Return False
            If CmdParts.Length Mod 2 = 0 Then Return False 'Must be a odd number always
            If CmdParts(1) = "=" Then
            ElseIf CmdParts(1) = "+=" Then
            ElseIf CmdParts(1) = "-=" Then
            ElseIf CmdParts(1) = "&=" Then
            Else
                Return False
            End If
            Dim ResultOpper As String = CmdParts(1)
            Dim ResultObj As String = CmdParts(0)
            If CmdParts.Length = 3 Then
                ReDim Result.ARGS(0)
                Result.ARGS(0) = ConvertArgToSv(CmdParts(2)) 'Only 1 argument
            Else
                Dim Arg(CmdParts.Length - 3) As String
                Array.Copy(CmdParts, 2, Arg, 0, Arg.Length)
                Result = ConvertPartsToCommands(Arg)
            End If
            Result.OPPER = ResultOpper
            Result.RESULT = ResultObj
        End If
        Return True
    End Function
    'Returns TRUE if the name matches a user control
    Private Function IsUserControl(ByVal Name As String) As Boolean
        If GuiForm Is Nothing Then Return False
        Dim C As Control
        If UserTabCount = 0 Then Return False
        Dim i As Integer
        Dim uTab As TabPage
        For i = 0 To UserTabCount - 1
            uTab = GuiForm.GetUserTab(i)
            For Each C In uTab.Controls
                If UCase(C.Name) = UCase(Name) Then Return True
            Next
        Next
        Return False
    End Function
    'Removes a user control from NAME
    Private Sub RemoveUserControl(ByVal Name As String)
        If GuiForm Is Nothing Then Exit Sub
        Dim C As Control
        If UserTabCount = 0 Then Exit Sub
        Dim i As Integer
        Dim uTab As TabPage
        For i = 0 To UserTabCount - 1
            uTab = GuiForm.GetUserTab(i)
            For Each C In uTab.Controls
                If UCase(C.Name) = UCase(Name) Then
                    uTab.Controls.Remove(C)
                    Exit Sub
                End If
            Next
        Next
    End Sub
    'Handles when the user clicks a button
    Private Sub ButtonHandler(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim MyButton As Button = CType(sender, Button)
        Dim EventToCall As String = MyButton.Name
        Dim EventThread As New Threading.Thread(AddressOf CallEvent)
        EventThread.Name = "Event:" & EventToCall
        EventThread.SetApartmentState(System.Threading.ApartmentState.STA)
        EventThread.Start(EventToCall)
        MyButton.Select()
    End Sub
    'Calls a event (wrapper for runscript)
    Public Sub CallEvent(ByVal EventName As Object)
        RaiseEvent printf("Button Hander::Calling Event: " & EventName)
        Dim eventnamestr As String = CStr(EventName)
        RunScriptFile(eventnamestr)
        RaiseEvent printf("Button Hander::Calling Event: Done")
    End Sub
    'Reads the data from flash and verifies it (returns nothing on error)
    Private Function ReadMemoryVerify(ByVal Start As UInteger, ByVal Length As Integer, ByVal index As Integer) As Byte()
        Dim FlashData1() As Byte = bcReadMemory(Start, Length, index, False)
        Dim FlashData2() As Byte = bcReadMemory(Start, Length, index, False)
        'UpdateProgress(0)
        If FlashData1 Is Nothing Then Return Nothing
        If FlashData2 Is Nothing Then Return Nothing
        If Not FlashData1.Length = FlashData2.Length Then Return Nothing 'Error already?
        If FlashData1.Length = 0 Then Return Nothing
        If FlashData2.Length = 0 Then Return Nothing
        Dim DataWords1() As UInt32 = ByteArrayToUintArray(FlashData1) 'This is the one corrected
        Dim DataWords2() As UInt32 = ByteArrayToUintArray(FlashData2)
        Dim Counter As Integer
        Dim CheckAddr, CheckValue, CheckArray() As UInt32
        Dim Data() As Byte
        Dim ErrCount As Integer = 0
        For Counter = 0 To DataWords1.Length - 1
            If Not DataWords1(Counter) = DataWords2(Counter) Then
                If ErrCount = 100 Then Return Nothing 'Too many errors
                ErrCount = ErrCount + 1
                CheckAddr = CUInt(Start + (Counter * 4)) 'Address to verify
                Data = bcReadMemory(CheckAddr, 4, index, True) 'Read the databack
                CheckArray = ByteArrayToUintArray(Data) 'Will only read one element
                CheckValue = CheckArray(0)
                If DataWords1(Counter) = CheckValue Then 'Our original data matched
                ElseIf DataWords2(Counter) = CheckValue Then 'Our original was incorrect
                    DataWords1(Counter) = DataWords2(Counter)
                Else
                    Return Nothing '3 reads of the same data did not match, return error!
                End If
            End If
        Next
        Dim DataOut() As Byte = UintArrayToByteArray(DataWords1)
        ReDim Preserve DataOut(FlashData1.Length - 1)
        Return DataOut 'Checked ok!
    End Function

#Region "String Parsing"

    Private Function IsLocalVariable(ByVal input As String) As Boolean
        If input.StartsWith("$") Then
            If input.Length > 1 Then
                If IsNumeric(Mid(input, 2)) Then Return True
            End If
        End If
        Return False
    End Function
    'Returns true if the sign is valid
    Private Function IsConnect(ByVal sign As String) As Boolean
        Select Case sign
            Case "+"
            Case "-"
            Case "&"
            Case Else
                Return False
        End Select
        Return True
    End Function
    'Strips out the parameter from a command
    Private Function StripParam(ByRef input As String, ByRef param As String) As Boolean
        Dim InQuote As Boolean = False
        Dim BuildStr As String = ""
        If input.StartsWith("(") Then input = Mid(input, 2)
        Dim i As Integer
        Dim sChr As Char
        Dim Level As Integer = 0
        For i = 1 To input.Length
            sChr = CChar(Mid(input, i, 1))
            If InQuote Then
                If sChr = """" Then InQuote = False
                BuildStr &= sChr
            Else
                If sChr = """" Then
                    InQuote = True
                ElseIf sChr = ")" And Level = 0 Then 'Parm complete
                    input = Trim(Mid(input, i + 1))
                    param = BuildStr
                    Return True
                ElseIf sChr = ")" Then
                    Level = Level - 1
                ElseIf sChr = "(" Then
                    Level = Level + 1
                End If
                BuildStr &= sChr
            End If
        Next
        Return False
    End Function
    'Converts a input string (comma separated) into a array
    Private Function ArgmentStrToArray(ByVal input As String) As String()
        Dim ArgCount As Integer = 0
        Dim Args(31) As String
        Dim i As Integer
        Do Until Not input.StartsWith(",")
            If input.Length = 1 Then Return Nothing
            input = Mid(input, 2)
        Loop
        Dim IsInQuote As Boolean = False
        Dim Level As Integer = 0
        Dim Part As String = ""
        Dim selChr As Char
        For i = 0 To input.Length - 1
            selChr = CChar(Mid(input, i + 1, 1))
            If IsInQuote Then
                If selChr = """" Then
                    IsInQuote = False
                End If
                Part &= selChr
            Else
                If selChr = """" Then
                    IsInQuote = True
                    Part &= selChr
                ElseIf selChr = "," And Level = 0 Then
                    Args(ArgCount) = Trim(Part)
                    Part = ""
                    ArgCount = ArgCount + 1
                    If ArgCount = 32 Then Return Nothing 'ERROR
                ElseIf selChr = "(" Then
                    Level = Level + 1
                    Part &= selChr
                ElseIf selChr = ")" Then
                    Level = Level - 1
                    Part &= selChr
                Else
                    Part &= selChr
                End If
            End If
        Next
        If IsInQuote Then Return Nothing 'ERROR
        If Not Part = "" Then
            Args(ArgCount) = Trim(Part)
            ReDim Preserve Args(ArgCount)
        Else
            Return Nothing
        End If
        Return Args
    End Function
    'Converts a function command into its function parts
    Private Function GetFunctionArg(ByVal FncParam As String) As FUNC
        FncParam = Trim(FncParam)
        Dim i As Integer = InStr(FncParam, "(")
        Dim nF As New FUNC
        If i > 0 And FncParam.EndsWith(")") Then
            nF.NAME = Mid(FncParam, 1, i - 1)
            FncParam = Mid(FncParam, i + 1)
            If FncParam = "" Then Return nF 'No more, so were done
            Dim FirstParam As String = ""
            Dim SecParam As String = ""
            If Not StripParam(FncParam, FirstParam) Then Return Nothing 'Error on parse
            If FncParam = "" Then 'Only 1 parameter
                i = InStr(nF.NAME, ".")
                If i > 0 Then
                    nF.SUBF = Mid(nF.NAME, i + 1)
                    nF.NAME = Mid(nF.NAME, 1, i - 1)
                End If
                nF.ARGS = ArgmentStrToArray(FirstParam) 'main params
            Else 'First parameter was a index
                If Not FncParam.StartsWith(".") Then Return Nothing 'Must contain a second command
                FncParam = Mid(FncParam, 2) 'Strip off period
                i = InStr(FncParam, "(")
                If i > 0 And FncParam.EndsWith(")") Then
                    nF.SUBF = Mid(FncParam, 1, i - 1)
                    FncParam = Mid(FncParam, i + 1)
                    If Not StripParam(FncParam, SecParam) Then Return Nothing 'Error on parse
                    If Not FncParam = "" Then Return Nothing 'Parse error (more trash?)
                    nF.ARGS = ArgmentStrToArray(SecParam) 'main params
                    nF.Index = GetIndexFromFunctionName(FirstParam)
                Else 'No additional parameters
                    nF.SUBF = FncParam
                End If
            End If
        Else 'No args
            nF.NAME = FncParam
            i = InStr(nF.NAME, ".")
            If i > 0 Then
                Dim FnName As String = Mid(nF.NAME, 1, i - 1)
                Dim x As Integer = InStr(FnName, "(")
                nF.SUBF = Mid(nF.NAME, i + 1)
                If x > 0 And FnName.EndsWith(")") Then
                    nF.NAME = Mid(FnName, 1, x - 1)
                    FncParam = Mid(FnName, x + 1)
                    FncParam = Mid(FncParam, 1, FncParam.Length - 1) 'Remove the last char which we know is ")"
                    nF.Index = GetIndexFromFunctionName(FncParam)
                Else
                    nF.NAME = FnName
                End If
            End If
        End If
        Return nF
    End Function

    Private Function GetIndexFromFunctionName(ByVal input As String) As Integer
        If IsNumeric(input) Then
            Return CInt(input)
        ElseIf isHex(input) Then
            Return HexToInt(input)
        ElseIf IsVarInteger(input) Then
            Return CInt(input)
        Else
            Return Nothing 'Error (index not reconized)
        End If
    End Function
    'Converts a array of command parts: 1 + 2 into a command obj
    Private Function ConvertPartsToCommands(ByVal CmdParts() As String) As CMD
        Dim Result As CMD = Nothing
        Dim i As Integer
        Dim counter As Integer = 0
        Dim MaxArgs As Integer = CInt(((CmdParts.Length - 1) / 2) + 1)
        ReDim Result.ARGS(MaxArgs - 1)
        ReDim Result.CONNECTS(MaxArgs - 2)
        For i = 0 To CmdParts.Length Step 2
            Result.ARGS(counter) = ConvertArgToSv(CmdParts(i))
            If Not i + 1 = CmdParts.Length Then
                If Not IsConnect(CmdParts(i + 1)) Then Return Nothing
                Result.CONNECTS(counter) = CmdParts(i + 1)
            End If
            counter = counter + 1
        Next
        Return Result
    End Function

    Private Function ConvertArgToSv(ByVal Arg As String, Optional ByVal VarName As Integer = 1) As ScriptVariable
        Dim sv As New ScriptVariable("$" & CStr(VarName))
        Dim Uarg As String = UCase(Arg)
        If IsLocalVariable(Arg) Then
            sv.MyType = DataType.Variable
            sv.Value = CStr(Arg)
        ElseIf IsNumeric(Arg) Then
            sv.MyType = DataType.Int
            sv.Value = CInt(Arg)
        ElseIf isHex(Arg) Then
            If UCase(Arg).StartsWith("0X") Then Arg = Mid(Arg, 3) 'Remove Prefix
            If Arg.Length > 8 Then
                sv.MyType = DataType.Bytes
                sv.Value = HexStringToBytes(Arg)
            Else
                sv.MyType = DataType.Int
                sv.Value = HexToUint(Arg)
            End If
        ElseIf isString(Arg) Then
            sv.MyType = DataType.Str
            sv.Value = Arg
        ElseIf IsFunction(Arg) Then
            sv.MyType = DataType.IntFunc
            sv.Value = GetFunctionArg(Arg)
        ElseIf IsEvent(Arg) Then
            sv.MyType = DataType.IntEvent
            sv.Value = GetFunctionArg(Arg)
        ElseIf IsData(Arg) Then
            sv.MyType = DataType.Bytes
            sv.Value = ConvertDataString(Arg)
        ElseIf Uarg = "TRUE" Then
            sv.MyType = DataType.Bool
            sv.Value = True
        ElseIf Uarg = "FALSE" Then
            sv.MyType = DataType.Bool
            sv.Value = False
        ElseIf Uarg = "NOTHING" Then
            sv.MyType = DataType.Null
        Else 'Must be variable
            sv.MyType = DataType.Variable
            sv.Value = Arg
        End If
        Return sv
    End Function

#End Region

    'Pops up a msgbox to the user
    Private Function msg(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("msgbox", 1, args, New DataType() {DataType.Str}) Then Return Nothing
        Dim MsgOut As String = args(0)
        MsgBox(RemoveStr(ConvertIfVariable(MsgOut)), MsgBoxStyle.Information, "Blackcat Script")
        Return Nothing
    End Function
    'Converts a integer to hex string
    Private Function hexcmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If args Is Nothing Then OnError("Error in Hex: Argument required") : Return Nothing
        Dim r As ScriptVariable = ConvertArgToSv(args(0))
        If r.MyType = DataType.Variable Then r = GetVariable(r.Name)
        Dim hexint As String = ""
        Select Case r.MyType
            Case DataType.Int
                hexint = "0x" & Hex(r.Value)
            Case DataType.Bytes
                hexint = "0x" & BytesToHexString(CType(r.Value, Byte()))
            Case Else
                If r.MyType = DataType.Variable Then OnError("Error in Hex: Integer or Data variable required") : Return Nothing
        End Select
        Dim sv As New ScriptVariable(Nothing, DataType.Str)
        sv.Value = hexint
        Return sv
    End Function
    'Enable for Disable verify flash
    Private Function verify(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If args Is Nothing Then
            Dim ret As New ScriptVariable("Verify", DataType.Bool)
            ret.Value = VerifyData
            Return ret
        ElseIf args.Length = 1 Then
            Dim Res As String = UCase(ConvertIfVariable(args(0)))
            If Res = "TRUE" Then
                VerifyData = True
            ElseIf Res = "FALSE" Then
                VerifyData = False
            Else
                OnError("verify command requires a true or false parameter")
            End If
        End If
        Return Nothing
    End Function
    'writes a message to the console
    Private Function writelinecmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("writeline", 1, args, New DataType() {DataType.Str}) Then Return Nothing
        Dim MsgOut As String = args(0)
        RaiseEvent printf(RemoveStr(ConvertIfVariable(MsgOut)))
        Return Nothing
    End Function


    Private MemType As DeviceTypes = DeviceTypes.NotSpecified
    Private MemAdr32 As UInt32 = 0 'We will allow a default of zero
    Private MemSize As Integer = 0


    'Handles all of the JTAG commands and settings
    Private Function jtagcmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not OperationMode = AvrMode.JTAG Then Return Nothing 'Only work on AVR
        Select Case UCase(subFun)
            Case "" 'ERROR
                OnError("JTAG commands require a sub function")
            Case "MEMORYADDRESS"
                If Not ArgRequire("JTAG.MemoryAddress", 1, args, New DataType() {DataType.Int}) Then Return Nothing
                MemAdr32 = CUInt(args(0))
            Case "MEMORYSIZE"
                If Not ArgRequire("JTAG.MemorySize", 1, args, New DataType() {DataType.Int}) Then Return Nothing
                MemSize = CInt(args(0))
            Case "MEMORYTYPE"
                If Not ArgRequire("JTAG.MemoryType", 1, args, New DataType() {DataType.Str}) Then Return Nothing
                Dim VarStr As String = RemoveStr(ConvertIfVariable(CStr(args(0))))
                Select Case VarStr.ToUpper
                    Case "SPI"
                        MemType = DeviceTypes.SPI
                    Case "CFI"
                        MemType = DeviceTypes.CFI
                    Case "RAM"
                        MemType = DeviceTypes.Dram
                    Case "DRAM"
                        MemType = DeviceTypes.Dram
                    Case Else
                        OnError("Error in Jtag.MemoryType: unknown type specified") : Return Nothing
                End Select
            Case "MEMORYINIT"
                If Not args Is Nothing Then OnError("JTAG.MemoryInit does not need arguments") : Return Nothing
                If MemSize = 0 And MemType = DeviceTypes.Dram Then OnError("Error in Jtag.MemoryInit: device size not specified for DRAM") : Return Nothing
                If MemType = DeviceTypes.NotSpecified Then OnError("Error in Jtag.MemoryInit: device type not specified") : Return Nothing
                Dim FlashIndex As Integer = -1
                If AddMemoryDevice(MemType, MemAdr32, MemSize, FlashIndex) Then
                    MemAdr32 = 0
                    MemSize = 0
                    MemType = DeviceTypes.NotSpecified
                    Dim n As New ScriptVariable(Nothing, DataType.Int)
                    OurFlashDevices.Add(FlashIndex)
                    n.Value = FlashIndex
                    Return n
                End If
            Case "FLASHFIND"
                EJ.FlashFind()
            Case "BIGENDIAN"
                EJ.BigEndian = True
            Case "LITTLEENDIAN"
                EJ.BigEndian = False
            Case "DEBUG"
                If Not ArgRequire("JTAG.Debug", 1, args, New DataType() {DataType.Bool}) Then Return Nothing
                Dim bval As Boolean = CBool(args(0))
                EJ.DebugMode(bval)
            Case "RESET"
                EJ.ProcessReset()
            Case "ENABLEVCC"
                If Not ArgRequire("JTAG.EnableVcc", 1, args, New DataType() {DataType.Bool}) Then Return Nothing
                EnableJtagVcc = CBool(args(0))
            Case "RUNSVF"
                If Not ArgRequire("JTAG.RunSVF", 1, args, New DataType() {DataType.Bytes}) Then Return Nothing
                Dim DataBytes() As Byte = Nothing
                IsVarBytes(args(0), DataBytes) 'Get bytes from var
                HandleFunctionProgressUpdate(0)
                AddHandler EJ.UpdateProgressBar, AddressOf HandleFunctionProgressUpdate
                RaiseEvent printf("Running SVF file in internal JTAG SVF player")
                Dim FileStr() As String = GetStringArray_Bytes(DataBytes)
                Dim result As Boolean = EJ.JSP.RunFile_SVF(FileStr)
                If result Then
                    RaiseEvent printf("SVF file successfully played")
                Else
                    RaiseEvent printf("Error playing the SVF file")
                End If
                Dim ret As New ScriptVariable("SVFRESULT", DataType.Bool)
                ret.Value = result
                HandleFunctionProgressUpdate(0)
                RemoveHandler EJ.UpdateProgressBar, AddressOf HandleFunctionProgressUpdate
                Return ret
            Case "RUNXSVF"
                If Not ArgRequire("JTAG.RunSVF", 1, args, New DataType() {DataType.Bytes}) Then Return Nothing
                Dim DataBytes() As Byte = Nothing
                IsVarBytes(args(0), DataBytes) 'Get bytes from var
                HandleFunctionProgressUpdate(0)
                AddHandler EJ.UpdateProgressBar, AddressOf HandleFunctionProgressUpdate
                RaiseEvent printf("Running XSVF file in internal JTAG SVF player")
                Dim result As Boolean = EJ.JSP.RunFile_XSVF(DataBytes)
                If result Then
                    RaiseEvent printf("XSVF file successfully played")
                Else
                    RaiseEvent printf("Error playing the XSVF file")
                End If
                Dim ret As New ScriptVariable("XSVFRESULT", DataType.Bool)
                ret.Value = result
                HandleFunctionProgressUpdate(0)
                RemoveHandler EJ.UpdateProgressBar, AddressOf HandleFunctionProgressUpdate
                Return ret
            Case "PEEK"
                If Not ArgRequire("JTAG.Peek", 1, args, New DataType() {DataType.Int}) Then Return Nothing
                Dim resu32 As UInt32 = EJ.Memory_Read_W(CUInt(args(0)))
                Dim n As New ScriptVariable("jtagresult", DataType.Int)
                n.Value = resu32
                RaiseEvent printf("Peek at 0x" & Hex(CUInt(args(0))) & ", result: 0x" & Hex(resu32))
                Return n
            Case "POKE"
                If Not ArgRequire("JTAG.Poke", 2, args, New DataType() {DataType.Int, DataType.Int}) Then Return Nothing
                EJ.Memory_Write_W(CUInt(args(0)), CUInt(args(1)))
        End Select
        Return Nothing
    End Function

    Private Sub HandleFunctionProgressUpdate(ByVal percent As Integer)
        If GuiForm IsNot Nothing Then
            If ScriptBar IsNot Nothing Then
                If GuiForm.InvokeRequired Then
                    Dim d As New cbUpdatePbar(AddressOf HandleFunctionProgressUpdate)
                    GuiForm.Invoke(d, New Object() {percent})
                Else
                    ScriptBar.Value = percent
                End If
            End If
        End If
    End Sub
    'sets the avr firmware controlled variables
    Private Function setparam(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not OperationMode = AvrMode.JTAG Then Return Nothing 'Only work on AVR
        If Not ArgRequire("SetParam", 2, args, New DataType() {DataType.Int, DataType.Int}) Then Return Nothing
        Dim MSG As String = "Setting device parameter "
        Select Case CInt(args(0))
            Case 1
                MSG &= "(Intel Flash delay) to 0x" & Hex(args(1))
                EJ.IntelFlashDelay = CInt(Int(args(1)))
            Case 2
                MSG &= "(AMD Flash delay) to 0x" & Hex(args(1))
                EJ.AmdFlashDelay = CInt(Int(args(1)))
            Case 3
                MSG &= "(Memory Read Delay) to 0x" & Hex(args(1))
            Case 4
                MSG &= "(Memory Read Mode) to 0x" & Hex(args(1))
            Case 5
                MSG &= "(Flash base location) to 0x" & Hex(args(1))
        End Select
        RaiseEvent printf(MSG)
        EJ.SetParameter(CUInt(args(0)), CUInt(args(1))) 'Sends the EJTAG set parm command
        Return Nothing
    End Function
    'Sends a JTAG control command
    Private Function ejctrl(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not OperationMode = AvrMode.JTAG Then Return Nothing 'Only work on AVR
        If Not ArgRequire("ejctrl", 1, args, New DataType() {DataType.Int}) Then Return Nothing
        Dim Res As Integer = EJ.EjtagCmd(CUInt(args(0)))
        RaiseEvent printf("EJTAG command issued: 0x" & Hex(args(0)) & " result: 0x" & Hex(Res))
        Dim n As New ScriptVariable("ejtag", DataType.Int)
        n.Value = Res
        Return n
    End Function
    'Pause
    Private Function pausecmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("pause", 1, args, New DataType() {DataType.Int}) Then Return Nothing
        Dim PauseAmount As Integer = CInt(args(0))
        Dim sw As New Stopwatch
        sw.Start()
        Do Until sw.ElapsedMilliseconds >= PauseAmount
            Application.DoEvents() 'We do this as not to lock up the other threads or processes
            Sleep(100)
        Loop
        Return Nothing
    End Function

    Private Function resizecmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        Dim DataLen As Integer = -1
        If args Is Nothing Then OnError("Error in resize: function requires 2 or 3 arguments") : Return Nothing
        If args.Length = 2 Then
            If Not ArgRequire("resize", 2, args, New DataType() {DataType.Bytes, DataType.Int}) Then Return Nothing
        ElseIf args.Length = 3 Then
            If Not ArgRequire("resize", 3, args, New DataType() {DataType.Bytes, DataType.Int, DataType.Int}) Then Return Nothing
            DataLen = CInt(args(2))
        Else
            OnError("Error in resize: function requires 2 or 3 arguments") : Return Nothing
        End If
        Dim DataVar As String = args(0)
        Dim DataStart As String = args(1)
        Dim DataBytes() As Byte = Nothing
        IsVarBytes(DataVar, DataBytes) 'Get bytes from var
        If DataLen = -1 Then DataLen = CInt(DataBytes.Length - CDbl(DataStart))
        Dim Source() As Byte = CType(DataBytes.Clone, Byte())
        ReDim DataBytes(DataLen - 1)
        Dim sourceInd As Integer = CInt(DataStart)
        Try
            Array.Copy(Source, sourceInd, DataBytes, 0, CInt(DataLen))
        Catch ex As Exception
            OnError("Error in resize: the input arguments are not in a valid range") : Return Nothing
        End Try
        Dim sv As New ScriptVariable(DataVar, DataType.Bytes)
        sv.Value = DataBytes
        SetVariable(sv)
        Return Nothing
    End Function
    'Handles tab creation
    Private Function tabcmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If GuiForm Is Nothing Then Return Nothing
        Select Case UCase(subFun)
            Case "" 'ERROR
                OnError("TAB commands require a sub function")
            Case "CREATE"
                If Not ArgRequire("TAB.Create", 1, args, New DataType() {DataType.Str}) Then Return Nothing
                Dim FormName As String = args(0)
                FormName = RemoveStr(ConvertIfVariable(FormName))
                GuiForm.CreateFormTab(UserTabCount, FormName) 'Thread-Safe
                UserTabCount = UserTabCount + 1
                Dim sv As New ScriptVariable(Nothing, DataType.Int)
                sv.Value = UserTabCount - 1
                Return sv
            Case "ADDGROUP"
                If Not ArgRequire("TAB.AddGroup", 5, args, New DataType() {DataType.Str, DataType.Int, DataType.Int, DataType.Int, DataType.Int}) Then Return Nothing
                If (UserTabCount - 1) < Index Then OnError("Error in Tab.Addgroup: tab index does not exist") : Return Nothing
                Dim NewGroup As New GroupBox
                Dim NameStr As String = RemoveStr(ConvertIfVariable(args(0)))
                NewGroup.Name = NameStr
                NewGroup.Text = NameStr
                NewGroup.Left = CInt(args(1))
                NewGroup.Top = CInt(args(2))
                NewGroup.Width = CInt(args(3))
                NewGroup.Height = CInt(args(4))
                GuiForm.AddToTab(Index, NewGroup)
            Case "ADDBOX"
                If Not ArgRequire("TAB.AddBox", 4, args, New DataType() {DataType.Any, DataType.Str, DataType.Int, DataType.Int}) Then Return Nothing
                If (UserTabCount - 1) < Index Then OnError("Error in Tab.AddBox: tab index does not exist") : Return Nothing
                Dim NewTextBox As New TextBox
                NewTextBox.Name = RemoveStr(ConvertIfVariable(args(0)))
                NewTextBox.Text = RemoveStr(ConvertIfVariable(args(1)))
                NewTextBox.Width = (NewTextBox.Text.Length * 6) + 2
                NewTextBox.TextAlign = HorizontalAlignment.Center
                NewTextBox.Left = CInt(args(2))
                NewTextBox.Top = CInt(args(3))
                GuiForm.AddToTab(Index, NewTextBox)
            Case "ADDTEXT"
                If Not ArgRequire("TAB.AddText", 4, args, New DataType() {DataType.Any, DataType.Str, DataType.Int, DataType.Int}) Then Return Nothing
                If (UserTabCount - 1) < Index Then OnError("Error in Tab.AddText: tab index does not exist") : Return Nothing
                Dim NewTextLabel As New Label
                NewTextLabel.Name = RemoveStr(ConvertIfVariable(args(0)))
                NewTextLabel.AutoSize = True
                NewTextLabel.Text = RemoveStr(ConvertIfVariable(args(1)))
                NewTextLabel.Width = (NewTextLabel.Text.Length * 7)
                NewTextLabel.Left = CInt(args(2))
                NewTextLabel.Top = CInt(args(3))
                NewTextLabel.BringToFront()
                GuiForm.AddToTab(Index, NewTextLabel)
            Case "ADDIMAGE"
                If Not ArgRequire("TAB.AddImage", 4, args, New DataType() {DataType.Any, DataType.Str, DataType.Int, DataType.Int}) Then Return Nothing
                If (UserTabCount - 1) < Index Then OnError("Error in Tab.AddImage: tab index does not exist") : Return Nothing
                Dim filen As String = RemoveStr(ConvertIfVariable(args(1)))
                Dim finfo As New IO.FileInfo(ScriptPath & filen)
                If Not finfo.Exists Then RaiseEvent printf("Tab.AddImage, specified image not found: " & filen) : Return Nothing
                Dim newImage As Image = Image.FromFile(finfo.FullName)
                Dim NewPB As New PictureBox
                NewPB.Name = RemoveStr(ConvertIfVariable(args(0)))
                NewPB.Image = newImage
                NewPB.Left = CInt(args(2))
                NewPB.Top = CInt(args(3))
                NewPB.Width = newImage.Width + 5
                NewPB.Height = newImage.Height + 5
                NewPB.BringToFront() 'does not work
                GuiForm.AddToTab(Index, NewPB)
            Case "ADDBUTTON"
                If Not ArgRequire("TAB.AddButton", 4, args, New DataType() {DataType.Any, DataType.Str, DataType.Int, DataType.Int}) Then Return Nothing
                If (UserTabCount - 1) < Index Then OnError("Error in Tab.AddButton: tab index does not exist") : Return Nothing
                Dim NewButton As New Button
                NewButton.AutoSize = True
                NewButton.Name = RemoveStr(ConvertIfVariable(args(0)))
                NewButton.Text = RemoveStr(ConvertIfVariable(args(1)))
                AddHandler NewButton.Click, AddressOf ButtonHandler
                NewButton.Left = CInt(args(2))
                NewButton.Top = CInt(args(3))
                NewButton.BringToFront() 'does not work
                GuiForm.AddToTab(Index, NewButton)
            Case "ADDPROGRESS"
                If Not ArgRequire("TAB.AddProgress", 3, args, New DataType() {DataType.Int, DataType.Int, DataType.Int}) Then Return Nothing
                If (UserTabCount - 1) < Index Then OnError("Error in Tab.AddProgress: tab index does not exist") : Return Nothing
                If GuiForm.Controls.Contains(ScriptBar) Then GuiForm.Controls.Remove(ScriptBar)
                ScriptBar = New ProgressBar
                ScriptBar.Name = "ScriptProgressBar"
                ScriptBar.Left = CInt(args(0))
                ScriptBar.Top = CInt(args(1))
                ScriptBar.Width = CInt(args(2))
                ScriptBar.Height = 12
                GuiForm.AddToTab(Index, ScriptBar)
            Case "REMOVE"
                If args Is Nothing Then OnError("TAB.Remove requires one or more arguments") : Return Nothing
                If (UserTabCount - 1) < Index Then OnError("Error in Tab.Remove: tab index does not exist") : Return Nothing
                Dim item As String
                For Each item In args
                    Dim ItemToRemove As String = RemoveStr(ConvertIfVariable(item))
                    If Not IsUserControl(ItemToRemove) Then
                        OnError("Error in Tab.Remove: object is not a tab element") : Return Nothing
                    End If
                    RemoveUserControl(ItemToRemove)
                Next
            Case "SETTEXT"
                If Not ArgRequire("TAB.SetText", 2, args, New DataType() {DataType.Any, DataType.Str}) Then Return Nothing
                If (UserTabCount - 1) < Index Then OnError("Error in Tab.SetText: tab index does not exist") : Return Nothing
                GuiForm.SetControlText(Index, RemoveStr(ConvertIfVariable(args(0))), RemoveStr(args(1)))
            Case "BUTTONENABLE"
                If args Is Nothing Then
                    GuiForm.HandleButtons(Index, True, "") 'Enable all buttons
                ElseIf args.Length = 1 Then
                    If Not ArgRequire("TAB.ButtonEnable", 1, args, New DataType() {DataType.Any}) Then Return Nothing
                    GuiForm.HandleButtons(Index, True, RemoveStr(ConvertIfVariable(args(0))))
                Else
                    OnError("Error in TAB.ButtonEnable: too many argments") : Return Nothing
                End If
            Case "BUTTONDISABLE"
                If args Is Nothing Then
                    GuiForm.HandleButtons(Index, False, "") 'Disable all buttons
                ElseIf args.Length = 1 Then
                    If Not ArgRequire("TAB.ButtonDisable", 1, args, New DataType() {DataType.Any}) Then Return Nothing
                    GuiForm.HandleButtons(Index, False, RemoveStr(ConvertIfVariable(args(0))))
                Else
                    OnError("Error in TAB.ButtonDisable: too many argments") : Return Nothing
                End If
        End Select
        Return Nothing
    End Function
    'Returns a string of the current mode ie ("JTAG") or ("SPI")
    Private Function mode(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("mode", 0, args, Nothing) Then Return Nothing
        Dim sv As New ScriptVariable(Nothing, DataType.Str)
        Select Case OperationMode
            Case AvrMode.JTAG
                sv.Value = """JTAG"""
            Case AvrMode.SPI
                sv.Value = """SPI"""
            Case Else
                Return Nothing
        End Select
        Return sv
    End Function
    'LEN command
    Private Function lencmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("len", 1, args, New DataType() {DataType.Any}) Then Return Nothing
        Dim sv As New ScriptVariable(Nothing, DataType.Int)
        Dim input As String = args(0)
        Dim t As ScriptVariable
        If IsVariable(input) Then
            t = GetVariable(input)
        Else
            t = ConvertArgToSv(input)
            If t.MyType = DataType.Variable Then
                OnError("Error in len: the input argument is not supported") : Return Nothing
            End If
        End If
        Select Case t.MyType
            Case DataType.Bytes
                Dim b() As Byte = CType(t.Value, Byte())
                sv.Value = b.Length
            Case DataType.Str
                sv.Value = RemoveStr(CStr(t.Value)).Length
            Case DataType.Int
                sv.Value = Math.Ceiling(Hex(t.Value).Length / 2)
            Case Else
                OnError("Error in len: the input argument is not supported") : Return Nothing
        End Select
        Return sv
    End Function

    Private Function openfilecmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If args Is Nothing Then
            OnError("Error in OpenFile: argument required") : Return Nothing
        ElseIf args.Length = 1 Or args.Length = 2 Then
        Else
            OnError("Error in OpenFile: one or two arguments allowed only") : Return Nothing
        End If
        Dim Prompt As String = args(0)
        Dim Filter As String = """All files (*.*)|*.*"""
        If args.Length = 2 Then Filter = args(1)
        Prompt = ConvertIfVariable(Prompt)
        Filter = ConvertIfVariable(Filter)
        If Not isString(Prompt) Then OnError("Error in OpenFile: first argument is not a string") : Return Nothing
        If Not isString(Filter) Then OnError("Error in OpenFile: second argument is not a string") : Return Nothing
        Dim sv As New ScriptVariable(Nothing, DataType.Bytes)
        Dim fDiag As New OpenFileDialog
        fDiag.CheckFileExists = True
        fDiag.Title = RemoveStr(Prompt)
        fDiag.Filter = RemoveStr(Filter)
        fDiag.InitialDirectory = Application.StartupPath
        If fDiag.ShowDialog = DialogResult.OK Then
            sv.Value = ReadBytes(fDiag.FileName)
        Else
            sv.Value = Nothing
        End If
        Return sv
    End Function

    Private Function HWordCmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("HWord", 2, args, New DataType() {DataType.Bytes, DataType.Int}) Then Return Nothing
        Dim sv As New ScriptVariable(Nothing, DataType.Int)
        Dim datavar As String = args(0)
        Dim dataoffset As String = args(1)
        Dim data() As Byte = Nothing
        IsVarBytes(datavar, data) 'Get bytes from var
        If Not IsNumeric(dataoffset) Then IsVarInteger(dataoffset)
        If (CInt(dataoffset) + 1) >= data.Length Then OnError("Error in HWord: The Data size is less than starting value") : Return Nothing
        sv.Value = (data(CInt(dataoffset)) * 256) + data(CInt(dataoffset) + 1)
        Return sv
    End Function

    Private Function WordCmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("Word", 2, args, New DataType() {DataType.Bytes, DataType.Int}) Then Return Nothing
        Dim sv As New ScriptVariable(Nothing, DataType.Int)
        Dim datavar As String = args(0)
        Dim dataoffset As String = args(1)
        Dim data() As Byte = Nothing
        IsVarBytes(datavar, data) 'Get bytes from var
        If Not IsNumeric(dataoffset) Then IsVarInteger(dataoffset)
        If (CInt(dataoffset) + 3) >= data.Length Then OnError("Error in Word: The Data size is less than starting value") : Return Nothing
        sv.Value = WordToUint32(data(CInt(dataoffset)), data(CInt(dataoffset) + 1), data(CInt(dataoffset) + 2), data(CInt(dataoffset) + 3))
        Return sv
    End Function

    Private Function ByteCmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("Byte", 2, args, New DataType() {DataType.Bytes, DataType.Int}) Then Return Nothing
        Dim sv As New ScriptVariable(Nothing, DataType.Int)
        Dim datavar As String = args(0)
        Dim dataoffset As String = args(1)
        Dim data() As Byte = Nothing
        IsVarBytes(datavar, data) 'Get bytes from var
        If Not IsNumeric(dataoffset) Then IsVarInteger(dataoffset)
        If (CInt(dataoffset) + 1) >= data.Length Then OnError("Error in Byte: The Data size is less than starting value") : Return Nothing
        sv.Value = data(CInt(dataoffset))
        Return sv
    End Function

    Private Function askcmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("ask", 1, args, New DataType() {DataType.Str}) Then Return Nothing
        Dim sv As New ScriptVariable(Nothing, DataType.Bool)
        Dim AskQuestion As String = RemoveStr(ConvertIfVariable(args(0)))
        If MsgBox(AskQuestion, MsgBoxStyle.YesNo, "FlashcatUSB") = MsgBoxResult.Yes Then
            sv.Value = True
        Else
            sv.Value = False
        End If
        Return sv
    End Function
    'Sets the main forms status text
    Private Function statuscmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("status", 1, args, New DataType() {DataType.Str}) Then Return Nothing
        Dim statusmsg As String = RemoveStr(ConvertIfVariable(args(0)))
        If GuiForm IsNot Nothing Then GuiForm.SetStatus(statusmsg)
        Application.DoEvents()
        Return Nothing
    End Function

    Private Function savefilecmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If args Is Nothing Then OnError("Error in SaveFile: Arguments required") : Return Nothing
        Dim DefaultTitle As String = ""
        If args.Length = 2 Then
            If Not ArgRequire("SaveFile", 2, args, New DataType() {DataType.Bytes, DataType.Str}) Then Return Nothing
        Else
            If Not ArgRequire("SaveFile", 3, args, New DataType() {DataType.Bytes, DataType.Str, DataType.Str}) Then Return Nothing
            DefaultTitle = RemoveStr(ConvertIfVariable(args(2)))
        End If
        Dim DataVar As String = args(0)
        Dim Title As String = RemoveStr(ConvertIfVariable(args(1)))
        Dim sv As ScriptVariable = GetVariable(DataVar)
        Dim Data() As Byte = CType(sv.Value, Byte())
        Dim fDiag As New SaveFileDialog
        fDiag.Filter = "All files (*.*)|*.*"
        fDiag.Title = Title
        fDiag.FileName = DefaultTitle
        fDiag.InitialDirectory = Application.StartupPath
        If fDiag.ShowDialog = DialogResult.OK Then
            WriteBytes(Data, fDiag.FileName)
            RaiseEvent printf("Data saved. " & Data.Length & " bytes written")
        Else
            RaiseEvent printf("User canceled operation to save data")
        End If
        Return Nothing
    End Function

    Private Function valuecmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If GuiForm Is Nothing Then Return Nothing
        If args Is Nothing Then OnError("") : Return Nothing
        Dim TabIndex As Integer = 0
        If args.Length = 1 Then
            If Not ArgRequire("value", 1, args, New DataType() {DataType.Str}) Then Return Nothing
        Else
            If Not ArgRequire("value", 2, args, New DataType() {DataType.Str, DataType.Int}) Then Return Nothing
            TabIndex = CInt(ConvertIfVariable(args(1)))
        End If
        Dim sv As New ScriptVariable(Nothing, DataType.Str)
        sv.Value = GuiForm.GetTabObjectText(RemoveStr(ConvertIfVariable(args(0))), TabIndex)
        Return sv
    End Function

    Private Function legacycmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        Dim sv As New ScriptVariable(Nothing)
        'SetFlashIndex
        Select Case UCase(subFun)
            Case "SETFLASH"
                If Not ArgRequire("Legacy.SetFlash", 1, args, New DataType() {DataType.Int}) Then Return Nothing
                Dim FlashInd As Integer = CInt(ConvertIfVariable(args(0)))
                LegacyNonVol.SetFlashIndex(FlashInd)
            Case "GETFWLOCATION"
                sv.Value = LegacyNonVol.GetFirmwareStart : Return sv
                Return sv
            Case "GETFWNAME"
                sv.Value = LegacyNonVol.GetFirmwareName() : Return sv
            Case "READHFCMAC"
                Dim c As New ScriptVariable(Nothing, DataType.Str)
                c.Value = LegacyNonVol.MacAddress
                Return c
            Case "SETHFCMAC"
                If Not ArgRequire("Legacy.SetHfcMac", 1, args, New DataType() {DataType.Str}) Then Return Nothing
                Dim MacAddress As String = RemoveStr(ConvertIfVariable(args(0)).Replace(":", ""))
                If Not MacAddress.Length = 12 Or Not isHex(MacAddress) Then
                    RaiseEvent printf("Error in SetHFCMac: New MAC address is not in the correct format")
                    If GuiForm IsNot Nothing Then GuiForm.SetStatus("Invalid MAC address format: XX:XX:XX:XX:XX:XX")
                    Return Nothing
                End If
                LegacyNonVol.MacAddress = MacAddress
            Case "READSERIAL"
                Dim c As New ScriptVariable(Nothing, DataType.Str)
                c.Value = LegacyNonVol.Serial
                Return c
            Case "READCONFIG"
                sv.Value = LegacyNonVol.ReadConfig()
                Return sv
            Case "WRITECONFIG"
                LegacyNonVol.WriteConfig()
            Case "SETSERIAL"
                If Not ArgRequire("Legacy.SetSerial", 1, args, New DataType() {DataType.Str}) Then Return Nothing
                Dim SerStr As String = ConvertIfVariable(args(0))
                LegacyNonVol.Serial = RemoveStr(SerStr)
            Case "GETFWLEN"
                sv.Value = LegacyNonVol.GetFirmwareLen : Return sv
            Case ""
                OnError("Legacy commands require a sub function")
            Case Else
                OnError("Legacy command not understood: " & subFun)
        End Select
        Return Nothing
    End Function

    Private Function ToInteger(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("ToInteger", 1, args, New DataType() {DataType.Str}) Then Return Nothing
        Dim sv As New ScriptVariable(Nothing, DataType.Int)
        sv.Value = CInt(RemoveStr(ConvertIfVariable(args(0))))
        Return sv
    End Function

    Private Function CopyCmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("Copy", 2, args, New DataType() {DataType.Bytes, DataType.Bytes}) Then Return Nothing
        Dim var1() As Byte = CType(GetVariable(args(0)).Value, Byte())
        Dim var2() As Byte = CType(GetVariable(args(1)).Value, Byte())
        Dim NewSize As Integer = var1.Length + var2.Length
        Dim Out(NewSize - 1) As Byte
        Array.Copy(var1, 0, Out, 0, var1.Length)
        Array.Copy(var2, 0, Out, var1.Length, var2.Length)
        Dim sv As New ScriptVariable(Nothing, DataType.Bytes)
        sv.Value = Out
        Return sv
    End Function
    'New command in build 260
    Private Function SpiCmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        Select Case UCase(subFun)
            Case "" 'ERROR
                OnError("SPI commands require a sub function")
            Case "FOSC"
                If Not ArgRequire("SPI.FOSC", 1, args, New DataType() {DataType.Int}) Then Return Nothing
                Dim ClockInt As Integer = CInt(ConvertIfVariable(args(0)))
                SPI.SpecifyClock(ClockInt)
            Case "ORDER"
                If Not ArgRequire("SPI.Order", 1, args, New DataType() {DataType.Str}) Then Return Nothing
                Dim OrderStr As String = ConvertIfVariable(args(0))
                SPI.SpecifyOrder(OrderStr)
            Case "MODE"
                If Not ArgRequire("SPI.Mode", 1, args, New DataType() {DataType.Int}) Then Return Nothing
                Dim ModeInt As Integer = CInt(ConvertIfVariable(args(0)))
                SPI.SpecifyMode(ModeInt)
            Case "SWAP"
                If Not ArgRequire("SPI.Swap", 1, args, New DataType() {DataType.Bool}) Then Return Nothing
                Dim DoSwap As Boolean = CBool(ConvertIfVariable(args(0)))
                SPI.ReverseEndian = DoSwap
                If DoSwap Then
                    WriteConsole("SPI data endian is being swapped")
                Else
                    WriteConsole("SPI data endian swap is now off")
                End If
        End Select
        Return Nothing
    End Function

    Private Function MemCmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        Select Case UCase(subFun)
            Case "" 'ERROR
                OnError("Memory commands require a sub function")
            Case "WRITEWORD"
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.Write: device not connected") : Return Nothing
                If args Is Nothing Then OnError("Error in Memory.Write: arguments required") : Return Nothing
                If Not ArgRequire("Memory.WriteWord", 2, args, New DataType() {DataType.Int, DataType.Int}) Then Return Nothing
                Dim Addr32 As UInt32 = CUInt(args(0))
                Dim Val32 As UInt32 = CUInt(args(1))
                EJ.Memory_Write_W(Addr32, Val32)
                RaiseEvent printf("Wrote 0x" & Hex(Val32) & " to memory address: 0x" & Hex(Addr32) & " over JTAG")
            Case "WRITE"
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.Write: device not connected") : Return Nothing
                If args Is Nothing Then OnError("Error in Memory.Write: arguments required") : Return Nothing
                Dim LenVal As Integer = -1
                If args.Length = 2 Then
                    If Not ArgRequire("Memory.Write", 2, args, New DataType() {DataType.Bytes, DataType.Int}) Then Return Nothing
                Else
                    If Not ArgRequire("Memory.Write", 3, args, New DataType() {DataType.Bytes, DataType.Int, DataType.Int}) Then Return Nothing
                    LenVal = CInt(ConvertIfVariable(args(2)))
                End If
                Dim DataVar As String = args(0)
                Dim OFFSET As Integer = CInt(ConvertIfVariable(args(1)))
                Dim sv As ScriptVariable = GetVariable(DataVar)
                Dim Data() As Byte = CType(sv.Value, Byte())
                If LenVal = -1 Then LenVal = Data.Length
                bcLedBlink()
                ReDim Preserve Data(LenVal - 1)
                If bcFlashWrite(CUInt(OFFSET), Data, Index, False) Then
                    RaiseEvent printf("Sucessfully programmed " & LenVal & " bytes")
                Else
                    RaiseEvent printf("Canceled memory write operation")
                End If
                bcLedOn()
                'UpdateProgress(0)
                Return Nothing
            Case "READ"
                Dim Silent As Boolean = True
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.Read: device not connected") : Return Nothing
                If args Is Nothing Then OnError("Error in Memory.Read: arguments required") : Return Nothing
                If args.Length = 2 Then
                    If Not ArgRequire("Memory.Read", 2, args, New DataType() {DataType.Int, DataType.Int}) Then Return Nothing
                ElseIf args.Length = 3 Then
                    If Not ArgRequire("Memory.Read", 3, args, New DataType() {DataType.Int, DataType.Int, DataType.Bool}) Then Return Nothing
                    Silent = CBool(args(2))
                Else
                    OnError("Error in Memory.Read: wrong arguments supplied") : Return Nothing
                End If
                Dim sv As New ScriptVariable(Nothing, DataType.Bytes)
                Dim StartVar As Integer = CInt(ConvertIfVariable(args(0)))
                Dim LenVar As Integer = CInt(ConvertIfVariable(args(1)))
                sv.Value = bcReadMemory(CUInt(StartVar), LenVar, Index, Silent)
                'UpdateProgress(0)
                Return sv
            Case "READSTRING"
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.ReadString: device not connected") : Return Nothing
                If Not ArgRequire("Memory.ReadString", 1, args, New DataType() {DataType.Int}) Then Return Nothing
                Dim FlashOffset As Integer = CInt(ConvertIfVariable(args(0)))
                Dim FlashSize As Integer = bcGetFlashSize(Index)
                If FlashOffset + 1 > FlashSize Then OnError("Error in Memory.ReadString: offset is greater than flash size") : Return Nothing
                Dim sv As New ScriptVariable(Nothing, DataType.Str)
                Dim i As Integer
                Dim strBuilder As String = ""
                Dim b As Byte
                For i = FlashOffset To FlashSize - 1
                    b = bcReadFlashByte(CUInt(i), Index)
                    If b > 31 And b < 127 Then
                        strBuilder &= Chr(b)
                    ElseIf b = 0 Then
                        Exit For
                    Else
                        Return Nothing 'Error
                    End If
                Next
                If strBuilder = "" Then Return Nothing
                sv.Value = """" & strBuilder & """"
                Return sv
            Case "READVERIFY"
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.ReadVerify: device not connected") : Return Nothing
                If Not ArgRequire("Memory.ReadVerify", 2, args, New DataType() {DataType.Int, DataType.Int}) Then Return Nothing
                Dim sv As New ScriptVariable(Nothing, DataType.Bytes)
                Dim FlashAddress As UInteger = CUInt(ConvertIfVariable(args(0)))
                Dim FlashLen As Integer = CInt(ConvertIfVariable(args(1)))
                Dim data() As Byte = ReadMemoryVerify(FlashAddress, FlashLen, Index)
                If data Is Nothing Then
                    RaiseEvent printf("Error: Data read failed integrity check")
                    sv.MyType = DataType.Null
                    Return sv
                End If
                sv.Value = data
                Return sv
            Case "GETSECTORCOUNT"
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.GetSectorCount: device not connected") : Return Nothing
                Dim sv As New ScriptVariable(Nothing, DataType.Int)
                sv.Value = bcFlashSectorCount(Index)
                Return sv
            Case "ERASESECTOR"
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.EraseSector: device not connected") : Return Nothing
                If Not ArgRequire("Memory.EraseSector", 1, args, New DataType() {DataType.Int}) Then Return Nothing
                Dim Sect As Integer = CInt(ConvertIfVariable(args(0)))
                bcFlashEraseSector(Sect, Index)
                bcReadMode(Index)
            Case "ERASESECTION"
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.EraseSection: device not connected") : Return Nothing
                If Not ArgRequire("Memory.EraseSection", 2, args, New DataType() {DataType.Int, DataType.Int}) Then Return Nothing
                Dim StartAdr As Integer = CInt(ConvertIfVariable(args(0)))
                Dim ByteCount As Integer = CInt(ConvertIfVariable(args(1)))
                If bcFlashEraseSection(StartAdr, ByteCount, Index) Then
                    RaiseEvent printf("Sucessfully erased section of the memory device")
                Else
                    RaiseEvent printf("Error: unable to erase memory section")
                End If
                bcReadMode(Index)
            Case "ERASEBULK"
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.EraseBulk: device not connected") : Return Nothing
                bcFlashEraseBulk(Index)
            Case "UPDATE"
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.Update: device not connected") : Return Nothing
                bcMemoryUpdate(Index)
            Case "GETSECTORSIZE"
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.GetSectorSize: device not connected") : Return Nothing
                If Not ArgRequire("Memory.GetSectorSize", 1, args, New DataType() {DataType.Int}) Then Return Nothing
                Dim sv As New ScriptVariable(Nothing, DataType.Int)
                Dim Sect As Integer = CInt(ConvertIfVariable(args(0)))
                sv.Value = bcFlashSectorSize(Sect, Index)
                Return sv
            Case "BACKUP"
                If Not HasMemoryAttached(Index) Then OnError("Error in Memory.Backup: device not connected") : Return Nothing
                RaiseEvent printf("Backing up entire memory data")
                Dim fsize As Integer = bcGetFlashSize(Index)
                RaiseEvent printf("Reading memory data (" & CStr(fsize) & " bytes)")
                Dim data() As Byte = ReadMemoryVerify(0, fsize, Index)
                If data Is Nothing Then OnError("Error in Memory.Backup: ReadMemory returned nothing") : Return Nothing
                RaiseEvent printf("Memory read and verified (data integrity is assured)")
                SaveFileFromRead(data, "Backup.bin")
            Case "EXIST"
                Dim sv As New ScriptVariable(Nothing, DataType.Bool)
                sv.Value = CBool(HasMemoryAttached(Index))
                Return sv
            Case Else
                OnError("Memory command not found: " & subFun)
        End Select
        Return Nothing
    End Function

    Private Function bytescmd(ByVal args() As String, ByVal Index As Integer, ByVal subFun As String) As ScriptVariable
        If Not ArgRequire("Bytes", 3, args, New DataType() {DataType.Bytes, DataType.Int, DataType.Int}) Then Return Nothing
        Dim data() As Byte = CType(GetVariable(args(0)).Value, Byte())
        Dim StartInt As Integer = CInt(ConvertIfVariable(args(1)))
        Dim StartLen As Integer = CInt(ConvertIfVariable(args(2)))
        Dim sv As New ScriptVariable(Nothing, DataType.Bytes)
        If (StartInt + StartLen + 1) > data.Length Then
            OnError("Error in Bytes: data requested out of bounds") : Return Nothing
        End If
        Dim newdata(StartLen - 1) As Byte
        Array.Copy(data, StartInt, newdata, 0, newdata.Length)
        sv.Value = newdata
        Return sv
    End Function

End Class
