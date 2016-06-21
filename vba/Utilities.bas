Attribute VB_Name = "Utilities"
Option Explicit
Option Compare Text
Option Private Module

'Global AOptions As New OptionsForm

Public theAppEvents As New EventCatcher
Private LSActive As Boolean
Public bRulesInitialized As Boolean
Public Probing As Boolean
Public iCRuleSets As CRuleSets
Public RulesWorkbook As String

Private Const AMZI_ARULES_KEY = "Software\Amzi\ARulesXL\2.0"

'Private Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Long
Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)

' Stuff to allow calls to shell
Private Type SHELLEXECUTEINFO
   cbSize As Long
   fMask As Long
   hwnd As Long
   lpVerb As String
   lpFile As String
   lpParameters As String
   lpDirectory As String
   nShow As Long
   hInstApp As Long
   lpIDList As Long
   lpClass As String
   hkeyClass As Long
   dwHotKey As Long
   hIcon As Long
   hProcess As Long
End Type

Type OPENFILENAME
   lStructSize As Long
   hwndOwner As Long
   hInstance As Long
   lpstrFilter As String
   lpstrCustomFilter As String
   nMaxCustFilter As Long
   nFilterIndex As Long
   lpstrFile As String
   nMaxFile As Long
   lpstrFileTitle As String
   nMaxFileTitle As Long
   lpstrInitialDir As String
   lpstrTitle As String
   flags As Long
   nFileOffset As Integer
   nFileExtension As Integer
   lpstrDefExt As String
   lCustData As Long
   lpfnHook As Long
   lpTemplateName As String
End Type

Private Const SEE_MASK_NOCLOSEPROCESS = &H40

Private Declare Function ShellExecuteEX Lib "shell32.dll" _
   Alias "ShellExecuteEx" (SEI As SHELLEXECUTEINFO) As Long

Declare Function GetSaveFileName Lib "comdlg32.dll" _
   Alias "GetSaveFileNameA" (pOpenfilename As OPENFILENAME) As Long

' Declarations for registry access, used by help for example
Declare Function RegOpenKeyEx Lib "advapi32.dll" Alias _
   "RegOpenKeyExA" (ByVal hKey As Long, ByVal lpSubKey As String, _
   ByVal ulOptions As Long, ByVal samDesired As Long, phkResult As _
   Long) As Long
Declare Function RegQueryValueExString Lib "advapi32.dll" Alias _
   "RegQueryValueExA" (ByVal hKey As Long, ByVal lpValueName As _
   String, ByVal lpReserved As Long, lpType As Long, ByVal lpData _
   As String, lpcbData As Long) As Long
Declare Function RegQueryValueExLong Lib "advapi32.dll" Alias _
   "RegQueryValueExA" (ByVal hKey As Long, ByVal lpValueName As _
   String, ByVal lpReserved As Long, lpType As Long, lpData As _
   Long, lpcbData As Long) As Long
Declare Function RegQueryValueExNULL Lib "advapi32.dll" Alias _
   "RegQueryValueExA" (ByVal hKey As Long, ByVal lpValueName As _
   String, ByVal lpReserved As Long, lpType As Long, ByVal lpData _
   As Long, lpcbData As Long) As Long
Declare Function RegSetValueExLong Lib "advapi32" Alias _
   "RegSetValueExA" (ByVal hKey As Long, ByVal lpValueName As _
   String, ByVal Reserved As Long, ByVal dwType As Long, szData As _
   Long, ByVal cbData As Long) As Long

   Public Const REG_SZ As Long = 1
   Public Const REG_DWORD As Long = 4


   Public Const HKEY_CLASSES_ROOT = &H80000000
   Public Const HKEY_CURRENT_USER = &H80000001
   Public Const HKEY_LOCAL_MACHINE = &H80000002
   Public Const HKEY_USERS = &H80000003


   Public Const ERROR_NONE = 0
   Public Const ERROR_BADDB = 1
   Public Const ERROR_BADKEY = 2
   Public Const ERROR_CANTOPEN = 3
   Public Const ERROR_CANTREAD = 4
   Public Const ERROR_CANTWRITE = 5
   Public Const ERROR_OUTOFMEMORY = 6
   Public Const ERROR_ARENA_TRASHED = 7
   Public Const ERROR_ACCESS_DENIED = 8
   Public Const ERROR_INVALID_PARAMETERS = 87
   Public Const ERROR_NO_MORE_ITEMS = 259


   Public Const KEY_QUERY_VALUE = &H1
   Public Const KEY_SET_VALUE = &H2
   Public Const KEY_ALL_ACCESS = &H3F


   Public Const REG_OPTION_NON_VOLATILE = 0



'Declare Function arxlBeep Lib "arxlthread.dll" (ByVal Frequency As Long, ByVal Duration As Long) As Long
'Declare Function arxlQuitBeep Lib "arxlthread.dll" () As Long
'Declare Function arxlHigherBeep Lib "arxlthread.dll" () As Long
'Declare Function arxlLowerBeep Lib "arxlthread.dll" () As Long
'Declare Function arxlBeepMessage Lib "arxlthread.dll" (ByVal Message As String) As Long

'Public Function beepTest(Frequency As Integer, Duration As Integer) As Long
''    MsgBox "beeping for " & Duration
'    beepTest = arxlBeep(Frequency, Duration)
''    beepTest = 4
'End Function

Public Sub VBShellExecute(FPath As String)

'    On Error GoTo catch

   Dim SEI As SHELLEXECUTEINFO
   With SEI
     .cbSize = Len(SEI)
     .fMask = SEE_MASK_NOCLOSEPROCESS
     .hwnd = 0
     .lpVerb = "open"
     .lpFile = FPath
     .lpParameters = vbNullChar
     .lpDirectory = vbNullChar
     .nShow = 0
     .hInstApp = 0
     .lpIDList = 0
   End With
   Call ShellExecuteEX(SEI)

    Exit Sub
'catch:
'    DealWithException ("VBShellExecute")
End Sub

Public Function VBGetSaveFileName(ByVal InitFile As String) As String
   Dim OFN As OPENFILENAME
   
'   On Error GoTo catch
   
   With OFN
     .lStructSize = Len(OFN)
     .hwndOwner = 0
     .hInstance = 0
     .lpstrTitle = "Save File As..." & vbNullChar
     .lpstrInitialDir = ActiveWorkbook.path & vbNullChar '"C:\" & vbNullChar
     .lpstrFilter = "HTML (*.htm)" & vbNullChar & "*.htm" _
       & vbNullChar & "All Files" & vbNullChar & "*.*" & vbNullChar
     .nFilterIndex = 1
     .lpstrDefExt = ".htm" & vbNullChar
     .lpstrFile = InitFile & Space$(1024) & vbNullChar
     .nMaxFile = Len(.lpstrFile)
     .lpstrFileTitle = Space$(1024) & vbNullChar
     .nMaxFileTitle = Len(.lpstrFileTitle)
   End With
   Dim returnval As Long
   returnval = GetSaveFileName(OFN)
   If returnval = 0 Then
     VBGetSaveFileName = ""
   Else
     VBGetSaveFileName = OFN.lpstrFile
   End If
    Exit Function
'catch:
'    VBGetSaveFileName = DealWithException("VBSGetSaveFileName")
End Function

Public Sub ATest()
    Dim x As Integer
    Dim r As Range
    
    Set r = Range("TaxTable")
    'ATest = "OK"
'    X = Application.WorksheetFunction.VLookup(80, r, 2)
    x = Evaluate("VLookup(80, TaxTable, 2)")
'    X = Application.Run("ARulesXL.xla!Application.WorksheetFunction.VLookup", 80, r, 2)
    MsgBox "OK: " & x
End Sub

' A catch all function for all those errors we didn't know existed
Public Function DealWithException(note As String) As String
    Dim msg As String
    Dim c As Range
    Dim cmsg As String
    Static suppressMsgBox As Boolean   ' starts as false I guess
    Dim response As Integer
    
'    msg = Err.Number & ": " & note & ": Contact Amzi!" & Chr(10) & _
'        Err.Description & Chr(10) & _
'        "Cancel to suppress these errors"
    msg = CStr(Err.Number) & "(" & note & "): " & Err.Description
    Err.Clear
    
    On Error Resume Next  'might not be a this cell
    Set c = Application.ThisCell
    If Err.Number = 0 Then
        cmsg = c.Worksheet.name & "!" & c.Address(False, False) & ": "
    Else
        cmsg = ""
    End If

    msg = cmsg & msg
'    If Not suppressMsgBox Then
'        response = MsgBox(msg, vbCritical + vbOKCancel, "ARulesXL Internal Error")
'    Else
'        response = vbOK
'    End If
    DealWithException = msg
'    If response = vbCancel Then suppressMsgBox = True
    
End Function
Public Sub ThrowError(src As String, msg As String)
Dim desc As String
    On Error Resume Next
    desc = GetText(msg)
    If Error <> 0 Then
        desc = msg
    End If
    Err.Raise Number:=10000, Source:=src, Description:=desc
End Sub

Public Function ARulesActive() As Boolean
    ARulesActive = LSActive
End Function
Public Sub SetARulesActive(val As Boolean)
    'MsgBox "Setting LSActive to " & val
    LSActive = val
End Sub
' Public app As Application
' Attribute VB_Name = "PMsgBoxModule"
' This is the extended predicate which can be called from Prolog
' It is passed the same EngineID argument which was passed to AddPredLS
'
Public Function PMsgBox(ByVal engineid As Long) As Long
Dim rc As Integer, tstr As String

    On Error GoTo catch
    
    tstr = Space$(255)
    rc = lsGetParm(engineid, 1, bSTR, ByVal tstr)
    MsgBox (RTrim$(tstr))
    PMsgBox = 1
    
    Exit Function
catch:
    Dim msg As String
    msg = "VBA Error: " & Err.Number & "(PMsgBox)" & Err.Description
    Err.Clear
    ErrRaiseLS msg
End Function
Public Function PMsgBox4(ByVal engineid As Long) As Long
    Dim rc As Integer
    Dim prompt As String
    Dim buttons As Integer
    Dim title As String
    Dim result As Integer
    
    On Error GoTo catch

    prompt = Space$(255)
    rc = lsGetParm(engineid, 1, bSTR, ByVal prompt)
    prompt = (RTrim$(prompt))
    rc = lsGetParm(engineid, 2, bINT, buttons)
    title = Space$(255)
    rc = lsGetParm(engineid, 3, bSTR, ByVal title)
    title = (RTrim$(title))
    
    result = MsgBox(prompt, buttons, title)
    
    PMsgBox4 = lsUnifyParm(engineid, 4, bINT, result)
    
    Exit Function
catch:
    Dim msg As String
    msg = "VBA Error: " & Err.Number & "(PMsgBox4)" & Err.Description
    Err.Clear
    ErrRaiseLS msg
End Function
' Public app As Application
' Attribute VB_Name = "PMsgBoxModule"
' This is the extended predicate which can be called from Prolog
' It is passed the same EngineID argument which was passed to AddPredLS
'
Public Function PDebugPrint(ByVal engineid As Long) As Long
Dim rc As Integer, tstr As String

    On Error GoTo catch
    
    tstr = Space$(255)
    rc = lsGetParm(engineid, 1, bSTR, ByVal tstr)
    ' Debug.Print (RTrim$(tstr))
    PDebugPrint = 1
    
    Exit Function
catch:
    Dim msg As String
    msg = "VBA Error: " & Err.Number & "(PDebugPrint)" & Err.Description
    Err.Clear
    ErrRaiseLS msg
End Function
' This is the extended predicate which can be called from Prolog
' It is passed the same EngineID argument which was passed to AddPredLS
'
Public Function PInputBox(ByVal engineid As Long) As Long
Dim tf As Boolean
Dim rc As Long
Dim prompt As String
Dim title As String
Dim default As String
Dim response As String

    On Error GoTo catch

    ' Trying to solve the problem of it asking on the first load...
    ' hmmm, doesn't let questions be asked in a new spreadsheet.  yuch.
    ' new plan is to simply have Prolog code not ask if no rules present.
    
    'If Not RulesInitialized Then
        'PInputBox = 0
        'Exit Function
    'End If
    
    'prompt = Space$(255)
    'title = Space$(255)
    'default = Space$(255)
    'rc = lsGetParm(engineid, 1, bSTR, ByVal prompt)
    'rc = lsGetParm(engineid, 2, bSTR, ByVal title)
    'rc = lsGetParm(engineid, 3, bSTR, ByVal default)
    prompt = GetStrParmLS(1)
    title = GetStrParmLS(2)
    default = GetStrParmLS(3)
    response = InputBox(prompt, title, default)
    rc = lsUnifyParm(engineid, 4, bSTR, ByVal response & Chr$(0))

    Select Case rc
        Case True
            PInputBox = 0
        Case 1
            PInputBox = 1
        Case Else
            Err.Raise 31111, "PInputBox", "Error processing input box"
    End Select
    
    Exit Function
catch:
    Dim msg As String
    msg = "VBA Error: " & Err.Number & "(PInputBox)" & Err.Description
    Err.Clear
    ErrRaiseLS msg
End Function
' This is the extended predicate which can be called from Prolog
' It is passed the same EngineID argument which was passed to AddPredLS
'
Public Function PCell2(ByVal engineid As Long) As Long
    Dim rc As Long
    Dim value As String
    Dim rname As String
    Dim cc As Range

    On Error GoTo catch

    rname = GetStrParmLS(1)
    
    Set cc = Range(rname)
    value = ValueToString(cc.value)

    rc = lsUnifyParm(engineid, 2, bSTR, ByVal value & Chr$(0))
    Select Case rc
        Case 0
            PCell2 = 0
        Case 1
            PCell2 = 1
        Case Else
            Err.Raise 31111, "PCell2", "Error getting cell value"
        End Select
    
    Exit Function
catch:
    Dim msg As String
    msg = "VBA Error: " & Err.Number & "(PCell2)" & Err.Description
    Err.Clear
    ErrRaiseLS msg
End Function
' Get the number of rows and cols in a range
Public Function PRangeSize3(ByVal engineid As Long) As Long
    Dim rc As Long
    Dim rname As String
    Dim nrows As Integer
    Dim ncols As Integer
    
    On Error GoTo catch
    
    rname = GetStrParmLS(1)
    nrows = Range(rname).Rows.Count
    ncols = Range(rname).Columns.Count
    rc = lsUnifyParm(engineid, 2, bINT, nrows)
    If rc = 1 Then rc = lsUnifyParm(engineid, 3, bINT, ncols)
    Select Case rc
        Case 0
            PRangeSize3 = 0
        Case 1
            PRangeSize3 = 1
        Case Else
            Err.Raise 31111, "PRangeSize3", "Error getting cell value"
        End Select
        
    Exit Function
catch:
    Dim msg As String
    msg = "VBA Error: " & Err.Number & "(PRangeSize3)" & Err.Description
    Err.Clear
    ErrRaiseLS msg
End Function
' This is the extended predicate which can be called from Prolog
' It is passed the same EngineID argument which was passed to AddPredLS
' Called with a range and R, C offset, returns value in cell
Public Function PCell4(ByVal engineid As Long) As Long
    Dim rc As Long
    Dim rname As String
    Dim yrow As Long
    Dim xcol As Long
    Dim value As String
    Dim cc As Range
    
    On Error GoTo catch

    rname = GetStrParmLS(1)
    yrow = GetIntParmLS(2)
    xcol = GetIntParmLS(3)
    
    Set cc = Range(rname).Cells(yrow, xcol)
    value = ValueToString(cc.value)
    
    rc = lsUnifyParm(engineid, 4, bSTR, ByVal value & Chr$(0))
    Select Case rc
        Case 0
            PCell4 = 0
        Case 1
            PCell4 = 1
        Case Else
            Err.Raise 31111, "PCell4", "Error getting cell value"
        End Select
    
    Exit Function
catch:
    Dim msg As String
    msg = "VBA Error: " & Err.Number & "(PCell4)" & Err.Description
    Err.Clear
    ErrRaiseLS msg
End Function
' This is the extended predicate which can be called from Prolog
' It is passed the same EngineID argument which was passed to AddPredLS
'
Public Function PCell3(ByVal engineid As Long) As Long
Dim rc As Long
Dim yrow As Long
Dim xcol As Long
Dim value As String

    On Error GoTo catch

    'value = Space$(255)
    'rc = lsGetParm(engineid, 1, bINT, yrow)
    'rc = lsGetParm(engineid, 2, bINT, xcol)
    yrow = GetIntParmLS(1)
    xcol = GetIntParmLS(2)
    value = Cells(yrow, xcol).value
    rc = lsUnifyParm(engineid, 3, bSTR, ByVal value & Chr$(0))
    Select Case rc
        Case 0
            PCell3 = 0
        Case 1
            PCell3 = 1
        Case Else
            Err.Raise 31111, "PCell3", "Error getting cell value"
        End Select
    
    Exit Function
catch:
    Dim msg As String
    msg = "VBA Error: " & Err.Number & "(PCell3)" & Err.Description
    Err.Clear
    ErrRaiseLS msg
End Function
Public Function PEvaluate(ByVal engineid As Long) As Long
    Dim expression As String
    Dim rc As Integer
    Dim v As Variant
    Dim s As String
    
    On Error GoTo catch
    expression = GetStrParmLS(1)
    v = Evaluate(expression)
    If VarType(v) = vbError Then
        Err.Clear
        ErrRaiseLS "VBA Error: bad evaluate expression: " & expression
    End If
    s = ValueToString(v)
    
    rc = lsUnifyParm(engineid, 2, bSTR, ByVal s & Chr$(0))

    Select Case rc
        Case True
            PEvaluate = 0
        Case 1
            PEvaluate = 1
        Case Else
            ThrowError src:="PEvaluate", msg:="bad_evaluate_unification"
    End Select
    Exit Function
catch:
    Dim msg As String
    msg = "VBA Error: " & Err.Number & "(PEvaluate)" & Err.Description
    Err.Clear
    ErrRaiseLS msg

End Function
Public Function CheckInitialized() As Boolean
    Dim name As String
    
    On Error GoTo endcheck
    name = Application.ActiveWorkbook.name
    On Error GoTo 0
    If bRulesInitialized And name <> RulesWorkbook Then
        'MsgBox "InitializeRuleSets"
        InitializeRuleSets
        CheckInitialized = False
    Else
        CheckInitialized = RulesInitialized
    End If
    
'Dim oldcalc As Integer
'    CheckInitialized = RulesInitialized
'    If RulesInitialized <> True Then
'        oldcalc = Application.Calculation
'        Application.Calculation = xlCalculationManual
'        InitializeRuleSets
'        Application.Calculation = oldcalc
'    End If
'    CheckInitialized = RulesInitialized
endcheck:
End Function
Public Function RulesInitialized() As Boolean
'MsgBox Application.ActiveWorkbook.name & " / " & RulesWorkbook
    If bRulesInitialized = True And Application.ActiveWorkbook.name = RulesWorkbook Then
        RulesInitialized = True
    Else
        RulesInitialized = False
    End If
End Function
Private Sub AboutARules()
    ARulesAbout.Show
End Sub
Private Sub ARulesOptions()
'    AOptions.Show
End Sub

Public Function GetARulesPath() As String
    Dim rc As Integer
    Dim arules_path As String
    Dim hKey, psize As Long


    ' Get path from the registry
    rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE, AMZI_ARULES_KEY, 0, KEY_QUERY_VALUE, hKey)
    If rc = ERROR_NONE Then
        rc = RegQueryValueExNULL(hKey, "InstallDir", 0&, REG_SZ, 0&, psize)
        arules_path = String(psize, 0)
        rc = RegQueryValueExString(hKey, "InstallDir", 0&, REG_SZ, arules_path, psize)
        If rc = ERROR_NONE Then
            arules_path = left$(arules_path, psize - 1)
        Else
            arules_path = ""
        End If
    Else
        arules_path = ""
    End If
    GetARulesPath = arules_path
    
    Exit Function
End Function


Public Function GetHelpPath() As String
    Dim arules_path As String


    ' Get path from the registry
    arules_path = GetARulesPath()
    GetHelpPath = arules_path & "docs\"
    
    Exit Function
End Function





Public Function IsMember(item As String, StrArray As Variant) As Boolean
    Dim i As Integer
    
    
    For i = LBound(StrArray) To UBound(StrArray)
        If item = StrArray(i) Then
            IsMember = True
            Exit Function
        End If
    Next
    IsMember = False
    
    Exit Function
End Function

Public Function RemoveItem(item As String, StrArray As Variant) As Variant
    Dim strNewArray As Variant
    Dim i As Integer
    Dim j As Integer
    
    
    If Not IsMember(item, StrArray) Then
        RemoveItem = StrArray
        Exit Function
    End If
    
    ReDim strNewArray(LBound(StrArray) To UBound(StrArray) - 1)
        
    j = LBound(strNewArray)
    For i = LBound(StrArray) To UBound(StrArray)
        If item <> StrArray(i) Then
            strNewArray(j) = StrArray(i)
            j = j + 1
        End If
    Next i
    
    RemoveItem = strNewArray
    Exit Function
End Function
Public Function ExistsRuleSet(rsName As String) As Boolean
    Dim x As String
    ' hack to find out if a ruleset is defined yet or not
    Err.Clear
    On Error Resume Next
    x = ActiveWorkbook.Names(rsName).name
    If Err.Number = 0 Then
        ExistsRuleSet = True
    Else
        ExistsRuleSet = False
    End If
End Function
Public Function ExistsRuleSets() As Boolean
' Determine if the Name RuleSets is defined yet or not
Dim x As String
    Err.Clear
    On Error Resume Next
    x = ActiveWorkbook.Names("RuleSets").name
    If Err.Number = 0 Then
        ExistsRuleSets = True
    Else
        ExistsRuleSets = False
    End If
End Function
Public Function FileExists(ByVal Filename As String) As Boolean
    FileExists = Len(Dir(Filename)) > 0
End Function

Public Function RVersion() As String
    Dim tf As Boolean
    Dim term As Long
    Dim Version As String
    
    On Error GoTo catch
    
    Version = Space(255)
    
    If ARulesActive = False Then
       RVersion = "ARules Engine not active"
       Exit Function
       End If
            
    tf = ExecStrLS(term, "arules_version(?v)")
    
    Version = GetStrArgLS(term, 1)
    RVersion = Trim(Version)
    
    Exit Function
catch:
    RVersion = DealWithException("RVersion")
End Function

Public Function RAbout() As String
    Dim tf As Boolean
    Dim term As Long
    Dim Edition, Version, Copyright As String

    On Error GoTo catch
    Version = Space(255)

    If ARulesActive = False Then
       RAbout = "ARules Engine not active"
       Exit Function
       End If

    tf = ExecStrLS(term, "arules_edition(?v)")
    Edition = GetStrArgLS(term, 1)
    tf = ExecStrLS(term, "arules_build(?v)")
    Version = GetStrArgLS(term, 1)
    tf = ExecStrLS(term, "arules_copyright(?v)")
    Copyright = GetStrArgLS(term, 1)

    RAbout = Trim(Edition) & ", " & Trim(Version) & vbCr & Copyright

    Exit Function
catch:
    MsgBox prompt:=DealWithException("RAbout"), title:="ARulesXL"
End Function

Public Function RProductDescription() As String
    Dim tf As Boolean
    Dim term As Long
    Dim Version As String
    
    
    Version = Space(255)
    
    If ARulesActive = False Then
       RProductDescription = "ARules Engine not active"
       Exit Function
       End If
            
    tf = ExecStrLS(term, "arules_version(?v)")
    
    Version = GetStrArgLS(term, 1)
    RProductDescription = Trim(Version)
    
    Exit Function
catch:
    RProductDescription = DealWithException("RProductDescription")
End Function
Public Function RUserInfo() As String
    Dim tf As Boolean
    Dim term As Long
    Dim UserName As String
    Dim HardwareFingerprint As String
    Dim MaintenanceDays As String
    
    On Error GoTo catch
    
    UserName = Space(255)
    HardwareFingerprint = Space(255)
    MaintenanceDays = Space(255)
    
    If ARulesActive = False Then
       RUserInfo = "ARules Engine not active"
       Exit Function
       End If
            
    tf = ExecStrLS(term, "user_info(?un, ?hf, ?md)")
    
    UserName = Trim(GetStrArgLS(term, 1))
    If UserName = "DEFAULT" Then
        UserName = "Free Trial"
    End If
    HardwareFingerprint = Trim(GetStrArgLS(term, 2))
    MaintenanceDays = Trim(GetStrArgLS(term, 3))
    
    If Edition = "Runtime" Then
        RUserInfo = GetText("runtime_about_message")
    Else
        RUserInfo = GetText("about_user_name") & UserName
        If MaintenanceDays <> "" Then
            RUserInfo = RUserInfo & vbCr & GetText("about_maintenance_days_left") & MaintenanceDays
        End If
    End If

    Exit Function
catch:
    RUserInfo = DealWithException("RUserInfo")
End Function
' Dump the rules to a listing text file.
Private Function RListing(listing_file As String) As String
    Dim tf As Boolean
    Dim term As Long
    
    On Error GoTo catch
    If Not EndTrace Then Exit Function
    
    tf = ExecStrLS(term, "tell(`" & listing_file & "`)")
    tf = ExecStrLS(term, "listing")
    tf = ExecStrLS(term, "told")
    RListing = "listed"
    
    Exit Function
catch:
    RListing = DealWithException("RListing")
End Function

Public Sub ExportRuleSets()
    iCRuleSets.ExportRuleSetNames
End Sub
Public Function tilt_slashes(ByVal path As String) As String
    Dim i As Integer
    Dim s As String


    s = ""
    For i = 1 To Len(path)
        If (Mid(path, i, 1) = "\") Then
            s = s & "/"
        Else
            s = s & Mid(path, i, 1)
        End If
    Next i
    tilt_slashes = s
End Function

    
Public Function doquery(rule_set As Range, query As String, Trace As Boolean, reuse As Boolean, vbaarray As Boolean, varArgs As Variant) As Variant
    Dim i As Integer
    Dim j As Integer
    Dim ij As Integer
    Dim i1 As Integer
    Dim i2 As Integer
    Dim jmax As Integer
    Dim imax As Integer
    Dim max As Integer
    Dim newquery As String
    Dim term As Long
    Dim tf As Boolean
    Dim ttype As Integer
    Dim ttype2 As Integer
    Dim ttype3 As Integer
    Dim c As Range
    Dim ca As Range
    Dim module As String
    Dim ruleset_return As String
    Dim answer_term As Long
    Dim list_term As Long
    Dim list_term2 As Long
    Dim list_item As Long
    Dim rc As Long
    Dim valstr As String
    Dim answers() As Variant   ' for an array of answers
    Dim x As Variant
    Dim formulastr As String
    Dim requery As Boolean
    Dim dimterm As Long
    Dim dims As Integer
    Dim rs As CRuleSet
    Dim rqs() As String   ' an array of linked dependent RQuerys
    Dim msg As String
    
'    Dim start As Single
    
'    On Error GoTo 0
    
    ' Debug.Print "  doquery Begin: " & query
'    start = Timer
    
    ' If the ruleset isn't actually used, Excel thinks its a false
    ' dependency (and might even if used) so we need to do this to
    ' ensure Excel is happy with rule_set before proceeding further.
    ' If Excel isn't happy, it simply drops execution in mid stream,
    ' goes out and fixes the cells and then starts again, royally
    ' messing up any loop of Logic Server calls.  Note that IsEmpty
    ' tests if a variant is uninitialized or empty which it thinks are
    ' the same.
    
    'MsgBox "in doquery"
        
    ' The rule set might be the actual rules, in which case we just
    ' reconsult them; or it might be indirect, in which case its taken
    ' care of but we still need to get the rule set name.
    requery = reuse
    ReDim rqs(1 To 1)
    rqs(1) = "&%#"  ' not a legal name
LookForRuleSet:
    Set c = rule_set.item(1, 1)
    If c.HasFormula Then  'i.e. rule_set points to a cell with =RQuery()
        If left(c.value, 5) = "Error" Then
            doquery = c.value
            Exit Function
        End If
        formulastr = c.Formula
        i1 = InStr(formulastr, "rquery(")
        If i1 > 0 Then
            formulastr = Mid(formulastr, i1 + 7)
            i1 = InStr(formulastr, ",")
            formulastr = Mid(formulastr, 1, i1 - 1)
            module = Trim(formulastr)
            requery = True
        Else
            i1 = InStr(c.value, ":")
            i2 = InStr(c.value, ",")
            If i2 = 0 Then i2 = Len(c.value) + 1
            module = Mid(c.value, i1 + 1, i2 - i1 - 1)
        End If
    Else
        module = c.value
    End If
    
    module = Trim(module)
    If IsMember(module, rqs) Then
        msg = GetText("circular_rquery_range_reference(`" & module & "`)")
        doquery = msg
        Exit Function
    End If
    i = UBound(rqs)
    ReDim Preserve rqs(1 To i + 1)
    rqs(i + 1) = module

    
    ' put this back in when we want to have links to links to links ...
    Set rs = iCRuleSets.GetRuleSet(module)
    If rs Is Nothing Then
        On Error Resume Next
        Set rule_set = Range(module)
        If Err <> 0 Then
            msg = GetText("invalid_ruleset(`" & module & "`)")
            doquery = msg
            Exit Function
        End If
        On Error GoTo 0
        GoTo LookForRuleSet
    End If
    
'    If Right(module, 1) = "*" Then
'       module = left(module, Len(module) - 1)
'    End If
    
    'MsgBox "module = " & module
    
    'probe
    ' Debug.Print "    init done at " & Timer - start
    
    newquery = query
'    For i = 1 To UBound(varArgs)
    For i = UBound(varArgs) To 1 Step -1
        j = i
        
        valstr = ValueToString(varArgs(i))
        ' valstr, if text, will usually be quoted as an atom, but if
        ' it contains a date term then it will be quoted with a backquote,
        ' which will cause problems when backquoted for ExecStr, so we double
        ' quote them. The reason dates are strings is so the parser can see it
        ' it is a date term and knows to try to parse such from the string.
        If left(valstr, 1) = "`" And Right(valstr, 1) = "`" Then
            newquery = Replace(newquery, "_" & j, "`" & valstr & "`")
        Else
'            newquery = Replace(newquery, "_" & j, "'" & valstr & "'")
            newquery = Replace(newquery, "_" & j, valstr)
        End If

    Next i
        
    If Trace = True Then
        tf = ExecStrLS(term, "initialize_trace(on)")
    Else
        tf = ExecStrLS(term, "initialize_trace(off)")
    End If
    
    
    'Note, you cannot use CurrentArray in a UDF as an array, it
    'is just a single cell.  But you can use the End property of it.
    'This project would be impossible without Google and the sites that address
    'these issues.
    '
    'Getting the array bounds if this is an array query.
    
    imax = 0
    jmax = 0
    dims = 0
    
    'vbaarray is set to true when the initial call was to RArrayQuery()
    'which is supported for the VBA interface.
    'vbaarray is set to false when called from RQuery() on the spreadsheet,
    'so the array stuff is figured out below from the answer instead
    
    If Not vbaarray Then
        ' it appears that if it's a one dimensional array, then
        ' VB throws an error on the .end() calls.  not sure but
        ' hope this kludge fixes it.
        On Error Resume Next
        If Application.ThisCell.HasArray Then
            Set ca = Application.ThisCell.CurrentArray
            ' this fails because it needs blank cells around the array, groan
            'imax = ca.End(xlDown).row - Application.ThisCell.row + 1
            'jmax = ca.End(xlToRight).Column - Application.ThisCell.Column + 1
            ' this fails because it always returns 1
            'imax = ca.Rows.Count
            'jmax = ca.Columns.Count
            ' Thanks to Brad Yundt, this approach works.
            imax = Application.Caller.Rows.Count
            jmax = Application.Caller.Columns.Count
            'If imax = 0 Then imax = 1
            'If jmax = 0 Then jmax = 1
            If imax = 1 Or jmax = 1 Then dims = 1 Else dims = 2
        End If
        If imax = 1 Or jmax = 1 Then dims = 1 Else dims = 2
    End If
    On Error GoTo 0
    
    'probe
    ' Debug.Print "    array messing about done: " & Timer - start
    ' Application.StatusBar = "RQuery: " & module & ": " & newquery
    
    If Trace = True Then
'MsgBox "about to execstrth " & newquery
        tf = ExecStrThLS(term, "query_rules('" & module & "', " & requery & ", `" & newquery & "`, ?ANS, ?imax, ?jmax)")
'MsgBox "leaving execstrth"
        Exit Function
    Else
        tf = ExecStrLS(term, "query_rules('" & module & "', " & requery & ", `" & newquery & "`, ?ANS, ?imax, ?jmax)")
    End If
    
    
    ' Debug.Print "    ExecStr done: " & Timer - start
    
    If (tf = True) Then
        ttype = GetArgTypeLS(term, 4)
        Select Case (ttype)
            Case pINT
                doquery = GetLongArgLS(term, 4)
            Case pATOM, pSTR, pWATOM, pWSTR
                doquery = GetStrArgLS(term, 4)
                If doquery = "[]" Then
                    ReDim answers(1 To imax, 1 To jmax)
                    For i = 1 To imax
                        For j = 1 To jmax
                            answers(i, j) = ""   ' don't use Empty, it causes zeros to display
                        Next j
                    Next i
                    doquery = answers
                End If
            Case pFLOAT, pREAL
                doquery = GetFloatArgLS(term, 4)
            Case pLIST
                ' we have a list argument and we might have an array
                ' query, in which case we need to fill in the array
                If vbaarray Then
                    imax = GetIntArgLS(term, 5)
                    jmax = GetIntArgLS(term, 6)
                    ' 1 returned for list of list, 0 if inner elements are not lists
                    If jmax = 0 Then
                        dims = 1
                        jmax = 1
                    Else
                        dims = 2
                    End If
                End If

                If imax > 0 And jmax > 0 Then
                    ReDim answers(1 To imax, 1 To jmax)
                    For i = 1 To imax
                        For j = 1 To jmax
                            answers(i, j) = ""   ' don't use Empty, it causes zeros to display
                        Next j
                    Next i
                    i = 1
                    Call GetArgLS(term, 4, bTERM, list_term)
                    If imax = 1 Then max = jmax Else max = imax
                    Do Until list_term = 0 Or i > max
                        rc = GetHeadLS(list_term, bTERM, list_item)
                        ttype2 = GetTermTypeLS(list_item)
                        ' list in list, two dimensional array
                        If ttype2 = pLIST Then
                            ' if we really have two dimensions to fill in
                            'If imax > 1 And jmax > 1 Then
                            If dims = 2 Then
                                list_term2 = list_item
                                j = 1
                                Do Until list_term2 = 0 Or j > jmax
                                    rc = GetHeadLS(list_term2, bTERM, list_item)
                                    ttype3 = GetTermTypeLS(list_item)
                                    Select Case (ttype3)
                                    Case pINT
                                        x = GetIntTermLS(list_item)
                                    Case pATOM, pSTR, pWATOM, pWSTR
                                        x = GetStrTermLS(list_item)
                                    Case pFLOAT, pREAL
                                        x = GetFloatTermLS(list_item)
                                    Case Else
                                        x = StringFromTermLS(list_item)
                                    End Select
                                    answers(i, j) = x
                                    j = j + 1
                                    list_term2 = GetTailLS(list_term2)
                                Loop
                            Else
                                x = StringFromTermLS(list_item)
                                If imax = 1 Then answers(1, i) = x Else answers(i, 1) = x
                            End If
                        Else
                            Select Case (ttype2)
                            Case pINT
                                x = GetIntTermLS(list_item)
                            Case pATOM, pSTR, pWATOM, pWSTR
                                x = GetStrTermLS(list_item)
                            Case pFLOAT, pREAL
                                x = GetFloatTermLS(list_item)
                            Case Else
                                x = StringFromTermLS(list_item)
                            End Select
                            If imax = 1 Then answers(1, i) = x Else answers(i, 1) = x
                        End If
                        i = i + 1
                        list_term = GetTailLS(list_term)
                    Loop
                    doquery = answers
                Else
                    Call GetArgLS(term, 4, bTERM, answer_term)
                    doquery = StringFromTermLS(answer_term)
                End If

            Case Else
                Call GetArgLS(term, 4, bTERM, answer_term)
                doquery = StringFromTermLS(answer_term)
        End Select
    Else
        tf = ExecStrLS(term, "query_error(?msg)")
        If tf = True Then
            doquery = GetStrArgLS(term, 1)
        Else
            doquery = "Query failed for unknown reason"
        End If
    End If
    
    'probe
    ' Debug.Print "  doquery   End: " & query & " " & Timer - start
    ' Application.StatusBar = False
    Exit Function
catch:
Dim err_number As Integer
Dim err_description As String
    If left(Err.Source, 4) = "Amzi" Then
        err_number = Err.Number
        err_description = Err.Description
        Err.Clear
        Select Case err_number
        Case 1021
            doquery = GetText("stack_overflow")
        Case Else
            doquery = "ARules Engine error: " & err_number & " " & err_description
        End Select
    Else
        doquery = DealWithException("doquery")
    End If
End Function
Public Function ValueToString(val As Variant) As String
    Dim vart As Integer
    Dim strout As String
    Dim tf As Boolean
    Dim term As Long
    
    On Error GoTo catch
    
    vart = VarType(val)
    Select Case vart
    
    ' Dates might come in as vbDates, or as vbStrings
    Case vbDate
        strout = "datetime(" & year(val) & _
                ", " & month(val) & _
                ", " & day(val) & _
                ", " & Hour(val) & _
                ", " & Minute(val) & _
                ", " & Second(val) & ")"
    
    Case vbInteger, vbLong, vbByte
        strout = CStr(val)
    
    Case vbSingle, vbDouble, vbCurrency, vbDecimal
        ' Need to quote the value since it might be an international
        ' decimal and we need to parse it on the Prolog side.
        strout = "`" & CStr(val) & "`"
        strout = GetVBAValue(strout)
            
    Case vbBoolean
        strout = "`" & CStr(val) & "`"
        strout = GetVBAValue(strout)
    
    Case vbString
        ' a vbString might be a date as well
        ' isdate checks the syntax of the string to see if it's datelike
        If IsDate(val) Then
            strout = "datetime(" & year(val) & _
                    ", " & month(val) & _
                    ", " & day(val) & _
                    ", " & Hour(val) & _
                    ", " & Minute(val) & _
                    ", " & Second(val) & ")"
        
        ' Note that get_vba_value behaves differently for strings and atoms
        ' An single quoted atom just gets taken as a value.  A backquoted
        ' string gets sent through the parser first to see if it parses.  This
        ' is how we pick up special values such as 2 weeks and ['MMR', 'DTaP']
        Else
            strout = "`" & val & "`"
            strout = GetVBAValue(strout)
            
        End If
    
    Case vbEmpty
        strout = "''"
        
    Case Else
        strout = "Unknown data type in argument: " & val
        
    End Select
    ValueToString = strout
    
    Exit Function
catch:
    ValueToString = DealWithException("ValueToString")
End Function
Public Function GetText(tin As String) As String
' Get the national language text associated with a token
Dim term As Long
Dim tf As Boolean

    tf = ExecStrLS(term, "get_text(" & tin & ", ?txt)")
    If tf Then GetText = GetStrArgLS(term, 2) Else GetText = "No text for " & tin
    
    Exit Function

End Function

Public Function GetVBAValue(tin As String) As String
' Use the parser to get a value
Dim tf As Boolean
Dim term As Long
Dim errmsg As String

'    On Error GoTo catch
    tf = ExecStrLS(term, "p_get_vba_value(" & tin & ", ?v)")
    If tf Then
        GetVBAValue = GetStrArgLS(term, 2)
    Else
        GetVBAValue = "Error: can't get value from: " & tin
'        ThrowError("GetVBAValue", "Can't get value from: " & tin)
        ThrowError src:="GetVBAValue", msg:="no_vba_value(`" & tin & "`)"
    End If
    Exit Function
'catch:
'    ' if an engine error, we've already caught it
'    If Err.Number = 1034 Then
'        GetVBAValue = "Error: " & Err.Description
'    Else
'        GetVBAValue = DealWithException("GetVBAValue")
'    End If

End Function

Public Function RRef(rule As String, ParamArray varArgs() As Variant)
' Deprecate this function
    Dim i As Integer
    Dim j As Integer
    Dim newrule As String
    Dim c As Range
    Dim date_str As String
    Dim wname As String
    Dim val As Variant
    Dim valstr As String
    
    On Error GoTo catch
    If Not EndTrace Then Exit Function
    
    For i = 0 To UBound(varArgs)
        If IsEmpty(varArgs(i)) And varArgs(i) <> Empty Then Exit Function
        Next i
    
    newrule = rule
    For i = 0 To UBound(varArgs)
        j = i + 1
        On Error Resume Next
        Set c = varArgs(i)
        If c Is Nothing Then
            wname = "formula"
        ElseIf InStr(c.Worksheet.name, " ") > 0 Then
            wname = "'" + c.Worksheet.name + "'" + "!" + c.Address(False, False)
        Else
            wname = c.Worksheet.name + "!" + c.Address(False, False)
        End If
        
        val = varArgs(i)
        valstr = ValueToString(val)
        newrule = Replace(newrule, "_" & j, valstr & " ~" & wname & "~")

    Next i
    RRef = newrule

    Exit Function
catch:
    RRef = DealWithException("RRef")
End Function
Public Function RInput(r As Range) As String
' Deprecate this function
    Dim name As String
    If Not EndTrace Then Exit Function
    
    On Error GoTo catch
    
    If IsEmpty(r) Then Exit Function
    
    If InStr(r.Worksheet.name, " ") > 0 Then
        name = "'" + r.Worksheet.name + "'"
        Else
        name = r.Worksheet.name
        End If
    RInput = "Input: " & name & "!" & r.Address(False, False)
    Exit Function
catch:
    RInput = DealWithException("RInput")
End Function
'--------------------------------------------
'Public Function RSuperSet(rs_name As String) As String
'    RSuperSet = "# Inherits from rule set: " + rs_name
'End Function
'-------------------------------------------------
'Private Function RSetObject(ParamArray varArgs() As Variant) As String
'    Dim i As Integer
'    Dim j As Integer
'
'    On Error GoTo catch
'
'    ' Make sure Excel is happy
'    For i = 0 To UBound(varArgs)
'        If IsEmpty(varArgs(i)) Then Exit Function
'        Next i
'
'    For i = 0 To UBound(varArgs) Step 2
'        j = i + 1
'        If WorksheetFunction.IsNumber(varArgs(j)) Then
'            RSetObject = varArgs(i) & " = " & varArgs(j)
'        Else
'            RSetObject = varArgs(i) & " = '" & varArgs(j) & "'"
'        End If
'    Next i
'
'
'    Exit Function
'catch:
'    RSetObject = DealWithException("RSetObject")
'End Function

'Private Sub QuickSort(strArray() As String, intBottom As Integer, intTop As Integer)
Private Sub QuickSort(StrArray As Variant, intBottom As Integer, intTop As Integer)

  Dim strPivot As String, strTemp As String
  Dim intBottomTemp As Integer, intTopTemp As Integer
  
  On Error GoTo catch

  intBottomTemp = intBottom
  intTopTemp = intTop

  strPivot = StrArray((intBottom + intTop) \ 2)

  While (intBottomTemp <= intTopTemp)
    
    While (StrArray(intBottomTemp) < strPivot And intBottomTemp < intTop)
      intBottomTemp = intBottomTemp + 1
    Wend
    
    While (strPivot < StrArray(intTopTemp) And intTopTemp > intBottom)
      intTopTemp = intTopTemp - 1
    Wend
    
    If intBottomTemp < intTopTemp Then
      strTemp = StrArray(intBottomTemp)
      StrArray(intBottomTemp) = StrArray(intTopTemp)
      StrArray(intTopTemp) = strTemp
    End If
    
    If intBottomTemp <= intTopTemp Then
      intBottomTemp = intBottomTemp + 1
      intTopTemp = intTopTemp - 1
    End If
  
  Wend

  'the function calls itself until everything is in good order
  If (intBottom < intTopTemp) Then QuickSort StrArray, intBottom, intTopTemp
  If (intBottomTemp < intTop) Then QuickSort StrArray, intBottomTemp, intTop

    Exit Sub
catch:
    DealWithException ("QuickSort")
End Sub

Function SheetExists(sheetname As String) As Boolean
' returns TRUE if the sheet exists in the active workbook
    SheetExists = False
    On Error GoTo NoSuchSheet
    If Len(Sheets(sheetname).name) > 0 Then
        SheetExists = True
        Exit Function
    End If
NoSuchSheet:
End Function

Function WrappedString(st As String, maxchars As Integer) As String
    Dim c As Integer, i As Integer, lastc As Integer, lastspace As Integer


    For i = 1 To Len(st)
        c = c + 1
        If Mid(st, i, 1) = " " Then lastspace = i: lastc = c
        If c > maxchars And lastc <> 0 Then Mid(st, lastspace, 1) = Chr(10): c = maxchars - lastc + 1
    Next
    WrappedString = Replace(st, Chr(10), vbCrLf)
End Function

Public Function GetRegistryMessageIndicator(name As String) As Boolean
    Dim hKey, value As Long
    Dim rc As Integer
    
    GetRegistryMessageIndicator = False
    
    rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE, AMZI_ARULES_KEY, 0, KEY_QUERY_VALUE, hKey)
    If rc = ERROR_NONE Then
        rc = RegQueryValueExLong(hKey, name, 0&, REG_DWORD, value, 4)
        If rc = ERROR_NONE Then
            If CLng(value) <> 0 Then
                GetRegistryMessageIndicator = True
            End If
        End If
    End If

End Function

Public Sub SetRegistryMessageIndicator(name As String, tf As Boolean)
    Dim hKey, value As Long
    Dim rc As Integer
      
    rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE, AMZI_ARULES_KEY, 0, KEY_ALL_ACCESS, hKey)
    If rc = ERROR_NONE Then
        If tf = False Then
            value = 0
        Else
            value = 1
        End If
        
        rc = RegSetValueExLong(hKey, name, 0&, REG_DWORD, value, 4)
    End If

End Sub

Public Sub ListButtonPicturesAndIDs(ByVal intStart As Integer, _
        ByVal intEnd As Integer)

    ' Purpose: Given a starting and ending number, creates a
    ' command bar with pictures corresponding to the face IDs
    ' in the range of numbers provided.

    Dim objCommandBar As Office.CommandBar
    Dim objCommandBarButton As Office.CommandBarButton
    Dim intButton As Integer

    On Error GoTo ListButtonPicturesAndIDs_Err

    If intStart > intEnd Then
        MsgBox "Ending number must be smaller than starting number. " & _
            "Please try again."
        Exit Sub
    End If

    For Each objCommandBar In Application.CommandBars
        If objCommandBar.name = "Button Pictures and IDs" Then
            objCommandBar.Delete
        End If
    Next objCommandBar

    Set objCommandBar = _
        Application.CommandBars.Add("Button Pictures and IDs", , , True)

    For intButton = intStart To intEnd
        Set objCommandBarButton = _
            objCommandBar.Controls.Add(msoControlButton, , , , True)
        With objCommandBarButton
            .FaceId = intButton
            .TooltipText = "FaceID = " & intButton
        End With
    Next intButton
    objCommandBar.Visible = True

ListButtonPicturesAndIDs_End:
    Exit Sub

ListButtonPicturesAndIDs_Err:
    Select Case Err.Number
        Case -2147467259    ' Invalid FaceIDs.
            MsgBox "Invalid range of numbers for face IDs. " & _
                "Please try again."
        Case Else
            MsgBox "Error " & Err.Number & ": " & Err.Description
    End Select
    Resume ListButtonPicturesAndIDs_End

End Sub

Function legal_ruleset_name(n As String) As Boolean
Dim r As Range

    legal_ruleset_name = True
    If InStr(n, " ") Then
        MsgBox prompt:=GetText("no_spaces_in_name"), title:="ARulesXL"
        legal_ruleset_name = False
        Exit Function
    End If
    
    Set r = Range("A1")
    On Error Resume Next
    r.name = n
    If Err <> 0 Then
        MsgBox prompt:=GetText("excel_name_conflict('" & n & "')"), title:="ARulesXL"
        legal_ruleset_name = False
        Exit Function
    End If
    
End Function

Public Sub LoadActiveRuleSets()
    If Not RulesInitialized Then
        InitializeRuleSets
    Else
        iCRuleSets.LoadActives
    End If
End Sub
Public Sub ReloadRuleSets()
    If Not RulesInitialized Then
        InitializeRuleSets
    Else
        iCRuleSets.ReLoadAll
    End If

End Sub

Public Sub OpenTraceLog()
    Dim tracefile As String
    
    On Error GoTo NoTraceLog
    tracefile = ActiveWorkbook.path & "\arulesxl trace.txt"
    Application.Workbooks.OpenText Filename:=tracefile, _
        DataType:=xlDelimited, _
        TextQualifier:=xlTextQualifierNone, _
        ConsecutiveDelimiter:=False, Semicolon:=True
        
    Exit Sub
    
NoTraceLog:
    MsgBox prompt:=GetText("trace_log_not_found"), title:="ARulesXL"
End Sub

Public Sub UpdateRQueries()
Dim ws As Worksheet
Dim c, r As Range
Dim s As String
Dim sup As String
Dim term As Long
Dim tf As Boolean
Dim i, j As Integer

    On Error Resume Next
    For Each ws In Sheets
        Set r = Range(ws.Cells(1, 1), ws.Cells.SpecialCells(xlCellTypeLastCell))
        For i = 1 To r.Rows.Count
        For j = 1 To r.Columns.Count
            Set c = r.Cells(i, j)
            If c.HasFormula Then
                If c.Formula Like "*RQuery*" Then
                    If c.HasArray Then
                        c.AddComment "Translate this cell manually"
                        c.Font.Color = vbRed
                    Else
                        s = "`" & c.Formula & "`"
                        tf = ExecStrLS(term, "pupq_text(" & s & ", ?x)")
                        If tf <> True Or Err <> 0 Then
                            c.AddComment "Translate this cell manually"
                            c.Font.Color = vbRed
                            c.value = Right(c.Formula, Len(c.Formula) - 1)
                        Else
                            Call GetArgLS(term, 2, bSTR, sup)
                            c.Formula = sup
                        End If
                    End If
                End If
            End If
        Next j
        Next i
    Next ws

End Sub

Sub EmbedFileInSheet(filestring As String, sheetname As String)
'reads in the passed file and stores it in a spreadsheet
'as a series of ascii characters.
    Dim tempStr As String * 1
    Dim rowNum As Long
    Dim colNum As Long
    Dim mySheet As Worksheet
    Dim readFile As String
    Dim tempStrBuild As String
    Dim buildCt As Long
    Dim s As String
    Dim r As Range
    
    Set mySheet = Sheets(sheetname)
    mySheet.UsedRange.Delete

    If Dir(filestring) > "" Then
        Open filestring For Binary As #1
        rowNum = 2
        GoTo StartHere
        Do
            If colNum = 256 Then
                colNum = 0
                rowNum = rowNum + 1
            End If
            tempStrBuild = tempStrBuild & "," & Asc(tempStr)
            buildCt = buildCt + 1
            If buildCt = 300 Then
                colNum = colNum + 1
                tempStrBuild = tempStrBuild & ","
                mySheet.Cells(rowNum, colNum).value = tempStrBuild
                tempStrBuild = ""
                buildCt = 0
            End If
StartHere:
            Get #1, , tempStr
        Loop Until EOF(1)
        'at the end.  Clean house & write the rest.
        colNum = colNum + 1
        tempStrBuild = tempStrBuild & ","
        mySheet.Cells(rowNum, colNum).value = tempStrBuild
        Close #1
        s = "=RBinaryRules(A2..IV" + Format$(rowNum, "#####") + ")"
        mySheet.Cells(1, 1).Formula = s
    Else
        MsgBox prompt:=GetText("cannot_find_axl('" & filestring & "')"), title:="ARulesXL"
    End If
End Sub
''StartHere:
''            Get #1, , tempStr
''        Loop Until EOF(1)
''        'at the end.  Clean house & write the rest.
''        colNum = colNum + 1
''        tempStrBuild = tempStrBuild & ","
''        mySheet.Cells(rowNum, colNum).value = tempStrBuild
''        Close #1
''        s = "=RBinaryRulesSP(A2..IV" + Format$(rowNum, "#####") + ")"
''        mySheet.Cells(1, 1).Formula = s
''    Else
''        MsgBox prompt:=GetText("cannot_find_axl('" & filestring & "')"), title:="ARulesXL"
''    End If
''End Sub


Function LoadEmbeddedFile(sheetname As String, ByRef size As Long) As Variant
'writes data stored in the workbook to the specified file.
'assumes the passes path exists, and kills the file if it exists.
    Dim currCell As Range
    Dim mySheet As Worksheet
    Dim writeFile As String
    Dim longStr As String
    Dim writeStr As String * 1
    Dim p As Double
    Dim myLen As Integer
    Dim idx As Long
    Dim i As Double
    Dim axl() As Byte

    Set mySheet = Sheets(sheetname)

'    If Dir(filestring) <> "" Then Kill filestring
'    Open filestring For Binary As #1
    
    ReDim axl(mySheet.UsedRange.Rows.Count * mySheet.UsedRange.Columns.Count * 300)
    idx = 0
    For Each currCell In mySheet.UsedRange
        If Not IsEmpty(currCell.value) And Not currCell.Formula Like "*RBinaryRules*" Then
            longStr = currCell.value
            longStr = Mid(longStr, 2)
            myLen = Len(longStr)
            While myLen > 1
                p = InStr(longStr, ",")
'                writeStr = Chr(Mid(longStr, 1, p - 1))
                axl(idx) = val(Mid(longStr, 1, p - 1))
                idx = idx + 1
'                writeStr = Chr(Mid(longStr, 1, p - 1))
                longStr = Mid(longStr, p + 1)
                myLen = Len(longStr)
                i = i + 1
'                Put #1, , writeStr
            Wend
        End If
    Next
'    Close #1
'    ReDim axl(idx)
    LoadEmbeddedFile = axl
    size = idx
End Function

Public Sub AddFxHelp()
    ' Moved this function here because apparently for some versions of Excel
    ' this type of code can't be in the workbook.  Lot's of Google gurus say
    ' so, no reasons given, another mystery of Microsoft
    
    On Error GoTo catch
    
    Call Application.MacroOptions(Macro:="RCell", _
        Description:=GetText("fx_rcell_description"), _
        HelpFile:=GetHelpPath() + "reference\ref_spreadsheet_functions.htm", _
        Category:="ARulesXL")
    Call Application.MacroOptions(Macro:="RArray", _
        Description:=GetText("fx_rarray_description"), _
        HelpFile:=GetHelpPath() + "reference\ref_spreadsheet_functions.htm", _
        Category:="ARulesXL")
    Call Application.MacroOptions(Macro:="RRowTable", _
        Description:=GetText("fx_rrowtable_description"), _
        HelpFile:=GetHelpPath() + "reference\ref_spreadsheet_functions.htm", _
        Category:="ARulesXL")
    Call Application.MacroOptions(Macro:="RColumnTable", _
        Description:=GetText("fx_rcolumntable_description"), _
        HelpFile:=GetHelpPath() + "reference\ref_spreadsheet_functions.htm", _
        Category:="ARulesXL")
    Call Application.MacroOptions(Macro:="RInputRow", _
        Description:=GetText("fx_rinputrow_description"), _
        HelpFile:=GetHelpPath() + "reference\ref_spreadsheet_functions.htm", _
        Category:="ARulesXL")
    Call Application.MacroOptions(Macro:="RInputColumn", _
        Description:=GetText("fx_rinputcolumn_description"), _
        HelpFile:=GetHelpPath() + "reference\ref_spreadsheet_functions.htm", _
        Category:="ARulesXL")
    Call Application.MacroOptions(Macro:="RQuery", _
        Description:=GetText("fx_rquery_description"), _
        HelpFile:=GetHelpPath() + "reference\ref_spreadsheet_functions.htm", _
        Category:="ARulesXL")
    Call Application.MacroOptions(Macro:="RXLDependency", _
        Description:=GetText("fx_rxldependency_description"), _
        HelpFile:=GetHelpPath() + "reference\ref_spreadsheet_functions.htm", _
        Category:="ARulesXL")
        
    Exit Sub
'Call ListButtonPicturesAndIDs(1, 999)
catch:
    MsgBox "Fx help information failed to install"
    Err.Clear
    
End Sub


