Attribute VB_Name = "Interface"
Option Explicit
Option Compare Text

' LFINDVALUE("FIND rate WHEN day = _1 AND time = _2", d4, d5)
Public Function RQuery( _
    RuleSet As Range, _
    QueryText As String, _
    ParamArray Cell_1_to_N() As Variant) As Variant
Attribute RQuery.VB_Description = "Queries the specified rule set with a query of the form 'FIND .object [WHEN .property1 = _1 AND ... OR ...] where the values for _1, _2, etc. come from the specified cells."
Attribute RQuery.VB_ProcData.VB_Invoke_Func = " \n17"
    
    Dim args() As Variant
    Dim i As Integer
    Dim j As Integer
    Dim ret As Variant
    Dim c As Range
    Dim term As Long
    Dim tf As Long
    
    On Error GoTo catch
    If Tracing Then Exit Function
    If Not RulesInitialized Then
        RQuery = "RQuery: " & GetText("rules_not_initialized")
        Exit Function
    End If
    
    ' If the ruleset isn't actually used, Excel thinks its a false
    ' dependency (and might even if used) so we need to do this to
    ' ensure Excel is happy with rule_set before proceeding further.
    ' If Excel isn't happy, it simply drops execution in mid stream,
    ' goes out and fixes the cells and then starts again, royally
    ' messing up any loop of Logic Server calls.  Note that IsEmpty
    ' tests if a variant is uninitialized or empty which it thinks are
    ' the same.

    ' Check for any dirty cells before going on, if there
    ' are, get out now instead of letting Excel get half way
    ' through and stopping in the middle of execution.
    
    ' Also note that WorksheetFunction.IsText causes an error when
    ' calculation = manual.  Not sure why.
    
    'probe
    ' Debug.Print "RQuery Begin"; query
    'probe
    
    If Not Application.Calculation = xlCalculationManual Then
        For Each c In RuleSet
            If IsEmpty(c) Then
                If WorksheetFunction.IsText(c.value) Then
                    If Not c = "" Then Exit Function
                End If
                'Exit For  was this a bug?
                'Exit Function
            End If
        Next c
    End If
    
    ' Need to do the same for optional arguments
    For i = 1 To UBound(Cell_1_to_N)
        'MsgBox "rquery arg " & i & "  " & varArgs(i).value
        If IsEmpty(Cell_1_to_N(i)) Then Exit Function
    Next i
    
    'MsgBox "in rquery"
    'MsgBox "ubound of varargs = " & UBound(varArgs)
    'ReDim g_args(0 To 0)
    For i = 0 To UBound(Cell_1_to_N)
        'MsgBox "i = " & i
        j = i + 1
        ReDim Preserve args(1 To j)
        ' Set args(j) = varArgs(i)   'pick up the range
        args(j) = Cell_1_to_N(i)    'pick up the value
        'MsgBox "varargs(i) = " & varArgs(i)
    Next i
    
    If UBound(Cell_1_to_N) = -1 Then
        ReDim args(0 To 0)
    End If
    
    'MsgBox "calling doquery"
    
    ret = doquery(RuleSet, QueryText, False, False, False, args)
    'MsgBox "rquery return = " & ret
    RQuery = ret
'    Application.ThisCell.Dirty
    'probe
    ' Debug.Print "RQuery End: "; query
    'probe
    Exit Function
catch:
    RQuery = DealWithException("RQuery")
End Function
Public Function RArrayQuery( _
    rule_set As Range, _
    first As Boolean, _
    query As String, _
    ParamArray varArgs() As Variant) As Variant
    
    Dim args() As Variant
    Dim i As Integer
    Dim j As Integer
    Dim ret As Variant
    Dim c As Range
    Dim term As Long
    Dim tf As Long
    Dim reuse As Boolean
'    Dim start As Single
    
    On Error GoTo catch
    If Tracing Then Exit Function
    If Not RulesInitialized Then
        RArrayQuery = "RArrayQuery: " & GetText("rules_not_initialized")
        Exit Function
    End If
    
    'probe
    ' Debug.Print "RArrayQuery Begin: " & query
'    start = Timer
    'probe
    
    If first = True Then reuse = False Else reuse = True
    
    ' If the ruleset isn't actually used, Excel thinks its a false
    ' dependency (and might even if used) so we need to do this to
    ' ensure Excel is happy with rule_set before proceeding further.
    ' If Excel isn't happy, it simply drops execution in mid stream,
    ' goes out and fixes the cells and then starts again, royally
    ' messing up any loop of Logic Server calls.  Note that IsEmpty
    ' tests if a variant is uninitialized or empty which it thinks are
    ' the same.

    ' Check for any dirty cells before going on, if there
    ' are, get out now instead of letting Excel get half way
    ' through and stopping in the middle of execution.
    
    If Not Application.Calculation = xlCalculationManual Then
        For Each c In rule_set
            'If Not c = "" And IsEmpty(c) Then
            If IsEmpty(c) Then
                If WorksheetFunction.IsText(c.value) Then
                    If Not c = "" Then Exit Function
                End If
                'Exit For  was this a bug?
                'Exit Function
            End If
        Next c
    End If
    ' Need to do the same for optional arguments
    For i = 1 To UBound(varArgs)
        'MsgBox "rquery arg " & i & "  " & varArgs(i).value
        If IsEmpty(varArgs(i)) Then Exit Function
    Next i
    
    'MsgBox "in rquery"
    'MsgBox "ubound of varargs = " & UBound(varArgs)
    For i = 0 To UBound(varArgs)
        'MsgBox "i = " & i
        j = i + 1
        ReDim Preserve args(1 To j)
        args(j) = varArgs(i)    'pick up the value
        'MsgBox "varargs(i) = " & varArgs(i)
    Next i
    
    If UBound(varArgs) = -1 Then
        ReDim args(0 To 0)
    End If
    
    'MsgBox "calling doquery"
        
    ret = doquery(rule_set, query, False, reuse, True, args)
    'MsgBox "rquery return = " & ret
    RArrayQuery = ret
    
    'probe
    ' Debug.Print "RArrayQuery   End: " & query & " " & Timer - start
    'probe
    
    Exit Function
catch:
    RArrayQuery = DealWithException("RArrayQuery")
End Function

Public Function LoadRules(ParamArray varArgs() As Variant) As String
Dim args() As Variant
Dim i As Integer
    If Tracing Then Exit Function

    If Not RulesInitialized Then
        LoadRules = "RXLDependency: " & GetText("rules_not_initialized")
        Exit Function
    End If

    ReDim args(LBound(varArgs) To UBound(varArgs))
    For i = LBound(args) To UBound(args)
        Set args(i) = varArgs(i)
    Next i
    LoadRules = XLDependency(args)
End Function

Public Function RXLDependency(ParamArray RuleSet_1_to_N() As Variant) As String
Attribute RXLDependency.VB_Description = "Tells Excel about rule sets that are dependent on the rule set this function is located in."
Attribute RXLDependency.VB_ProcData.VB_Invoke_Func = " \n17"
Dim args() As Variant
Dim i As Integer
Dim iarg As Integer
Dim msg As String

    If Tracing Then Exit Function

    If Not RulesInitialized Then
        RXLDependency = "RXLDependency: " & GetText("rules_not_initialized")
        Exit Function
    End If

    ReDim args(LBound(RuleSet_1_to_N) To UBound(RuleSet_1_to_N))
    For i = LBound(args) To UBound(args)
        If VarType(RuleSet_1_to_N(i)) = vbError Then
            iarg = i + 1
            msg = GetText("invalid_rule_set_name(" & iarg & ")")
            RXLDependency = "RXLDependency: " & GetText("error(`" & msg & "`)")
            Exit Function
        End If
        Set args(i) = RuleSet_1_to_N(i)
    Next i
    RXLDependency = XLDependency(args)
    
End Function

Public Function XLDependency(varArgs() As Variant) As String
' Called by both RXLDependency and deprecated LoadRules

    Dim RuleSetNames As Variant
    Dim irs As Integer
    Dim irsarg As Integer
    Dim msg As String
    Dim ars As CRuleSet
    Dim r As Range
    Dim c As Range
    Dim module As String
  
    On Error GoTo catch
    
    For irs = 0 To UBound(varArgs)
        If VarType(varArgs(irs)) = vbError Then
            irsarg = irs + 1
            msg = GetText("invalid_rule_set_name(" & irsarg & ")")
            XLDependency = "RXLDependency: " & GetText("error(`" & msg & "`)")
            Exit Function
        End If
    Next irs
    
    XLDependency = "RXLDependency: "
    For irs = 0 To UBound(varArgs)
        If irs > 0 Then XLDependency = XLDependency & ", "
        
        On Error Resume Next
        Set r = varArgs(irs)
        
        If Err.Number <> 0 Then
            irsarg = irs + 1
            msg = GetText("invalid_rule_set_name(" & irsarg & ")")
            XLDependency = "RXLDependency: " & GetText("error(`" & msg & "`)")
            Err.Clear
            Exit Function
        End If
        
        On Error GoTo catch
        
        ' Get the module name from the first row of the rule set
        Set c = r.item(1, 1)
        module = c.value
        
        XLDependency = XLDependency & module
        
    Next irs  ' loop through rule sets in function call
    
    Exit Function
catch:
    XLDependency = DealWithException("XLDependency")

End Function

Public Function RCell(PropertyName As String, cell As Range) As String
Attribute RCell.VB_Description = "Sets a property to the value in a cell."
Attribute RCell.VB_ProcData.VB_Invoke_Func = " \n17"
    Dim tf As Boolean
    Dim term As Long
    Dim module As String
    Dim wname As String
    Dim valstr As String
    
    On Error GoTo catch
    
    If Tracing Then Exit Function
    If Not RulesInitialized Then
        RCell = "RCell: " & GetText("rules_not_initialized")
        Exit Function
    End If
    
    If Not cell = "" And IsEmpty(cell) Then
        'Exit For  was this a bug?
        RCell = "RCell:"
        Exit Function
    End If
    module = iCRuleSets.FindRuleSetName(Application.ThisCell)

    If module = "not found" Then
        RCell = "RCell Error: " & GetText("rcell_ruleset")
        Exit Function
    End If
    
    If left(module, 13) = "System Error:" Then
        RCell = "RCell Error: " & module
        Exit Function
    End If
        
    If InStr(cell.Worksheet.name, " ") > 0 Then
        wname = "'" + cell.Worksheet.name + "'"
    Else
        wname = cell.Worksheet.name
    End If

    valstr = ValueToString(cell)
    tf = ExecStrLS(term, "add_data_cell(" & module & ", `" & PropertyName & "`, " & valstr & _
            ", `" & wname & "!" & cell.Address(False, False) & "`)")
    RCell = "RCell: " & PropertyName & " = " & cell.value & "  " & wname & "!" & cell.Address(False, False)
    
'    iCRuleSets.DirtyAllBut (module)
    Exit Function
catch:
    RCell = DealWithException("RCell")
End Function
Public Function RRowTable(PropertyName As String, Cells As Range) As String
Attribute RRowTable.VB_Description = "Sets a two-dimensional array from a range with column headers."
Attribute RRowTable.VB_ProcData.VB_Invoke_Func = " \n17"
        
    RRowTable = RArray(PropertyName, Cells, False, True, False)
        
End Function
Public Function RColumnTable(PropertyName As String, Cells As Range) As String
Attribute RColumnTable.VB_Description = "Sets a two-dimensional array from a range with row headers."
Attribute RColumnTable.VB_ProcData.VB_Invoke_Func = " \n17"
        
    RColumnTable = RArray(PropertyName, Cells, True, False, False)
        
End Function
Public Function RList(PropertyName As String, Cells As Range) As String
    
    RList = RArray(PropertyName, Cells, False, False, True)
        
End Function
Public Function RInputColumn(PropertyName As String, Cells As Range) As String
Attribute RInputColumn.VB_Description = "Sets a one-dimensional input array from a range with row headers."
Attribute RInputColumn.VB_ProcData.VB_Invoke_Func = " \n17"
    
    RInputColumn = RArray(PropertyName, Cells, True, False, True)
        
End Function
Public Function RInputRow(PropertyName As String, Cells As Range) As String
Attribute RInputRow.VB_Description = "Sets a one-dimensional input array from a range with column headers."
Attribute RInputRow.VB_ProcData.VB_Invoke_Func = " \n17"
    
    RInputRow = RArray(PropertyName, Cells, False, True, True)
        
End Function
Public Function RTable(PropertyName As String, Cells As Range, _
        Optional hasRowHdrs As Boolean = True, _
        Optional hasColHdrs As Boolean = True, _
        Optional isVector As Boolean = False) As String

' Just to allow translation from old to new - without this, all RTable()
' calls are translated to unknowns sorts...

    RTable = RArray(PropertyName, Cells, hasRowHdrs, hasColHdrs, isVector)
    
        End Function

Public Function RArray(PropertyName As String, Cells As Range, _
        Optional hasRowHdrs As Boolean = True, _
        Optional hasColHdrs As Boolean = True, _
        Optional isVector As Boolean = False) As String
Attribute RArray.VB_Description = "Sets an array from a range with row and column headers. (These can optionally be turned off.)"
Attribute RArray.VB_ProcData.VB_Invoke_Func = " \n17"
    Dim tf As Boolean
    Dim term As Long
    Dim module As String
    Dim wname As String
    Dim c As Range
    Dim objTerm As Long
    Dim result As String
    Dim row_headers As Variant
    Dim col_headers As Variant
    Dim i As Integer
    Dim j As Integer
    Dim ii As Integer
    Dim jj As Integer
    Dim valstr As String
    Dim rowstr As String
    Dim colstr As String
    Dim irow As Integer
    Dim jcol As Integer
    Dim row_vector As Boolean
    Dim col_vector As Boolean
    Dim start As Single
    
    
    On Error GoTo catch
    If Tracing Then Exit Function
    If Not RulesInitialized Then
        RArray = "RArray: " & GetText("rules_not_initialized")
        Exit Function
    End If
    
'    Application.ScreenUpdating = False
    Application.StatusBar = "RArray " & PropertyName

    'probe
    ' Debug.Print "RTable Begin: " & table.Worksheet.name & "!" & table.address(False, False)
    ' Application.StatusBar = "RTable: " & table.Worksheet.name & "!" & table.address(False, False)
    start = Timer
    
    For Each c In Cells
        If Not c = "" And IsEmpty(c) Then
            'Exit For  was this a bug?
            RArray = "RArray: "
            Exit Function
            End If
        Next c
    module = iCRuleSets.FindRuleSetName(Application.ThisCell)

    If module = "not found" Then
        RArray = "RTable error: " & GetText("rtable_ruleset")
        Exit Function
    End If
    
    If left(module, 13) = "System Error:" Then
        RArray = RArray = "RArray error: " & module
        Exit Function
    End If
        
    If InStr(Cells.Worksheet.name, " ") > 0 Then
        wname = "'" + Cells.Worksheet.name + "'"
    Else
        wname = Cells.Worksheet.name
    End If
    
    'Clear the table and get table object (saved on Prolog side)
    tf = ExecStrLS(term, "initialize_table(" & module & ", `" & PropertyName & "`)")
    If tf = False Then
        RArray = "RTable Error: Failure initializing table " & PropertyName
        Exit Function
    End If
    
    jcol = 1
    If hasRowHdrs Then
        row_headers = Cells.Columns(1)
        jcol = 2
    End If
    
    irow = 1
    If hasColHdrs Then
        col_headers = Cells.Rows(1)
        irow = 2
    End If
    
    If Not isVector Then
        For i = irow To Cells.Rows.Count
            For j = jcol To Cells.Columns.Count
                Set c = Cells.Cells(i, j)
                valstr = ValueToString(c)
                If hasRowHdrs And hasColHdrs Then
                    rowstr = ValueToString(row_headers(i, 1))
                    colstr = ValueToString(col_headers(1, j))
                    tf = ExecStrLS(term, "add_to_table(" & module & ", " _
                                          & rowstr & ", " & colstr & ", " & valstr & _
                                          ", `" & wname & "!" & c.Address(False, False) & "`)")
                ElseIf hasRowHdrs Then
                    rowstr = ValueToString(row_headers(i, 1))
                    jj = j - 1
                    tf = ExecStrLS(term, "add_to_table(" & module & ", " _
                                          & rowstr & ", " & jj & ", " & valstr & _
                                          ", `" & wname & "!" & c.Address(False, False) & "`)")
                ElseIf hasColHdrs Then
                    colstr = ValueToString(col_headers(1, j))
                    ii = i - 1
                    tf = ExecStrLS(term, "add_to_table(" & module & ", " _
                                          & ii & ", " & colstr & ", " & valstr & _
                                          ", `" & wname & "!" & c.Address(False, False) & "`)")
                Else
                    tf = ExecStrLS(term, "add_to_table(" & module & ", " _
                                          & i & ", " & j & ", " & valstr & _
                                          ", `" & wname & "!" & c.Address(False, False) & "`)")
                End If
                
                If tf = False Then
                    RArray = "RArray Error: Failure adding " & c.Address(False, False) _
                              & "to table " & PropertyName
                    Exit Function
                End If
            Next j
        Next i
        RArray = "RArray: " & PropertyName & "[?,?]  " & wname & "!" & Cells.Address(False, False)
    
    ElseIf Cells.Columns.Count - jcol = 0 Then
        For i = irow To Cells.Rows.Count
            Set c = Cells.Cells(i, jcol)
            valstr = ValueToString(c)
            If hasRowHdrs Then
            rowstr = ValueToString(row_headers(i, 1))
            tf = ExecStrLS(term, "add_to_vector(" & module & ", " _
                                  & rowstr & ", " & valstr & _
                                  ", `" & wname & "!" & c.Address(False, False) & "`)")
            Else
            tf = ExecStrLS(term, "add_to_vector(" & module & ", " _
                                  & i & ", " & valstr & _
                                  ", `" & wname & "!" & c.Address(False, False) & "`)")
            End If
            If tf = False Then
                RArray = "RTable Error: Failure adding " & c.Address(False, False) _
                          & "to table " & PropertyName
                Exit Function
            End If
        Next i
        RArray = "RArray: " & PropertyName & "[?]  " & wname & "!" & Cells.Address(False, False)
    
    ElseIf Cells.Rows.Count - irow = 0 Then
        For j = jcol To Cells.Columns.Count
            Set c = Cells.Cells(irow, j)
            valstr = ValueToString(c)
            If hasColHdrs Then
            colstr = ValueToString(col_headers(1, j))
            tf = ExecStrLS(term, "add_to_vector(" & module & ", " _
                                  & colstr & ", " & valstr & _
                                  ", `" & wname & "!" & c.Address(False, False) & "`)")
            Else
            tf = ExecStrLS(term, "add_to_vector(" & module & ", " _
                                  & j & ", " & valstr & _
                                  ", `" & wname & "!" & c.Address(False, False) & "`)")
            End If
            If tf = False Then
                RArray = "RArray Error: Failure adding " & c.Address(False, False) _
                          & "to table " & PropertyName
                Exit Function
            End If
        Next j
        RArray = "RArray: " & PropertyName & "[?]  " & wname & "!" & Cells.Address(False, False)
    
    Else
        RArray = GetText("rtable_too_many_rows")
    End If
    'probe
    ' Debug.Print "RTable   End: " & Timer - start
    ' Application.StatusBar = False

    Exit Function
catch:
    RArray = DealWithException("RArray")
End Function

Public Function RBinaryRules(r As Range)
    RBinaryRules = GetText("binary_rules")
End Function

Public Sub InitializeRuleSets()
' We've just activated a workbook and need to get
' the rule sets from the Names collection.

'    If RulesWorkbook = Application.ActiveWorkbook.name Then
'        bRulesInitialized = True
'        Exit Sub
'    End If
Dim ws As Worksheet
Dim calc As Integer

'    On Error GoTo catch
    If Tracing Then Exit Sub
    If RulesInitialized Then Exit Sub
    Set iCRuleSets = New CRuleSets

    Set ws = Application.ActiveSheet
'MsgBox "InitializeRuleSets for " & ws.name
    
    ' 2007-Feb-13: Setting the calculation mode interferes with Boeings SDI add-in.
    ' It is not necessary to set the mode to manual to cause the problem;
    ' just saving the setting and setting it back causes failure, hence
    ' Excel must do something when calculation is set (even if it is unchanged).
'    calc = Application.Calculation
'    Application.Calculation = xlCalculationManual
    Application.ScreenUpdating = False
    
    Application.StatusBar = GetText("initializing_rule_sets")
    
    ' 2007-Feb-17: Must set this flag before dirty because calculation mode
    ' is not changed to manual. Otherwise RQueries will not recalculate.
    ' 2007-Feb-18: dcm: Needs to be put before the call to Initialize
    ' because it loads rules which dirties all which starts initialize again.
    ' Also need to set RulesWorkbook here as well since it is used in
    ' CheckInitialized which is call from sheet_activate.
    
    bRulesInitialized = True
    RulesWorkbook = Application.ActiveWorkbook.name
    
    iCRuleSets.Initialize
    RulesWorkbook = Application.ActiveWorkbook.name
    
    Application.StatusBar = GetText("inputing_data")
    
    ' 2007-Feb-17: Must set this flag before dirty because calculation mode
    ' is not changed to manual. Otherwise RQueries will not recalculate.
    bRulesInitialized = True
    iCRuleSets.DirtyAll
        
    Application.StatusBar = "Done"
    
    Application.StatusBar = False
'    Application.Calculation = calc
    Application.ScreenUpdating = True

    ws.Activate
    
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("InitializeRuleSets"), title:="ARulesXL"
End Sub

Public Sub CloseRuleSets()
    On Error GoTo catch
    If Tracing Then Exit Sub
    If iCRuleSets Is Nothing Then Exit Sub
    iCRuleSets.CloseAll
    Set iCRuleSets = Nothing
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("CloseRuleSets"), title:="ARulesXL"
End Sub
Private Sub ARulesHelp()
    Dim arules_path As String
    On Error GoTo catch

    ' Get path from the registry
    arules_path = GetHelpPath()
    VBShellExecute arules_path + "index.htm"
    
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("ARulesHelp"), title:="ARulesXL"
End Sub

Private Sub ARulesSupport()
    On Error GoTo catch
    VBShellExecute "http://forum.arulesxl.com/"
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("ARulesSupport"), title:="ARulesXL"
End Sub

Public Sub NewRuleSet()
    On Error GoTo catch
    If Tracing Then Exit Sub
    Call iCRuleSets.NewRuleSet
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("NewRuleSet"), title:="ARulesXL"
End Sub

Private Sub ARulesTutorial()
    Dim arules_path As String

    On Error GoTo catch

    ' Get path from the registry
    arules_path = GetHelpPath()
    VBShellExecute arules_path + "index_tutor.htm"
    
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("ARulesTutorial"), title:="ARulesXL"
End Sub

Private Sub ARulesSamples()
    Dim arules_path As String
    Dim sample As Workbook
    Dim sample_path As String

    On Error GoTo catch
    If Tracing Then Exit Sub

    ' Get path from arulesxl_link.txt for now
'    FileID = FreeFile
'    Open ARulesWB.Path + "\arulesxl_link.txt" For Input As #FileID
'    Input #FileID, arules_path
'    Close #FileID


    ' Get path from the registry
    arules_path = GetARulesPath()
    
    With Application.FileDialog(msoFileDialogFilePicker)
        .title = "ARulesXL Samples"
        .Filters.Add "Excel Files", "*.xls", 1
        .InitialFileName = arules_path + "samples"
        If .Show = False Then Exit Sub
        sample_path = .SelectedItems(1)
        Set sample = Workbooks.Open(Filename:=sample_path, AddToMru:=True)
    End With
        
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("ARulesSamples"), title:="ARulesXL"
End Sub

Public Function ExportRules() As String
    Dim tf As Boolean
    Dim term As Long
    Dim defname As String
    Dim Filename As String
    Dim tempfile As String
    Dim tempdir As String
    Dim s As String
    Dim x As Long
    Dim fso As Object

    On Error GoTo catch
    If Tracing Then Exit Function

    ' Professional edition only
    tf = ExecStrLS(term, "license_type(?type)")
    s = Trim(GetStrArgLS(term, 1))
    If s <> "Professional" Then
        MsgBox (GetText("no_license"))
        Exit Function
    End If

    ' Used to parse file names, delete files, check for existence, etc.
    Set fso = CreateObject("Scripting.FileSystemObject")

    ' Default export file is same as workbook but with .axl
    defname = Application.ActiveWorkbook.name
    If InStr(defname, ".xls") > 0 Then
        defname = Replace(defname, ".xls", ".axl")
    Else
        defname = Replace(defname, ".XLS", ".AXL")
    End If

    ' Get the export file name
    Filename = Application.GetSaveAsFilename(InitialFileName:=defname, _
                FileFilter:="ARulesXL Files (*.axl), *.axl", _
                title:="ARulesXL Export")
    If Filename = "False" Then
        ExportRules = "CANCEL"
        Exit Function
    End If
        
    ' If the file exists, see if we want to overwrite it
    If fso.FileExists(Filename) Then
        If MsgBox("The file " & Filename & " already exists. Do you want to replace the existing file?", vbYesNo, "ARulesXL") = vbNo Then
            Exit Function
        End If
    End If

    ' Set the temporary file and directory
    tempdir = fso.GetParentFolderName(Filename)
    tempfile = tempdir & Application.PathSeparator & "arulesxl.tmp"

    ' arxl_init_export throws errors and never fails
    tf = ExecStrLS(term, "arxl_init_export(`" & tilt_slashes(tempfile) & "`)")

    ExportRuleSets
    tf = ExecStrLS(term, "told")

    On Error GoTo compile_error
    tf = ExecStrLS(term, "arxl_compile(`" & tilt_slashes(tempfile) & "`, `" & tilt_slashes(Filename) & "`)")
    If tf = False Then
        DealWithException "ExportRules: Compile"
    End If

    fso.deletefile tempfile
    ExportRules = Filename
    Exit Function
catch:
    MsgBox prompt:=DealWithException("ExportRules"), title:="ARulesXL"
    ExportRules = "ERROR"
    Exit Function
compile_error:
    If Err.Source Like "*lsExecStr*" Then
        MsgBox prompt:="Export Compile Error: " & Err.Number & " - " & Err.Description, title:="ARulesXL"
    Else
        MsgBox prompt:=DealWithException("ExportRules"), title:="ARulesXL"
    End If
    ExportRules = "ERROR"
End Function

Public Sub ExportExcelRuntime()
    Dim Filename As String
    Dim sheetname As String
    Dim sheet As Worksheet
    Dim currentsheet As Worksheet
    Dim tf As Boolean
    Dim term As Long
    Dim s, tempfile, tempdir, axlfile As String
    Dim fso As Object

    On Error GoTo catch
    If Tracing Then Exit Sub

    ' Professional edition only
    tf = ExecStrLS(term, "license_type(?type)")
    s = Trim(GetStrArgLS(term, 1))
    If s <> "Professional" Then
        MsgBox (GetText("no_license"))
        Exit Sub
    End If

    ' Used to parse file names, delete files, check for existence, etc.
    Set fso = CreateObject("Scripting.FileSystemObject")

    ' Set the temporary file and directory
    tempdir = fso.GetParentFolderName(Filename)
    tempfile = tempdir & Application.PathSeparator & "arulesxl.tmp"
    axlfile = tempdir & Application.PathSeparator & "arulesxl.axl"

    ' arxl_init_export throws errors and never fails
    tf = ExecStrLS(term, "arxl_init_export(`" & tilt_slashes(tempfile) & "`)")

    ExportRuleSets
    tf = ExecStrLS(term, "told")

    On Error GoTo compile_error
    tf = ExecStrLS(term, "arxl_compile(`" & tilt_slashes(tempfile) & "`, `" & tilt_slashes(axlfile) & "`)")
    If tf = False Then
        DealWithException "ExportExcelRuntime: Compile"
    End If

    fso.deletefile tempfile
    
    ' Save current sheet, there is none!
    'currentsheet = ActiveWorkbook.ActiveSheet
    sheetname = "ARulesXL Binary"
    
    On Error Resume Next
    Set sheet = Worksheets(sheetname)
    If sheet Is Nothing Then
        Sheets.Add.name = sheetname
        Sheets(sheetname).Move After:=Sheets(Sheets.Count)
    End If
    On Error GoTo catch

    Call EmbedFileInSheet(axlfile, sheetname)
    fso.deletefile axlfile
    
    'currentsheet.Activate
    
    Exit Sub
    
catch:
    MsgBox prompt:=DealWithException("ExportExcelRuntime"), title:="ARulesXL"
    Exit Sub
compile_error:
    If Err.Source Like "*lsExecStr*" Then
        MsgBox prompt:="ExportExcelRuntime Compile Error: " & Err.Number & " - " & Err.Description, title:="ARulesXL"
    End If
End Sub

Public Sub ExportSharepoint()
    Dim s As String
    
    Call ExportExcelRuntime
    On Error GoTo Convert
    s = Names("arxlType").name
    If Names("arxlType").value Like "*Sharepoint*" Then GoTo SkipConvert
Convert:
    ConvertRFunctions ("SP")
    Names.Add name:="arxlType", RefersTo:="Sharepoint"
SkipConvert:

End Sub

Public Sub RevertSharepoint()
    Dim calc As Integer
    Dim was_saved As Boolean
    If Names("arxlType").value Like "*Sharepoint*" Then
        calc = Application.Calculation
        Application.Calculation = xlCalculationManual
        ConvertRFunctions ("")
        Application.Calculation = calc
        Names("arxlType").RefersTo = "Standard"
        was_saved = ActiveWorkbook.Saved
        CloseRuleSets
        bRulesInitialized = False
        RulesWorkbook = ""
        Call InitializeRuleSets
        ActiveWorkbook.Saved = was_saved   ' we haven't really changed anything yet

    End If
End Sub

Public Sub ConvertRFunctions(suffix As String)
Dim ws As Worksheet
Dim c, r As Range
Dim s As String
Dim sup As String
Dim term As Long
Dim tf As Boolean
Dim i, j, paren, comma1, comma2, arg1 As Integer
Dim RuleSet As String
Dim depends As String
Dim found As Boolean

    On Error GoTo catch
    depends = "'ARulesXL Binary'!A1"
    For Each ws In Sheets
        Set r = Range(ws.Cells(1, 1), ws.Cells.SpecialCells(xlCellTypeLastCell))
        For i = 1 To r.Rows.Count
        For j = 1 To r.Columns.Count
            Set c = r.Cells(i, j)
            If c.HasFormula Then
                s = c.Formula
                paren = InStr(1, s, "(", vbTextCompare)
                found = False
                If c.Formula Like "*RQuery*" Then
                    If suffix = "" Then
                        comma1 = InStr(1, s, ",", vbTextCompare)
                        ' Skip over any spaces
                        comma1 = comma1 + 1
                        While (Mid$(s, comma1, 1) = " ")
                            comma1 = comma1 + 1
                        Wend
                        s = "=RQuery(" + Mid$(s, comma1)
                    Else
                        s = "=RQuery" + suffix + "(" + depends + ", " + Mid$(s, paren + 1)
                    End If
                    found = True
                End If
                If c.Formula Like "*RCell*" Then
                    RuleSet = iCRuleSets.FindRuleSetName(r.Cells(i, j))
                    s = "=RCell" + suffix + Mid$(s, paren)
                    If suffix = "" Then
                        paren = InStr(1, s, "(", vbTextCompare)
                        comma1 = InStr(paren, s, ",", vbTextCompare)
                        comma2 = InStr(comma1 + 1, s, ",", vbTextCompare)
                        ' Skip over any spaces
                        comma2 = comma2 + 1
                        While (Mid$(s, comma2, 1) = " ")
                            comma2 = comma2 + 1
                        Wend
                        s = left$(s, paren) + Right$(s, Len(s) - (comma2 - 1))
                    Else
                        paren = InStr(1, s, "(", vbTextCompare)
                        s = left$(s, paren) & depends & ", """ & RuleSet & """, " & Right$(s, Len(s) - paren)
                    End If

                    found = True
                End If
                If c.Formula Like "*RTable*" Then
                    RuleSet = iCRuleSets.FindRuleSetName(r.Cells(i, j))
                    s = "=RTable" + suffix + Mid$(s, paren)
                    If suffix = "" Then
                        paren = InStr(1, s, "(", vbTextCompare)
                        comma1 = InStr(paren, s, ",", vbTextCompare)
                        comma2 = InStr(comma1 + 1, s, ",", vbTextCompare)
                        ' Skip over any spaces
                        comma2 = comma2 + 1
                        While (Mid$(s, comma2, 1) = " ")
                            comma2 = comma2 + 1
                        Wend
                        s = left$(s, paren) + Right$(s, Len(s) - (comma2 - 1))
                    Else
                        paren = InStr(1, s, "(", vbTextCompare)
                        s = left$(s, paren) & depends & ", """ & RuleSet & """, " & Right$(s, Len(s) - paren)
                    End If

                    found = True
                End If
                If c.Formula Like "*RBinaryRules*" Then
                    s = "=RBinaryRules" + suffix + Mid$(s, paren)
                    found = True
                End If
                If c.Formula Like "*RXLDependency*" Then
                    s = "=RXLDependency" + suffix + Mid$(s, paren)
                    found = True
                End If
                If found Then
                    c.Formula = s
                End If
            End If
        Next j
        Next i
    Next ws
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("Convert Functions"), title:="ARulesXL"
End Sub

Public Sub DeleteRuleSet()
    On Error GoTo catch
    If Tracing Then Exit Sub
    If Not RulesInitialized Then Exit Sub
    iCRuleSets.DeleteRuleSet
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("DeleteRuleSet"), title:="ARulesXL"
End Sub


Public Function VBARQuery( _
    RuleSet As Range, _
    QueryText As String, _
    ParamArray Cell_1_to_N() As Variant) As Variant
    
    Dim args() As Variant
    Dim i As Integer
    Dim j As Integer
    Dim ret As Variant
    Dim c As Range
    Dim term As Long
    Dim tf As Long
    
    On Error GoTo catch
    If Tracing Then Exit Function
    If Not RulesInitialized Then
        VBARQuery = "RQuery: " & GetText("rules_not_initialized")
        Exit Function
    End If
    
    ' If the ruleset isn't actually used, Excel thinks its a false
    ' dependency (and might even if used) so we need to do this to
    ' ensure Excel is happy with rule_set before proceeding further.
    ' If Excel isn't happy, it simply drops execution in mid stream,
    ' goes out and fixes the cells and then starts again, royally
    ' messing up any loop of Logic Server calls.  Note that IsEmpty
    ' tests if a variant is uninitialized or empty which it thinks are
    ' the same.

    ' Check for any dirty cells before going on, if there
    ' are, get out now instead of letting Excel get half way
    ' through and stopping in the middle of execution.
    
    ' Also note that WorksheetFunction.IsText causes an error when
    ' calculation = manual.  Not sure why.
    
    'probe
    ' Debug.Print "RQuery Begin"; query
    'probe
    
    If Not Application.Calculation = xlCalculationManual Then
        For Each c In RuleSet
            If IsEmpty(c) Then
                If WorksheetFunction.IsText(c.value) Then
                    If Not c = "" Then Exit Function
                End If
                'Exit For  was this a bug?
                'Exit Function
            End If
        Next c
    End If
    
    ' Need to do the same for optional arguments
    For i = 1 To UBound(Cell_1_to_N)
        'MsgBox "rquery arg " & i & "  " & varArgs(i).value
        If IsEmpty(Cell_1_to_N(i)) Then Exit Function
    Next i
    
    'MsgBox "in rquery"
    'MsgBox "ubound of varargs = " & UBound(varArgs)
    'ReDim g_args(0 To 0)
    For i = 0 To UBound(Cell_1_to_N)
        'MsgBox "i = " & i
        j = i + 1
        ReDim Preserve args(1 To j)
        ' Set args(j) = varArgs(i)   'pick up the range
        args(j) = Cell_1_to_N(i)    'pick up the value
        'MsgBox "varargs(i) = " & varArgs(i)
    Next i
    
    If UBound(Cell_1_to_N) = -1 Then
        ReDim args(0 To 0)
    End If
    
    'MsgBox "calling doquery"
    
    ret = doquery(RuleSet, QueryText, False, False, False, args)
    'MsgBox "rquery return = " & ret
    VBARQuery = ret
'    Application.ThisCell.Dirty
    'probe
    ' Debug.Print "RQuery End: "; query
    'probe
    Exit Function
catch:
    VBARQuery = DealWithException("RQuery")
End Function

Public Function VBARQueryMore( _
    RuleSet As Range, _
    QueryText As String, _
    ParamArray Cell_1_to_N() As Variant) As Variant
    
    Dim args() As Variant
    Dim i As Integer
    Dim j As Integer
    Dim ret As Variant
    Dim c As Range
    Dim term As Long
    Dim tf As Long
    
    On Error GoTo catch
    If Tracing Then Exit Function
    If Not RulesInitialized Then
        VBARQueryMore = "RQuery: " & GetText("rules_not_initialized")
        Exit Function
    End If
    
    ' If the ruleset isn't actually used, Excel thinks its a false
    ' dependency (and might even if used) so we need to do this to
    ' ensure Excel is happy with rule_set before proceeding further.
    ' If Excel isn't happy, it simply drops execution in mid stream,
    ' goes out and fixes the cells and then starts again, royally
    ' messing up any loop of Logic Server calls.  Note that IsEmpty
    ' tests if a variant is uninitialized or empty which it thinks are
    ' the same.

    ' Check for any dirty cells before going on, if there
    ' are, get out now instead of letting Excel get half way
    ' through and stopping in the middle of execution.
    
    ' Also note that WorksheetFunction.IsText causes an error when
    ' calculation = manual.  Not sure why.
    
    'probe
    ' Debug.Print "RQuery Begin"; query
    'probe
    
    If Not Application.Calculation = xlCalculationManual Then
        For Each c In RuleSet
            If IsEmpty(c) Then
                If WorksheetFunction.IsText(c.value) Then
                    If Not c = "" Then Exit Function
                End If
                'Exit For  was this a bug?
                'Exit Function
            End If
        Next c
    End If
    
    ' Need to do the same for optional arguments
    For i = 1 To UBound(Cell_1_to_N)
        'MsgBox "rquery arg " & i & "  " & varArgs(i).value
        If IsEmpty(Cell_1_to_N(i)) Then Exit Function
    Next i
    
    'MsgBox "in rquery"
    'MsgBox "ubound of varargs = " & UBound(varArgs)
    'ReDim g_args(0 To 0)
    For i = 0 To UBound(Cell_1_to_N)
        'MsgBox "i = " & i
        j = i + 1
        ReDim Preserve args(1 To j)
        ' Set args(j) = varArgs(i)   'pick up the range
        args(j) = Cell_1_to_N(i)    'pick up the value
        'MsgBox "varargs(i) = " & varArgs(i)
    Next i
    
    If UBound(Cell_1_to_N) = -1 Then
        ReDim args(0 To 0)
    End If
    
    'MsgBox "calling doquery"
    
    ret = doquery(RuleSet, QueryText, False, True, False, args)
    'MsgBox "rquery return = " & ret
    VBARQueryMore = ret
'    Application.ThisCell.Dirty
    'probe
    ' Debug.Print "RQuery End: "; query
    'probe
    Exit Function
catch:
    VBARQueryMore = DealWithException("RQuery")
End Function

Public Function VBARArrayQuery( _
    rule_set As Range, _
    first As Boolean, _
    query As String, _
    ParamArray varArgs() As Variant) As Variant
    
    Dim args() As Variant
    Dim i As Integer
    Dim j As Integer
    Dim ret As Variant
    Dim c As Range
    Dim term As Long
    Dim tf As Long
    Dim reuse As Boolean
'    Dim start As Single
    
    On Error GoTo catch
    If Tracing Then Exit Function
    If Not RulesInitialized Then
        VBARArrayQuery = "RArrayQuery: " & GetText("rules_not_initialized")
        Exit Function
    End If
    
    'probe
    ' Debug.Print "RArrayQuery Begin: " & query
'    start = Timer
    'probe
    
    If first = True Then reuse = False Else reuse = True
    
    ' If the ruleset isn't actually used, Excel thinks its a false
    ' dependency (and might even if used) so we need to do this to
    ' ensure Excel is happy with rule_set before proceeding further.
    ' If Excel isn't happy, it simply drops execution in mid stream,
    ' goes out and fixes the cells and then starts again, royally
    ' messing up any loop of Logic Server calls.  Note that IsEmpty
    ' tests if a variant is uninitialized or empty which it thinks are
    ' the same.

    ' Check for any dirty cells before going on, if there
    ' are, get out now instead of letting Excel get half way
    ' through and stopping in the middle of execution.
    
    If Not Application.Calculation = xlCalculationManual Then
        For Each c In rule_set
            'If Not c = "" And IsEmpty(c) Then
            If IsEmpty(c) Then
                If WorksheetFunction.IsText(c.value) Then
                    If Not c = "" Then Exit Function
                End If
                'Exit For  was this a bug?
                'Exit Function
            End If
        Next c
    End If
    ' Need to do the same for optional arguments
    For i = 1 To UBound(varArgs)
        'MsgBox "rquery arg " & i & "  " & varArgs(i).value
        If IsEmpty(varArgs(i)) Then Exit Function
    Next i
    
    'MsgBox "in rquery"
    'MsgBox "ubound of varargs = " & UBound(varArgs)
    For i = 0 To UBound(varArgs)
        'MsgBox "i = " & i
        j = i + 1
        ReDim Preserve args(1 To j)
        args(j) = varArgs(i)    'pick up the value
        'MsgBox "varargs(i) = " & varArgs(i)
    Next i
    
    If UBound(varArgs) = -1 Then
        ReDim args(0 To 0)
    End If
    
    'MsgBox "calling doquery"
        
    ret = doquery(rule_set, query, False, reuse, True, args)
    'MsgBox "rquery return = " & ret
    VBARArrayQuery = ret
    
    'probe
    ' Debug.Print "RArrayQuery   End: " & query & " " & Timer - start
    'probe
    
    Exit Function
catch:
    VBARArrayQuery = DealWithException("RArrayQuery")
End Function





