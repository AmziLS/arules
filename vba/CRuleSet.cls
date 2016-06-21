VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "CRuleSet"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private WithEvents rsWorksheet As Worksheet
Attribute rsWorksheet.VB_VarHelpID = -1

Private rsRange As Range
Private rsName As String
Private rsActive As Boolean
Private rsConsulted As Boolean
Private rsHasErrors As Boolean
Private rsOldCalculation As Integer
Private rsLastCell As Range
Private rsTableText As String
Private rsInternalBorderColor As Variant
Private rsInternalBorderStyle As Variant
Private rsInternalBorderWeight As Variant
Private rsFontColor As Variant


'Private Sub Class_Initialize()
'    rsActive = False
'    rsConsulted = False
'    rsHasErrors = False
'End Sub

Public Sub InitializeVeryOld(name As String)
' Initialize an existing rule set from the named range
Dim wsName As String

    Set rsRange = Range(name)
    Set rsWorksheet = rsRange.Worksheet
    rsName = name
    rsRange.Cells(1, 1) = name
    Names.Add name:=rsName & "_arxl", RefersTo:=rsName
    
    rsRange.BorderAround (xlDouble)
    
End Sub

Public Sub InitializeOld(name As String)
' Initialize an existing rule set from the named range
Dim wsName As String

    Set rsRange = Range(name)
    Set rsWorksheet = rsRange.Worksheet
    rsName = name
    
    rsRange.BorderAround (xlDouble)
    
End Sub
Public Sub InitializeNew(name As String, Address As String)
' Initialize a new rule set
Dim wsName As String
Dim i As Integer

    Set rsRange = Range(Address)
    rsName = name
    rsRange.Cells(1, 1) = rsName
    rsRange.Cells(1, 1).Font.Bold = True
    
    i = InStr(Address, "!")
    wsName = left(Address, i - 1)
    ' sheets need single quotes in ranges, but can't have them for identification, sigh
    If left(wsName, 1) = "'" Then
        i = Len(wsName)
        wsName = Mid(wsName, 2, i - 2)
    End If

    Set rsWorksheet = Worksheets(wsName)

    ' save the name in the names collection as well
    On Error Resume Next
    rsRange.name = name
    If Err <> 0 Then
        Call MsgBox(GetText("excel_name_conflict"), vbOKOnly, "ARulesXL")
        Exit Sub
    End If
    Names.Add name:=rsName & "_arxl", RefersTo:=rsName

    NormalStyle
    EditMode
    rsActive = True
End Sub
Public Sub DeleteLoadedRules()
Dim term As Long
Dim tf As Boolean
    If Edition = "Runtime" Then Exit Sub
    If ARulesActive Then tf = ExecStrLS(term, "delete_ruleset('" & rsName & "')")

End Sub

Public Sub LoadRulesInitial()
'    If Edition = "Runtime" Then Exit Sub
    rsRange.ClearComments
    If Not ConsultRules Then
        CheckRuleSet
        EditEnd
        MsgBox prompt:=GetText("rule_set_errors(`" & rsName & "`)"), title:="ARulesXL"
    Else
        EditEnd
    End If
End Sub

Public Sub LoadRules()
    If Edition = "Runtime" Then Exit Sub
    rsRange.ClearComments
    If Not ConsultRules Then
        CheckRuleSet
        EditEnd
        MsgBox prompt:=GetText("rule_set_errors(`" & rsName & "`)"), title:="ARulesXL"
    Else
        EditEnd
    End If
End Sub

Public Sub CheckRuleSet()
    Dim c As Range
    Dim r As Range
    Dim module As String
    Dim i, j, k As Integer
    Dim term As Long
    Dim tf As Boolean
    Dim errmsg As String
    Dim vstr As String
    Dim cc As Range
    Dim date_str As String
    Dim wname As String
    
    On Error GoTo catch
    If Not EndTrace Then Exit Sub
    
    Set r = Range(rsName)
    r.ClearComments
    If InStr(r.Worksheet.name, " ") > 0 Then
        wname = "'" + r.Worksheet.name + "'"
    Else
        wname = r.Worksheet.name
    End If

    
    module = rsName
    
    ' Clear the existing rules and make sure the module exists
    tf = ExecStrLS(term, "clear_ruleset('" & module & "')")
      
    For i = 2 To r.Rows.Count
    For j = 1 To r.Columns.Count
        Set c = r.Cells(i, j)
        vstr = "`" & CStr(c.value) & "`"
        
        ' Do not combine into a single if because and checks all the conditions
        If c.HasFormula Then
            If (c.Formula Like "*RCell*" Or _
                c.Formula Like "*RTable*" Or _
                c.Formula Like "*LoadRules*" Or _
                c.Formula Like "*RXLDependency*") Then
            ' do nothing
            End If
                
        ElseIf Len(vstr) <= 2 Or _
                    left(vstr, 2) = "`#" Or _
                    left(vstr, 2) = "`*" Then
                ' do nothing
                
        ElseIf left(vstr, 6) = "`Table" Or _
                left(vstr, Len(rsTableText) + 1) = "`" & rsTableText Then
            If j = 1 Then
                i = CheckDecisionTable(wname, module, i)
                For k = 1 To r.Columns.Count
                    Set c = r.Cells(i, k)
                    vstr = CStr(c.value)
                    If Len(vstr) > 0 Then
                        c.AddComment GetText("blank_row_ends_table")
                        c.Font.Color = vbRed
                    End If
                Next k
                i = i + 1
            Else
                c.AddComment GetText("tables_first_column")
                c.Font.Color = vbRed
            End If
        
    ' a real rule to add
        Else
            tf = ExecStrLS(term, "add_rule('" & _
                module & "', '" & _
                c.Address(False, False) & "', " & _
                vstr & ")")
            If tf <> True Then FlagCellError cell:=c
        
        End If
    Next j
    Next i
    
    Exit Sub
catch:
    DealWithException ("CheckRuleSet")
End Sub

Public Sub CheckARule()
Dim c As Range
Dim module As String
Dim i, j As Integer       ' row and column indices
Dim term As Long
Dim tf As Boolean
Dim errmsg As String
Dim vstr As String        ' value from a cell
'Dim rule_str As String
'Dim cc As Range
'Dim date_str As String
    
    On Error GoTo catch
    If Not EndTrace Then Exit Sub
    If rsLastCell Is Nothing Then Exit Sub
    
    Set c = rsLastCell
    vstr = "`" & CStr(c.value) & "`"
    
    i = c.row - rsRange.row + 1
    j = c.Column - rsRange.Column + 1
    If i = 1 And j = 1 Then Exit Sub

    c.ClearComments
    If c.Font.Color <> rsFontColor Then c.Font.Color = rsFontColor
    
    module = rsName
            
    ' Do not combine into a single 'if' because 'and' checks all the conditions
    If c.HasFormula Then
        If (c.Formula Like "*RCell*" Or _
            c.Formula Like "*RTable*" Or _
            c.Formula Like "*LoadRules*" Or _
            c.Formula Like "*RXLDependency*") Then
        ' do nothing
        End If
    
    ElseIf Len(vstr) <= 2 Or _
            left(vstr, 2) = "`#" Or _
            left(vstr, 2) = "`*" Then
        ' do nothing
            
    ElseIf left(vstr, Len(rsTableText) + 1) = "`" & rsTableText Or _
            left(vstr, 6) = "`TABLE" Then
        ' check table heading
        tf = ExecStrLS(term, "define_decision_table(" & module & ", " & vstr & ")")
        If tf = False Then
            FlagCellError cell:=c
        End If
        
    ElseIf is_table_header(i, j) = True Then
        ' check a header entry
        If Len(vstr) > 2 Then
            tf = ExecStrLS(term, "check_dtable_header(" & vstr & ")")
            If tf = False Then FlagCellError cell:=c
        End If
        
    ElseIf is_table_entry(i, j) = True Then
        ' check a table entry
        tf = ExecStrLS(term, "check_dtable_entry(" & vstr & ")")
        If tf = False Then FlagCellError cell:=c
    
    Else
        tf = ExecStrLS(term, "check_rule('" & _
            module & "', '" & _
            c.Address(False, False) & "', " & _
            vstr & ")")
        If tf <> True Then FlagCellError cell:=c
        
    End If
            
    Exit Sub
catch:
    DealWithException ("CheckARule")
End Sub
Private Function is_table_header(ByVal row As Integer, ByVal col As Integer) As Boolean
Dim i, j As Integer
Dim vstr As String

    is_table_header = False
    
    i = row - 1
    vstr = "`" & CStr(rsRange.Cells(i, 1).value) & "`"
    If left(vstr, 6) = "`Table" Or _
            left(vstr, Len(rsTableText) + 1) = "`" & rsTableText Then
        is_table_header = True
    End If
    
End Function
Private Function is_table_entry(ByVal row As Integer, ByVal col As Integer) As Boolean
Dim i, j As Integer
Dim vstr As String

    is_table_entry = False
    
    i = row - 1
    vstr = "`" & CStr(rsRange.Cells(i, 1).value) & "`"
    
    While i > 1 And Len(vstr) > 2
        If left(vstr, 6) = "`Table" Or _
                left(vstr, Len(rsTableText) + 1) = "`" & rsTableText Then
            is_table_entry = True
            
            Exit Function
        End If
        i = i - 1
        vstr = "`" & CStr(rsRange.Cells(i, 1).value) & "`"
    Wend
    
End Function

Public Function GetTableRowText(c As Range) As String
Dim i, j, row, col As Integer      ' rows and cols in the range
Dim table_row As Integer
Dim vstr As String
Dim tc As Range                    ' cells in the row

    GetTableRowText = ""
    row = c.row - rsRange.row + 1
    col = c.Column - rsRange.Column + 1
    
    i = row - 1
    vstr = CStr(rsRange.Cells(i, 1).value)
    
    While i > 1 And Len(vstr) > 0
        If left(vstr, 5) = "Table" Or _
                left(vstr, Len(rsTableText)) = rsTableText Then
            table_row = row - i - 1
            GetTableRowText = vstr & " #" & table_row & " "
            
            j = 1
            vstr = CStr(rsRange.Cells(row, j).value)
            While j <= rsRange.Columns.Count And Len(vstr) > 0
                If j = 1 Then
                    GetTableRowText = GetTableRowText & "[" & vstr
                Else
                    GetTableRowText = GetTableRowText & ", " & vstr
                End If
                j = j + 1
                vstr = CStr(rsRange.Cells(row, j).value)
            Wend
            GetTableRowText = GetTableRowText & "]"
            Exit Function
        End If
        i = i - 1
        vstr = CStr(rsRange.Cells(i, 1).value)
    Wend

End Function

Public Function ConsultRules() As Boolean
Dim c As Range
Dim tf As Boolean
Dim term As Long
Dim i, j As Integer
Dim vstr As String
Dim module As String
Dim r As Range
Dim wname As String
Dim mbret As Integer
Dim renamestr As String
Dim deletestr As String
'Dim t1, t2, tx, taddrule As Single

    On Error GoTo catch
    If Not EndTrace Then Exit Function

't1 = Timer

    Set r = rsRange
    
    SaveFormatting
    r.Font.Color = rsFontColor
    r.Borders.LineStyle = xlLineStyleNone
    ResetFormatting

    If rsRange.Cells(1, 1) = "" Then
        deletestr = GetText("delete_ok(" & rsName & ")")
        mbret = MsgBox(deletestr, vbYesNo, "ARulesXL")
        If mbret = vbYes Then
            DeleteRS
            ConsultRules = True  ' so we don't do a check rules next
            Exit Function
        Else
            rsRange.Cells(1, 1) = rsName
        End If
    End If
    
    If rsRange.Cells(1, 1) <> rsName Then
        renamestr = GetText("rename_ok(" & rsName & ", '" & rsRange.Cells(1, 1) & "')")
        mbret = MsgBox(renamestr, vbYesNo, "ARulesXL")
        If mbret = vbYes Then
            RenameRS (rsRange.Cells(1, 1))
        Else
            rsRange.Cells(1, 1) = rsName
        End If
    End If

    ' Check for any dirty cells before going on, if there
    ' are, get out now instead of letting Excel get half way
    ' through and stopping in the middle of execution.
    For Each c In r
        If IsEmpty(c) Then
            If WorksheetFunction.IsText(c.value) Then
                If Not c = "" Then Exit Function
            End If
        End If
    Next c

    module = rsName
    If InStr(r.Worksheet.name, " ") > 0 Then
        wname = "'" + r.Worksheet.name + "'"
    Else
        wname = r.Worksheet.name
    End If

'    If EditMode Then EditStyle Else NormalStyle
    
    rsTableText = GetText("Table")
    ' Clear the existing rules and make sure the module exists
    tf = ExecStrLS(term, "clear_ruleset('" & module & "')")
    
't2 = Timer
'Debug.Print "Initialize: " & (t2 - t1)
't1 = t2
'taddrule = 0

    For i = 2 To r.Rows.Count
    For j = 1 To r.Columns.Count
        Set c = r.Cells(i, j)
        vstr = "`" & CStr(c.value) & "`"
                
        ' Do not make single if with an 'and' it checks all the conditions
        If c.HasFormula Then
            If (c.Formula Like "*RCell*" Or _
                c.Formula Like "*RTable*" Or _
                c.Formula Like "*LoadRules*" Or _
                c.Formula Like "*RXLDependency*") Then
            ' do nothing
            End If
                       
        ' if its a comment, let it slide
        ElseIf Len(vstr) <= 2 Or _
                left(vstr, 2) = "`#" Or _
                left(vstr, 2) = "`*" Then
            ' do nothing
        
        ' maybe a table
        ElseIf left(vstr, 6) = "`Table" Or _
                    left(vstr, Len(rsTableText) + 1) = "`" & rsTableText Then
            If j = 1 Then
                i = ConsultDecisionTable(wname, module, i)
                If i < 0 Then GoTo Fault
            Else
                GoTo Fault
            End If
            
        ' a real rule to add
        Else
            ' add the rule
'tx = Timer
            tf = ExecStrLS(term, "add_rule('" & _
                    module & "', `" & _
                    wname & "!" & c.Address(False, False) & "`, " & _
                    vstr & ")")
'taddrule = taddrule + Timer - tx
            If tf = False Then GoTo Fault
        End If  ' it was a rule or table
        
    Next j
    Next i  ' loop through cells of a rule set
'    Next j
    
't2 = Timer
'Debug.Print "Consult: " & (t2 - t1)
't1 = t2
'Debug.Print "TAddRule: " & taddrule
    
    tf = ExecStrLS(term, "close_ruleset('" & module & "')")   ' end the module specification
    tf = ExecStrLS(term, "unflux_ruleset('" & module & "')")  ' take off in_flux flag
'    iCRuleSets.DirtyAll   ' might be other rulesets referring to this one, dirty them all
    rsConsulted = True
    rsHasErrors = False
    ConsultRules = True

't2 = Timer
'Debug.Print "Clean Up: " & (t2 - t1)
't1 = t2

    'probe
    ' Debug.Print "  Rule Set " & module & " loaded: " & Timer - start

'Next irs  ' loop through rule sets in function call

    'probe
    ' Debug.Print "LoadRules   End: " & Timer - start
    ' Excel worksheet rename bug, don't mess with status bar
    ' Application.StatusBar = False
    Exit Function
    
Fault:
    ' we don't need to process the error here, we'll call check rules next
'    tf = ExecStrLS(term, "get_rule_error(?x)")
'    errmsg = GetStrArgLS(term, 1)
    rsConsulted = False
    rsHasErrors = True
    ConsultRules = False
    ' reset the rules
    tf = ExecStrLS(term, "close_ruleset('" & module & "')")
    tf = ExecStrLS(term, "clear_ruleset('" & module & "')")
    tf = ExecStrLS(term, "unflux_ruleset('" & module & "')")
    Exit Function

catch:
    ConsultRules = DealWithException("ConsultRules")
End Function
Private Function ConsultDecisionTable(wname As String, module As String, ByVal i As Integer) As Integer
' Read in a full decision table, updating the row index
' to return to ConsultRules for continuing it's loop

Dim r, c As Range                        ' rule set range and the current cell
Dim j, k As Integer                      ' row and col indexes
Dim dtab_ul, dtab_lr As Range            ' the boundaries of the table
Dim headers, tablerow As String          ' the column headers and row values
Dim vstr As String                       ' a value from a cell as a string
Dim term As Long                         ' logic server term
Dim tf As Boolean                        ' logic server return

    Set r = rsRange
    If r.Columns.Count = 1 Then GoTo Fault
    
    Set c = r.item(i, 1)
    vstr = "`" & CStr(c.value) & "`"
    
    ' Read the table output column definitions
    tf = ExecStrLS(term, "define_decision_table(" & module & ", " & vstr & ")")
    
    ' Read the table headers
    i = i + 1
    Set c = r.item(i, 1)
    Set dtab_ul = c
    vstr = "`" & CStr(c.value) & "`"
    headers = "[" & vstr
    For k = 2 To r.Columns.Count
        Set c = r.item(i, k)
        vstr = "`" & CStr(c.value) & "`"
        If Len(vstr) > 2 Then
            headers = headers & ", " & vstr
        End If
    Next k
    headers = headers & "]"
    tf = ExecStrLS(term, "decision_table_headers(" & module & ", " & headers & ")")
    
    ' Read the table rows
    i = i + 1
    Set c = r.item(i, 1)
    vstr = "`" & CStr(c.value) & "`"
    While Len(vstr) > 2
        tablerow = "[" & vstr
        For k = 2 To r.Columns.Count
            Set c = r.item(i, k)
            vstr = "`" & CStr(c.value) & "`"
            If Len(vstr) > 2 Then
                tablerow = tablerow & ", " & vstr
                Set dtab_lr = c
            End If
        Next k
        tablerow = tablerow & "]"
        Set c = r.item(i, 1)
        tf = ExecStrLS(term, "decision_table_row(" & _
                module & ", `" & _
                wname & "!" & c.Address(False, False) & "`, " & _
                tablerow & ")")
        If tf = False Then GoTo Fault
        i = i + 1
        Set c = r.item(i, 1)
        vstr = "`" & CStr(c.value) & "`"
    Wend

    On Error Resume Next
    Range(dtab_ul, dtab_lr).Borders.LineStyle = xlContinuous
    Range(dtab_ul, dtab_lr).Borders.Color = vbBlack
    ConsultDecisionTable = i
    Exit Function
    
Fault:
    ConsultDecisionTable = -1
    Exit Function
End Function
Private Function CheckDecisionTable(wname As String, module As String, ByVal i As Integer) As Integer
' Read in a full decision table, updating the row index
' to return to ConsultRules for continuing it's loop

Dim r, c As Range                        ' rule set range and the current cell
Dim j, k As Integer                      ' row and col indexes
Dim dtab_ul, dtab_lr As Range            ' the boundaries of the table
Dim table_cell As Range                  ' the cell with the table statement
Dim headers, tablerow As String          ' the column headers and row values
Dim vstr As String                       ' a value from a cell as a string
Dim term As Long                         ' logic server term
Dim tf As Boolean                        ' logic server return
Dim errmsg As String
Dim row_errors As Integer                ' count the errors on a row
Dim headers_ok As Boolean                ' needed before testing whole row

    Set r = rsRange
    Set c = r.item(i, 1)
    Set table_cell = c
    vstr = "`" & CStr(c.value) & "`"
    If r.Columns.Count = 1 Then
        c.AddComment GetText("narrow_dtable")
        c.Font.Color = vbRed
        GoTo Fault
    End If
    headers_ok = True
    
    ' Read the table output column definitions
    tf = ExecStrLS(term, "define_decision_table(" & module & ", " & vstr & ")")
    If tf = False Then
        FlagCellError cell:=c
        headers_ok = False
    End If
    
    ' Read the table headers
    i = i + 1
    Set c = r.item(i, 1)
    Set dtab_ul = c
    headers = "["
    For k = 1 To r.Columns.Count
        Set c = r.item(i, k)
        vstr = "`" & CStr(c.value) & "`"
        If Len(vstr) > 2 Then
            tf = ExecStrLS(term, "check_dtable_header(" & vstr & ")")
            If tf = False Then
                FlagCellError cell:=c
                headers_ok = False
            End If
            If k > 1 Then headers = headers & ", " & vstr Else headers = headers & " " & vstr
        End If
    Next k
    headers = headers & "]"
    tf = ExecStrLS(term, "decision_table_headers(" & module & ", " & headers & ")")
    If tf = False Then
        FlagCellError cell:=table_cell
        headers_ok = False
    End If
    
    ' Read the table rows
    i = i + 1
    Set c = r.item(i, 1)
    vstr = "`" & CStr(c.value) & "`"
    While Len(vstr) > 2
        tablerow = "["
        row_errors = 0
        For k = 1 To r.Columns.Count
            Set c = r.item(i, k)
            vstr = "`" & CStr(c.value) & "`"
            If Len(vstr) > 2 Then
                tf = ExecStrLS(term, "check_dtable_entry(" & k & ", " & vstr & ")")
                If tf = False Then
                    FlagCellError cell:=c
                    row_errors = row_errors + 1
                Else
                    If k = 1 Then tablerow = tablerow & " " & vstr Else tablerow = tablerow & ", " & vstr
                End If
                Set dtab_lr = c
            End If
        Next k
        tablerow = tablerow & "]"
        ' there might be a rule error not caught in rule, so check it too
        If row_errors = 0 And headers_ok = True Then
            tf = ExecStrLS(term, "decision_table_row(" & _
                module & ", `" & _
                wname & "!" & c.Address(False, False) & "`, " & _
                tablerow & ")")
            If tf = False Then
                Set c = r.item(i, 1)
                FlagCellError cell:=c, note:="Row to rule: "
                For k = 1 To r.Columns.Count
                    Set c = r.item(i, k)
                    c.Font.Color = vbRed
                Next k
            End If
            
        End If
        i = i + 1
        Set c = r.item(i, 1)
        vstr = "`" & CStr(c.value) & "`"
    Wend
    
    On Error Resume Next
    Range(dtab_ul, dtab_lr).Borders.LineStyle = xlContinuous
    Range(dtab_ul, dtab_lr).Borders.Color = vbBlack
    CheckDecisionTable = i
    Exit Function
    
Fault:
    CheckDecisionTable = i
    Exit Function
End Function
Sub FlagCellError(cell As Range, Optional note As String = "")
'When checking a cell for syntax error, this is what we do
Dim tf As Boolean
Dim term As Long
Dim errmsg As String

    tf = ExecStrLS(term, "get_rule_error(?x)")
    If tf = True Then
        errmsg = note & GetStrArgLS(term, 1)
    Else
        errmsg = note & "Error"
    End If
    cell.AddComment errmsg
    cell.Font.Color = vbRed

End Sub
Sub RenameRS(newname As String)
Dim term As Long
Dim tf As Boolean

    On Error Resume Next
    If Not legal_ruleset_name(newname) Then
        rsRange.Cells(1, 1) = rsName
        Exit Sub
    End If
    tf = ExecStrLS(term, "delete_ruleset('" & rsName & "')")
    Names(rsName).Delete
    Names(rsName & "_arxl").Delete
    
    rsName = newname
    rsRange.name = rsName
    Names.Add name:=rsName & "_arxl", RefersTo:=rsName

End Sub
Sub UpdateRS()
    Dim c As Range
    Dim r As Range
    Dim module As String
    Dim i, j, k As Integer
    Dim term As Long
    Dim tf As Boolean
    Dim errmsg As String
    Dim vstr As String
    Dim cc As Range
    Dim date_str As String
    Dim wname As String
    Dim vupstr As String
    
    On Error GoTo catch
    If Not EndTrace Then Exit Sub
    
    Set r = Range(rsName)
    r.ClearComments
    If InStr(r.Worksheet.name, " ") > 0 Then
        wname = "'" + r.Worksheet.name + "'"
    Else
        wname = r.Worksheet.name
    End If

    
    module = rsName
    
    ' Clear the existing rules and make sure the module exists
'    tf = ExecStrLS(term, "clear_ruleset('" & module & "')")
      
    For i = 2 To r.Rows.Count
    For j = 1 To r.Columns.Count
        Set c = r.Cells(i, j)
        vstr = "`" & CStr(c.value) & "`"
        
                
        If Len(vstr) <= 2 Or _
                    left(vstr, 2) = "`#" Or _
                    left(vstr, 2) = "`*" Then
                ' do nothing
                
        ' Do not combine into a single if because something checks all the conditions
        ElseIf c.HasFormula Then
            If (c.Formula Like "*RCell*" Or _
                c.Formula Like "*RTable*" Or _
                c.Formula Like "*LoadRules*" Or _
                c.Formula Like "*RXLDependency*") Then
                
                vstr = "`" & c.Formula & "`"
                tf = ExecStrLS(term, "pup_text(" & vstr & ", ?x)")
                If tf <> True Then
                    c.AddComment "Translate this cell manually"
                    c.Font.Color = vbGreen
                Else
                    Call GetArgLS(term, 2, bSTR, vupstr)
                    c.Formula = vupstr
                End If
            
            End If
                
    ' a rule or decision table cell
        Else
            tf = ExecStrLS(term, "pup_text(" & vstr & ", ?x)")
            If tf <> True Then
                c.AddComment "Translate this cell manually"
                c.Font.Color = vbGreen
            Else
                Call GetArgLS(term, 2, bSTR, vupstr)
                c.value = vupstr
            End If
        
        End If
    Next j
    Next i
    
    Exit Sub
catch:
    DealWithException ("UpdateRS")

End Sub
Sub DeleteRS()
Dim term As Long
Dim tf As Boolean

    On Error Resume Next
    
    tf = ExecStrLS(term, "delete_ruleset('" & rsName & "')")
    Names(rsName).Delete
    Names(rsName & "_arxl").Delete
'    rsRange.Borders.LineStyle = xlLineStyleNone
    rsRange.ClearComments
    
    rsName = "deleted"
    iCRuleSets.setDeleted

End Sub

Sub DeleteRSNames()
' worksheet might have been deleted, so just delete the names.
' the caller is CheckDeletedSheets, so it will do the rest of
' the clean up.
Dim term As Long
Dim tf As Boolean

    On Error Resume Next
    
    tf = ExecStrLS(term, "delete_ruleset('" & rsName & "')")
    Names(rsName).Delete
    Names(rsName & "_arxl").Delete

End Sub

Function InRange(c As Range) As Boolean
' See if a given cell is within the ruleset

    If Not c.Worksheet.name = rsRange.Worksheet.name Then
        InRange = False
        Exit Function
    End If
        
    If Not Intersect(c, rsRange) Is Nothing Then
        InRange = True
    Else
        InRange = False
    End If
    
End Function
Function OverlapRange(r As Range) As Boolean
' See if a given cell is within the ruleset

    If Not r.Worksheet.name = rsRange.Worksheet.name Then
        OverlapRange = False
        Exit Function
    End If
        
    If Not Intersect(r, rsRange) Is Nothing Then
        OverlapRange = True
    Else
        OverlapRange = False
    End If
    
End Function
Function GetRange() As Range
    GetRange = rsRange
End Function
Function GetName() As String
    GetName = rsName
End Function
Function HasErrors() As Boolean
    HasErrors = rsHasErrors
End Function

Public Sub NormalStyle()
    Dim name As String
    Dim s As style
    Dim l, w, c
    
    rsRange.BorderAround LineStyle:=xlDouble, Weight:=xlMedium

End Sub
Private Sub EditMode()
    Dim name As String
    Dim l, w, c
    Dim term As Long
    Dim tf As Boolean
    
    EditStyle
'    rsRange.BorderAround LineStyle:=xlSlantDashDot, Weight:=xlThick
'    rsRange.Cells(1, 1) = rsRange.Cells(1, 1) & "*"
    tf = ExecStrLS(term, "flux_ruleset('" & rsName & "')")

End Sub
Private Sub EditEnd()
Dim term As Long
Dim tf As Boolean

    rsActive = False
    NormalStyle
    tf = ExecStrLS(term, "unflux_ruleset('" & rsName & "')")
    MakeDirty

End Sub

Private Sub SaveFormatting()
    rsFontColor = rsRange.Cells(1, 1).Font.Color
    rsInternalBorderStyle = rsRange.Cells(1, 1).Borders(xlEdgeBottom).LineStyle
    rsInternalBorderWeight = rsRange.Cells(1, 1).Borders(xlEdgeBottom).Weight
    rsInternalBorderColor = rsRange.Cells(1, 1).Borders(xlEdgeBottom).ColorIndex
End Sub

Private Sub ResetFormatting()
    rsRange.Font.Color = rsFontColor
    rsRange.Borders.LineStyle = rsInternalBorderStyle
    rsRange.Borders.Weight = rsInternalBorderWeight
    rsRange.Borders.ColorIndex = rsInternalBorderColor
End Sub

Private Sub EditStyle()
    Dim name As String
    Dim l, w, c
    Dim term As Long
    Dim tf As Boolean
    
'    rsRange.BorderAround LineStyle:=xlSlantDashDot, Weight:=xlThick
'''    rsRange.Cells(1, 1) = rsRange.Cells(1, 1) & "*"
'''    tf = ExecStrLS(term, "flux_ruleset('" & rsname & "')")

End Sub
Public Sub MakeDirty()
' actually called to force recalculation of RCell() etc. in the rules
' but for some reason dirty doesn't dirty unless the worksheet is active.
    rsRange.Worksheet.Activate
    rsRange.Dirty
End Sub
Public Sub LoadActive()
' Called from utilities when someone right clicks inside a rule set
    If rsActive = True Then
        NormalStyle
        LoadRules
        rsActive = False
        MakeDirty
    End If
End Sub
Public Function isSheetDeleted() As Boolean
    isSheetDeleted = True
    On Error GoTo notthere
    If SheetExists(rsRange.Worksheet.name) Then
        isSheetDeleted = False
    Else
        isSheetDeleted = True
    End If
notthere:
End Function

Private Sub Class_Initialize()
    ' get the national language word for table
    rsTableText = GetText("Table")
    rsFontColor = vbBlack
    rsInternalBorderStyle = xlLineStyleNone
    rsInternalBorderColor = 15
    rsInternalBorderWeight = xlThin
End Sub

Private Sub rsWorksheet_SelectionChange(ByVal Target As Range)
    On Error GoTo catch
    If Edition = "Runtime" Then ActiveSheet.Unprotect ("")
'    If Edition = "Runtime" Then Exit Sub
    If Tracing Then Exit Sub
    If rsName = "deleted" Then Exit Sub
    If Not RulesInitialized Then Exit Sub
    If Intersect(Target, rsRange) Is Nothing Then
        If rsActive = True Then Exit Sub
            
        ' do nothing now, user must right click to load new
        ' rules
''            LoadRules
''            Application.Calculation = rsOldCalculation
'' done after loadrules now:
''            rsActive = False
''            NormalStyle
''            MakeDirty

'        End If
    Else
        
        If rsActive = False Then
            If Edition <> "Runtime" Then
                EditMode
    ''            rsOldCalculation = Application.Calculation
    ''            Application.Calculation = xlCalculationManual
                Set rsLastCell = ActiveCell
                rsActive = True
            Else
'                MsgBox "Editting rules has no effect"
'                MsgBox GetText("no_runtime_edit")
                'ActiveSheet.Unprotect ("")
                ActiveCell.Locked = True
                ActiveSheet.Protect ("")
                Call MsgBox(GetText("no_runtime_edit"), vbOKOnly, "ARulesXL")
            End If
        Else
            If Edition <> "Runtime" Then
                CheckARule
                Set rsLastCell = ActiveCell
            Else
'                MsgBox "Editting rules has no effect"
'                MsgBox GetText("no_runtime_edit")
                'ActiveSheet.Unprotect ("")
                ActiveCell.Locked = True
                ActiveSheet.Protect ("")
                Call MsgBox(GetText("no_runtime_edit"), vbOKOnly, "ARulesXL")
            End If
        End If
    End If
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("RuleSetEdit"), title:="ARulesXL"
End Sub

Sub lockrules()
    rsRange.Locked = True
End Sub

Sub unlockrules()
    rsRange.Locked = False
End Sub

End Sub
