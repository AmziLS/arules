VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "EventCatcher"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
' Module used to respond to various Excel
' events such as workbook open, none active
' right now.
Option Explicit
Option Compare Text
Public WithEvents app As Application
Attribute app.VB_VarHelpID = -1
Public axlName As String

'Dim saveas_flag As Boolean

Private Sub app_NewWorkbook(ByVal wb As Workbook)
'MsgBox "NewWorkbook " & Wb.name
End Sub

Private Sub app_SheetActivate(ByVal sh As Object)
' no way to tell if a sheet has been deleted, so each time a new
' sheet is activated we have to go make sure all the sheets we used
' to have rules on are still there.  sigh.
    iCRuleSets.CheckDeletedSheets
End Sub

Private Sub app_SheetCalculate(ByVal sh As Object)
    CheckInitialized
End Sub



Private Sub app_WorkbookActivate(ByVal wb As Workbook)
    Dim was_saved As Boolean
    Dim calc As Integer
    Dim axl As String
    
'MsgBox "WorkbookActivate " & wb.name
    ' 7-Feb-2007: Do not process other add-ins as it may cause them to fail
    If wb.IsAddin Then Exit Sub
    
    If wb.name = "ARulesXL Trace.xls" Then Exit Sub
    If wb.name = "ARulesXL.xla" Then Exit Sub


    If wb.name = "arulesxl trace.txt" Then
        Dim wsTrace As Worksheet
        Set wsTrace = Application.ActiveSheet

        ' Change here and in trace_format_sheet in Trace
        wsTrace.Columns("A").ColumnWidth = wsTrace.StandardWidth * 2
        wsTrace.Columns("B").ColumnWidth = wsTrace.StandardWidth * 2
        wsTrace.Columns("C").ColumnWidth = wsTrace.StandardWidth * 2
        wsTrace.Columns("D").ColumnWidth = wsTrace.StandardWidth * 2
        wsTrace.Columns("E").ColumnWidth = wsTrace.StandardWidth * 2
        wsTrace.Columns("F").ColumnWidth = wsTrace.StandardWidth * 4

        wb.Saved = True
    End If

    If ARulesActive Then
'MsgBox "activate " & wb.name & " initializing rules"
        ' Unload the current axl
' just treat runtime as any other ARulesXL, stop it from updating later
'        If Edition = "Runtime" Then
'            Call UnloadAXL
'        End If
    
        ' Save as doesn't activate the new workbook, so we can't close the old one
        was_saved = wb.Saved

        CloseRuleSets
        bRulesInitialized = False
        RulesWorkbook = ""
        
        ' Load the new AXL
' nope don't, Runtime like anyother license for running rules
'        If Edition = "Runtime" Then
'            If wb.name <> "ARulesXL Trace.xls" And wb.name <> "ARulesXL.xla" Then
'                ' Replaces .xls or .xlsx with .axl
'                axl = Replace(UCase(wb.FullName), ".XLSX", ".AXL", 1, 1, vbTextCompare)
'                If InStr(1, axl, ".AXL", vbTextCompare) = 0 Then
'                    axl = Replace(UCase(wb.FullName), ".XLS", ".AXL", 1, 1, vbTextCompare)
'                End If
'
'                ' Load the AXL file if this is a real workbook (not Book1)
'                If InStr(1, UCase(wb.FullName), ".XLS", vbTextCompare) <> 0 Then
'                    LoadAXL (axl)
'                End If
'            End If
'        End If

        ' Initialize the rule sets
        Call InitializeRuleSets
        wb.Saved = was_saved   ' we haven't really changed anything yet
    End If
    
Convert:
    ' Convert Sharepoint workbooks back. Must be after InitializeRuleSets
    On Error GoTo SkipConvert
    If Names("arxlType").value Like "*Sharepoint*" Then
        calc = Application.Calculation
        Application.Calculation = xlCalculationManual
        ConvertRFunctions ("")
        Application.Calculation = calc
        Names("arxlType").RefersTo = "Standard"
    End If

SkipConvert:
    On Error GoTo 0
    
'MsgBox "activate " & Wb.Name & " initialized rules"
End Sub

Sub LoadAXL(axl As String)
    Dim sheet As Worksheet
    Dim axlbytes() As Byte
    Dim size As Long
    Dim t As Long
    Dim ptr As Long
    Dim s As String

    ' Unload it if its already loaded
    If axlName <> "" Then
        Call UnloadAXL
    End If
        
    ' Look for the file in our spreadsheet
    On Error Resume Next
    Set sheet = Worksheets("ARulesXL Binary")
    On Error GoTo ErrorCatch
    
    ' If it does not exist, find it externally
    If sheet Is Nothing Then
        On Error GoTo ErrorCatch
        ' Identical pathnames needed for load and unload
        If Not ExecStrLS(t, "load('" + tilt_slashes(axl) + "')") Then GoTo ErrorCatch
    Else
        axlbytes = LoadEmbeddedFile("ARulesXL Binary", size)
        ' Identical pathnames needed for load and unload
'        Call LoadFromMemoryLS(tilt_slashes(axl), size, axlbytes)
        ptr = VarPtr(axlbytes(0))
        s = "load(memory, '" + tilt_slashes(axl) + "', " + CStr(size) + ", " + CStr(ptr) + ")"
        If Not ExecStrLS(t, s) Then GoTo ErrorCatch
    End If
    Call ExecStrLS(t, "check_license")
    
    ' Save the name to unload
    axlName = axl
    Exit Sub
    
ErrorCatch:
    MsgBox prompt:=GetText("cannot_find_axl(`" + tilt_slashes(axl) + "`)") & " / " & Err.Description, title:="ARulesXL"
End Sub

Sub UnloadAXL()
    Dim t As Long
    
    If axlName = "" Then Exit Sub
    On Error GoTo ErrorCatch
    ' Identical pathnames needed for load and unload
    If Not ExecStrLS(t, "unload('" + tilt_slashes(axlName) + "')") Then GoTo ErrorCatch
    
    ' Save the name to flag its gone
    axlName = ""
    Exit Sub
ErrorCatch:
    MsgBox prompt:=GetText("cannot_unload_axl(`" + tilt_slashes(axlName) + "`)") & " / " & Err.Description, title:="ARulesXL"
End Sub

Private Sub app_WorkbookBeforeClose(ByVal wb As Workbook, Cancel As Boolean)

'MsgBox "WorkbookBeforeClose " & Wb.name
'    If Cancel Then Exit Sub
Dim rc As Integer

    If wb.name = "ARulesXL Trace.xls" Then
        If ARulesActive Then EndTrace
        wb.Saved = False
'        Wb.Close savechanges:=True
        CommandBars("ARulesXL Trace").Visible = False
        Exit Sub
    End If

    If wb.name = "ARulesXL.xla" Then Exit Sub

'    rc = MsgBox("Save changes? " & Wb.name, vbYesNoCancel)
'    Select Case (rc)
'        Case vbYes
'            Wb.Close savechanges:=True
'        Case vbNo
'            Wb.Close savechanges:=False
'        Case vbCancel
'            Cancel = True
'    End Select

'    ' save as doesn't activate the new workbook, so we can't close the old one
'    If Not Cancel Then
'        CloseRuleSets
'        bRulesInitialized = False
'        RulesWorkbook = ""
'    End If
'

End Sub

Private Sub app_WorkbookBeforeSave(ByVal wb As Workbook, ByVal SaveAsUI As Boolean, Cancel As Boolean)
'MsgBox "WorkbookBeforeSave " & Wb.name & " saveas: " & SaveAsUI
'    If SaveAsUI = True Then
'
'    End If

' 21-Feb-2007: This does not work, and if it did, it would break all links.
'    wb.SaveLinkValues = False
    
' 20-Feb-2007: The following code breaks all the links in the spreadsheet.
' Unfortunately it replaces the cell values with the result of the formula.
'    Dim links As Variant
'    Dim i As Integer
'    Dim s As String
   
'    links = wb.LinkSources(xlExcelLinks)
'    If Not IsEmpty(links) Then
'    For i = 1 To UBound(links)
'        MsgBox "Link " & i & ":" & Chr(13) & links(i)
'        s = links(i)
'        If UCase$(Right$(s, 12)) = "ARULESXL.XLA" Then
'            Call wb.BreakLink(links(i), xlLinkTypeExcelLinks)
'        End If
'    Next i
'End If


End Sub


Private Sub app_WorkbookOpen(ByVal wb As Workbook)

'MsgBox "WorkbookOpen " & Wb.name

End Sub

