Attribute VB_Name = "Trace"
Option Explicit
Option Private Module
Option Compare Text

'Dim call_stack_row As Integer
'Dim call_stack_col As Integer
Dim known_row As Integer
Dim known_col As Integer
Dim known_sub_cols As Integer
Dim trace_row As Integer
Dim trace_col As Integer
Dim trace_sub_cols As Integer
Dim starting_row As Integer
Dim heading_row As Integer
Dim query_row As Integer
Dim query_col As Integer
Dim message_row As Integer
Dim message_col As Integer
Dim wsTrace As Worksheet
Dim wbTrace As Workbook
Dim wbActive As Workbook
Dim query_cell As Range
Dim trace_message As String
Dim previous_message As String
Public windowTrace_height As Double
Public windowTrace_width As Double
Public windowTrace_left As Double
Public windowTrace_top As Double
Dim trace_made As Boolean
Public Tracing As Boolean
Private msgStartTrace As String
Private msgStopTrace As String
Private msgContinueTrace As String
Private msgTraceRun As String


Public Sub StartTrace()
Dim cell_str As String

'    Set theAppEvents.app = Application
    On Error GoTo catch
    If Not EndTrace Then Exit Sub
    
    ' First check that the active cell is pointing to a query
    If Not ActiveCell.HasFormula Then
        MsgBox GetText("trace_error_select_query_cell")
        Exit Sub
    Else
        If Not ActiveCell.Formula Like "*RQuery(*" Then
            MsgBox GetText("trace_error_select_query_cell")
            Exit Sub
        End If
    End If
    
    'fill in various text messages
    msgStartTrace = GetText("trace_start_msg")
    msgStopTrace = GetText("trace_stop_msg")
    msgContinueTrace = GetText("trace_continue_msg")
    msgTraceRun = GetText("trace_run_msg")
    
    ' Grab it now before initialize makes something else active or selected
    Set query_cell = ActiveCell
    
    cell_str = ActiveCell.Formula
    
    Tracing = True
    
    InitializeTrace
    RunTrace (cell_str)
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("StartTrace"), title:="ARulesXL"
End Sub

Private Sub InitializeTrace()

Dim ws As Worksheet
Dim wsLast As Worksheet
Dim wb As Workbook
Dim tracewb_file As String
Dim tracewb_path As String
Dim height As Double
Dim width As Double
Dim top As Double
Dim left As Double
Dim tf As Boolean
Dim term As Long

   
    Set wbActive = ActiveWorkbook
    tracewb_file = wbActive.path & "\ARulesXL Trace.xls"
    tracewb_path = tilt_slashes(wbActive.path)
    
    tf = ExecStrLS(term, "set_trace_path(`" & tracewb_path & "`)")
    
    Application.DisplayAlerts = False
    On Error Resume Next
        
'    wbTrace.Close savechanges:=False
    wbTrace.Close
    
    Set wbTrace = Nothing
    On Error GoTo 0
    
    If FileExists(tracewb_file) Then
        Set wbTrace = Workbooks.Open(Filename:=tracewb_file)
    Else
        Set wbTrace = Application.Workbooks.Add
    End If
    
    wbTrace.SaveAs Filename:=tracewb_file, FileFormat:=xlWorkbookNormal, AddToMru:=False
    If trace_made And windowTrace_width > 0 Then
        wbTrace.Windows(1).width = windowTrace_width
        wbTrace.Windows(1).height = windowTrace_height
        wbTrace.Windows(1).top = windowTrace_top
        wbTrace.Windows(1).left = windowTrace_left
    Else
        trace_made = True
    End If
    Application.DisplayAlerts = True
    Set wsTrace = wbTrace.Worksheets(1)
    
    trace_message = msgStartTrace
    previous_message = ""
    trace_format_sheet
    
'    query_cell.Activate

'    Dim cb As CommandBar
'    For Each cb In CommandBars
'        MsgBox cb.name
'    Next cb
    
        
    ' Wipe out the old toolbar, is this really necessary?
    ' No unless we want to reset the controls, in which case uncomment
    ' these lines.
'    On Error Resume Next
'    CommandBars("ARulesXL Trace").Delete
'    On Error GoTo 0
        
'    If CommandBars("ARulesXL Trace").FindControl Is Nothing Then
'    If CommandBars.FindControl(recursive:=True) Is Nothing Then
    If Not ControlBarExists("ARulesXL Trace") Then
        ' Create the trace toolbar
        ' see list of faceids on pg 540 of excel 2003 vba book
        With CommandBars.Add(name:="ARulesXL Trace")
            With .Controls.Add(Type:=msoControlButton)
                .caption = GetText("trace_step")
                .FaceId = 188
                .OnAction = "TraceStep"
            End With
            With .Controls.Add(Type:=msoControlButton)
                .caption = GetText("trace_skip")
                .FaceId = 194
                .OnAction = "TraceSkip"
            End With
            With .Controls.Add(Type:=msoControlButton)
                .caption = GetText("trace_run")
                .FaceId = 186
                .OnAction = "TraceRun"
            End With
            With .Controls.Add(Type:=msoControlButton)
                .caption = GetText("trace_end")
                .FaceId = 228
                .OnAction = "TraceQuit"
            End With
            With .Controls.Add(Type:=msoControlButton)
                .caption = GetText("trace_help")
                .FaceId = 984
                .OnAction = "TraceHelp"
            End With
        End With
    End If
    
    CommandBars("ARulesXL Trace").Visible = True
    
'    wbActive.Activate
End Sub
Private Sub TraceQuit()
    On Error GoTo catch
    PutActionLS "break"
    If PrologReady = False Then
        Beep
    Else
        trace_message = msgStopTrace
        TraceReport
    End If
    CommandBars("ARulesXL Trace").Visible = False
'    wbTrace.Saved = True
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("TraceQuit"), title:="ARulesXL"
End Sub
Private Sub TraceSkip()
    On Error GoTo catch
    PutActionLS "skip"
    If PrologReady = False Then
        Beep
    Else
        trace_message = msgContinueTrace
        ' TraceReport returns false when the query is complete
        If TraceReport <> True Then
            CommandBars("ARulesXL Trace").Visible = False
'            wbTrace.Saved = True
            Tracing = False
        End If
    End If
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("TraceSkip"), title:="ARulesXL"
End Sub
Private Sub TraceHelp()
    On Error GoTo catch
    Dim HelpFile As String

    HelpFile = GetHelpPath() & "reference\ref_debug.htm"
    VBShellExecute HelpFile
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("TraceHelp"), title:="ARulesXL"
End Sub


Private Sub TraceStep()
'    If GetActionStateLS = False Then
'        Beep
'    End If
'    trace_message = "Click 'step' to continue stepping; 'run' to run to completion; 'end' to stop trace"
'    ' TraceReport returns false when the query is complete
'    If TraceReport = True Then
'        PutActionLS "step"
'    Else
'        CommandBars("ARulesXL Trace").Visible = False
'        wbTrace.Saved = True
'    End If
    On Error GoTo catch
    PutActionLS "step"
    If PrologReady = False Then
        Beep
    Else
        trace_message = msgContinueTrace
        ' TraceReport returns false when the query is complete
        If TraceReport <> True Then
            CommandBars("ARulesXL Trace").Visible = False
'            wbTrace.Saved = True
        Tracing = False
        End If
    End If
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("TraceStep"), title:="ARulesXL"
End Sub
Private Sub TraceRun()
    On Error GoTo catch
    PutActionLS "run"
    If PrologReady = False Then
        trace_message = msgTraceRun
        Beep
    Else
        If TraceReport <> True Then
            CommandBars("ARulesXL Trace").Visible = False
'            wbTrace.Saved = True
        Tracing = False
        End If
    End If
    Exit Sub
catch:
    MsgBox prompt:=DealWithException("TraceRun"), title:="ARulesXL"
End Sub

Private Sub RunTrace(cell_str As String)
    Dim rule_set As Range
    Dim c As Range
    Dim rsName As String
    Dim query As String
    Dim temp As String
    Dim i As Integer
    Dim i1 As Integer
    Dim i2 As Integer
    Dim module As String
    Dim tf As Long
    Dim term As Long
    Dim pargs() As Variant
    Dim sarg As String
    Dim result As Variant
    ' following used for final explanation
    Dim rc As Integer
    Dim fact_term As Long
    Dim fact_str As String
    Dim mod_term As Long
    Dim mod_str As String
    Dim src_term As Long
    Dim src_str As String
    Dim cell As Range
    Dim val_term As Long
    Dim val_str As String
    Dim activecellstr As String
    
    wbActive.Activate
    ' First extract the rule set from the query
    temp = cell_str
    
'    query_cell.Activate
'    query_cell.Select
    
    'MsgBox "temp = " & temp
    i = InStr(temp, "rquery")
    temp = Mid(temp, i + 7)
    'MsgBox "i = " & i & " temp = " & temp
    i = InStr(temp, ",")
    rsName = left(temp, i - 1)
    rsName = Trim(rsName)
    'MsgBox "rsname = " & rsName
    If ExistsRuleSet(rsName) Then
        Set rule_set = Range(rsName)
    Else
        MsgBox "Rule set " & rsName & " not defined"
        Exit Sub
    End If
    
'    color_rule_sets 0
    
    ' then the query
    temp = Mid(temp, i + 1)
    i = InStr(temp, """")
    temp = Mid(temp, i + 1)
    i = InStr(temp, """")
    query = left(temp, i - 1)
    'MsgBox "query = " & query
    
    ' and finally the optional range parameters
    temp = Mid(temp, i)
    i1 = InStr(temp, ",")
    i = 0
    Do While i1 <> 0
        temp = LTrim(Mid(temp, i1 + 1))
        i2 = InStr(temp, ",")
        If i2 = 0 Then
            'i2 = InStr(temp, ")")
            i2 = InStrRev(temp, ")")
            i1 = 0
        Else
            i1 = i2
        End If
        i = i + 1
        ReDim Preserve pargs(1 To i)
        sarg = Mid(temp, 1, i2 - 1)
        'MsgBox "sarg = " & sarg
        'Set pargs(i) = Range(sarg)
        pargs(i) = Evaluate("=" & sarg)
    Loop
    
    If i = 0 Then ReDim pargs(0 To 0)
    
    'MsgBox "i = " & i
    If i <> 0 Then
        For i1 = LBound(pargs) To UBound(pargs)
            'MsgBox "parg " & i1 & " " & pargs(i1)
        Next i1
    End If

    '*******************************************************
    ' Start reasoning
    '
'MsgBox "about to doquery"
    result = doquery(rule_set, query, True, False, False, pargs)
'MsgBox "exiting doquery"
    '*******************************************************
    ' put the trace window on top
    wbTrace.Activate
    
    Exit Sub
End Sub
Private Function TraceReport() As Boolean
' returns false when no more to do
Dim term As Long
Dim s As String
Dim tf As Boolean
Dim list As Long
Dim head As String
Dim rc As Long
Dim argtype As Integer
Dim more As Boolean
Dim traceHeadings As Range
Dim knownHeadings As Range

'    On Error GoTo catch
    wsTrace.Activate
    wsTrace.Cells(1, 1).Select
    
    trace_reset
    more = True
        
'    tf = CallStrLS(term, "arxl_call_stack_item(?s)")
'    While tf = True
'        s = GetStrArgLS(term, 1)
'        call_stack_output (s)
'        tf = RedoLS
'    Wend

'    wsTrace.Cells(trace_row, trace_col) = "Trace"
'    wsTrace.Cells(trace_row, trace_col).Font.Bold = True
'    trace_row = trace_row + 1
'    wsTrace.Rows(trace_row).Font.Bold = True
    Set traceHeadings = wsTrace.Range(Cells(trace_row, trace_col), Cells(trace_row, trace_col + 4))
    traceHeadings.Borders(xlEdgeBottom).LineStyle = xlContinuous
    traceHeadings.Font.Bold = True
    wsTrace.Cells(trace_row, trace_col) = "Action"
    wsTrace.Cells(trace_row, trace_col + 1) = "Rule Set"
    wsTrace.Cells(trace_row, trace_col + 2) = "Object"
    wsTrace.Cells(trace_row, trace_col + 3) = "Value"
    wsTrace.Cells(trace_row, trace_col + 4) = "Active Rule"
    trace_row = trace_row + 1
    
    tf = CallStrLS(term, "arxl_trace_item(?s)")
    While tf = True
        s = GetStrArgLS(term, 1)
        trace_output (s)
        If s = "arules_trace_done" Then more = False
        If left(s, 5) = "Error" Then more = False
        tf = RedoLS
    Wend
    
    known_row = trace_row + 1
    known_col = trace_col
    
'    wsTrace.Cells(known_row, known_col) = "Known"
'    wsTrace.Cells(known_row, known_col).Font.Bold = True
'    known_row = known_row + 1
'    wsTrace.Rows(known_row).Font.Bold = True
    
    Set knownHeadings = wsTrace.Range(Cells(known_row, known_col), Cells(known_row, known_col + 4))
    knownHeadings.Borders(xlEdgeBottom).LineStyle = xlContinuous
    knownHeadings.Font.Bold = True
    
    wsTrace.Cells(known_row, known_col) = "Known"
    wsTrace.Cells(known_row, known_col + 1) = "Rule Set"
    wsTrace.Cells(known_row, known_col + 2) = "Object"
    wsTrace.Cells(known_row, known_col + 3) = "Value"
    wsTrace.Cells(known_row, known_col + 4) = "Source"
    known_row = known_row + 1
    
    tf = CallStrLS(term, "arxl_known_item(?s)")
    While tf = True
        s = GetStrArgLS(term, 1)
        known_output (s)
        tf = RedoLS
    Wend
    
    If more = False Then
        trace_message = "Trace finished"
        set_message
    End If
    
    TraceReport = more
    Exit Function
   
End Function


Private Sub trace_reset()
Dim last_cell As Range
    
'    call_stack_row = starting_row
    known_row = starting_row
    trace_row = starting_row
    
    Set last_cell = wsTrace.Cells.SpecialCells(xlCellTypeLastCell)
    If last_cell.row <= starting_row Then Set last_cell = wsTrace.Cells(starting_row, last_cell.Column)
    Range(wsTrace.Cells(starting_row, 1), last_cell).ClearContents
    Range(wsTrace.Cells(starting_row, 1), last_cell).ClearComments
    Range(wsTrace.Cells(starting_row, 1), last_cell).Font.Bold = False
    Range(wsTrace.Cells(starting_row, 1), last_cell).Font.Underline = False
    Range(wsTrace.Cells(starting_row, 1), last_cell).Font.Color = 0
    Range(wsTrace.Cells(starting_row, 1), last_cell).Borders.LineStyle = xlLineStyleNone
    
    set_message
    
End Sub
Private Sub set_message()
    wsTrace.Rows(message_row).WrapText = False
    wsTrace.Cells(message_row, message_col) = trace_message
    If trace_message = previous_message Then
        wsTrace.Rows(message_row).Font.Color = vbBlack
        wsTrace.Rows(message_row).Font.Bold = False
    Else
        wsTrace.Rows(message_row).Font.Color = vbRed
        wsTrace.Rows(message_row).Font.Bold = True
        previous_message = trace_message
    End If

End Sub
Private Sub trace_format_sheet()
Dim i As Integer
Dim heading As Range

    wsTrace.Cells.ClearComments
    wsTrace.Cells.ClearNotes
    wsTrace.Cells.ClearContents
    wsTrace.Cells.WrapText = True
    wsTrace.Cells.Font.Bold = False
    wsTrace.Cells.HorizontalAlignment = xlLeft
    wsTrace.Cells.Borders.LineStyle = xlLineStyleNone
    
'    wsTrace.Cells(2, 2) = "ARulesXL Trace"
'    wsTrace.Cells(2, 2).Font.Bold = True
'    wsTrace.Rows(2).WrapText = False
'    wsTrace.Cells(2, 2).Font.Size = 16
'    wsTrace.Rows(3).WrapText = False
'    wsTrace.Cells(3, 2) = trace_message
    
    query_row = 1
    query_col = 2
    wsTrace.Rows(query_row).WrapText = False
    wsTrace.Rows(query_row).Font.Bold = True
    wsTrace.Cells(query_row, query_col) = "ARulesXL Trace"
    message_row = 2
    message_col = 2
    wsTrace.Rows(message_row).WrapText = False
    wsTrace.Rows(message_row).Font.Bold = False
    set_message
    
'    wsTrace.Rows("2").Font.Bold = True
'    wsTrace.Rows("2").WrapText = False
'    wsTrace.Rows(4).Font.Bold = True
'    wsTrace.Rows(4).WrapText = False
    
'    call_stack_col = 2
'    known_col = 8
    trace_col = 2
    known_sub_cols = 4
    trace_sub_cols = 5
    
    heading_row = 4
    starting_row = heading_row
    
    ' Change here and in OpenTraceLog in Utilities
    If wsTrace.Columns("A").ColumnWidth = wsTrace.StandardWidth Then
        wsTrace.Columns("A").ColumnWidth = wsTrace.StandardWidth / 3
    End If
    If wsTrace.Columns("B").ColumnWidth = wsTrace.StandardWidth Then
        wsTrace.Columns("B").ColumnWidth = wsTrace.StandardWidth * 2
    End If
    If wsTrace.Columns("C").ColumnWidth = wsTrace.StandardWidth Then
        wsTrace.Columns("C").ColumnWidth = wsTrace.StandardWidth * 2
    End If
    If wsTrace.Columns("D").ColumnWidth = wsTrace.StandardWidth Then
        wsTrace.Columns("D").ColumnWidth = wsTrace.StandardWidth * 2
    End If
    If wsTrace.Columns("E").ColumnWidth = wsTrace.StandardWidth Then
        wsTrace.Columns("E").ColumnWidth = wsTrace.StandardWidth * 2
    End If
    If wsTrace.Columns("F").ColumnWidth = wsTrace.StandardWidth Then
        wsTrace.Columns("F").ColumnWidth = wsTrace.StandardWidth * 2
    End If
    If wsTrace.Columns("G").ColumnWidth = wsTrace.StandardWidth Then
        wsTrace.Columns("G").ColumnWidth = wsTrace.StandardWidth * 4
    End If

'    If wsTrace.Columns("H").ColumnWidth = wsTrace.StandardWidth Then
'        wsTrace.Columns("H").ColumnWidth = wsTrace.StandardWidth * 2
'    End If
'    If wsTrace.Columns("I").ColumnWidth = wsTrace.StandardWidth Then
'        wsTrace.Columns("I").ColumnWidth = wsTrace.StandardWidth * 2
'    End If
'    If wsTrace.Columns("J").ColumnWidth = wsTrace.StandardWidth Then
'        wsTrace.Columns("J").ColumnWidth = wsTrace.StandardWidth * 2
'    End If
'    If wsTrace.Columns("K").ColumnWidth = wsTrace.StandardWidth Then
'        wsTrace.Columns("K").ColumnWidth = wsTrace.StandardWidth * 2
'    End If
        
    ' remove all the old hyperlinks by removing the first
    ' one each time through the loop
'    For i = 1 To wsTrace.Hyperlinks.Count
'        wsTrace.Hyperlinks.Remove 1
'    Next i
        
'    Set heading = Range(wsTrace.Cells(heading_row, trace_col), wsTrace.Cells(heading_row, trace_col + trace_sub_cols - 1))
'    heading.Merge
'    heading.Font.Bold = True
'    heading.Font.Size = 12
'    heading = "Trace"
'    heading.HorizontalAlignment = xlCenter
'
'    Set heading = Range(wsTrace.Cells(heading_row, known_col), wsTrace.Cells(heading_row, known_col + known_sub_cols - 1))
'    heading.Merge
'    heading.Font.Bold = True
'    heading.Font.Size = 12
'    heading = "Known"
'    heading.HorizontalAlignment = xlCenter

End Sub
Private Sub make_link(r As Range)
Dim s As String
Dim j As Integer
Dim l As Integer
Dim wbname As String
Dim dot As Integer
Dim sh As String
Dim sr As String

' Get the rule from the user's workbook and put it next to
' the reference on the trace workbook

    s = r.value
    j = InStr(s, "rule(")
    If j = 0 Then Exit Sub
    l = Len(s)
    s = Mid(s, 6, l - 6)
    ' get the sheet and range
    j = InStr(s, "!")
    sh = left(s, j - 1)
    sr = Mid(s, j + 1)
    r = wbActive.Worksheets(sh).Range(sr)
    
' We were unable to figure out how to make this work
' when the workbook name had a space in it.  Using the
' full address sort of works, but puts Excel in a browse
' mode that is unattractive.  So this is not used anymore.
' The issue was workbook -> workbook links.  We wanted the trace
' workbook to link to the already opened user workbook using the
' [wbname] notation.  This is what doesn't work with spaces in
' the workbook name.
'    wsTrace.Hyperlinks.Add r, "", _
'        SubAddress:="[" & wbActive.name & "]" & s, _
'        TextToDisplay:=s
        
End Sub
'Private Sub call_stack_output(text As String)
'Dim r As Range
'    Set r = wsTrace.Cells(call_stack_row, call_stack_col)
'    wsTrace.Hyperlinks.Add r, "", SubAddress:=text, TextToDisplay:=text
'    call_stack_row = call_stack_row + 1
'End Sub
Private Sub known_output(Text As String)
Dim i As Integer
Dim j As Integer
Dim r As Range
Dim s As String
Dim l As Integer
Dim sh As String
Dim sr As String
Dim cell_value As String
Dim rwidth As Single
Dim rule_text As String

'    wsTrace.Cells(known_row, known_col) = text
    ' knowns start same as trace, skip one column though so start at 1
    
    wsTrace.Rows(known_row).Font.Color = vbBlue
    For i = 1 To known_sub_cols
        j = InStr(Text, ";")
        Set r = wsTrace.Cells(known_row, known_col + i)
        If j > 0 Then
            cell_value = Mid(Text, 1, j - 1)
            Text = Mid(Text, j + 1)
        Else
            cell_value = Text
        End If
        
        ' for the value column, we'll truncate
        ' the width is the width in units of a standard 0 character
        rwidth = r.ColumnWidth - 4
        rwidth = rwidth + 0.1 * rwidth
        If i = trace_sub_cols - 2 And Len(cell_value) > rwidth Then
            r.value = left(cell_value, rwidth) & "..."
            If left(cell_value, 1) <> "[" Then
                r.AddComment (WrappedString(cell_value, 80))
            Else
                r.AddComment (cell_value)
            End If
            r.Comment.Shape.TextFrame.AutoSize = True
        Else
            r.value = cell_value
        End If
        If i = known_sub_cols Then
            s = r.value
            j = InStr(s, "rule(")
            If j > 0 Then
                l = Len(s)
                s = Mid(s, 6, l - 6)
                r.value = s
                ' get the sheet and range
                j = InStr(s, "!")
                sh = left(s, j - 1)
                ' sheets need single quotes in ranges, but can't have them for identification, sigh
                If left(sh, 1) = "'" Then
                    l = Len(sh)
                    sh = Mid(sh, 2, l - 2)
                End If
                sr = Mid(s, j + 1)
                
' go to the rule set and see if this cell is in a decision table or not,
' if so, get the whole row to display
' OR... maybe dt rules should be flagged differently
                ' put the rule text in as a comment on the rule cell
                rule_text = wbActive.Worksheets(sh).Range(sr)
                r.AddComment (rule_text)
                r.Comment.Shape.TextFrame.AutoSize = True
                
                ' but make a copy of the start of it on the cell to the right as well
                rwidth = wsTrace.Columns("G").ColumnWidth - 4
                rwidth = rwidth + 0.1 * rwidth
                Set r = wsTrace.Cells(known_row, known_col + known_sub_cols + 1)
                If Len(rule_text) > rwidth Then
                    rule_text = left(rule_text, rwidth) & "..."
                End If
                r.value = rule_text
                r.WrapText = False
            End If
        End If
'        Call make_link(r)
    Next i
    known_row = known_row + 1
End Sub
Private Sub trace_output(Text As String)
Dim i As Integer
Dim j As Integer
Dim l As Integer
Dim r As Range
Dim s As String
Dim tf As Boolean
Dim term As Long
Dim error_message As String
Dim sh As String
Dim sr As String
Dim rule_col As Integer
Dim cell_value As String
Dim rwidth As Single
Dim rule_text As String
Dim rule_cell As Range
Dim irs As CRuleSet

    If InStr(Text, "arules_trace_done") Then
'        MsgBox "found done"
        Exit Sub
    End If
    
    If InStr(Text, "Query") = 1 Then
'        wsTrace.Rows(query_row).WrapText = False
        wsTrace.Cells(query_row, query_col) = Text
        Exit Sub
    End If
    
    wsTrace.Rows(trace_row).Font.ColorIndex = 5
    
    For i = 0 To trace_sub_cols - 1
        j = InStr(Text, ";")
        Set r = wsTrace.Cells(trace_row, trace_col + i)
        If j > 0 Then
            cell_value = Mid(Text, 1, j - 1)
            Text = Mid(Text, j + 1)
        Else
            cell_value = Text
        End If
        ' Excel appears to chop off a leading single quote
        If left(cell_value, 1) = "'" Then cell_value = " " & cell_value
        
        ' for the value column, we'll truncate
        ' the width is the width in units of a standard 0 character
        rwidth = r.ColumnWidth - 4
        rwidth = rwidth + 0.1 * rwidth
        If i = trace_sub_cols - 2 And Len(cell_value) > rwidth Then
            r.value = left(cell_value, rwidth) & "..."
            If left(cell_value, 1) <> "[" Then
                r.AddComment (WrappedString(cell_value, 80))
            Else
                r.AddComment (cell_value)
            End If
            r.Comment.Shape.TextFrame.AutoSize = True
        Else
            r.value = cell_value
        End If
        If i = 0 And r.value = "error" Then
            tf = ExecStrLS(term, "query_error(?msg)")
            If tf = True Then
                error_message = GetStrArgLS(term, 1)
            Else
                error_message = "Query failed for unknown reason"
            End If
            wsTrace.Rows(trace_row).WrapText = False
            r.value = "Error: " & error_message
            r.Font.ColorIndex = 3
            r.Font.Bold = True
            trace_row = trace_row + 1
            Exit Sub
        End If
        ' if it's the last column then maybe it's a rule and we'll grab the text for them
        If i = trace_sub_cols - 1 Then
            s = r.value
            j = InStr(s, "rule(")
            If j > 0 Then
                l = Len(s)
                s = Mid(s, 6, l - 6)
                r.value = s
                ' get the sheet and range
                j = InStr(s, "!")
                sh = left(s, j - 1)
                ' sheets need single quotes in ranges, but can't have them for identification, sigh
                If left(sh, 1) = "'" Then
                    l = Len(sh)
                    sh = Mid(sh, 2, l - 2)
                End If
                sr = Mid(s, j + 1)
'                rule_col = wbActive.Worksheets(sh).Range(sr).Column
'                If wbActive.Worksheets(sh).Columns(rule_col).ColumnWidth > wsTrace.Columns("G").ColumnWidth Then
'                    wsTrace.Columns("G").ColumnWidth = wbActive.Worksheets(sh).Columns(rule_col).ColumnWidth
'                End If
                
                ' put the rule text in as a comment on the rule cell
                
                Set rule_cell = wbActive.Worksheets(sh).Range(sr)
                Set irs = iCRuleSets.FindRuleSet(rule_cell)
                ' see if it's in a table first, if not, just take the rule
                If irs Is Nothing Then
                    rule_text = ""
                Else
                    rule_text = irs.GetTableRowText(rule_cell)
                End If
                If rule_text = "" Then rule_text = rule_cell.value
                
                r.AddComment (rule_text)
                r.Comment.Shape.TextFrame.AutoSize = True
                
                ' but make a copy of the start of it on the cell to the right as well
                rwidth = wsTrace.Columns("G").ColumnWidth - 4
                rwidth = rwidth + 0.1 * rwidth
                Set r = wsTrace.Cells(trace_row, trace_col + trace_sub_cols)
                If Len(rule_text) > rwidth Then
                    rule_text = left(rule_text, rwidth) & "..."
                End If
                r.value = rule_text
                r.WrapText = False
            End If
        End If

'        Call make_link(r)
    Next i
    wsTrace.Cells(trace_row, 1).Select
    trace_row = trace_row + 1
    
End Sub



Function ControlBarExists(ControlName As String) As Boolean
' returns TRUE if the sheet exists in the active workbook
    ControlBarExists = False
    On Error GoTo NoSuchControlBar
    If Len(CommandBars(ControlName).name) > 0 Then
        ControlBarExists = True
        Exit Function
    End If
NoSuchControlBar:
End Function

Function PrologReady() As Boolean
Dim i As Integer

    For i = 1 To 5
        If GetActionStateLS > 0 Then
            PrologReady = True
            Exit Function
        Else
            Application.Cursor = xlWait
            Sleep 1000
            Application.Cursor = xlDefault
        End If
    Next i
    
    PrologReady = False

End Function

Function PrologDone() As Boolean
Dim i As Integer
'Dim newHour As Variant
'Dim newMinute As Variant
'Dim newSecond As Variant
'Dim waitTime As Variant
'Dim andNow As Variant

'Application.Wait (Now + TimeValue("0:00:02"))

    For i = 1 To 5
        If GetActionStateLS = 2 Then
            PrologDone = True
            Exit Function
        Else
'            MsgBox "about to wait"
'            newHour = Hour(Now())
'            newMinute = Minute(Now())
'            newSecond = Second(Now()) + 10
'            waitTime = TimeSerial(newHour, newMinute, newSecond)
'            waitTime = Now() + TimeValue("0:00:10")
            Application.Cursor = xlWait
            Sleep 1000
            Application.Cursor = xlDefault
'            Application.Wait (waitTime)
'            andNow = Now()
'            Application.Wait (Now() + TimeValue("0:00:10"))
'            MsgBox "done waiting"
        End If
    Next i
    
    PrologDone = False

End Function


' These are all attempts to figure out how to get Excel to link to
' a workbook that has a space in the name.  Many fruitless hours, and
' no success.  Abandoning hyperlinks for now.

Sub diagnostic_links()
Dim wbname As String
Dim s As String
Dim suba As String
Dim addr As String
'    wbname = wbActive.name
    wbname = "'Beep Beep'.xls"
'    wbname = "'" & wbname & "'"
    s = "BBRules!A1"
    suba = "[" & wbname & "]" & s
'    suba = Replace(suba, " ", "%20")
'    suba = "'" & suba & "'"
'    addr = wbActive.FullName
    addr = ""
    wsTrace.Hyperlinks.Add wsTrace.Range("A1"), addr, _
        SubAddress:=suba, _
        TextToDisplay:=s

End Sub

Function EndTrace() As Boolean

    If Tracing Then
'        If vbYes = MsgBox("Tracing is still active, end?", vbYesNo) Then
        If ARulesActive = True Then
            PutActionLS "break"
        End If
        If PrologDone Then
            Tracing = False
            EndTrace = True
        Else
            MsgBox prompt:="Trace stuck, exit and restart Excel", title:="ARulesXL"
            EndTrace = False
        End If
'        End If
    Else
        EndTrace = True
    End If
        
End Function


