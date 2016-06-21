VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ARulesXL"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
' Error constants
Const eCannotLoadRules = 1 + vbObjectError + 512
Const eARulesError = 2 + vbObjectError + 512

Public Sub OpenRules(ByVal rulesetfile As String, ByVal arulespath As String)
    Dim term As Long
    Dim s As String
    Dim xplname As String
    Dim tf As Boolean
    
    'Set the logicserver error handling to raise errors
    ErrorMethod = 1
    
    'Get our logicserver and initialize it
    xplname = doubleSlashes(arulespath + "arulesrt.xpl")
    InitLS (xplname)
    
    'Try to load MySQL lsx
    On Error GoTo LoadXPL
    AddLSX (arulespath + "ar_mysqlrt.lsx")
    
LoadXPL:
    On Error GoTo 0
    
    'Load the arulesxl engine
    LoadLS (xplname)

    'Load the ruleset
    s = "load('" + doubleSlashes(rulesetfile) + "')"
    tf = ExecStrLS(term, s)
    If tf = False Then
        Err.Raise _
            Number:=eCannotLoadRules, _
            Source:=rulesetfile, _
            Description:="OpenRules / Unable to load ruleset"
    End If
End Sub

Public Sub CloseRules()
    Dim term As Long
    
    On Error GoTo CloseUp
    tf = ExecStrLS(term, "mysql_close")
CloseUp:
    Call CloseLS
End Sub

Public Function GetLS() As Long
    GetLS = EngineID
End Function

Public Sub ClearTable(ByVal ruleset As String, ByVal objectname As String)
    Dim term As Long
    Dim tf As Boolean
    
    tf = ExecStrLS(term, "arxl_initialize_table(" + ruleset + ", `" + objectname + "`)")
    If (tf = False) Then
        Err.Raise _
            Number:=eARulesError, _
            Source:=ruleset + ": " + objectname, _
            Description:=FormatARulesError()
    End If
End Sub

Public Sub ClearVector(ByVal ruleset As String, ByVal objectname As String)
    Dim term As Long
    Dim tf As Boolean
    
    tf = ExecStrLS(term, "arxl_initialize_table(" + ruleset + ", `" + objectname + "`)")
    If (tf = False) Then
        Err.Raise _
            Number:=eARulesError, _
            Source:=ruleset + ": " + objectname, _
            Description:=FormatARulesError()
    End If
End Sub

Public Sub AddToTable(ByVal ruleset As String, ByVal objectname As String, ByVal rowname As String, ByVal colname As String, ByVal value As String)
    Dim term As Long
    Dim tf As Boolean
    
    tf = ExecStrLS(term, "arxl_add_to_table(" + ruleset + ", `" + objectname + "`, `" + rowname + "`, `" + colname + "`, `" + value + "`)")
    If (tf = False) Then
        Err.Raise _
            Number:=eARulesError, _
            Source:=ruleset + ": " + objectname, _
            Description:=FormatARulesError()
    End If
End Sub

Public Sub AddToVector(ByVal ruleset As String, ByVal objectname As String, ByVal rowname As String, ByVal value As String)
    Dim term As Long
    Dim s As String
    Dim tf As Boolean
    
    s = "arxl_add_to_vector(" + ruleset + ", `" + objectname + "`, `" + rowname + "`, `" + value + "`)"
    tf = ExecStrLS(term, s)
    If (tf = False) Then
        Err.Raise _
            Number:=eARulesError, _
            Source:=ruleset + ": " + objectname, _
            Description:=FormatARulesError()
    End If
End Sub

Public Sub AddObject(ByVal ruleset As String, ByVal objectname As String, ByVal value As String)
    Dim term As Long
    Dim tf As Boolean
    
    tf = ExecStrLS(term, "arxl_add_data_cell(" + ruleset + ", `" + objectname + "`, `" + value + "`)")
    If (tf = False) Then
        Err.Raise _
            Number:=eARulesError, _
            Source:=ruleset + ": " + objectname, _
            Description:=FormatARulesError()
    End If
End Sub

Public Function QueryRules(ByVal ruleset As String, ByVal query As String) As String
    Dim term As Long
    Dim s As String
    Dim answer As Long
    Dim length As Long
    Dim tf As Boolean

    s = "arxl_query(" + ruleset + ", false, `" + query + "`, ?answer)"
    tf = ExecStrLS(term, s)
    If tf = False Then
        Err.Raise _
            Number:=eARulesError, _
            Source:=ruleset + ": " + query, _
            Description:=FormatARulesError()
    End If

    'The answer is a string in the 4th argument
    'Could be any type, turn it into a string
    Call GetArgLS(term, 4, bTERM, answer)
    length = StrTermLenLS(answer) + 1
    Call TermToStrLS(answer, s, length)
    QueryRules = s
End Function

Public Function QueryMore(ByVal ruleset As String, ByVal query As String) As String
    Dim term As Long
    Dim s As String
    Dim answer As Long
    Dim length As Long
    Dim tf As Boolean
    
    s = "arxl_query(" + ruleset + ", true, `" + query + "`, ?answer)"
    tf = ExecStrLS(term, s)
    If tf = False Then
        Err.Raise _
            Number:=eARulesError, _
            Source:=ruleset + ": " + query, _
            Description:=FormatARulesError()
    End If
    
    'The answer is a string in the 4th argument
    'Could be any type, turn it into a string
    Call GetArgLS(term, 4, bTERM, answer)
    length = StrTermLenLS(answer) + 1
    Call TermToStrLS(answer, s, length)
    QueryMore = s
End Function

'Display errors from ARulesXL
Private Function FormatARulesError() As String
    Dim term As Long
    Dim tf As Boolean
    
    tf = ExecStrLS(term, "query_error(?x)")
    If (tf) Then
       FormatARulesError = GetStrArgLS(term, 1)
    Else
       FormatARulesError = "Unknown ARulesXL error"
    End If
End Function

' This functions doubles backslashes in a pathname before it is passed
' to the Amzi! Logic Server
Private Function doubleSlashes(ByVal path As String) As String
    Dim i As Integer
    Dim s As String
    
    s = ""
    For i = 1 To Len(path)
        If (Mid(path, i, 1) = "\") Then
            s = s & "\\"
        Else
            s = s & Mid(path, i, 1)
        End If
    Next i
    doubleSlashes = s
End Function

