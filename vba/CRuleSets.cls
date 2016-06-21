VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "CRuleSets"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private clRuleSets As Collection
Private rsDeleted As Boolean

Public Sub Initialize()
' RuleSets are stored persistently in Names.  There is a name
' for each range associated with a rule set, and a name "RuleSets" which
' is an array of the range / rule set names.
'
' That's how it used to be.  We need to get that old RuleSets way
' and convert it to the new way, which is cell(1,1) has "Rule Set ..."

Dim aRuleSetNames As Variant
Dim rsName As String
Dim i As Integer
Dim irs As CRuleSet
Dim strRuleSets As String
Dim aname As name
Dim aRange As Range
Dim iRuleSet As CRuleSet
Dim ws As Worksheet

'    bRulesInitialized = False

    rsDeleted = False
    Set clRuleSets = New Collection
    If ExistsRuleSets Then
        strRuleSets = ActiveWorkbook.Names("RuleSets").value
        On Error Resume Next
        aRuleSetNames = Evaluate(strRuleSets)
        If Err.Number <> 0 Or VarType(aRuleSetNames) = vbError Then
            MsgBox prompt:="Rule Sets corrupted, recreate each rule set", title:="ARulesXL"
            Exit Sub
        End If
        On Error GoTo 0
        For i = LBound(aRuleSetNames) To UBound(aRuleSetNames)
            rsName = aRuleSetNames(i)
            Set iRuleSet = New CRuleSet
            iRuleSet.InitializeVeryOld name:=rsName
            Call clRuleSets.Add(iRuleSet)
        Next i
        ActiveWorkbook.Names("RuleSets").Delete
    Else
        For Each aname In Names
            If Right(aname.name, 5) = "_arxl" Then
                Set iRuleSet = New CRuleSet
                rsName = Evaluate(aname.RefersTo)
                iRuleSet.InitializeOld name:=rsName
                Call clRuleSets.Add(iRuleSet)
            End If
        Next aname
    End If
    
    UpdateMaybe
    LoadAll
End Sub
Public Sub UpdateMaybe()
Dim aname As name
Dim x As String
Dim mbret As Integer
Dim Version As String
Dim tf As Boolean
Dim term As Long
Dim oldcalc As Integer

    ' First see if we've set the version, which we only do after 3.0
    On Error Resume Next
    x = Names("arxlVersion").name
    If Err.Number = 0 Then Exit Sub
    
    ' Next see if there are any rulesets in the workbook
    For Each aname In Names
        If Right(aname.name, 5) = "_arxl" Then GoTo FoundOne
    Next aname
    
    LoadAll
    Exit Sub
    
FoundOne:
    
    mbret = MsgBox("Update ARulesXL RuleSets and Queries?", vbYesNo, "ARulesXL")
    If mbret = vbNo Then Exit Sub

'    Call InitializeRuleSets
    oldcalc = Application.Calculation
    Application.Calculation = xlCalculationManual
    
    UpdateRuleSets
    LoadAll
    UpdateRQueries
    
    tf = ExecStrLS(term, "arules_build(?v)")
'    If tf Then
        Version = GetStrArgLS(term, 1)
        Names.Add name:="arxlVersion", RefersTo:=Version
'    End If
    
    Application.Calculation = oldcalc
    
End Sub
Public Sub LoadAll()
Dim irs As CRuleSet

    For Each irs In clRuleSets
        Application.StatusBar = irs.GetName
        irs.LoadRulesInitial
        irs.NormalStyle
    Next irs

End Sub

Public Sub CloseAll()
Dim irs As CRuleSet

    If Not EndTrace Then Exit Sub
    If rsDeleted Then doDeleted
    
    For Each irs In clRuleSets
        irs.DeleteLoadedRules
    Next irs
    
End Sub

Public Sub NewRuleSet()
Dim rsName As String
Dim rsRange As Range
Dim fNewCRS As FNewRuleSet
Dim wsName As String
Dim iRuleSet As CRuleSet
Dim r As Range
Dim term As Long
Dim Version As String
Dim tf As Boolean

'    On Error GoTo catch
    If Not EndTrace Then Exit Sub
    If rsDeleted Then doDeleted
    
    Set fNewCRS = New FNewRuleSet
'    fNewCRS.lbName = GetText("rule_set_name")
'    fNewCRS.lbRange = GetText("rule_set_range")
    
showloop:
    wsName = Selection.Worksheet.name
    If InStr(wsName, " ") <> 0 Then
        wsName = "'" & wsName & "'"
    End If
    fNewCRS.rfRange = wsName & "!" & Selection.Address
    fNewCRS.tbName = Selection.Cells(1, 1)
    fNewCRS.Show
    If fNewCRS.Cancelled = True Then Exit Sub
    
'    If Not GetRuleSet(fNewCRS.tbName) Is Nothing Then
'        Call MsgBox(GetText("rule_set_exists(" & fNewCRS.tbName & ")"), vbOKOnly, "ARulesXL")
'        GoTo showloop
'    End If
    
'    Set r = Range(fNewCRS.rfRange)
'    r.Select
'
'    If OverlapRuleSet(fNewCRS.rfRange) Then
'        Call MsgBox(GetText("range_overlaps"), vbOKOnly, "ARulesXL")
'        GoTo showloop
'    End If
    If Not legal_ruleset_name(fNewCRS.tbName) Then GoTo showloop
    
    Set iRuleSet = New CRuleSet
    iRuleSet.InitializeNew name:=fNewCRS.tbName, Address:=fNewCRS.rfRange
    Call clRuleSets.Add(iRuleSet)
    
    ' Set the version, Add replaces if it already exists
    tf = ExecStrLS(term, "arules_build(?v)")
'    If tf Then
        Version = GetStrArgLS(term, 1)
        Names.Add name:="arxlVersion", RefersTo:=Version
'    End If
    
    iRuleSet.LoadRules
'    Exit Sub
'catch:
'    DealWithException ("NewRuleSet")
End Sub


Public Function GetRuleSet(name As String) As CRuleSet
Dim irs As CRuleSet

    If rsDeleted Then doDeleted
    
    For Each irs In clRuleSets
        If irs.GetName = name Then
            Set GetRuleSet = irs
            Exit Function
        End If
    Next irs
'    MsgBox GetText("rule_set_not_found") & name
    Set GetRuleSet = Nothing
    
End Function

Public Function FindRuleSetName(c As Range) As String
' the way the function should be, except that RCell needs this
' and Excel starts recalculation before we can set up the rule
' set collection.  So the old way, below, sigh, is better. No, don't
' let RCell go until rules are initialized.

Dim irs As CRuleSet
        
    If rsDeleted Then doDeleted
    
    For Each irs In clRuleSets
        If irs.InRange(c) Then
            FindRuleSetName = irs.GetName
            Exit Function
        End If
    Next irs
    
    FindRuleSetName = "not found"
        
End Function
Public Function FindRuleSet(c As Range) As CRuleSet
' the way the function should be, except that RCell needs this
' and Excel starts recalculation before we can set up the rule
' set collection.  So the old way, below, sigh, is better. No, don't
' let RCell go until rules are initialized.

Dim irs As CRuleSet
        
    If rsDeleted Then doDeleted
    
    For Each irs In clRuleSets
        If irs.InRange(c) Then
            Set FindRuleSet = irs
            Exit Function
        End If
    Next irs
    
    Set FindRuleSet = Nothing
        
End Function
Public Sub CheckDeletedSheets()
' Sigh no way to tell when a sheet is deleted so periodically
' just walk through all the rule sets to see if any sheets disappeared
' so we can kill the rulesets that were on it.  Get other deleted
' rule sets as well on the way.
Dim i As Integer

    For i = clRuleSets.Count To 1 Step -1
        If clRuleSets.item(i).isSheetDeleted Then
            clRuleSets.item(i).DeleteRSNames
            clRuleSets.Remove (i)
        ElseIf clRuleSets.item(i).GetName = "deleted" Then
            clRuleSets.Remove (i)
        End If
    Next i
    rsDeleted = False
    
End Sub
Public Sub setDeleted()
    rsDeleted = True
End Sub

Private Sub doDeleted()
Dim i As Integer
    For i = clRuleSets.Count To 1 Step -1
        If clRuleSets.item(i).GetName = "deleted" Then clRuleSets.Remove (i)
    Next i
    rsDeleted = False
End Sub
Public Function OverlapRuleSet(radd As String) As Boolean
' returns true if the range overlaps an existing rule set

Dim irs As CRuleSet
Dim r As Range
    
    If rsDeleted Then doDeleted
    
    OverlapRuleSet = False
    Set r = Range(radd)
    For Each irs In clRuleSets
        If irs.OverlapRange(r) Then
            OverlapRuleSet = True
            Exit Function
        End If
    Next irs
        
End Function

Public Sub DirtyAll()
' After a change in a rule set, it's necessary to flag all the ranges as dirty
' so any and all rquerys are redone.  This is because there might be dependencies
' between rule sets.
Dim rs As CRuleSet
    If rsDeleted Then doDeleted
    For Each rs In clRuleSets
'        Application.StatusBar = "Marking " & rs.GetName
        rs.MakeDirty
    Next rs
End Sub
Public Sub ExportRuleSetNames()
    Dim i As Integer
    Dim term As Long
    Dim tf As Boolean
    Dim ruleset_return As String
    Dim rs As CRuleSet

    On Error GoTo catch
    If rsDeleted Then doDeleted
    
    tf = ExecStrLS(term, "amzi_system:user_listing(data/5)")
    If tf = False Then
        MsgBox "Error in user_listing(data/5)"
        Exit Sub
    End If
'    For Each rs In clRuleSets
''    For i = LBound(RuleSetNames) To UBound(RuleSetNames)
'        If rs.GetName <> "deleted" Then
'            rs.LoadRules
'            tf = ExecStrLS(term, "amzi_system:module_listing('" & rs.GetName & "')")
'            If tf = False Then
'                MsgBox prompt:=GetText("export_error(`" & rs.GetName & "`)"), title:="ARulesXL"
'                Exit Sub
'            End If
'        End If
'    Next rs

    For Each rs In clRuleSets
'    For i = LBound(RuleSetNames) To UBound(RuleSetNames)
        If rs.GetName <> "deleted" Then
            rs.LoadRules
            tf = ExecStrLS(term, "arxl_export_ruleset('" & rs.GetName & "')")
            If tf = False Then
                MsgBox prompt:=GetText("export_error(`" & rs.GetName & "`)"), title:="ARulesXL"
                Exit Sub
            End If
        End If
    Next rs
    
    Exit Sub
catch:
    DealWithException "ExportRuleSetNames"
End Sub

Sub LoadActives()
Dim rs As CRuleSet
    If rsDeleted Then doDeleted
    For Each rs In clRuleSets
        rs.LoadActive
    Next rs
End Sub
Sub ReLoadAll()
Dim rs As CRuleSet
    If rsDeleted Then doDeleted
    For Each rs In clRuleSets
        rs.LoadRules
    Next rs
End Sub
Sub UpdateRuleSets()
Dim rs As CRuleSet

    For Each rs In clRuleSets
        rs.UpdateRS
    Next rs
End Sub
Sub DeleteRuleSet()
Dim rsName As String
Dim deletestr As String
Dim mbret As Integer

    rsName = FindRuleSetName(ActiveCell)
    If rsName = "not found" Then
        MsgBox prompt:=GetText("select_delete_cell"), title:="ARulesXL"
    Else
        deletestr = GetText("delete_ok(" & rsName & ")")
        mbret = MsgBox(deletestr, vbYesNo, "ARulesXL")
        If mbret = vbYes Then
            GetRuleSet(rsName).DeleteRS
        End If
    End If
    
End Sub

