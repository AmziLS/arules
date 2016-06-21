VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} FNewRuleSet 
   Caption         =   "New Rule Set"
   ClientHeight    =   2430
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   6135
   OleObjectBlob   =   "FNewRuleSet.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "FNewRuleSet"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit
Public Cancelled As Boolean

Private Sub btnCancel_Click()
    Cancelled = True
    Me.Hide
End Sub

Private Sub btnHelp_Click()
    Dim HelpFile As String
    
    On Error GoTo catch
    HelpFile = GetHelpPath() & "reference\ref_menu_commands.htm"
    VBShellExecute HelpFile
    Exit Sub
catch:
    DealWithException ("HelpButton_Click")
End Sub

Private Sub btnOK_Click()
    Dim r As Range
    
    If Not iCRuleSets.GetRuleSet(tbName) Is Nothing Then
        Call MsgBox(GetText("rule_set_exists(" & tbName & ")"), vbOKOnly, "ARulesXL")
        Exit Sub
    End If
    
    If Not legal_ruleset_name(tbName) Then Exit Sub
    
    Set r = Range(rfRange)
    If r.Count < 2 Then
        MsgBox prompt:=GetText("one_cell_ruleset"), title:="ARulesXL"
        Exit Sub
    End If
    
    r.Select
    
    If iCRuleSets.OverlapRuleSet(rfRange) Then
        Call MsgBox(GetText("range_overlaps"), vbOKOnly, "ARulesXL")
        Exit Sub
    End If

    Cancelled = False
    Me.Hide
End Sub

Private Sub UserForm_Initialize()
    lbName = GetText("rule_set_name")
    lbRange = GetText("rule_set_range")
    lbHelp = GetText("rule_set_new_help")

End Sub

Private Sub UserForm_Terminate()
    Cancelled = True
    Me.Hide
End Sub
