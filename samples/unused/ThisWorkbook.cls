VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "ThisWorkbook"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = True
Option Explicit



Private Sub AddControls()
    Dim cb As CommandBar
    Dim PeopleMenu As CommandBarControl
    Dim iWindow As Integer
    
    Set cb = Application.CommandBars("Worksheet Menu Bar")
    iWindow = cb.Controls("Window").Index
    Set PeopleMenu = cb.Controls.Add(Type:=msoControlPopup, Before:=iWindow)
    
    With PeopleMenu
        .Caption = "&Budgets"
        With .Controls.Add(Type:=msoControlButton)
            .Caption = "&Analyze"
            .OnAction = "Analyze"
        End With
    End With
End Sub

Private Sub RemoveControls()
    Dim cb As CommandBar
    Dim i As Long
    
    On Error Resume Next
    Set cb = Application.CommandBars("Worksheet Menu Bar")
    For i = cb.Controls.Count To 1 Step -1
        If cb.Controls(i).Caption = "&Budgets" Then
            cb.Controls(i).Delete
        End If
    Next i

End Sub

Private Sub Workbook_BeforeClose(Cancel As Boolean)
    Call RemoveControls
End Sub

Private Sub Workbook_Open()
    Call AddControls
End Sub



