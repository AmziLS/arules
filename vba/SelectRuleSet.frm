VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} SelectRuleSet 
   Caption         =   "Select Rule Set"
   ClientHeight    =   1440
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   3735
   OleObjectBlob   =   "SelectRuleSet.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "SelectRuleSet"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public Cancelled As Boolean

Private Sub Cancel_Click()
    Cancelled = True
    Me.Hide
End Sub

Private Sub OK_Click()
    Cancelled = False
    Me.Hide
End Sub

Private Sub UserForm_Initialize()

    SelectRuleSet.caption = GetText("select_rule_set_title")
    Label1.caption = GetText("select_rule_set_label")
    OK.caption = GetText("button_ok")
    Cancel.caption = GetText("button_cancel")

End Sub

Private Sub UserForm_Terminate()
    Cancelled = True
End Sub
