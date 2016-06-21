VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} AssistantText 
   Caption         =   "ARulesXL"
   ClientHeight    =   2730
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   5370
   OleObjectBlob   =   "AssistantText.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "AssistantText"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private registry_key As String

Public Sub Set_RegistryKey(name As String)
    registry_key = name
End Sub

Private Sub OKButton_Click()
    If DisplayCheckBox.value = True Then
        Call SetRegistryMessageIndicator(registry_key, True)
    End If
    
    Unload Me
End Sub

Private Sub UserForm_Initialize()
    DisplayCheckBox.caption = GetText("do_not_display_message_again")
    OKButton.caption = GetText("ok")
End Sub

