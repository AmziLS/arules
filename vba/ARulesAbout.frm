VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} ARulesAbout 
   Caption         =   "ARulesXL"
   ClientHeight    =   4290
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   5265
   OleObjectBlob   =   "ARulesAbout.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "ARulesAbout"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub CloseButton_Click()
    Unload Me
End Sub

Private Sub ResetCellMenu_Click()
    Application.CommandBars("Cell").Reset
End Sub

Private Sub UserForm_Initialize()
    Version.caption = RAbout()
    UserInfo.caption = RUserInfo()
    Label1.caption = GetText("about_licensed_to")
    CloseButton.caption = GetText("ok")
    WebsiteButton.caption = GetText("visit_website")
End Sub

Private Sub WebsiteButton_Click()
    VBShellExecute "http://www.arulesxl.com/"
End Sub
