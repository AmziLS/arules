VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} OptionsForm 
   Caption         =   "ARulesXL Options"
   ClientHeight    =   3825
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   5430
   OleObjectBlob   =   "OptionsForm.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "OptionsForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim Refresh, style, SelectedStyle As String

Private Sub CancelButton_Click()
    Me.Hide
End Sub

Private Sub HelpButton_Click()
    Dim HelpFile As String
    
    On Error GoTo catch
    HelpFile = GetHelpPath() & "reference\ref_menu_commands.htm"
    VBShellExecute HelpFile
    Exit Sub
catch:
    DealWithException ("HelpButton_Click")
End Sub

Private Sub OKButton_Click()
    Call SaveOptionValues
    Me.Hide
End Sub

Private Sub SaveOptionValues()
    Dim StrArray(3) As String
    Dim value As String
    
    If RefreshAlwaysButton.value = True Then
        StrArray(0) = "refresh=A"
    Else
        StrArray(0) = "refresh=E"
    End If
    
    If RuleSetStyleListBox.ListIndex = -1 Then
        StrArray(1) = "style="
    Else
        StrArray(1) = "style=" & RuleSetStyleListBox.value
    End If
        
    If SelectedRuleSetStyleListBox.ListIndex = -1 Then
        StrArray(2) = "selectedstyle="
    Else
        StrArray(2) = "selectedstyle=" & SelectedRuleSetStyleListBox.value
    End If
    
    value = Join(StrArray, ";")
    ActiveWorkbook.Names.Add name:="ARulesXL_Options", RefersTo:=value
End Sub

Private Sub GetOptionValues()
    Dim StrArray As Variant
    Dim NameValue As String
    Dim i As Long
    Dim idx As Integer
    Dim opt, val As String
    
    ' Use split function to get the values from a name (join to put them back)
    On Error GoTo SetDefaults
    NameValue = ActiveWorkbook.Names("ARulesXL_Options").value
    StrArray = Split(NameValue, ";")
    For i = 0 To UBound(StrArray)
        idx = InStr(StrArray(i), "=", vbTextCompare)
        If idx <> 0 Then
            opt = LCase(Mid(StrArray(i), 0, idx - 1))
            val = Mid(StrArray(i), idx + 1)
            
            Select Case LCase(opt)
                Case "refresh"
                    Refresh = opt
                    
                Case "style"
                    If Len(opt) > 0 Then
                        style = opt
                    Else
                        Call CreateStyle("RuleSet")
                    End If
                
                Case "selectedstyle"
                    If Len(opt) > 0 Then
                        SelectedStyle = opt
                    Else
                        Call CreateSelectedStyle("RuleSetSelected")
                    End If
    
            End Select
        End If
    Next i
    Exit Sub
    
SetDefaults:
    Refresh = "A"
    style = "RuleSet"
    CreateStyle ("RuleSet")
    SelectedStyle = "RuleSetSelected"
    CreateSelectedStyle ("RuleSetSelected")
End Sub

Private Sub CreateStyle(name As String)
    Dim i As Integer
    Dim rStyle As style
    
    ' Exit if the style already exists
    For i = 1 To ActiveWorkbook.Styles.Count
        If ActiveWorkbook.Styles(i).name = name Then
            Exit Sub
        End If
    Next i

    With ActiveWorkbook.Styles
        Set rStyle = .Add(name:=name)
        ' NOTE!!! If you just set Borders.LineStyle you cannot access the property elsewhere
        rStyle.Borders.LineStyle = xlDouble
        rStyle.Borders.Color = vbBlack
        rStyle.Borders(xlEdgeTop).LineStyle = xlDouble
        rStyle.Borders(xlEdgeTop).Color = vbBlack
        rStyle.Borders(xlEdgeBottom).LineStyle = xlDouble
        rStyle.Borders(xlEdgeBottom).Color = vbBlack
        rStyle.Borders(xlEdgeRight).LineStyle = xlDouble
        rStyle.Borders(xlEdgeRight).Color = vbBlack
        rStyle.Borders(xlEdgeLeft).LineStyle = xlDouble
        rStyle.Borders(xlEdgeLeft).Color = vbBlack
        rStyle.Borders(xlDiagonalDown).LineStyle = xlLineStyleNone
        rStyle.Borders(xlDiagonalUp).LineStyle = xlLineStyleNone
        rStyle.IncludeBorder = True
        rStyle.IncludeAlignment = False
        rStyle.IncludeFont = False
        rStyle.IncludeNumber = False
        rStyle.IncludePatterns = False
        rStyle.IncludeProtection = False
        rStyle.IndentLevel = True
    End With
End Sub

Private Sub CreateSelectedStyle(name As String)
    Dim i As Integer
    Dim rStyle As style
    
    ' Exit if the style already exists
    For i = 1 To ActiveWorkbook.Styles.Count
        If ActiveWorkbook.Styles(i).name = name Then
            Exit Sub
        End If
    Next i

    With ActiveWorkbook.Styles
        Set rStyle = .Add(name:=name)
        ' NOTE!!! If you just set Borders.LineStyle you cannot access the property elsewhere
        rStyle.Borders.LineStyle = xlDouble
        rStyle.Borders.Color = vbBlue
        rStyle.Borders(xlEdgeTop).LineStyle = xlDouble
        rStyle.Borders(xlEdgeTop).Color = vbBlue
        rStyle.Borders(xlEdgeBottom).LineStyle = xlDouble
        rStyle.Borders(xlEdgeBottom).Color = vbBlue
        rStyle.Borders(xlEdgeRight).LineStyle = xlDouble
        rStyle.Borders(xlEdgeRight).Color = vbBlue
        rStyle.Borders(xlEdgeLeft).LineStyle = xlDouble
        rStyle.Borders(xlEdgeLeft).Color = vbBlue
'        rStyle.Borders(xlDiagonalDown).LineStyle = xlLineStyleNone
'        rStyle.Borders(xlDiagonalUp).LineStyle = xlLineStyleNone
        rStyle.IncludeBorder = True
        rStyle.IncludeAlignment = False
        rStyle.IncludeFont = False
        rStyle.IncludeNumber = False
        rStyle.IncludePatterns = False
        rStyle.IncludeProtection = True
        rStyle.IndentLevel = False
    End With
End Sub

Private Sub UserForm_Activate()
    Dim i As Integer
    
    ' Get all the option values, this might create default styles
    Call GetOptionValues
    
    ' Get all the current style names
    RuleSetStyleListBox.Clear
    SelectedRuleSetStyleListBox.Clear
    For i = 1 To ActiveWorkbook.Styles.Count
        RuleSetStyleListBox.AddItem (ActiveWorkbook.Styles(i).name)
        SelectedRuleSetStyleListBox.AddItem (ActiveWorkbook.Styles(i).name)
    Next i
    
    If Refresh = "A" Then
        RefreshAlwaysButton.value = True
        RefreshExitButton.value = False
    Else
        RefreshAlwaysButton.value = False
        RefreshExitButton.value = True
    End If
    
    RuleSetStyleListBox.value = style
    SelectedRuleSetStyleListBox.value = SelectedStyle

End Sub

Private Sub UserForm_Initialize()
    RefreshAlwaysLabel = GetText("options_refresh_rules_always")
    RefreshExitLabel = GetText("options_refresh_rules_exit")
    RuleSetStyleLabel = GetText("options_ruleset_style")
    SelectedRuleSetStyleLabel = GetText("options_selected_ruleset_style")
    OKButton.caption = GetText("button_ok")
    CancelButton.caption = GetText("button_cancel")
End Sub

Function GetRefresh() As String
    If Len(Refresh) = 0 Then
        Call GetOptionValues
    End If
    GetRefresh = Refresh
End Function

Function GetRuleSetStyle() As String
    If style = Null Or Len(style) = 0 Then
        Call GetOptionValues
    End If
    GetRuleSetStyle = style
End Function

Function GetSelectedRuleSetStyle() As String
    If SelectedStyle = Null Or Len(SelectedStyle) = 0 Then
        Call GetOptionValues
    End If
    GetSelectedRuleSetStyle = SelectedStyle
End Function

Public Sub CreateRuleSetStyles()
    Dim i As Integer
    Dim rStyle As style
    
    ' Exit if the style already exists
'    For i = 1 To ActiveWorkbook.Styles.Count
'        If ActiveWorkbook.Styles(i).name = "RuleSet Then
'            Exit Sub
'        End If
'    Next i

    Set rStyle = ActiveWorkbook.Styles.Add(name:="RuleSet")

    With ActiveWorkbook.Styles("RuleSet")

        ' NOTE!!! If you just set Borders.LineStyle you cannot access the property elsewhere
'        rStyle.Borders.LineStyle = xlDouble
'        rStyle.Borders.Color = vbGreen
'        rStyle.Borders.Weight = xlThin

        rStyle.Borders(xlEdgeTop).LineStyle = xlDouble
        rStyle.Borders(xlEdgeTop).Color = vbGreen
        rStyle.Borders(xlEdgeBottom).LineStyle = xlDouble
        rStyle.Borders(xlEdgeBottom).Color = vbGreen
        rStyle.Borders(xlEdgeRight).LineStyle = xlDouble
        rStyle.Borders(xlEdgeRight).Color = vbGreen
        rStyle.Borders(xlEdgeLeft).LineStyle = xlDouble
        rStyle.Borders(xlEdgeLeft).Color = vbGreen
'        rStyle.Borders(xlDiagonalDown).LineStyle = xlLineStyleNone
'        rStyle.Borders(xlDiagonalUp).LineStyle = xlLineStyleNone
'        rStyle.IncludeBorder = True
'        rStyle.IncludeAlignment = False
'        rStyle.IncludeFont = False
'        rStyle.IncludeNumber = False
'        rStyle.IncludePatterns = False
'        rStyle.IncludeProtection = False
'        rStyle.IndentLevel = True
    End With
    
    ' Exit if the style already exists
'    For i = 1 To ActiveWorkbook.Styles.Count
'        If ActiveWorkbook.Styles(i).name = name Then
'            Exit Sub
'        End If
'    Next i

    With ActiveWorkbook.Styles
        Set rStyle = .Add(name:="RuleSetSelected")
        ' NOTE!!! If you just set Borders.LineStyle you cannot access the property elsewhere
        rStyle.Borders.LineStyle = xlDash
        rStyle.Borders.Color = vbBlue
        rStyle.Borders.Weight = xlThin
        
'        rStyle.Borders(xlEdgeTop).LineStyle = xlDouble
'        rStyle.Borders(xlEdgeTop).Color = vbBlue
'        rStyle.Borders(xlEdgeBottom).LineStyle = xlDouble
'        rStyle.Borders(xlEdgeBottom).Color = vbBlue
'        rStyle.Borders(xlEdgeRight).LineStyle = xlDouble
'        rStyle.Borders(xlEdgeRight).Color = vbBlue
'        rStyle.Borders(xlEdgeLeft).LineStyle = xlDouble
'        rStyle.Borders(xlEdgeLeft).Color = vbBlue
'        rStyle.Borders(xlDiagonalDown).LineStyle = xlLineStyleNone
'        rStyle.Borders(xlDiagonalUp).LineStyle = xlLineStyleNone
'        rStyle.IncludeBorder = True
'        rStyle.IncludeAlignment = False
'        rStyle.IncludeFont = False
'        rStyle.IncludeNumber = False
'        rStyle.IncludePatterns = False
'        rStyle.IncludeProtection = True
'        rStyle.IndentLevel = False
    End With
End Sub

' Failed Code from CRuleSet/NormalStyle()
'    name = AOptions.GetRuleSetStyle()
'    l = rsRange.Application.ActiveWorkbook.Styles("RuleSet").Borders(xlEdgeTop).LineStyle
'    w = rsRange.Application.ActiveWorkbook.Styles("RuleSet").Borders(xlEdgeTop).Weight
'    c = rsRange.Application.ActiveWorkbook.Styles("RuleSet").Borders(xlEdgeTop).Color
'    l = rsRange.Application.ActiveWorkbook.Styles("RuleSet").Borders.LineStyle
'    w = rsRange.Application.ActiveWorkbook.Styles("RuleSet").Borders.Weight
'    c = rsRange.Application.ActiveWorkbook.Styles("RuleSet").Borders.Color
    ' NOTE!!! Cannot access Borders.LineStyle etc. directly, returns null values
    ' Must access one of the edges
'    rsRange.BorderAround LineStyle:=Application.ActiveWorkbook.Styles(name).Borders(xlEdgeTop).LineStyle, _
'        Weight:=Application.ActiveWorkbook.Styles(name).Borders(xlEdgeTop).Weight, _
'        Color:=Application.ActiveWorkbook.Styles(name).Borders(xlEdgeTop).Color
'    rsRange.BorderAround LineStyle:=Application.ActiveWorkbook.Styles(name).Borders.LineStyle, _
'        Weight:=Application.ActiveWorkbook.Styles(name).Borders.Weight, _
'        Color:=Application.ActiveWorkbook.Styles(name).Borders.Color

' Failed Code from CRuleSet/EditStyle()
'    name = AOptions.GetSelectedRuleSetStyle()
'    l = rsRange.Application.ActiveWorkbook.Styles("RuleSetSelected").Borders.LineStyle
'    w = rsRange.Application.ActiveWorkbook.Styles("RuleSetSelected").Borders.Weight
'    c = rsRange.Application.ActiveWorkbook.Styles("RuleSetSelected").Borders.Color
    ' NOTE!!! Cannot access Borders.LineStyle etc. directly, returns null values
    ' Must access one of the edges
'    rsRange.BorderAround LineStyle:=ActiveWorkbook.Styles(name).Borders.LineStyle, _
'        Weight:=ActiveWorkbook.Styles(name).Borders.Weight, _
'        Color:=ActiveWorkbook.Styles(name).Borders.Color

