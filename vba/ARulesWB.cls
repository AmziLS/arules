VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "ARulesWB"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = True
' Set here and in Engine
'#Const runtime = False

Private Declare Function FreeLibrary Lib "kernel32" (ByVal hLibModule As Long) As Long
Private Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Long

Private Sub Workbook_BeforeClose(Cancel As Boolean)

    If ARulesActive Then
        EndTrace
        Call StopARules
        Call RemoveControls
    End If
    
End Sub

Private Sub Workbook_Open()

'    Call AddControls
    
    Set theAppEvents.app = Application
    If Not ARulesActive Then
        Call StartARules
        Call RemoveControls
        If ARulesActive Then Call AddControls
    End If
    
End Sub

Private Sub AddControls()
    Dim cb As CommandBar
    Dim ARulesMenu As CommandBarControl
    Dim iWindow As Integer
    Dim caption As String
'    Dim term As Long
'    Dim tf As Boolean

    On Error GoTo catch

    Set cb = Application.CommandBars("Worksheet Menu Bar")
    iWindow = cb.FindControl(ID:=30009).Index
'    iWindow = cb.Controls("Window").Index
    Set ARulesMenu = cb.Controls.Add(Type:=msoControlPopup, before:=iWindow)

    With ARulesMenu
        .caption = "&ARulesXL"
'        With .Controls.Add(Type:=msoControlButton)
'            .caption = "testing..."
'            .OnAction = "ATest"
'        End With
'#If runtime = False Then
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_new")
            .FaceId = 6862
            .style = msoButtonIconAndCaption
            .OnAction = "NewRuleSet"
            If Edition = "Runtime" Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_delete")
            .FaceId = 232
            .style = msoButtonIconAndCaption
            .OnAction = "DeleteRuleSet"
            If Edition = "Runtime" Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_load_actives")
            .FaceId = 232
            .style = msoButtonIconAndCaption
            .OnAction = "LoadActiveRuleSets"
            If Edition = "Runtime" Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_reload_all")
            .FaceId = 232
            .style = msoButtonIconAndCaption
            .OnAction = "ReLoadRuleSets"
            If Edition = "Runtime" Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_trace")
            .BeginGroup = True
            .FaceId = 940
            .style = msoButtonIconAndCaption
            .OnAction = "StartTrace"
            If Edition = "Runtime" Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_open_trace_log")
            .FaceId = 230
            .style = msoButtonIconAndCaption
            .OnAction = "OpenTraceLog"
            If Edition = "Runtime" Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_export")
            .BeginGroup = True
'            .Enabled = False
            .FaceId = 1104
            .style = msoButtonIconAndCaption
            .OnAction = "ExportRules"
            If (Edition = "Runtime" Or Edition = "Standard") Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_exportexcel")
'            .Enabled = False
            .FaceId = 142
            .style = msoButtonIconAndCaption
            .OnAction = "ExportExcelRuntime"
            If (Edition = "Runtime" Or Edition = "Standard") Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_sharepoint")
'            .Enabled = False
            .FaceId = 659
            .style = msoButtonIconAndCaption
            .OnAction = "ExportSharepoint"
            If (Edition = "Runtime" Or Edition = "Standard") Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_sharepoint_revert")
'            .Enabled = False
            .FaceId = 659
            .style = msoButtonIconAndCaption
            .OnAction = "RevertSharepoint"
            If (Edition = "Runtime" Or Edition = "Standard") Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_tutorial")
            .BeginGroup = True
            .FaceId = 3512
            .style = msoButtonIconAndCaption
            .OnAction = "ARulesTutorial"
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_help")
            .FaceId = 983
            .style = msoButtonIconAndCaption
            .OnAction = "ARulesHelp"
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_samples")
            .FaceId = 300
            .style = msoButtonIconAndCaption
            .OnAction = "ARulesSamples"
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_support")
            .FaceId = 2104
            .style = msoButtonIconAndCaption
            .OnAction = "ARulesSupport"
        End With
'        With .Controls.Add(Type:=msoControlButton)
'            .caption = GetText("menu_options")
'            .BeginGroup = True
'            .OnAction = "ARulesOptions"
'        End With
'#End If
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_about")
            .BeginGroup = True
            .FaceId = 931
            .style = msoButtonIconAndCaption
            .OnAction = "AboutARules"
        End With
'        With .Controls.Add(Type:=msoControlButton)
'            .caption = "Beep"
'            .OnAction = "DoDaBeep"
'        End With

    End With

    ' To reset the command bar (do NOT leave uncommented for release)
    ' Application.CommandBars("Cell").Reset

'#If runtime = False Then
    With Application.CommandBars("Cell")
        With .Controls.Add(Type:=msoControlButton)
            .BeginGroup = True
            .caption = GetText("menu_new")
            .FaceId = 6862
            .style = msoButtonIconAndCaption
            .OnAction = "NewRuleSet"
            If Edition = "Runtime" Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_delete")
            .FaceId = 232
            .style = msoButtonIconAndCaption
            .OnAction = "DeleteRuleSet"
            If Edition = "Runtime" Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_trace")
            .FaceId = 940
            .style = msoButtonIconAndCaption
            .OnAction = "StartTrace"
            If Edition = "Runtime" Then .Enabled = False
        End With
        With .Controls.Add(Type:=msoControlButton)
            .caption = GetText("menu_load_actives")
            .FaceId = 620
            .style = msoButtonIconAndCaption
            .OnAction = "LoadActiveRuleSets"
            If Edition = "Runtime" Then .Enabled = False
        End With
   End With
'#End If

    ' Enter descriptions for the Excel wizard
    ' Must do this for some reason
    ThisWorkbook.IsAddin = False
    Call AddFxHelp
    ThisWorkbook.IsAddin = True
    ThisWorkbook.Saved = True
    Exit Sub
catch:
    MsgBox "ARulesXL Initialization Error[" & Err.Number & "] " & Err.Description & " in " & Err.Source, vbCritical
    Err.Clear

End Sub
Private Sub RemoveControls()
    Dim cb As CommandBar
    Dim i As Long

    On Error Resume Next
    Set cb = Application.CommandBars("Worksheet Menu Bar")
    For i = cb.Controls.Count To 1 Step -1
        If cb.Controls(i).caption = "&ARulesXL" Then
            cb.Controls(i).Delete
        End If
'        If cb.Controls(i).caption = "" Then
'            cb.Controls(i).Delete
'        End If
    Next i

    Set cb = Application.CommandBars("Cell")
    For i = cb.Controls.Count To 1 Step -1
        If cb.Controls(i).caption = GetText("menu_new") Or _
           cb.Controls(i).caption = GetText("menu_trace") Or _
           cb.Controls(i).caption = GetText("menu_delete") Or _
           cb.Controls(i).caption = GetText("menu_load_actives") Then
            cb.Controls(i).Delete
        End If
    Next i

End Sub

Private Sub StartARules()
    Dim t As Long
    Dim tf As Boolean
    Dim cc As Integer
    
    On Error GoTo catch

    'MsgBox "Attempting to start ARules"
    If ARulesActive = False Then
        'MsgBox "Starting ARules"
'#If runtime Then
'        LoadLibrary ThisWorkbook.path & "\arulesrt.dll"
'        Call InitLS(ThisWorkbook.path & "\arulesrt.cfg")
'#Else
        'MsgBox "Loading arules.dll from " & ThisWorkbook.path
        LoadLibrary ThisWorkbook.path & "\arules.dll"
        Call InitLS(ThisWorkbook.path & "\arules.cfg")
'#End If
        
        On Error Resume Next
'#If runtime Then
'        Call AddLSX(ThisWorkbook.path & "\ar_osutilsrt.lsx")
'        Call AddLSX(ThisWorkbook.path & "\ar_mysqlrt.lsx")
'#Else
        Call AddLSX(ThisWorkbook.path & "\ar_osutils.lsx")
        Call AddLSX(ThisWorkbook.path & "\ar_mysql.lsx")
'#End If
        On Error GoTo catch
        
        Call AddPredLS("xl_msgbox", 1, AddressOf PMsgBox)
        Call AddPredLS("xl_debugprint", 1, AddressOf PDebugPrint)
        Call AddPredLS("xl_msgbox", 4, AddressOf PMsgBox4)
        Call AddPredLS("xl_inputbox", 4, AddressOf PInputBox)
        Call AddPredLS("xl_cell", 2, AddressOf PCell2)
        Call AddPredLS("xl_cell", 3, AddressOf PCell3)
        Call AddPredLS("xl_cell", 4, AddressOf PCell4)
        Call AddPredLS("xl_range_size", 3, AddressOf PRangeSize3)
'        Call AddPredLS("xl_format_cell", 3, AddressOf PFormatCell)
'        Call AddPredLS("xl_trace_step", 0, AddressOf PTraceStep)
'        Call AddPredLS("xl_debug_output", 1, AddressOf PTraceOutput)   ' was PTraceOutput
        Call AddPredLS("xl_evaluate", 2, AddressOf PEvaluate)
        ' Get the path from the AddIn object
        
'#If runtime Then
'        LoadLS ThisWorkbook.path & "\arulesrt.xpl"
'#Else
        LoadLS ThisWorkbook.path & "\arules.xpl"
        
        If Not ExecStrLS(t, "check_license") Then
            MsgBox GetText("license_expired")
            Call CloseLS
            Exit Sub
        End If
        tf = ExecStrLS(t, "check_maintenance")
'#End If
        
        'MsgBox Application.LanguageSettings.LanguageID(msoLanguageIDUI)
        cc = Application.International(xlCountryCode)
        tf = ExecStrLS(t, "set_parm(CountryCode, " & cc & ")")
        
        tf = ExecStrLS(t, "license_type(?l)")
        If (tf) Then
            Edition = GetStrArgLS(t, 1)
        Else
            Edition = "Unknown"
        End If
    End If
    SetARulesActive True
    
    ' See if we should help the user
'    If GetRegistryMessageIndicator("NewRulesetHelp") = False Then
'        AssistantText.Set_RegistryKey ("NewRulesetHelp")
'        AssistantText.Text.caption = GetText("new_ruleset_help")
'        AssistantText.Show
'    End If
        
Exit Sub
catch:
    SetARulesActive False
    MsgBox "ARulesXL Initialization Error[" & Err.Number & "] " & Err.Description & " in " & Err.Source, vbCritical
    Err.Clear

End Sub
Private Sub StopARules()

    On Error GoTo catch

    'MsgBox "Attempting to Stop ARules"
    If ARulesActive = True Then Call CloseLS
    SetARulesActive False
    
    Exit Sub
catch:
    DealWithException ("StopARules")
End Sub


