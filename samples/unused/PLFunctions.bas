Attribute VB_Name = "PLFunctions"
Option Explicit


Public Sub Analyze()
Dim result As Variant
Dim rules As Range
Dim bc As Range
Dim i As Integer
Dim j As Integer
Dim ul As Range
Dim lr As Range
Dim r As Range
Dim tables As Variant

    tables = Array("East2004", "West2004", "East2005", "West2005")
    For i = LBound(tables) To UBound(tables)
        Set r = Range(tables(i))
        Set ul = r.Cells(2, 2)
        Set lr = r.Cells(r.Rows.Count, r.Columns.Count)
        Range(ul, lr).Interior.ColorIndex = 40
    Next i
    
    Set rules = Range("BudgetRules")
    
'    result = Application.Run("ARulesXL.xla!RQuery", rules, "FIND .Test")
'    Range(result).Interior.ColorIndex = 3
    
    result = Application.Run("ARulesXL.xla!RArrayQuery", rules, "FIND .analysis")
    
    If VarType(result) = vbArray Then
        For i = LBound(result) To UBound(result)
            Range(result(i, 1)).Interior.ColorIndex = 46
        Next i
    Else
        MsgBox "Error: " & result
    End If
    
    
'    For i = 1 To 56
'        Cells(i, "P").Interior.ColorIndex = i
'    Next i
    
End Sub


