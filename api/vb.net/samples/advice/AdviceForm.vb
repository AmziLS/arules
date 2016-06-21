'arulesnet.dll provides an interface between .NET and the ARulesXL engine

Imports arulesnet

Public Class AdviceForm

   Private Sub GoButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GoButton.Click
      Dim arxl As ARulesXL
      Dim ans, path As String

      'Determine where we are
      path = Application.StartupPath + "\"

      'Get an ARulesXL engine
      arxl = New ARulesXL

      Try
         'Load the ARulesXL engine and ruleset file
         arxl.OpenRules(path + "advice.axl", path)

         'Load the user inputs, first clear the .in vector, then load it up
         arxl.ClearVector("ShaftRules", "in")
         arxl.AddToVector("ShaftRules", "in", "Favor", Favor.SelectedValue)
         arxl.AddToVector("ShaftRules", "in", "Swing Speed", SwingSpeed.Text)
         arxl.AddToVector("ShaftRules", "in", "Club Type", ClubType.SelectedValue)
         arxl.AddToVector("ShaftRules", "in", "Ball Flight", BallFlight.SelectedValue)

         'Query the rule set
         ans = arxl.QueryRules("ShaftRules", "FIND advice")

         'Display the answer
         Advice.Text = ans

         arxl.CloseRules()
      Catch ex As Exception
         Advice.Text() = "runRuleset / " + ex.Message
         If (Not IsDBNull(arxl)) Then
            arxl.CloseRules()
         End If
         Return
      End Try

   End Sub
End Class
