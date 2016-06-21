Imports System.Collections.Specialized
Imports System.IO

'arulesnet.dll provides an interface between .NET and the ARulesXL engine
Imports arulesnet


Namespace advice


' If you get the error cannot find arulesnet or one of its dependencies, it is
' likely because arulesrt.cfg, or arulesnet.dll are not in the bin directory.
' Or, arulesrt.dll is not on your system PATH.

' After changing your PATH you must stop/restart IIS. Sometimes you need to
' use Task Manager to stop the aspnet_wp process and any iexplore processes 
' that are hung.

' If your ruleset is not working the same as in Excel, then
' remove all extra elements from the ARulesXLForm.aspx file so you are running
' on a blank page. If that works, one of your additional elements is
' causing GET requests.

' To Debug under Visual Studio, in Web.config under compilation, set Debug="true"
' and build the Debug version of the project.

' To display a Debug log at the bottom of each screen in Web.config under trace 
' set enabled="true" and pageOutput="true" and if needed localOnly="false"

' The ARULESXL_DEBUG constant will append the debug log onto the end of the output
' You can modify the code to put the debugLog string wherever is convenient.
#Const ARULESXL_DEBUG = False

' The ARULESXL_LOGFILE constant will write the debug log to a file in the directory
' set in Session("arulesxl.session_directory"). (You must set this value without a
' trailing \). Make sure the ASPNET user has write access to the directory. A
' separate log file is created for each user session.
#Const ARULESXL_LOGFILE = False

Partial Class AdviceForm
   Inherits System.Web.UI.Page

#Region " Web Form Designer Generated Code "

   'This call is required by the Web Form Designer.
   <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()

   End Sub


   Private Sub Page_Init(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Init
      'CODEGEN: This method call is required by the Web Form Designer
      'Do not modify it using the code editor.
      InitializeComponent()
      RulesetFilename.Text = "advice.axl"
      Ruleset.Text = "ShaftRules"
         Query.Text = "FIND advice"
      AnswerLabel.Visible = False
      AnswerText.Visible = False
   End Sub

#End Region

   Dim debugLog As String
   Dim arxl As arulesxl

   Private Sub Page_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
      Dim id, charset, s As String
      Dim ls
      Dim i As Integer

      'Get an ARulesXL engine
      arxl = New arulesxl

      'Start with a blank string
      debugLog = ""
      Session.Add("arulesxl.session_directory", doubleSlashes(Request.QueryString.Get("temp_directory")) + "\\")

      'Get a unique session id for us (also in Global.axax.vb/Session_End)
      id = "arulesxl_" + Session.SessionID
      log("ARulesXL SessionID = " + Session.SessionID + "  ")

      'Log the parameters
      'For i = 0 To Request.Params.Count - 1
      '   log("Param:  " + Request.Params.GetKey(i) + " = " + Request.Params.Get(i))
      'Next

      'Log a bunch of details about our state and configuration
      log("Sender = " + sender.GetType.ToString())
      log("Event Args = " + e.ToString())
      log("HttpMethod = " + Request.HttpMethod.ToString())
      If Session.IsNewSession Then
         log(" NewSession")
      End If
      If Session.IsCookieless Then
         log(" Cookieless")
      End If
      If Session.IsReadOnly Then
         log(" ReadOnly")
      End If
      If Session.IsSynchronized Then
         log(" Synchronized")
      End If
      If Session.Mode = Web.SessionState.SessionStateMode.InProc Then
         log("Session State Mode = InProc")
      End If
      If Session.Mode = Web.SessionState.SessionStateMode.Off Then
         log("Session State Mode = Off")
      End If
      If Session.Mode = Web.SessionState.SessionStateMode.SQLServer Then
         log("Session State Mode = SQLServer")
      End If
      If Session.Mode = Web.SessionState.SessionStateMode.StateServer Then
         log("Session State Mode = StateServer")
      End If
      log("Session Key Count = " + Session.Count.ToString())
      log("Postback = " + IsPostBack.ToString())

      'IsPostBack is set when the user submits the form
      'So this is the second time through Page_Load
      'and we run the query
      If IsPostBack Then
         log("PostBack")

         'Run the ruleset
         AnswerLabel.Visible = True
         AnswerText.Visible = True
         runRuleset(id, Session.IsNewSession)
      End If

   End Sub

   Private Sub runRuleset(ByVal id As String, ByVal newSession As Boolean)
      Dim ls
      Dim charset, ans, path As String
      Dim term, charsetTerm As Long

      Try
         'Determine where we are
         path = Request.PhysicalApplicationPath + "bin\"

         'Load the ARulesXL engine and ruleset file
         log("Opening ARulesXL Engine")
         arxl.OpenRules(path + RulesetFilename.Text, path)

         Session.Add("arulesxl.logicserver", arxl.GetLS())
         log("Set Session(arulesxl.logicserver)")
         ls = arxl.GetLS

         'Get the character set from the ruleset and set it for http
         charset = vb_get_charset(ls)
         If charset <> Nothing Then
            Session.Add("arulesxl.charset", charset)
            log("Set Session(arulesxl.charset) to: " + charset)
            Response.Charset = charset
         End If

         'Load the user inputs, first clear the .in vector, then load it up
            arxl.ClearVector("ShaftRules", "in")
            arxl.AddToVector("ShaftRules", "in", "Favor", Favor.SelectedValue)
            arxl.AddToVector("ShaftRules", "in", "Swing Speed", SwingSpeed.Text)
            arxl.AddToVector("ShaftRules", "in", "Club Type", ClubType.SelectedValue)
            arxl.AddToVector("ShaftRules", "in", "Ball Flight", BallFlight.SelectedValue)

         'Query the rule set
         ans = arxl.QueryRules(Ruleset.Text, Query.Text)

         'Display the answer
         AnswerText.Text = ans

#If ARULESXL_DEBUG Then
         'If we need to append the debug log, do that
         AnswerText.Text = AnswerText.Text + vbCr + vbCr + "ARulesXL Debug Log" + vbCr + debugLog
#End If

         arxl.CloseRules()
      Catch ex As Exception
         AnswerText.Text() = "runRuleset / " + ex.Message
         arxl.CloseRules()
         Return
      End Try

   End Sub

   'A nice cover for get charset
   Private Function vb_get_charset(ByRef ls As LogicServer) As String
      '         charsetTerm = ls.ExecStr("")
      '         If charsetTerm <> 0 Then
      '         charset = ls.GetStrArg(charsetTerm, 1)

      Return Nothing
   End Function


   ' This functions doubles backslashes in a pathname before it is passed
   ' to the Amzi! Logic Server
   Private Function doubleSlashes(ByVal path As String) As String
      Dim i As Short
      Dim s As String

      s = ""
      For i = 1 To Len(path)
         If (Mid(path, i, 1) = "\") Then
            s = s & "\\"
         Else
            s = s & Mid(path, i, 1)
         End If
      Next i
      doubleSlashes = s
   End Function

   Private Sub log(ByVal str As String)
      ' Append it to our string to display
      debugLog += str + vbCr

#If ARULESXL_LOGFILE Then
      ' And write it out to our file
      Dim fname As String
      fname = Session("arulesxl.session_directory") + "/aruleslog_" + Session.SessionID + ".log"
      Dim w As StreamWriter = File.AppendText(fname)
      Dim thisMoment As Date
      thisMoment = Now
      w.Write(thisMoment.ToShortDateString + " " + thisMoment.ToLongTimeString)
      w.Write(" -- ")
      w.WriteLine(str)
      w.Flush()
      w.Close()
#End If
   End Sub

End Class

End Namespace
