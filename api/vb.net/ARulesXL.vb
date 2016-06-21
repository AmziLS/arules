'arulesnet.dll provides an interface between .NET and the ARulesXL engine
Imports arulesnet

Public Class ARulesXL
   Implements IDisposable

   Private ls As LogicServer = Nothing

   ' Implement IDisposable.
   Public Overloads Sub Dispose() Implements IDisposable.Dispose
      Dispose(True)
      GC.SuppressFinalize(Me)
   End Sub

   Protected Overridable Overloads Sub Dispose(ByVal disposing As Boolean)
      If disposing Then
         ' Free other state (managed objects).
      End If
      ' Free your own state (unmanaged objects).
      If (Not ls Is Nothing) Then
         ls.Close()
         ls = Nothing
      End If
      ' Set large fields to null.
   End Sub

   Protected Overrides Sub Finalize()
      ' Simply call Dispose(False).
      Dispose(False)
   End Sub

   Public Sub OpenRules(ByVal rulesetfile As String, ByVal arulespath As String)
      Dim term As Long
      Dim s As String

      Try
         'Get our logicserver and initialize it
         ls = New LogicServer
         ls.Init(doubleSlashes(arulespath + "arulesrt.cfg"))
         'ls.Init2("apitrace = on, logfile = c:/InetPub/wwwroot/temp/" + id + ".api")

         'Load the arulesxl engine
         ls.Load(doubleSlashes(arulespath + "arulesrt.xpl"))

         'Load the ruleset
         s = "consult(`" + doubleSlashes(rulesetfile) + "`)"
         term = ls.ExecStr(s)
         If term = 0 Then
            Throw New Exception("Unable to consult ruleset")
         End If
      Catch ex As LSException
         Throw New Exception("OpenRules / " + FormatLSException(ex))
      End Try

   End Sub

   Public Sub CloseRules()
      If (Not ls Is Nothing) Then
         ls.Close()
         ls = Nothing
      End If
   End Sub

   Public Function GetLS() As LogicServer
      Return ls
   End Function

   Public Function QueryRules(ByVal ruleset As String, ByVal query As String) As String
      Dim s As String
      Dim term, answer As Long
      Dim len As Integer

      Try
         s = "arxl_query(" + ruleset + ", false, `" + query + "`, ?answer)"
         term = ls.ExecStr(s)
         If term = 0 Then
            Throw New Exception(FormatARulesError())
         End If

         'The answer is a string in the 4th argument
         'Could be any type, turn it into a string
         answer = ls.GetArg(term, 4)
         len = ls.StrTermLen(answer)
         Return ls.TermToStr(answer, len+1)
      Catch ex As LSException
         Throw New System.Exception("Error getting arules error: " + ex.GetMsg())
      End Try
   End Function

   Public Function QueryMore(ByVal ruleset As String, ByVal query As String) As String
      Dim s As String
      Dim term, answer As Long
      Dim len As Integer

      Try
         s = "arxl_query(" + ruleset + ", true, `" + query + "`, ?answer)"
         term = ls.ExecStr(s)
         If term = 0 Then
            Throw New Exception(FormatARulesError())
         End If

         'The answer is a string in the 4th argument
         'Could be any type, turn it into a string
         answer = ls.GetArg(term, 4)
         len = ls.StrTermLen(answer)
         Return ls.TermToStr(answer, len+1)
      Catch ex As LSException
         Throw New Exception("Error getting arules error: " + ex.GetMsg())
      End Try
   End Function

   Private Function vb_get_charset(ByRef ls As LogicServer) As String
      '         charsetTerm = ls.ExecStr("")
      '         If charsetTerm <> 0 Then
      '         charset = ls.GetStrArg(charsetTerm, 1)

      Return Nothing
   End Function

   Public Sub ClearTable(ByVal ruleset As String, ByVal objectname As String)
      Dim term As Long

      term = ls.ExecStr("arxl_initialize_table(" + ruleset + ", `" + objectname + "`)")
      If (term = 0) Then
         Throw New Exception(FormatARulesError())
      End If
   End Sub

   Public Sub ClearVector(ByVal ruleset As String, ByVal objectname As String)
      Dim term As Long

      term = ls.ExecStr("arxl_initialize_table(" + ruleset + ", `" + objectname + "`)")
      If (term = 0) Then
         Throw New Exception(FormatARulesError())
      End If
   End Sub

   Public Sub AddToTable(ByVal ruleset As String, ByVal objectname As String, ByVal rowname As String, ByVal colname As String, ByVal value As String)
      Dim term As Long

      term = ls.ExecStr("arxl_add_to_table(" + ruleset + ", `" + objectname + "`, `" + rowname + "`, `" + colname + "`, `" + value + "`)")
      If (term = 0) Then
         Throw New Exception(FormatARulesError())
      End If
   End Sub

   Public Sub AddToVector(ByVal ruleset As String, ByVal objectname As String, ByVal rowname As String, ByVal value As String)
      Dim term As Long
      Dim s As String

      s = "arxl_add_to_vector(" + ruleset + ", `" + objectname + "`, `" + rowname + "`, `" + value + "`)"
      term = ls.ExecStr(s)
      If (term = 0) Then
         Throw New Exception(FormatARulesError())
      End If
   End Sub

   Public Sub AddObject(ByVal ruleset As String, ByVal objectname As String, ByVal value As String)
      Dim term As Long

      term = ls.ExecStr("arxl_add_data_cell(" + ruleset + ", `" + objectname + "`, `" + value + "`)")
      If (term = 0) Then
         Throw New Exception(FormatARulesError())
      End If
   End Sub

   Public Function ARulesRegisterRuntime(ByVal method As Long, ByVal proxylist As String, ByVal runtimeid As String)
      ARulesRegisterRuntime = ls.ARulesRegisterRuntime(method, proxylist, runtimeid)
   End Function

   'Display errors from the underlying Amzi! Logic Server
   Private Function FormatLSException(ByVal e As LSException) As String
      Dim msg = "", lineno As String

      Try
         'Consult errors
         If e.GetExceptType() = exLSTYPE.READ Then
            lineno = e.GetReadLineno().ToString()
            msg = e.GetMsg() + vbCr + "in file " + e.GetReadFileName() + vbCr + "at line " + lineno + vbCr + e.GetReadBuffer()
            Return (msg)
         End If

         'Other Logic Server error
         Return (e.GetMsg())
      Catch ex2 As LSException
         Return ("Error catching ARulesXL error")
      End Try
   End Function

   'Display errors from ARulesXL
   Private Function FormatARulesError() As String
      Dim term As Long

      Try
         term = ls.ExecStr("query_error(?x)")
         If (term) Then
            Return ls.GetStrArg(term, 1)
         Else
            Return "Unknown ARulesXL error"
         End If
      Catch ex As LSException
         Return ("Error getting ARulesXL error: " + ex.GetMsg())
      End Try
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

   ' This function converts a Prolog list to a Collection. In general, Prolog lists
   ' are expected to be flat and consist of ITEM = VALUE. If for some reason, a list
   ' element is itself a list, we quote the entire element so that it can be passed
   ' to StrToTermLS for further processing using the Amzi! Logic Server.
   Private Sub prologListToCollection(ByRef ls As LogicServer, ByVal slotList As Long, ByRef c As Collection)
        Dim value, element, fname, list, type As Long
        Dim i As Short
        Dim vstr, nstr As String

      list = slotList
      If (c.Count() > 0) Then
         For i = 1 To c.Count()
            c.Remove((1))
         Next i
      End If

      ' Check for the empty list or an atom
      type = ls.GetTermType(list)
      If (type <> pLSTYPE.pLIST) Then
         Exit Sub
      End If

      ' Otherwise get the head
      element = ls.GetHead(list)
      fname = ls.GetArg(element, 1)
      value = ls.GetArg(element, 2)
      nstr = ls.TermToStr(fname, 1000)
      If (ls.GetTermType(value) = pLSTYPE.pLIST) Then
         vstr = ls.TermToStrQ(value, 100000)
      Else
         vstr = ls.TermToStr(value, 100000)
      End If
      c.Add(vstr, nstr)

      ' And the rest of the list
      list = ls.GetTail(list)
      While (list <> 0)
         element = ls.GetHead(list)
         fname = ls.GetArg(element, 1)
         value = ls.GetArg(element, 2)
         nstr = ls.TermToStr(fname, 1000)
         If (ls.GetTermType(value) = pLSTYPE.pLIST) Then
            vstr = ls.TermToStrQ(value, 100000)
         Else
            vstr = ls.TermToStr(value, 100000)
         End If
         c.Add(vstr, nstr)

         list = ls.GetTail(list)
      End While

   End Sub

   ' This function takes a simple Prolog list and creates a collection where each element
   ' is numbered.
   Private Sub prologListToIndexedCollection(ByRef ls As LogicServer, ByVal slotList As Long, ByRef c As Collection)
      Dim element, list As Long
      Dim i As Short
      Dim vstr As String

      list = slotList
      If (c.Count() > 0) Then
         For i = 1 To c.Count()
            c.Remove((1))
         Next i
      End If

      ' Check for the empty list or an atom
      If (ls.GetTermType(list) <> pLSTYPE.pLIST) Then
         Exit Sub
      End If

      ' Otherwise get the head
      element = ls.GetHead(list)
      vstr = ls.TermToStr(element, 100000)
      c.Add(vstr)

      ' And the rest of the list
      list = ls.GetTail(list)
      While (list <> 0)
         element = ls.GetHead(list)
         vstr = ls.TermToStr(element, 100000)
         c.Add(vstr)

         list = ls.GetTail(list)
      End While

   End Sub

End Class
