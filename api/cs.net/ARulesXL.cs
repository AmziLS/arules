using System;
using System.Text;

/// <summary>
/// Summary description for arulesxl
/// </summary>
public class ARulesXL: IDisposable
{
    private LogicServer ls = null;
    private bool disposed = false;

	public ARulesXL()
	{
		//
		// TODO: Add constructor logic here
		//
	}

    // Implement IDisposable.
    // Do not make this method virtual.
    // A derived class should not be able to override this method.
    public void Dispose()
    {
        Dispose(true);
        // This object will be cleaned up by the Dispose method.
        // Therefore, you should call GC.SupressFinalize to
        // take this object off the finalization queue 
        // and prevent finalization code for this object
        // from executing a second time.
        GC.SuppressFinalize(this);
    }

    // Dispose(bool disposing) executes in two distinct scenarios.
    // If disposing equals true, the method has been called directly
    // or indirectly by a user's code. Managed and unmanaged resources
    // can be disposed.
    // If disposing equals false, the method has been called by the 
    // runtime from inside the finalizer and you should not reference 
    // other objects. Only unmanaged resources can be disposed.
    private void Dispose(bool disposing)
    {
        // Check to see if Dispose has already been called.
        if (!this.disposed)
        {
            // If disposing equals true, dispose all managed 
            // and unmanaged resources.
            if(disposing)
            {
                // Dispose managed resources.
            }
		 
            // Call the appropriate methods to clean up 
            // unmanaged resources here.
            // If disposing is false, 
            // only the following code is executed.
            if (ls != null)
            {
                ls.Close();
                ls = null;
            }
        }
        disposed = true;         
    }

    // Use C# destructor syntax for finalization code.
    // This destructor will run only if the Dispose method 
    // does not get called.
    // It gives your base class the opportunity to finalize.
    // Do not provide destructors in types derived from this class.
    ~ARulesXL()      
    {
        // Do not re-create Dispose clean-up code here.
        // Calling Dispose(false) is optimal in terms of
        // readability and maintainability.
        Dispose(false);
    }

    public void OpenRules(string rulesetfile, string arulespath)
    {
        string s;
        long term;

        try
        {
            // Get our logicserver and initialize it
            ls = new LogicServer();
            ls.Init(DoubleSlashes(arulespath + "arulesrt.cfg"));

            // Load the arulesxl engine
            ls.Load(DoubleSlashes(arulespath + "arulesrt.xpl"));

            // Load the ruleset
            if (rulesetfile != null)
            {
                s = "consult(`" + DoubleSlashes(rulesetfile) + "`)";
                term = ls.ExecStr(s);
                if (term == 0)
                    throw new Exception("Unable to consult ruleset");
            }
        }
        catch (LSException ex)
        {
            throw new Exception("OpenRules / " + FormatLSException(ex));
        }
    }

    public void CloseRules()
    {
        if (ls != null)
        {
            ls.Close();
            ls = null;
        }
    }

    public LogicServer GetLS()
    {
        return ls;
    }
    
    public string QueryRules(string ruleset, string query)
    {
        string s;
        int term, answer;
        int len;

        try
        {
            s = "arxl_query(" + ruleset + ", false, `" + query + "`, ?answer)";
            term = ls.ExecStr(s);
            if (term == 0)
                throw new Exception(FormatARulesError());

            // The answer is a string in the 4th argument
            // Could be any type, turn it into a string
            answer = ls.GetArg(term, 4);
            len = ls.StrTermLen(answer);
            return(ls.TermToStr(answer, len+1));
        }
        catch (LSException ex)
        {
            throw new Exception("Error getting arules error: " + ex.GetMsg());
        }
    }

    public string QueryMore(string ruleset, string query)
    {
        string s;
        int term, answer;
        int len;

        try
        {
            s = "arxl_query(" + ruleset + ", true, `" + query + "`, ?answer)";
            term = ls.ExecStr(s);
            if (term == 0)
                throw new Exception(FormatARulesError());

            // The answer is a string in the 4th argument
            // Could be any type, turn it into a string
            answer = ls.GetArg(term, 4);
            len = ls.StrTermLen(answer);
            return(ls.TermToStr(answer, len+1));
        }
        catch (LSException ex)
        {
            throw new Exception("Error getting arules error: " + ex.GetMsg());
        }
    }

    public void ClearTable(string ruleset, string objectname)
    {
        int term;

        term = ls.ExecStr("arxl_initialize_table(" + ruleset + ", `" + objectname + "`)");
        if (term == 0) 
            throw new Exception(FormatARulesError());
    }

    public void ClearVector(string ruleset, string objectname)
    {
        int term;

        term = ls.ExecStr("arxl_initialize_table(" + ruleset + ", `" + objectname + "`)");
        if (term == 0) 
            throw new Exception(FormatARulesError());
    }

    public void AddToTable(string ruleset, string objectname, string rowname, string colname, string value)
    {
        int term;

        term = ls.ExecStr("arxl_add_to_table(" + ruleset + ", `" + objectname + "`, `" + rowname + "`, `" + colname + "`, `" + value + "`)");
        if (term == 0) 
            throw new Exception(FormatARulesError());
    }

    public void AddToVector(string ruleset, string objectname, string rowname, string value)
    {
        int term;

        term = ls.ExecStr("arxl_add_to_vector(" + ruleset + ", `" + objectname + "`, `" + rowname + "`, `" + value + "`)");
        if (term == 0)
            throw new Exception(FormatARulesError());
    }

    public void AddObject(string ruleset, string objectname, string value)
    {
        int term;

        term = ls.ExecStr("arxl_add_data_cell(" + ruleset + ", `" + objectname + "`, `" + value + "`)");
        if (term == 0)
            throw new Exception(FormatARulesError());
    }

	public int ARulesRegisterRuntime(uint method, string proxylist, string runtimeid)
	{
		return ls.ARulesRegisterRuntime(method, proxylist, runtimeid);
	}

   // Display errors from ARulesXL
    private string FormatARulesError() 
    {
        int term;

        try
        {
            term = ls.ExecStr("query_error(?x)");
            if (term != 0)
                return(ls.GetStrArg(term, 1));
            else
                return("Unknown ARulesXL error");
        }
        catch (LSException ex)
        {
            return ("Error getting ARulesXL error: " + ex.GetMsg());
        }
    }

    // Display errors from the underlying Amzi! Logic Server
    private string FormatLSException(LSException e)
    {
        string msg = "";
        string lineno;

        try
        {
             // Consult errors
            if (e.GetExceptType() == (int)exLSTYPE.READ)
             {
                lineno = e.GetReadLineno().ToString();
                msg = e.GetMsg() + "\nin file " + e.GetReadFileName() + "\nat line " + lineno + "\n" + e.GetReadBuffer();
                return (msg);
             }

             // Other Logic Server error
             msg = e.GetMsg();
             return (msg);
        }
        catch (LSException)
        {
            return ("Error catching ARulesXL error");
        }
    }

    private string DoubleSlashes(string path)
    {
      int i;
      StringBuilder sb;

      sb = new StringBuilder(path);
      for (i = 0 ; i < sb.Length ; i++) 
      {
         if (sb[i] == '\\') 
         {
            sb.Insert(i, '\\');
            i++;
         }
      }
      return(sb.ToString());
    }

}
