// If you get the error cannot find arulesnet or one of its dependencies, it is
// likely because arulesrt.cfg, or arulesnet.dll are not in the bin directory.
// Or, arulesrt.dll is not on your system PATH.

// After changing your PATH you must stop/restart IIS. Sometimes you need to
// use Task Manager to stop the aspnet_wp process and any iexplore processes 
// that are hung.

// If your ruleset is not working the same as in Excel, then
// remove all extra elements from the <form>.aspx file so you are running
// on a blank page. If that works, one of your additional elements is
// causing GET requests.

// To Debug under Visual Studio, in Web.config under compilation, set Debug="true"
// and build the Debug version of the project.

// To display a Debug log at the bottom of each screen in Web.config under trace 
// set enabled="true" and pageOutput="true" and if needed localOnly="false"

// The ARULESXL_DEBUG constant will append the debug log onto the end of the output
// You can modify the code to put the debugLog string wherever is convenient.
//#define ARULESXL_DEBUG

using System;
using System.Data;
using System.Configuration;
using System.Text;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class AdviceForm : System.Web.UI.Page 
{
    ARulesXL arxl;
    StringBuilder debugLog = new StringBuilder("");

    protected void Page_Load(object sender, EventArgs e)
    {
        string id;

        // Create a new ARulesXL engine
        arxl = new ARulesXL();
         
        // Get a unique session id for us
        id = "arulesxl_" + Session.SessionID;
        log("ARulesXL SessionID = " + Session.SessionID + "  ");

        // Log a bunch of details about our state and configuration
        if (Session.IsNewSession)
            log(" NewSession");
        log("Postback = " + IsPostBack.ToString());

        // IsPostBack is set when the user submits the form
        // So this is the second time through Page_Load
        // and we run the query
        if (IsPostBack)
        {
            // Run the ruleset
            log("PostBack");
            AnswerLabel.Visible = true;
            AnswerText.Visible = true;
            runRuleset(id, Session.IsNewSession);
        }
        // This is the first time through Page_Load
        // Set default values for some of the controls
        else
        {
            RulesetFilename.Text = "advice.axl";
            Ruleset.Text = "ShaftRules";
            Query.Text = "FIND .advice";
            AnswerLabel.Visible = false;
            AnswerText.Visible = false;
        }
    }

    private void runRuleset(string id, bool newSession)
    {
        string path, ans;

        try
        {
            // Determine where we are
            path = Request.PhysicalApplicationPath + "bin\\";

            // Load the ARulesXL engine and ruleset file
            log("Opening ARulesXL Engine");
            arxl.OpenRules(path + RulesetFilename.Text, path);

            // Load the user inputs, first clear the .in vector, then load it up
            arxl.ClearVector("ShaftRules", ".in");
            arxl.AddToVector("ShaftRules", ".in", "Favor", Favor.SelectedItem.ToString());
            arxl.AddToVector("ShaftRules", ".in", "Swing Speed", SwingSpeed.Text);
            arxl.AddToVector("ShaftRules", ".in", "Club Type", ClubType.SelectedItem.ToString());
            arxl.AddToVector("ShaftRules", ".in", "Ball Flight", BallFlight.SelectedItem.ToString());

            // Query the rule set
            ans = arxl.QueryRules(Ruleset.Text, Query.Text);

            // Display the answer
            AnswerText.Text = ans;

#if (ARULESXL_DEBUG)
            // If we need to append the debug log, do that
            AnswerText.Text = AnswerText.Text + "\n\nARulesXL Debug Log" + "\n" + debugLog;
#endif
            // Close the ARulesXL engine
            arxl.CloseRules();
        }
        catch (Exception ex)
        {
            AnswerText.Text = "runRuleset / " + ex.Message;
            arxl.CloseRules();
        }
    }

    private void log(string msg)
    {
        debugLog.AppendLine(msg);
    }
}

