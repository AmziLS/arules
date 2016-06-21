package advice;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.Properties;

import javax.servlet.Servlet;
import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import amzi.arulesxl.ARulesXL;

public class AdviceServlet extends HttpServlet implements Servlet {

	// The directory where the ARulesXL engine and advice.axl are found
	private static final String ARULES_DIR = "c:/Documents and Settings/Mary/workspace/advice/WebContent/"; 
//	private static final String ARULES_DIR = "/amzi/webapps/arules/"; 

	// The URL used to start the servlet
	private static final String FORM_URL = "http://localhost:8080/advice/AdviceServlet";
//	private static final String FORM_URL = "/arules/AdviceServlet";

	private static final long serialVersionUID = -3298275853816250504L;

	private ServletContext context;
	private ARulesXL arxl;
	
	/* (non-Java-doc)
	 * @see javax.servlet.http.HttpServlet#HttpServlet()
	 */
	public AdviceServlet() {
		super();
	} 
	
	/* (non-Javadoc)
	 * @see javax.servlet.Servlet#init()
	 */
   public void init(ServletConfig config) throws ServletException
   {
      super.init(config);
      context = config.getServletContext();
   }

	/* (non-Javadoc)
	 * @see javax.servlet.Servlet#destroy()
	 */
	public void destroy() {
		super.destroy();
	}   	
	
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	    PrintWriter out = null;
		
		// Get the user's session with logic server (must do this first!)
		HttpSession session = request.getSession(true);
		boolean newSession = session.isNew();
        String servletPath = context.getRealPath(request.getServletPath());

		// Must be called before getting values
		String charset = (String)session.getAttribute("arulesxl.charset");
		if (charset != null && charset.length() > 0)
		   try {
		      request.setCharacterEncoding(charset);
		   }
		   catch (UnsupportedEncodingException ex) {
		      throw new ServletException("doGet failed because charset encoding "
		      + charset + " is not supported");
		   }

		// Get the output
        out = response.getWriter();

        // For a new session put out the default form
        if (newSession) {
        	outputForm(request, out, new Properties());
        	return;
        }
        
		// Query for the advice
		arxl = new ARulesXL();
		try {
			// Open the ruleset
			arxl.openRules(ARULES_DIR + "advice.axl", ARULES_DIR);

			// Clear then set the input data, .in[*]
			arxl.clearVector("ShaftRules", ".in");
			arxl.addToVector("ShaftRules", ".in", "Swing Speed", request.getParameter("swingspeed"));
			arxl.addToVector("ShaftRules", ".in", "Club Type", request.getParameter("clubtype"));
			arxl.addToVector("ShaftRules", ".in", "Favor", request.getParameter("favor"));
			arxl.addToVector("ShaftRules", ".in", "Ball Flight", request.getParameter("ballflight"));

			// Get the advice
			out.println("<h2><font color=blue>Shaft Advisor</font></h2>");
			String advice = arxl.queryRules("ShaftRules", "FIND .advice");
			out.println(advice);
			arxl.closeRules();
		} 
		catch (Exception ex) {
			try {
				arxl.closeRules();
			} 
			catch (Exception e) {
			}
			throw new ServletException("AdviceServlet / doGet failed " + ex.getMessage());
		}
	}  	

	/*
	 * Output the form with the input parameters
	 */
	private void outputForm(HttpServletRequest request, PrintWriter out, Properties testData) {
		out.println("<h1><font color=blue>Shaft Advisor</font></h1>");

		out.println("<form action=\"" + FORM_URL + "\" method=\"get\" name=\"advice_form\" id=\"advice_form\">");
		
		out.println("<table border=\"0\">");
		out.println("<tr><td>Swing Speed:&nbsp;&nbsp;&nbsp;</td><td><input type=\"text\" name=\"swingspeed\" value=\"0\"></td></tr>");
		out.println("<tr><td>Club Type:</td><td>");
		out.println("<select name=\"clubtype\">");
		out.println("<option value=\"Driver < 11 Degrees\">Driver < 11 Degrees</option>");
		out.println("<option value=\"Driver >= 11 Degrees\">Driver >= 11 Degrees</option>");
		out.println("<option value=\"Fairway Wood\">Fairway Wood</option>");
		out.println("<option value=\"Hybrid/Utility\">Hybrid/Utility</option>");
		out.println("<option value=\"Iron\">Iron</option>");
		out.println("<option value=\"Wedge\">Wedge</option>");
		out.println("</select></td></tr>");
		out.println("<tr><td>Favor:</td><td>");
		out.println("<select name=\"favor\">");
		out.println("<option value=\"Distance\">Distance</option>");
		out.println("<option value=\"Accuracy\">Accuracy</option>");
		out.println("</select></td></tr>");
		out.println("<tr><td>Ball Flight:</td><td>");
		out.println("<select name=\"ballflight\">");
		out.println("<option value=\"Normal\">Normal</option>");
		out.println("<option value=\"High\">High</option>");
		out.println("<option value=\"Low\">Low</option>");
		out.println("</select></td></tr>");
		out.println("</table>");

		out.println("<input type=\"submit\" name=\"Run\" value=\"Go\"></form>");
	}

}
