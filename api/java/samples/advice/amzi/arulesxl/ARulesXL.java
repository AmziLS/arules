package amzi.arulesxl;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import amzi.ls.LogicServer;
import amzi.ls.LSException;

public class ARulesXL {
	private LogicServer ls = null;
	private boolean initialized=false;
	private PrintWriter out;
	
	public ARulesXL() {
		out = null;
	}
	public ARulesXL(PrintWriter out) {
		this.out = out;
	}
	
	public void openRules(String rulesfile, String arulespath) throws Exception {
		try {
			ls = new LogicServer();
			ls.Init(arulespath + "arules");
			initialized = true;
			ls.Load(arulespath + "arules");
			long term = ls.ExecStr("load('" + rulesfile + "')");
			if (term == 0)
				throw new Exception("Unable to load rules file: " + rulesfile);		
		} 
		catch (LSException ex) {
			throw new Exception(formatLSException(ex));
		}
	}
	
	public void closeRules() throws Exception {
		try {
			if (initialized && ls != null) {
				ls.Close();
				ls = null;
			}
		} 
		catch (LSException ex) {
			throw new Exception(formatLSException(ex));
		}
		initialized = false;
	}
	
	public LogicServer getLS() {
		return ls;
	}
	
	public boolean clearTable(String ruleset, String objectname) throws Exception {
		try {
			long term = ls.ExecStr("arxl_initialize_table(" + ruleset + ", `" +
				objectname + "`)");
			if (term == 0)
				throw new Exception(formatARulesError());
		} 
		catch (LSException ex) {
			throw new Exception(formatLSException(ex));
		}
		return true;
	}

	public boolean clearVector(String ruleset, String objectname) throws Exception {
		try {
			long term = ls.ExecStr("arxl_initialize_table(" + ruleset + ", `" +
				objectname + "`)");
			if (term == 0)
				throw new Exception(formatARulesError());
		} 
		catch (LSException ex) {
			throw new Exception(formatLSException(ex));
		}
		return true;		
	}

	public boolean addObject(String ruleset, String objectname, String value) throws Exception {
		try {
			long term = ls.ExecStr("arxl_add_data_cell(" + ruleset + ", `" +
				objectname + "`, `" + value +"`)");
			if (term == 0)
				throw new Exception(formatARulesError());
		} 
		catch (LSException ex) {
			throw new Exception(formatLSException(ex));
		}
		return true;	
	}

	public boolean addObject(String ruleset, String objectname, int value) throws Exception {
		return addObject(ruleset, objectname, new Integer(value).toString());
	}
	
	public boolean addToVector(String ruleset, String objectname, String rowname, String value) throws Exception {
		try {
			long term = ls.ExecStr("arxl_add_to_vector(" + ruleset + ", `" +
				objectname + "`, `" + rowname + "`, `" + value +"`)");
			if (term == 0)
				throw new Exception(formatARulesError());
		} 
		catch (LSException ex) {
			throw new Exception(formatLSException(ex));
		}
		return true;	
	}

	public boolean addToVector(String ruleset, String objectname, int rownum, String value) throws Exception {
		return addToVector(ruleset, objectname, new Integer(rownum).toString(), value);
	}

	public boolean addToVector(String ruleset, String objectname, String rowname, int value) throws Exception {
		return addToVector(ruleset, objectname, rowname, new Integer(value).toString());
	}

	public boolean addToVector(String ruleset, String objectname, int rownum, int value) throws Exception {
		return addToVector(ruleset, objectname, new Integer(rownum).toString(), new Integer(value).toString());
	}

	public boolean addToTable(String ruleset, String objectname, String rowname, String colname, String value) throws Exception {
		try {
			String s = "arxl_add_to_table(" + ruleset + ", `" +
				objectname + "`, `" + rowname + "`, `" + colname + "`, `" + value +"`)";
			long term = ls.ExecStr(s);
			if (term == 0)
				throw new Exception(formatARulesError());
		} 
		catch (LSException ex) {
			throw new Exception(formatLSException(ex));
		}
		return true;	
	}

	public boolean addToTable(String ruleset, String objectname, int rownum, int colnum, String value) throws Exception {
		return addToTable(ruleset, objectname, new Integer(rownum).toString(), new Integer(colnum).toString(), value);
	}

	public boolean addToTable(String ruleset, String objectname, String rowname, String colname, int value) throws Exception {
		return addToTable(ruleset, objectname, rowname, colname, new Integer(value).toString());
	}

	public boolean addToTable(String ruleset, String objectname, int rownum, int colnum, int value) throws Exception {
		return addToTable(ruleset, objectname, new Integer(rownum).toString(), new Integer(colnum).toString(), new Integer(value).toString());
	}

	public String queryRules(String ruleset, String query) throws Exception {
		try {
			long term = ls.ExecStr("arxl_query(" + ruleset + ", false, `" + query + "`, ?x)");
			if (term == 0)
				throw new Exception(formatARulesError());
			long answer = ls.GetArg(term, 4);
			int len = ls.StrTermLen(answer);
			return ls.TermToStr(answer, len+1);
		} 
		catch (LSException ex) {
			throw new Exception(formatLSException(ex));
		}
	}

	public String queryMore(String ruleset, String query) throws Exception {
		try {
			long term = ls.ExecStr("arxl_query(" + ruleset + ", true, `" + query + "`, ?x)");
			if (term == 0)
				throw new Exception(formatARulesError());
			long answer = ls.GetArg(term, 4);
			int len = ls.StrTermLen(answer);
			return ls.TermToStr(answer, len+1);
		} 
		catch (LSException ex) {
			throw new Exception(formatLSException(ex));
		}
	}

	public Vector queryRulesToStringList(String ruleset, String query) throws Exception {
		long term, list, list2, type, element;
		int len;
		String s;
		
		Vector rows = new Vector();

		try {
			term = ls.ExecStr("arxl_query(" + ruleset + ", false, `" + query + "`, ?x)");
			if (term == 0)
				throw new Exception(formatARulesError());
			list = ls.GetArg(term, 4);

			type = ls.GetTermType(list);
			len = ls.StrTermLen(list);
			if (type != LogicServer.pLIST) {
				rows.add(ls.TermToStr(list, len));
				return rows;
			}

			while (list != 0) {
				list2 = ls.GetHead(list);
				type = ls.GetTermType(list);
				if (type != LogicServer.pLIST) {
					len = ls.StrTermLen(list);
					rows.add(ls.TermToStr(list2, len+1));
				}
				else {
					List array = new ArrayList();
					while (list2 != 0) {
						element = ls.GetHead(list2);
						len = ls.StrTermLen(element);
						s = ls.TermToStr(element, len+1);
						array.add(s);	
						list2 = ls.GetTail(list2);
					}
					String[] onerow= new String[array.size()];
					array.toArray(onerow);
					rows.add(onerow);
				}
				list = ls.GetTail(list);
			}
			return rows;
		} 
		catch (LSException ex) {
			throw new Exception(formatLSException(ex));
		}
	}
	
	public Vector queryMoreToStringList(String ruleset, String query) throws Exception {
		long term, list, list2, type, element;
		int len;
		String s;
		
		Vector rows = new Vector();

		try {
			term = ls.ExecStr("arxl_query(" + ruleset + ", true, `" + query + "`, ?x)");
			if (term == 0)
				throw new Exception(formatARulesError());
			list = ls.GetArg(term, 4);

			type = ls.GetTermType(list);
			len = ls.StrTermLen(list);
			if (type != LogicServer.pLIST) {
				rows.add(ls.TermToStr(list, len+1));
				return rows;
			}

			while (list != 0) {
				list2 = ls.GetHead(list);
				type = ls.GetTermType(list);
				if (type != LogicServer.pLIST) {
					len = ls.StrTermLen(list);
					rows.add(ls.TermToStr(list2, len));
				}
				else {
					List array = new ArrayList();
					while (list2 != 0) {
						element = ls.GetHead(list2);
						len = ls.StrTermLen(element);
						s = ls.TermToStr(element, len+1);
						array.add(s);	
						list2 = ls.GetTail(list2);
					}
					String[] onerow= new String[array.size()];
					array.toArray(onerow);
					rows.add(onerow);
				}
				list = ls.GetTail(list);
			}
			return rows;
		} 
		catch (LSException ex) {
			throw new Exception(formatLSException(ex));
		}
	}
	
	/*
	public int ARulesRegisterRuntime(long Method, String ProxyList, String RuntimeID) {
		return ls.ARulesRegisterRuntime(Method, ProxyList, RuntimeID);
	}
	*/
	
	private String formatLSException(LSException ex) {
		String msg, lineno;
		
		if (ex.GetType() == LSException.READ) {
			lineno = new Integer(ex.GetLineno()).toString();
			msg = ex.GetMsg() + " in file " + ex.GetReadFileName() + " at line " + lineno + "\n" + ex.GetReadBuffer();
			return msg;
		}
		
		return ex.GetMsg();
	}	
	
	private String formatARulesError() {
		try {
			long term = ls.ExecStr("query_error(?x)");
			if (term != 0)
				return ls.GetStrArg(term, 1);
			else
				return "Unknown ARulesXL error";
		}
		catch (LSException ex) {
			return "Error getting ARulesXL error: " + ex.GetMsg();
		}
	}
	
	/*
	 *  (non-Javadoc)
	 * @see java.lang.Object#finalize()
	 */
	protected void finalize() throws Throwable {
		super.finalize();
		closeRules();
	}
	
}
