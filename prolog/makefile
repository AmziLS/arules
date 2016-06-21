# arules/src/prolog
.SUFFIXES: .plm .pro

CDIR = c:\amzi\source\apls\compile
LDIR = c:\amzi\source\apls\libs
ACMP = $(APLS_RELEASE)\bin\acmp
ALNK = $(APLS_RELEASE)\bin\alnk
ARULES = arules.plm decision_table.plm english.plm french.plm parser.plm parser2.plm reasoner.plm rename.plm sql.plm test.plm tokenizer.plm tokenizer2.plm trace.plm utilities.plm
COMPILER = $(CDIR)\assemb.plm $(CDIR)\builtins.plm $(CDIR)\cclause.plm $(CDIR)\compile.plm $(CDIR)\writeplm.plm
LIBRARY = $(LDIR)\list.plm $(LDIR)\date_time.plm $(LDIR)\xml.plm
ARXL = ..\library
#ARXLRT = "c:\Program Files\Microsoft Office\Office11\Library\arulesrt.xpl"

$(ARXL) : $(ARULES)
	$(ALNK) arules $(LIBRARY) $(ARULES) $(COMPILER)
	copy arules.xpl $(ARXL)
#	copy arules.xpl $(ARXLRT)

.pro.plm:
	$(ACMP) $<

