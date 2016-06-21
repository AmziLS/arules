API_DIR = ..\..\..\ARulesXL\api

all: amzi\ls\ARulesLogicServer.class amzi\arulesxl\ARulesXl.class _amzijni _dirs _copy

amzi\arulesxl\ARulesXL.class: amzi\arulesxl\ARulesXL.java
  javac amzi\arulesxl\ARulesXL.java
  
amzi\ls\ARulesLogicServer.class: amzi\ls\ARulesLogicServer.java amzi\ls\ARulesLSException.java
	javac amzi\ls\ARulesLSException.java
	javac amzi\ls\ARulesLogicServer.java

_amzijni: jnilib\amzi_ls_ARulesLogicServer.h jnilib\amzi_ls_ARulesLSException.h _jnilib

jnilib\amzi_ls_ARulesLogicServer.h: amzi\ls\ARulesLogicServer.java
	javah -jni -d jnilib amzi.ls.ARulesLogicServer

jnilib\amzi_ls_ARulesLSException.h: amzi\ls\ARulesLSException.java
	javah -jni -d jnilib amzi.ls.ARulesLSException

_jnilib:
  cd jnilib
  devenv wjnilib.sln /rebuild Release
  cd ..

_dirs:
	-md $(API_DIR)\java
	-md $(API_DIR)\java\samples
  -md $(API_DIR)\java\samples\advice
  -md $(API_DIR)\java\samples\advice_servlet
  -md $(API_DIR)\java\samples\advice_servlet\advice
  -md $(API_DIR)\java\samples\advice_servlet\WEB-INF

_copy:
	xcopy /Q /S /I amzi $(API_DIR)\java\amzi
	-del /S /Q $(API_DIR)\java\amzi\CVS
	-rmdir /S /Q $(API_DIR)\java\amzi\CVS
	-del /S /Q $(API_DIR)\java\amzi\arulesxl\CVS
	-rmdir /S /Q $(API_DIR)\java\amzi\arulesxl\CVS
	-del /S /Q $(API_DIR)\java\amzi\ls\CVS
	-rmdir /S /Q $(API_DIR)\java\amzi\ls\CVS
	copy jnilib\arulesjni.dll $(API_DIR)\bin_redist
	copy samples\advice\Advice.java $(API_DIR)\java\samples\advice
	copy samples\advice_servlet\advice\AdviceServlet.java $(API_DIR)\java\samples\advice_servlet\advice
	copy samples\advice_servlet\WEB-INF\web.xml $(API_DIR)\java\samples\advice_servlet\WEB-INF
