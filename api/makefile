# arules/src/api
ADDIN_DIR = ..\..\ARulesXL\addin
API_DIR = ..\..\ARulesXL\api

all : _dirs _delphi _vbdotnet _csdotnet _java _modl _vba _excel _copy

_dirs:
  -md $(API_DIR)\bin_redist
  -md $(API_DIR)\excel
  
_excel:
  copy ..\library\ARulesXL.xla $(API_DIR)\excel
  
_delphi:
  cd delphi
  bmake
  cd ..
  
_vbdotnet:
  cd vb.net
  bmake
  cd ..
  
_csdotnet:
  cd cs.net
  bmake
  cd ..
  
_java:
  cd java
  bmake
  cd ..

_modl:
  cd modl
  bmake
  cd ..

_vba:
  cd vb6
  bmake
  cd ..
  
_copy:
	copy $(AMZI_DEV_DIR)\src\acore\engine\arules.dll $(API_DIR)\bin_redist
	copy $(AMZI_DEV_DIR)\src\lsx\mysql\ar_mysql.lsx $(API_DIR)\bin_redist
	copy $(AMZI_DEV_DIR)\src\lsx\osutils\ar_osutils.lsx $(API_DIR)\bin_redist
	copy $(ADDIN_DIR)\arules.cfg $(API_DIR)\bin_redist\arules.cfg
	copy $(ADDIN_DIR)\arules.xpl $(API_DIR)\bin_redist\arules.xpl
#  copy $(AMZI_DEV_DIR)\src\acore\engine\arules.dll c:\Extend6LT\Extensions\arules.dll
#  copy $(API_DIR)\bin_redist\*.lsx c:\Extend6LT\Extensions
#  copy $(API_DIR)\bin_redist\arules.cfg c:\Extend6LT\Extensions
#  copy $(API_DIR)\bin_redist\arules.xpl c:\Extend6LT\Extensions
	
	
