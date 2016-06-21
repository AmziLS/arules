API_DIR = ..\..\..\ARulesXL\api

all : _dirs _copy

_dirs:
  -md $(API_DIR)\vb.net
  -md $(API_DIR)\vb.net\samples
  -md $(API_DIR)\vb.net\samples\advice
  -md "$(API_DIR)\vb.net\samples\advice\My Project"
  -md $(API_DIR)\vb.net\samples\advice_web
  
_copy:
  copy $(AMZI_DEV_DIR)\src\lsapis\dotnet\arulesnet\arulesnet.dll $(API_DIR)\bin_redist
  copy ARulesXL.vb $(API_DIR)\vb.net
  copy samples\advice\advice.sln $(API_DIR)\vb.net\samples\advice
  copy samples\advice\advice.vbproj $(API_DIR)\vb.net\samples\advice
  copy samples\advice\AdviceForm.resx $(API_DIR)\vb.net\samples\advice
  copy samples\advice\AdviceForm.vb $(API_DIR)\vb.net\samples\advice
  copy samples\advice\AdviceForm.Designer.vb $(API_DIR)\vb.net\samples\advice
  copy "samples\advice\My Project\*.*" "$(API_DIR)\vb.net\samples\advice\My Project"
  copy samples\advice_web\advice.sln $(API_DIR)\vb.net\samples\advice_web
  copy samples\advice_web\AdviceForm.aspx $(API_DIR)\vb.net\samples\advice_web
  copy samples\advice_web\AdviceForm.aspx.vb $(API_DIR)\vb.net\samples\advice_web
  copy samples\advice_web\Web.config $(API_DIR)\vb.net\samples\advice_web
  