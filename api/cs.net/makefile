API_DIR = ..\..\..\ARulesXL\api

all : _dirs _copy

_dirs:
  -md $(API_DIR)\cs.net
  -md $(API_DIR)\cs.net\samples
  -md $(API_DIR)\cs.net\samples\advice
  -md $(API_DIR)\cs.net\samples\advice\Properties
  -md $(API_DIR)\cs.net\samples\advice_web
  -md $(API_DIR)\cs.net\samples\advice_web\App_Code
  
_copy:
  copy ARulesXL.cs $(API_DIR)\cs.net
  copy samples\advice\advice.sln $(API_DIR)\cs.net\samples\advice
  copy samples\advice\advice.csproj $(API_DIR)\cs.net\samples\advice
  copy samples\advice\AdviceForm.resx $(API_DIR)\cs.net\samples\advice
  copy samples\advice\AdviceForm.cs $(API_DIR)\cs.net\samples\advice
  copy samples\advice\AdviceForm.Designer.cs $(API_DIR)\cs.net\samples\advice
  copy samples\advice\Program.cs $(API_DIR)\cs.net\samples\advice
  copy "samples\advice\Properties\*.*" "$(API_DIR)\cs.net\samples\advice\Properties"
  copy samples\advice_web\advice.sln $(API_DIR)\cs.net\samples\advice_web
  copy samples\advice_web\AdviceForm.aspx $(API_DIR)\cs.net\samples\advice_web
  copy samples\advice_web\AdviceForm.aspx.cs $(API_DIR)\cs.net\samples\advice_web
  copy samples\advice_web\Web.config $(API_DIR)\cs.net\samples\advice_web
  copy ARulesXL.cs $(API_DIR)\cs.net\samples\advice_web\App_Code