API_DIR = ..\..\..\ARulesXL\api

all : _dirs _copy

_dirs:
  -md $(API_DIR)\delphi
  -md $(API_DIR)\delphi\samples
  -md $(API_DIR)\delphi\samples\advice  
  
_copy:
  copy arules.pas $(API_DIR)\delphi
  copy arules.dcr $(API_DIR)\delphi
  copy arulesxl.pas $(API_DIR)\delphi
  copy samples\advice\Advice.dpr $(API_DIR)\delphi\samples\advice
  copy samples\advice\Advice.res $(API_DIR)\delphi\samples\advice
  copy samples\advice\AdviceUnit.dfm $(API_DIR)\delphi\samples\advice
  copy samples\advice\AdviceUnit.pas $(API_DIR)\delphi\samples\advice
  