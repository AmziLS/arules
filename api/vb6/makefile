API_DIR = ..\..\..\ARulesXL\api

all : _dirs _copy

_dirs:
  -md $(API_DIR)\vb6_vba
  -md $(API_DIR)\vb6_vba\samples
  -md $(API_DIR)\vb6_vba\samples\advice
  
_copy:
  copy arules.bas $(API_DIR)\vb6_vba
  copy arulesxl.cls $(API_DIR)\vb6_vba
  copy samples\advice\advice.vbp $(API_DIR)\vb6_vba\samples\advice
  copy samples\advice\advice.frm $(API_DIR)\vb6_vba\samples\advice
  copy samples\advice\advice.frx $(API_DIR)\vb6_vba\samples\advice
  copy samples\advice\advice.vbw $(API_DIR)\vb6_vba\samples\advice
  