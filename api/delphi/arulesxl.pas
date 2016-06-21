unit ARulesXL;

{
  ARulesXL.PAS -- ARulesXL Delphi API Calls the Amzi! Logic Server
  Copyright (c) 2006 Amzi! inc. All Rights Reserved.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Arules;

type
  EARulesXLException = class(Exception);

  TARulesXL = class(TComponent)
  private
    ls: TLSARules;
    initialized: Boolean;
    function formatARulesError(): string;
    function formatLSException(): string;
  protected
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function GetLS(): TLSARules;

    { ARulesXL Standard Methods }
    procedure OpenRules(rulesfile: string; arulespath: string);
    procedure CloseRules();

    procedure ClearTable(ruleset: string; objectname: string);
    procedure ClearVector(ruleset: string; objectname: string);
    procedure AddToTable(ruleset: string; objectname: string; rowname: string; colname: string; value: string); overload;
    procedure AddToTable(ruleset: string; objectname: string; rownum: integer; colnum: integer; value: string); overload;
    procedure AddToTable(ruleset: string; objectname: string; rowname: string; colname: string; ivalue: integer); overload;
    procedure AddToTable(ruleset: string; objectname: string; rownum: integer; colnum: integer; ivalue: integer); overload;
    procedure AddToVector(ruleset: string; objectname: string; rowname: string; value: string); overload;
    procedure AddToVector(ruleset: string; objectname: string; rownum: integer; value: string); overload;
    procedure AddToVector(ruleset: string; objectname: string; rowname: string; ivalue: integer); overload;
    procedure AddToVector(ruleset: string; objectname: string; rownum: integer; ivalue: integer); overload;
    procedure AddObject(ruleset: string; objectname: string; value: string); overload;
    procedure AddObject(ruleset: string; objectname: string; ivalue: integer); overload;

    function QueryRules(ruleset: string; query: string): string;
    function QueryMore(ruleset: string; query: string): string;

    function ARulesRegisterRuntime(method: longint; proxylist: string; runtimeid: string): integer;

    { Delphi-Only Methods }
    function QueryRulesToStringList(ruleset: string; query: string; answer: TStringList): boolean;
    function QueryMoreToStringList(ruleset: string; query: string; answer: TStringList): boolean;
    procedure LoadTableFromNameValueList(ruleset: string; objectname: string; col1name: string; col2name: string; nvlist: TStrings);

  published
  end;

procedure Register;

implementation

constructor TARulesXL.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  ls := TLSARules.Create(Owner);
  initialized := false;
end;

destructor TARulesXL.Destroy;
begin
  if initialized then ls.Close();
  inherited Destroy;
end;

function TARulesXL.GetLS(): TLSARules;
begin
  Result := ls;
end;

procedure TARulesXL.OpenRules(rulesfile: string; arulespath: string);
var
  tf: boolean;
  t: TTerm;
begin
  try
    ls.InitLS(arulespath + 'arulesrt');  { Initialize engine }
    initialized := true;
    ls.Load(arulespath + 'arulesrt'); { Load .xpl file }
    tf := ls.ExecPStr(t, 'load(''' + rulesfile + ''')');
    if (tf <> true) then
      raise EARulesXLException.Create('Unable to load rules file: '+rulesfile);
  except on ELogicServer do raise EARulesXLException.Create(formatLSException());
  end;
end;

procedure TARulesXL.CloseRules();
begin
  try
    ls.Close();
  except on ELogicServer do raise EARulesXLException.Create(formatLSException());
  end;
  initialized := false;
end;

procedure TARulesXL.ClearTable(ruleset: string; objectname: string);
var
  t: TTerm;
  tf: boolean;
begin
  try
    tf := ls.ExecPStr(t, 'arxl_initialize_table(' + ruleset +', `' + objectname + '`)');
    if (tf = false) then
      raise EARulesXLException.Create(FormatARulesError());
  except on ELogicServer do raise EARulesXLException.Create(formatLSException());
  end;
end;

procedure TARulesXL.ClearVector(ruleset: string; objectname: string);
begin
  ClearTable(ruleset, objectname);
end;

procedure TARulesXL.AddObject(ruleset: string; objectname: string; value: string);
var
  t: TTerm;
  tf: boolean;
begin
  try
    tf := ls.ExecPStr(t, 'arxl_add_data_cell(' + ruleset + ', `' + objectname +
      '`, `' + value + '`)');
    if (tf = false) then
      raise EARulesXLException.Create(FormatARulesError());
  except on ELogicServer do raise EARulesXLException.Create(formatLSException());
  end;
end;

procedure TARulesXL.AddObject(ruleset: string; objectname: string; ivalue: integer);
var
  value: string;
begin
  Str(ivalue, value);
  AddObject(ruleset, objectname, value);
end;

procedure TARulesXL.AddToVector(ruleset: string; objectname: string; rowname: string; value: string);
var
  t: TTerm;
  tf: boolean;
begin
  try
    tf := ls.ExecPStr(t, 'arxl_add_to_vector(' + ruleset + ', `' + objectname +
      '`, `' + rowname + '`, `' + value + '`)');
    if (tf = false) then
      raise EARulesXLException.Create(FormatARulesError());
  except on ELogicServer do raise EARulesXLException.Create(formatLSException());
  end;
end;

procedure TARulesXL.AddToVector(ruleset: string; objectname: string; rownum: integer; value: string);
var
  rowname: string;
begin
  Str(rownum, rowname);
  AddToVector(ruleset, objectname, rowname, value);
end;

procedure TARulesXL.AddToVector(ruleset: string; objectname: string; rowname: string; ivalue: integer);
var
  value: string;
begin
  Str(ivalue, value);
  AddToVector(ruleset, objectname, rowname, value);
end;

procedure TARulesXL.AddToVector(ruleset: string; objectname: string; rownum: integer; ivalue: integer);
var
  rowname, value: string;
begin
  Str(rownum, rowname);
  Str(ivalue, value);
  AddToVector(ruleset, objectname, rowname, value);
end;

procedure TARulesXL.AddToTable(ruleset: string; objectname: string; rowname: string; colname: string; value: string);
var
  t: TTerm;
  tf: boolean;
begin
  try
    tf := ls.ExecPStr(t, 'arxl_add_to_table(' + ruleset + ', `' + objectname +
      '`, `' + rowname + '`, `' + colname + '`, `' + value + '`)');
    if (tf = false) then
      raise EARulesXLException.Create(FormatARulesError());
  except on ELogicServer do raise EARulesXLException.Create(formatLSException());
  end;
end;

procedure TARulesXL.AddToTable(ruleset: string; objectname: string; rowname: string; colname: string; ivalue: integer);
var
  value: string;
begin
  Str(ivalue, value);
  AddToTable(ruleset, objectname, rowname, colname, value);
end;

procedure TARulesXL.AddToTable(ruleset: string; objectname: string; rownum: integer; colnum: integer; value: string);
var
  rowname, colname: string;
begin
  Str(rownum, rowname);
  Str(colnum, colname);
  AddToTable(ruleset, objectname, rowname, colname, value);
end;

procedure TARulesXL.AddToTable(ruleset: string; objectname: string; rownum: integer; colnum: integer; ivalue: integer);
var
  rowname, colname, value: string;
begin
  Str(rownum, rowname);
  Str(colnum, colname);
  Str(ivalue, value);
  AddToTable(ruleset, objectname, rowname, colname, value);
end;

function TARulesXL.QueryRules(ruleset: string; query: string): string;
var
  t, answer: TTerm;
  tf: boolean;
begin
  try
    tf := ls.ExecPStr(t, 'arxl_query(' + ruleset + ', false, `' + query + '`, ?x)');
    if (tf) then
    begin
      ls.GetArg(t, 4, dTERM, @answer);
      Result := ls.TermToPStr(answer);
    end
    else
      raise EARulesXLException.Create(FormatARulesError());
  except on ELogicServer do raise EARulesXLException.Create(formatLSException());
  end;
end;

function TARulesXL.QueryMore(ruleset: string; query: string): string;
var
  t, answer: TTerm;
  tf: boolean;
begin
  try
    tf := ls.ExecPStr(t, 'arxl_query(' + ruleset + ', true, `' + query + '`, ?x)');
    if (tf) then
    begin
      ls.GetArg(t, 4, dTERM, @answer);
      Result := ls.TermToPStr(answer);
    end
    else
      raise EARulesXLException.Create(FormatARulesError());
  except on ELogicServer do raise EARulesXLException.Create(formatLSException());
  end;
end;

{ Delphi-Only Functions }

procedure TARulesXL.LoadTableFromNameValueList(ruleset: string; objectname: string; col1name: string; col2name: string; nvlist: TStrings);
var
  t: TTerm;
  tf: boolean;
  i: integer;
  name, value, idx: string;
begin
  try
    tf := ls.ExecPStr(t, 'arxl_initialize_table(' + ruleset + ', `' + objectname + '`)');
    if (tf <> true) then
      raise EARulesXLException.Create(FormatARulesError());

    for i := 0 to nvlist.Count-1 do
    begin
      Str(i, idx);
      name := nvlist.Names[i];
      value := nvlist.ValueFromIndex[i];
      tf := ls.ExecPStr(t, 'arxl_add_to_table(' + idx +  ', `' + col1name + '`, `' + name + '`)');
      if (tf = false) then
        raise EARulesXLException.Create(FormatARulesError());
      tf := ls.ExecPStr(t, 'arxl_add_to_table(' + idx +  ', `' + col2name + '`, `' + value + '`)');
      if (tf = false) then
        raise EARulesXLException.Create(FormatARulesError());
    end;
  except on ELogicServer do raise EARulesXLException.Create(formatLSException());
  end;
end;

function TARulesXL.QueryRulesToStringList(ruleset: string; query: string; answer: TStringList): boolean;
var
  t, list, item: TTerm;
  argtype: TPType;
  s: string;
  tf: boolean;
  rc: integer;
  msg: array [0..1000] of char;
begin
  try
    { Query the Logic Server }
    tf := ls.ExecPStr(t, 'arxl_query(' + ruleset + ', false, `' + query + '`, ?x)');

    if (tf) then
      begin

      { Get the 4th argument of the result back }
      argtype := ls.GetArgType(t, 4);
      ls.GetArg(t, 4, dTERM, @list);
      if (argtype <> pLIST) then
      begin
        ls.TermToStr(list, msg, 1000);
        raise EARulesXLException.Create(msg);
      end;

      { Walk the list of lists }
      repeat
        rc := ls.PopList(list, dTERM, @item);
        if (rc = 0) then
        begin
          s := ls.TermToPStr(item);
          answer.Add(s);
        end
      until (rc <> 0);
      end
    else
      raise EARulesXLException.Create(FormatARulesError());
  except on ELogicServer do raise EARulesXLException.Create(formatLSException());
  end;

  Result := tf;
end;

function TARulesXL.QueryMoreToStringList(ruleset: string; query: string; answer: TStringList): boolean;
var
  t, list, item: TTerm;
  argtype: TPType;
  s: string;
  tf: boolean;
  rc: integer;
  msg: array [0..1000] of char;
begin
  try
    { Query the Logic Server }
//  before := Time;
    tf := ls.ExecPStr(t, 'arxl_query(' + ruleset + ', true, `' + query + '`, ?x)');
{  after := Time;
  elapsed := MilliSecondsBetween(before, after);
  Str(elapsed, s);
  ShowMessage('Time= ' + s);
}
    if (tf) then
      begin

      { Get the 4th argument of the result back }
      argtype := ls.GetArgType(t, 4);
      ls.GetArg(t, 4, dTERM, @list);
      if (argtype <> pLIST) then
      begin
        ls.TermToStr(list, msg, 1000);
        raise EARulesXLException.Create(msg);
      end;

      { Walk the list of lists }
      repeat
        rc := ls.PopList(list, dTERM, @item);
        if (rc = 0) then
        begin
          s := ls.TermToPStr(item);
          answer.Add(s);
        end
      until (rc <> 0);
      end
    else
      raise EARulesXLException.Create(FormatARulesError());
  except on ELogicServer do raise EARulesXLException.Create(formatLSException());
  end;

  Result := tf;
end;

function TARulesXL.ARulesRegisterRuntime(method: longint; proxylist: string; runtimeid: string): integer;
begin
  Result := ls.ARulesRegisterRuntime(method, proxylist, runtimeid);
end;

function TARulesXL.FormatARulesError(): string;
var
  t: TTerm;
  tf: boolean;
  msg: array [0..1000] of char;
begin
  try
    tf := ls.ExecPStr(t, 'query_error(?x)');
    If tf = true Then
      Result := ls.GetPStrArg(t, 1)
    Else
      Result := 'Unknown ARulesXL error'
  except on ELogicServer do
    begin
    ls.GetExceptMsg(msg, 1000);
    Result := 'Error getting ARulesXL error: ' + msg;
    end;
  end;
end;

function TARulesXL.FormatLSException(): string;
var
  msg: array [0..1000] of char;
begin
  try
    ls.GetExceptMsg(msg, 1000);
    Result := 'Logic Server Error: ' +  msg;
  except on ELogicServer do Result := 'Error getting Logic Server error';
  end;
end;


procedure Register;
begin
  RegisterComponents('Additional', [TARulesXL]);
end;

end.
