{
 ARULES.PAS -- Delphi DLL Logic Server API Definitions
 and Delphi Cover Functions
 Copyright (c)1994-2006 Amzi! inc. All Rights Reserved.
}

unit Arules;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs;

const
  lsfalse: integer = 0;
  lstrue:  integer = 1;

type
  ELogicServer = class(Exception);

  { Various types used by the Logic Server API calls }
  TTerm = pointer;  { The basic Prolog term }
  { Enumerated Prolog types and enumerated Delphi types, used for mapping
    Prolog types to Delphi types }
  TPType = (pATOM, pINT, pSTR, pFLOAT, pSTRUCT, pLIST, pTERM, pADDR, pVAR, pWSTR, pWATOM, pREAL);
  TDType = (dATOM, dSTR, dINT, dLONG, dSHORT, dFLOAT, dDOUBLE, dADDR, dTERM, dWSTR, dWATOM, dMOD, dGOAL);
  TTypeInt = integer; { Generic type for casting types in DLL calls }
  { Enumerated stream identifier, used when redirecting Prolog I/O }
  TPStream = (CUR_IN, CUR_OUT, CUR_ERR, USER_IN, USER_OUT, USER_ERR);
  TPStreamInt = integer; { Generic type for stream identifiers in DLL calls}
  TTFi = integer;  { Prolog T/F or error code return code }
  TRC = integer;  { Integer return code }
  TArity = Word;  { The arity of a functor }
  TEngID = longint;  { ID for Engine, only one allowed now }
  TExtPred = function(EngID: TEngID): TTFi; stdcall; { An extended predicate function }

  TPutC = procedure(p:pointer;c: Integer);stdcall; 
  TPutS = procedure(p:pointer;s: PChar);stdcall;
  TGetC = function(p:pointer): Integer;stdcall;
  TUngetC = procedure(p:pointer);stdcall;
  
  TPredInit = record
    Pname: PChar;
    Parity: TArity;
    Pfunc: TExtPred;
  end;
  TPredInitPtr = ^TPredInit;

  { The Logic Server component, a class that encapsulates all of the API
    calls as methods }
  TLSARules = class(TComponent)
  private
    eng: TEngID;
    rc: TRC;
    initializedB, createdB: BOOL;
    buf: array[0..100000] of char;
    procedure LSError(apiname: String; rc: integer);
  protected
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    { Main entry points to set up Prolog environment }
    procedure Init(xplname: String);
    procedure InitLS(xplname: String);
    procedure InitLSXP(p: pointer);
    procedure InitLSX;
    procedure AddLSX(lsxname: String);
    procedure AddPred(pname: String; parity: TArity; pfunc: TExtPred);
    procedure InitPreds(PIptr: TPredInitPtr);
    procedure Load(xplname: String);
    procedure LoadXPL(xplname: String);
    function Main: Boolean;
    procedure Reset;
    procedure Close;
    procedure CloseLS;
    { Function and predicate parameters }
    procedure GetParm(n: integer; dt: TDType; p: pointer);
    function GetPStrParm(n: integer): string;
    function GetIntParm(n: integer): integer;
    function GetLongParm(n: integer): longint;
    function GetShortParm(n: integer): longint;
    function GetFloatParm(n: integer): double;
    function GetParmType(n: integer): TPType;
    function StrParmLen(n: integer): integer;
    function UnifyParm(n: integer; dt: TDType; p: pointer): Boolean;
    function UnifyPStrParm(n: integer; s: string): Boolean;
    function UnifyAtomParm(n: integer; s: string): Boolean;
    function UnifyIntParm(n: integer; i: integer): Boolean;
    function UnifyLongParm(n: integer; i: longint): Boolean;
    function UnifyShortParm(n: integer; i: longint): Boolean;
    function UnifyFloatParm(n: integer; f: double): Boolean;
    { Calling Prolog from Delphi }
    function Exec(var tp: TTerm): Boolean;
    function ExecStr(var tp: TTerm; s: PChar): Boolean;
    function ExecPStr(var tp: TTerm; s: string): Boolean;
    function Call(var tp: TTerm): Boolean;
    function CallStr(var tp: TTerm; s: PChar): Boolean;
    function CallPStr(var tp: TTerm; s: string): Boolean;
    function Redo: Boolean;
    procedure ClearCall;
    { Asserting and retracting }
    procedure Asserta(t: TTerm);
    procedure Assertz(t: TTerm);
    procedure Retract(t: TTerm);
    procedure AssertaStr(s: PChar);
    procedure AssertzStr(s: PChar);
    procedure RetractStr(s: PChar);
    procedure AssertaPStr(s: string);
    procedure AssertzPStr(s: string);
    procedure RetractPStr(s: string);
    { String/term conversion functions }
    procedure TermToStr(t: TTerm; s: PChar; n: integer);
    procedure TermToStrQ(t: TTerm; s: PChar; n: integer);
    procedure StrToTerm(var tp: TTerm; s: PChar);
    function TermToPStr(t: TTerm): string;
    function TermToPStrQ(t: TTerm): string;
    procedure PStrToTerm(var tp: TTerm; s: string);
    function StrTermLen(t: TTerm): integer;
     { Making Prolog types }
    procedure MakeAtom(var tp: TTerm; s: string);
    procedure MakeStr(var tp: TTerm; s: PChar);
    procedure MakePStr(var tp: TTerm; s: string);
    procedure MakeInt(var tp: TTerm; i: longint);
    procedure MakeFloat(var tp: TTerm; f: double);
    procedure MakeAddr(var tp: TTerm; p: pointer);
    { Getting C values from Prolog terms }
    function GetTermType(t: TTerm): TPType;
    procedure GetTerm(t: TTerm; dt: TDType; p: pointer);
    function GetPStrTerm(t: TTerm): string;
    function GetIntTerm(t: TTerm): integer;
    function GetLongTerm(t: TTerm): longint;
    function GetShortTerm(t: TTerm): longint;
    function GetFloatTerm(t: TTerm): double;
    { Structure hacking functions }
    procedure GetFA(t: TTerm; var s: string; var ap: TArity);
    function GetFunctor(t: TTerm): string;
    function GetArity(t: TTerm): integer;
    procedure MakeFA(var tp: TTerm; s: string; a: TArity);
    function UnifyArg(var tp: TTerm; n: integer; dt: TDType; p: pointer): Boolean;
    function UnifyPStrArg(var tp: TTerm; n: integer; s: string): Boolean;
    function UnifyAtomArg(var tp: TTerm; n: integer; s: string): Boolean;
    function UnifyIntArg(var tp: TTerm; n: integer; i: integer): Boolean;
    function UnifyLongArg(var tp: TTerm; n: integer; i: longint): Boolean;
    function UnifyShortArg(var tp: TTerm; n: integer; i: longint): Boolean;
    function UnifyFloatArg(var tp: TTerm; n: integer; f: double): Boolean;
    procedure GetArg(t: TTerm; n: integer; dt: TDType; p: pointer);
    function GetPStrArg(t: TTerm; n: integer): string;
    function GetIntArg(t: TTerm; n: integer): integer;
    function GetLongArg(t: TTerm; n: integer): longint;
    function GetShortArg(t: TTerm; n: integer): longint;
    function GetFloatArg(t: TTerm; n: integer): double;
    function GetArgType(t: TTerm; n: integer): TPType;
    function StrArgLen(t: TTerm; i: integer): integer;
    function Unify(t1: TTerm; t2: TTerm): Boolean;
    { List hacking functions }
    procedure MakeList(var tp: TTerm);
    procedure PushList(var tp: TTerm; t: TTerm);
    function PopList(var tp: TTerm; dt: TDType; p: pointer): TRC;
    function PopPStrList(var tp: TTerm; var s: string): TRC;
    function PopIntList(var tp: TTerm; var i: integer): TRC;
    function PopLongList(var tp: TTerm; var i: longint): TRC;
    function PopShortList(var tp: TTerm; var i: longint): TRC;
    function PopFloatList(var tp: TTerm; var f: double): TRC;
    function GetHead(t: TTerm; dt: TDType; p: pointer): TRC;
    function GetPStrHead(t: TTerm; var s: string): TRC;
    function GetIntHead(t: TTerm; var i: integer): TRC;
    function GetLongHead(t: TTerm; var i: longint): TRC;
    function GetShortHead(t: TTerm; var i: longint): TRC;
    function GetFloatHead(t: TTerm; var f: double): TRC;
    function GetTail(t: TTerm): TTerm;
    { Stream I/O functions }
    procedure SetStream(st: TPStream; i: integer);
    function GetStream(st: TPStream): integer;
    procedure SetInput(pfunc1: TGetC; pfunc2: TUngetC);
    procedure SetOutput(pfunc1: TPutC; pfunc2: TPutS);
    { Miscellaneous functions }
    procedure GetVersion(var s: string);
    function GetPVersion: string;
    function ARulesRegisterRuntime(method: longint; proxylist: string; runtimeid: string): integer;
    { Error handling functions }
    function GetExceptRC: TRC;
    procedure GetExceptMsg(s: PChar; l: integer);
    procedure GetExceptReadBuffer(s: PChar; l:integer);
    procedure GetExceptCallStack(s: PChar; l: integer);
  published
  end;

procedure Register;

implementation

{ Defines the actual DLL entry points for the Logic Server API.
  See the file AMZI.H for the complete C header file definition. }

{ Main entry points to set up Prolog environment }
function lsInitA(var eng: TEngID; xplname: PChar): TRC; stdcall; external 'ARULESRT.DLL';
function lsInit2A(var eng: TEngID; xplname: PChar): TRC; stdcall; external 'ARULESRT.DLL';
function lsInitLSX(eng: TEngID; p: pointer): TRC; stdcall; external 'ARULESRT.DLL';
function lsAddLSXA(eng: TEngID; lsxname: PChar; p: pointer): TRC; stdcall; external 'ARULESRT.DLL';
function lsAddPredA(eng: TEngID; pname: PChar; parity: TArity; pfunc: TExtPred; ptr: Pointer): TRC; stdcall; external 'ARULESRT.DLL';
function lsInitPredsA(eng: TEngID; PIptr: TPredInitPtr): TRC; stdcall; external 'ARULESRT.DLL';
function lsLoadA(eng: TEngID; xplname: PChar): TRC; stdcall; external 'ARULESRT.DLL';
function lsMain(eng: TEngID): TTFi; stdcall; external 'ARULESRT.DLL';
function lsReset(eng: TEngID): TRC; stdcall; external 'ARULESRT.DLL';
function lsClose(eng: TEngID): TRC; stdcall; external 'ARULESRT.DLL';
{ Function and predicate parameters }
function lsGetParm(eng: TEngID; n: integer; dt: TTypeInt; p: pointer): TRC; stdcall; external 'ARULESRT.DLL';
function lsGetParmType(eng: TEngID; n: integer): TTypeInt; stdcall; external 'ARULESRT.DLL';
function lsStrParmLen(eng: TEngID; n: integer): integer; stdcall; external 'ARULESRT.DLL';
function lsUnifyParm(eng: TEngID; n: integer; dt: TTypeInt; p: pointer): TTFi; stdcall; external 'ARULESRT.DLL';
{ Calling Prolog from Delphi }
function lsExec(eng: TEngID; var tp: TTerm): TTFi; stdcall; external 'ARULESRT.DLL';
function lsExecStrA(eng: TEngID; var tp: TTerm; s: PChar): TTFi; stdcall; external 'ARULESRT.DLL';
function lsCall(eng: TEngID; var tp: TTerm): TTFi; stdcall; external 'ARULESRT.DLL';
function lsCallStrA(eng: TEngID; var tp: TTerm; s: PChar): TTFi; stdcall; external 'ARULESRT.DLL';
function lsRedo(eng: TEngID): TTFi; stdcall; external 'ARULESRT.DLL';
function lsClearCall(eng: TEngID): TRC; stdcall; external 'ARULESRT.DLL';
{ Asserting and retracting }
function lsAsserta(eng: TEngID; t: TTerm): TRC; stdcall; external 'ARULESRT.DLL';
function lsAssertz(eng: TEngID; t: TTerm): TRC; stdcall; external 'ARULESRT.DLL';
function lsRetract(eng: TEngID; t: TTerm): TRC; stdcall; external 'ARULESRT.DLL';
function lsAssertaStrA(eng: TEngID; s: PChar): TRC; stdcall; external 'ARULESRT.DLL';
function lsAssertzStrA(eng: TEngID; s: PChar): TRC; stdcall; external 'ARULESRT.DLL';
function lsRetractStrA(eng: TEngID; s: PChar): TRC; stdcall; external 'ARULESRT.DLL';
{ String/term conversion functions }
function lsTermToStrA(eng: TEngID; t: TTerm; s: PChar; n: integer): TRC; stdcall; external 'ARULESRT.DLL';
function lsTermToStrQA(eng: TEngID; t: TTerm; s: PChar; n: integer): TRC; stdcall; external 'ARULESRT.DLL';
function lsStrToTermA(eng: TEngID; var tp: TTerm; s: PChar): TRC; stdcall; external 'ARULESRT.DLL';
{ Making Prolog types }
function lsMakeAtomA(eng: TEngID; var tp: TTerm; s: PChar): TRC; stdcall; external 'ARULESRT.DLL';
function lsMakeStrA(eng: TEngID; var tp: TTerm; s: PChar): TRC; stdcall; external 'ARULESRT.DLL';
function lsMakeInt(eng: TEngID; var tp: TTerm; i: longint): TRC; stdcall; external 'ARULESRT.DLL';
function lsMakeFloat(eng: TEngID; var tp: TTerm; f: double): TRC; stdcall; external 'ARULESRT.DLL';
function lsMakeAddr(eng: TEngID; var tp: TTerm; p: pointer): TRC; stdcall; external 'ARULESRT.DLL';
{ Getting C values from Prolog terms }
function lsGetTermType(eng: TEngID; t: TTerm): TTypeInt; stdcall; external 'ARULESRT.DLL';
function lsGetTerm(eng: TEngID; t: TTerm; dt: TTypeInt; p: pointer): TRC; stdcall; external 'ARULESRT.DLL';
function lsStrTermLen(eng: TEngID; t: TTerm): integer; stdcall; external 'ARULESRT.DLL';
{ Structure hacking functions }
function lsGetFAA(eng: TEngID; t: TTerm; s: PChar; var ap: TArity): TRC; stdcall; external 'ARULESRT.DLL';
function lsMakeFAA(eng: TEngID; var tp: TTerm; s: PChar; a: TArity): TRC; stdcall; external 'ARULESRT.DLL';
function lsUnifyArg(eng: TEngID; var tp: TTerm; n: integer; dt: TTypeInt; p: pointer): TTFi; stdcall; external 'ARULESRT.DLL';
function lsGetArg(eng: TEngID; t: TTerm; n: integer; dt: TTypeInt; p: pointer): TRC; stdcall; external 'ARULESRT.DLL';
function lsGetArgType(eng: TEngID; t: TTerm; n: integer): TTypeInt; stdcall; external 'ARULESRT.DLL';
function lsStrArgLen(eng: TEngID; t: TTerm; i: integer): integer; stdcall; external 'ARULESRT.DLL';
function lsUnify(eng: TEngID; t1: TTerm; t2: TTerm): TTFi; stdcall; external 'ARULESRT.DLL';
{ List hacking functions }
function lsMakeList(eng: TEngID; var tp: TTerm): TRC; stdcall; external 'ARULESRT.DLL';
function lsPushList(eng: TEngID; var tp: TTerm; t: TTerm): TRC; stdcall; external 'ARULESRT.DLL';
function lsPopList(eng: TEngID; var tp: TTerm; dt: TTypeInt; p: pointer): TRC; stdcall; external 'ARULESRT.DLL';
function lsGetHead(eng: TEngID; t: TTerm; dt: TTypeInt; p: pointer): TRC; stdcall; external 'ARULESRT.DLL';
function lsGetTail(eng: TEngID; t: TTerm): TTerm; stdcall; external 'ARULESRT.DLL';
{ Stream I/O functions }
function lsSetStream(eng: TEngID; st: TPStreamInt; i: integer): TRC; stdcall; external 'ARULESRT.DLL';
function lsGetStream(eng: TEngID; st: TPStreamInt): integer; stdcall; external 'ARULESRT.DLL';
function lsSetInput(eng: TEngID; pfunc1: TGetC; pfunc2: TUngetC): TRC; stdcall; external 'ARULESRT.DLL';
function lsSetOutputA(eng: TEngID; pfunc1: TPutC; pfunc2: TPutS): TRC; stdcall; external 'ARULESRT.DLL';
{ Miscellaneous functions }
function lsGetVersionA(eng: TEngID; s: PChar): TRC; stdcall; external 'ARULESRT.DLL';
function RegisterRuntime(eng: TEngID; method: longint; proxynames: string; runtimeid: string): TRC; stdcall; external 'ARULESRT.DLL';
{ Error handling functions }
function lsGetExceptRC(eng: TEngID): TRC; stdcall; external 'ARULESRT.DLL';
procedure lsGetExceptMsgA(eng: TEngID; s: PChar; l: integer) stdcall; external 'ARULESRT.DLL';
procedure lsGetExceptReadBufferA(eng: TEngID; s: PChar; l: integer) stdcall; external 'ARULESRT.DLL';
procedure lsGetExceptCallStackA(eng: TEngID; s: PChar; l: integer) stdcall; external 'ARULESRT.DLL';

constructor TLSARules.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  createdB := true;
  initializedB := false;
end;

destructor TLSARules.Destroy;
begin
  if initializedB then lsClose(eng);
  inherited Destroy;
end;

{ The function definitions map Logic Server methods to the
  actual DLL entry points. }

{ Main entry points to set up Prolog environment }

procedure TLSARules.Init(xplname: string);
begin
  InitLS(xplname);
end;

procedure TLSARules.InitLS(xplname: String);
begin
  if not createdB then LSError('LS not created', 0);
  if initializedB then lsClose(eng);
  StrPCopy(buf, xplname);
  rc := lsInitA(eng, buf);
  if rc <> 0 then LSError('lsInit', rc);
  initializedB := true;
end;

procedure TLSARules.InitLSX;
begin
  rc := lsInitLSX(eng, nil);
  if rc<> 0 then LSError('lsInitLSX', rc);
end;

procedure TLSARules.InitLSXP(p: pointer);
begin
  rc := lsInitLSX(eng, p);
  if rc<> 0 then LSError('lsInitLSX', rc);
end;

procedure TLSARules.AddLSX(lsxname: string);
begin
  StrPCopy(buf, lsxname);
  rc := lsAddLSXA(eng, buf, nil);
  if rc <> 0 then LSError('lsAddLSX', rc);
end;

procedure TLSARules.AddPred(pname: string; parity: TArity; pfunc: TExtPred);
begin
  StrPCopy(buf, pname);
  rc := lsAddPredA(eng, buf, parity, pfunc, Pointer(eng));
  if rc <> 0 then LSError('lsAddPred', rc);
end;

procedure TLSARules.InitPreds(PIptr: TPredInitPtr);
begin
  rc := lsInitPredsA(eng, PIptr);
  if rc <> 0 then LSError('lsInitPreds', rc);
end;

procedure TLSARules.Load(xplname: string);
begin
  LoadXPL(xplname);
end;

procedure TLSARules.LoadXPL(xplname: string);
begin
  StrPCopy(buf, xplname);
  rc := lsLoadA(eng, buf);
  if rc <> 0 then LSError('lsLoad', rc);
end;

function TLSARules.Main: Boolean;
begin
  Result := False;
  rc := lsMain(eng);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsMain', rc);
  end;
end;

procedure TLSARules.Reset;
begin
  rc := lsReset(eng);
  if rc <> 0 then LSError('lsReset', rc);
end;

procedure TLSARules.Close;
begin
  CloseLS;
end;

procedure TLSARules.CloseLS;
begin
  rc := lsClose(eng);
  if rc <> 0 then LSError('lsClose', rc);
  initializedB := false;
end;

{ Function and predicate parameters }

procedure TLSARules.GetParm(n: integer; dt: TDType; p: pointer);
begin
  rc := lsGetParm(eng, n, TTypeInt(dt), p);
  if rc <> 0 then LSError('lsGetParm', rc);
end;

function TLSARules.GetPStrParm(n: integer): string;
var
  pbuf: PChar;
begin
  pbuf := buf;
  rc := lsGetParm(eng, n, TTypeInt(dSTR), pbuf);
  if rc <> 0 then LSError('lsGetParm', rc);
  Result := StrPas(buf);
end;

function TLSARules.GetIntParm(n: integer): integer;
var
  i: integer;
begin
  rc := lsGetParm(eng, n, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetParm', rc);
  Result := i;
end;

function TLSARules.GetLongParm(n: integer): longint;
var
  i: longint;
begin
  rc := lsGetParm(eng, n, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetParm', rc);
  Result := i;
end;

function TLSARules.GetShortParm(n: integer): longint;
var
  i: longint;
begin
  rc := lsGetParm(eng, n, TTypeInt(dSHORT), @i);
  if rc <> 0 then LSError('lsGetParm', rc);
  Result := i;
end;

function TLSARules.GetFloatParm(n: integer): double;
var
  f: double;
begin
  rc := lsGetParm(eng, n, TTypeInt(dDOUBLE), @f);
  if rc <> 0 then LSError('lsGetParm', rc);
  Result := f;
end;

function TLSARules.GetParmType(n: integer): TPType;
begin
  Result := TPType(lsGetParmType(eng, n));
end;

function TLSARules.StrParmLen(n: integer): integer;
begin
  Result := lsStrParmLen(eng, n);
end;

function TLSARules.UnifyParm(n: integer; dt: TDType; p: pointer): Boolean;
begin
  Result := False;
  rc := lsUnifyParm(eng, n, TTypeInt(dt), p);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSARules.UnifyPStrParm(n: integer; s: string): Boolean;
begin
  Result := False;
  StrPCopy(buf, s);
  rc := lsUnifyParm(eng, n, TTypeInt(dSTR), @buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSARules.UnifyAtomParm(n: integer; s: string): Boolean;
begin
  Result := False;
  StrPCopy(buf, s);
  rc := lsUnifyParm(eng, n, TTypeInt(dATOM), @buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSARules.UnifyIntParm(n: integer; i: integer): Boolean;
begin
  Result := False;
  rc := lsUnifyParm(eng, n, TTypeInt(dLONG), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSARules.UnifyLongParm(n: integer; i: longint): Boolean;
begin
  Result := False;
  rc := lsUnifyParm(eng, n, TTypeInt(dLONG), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSARules.UnifyShortParm(n: integer; i: longint): Boolean;
begin
  Result := False;
  rc := lsUnifyParm(eng, n, TTypeInt(dSHORT), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;

function TLSARules.UnifyFloatParm(n: integer; f: double): Boolean;
begin
  Result := False;
  rc := lsUnifyParm(eng, n, TTypeInt(dDOUBLE), @f);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', rc);
  end;
end;


{ Calling Prolog from Delphi }

function TLSARules.Exec(var tp: TTerm): Boolean;
begin
  Result := False;
  rc := lsExec(eng, tp);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsExec', rc);
  end;
end;

function TLSARules.ExecStr(var tp: TTerm; s: PChar): Boolean;
begin
  Result := False;
  rc := lsExecStrA(eng, tp, s);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsExecStr', rc);
  end;
end;

function TLSARules.ExecPStr(var tp: TTerm; s: string): Boolean;
begin
  Result := False;
  StrPCopy(buf, s);
  rc := lsExecStrA(eng, tp, buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsExecStr', rc);
  end;
end;

function TLSARules.Call(var tp: TTerm): Boolean;
begin
  Result := False;
  rc := lsCall(eng, tp);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsCall', rc);
  end;
end;

function TLSARules.CallStr(var tp: TTerm; s: PChar): Boolean;
begin
  Result := False;
  rc := lsCallStrA(eng, tp, s);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsCallStr', rc);
  end;
end;

function TLSARules.CallPStr(var tp: TTerm; s: string): Boolean;
begin
  Result := False;
  StrPCopy(buf, s);
  rc := lsCallStrA(eng, tp, buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsCallStr', rc);
  end;
end;

function TLSARules.Redo: Boolean;
begin
  Result := False;
  rc := lsRedo(eng);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsRedo', rc);
  end;
end;

procedure TLSARules.ClearCall;
begin
  rc := lsClearCall(eng);
  if rc <> 0 then LSError('lsClearCall', rc);
end;

{ Asserting and retracting }

procedure TLSARules.Asserta(t: TTerm);
begin
  rc := lsAsserta(eng, t);
  if rc <> 0 then LSError('lsAsserta', rc);
end;

procedure TLSARules.Assertz(t: TTerm);
begin
  rc := lsAssertz(eng, t);
  if rc <> 0 then LSError('lsAssertz', rc);
end;

procedure TLSARules.Retract(t: TTerm);
begin
  rc := lsRetract(eng, t);
  if rc <> 0 then LSError('lsRetract', rc);
end;

procedure TLSARules.AssertaStr(s: PChar);
begin
  rc := lsAssertaStrA(eng, s);
  if rc <> 0 then LSError('lsAssertaStr', rc);
end;

procedure TLSARules.AssertzStr(s: PChar);
begin
  rc := lsAssertzStrA(eng, s);
  if rc <> 0 then LSError('lsAssertzStr', rc);
end;

procedure TLSARules.RetractStr(s: PChar);
begin
  rc := lsRetractStrA(eng, s);
  if rc <> 0 then LSError('lsRetractStr', rc);
end;

procedure TLSARules.AssertaPStr(s: string);
begin
  StrPCopy(buf, s);
  rc := lsAssertaStrA(eng, buf);
  if rc <> 0 then LSError('lsAssertaStr', rc);
end;

procedure TLSARules.AssertzPStr(s: string);
begin
  StrPCopy(buf, s);
  rc := lsAssertzStrA(eng, buf);
  if rc <> 0 then LSError('lsAssertzStr', rc);
end;

procedure TLSARules.RetractPStr(s: string);
begin
  StrPCopy(buf, s);
  rc := lsRetractStrA(eng, buf);
  if rc <> 0 then LSError('lsRetractStr', rc);
end;

{ String/term conversion functions }

procedure TLSARules.TermToStr(t: TTerm; s: PChar; n: integer);
begin
  rc := lsTermToStrA(eng, t, s, n);
  if rc <> 0 then LSError('lsTermToStr', rc);
end;

procedure TLSARules.TermToStrQ(t: TTerm; s: PChar; n: integer);
begin
  rc := lsTermToStrQA(eng, t, s, n);
  if rc <> 0 then LSError('lsTermToStrQ', rc);
end;

procedure TLSARules.StrToTerm(var tp: TTerm; s: PChar);
begin
  rc := lsStrToTermA(eng, tp, s);
  if rc <> 0 then LSError('lsStrToTerm', rc);
end;

function TLSARules.TermToPStr(t: TTerm): string;
begin
  rc := lsTermToStrA(eng, t, buf, 100000);
  if rc <> 0 then LSError('lsTermToStr', rc);
  Result := strpas(buf);
end;

function TLSARules.TermToPStrQ(t: TTerm): string;
begin
  rc := lsTermToStrQA(eng, t, buf, 100000);
  if rc <> 0 then LSError('lsTermToStrQ', rc);
  Result := strpas(buf);
end;

procedure TLSARules.PStrToTerm(var tp: TTerm; s: string);
begin
  StrPCopy(buf, s);
  rc := lsStrToTermA(eng, tp, buf);
  if rc <> 0 then LSError('lsStrToTerm', rc);
end;

function TLSARules.StrTermLen(t: TTerm): integer;
begin
  Result := lsStrTermLen(eng, t);
end;

{ Making Prolog types }

procedure TLSARules.MakeAtom(var tp: TTerm; s: string);
begin
  StrPCopy(buf, s);
  rc := lsMakeAtomA(eng, tp, buf);
  if rc <> 0 then LSError('lsMakeAtom', rc);
end;

procedure TLSARules.MakeStr(var tp: TTerm; s: PChar);
begin
  rc := lsMakeStrA(eng, tp, s);
  if rc <> 0 then LSError('lsMakeStr', rc);
end;

procedure TLSARules.MakePStr(var tp: TTerm; s: string);
begin
  StrPCopy(buf, s);
  rc := lsMakeStrA(eng, tp, buf);
  if rc <> 0 then LSError('lsMakeStr', rc);
end;

procedure TLSARules.MakeInt(var tp: TTerm; i: longint);
begin
  rc := lsMakeInt(eng, tp, i);
  if rc <> 0 then LSError('lsMakeInt', rc);
end;

procedure TLSARules.MakeFloat(var tp: TTerm; f: double);
begin
  rc := lsMakeFloat(eng, tp, f);
  if rc <> 0 then LSError('lsMakeFloat', rc);
end;

procedure TLSARules.MakeAddr(var tp: TTerm; p: pointer);
begin
  rc := lsMakeAddr(eng, tp, p);
  if rc <> 0 then LSError('lsMakeAddr', rc);
end;

{ Getting C values from Prolog terms }

function TLSARules.GetTermType(t: TTerm): TPType;
begin
  Result := TPType(lsGetTermType(eng, t));
end;

procedure TLSARules.GetTerm(t: TTerm; dt: TDType; p: pointer);
begin
  rc := lsGetTerm(eng, t, TTypeInt(dt), p);
  if rc <> 0 then LSError('lsGetTerm', rc);
end;

function TLSARules.GetPStrTerm(t: TTerm): string;
var
  pbuf: PChar;
begin
  pbuf := buf;
  rc := lsGetTerm(eng, t, TTypeInt(dSTR), pbuf);
  if rc <> 0 then LSError('lsGetTerm', rc);
  Result := StrPas(buf);
end;

function TLSARules.GetIntTerm(t: TTerm): integer;
var
  i: integer;
begin
  rc := lsGetTerm(eng, t, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetTerm', rc);
  Result := i;
end;

function TLSARules.GetLongTerm(t: TTerm): longint;
var
  i: longint;
begin
  rc := lsGetTerm(eng, t, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetTerm', rc);
  Result := i;
end;

function TLSARules.GetShortTerm(t: TTerm): longint;
var
  i: longint;
begin
  rc := lsGetTerm(eng, t, TTypeInt(dSHORT), @i);
  if rc <> 0 then LSError('lsGetTerm', rc);
  Result := i;
end;

function TLSARules.GetFloatTerm(t: TTerm): double;
var
  f: double;
begin
  rc := lsGetTerm(eng, t, TTypeInt(dDOUBLE), @f);
  if rc <> 0 then LSError('lsGetTerm', rc);
  Result := f;
end;

{ Structure hacking functions }

procedure TLSARules.GetFA(t: TTerm; var s: string; var ap: TArity);
begin
  rc := lsGetFAA(eng, t, buf, ap);
  if rc <> 0 then LSError('lsGetFA', rc);
  s := StrPas(buf);
end;

function TLSARules.GetFunctor(t: TTerm): string;
var
  ap: TArity;
begin
  rc := lsGetFAA(eng, t, buf, ap);
  if rc <> 0 then LSError('lsGetFunctor', rc);
  Result := StrPas(buf);
end;

function TLSARules.GetArity(t: TTerm): integer;
var
  ap: TArity;
begin
  rc := lsGetFAA(eng, t, buf, ap);
  if rc <> 0 then LSError('lsGetArity', rc);
  Result := ap;
end;

procedure TLSARules.MakeFA(var tp: TTerm; s: string; a: TArity);
begin
  StrPCopy(buf, s);
  rc := lsMakeFAA(eng, tp, buf, a);
  if rc <> 0 then LSError('lsMakeFA', rc);
end;

function TLSARules.UnifyArg(var tp: TTerm; n: integer; dt: TDType; p: pointer): Boolean;
begin
  Result := False;
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dt), p);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSARules.UnifyPStrArg(var tp: TTerm; n: integer; s: string): Boolean;
begin
  Result := False;
  StrPCopy(buf, s);
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dSTR), @buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSARules.UnifyAtomArg(var tp: TTerm; n: integer; s: string): Boolean;
begin
  Result := False;
  StrPCopy(buf, s);
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dATOM), @buf);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSARules.UnifyIntArg(var tp: TTerm; n: integer; i: integer): Boolean;
begin
  Result := False;
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dLONG), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSARules.UnifyLongArg(var tp: TTerm; n: integer; i: longint): Boolean;
begin
  Result := False;
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dLONG), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSARules.UnifyShortArg(var tp: TTerm; n: integer; i: longint): Boolean;
begin
  Result := False;
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dSHORT), @i);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;

function TLSARules.UnifyFloatArg(var tp: TTerm; n: integer; f: double): Boolean;
begin
  Result := False;
  rc := lsUnifyArg(eng, tp, n, TTypeInt(dDOUBLE), @f);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', rc);
  end;
end;


procedure TLSARules.GetArg(t: TTerm; n: integer; dt: TDType; p: pointer);
begin
  rc := lsGetArg(eng, t, n, TTypeInt(dt), p);
  if rc <> 0 then LSError('lsGetArg', rc);
end;

function TLSARules.GetPStrArg(t: TTerm; n: integer): string;
var
  pbuf: PChar;
begin
  pbuf := buf;
  rc := lsGetArg(eng, t, n, TTypeInt(dSTR), pbuf);
  if rc <> 0 then LSError('lsGetArg', rc);
  Result := StrPas(buf);
end;

function TLSARules.GetIntArg(t: TTerm; n: integer): integer;
var
  i: integer;
begin
  rc := lsGetArg(eng, t, n, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetArg', rc);
  Result := i;
end;

function TLSARules.GetLongArg(t: TTerm; n: integer): longint;
var
  i: longint;
begin
  rc := lsGetArg(eng, t, n, TTypeInt(dLONG), @i);
  if rc <> 0 then LSError('lsGetArg', rc);
  Result := i;
end;

function TLSARules.GetShortArg(t: TTerm; n: integer): longint;
var
  i: longint;
begin
  rc := lsGetArg(eng, t, n, TTypeInt(dSHORT), @i);
  if rc <> 0 then LSError('lsGetArg', rc);
  Result := i;
end;

function TLSARules.GetFloatArg(t: TTerm; n: integer): double;
var
  f: double;
begin
  rc := lsGetArg(eng, t, n, TTypeInt(dDOUBLE), @f);
  if rc <> 0 then LSError('lsGetArg', rc);
  Result := f;
end;

function TLSARules.GetArgType(t: TTerm; n: integer): TPType;
begin
  Result := TPType(lsGetArgType(eng, t, n));
end;

function TLSARules.StrArgLen(t: TTerm; i: integer): integer;
begin
  Result := lsStrArgLen(eng, t, i);
end;

function TLSARules.Unify(t1: TTerm; t2: TTerm): Boolean;
begin
  Result := False;
  rc := lsUnify(eng, t1, t2);
  case rc of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnify', rc);
  end;
end;

{ List hacking functions }

procedure TLSARules.MakeList(var tp: TTerm);
begin
  rc := lsMakeList(eng, tp);
  if rc <> 0 then LSError('lsMakeList', rc);
end;

procedure TLSARules.PushList(var tp: TTerm; t: TTerm);
begin
  rc := lsPushList(eng, tp, t);
  if rc <> 0 then LSError('lsPushList', rc);
end;

function TLSARules.PopList(var tp: TTerm; dt: TDType; p: pointer): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dt), p);
  Result := rc;
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSARules.PopPStrList(var tp: TTerm; var s: string): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dSTR), @buf);
  Result := rc;
  s := StrPas(buf);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSARules.PopIntList(var tp: TTerm; var i: integer): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dLONG), @i);
  Result := rc;
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSARules.PopLongList(var tp: TTerm; var i: longint): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dLONG), @i);
  Result := rc;
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSARules.PopShortList(var tp: TTerm; var i: longint): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dSHORT), @i);
  Result := rc;
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSARules.PopFloatList(var tp: TTerm; var f: double): TRC;
begin
  rc := lsPopList(eng, tp, TTypeInt(dDOUBLE), @f);
  Result := rc;
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsPopList', rc);
  end;
end;

function TLSARules.GetHead(t: TTerm; dt: TDType; p: pointer): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dt), p);
  Result := rc;
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSARules.GetPStrHead(t: TTerm; var s: string): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dSTR), @buf);
  Result := rc;
  s := StrPas(buf);
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSARules.GetIntHead(t: TTerm; var i: integer): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dLONG), @i);
  Result := rc;
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSARules.GetLongHead(t: TTerm; var i: longint): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dLONG), @i);
  Result := rc;
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSARules.GetShortHead(t: TTerm; var i: longint): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dSHORT), @i);
  Result := rc;
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSARules.GetFloatHead(t: TTerm; var f: double): TRC;
begin
  rc := lsGetHead(eng, t, TTypeInt(dDOUBLE), @f);
  Result := rc;
  case rc of
    0: Result := rc;
    -1: Result := rc;
    else LSError('lsGetHead', rc);
  end;
end;

function TLSARules.GetTail(t: TTerm): TTerm;
begin
  Result := lsGetTail(eng, t);
end;

{ Stream I/O functions }

procedure TLSARules.SetStream(st: TPStream; i: integer);
begin
  rc := lsSetStream(eng, TPStreamInt(st), i);
  if rc <> 0 then LSError('lsSetStream', rc);
end;

function TLSARules.GetStream(st: TPStream): integer;
begin
  rc := lsGetStream(eng, TPStreamInt(st));
  Result := rc;
end;

procedure TLSARules.SetInput(pfunc1: TGetC; pfunc2: TUngetC);
begin
  rc := lsSetInput(eng, pfunc1, pfunc2);
  if rc <> 0 then LSError('lsSetInput', rc);
end;

procedure TLSARules.SetOutput(pfunc1: TPutC; pfunc2: TPutS);
begin
  rc := lsSetOutputA(eng, pfunc1, pfunc2);
  if rc <> 0 then LSError('lsSetOutput', rc);
end;

{ Miscellaneous functions }

procedure TLSARules.GetVersion(var s: string);
begin
  rc := lsGetVersionA(eng, buf);
  if rc <> 0 then LSError('lsGetVersion', rc);
  s := StrPas(buf);
end;

function TLSARules.GetPVersion: string;
begin
  rc := lsGetVersionA(eng, buf);
  if rc <> 0 then LSError('lsGetVersion', rc);
  Result := StrPas(buf);
end;

function TLSARules.ARulesRegisterRuntime(method: longint; proxylist: string; runtimeid: string): integer;
var
  list: array[0..100000] of char;
begin
  StrPCopy(buf, runtimeid);
  StrPCopy(list, proxylist);
  rc := RegisterRuntime(eng, method, list, buf);
  Result := rc;
end;

{ Error handling functions }

procedure TLSARules.GetExceptMsg(s: PChar; l:integer);
begin
  lsGetExceptMsgA(eng, s, l);
end;

function TLSARules.GetExceptRC: TRC;
begin
  Result := lsGetExceptRC(eng);
end;

procedure TLSARules.GetExceptReadBuffer(s: PChar; l: integer);
begin
  lsGetExceptReadBufferA(eng, s, l);
  if rc <> 0 then LSError('lsGetExceptReadBuffer', rc);
end;


procedure TLSARules.GetExceptCallStack(s: PChar; l: integer);
begin
  lsGetExceptCallStackA(eng, s, l);
  if rc <> 0 then LSError('lsGetExceptCallStack', rc);
end;

{ Non-Logic Server functions }

{ Error handling for most logic server functions.
  An exception is raised and the logic server is closed.
  This is important as it frees up all the memory allocated
  by the logic server. }

procedure TLSARules.LSError(apiname: String; rc: integer);
var
  s: array[0..256] of char;
  ps: PChar;
  ss: String;
begin
  ps := s;
  lsGetExceptMsgA(eng, ps, 255);
  ss := strpas(ps);
  raise ELogicServer.Create(apiname + ': ' + IntToStr(rc) + ' ' + ss);
end;

procedure Register;
begin
  RegisterComponents('Additional', [TLSARules]);
end;

end.


