program Advice;

uses
  Forms,
  AdviceUnit in 'AdviceUnit.pas' {MainForm},
  ARulesXL in '..\..\arulesxl.pas',
  Arules in '..\..\arules.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
