unit AdviceUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ARulesXL, StdCtrls, Buttons, Grids, ValEdit, DateUtils, Arules;

type
  TMainForm = class(TForm)
    QueryBtn: TBitBtn;
    AdviceMemo: TMemo;
    Label1: TLabel;
    ClubType: TComboBox;
    SwingSpeed: TEdit;
    Label3: TLabel;
    ls: TLSARules;
    Label4: TLabel;
    Label2: TLabel;
    Favor: TComboBox;
    Label5: TLabel;
    BallFlight: TComboBox;
    procedure QueryBtnClick(Sender: TObject);
  private
    { Private declarations }
    arxl: TARulesXL;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.QueryBtnClick(Sender: TObject);
var
  s: string;
begin
  { Start ARulesXL and open the exported rule set }
  arxl := TARulesXL.Create(Owner);
  arxl.OpenRules('advice.axl', '');

  { Assert the user input data to in[?] }
  arxl.ClearVector('ShaftRules', 'in');
  arxl.AddToVector('ShaftRules', 'in', 'Swing Speed', SwingSpeed.Text);
  arxl.AddToVector('ShaftRules', 'in', 'Favor', Favor.SelText);
  arxl.AddToVector('ShaftRules', 'in', 'Club Type', ClubType.SelText);
  arxl.AddToVector('ShaftRules', 'in', 'Ball Flight', BallFlight.SelText);

  { Query ARulesXL }
  s := arxl.QueryRules('ShaftRules', 'FIND advice');

  { Set the memo to the query result }
  AdviceMemo.Text := s;

  { Close rule engine }
  arxl.CloseRules();
end;

end.
