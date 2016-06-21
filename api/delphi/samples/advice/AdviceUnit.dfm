object MainForm: TMainForm
  Left = 223
  Top = 679
  Width = 506
  Height = 253
  Caption = 'Shaft Advisor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 136
    Width = 36
    Height = 13
    Caption = 'Advice:'
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 66
    Height = 13
    Caption = 'Swing Speed:'
  end
  object Label4: TLabel
    Left = 8
    Top = 40
    Width = 51
    Height = 13
    Caption = 'Club Type:'
  end
  object Label2: TLabel
    Left = 8
    Top = 72
    Width = 30
    Height = 13
    Caption = 'Favor:'
  end
  object Label5: TLabel
    Left = 8
    Top = 104
    Width = 48
    Height = 13
    Caption = 'Ball Flight:'
  end
  object QueryBtn: TBitBtn
    Left = 416
    Top = 8
    Width = 73
    Height = 25
    Caption = 'Query'
    TabOrder = 0
    OnClick = QueryBtnClick
  end
  object AdviceMemo: TMemo
    Left = 8
    Top = 152
    Width = 481
    Height = 57
    Lines.Strings = (
      '')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object ClubType: TComboBox
    Left = 80
    Top = 40
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    Items.Strings = (
      'Driver < 11 Degrees'
      'Driver >= 11 Degrees'
      'Fairway Wood'
      'Hybrid/Utility'
      'Iron'
      'Wedge')
  end
  object SwingSpeed: TEdit
    Left = 80
    Top = 8
    Width = 113
    Height = 17
    TabOrder = 3
  end
  object Favor: TComboBox
    Left = 80
    Top = 72
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      'Distance'
      'Accuracy')
  end
  object BallFlight: TComboBox
    Left = 80
    Top = 104
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    Items.Strings = (
      'Normal'
      'High'
      'Low')
  end
  object ls: TLSARules
    Left = 456
    Top = 40
  end
end
