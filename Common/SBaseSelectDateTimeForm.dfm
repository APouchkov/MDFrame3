inherited SBaseSelectDateFrm: TSBaseSelectDateFrm
  Left = 382
  Top = 63
  BorderStyle = bsDialog
  Caption = #1042#1099#1073#1086#1088' '#1076#1072#1090#1099
  ClientHeight = 39
  ClientWidth = 235
  KeyPreview = True
  OnKeyDown = FormKeyDown
  ExplicitLeft = 382
  ExplicitTop = 63
  ExplicitWidth = 251
  ExplicitHeight = 77
  DesignSize = (
    235
    39)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel: TBevel [0]
    Left = 0
    Top = 0
    Width = 235
    Height = 39
    Align = alClient
    Shape = bsFrame
  end
  object LabelDate: TLabel [1]
    Left = 10
    Top = 14
    Width = 26
    Height = 13
    Caption = #1044#1072#1090#1072
  end
  object ButtonOk: TButton [2]
    Left = 150
    Top = 10
    Width = 74
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Ok'
    Default = True
    TabOrder = 0
    TabStop = False
    OnClick = ButtonOkClick
  end
  object EditDate: TcxDateEdit [3]
    Left = 50
    Top = 10
    Properties.DateButtons = [btnClear, btnToday]
    Style.BorderStyle = ebsFlat
    Style.LookAndFeel.Kind = lfFlat
    StyleDisabled.LookAndFeel.Kind = lfFlat
    StyleFocused.BorderStyle = ebsFlat
    StyleFocused.LookAndFeel.Kind = lfFlat
    StyleHot.BorderStyle = ebsFlat
    StyleHot.LookAndFeel.Kind = lfFlat
    TabOrder = 1
    Width = 90
  end
  inherited BarManager: TdxBarManager
    DockControlHeights = (
      0
      0
      0
      0)
  end
end
