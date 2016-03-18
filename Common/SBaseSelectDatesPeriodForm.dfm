inherited SBaseSelectDatesPeriodFrm: TSBaseSelectDatesPeriodFrm
  Left = 454
  Top = 53
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1042#1074#1086#1076' '#1087#1077#1088#1080#1086#1076#1072' '#1074#1099#1073#1086#1088#1082#1080' '#1076#1072#1090
  ClientHeight = 163
  ClientWidth = 440
  Position = poOwnerFormCenter
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  ExplicitLeft = 454
  ExplicitTop = 53
  ExplicitWidth = 456
  ExplicitHeight = 201
  PixelsPerInch = 96
  TextHeight = 13
  object tInfo: TPanel [0]
    Left = 10
    Top = 8
    Width = 420
    Height = 18
    BevelOuter = bvLowered
    Caption = #1042#1099#1073#1088#1072#1085' '#1087#1077#1088#1080#1086#1076' '#1089' .... '#1087#1086' ....'
    TabOrder = 15
  end
  object t1: TRadioButton [1]
    Tag = 1
    Left = 10
    Top = 40
    Width = 80
    Height = 12
    Caption = #1050#1074#1072#1088#1090#1072#1083
    TabOrder = 4
    OnClick = tXClick
  end
  object t2: TRadioButton [2]
    Tag = 2
    Left = 10
    Top = 65
    Width = 80
    Height = 14
    Caption = #1052#1077#1089#1103#1094
    TabOrder = 5
    OnClick = tXClick
  end
  object t3: TRadioButton [3]
    Tag = 3
    Left = 10
    Top = 90
    Width = 80
    Height = 14
    Caption = #1044#1072#1090#1072
    Checked = True
    TabOrder = 8
    TabStop = True
    OnClick = tXClick
  end
  object t4: TRadioButton [4]
    Tag = 4
    Left = 10
    Top = 115
    Width = 80
    Height = 14
    Caption = #1048#1085#1090#1077#1088#1074#1072#1083
    TabOrder = 9
    OnClick = tXClick
  end
  object f1: TEdit [5]
    Left = 100
    Top = 36
    Width = 110
    Height = 21
    AutoSelect = False
    Color = clBtnFace
    TabOrder = 7
    OnClick = f1Click
  end
  object f2: TEdit [6]
    Left = 100
    Top = 61
    Width = 110
    Height = 21
    AutoSelect = False
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 6
    OnClick = f2Click
  end
  object UD1: TUpDown [7]
    Left = 191
    Top = 38
    Width = 17
    Height = 18
    Min = -100
    TabOrder = 16
    OnClick = UD1Click
  end
  object UD2: TUpDown [8]
    Left = 191
    Top = 63
    Width = 17
    Height = 18
    Min = -100
    TabOrder = 17
    OnClick = UD2Click
  end
  object btnOk: TButton [9]
    Left = 350
    Top = 38
    Width = 80
    Height = 21
    Caption = #1054#1050
    Default = True
    ModalResult = 1
    TabOrder = 13
    OnClick = btnOkClick
  end
  object btnCancel: TButton [10]
    Left = 350
    Top = 63
    Width = 80
    Height = 21
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 14
  end
  object t5: TRadioButton [11]
    Tag = 5
    Left = 225
    Top = 40
    Width = 113
    Height = 14
    Caption = #1057' '#1085#1072#1095#1072#1083#1072' '#1075#1086#1076#1072
    TabOrder = 10
    OnClick = tXClick
  end
  object t6: TRadioButton [12]
    Tag = 6
    Left = 225
    Top = 65
    Width = 115
    Height = 14
    Caption = #1057' '#1085#1072#1095#1072#1083#1072' '#1082#1074#1072#1088#1090#1072#1083#1072
    TabOrder = 11
    OnClick = tXClick
  end
  object t7: TRadioButton [13]
    Tag = 7
    Left = 225
    Top = 90
    Width = 115
    Height = 14
    Caption = #1057' '#1085#1072#1095#1072#1083#1072' '#1084#1077#1089#1103#1094#1072
    TabOrder = 12
    OnClick = tXClick
  end
  object t8: TRadioButton [14]
    Tag = 8
    Left = 225
    Top = 120
    Width = 80
    Height = 12
    Caption = #1043#1086#1076
    TabOrder = 0
    Visible = False
    OnClick = tXClick
  end
  object f8: TEdit [15]
    Left = 315
    Top = 116
    Width = 110
    Height = 21
    AutoSelect = False
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
    Visible = False
  end
  object ud8: TUpDown [16]
    Left = 406
    Top = 118
    Width = 17
    Height = 18
    Min = -100
    TabOrder = 18
    Visible = False
    OnClick = UD8Click
  end
  object t9: TRadioButton [17]
    Tag = 9
    Left = 225
    Top = 144
    Width = 80
    Height = 12
    Caption = #1055#1086#1083#1091#1075#1086#1076#1080#1077
    TabOrder = 2
    Visible = False
    OnClick = tXClick
  end
  object f9: TEdit [18]
    Left = 315
    Top = 140
    Width = 110
    Height = 21
    AutoSelect = False
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
    Visible = False
  end
  object ud9: TUpDown [19]
    Left = 406
    Top = 142
    Width = 17
    Height = 18
    Min = -100
    TabOrder = 19
    Visible = False
    OnClick = UD9Click
  end
  object f3: TcxDateEdit [20]
    Left = 100
    Top = 86
    Properties.ReadOnly = True
    Properties.OnEditValueChanged = f3Change
    Style.Color = clBtnFace
    TabOrder = 20
    OnClick = f3Click
    Width = 110
  end
  object f41: TcxDateEdit [21]
    Left = 100
    Top = 111
    Properties.ReadOnly = True
    Properties.OnEditValueChanged = f41Change
    Style.Color = clBtnFace
    TabOrder = 21
    OnClick = f41Click
    Width = 110
  end
  object f42: TcxDateEdit [22]
    Left = 100
    Top = 136
    Properties.ReadOnly = True
    Properties.OnEditValueChanged = f42Change
    Style.Color = clBtnFace
    TabOrder = 22
    OnClick = f42Click
    Width = 110
  end
  inherited BarManager: TdxMDBarManager
    DockControlHeights = (
      0
      0
      0
      0)
  end
end
