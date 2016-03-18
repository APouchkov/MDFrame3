inherited SBaseWizardFrm: TSBaseWizardFrm
  Left = 158
  Top = 52
  ClientHeight = 388
  ClientWidth = 575
  ExplicitLeft = 158
  ExplicitTop = 52
  ExplicitWidth = 591
  ExplicitHeight = 426
  PixelsPerInch = 96
  TextHeight = 13
  object WizardControl: TdxMDWizardControl [0]
    Left = 0
    Top = 0
    Width = 575
    Height = 388
    Buttons.Back.Caption = '&'#1053#1072#1079#1072#1076
    Buttons.Cancel.Caption = '&'#1054#1090#1084#1077#1085#1080#1090#1100
    Buttons.CustomButtons.Buttons = <>
    Buttons.Finish.Caption = '&'#1043#1086#1090#1086#1074#1086
    Buttons.Help.Caption = '&'#1055#1086#1084#1086#1097#1100
    Buttons.Help.Visible = False
    Buttons.Next.Caption = '&'#1044#1072#1083#1077#1077
    OnButtonClick = WizardControlButtonClick
    object WizardControlPageStart: TdxMDWizardControlPage
      Header.Description = #1054#1087#1080#1089#1072#1085#1080#1077
      Header.Title = #1052#1072#1089#1090#1077#1088
    end
  end
  inherited BarManager: TdxBarManager
    Left = 28
    Top = 88
    DockControlHeights = (
      0
      0
      0
      0)
  end
  inherited ActionList: TActionList
    Left = 88
    Top = 88
  end
  object DataSetMain: TSQLDataSetProvider
    Params = <>
    Left = 211
    Top = 82
  end
  object DataSourceMain: TDataSource
    DataSet = DataSetMain
    Left = 259
    Top = 108
  end
end
