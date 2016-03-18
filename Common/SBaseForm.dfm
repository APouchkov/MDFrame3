object SBaseFrm: TSBaseFrm
  Left = 206
  Top = 75
  ClientHeight = 416
  ClientWidth = 695
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BarManager: TdxMDBarManager
    AllowReset = False
    AlwaysSaveText = True
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    CanCustomize = False
    Categories.Strings = (
      #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074)
    Categories.ItemsVisibles = (
      3)
    Categories.Visibles = (
      True)
    PopupMenuLinks = <>
    UseSystemFont = False
    Left = 20
    DockControlHeights = (
      0
      0
      0
      0)
    object mnExcel: TdxBarButton
      Category = 0
      Visible = ivAlways
    end
  end
  object ActionList: TActionList
    Left = 64
    object ActionObjectInspector: TAction
      ShortCut = 49225
      OnExecute = ActionObjectInspectorExecute
    end
    object ActionScriptDebugger: TAction
      ShortCut = 49235
      OnExecute = ActionScriptDebuggerExecute
    end
  end
end
