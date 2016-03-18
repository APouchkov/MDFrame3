inherited SBaseDfmView: TSBaseDfmView
  Left = 225
  Top = 85
  Caption = #1058#1077#1082#1089#1090' DFM '#1092#1086#1088#1084#1099
  ClientHeight = 662
  ClientWidth = 984
  KeyPreview = True
  ExplicitWidth = 1000
  ExplicitHeight = 700
  PixelsPerInch = 96
  TextHeight = 13
  object dxStatusBar: TdxMDStatusBar [0]
    Left = 0
    Top = 642
    Width = 984
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Fixed = False
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end
      item
        PanelStyleClassName = 'TdxStatusBarKeyboardStatePanelStyle'
        PanelStyle.CapsLockKeyAppearance.ActiveCaption = 'CAPS'
        PanelStyle.CapsLockKeyAppearance.InactiveCaption = 'CAPS'
        PanelStyle.NumLockKeyAppearance.ActiveCaption = 'NUM'
        PanelStyle.NumLockKeyAppearance.InactiveCaption = 'NUM'
        PanelStyle.ScrollLockKeyAppearance.ActiveCaption = 'SCRL'
        PanelStyle.ScrollLockKeyAppearance.InactiveCaption = 'SCRL'
        PanelStyle.InsertKeyAppearance.ActiveCaption = 'OVR'
        PanelStyle.InsertKeyAppearance.InactiveCaption = 'INS'
      end>
    DefaultPaintStyle = stpsOffice11
  end
  object SynEdit: TSynEdit [1]
    Left = 0
    Top = 0
    Width = 984
    Height = 642
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 5
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = MainDataModule.SynDfmSyn
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
    ReadOnly = True
    SearchEngine = MainDataModule.SynEditSearch
    OnSpecialLineColors = SynEditSpecialLineColors
    OnStatusChange = SynEditStatusChange
    FontSmoothing = fsmNone
  end
  inherited BarManager: TdxMDBarManager
    Left = 52
    Top = 88
    DockControlHeights = (
      0
      0
      0
      0)
  end
  inherited ActionList: TActionList
    Left = 54
    Top = 31
    object ActionFind: TAction
      ShortCut = 16454
    end
    object ActionFindNext: TAction
      ShortCut = 114
    end
  end
end
