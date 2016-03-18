inherited SBaseSelectFrm: TSBaseSelectFrm
  Left = 434
  Top = 61
  ClientHeight = 562
  ClientWidth = 584
  ExplicitWidth = 600
  ExplicitHeight = 600
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TdxMDStatusBar
    Top = 542
    Width = 584
    ExplicitTop = 542
    ExplicitWidth = 584
  end
  inherited PanelMain: TcxGroupBox
    ExplicitWidth = 584
    ExplicitHeight = 516
    Height = 516
    Width = 584
    inherited PanelData: TcxGroupBox
      ExplicitWidth = 584
      ExplicitHeight = 516
      Height = 516
      Width = 584
      inherited PanelDetails: TcxGroupBox
        Top = 436
        ExplicitTop = 436
        ExplicitWidth = 584
        Width = 584
      end
      inherited SplitterDetails: TcxSplitter
        Top = 433
        Width = 584
        ExplicitTop = 433
        ExplicitWidth = 584
      end
      inherited PanelGrid: TcxGroupBox
        ExplicitWidth = 584
        ExplicitHeight = 433
        Height = 433
        Width = 584
        object cxGrid: TcxMDGrid
          Left = 0
          Top = 0
          Width = 584
          Height = 433
          Align = alClient
          BorderStyle = cxcbsNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          TabOrder = 0
          object cxGridLevel: TcxMDGridLevel
            Options.DetailFrameWidth = 0
          end
        end
      end
    end
  end
  inherited dxBarDockControl: TdxMDBarDockControl
    Width = 584
    ExplicitWidth = 584
  end
  inherited BarManager: TdxMDBarManager
    Categories.Strings = (
      #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
      #1052#1077#1085#1102' '#1043#1083#1072#1074#1085#1086#1077
      #1052#1077#1085#1102' '#1044#1077#1081#1089#1090#1074#1080#1081)
    Categories.ItemsVisibles = (
      2
      2
      3)
    Categories.Visibles = (
      True
      True
      True)
    DockControlHeights = (
      0
      0
      0
      0)
    inherited ToolBar: TdxBar
      ItemLinks = <
        item
          ItemName = 'mnPriorLevel'
        end
        item
          ItemName = 'mnNext'
        end
        item
          ItemName = 'mnFormClose'
        end
        item
          ItemName = 'mnSelect'
        end>
    end
    inherited mnNew: TdxMDBarButton [18]
    end
    inherited mnView: TdxMDBarButton [19]
    end
    inherited mnEdit: TdxMDBarButton [20]
    end
    inherited mnCopy: TdxMDBarButton [21]
    end
    inherited mnRefresh: TdxBarButton [22]
    end
    inherited mnRefreshRecord: TdxBarButton [23]
    end
    inherited mnMarkAsDeleted: TdxBarButton [24]
    end
    inherited mnDelete: TdxBarButton [25]
    end
    inherited mnTake: TdxBarButton [26]
      Visible = ivNever
    end
    inherited mnSelect: TdxBarButton [27]
      Align = iaRight
      Category = 2
      ShortCut = 0
    end
    object mnPriorLevel: TdxBarButton
      Category = 2
      Enabled = False
      Visible = ivAlways
      PaintStyle = psCaptionGlyph
      ShortCut = 16392
      OnClick = mnPriorLevelClick
    end
    object mnNext: TdxBarButton
      Category = 2
      Enabled = False
      Visible = ivAlways
      PaintStyle = psCaptionGlyph
      ShortCut = 13
    end
    object mnNextLevel: TdxBarButton
      Category = 2
      Visible = ivNever
    end
    object LevelChanged: TdxBarButton
      Category = 2
      Visible = ivNever
    end
  end
end
