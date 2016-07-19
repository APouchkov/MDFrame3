inherited SBaseCxCustomTreeFrm: TSBaseCxCustomTreeFrm
  Left = 199
  Top = 143
  ClientWidth = 1084
  ExplicitWidth = 1100
  ExplicitHeight = 561
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TdxMDStatusBar
    Width = 1084
    ExplicitWidth = 1084
  end
  inherited PanelMain: TcxGroupBox
    ExplicitWidth = 1084
    Width = 1084
    inherited PanelData: TcxGroupBox
      ExplicitWidth = 1084
      Width = 1084
      inherited PanelDetails: TcxGroupBox
        ExplicitWidth = 1084
        Width = 1084
      end
      inherited SplitterDetails: TcxSplitter
        Width = 1084
        ExplicitWidth = 1084
      end
      inherited PanelGrid: TcxGroupBox
        ExplicitWidth = 1084
        Width = 1084
        inherited cxDBTreeList: TcxMDDBTreeList
          Width = 1084
          OptionsBehavior.CellHints = False
          OptionsBehavior.DragCollapse = False
          OptionsBehavior.DragExpand = False
          OptionsBehavior.IncSearch = False
          OptionsBehavior.MultiSort = False
          OptionsCustomizing.BandCustomizing = False
          OptionsCustomizing.ColumnCustomizing = False
          OptionsSelection.CellSelect = False
          OptionsSelection.HideFocusRect = True
          OptionsSelection.HideSelection = True
          OptionsView.Indicator = True
          OnCustomDrawDataCell = cxDBTreeListCustomDrawDataCell
          ExplicitWidth = 1084
        end
      end
    end
  end
  inherited dxBarDockControl: TdxMDBarDockControl
    Width = 1084
    ExplicitWidth = 1084
  end
  inherited BarManager: TdxMDBarManager
    DockControlHeights = (
      0
      0
      0
      0)
  end
end
