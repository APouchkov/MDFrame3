inherited SBaseCxCustomTreeFrm: TSBaseCxCustomTreeFrm
  Left = 199
  Top = 143
  ClientWidth = 1084
  ExplicitLeft = 199
  ExplicitTop = 143
  ExplicitWidth = 1100
  ExplicitHeight = 561
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TdxMDStatusBar
    Width = 1084
    ExplicitWidth = 913
  end
  inherited PanelMain: TcxGroupBox
    ExplicitWidth = 1084
    Width = 1084
    inherited PanelData: TcxGroupBox
      ExplicitWidth = 1084
      Width = 1084
      inherited PanelDetails: TcxGroupBox
        ExplicitTop = 369
        ExplicitWidth = 1084
        Width = 1084
      end
      inherited SplitterDetails: TcxSplitter
        Width = 1084
        ExplicitTop = 366
        ExplicitWidth = 913
      end
      inherited PanelGrid: TcxGroupBox
        ExplicitWidth = 1084
        ExplicitHeight = 319
        Width = 1084
        inherited cxDBTreeList: TcxMDDBTreeList
          Width = 1084
          OptionsBehavior.CellHints = False
          OptionsBehavior.DragCollapse = False
          OptionsBehavior.DragExpand = False
          OptionsBehavior.IncSearch = False
          OptionsBehavior.MultiSort = False
          OptionsBehavior.Sorting = False
          OptionsCustomizing.BandCustomizing = False
          OptionsCustomizing.ColumnCustomizing = False
          OptionsSelection.CellSelect = False
          OptionsSelection.HideFocusRect = True
          OptionsSelection.HideSelection = True
          OptionsView.Indicator = True
          OnCustomDrawDataCell = cxDBTreeListCustomDrawDataCell
          ExplicitWidth = 911
        end
      end
    end
  end
  inherited BarManager: TdxBarManager
    DockControlHeights = (
      0
      0
      26
      0)
  end
end
