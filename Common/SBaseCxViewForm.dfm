inherited SBaseCxViewFrm: TSBaseCxViewFrm
  Left = 428
  Top = 77
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelMain: TcxGroupBox
    inherited PanelData: TcxGroupBox
      inherited PanelGrid: TcxGroupBox
        object cxGrid: TcxMDGrid
          Left = 0
          Top = 0
          Width = 1184
          Height = 394
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          TabOrder = 0
          OnActiveTabChanged = cxGridActiveTabChanged
          OnFocusedViewChanged = cxGridFocusedViewChanged
          object cxGridLevel: TcxMDGridLevel
            Options.DetailFrameWidth = 0
            OnAddView = cxGridLevelOnAddView
            OnViewChanged = cxGridLevelViewChanged
          end
        end
      end
    end
  end
  inherited BarManager: TdxMDBarManager
    DockControlHeights = (
      0
      0
      0
      0)
    inherited ToolBar: TdxBar
      ItemLinks = <
        item
          ItemName = 'mnNew'
        end
        item
          ItemName = 'mnView'
        end
        item
          ItemName = 'mnEdit'
        end
        item
          ItemName = 'mnCopy'
        end
        item
          BeginGroup = True
          ItemName = 'mnMarkAsDeleted'
        end
        item
          ItemName = 'mnDelete'
        end
        item
          ItemName = 'mnRefresh'
        end
        item
          BeginGroup = True
          ItemName = 'mnFilter'
        end
        item
          ItemName = 'mnGroupBox'
        end
        item
          ItemName = 'mnGroupingClear'
        end
        item
          ItemName = 'mnExpandAll'
        end
        item
          ItemName = 'mnCollapseAll'
        end
        item
          ItemName = 'mnDetails'
        end
        item
          BeginGroup = True
          ItemName = 'mnFitToWidth'
        end
        item
          ItemName = 'mnFitToHeight'
        end
        item
          ItemName = 'mnFitToScreen'
        end
        item
          BeginGroup = True
          ItemName = 'mnProperties'
        end>
    end
    inherited mnFitToWidth: TdxBarButton
      OnClick = mnFitToWidthClick
    end
    inherited mnFitToHeight: TdxBarButton
      OnClick = mnFitToHeightClick
    end
    inherited mnGroupBox: TdxBarButton
      ButtonStyle = bsChecked
      OnClick = mnGroupBoxClick
    end
    inherited mnGroupingClear: TdxBarButton
      OnClick = mnGroupingClearClick
    end
    inherited mnExpandAll: TdxBarButton
      Enabled = False
      OnClick = mnExpandAllClick
    end
    inherited mnCollapseAll: TdxBarButton
      Enabled = False
      OnClick = mnCollapseAllClick
    end
  end
  inherited PopupMenu: TdxBarPopupMenu
    ItemLinks = <
      item
        Position = ipBeginsNewRow
        ItemName = 'mnSelect'
      end
      item
        BeginGroup = True
        Position = ipBeginsNewRow
        ItemName = 'mnNew'
      end
      item
        Position = ipBeginsNewRow
        ItemName = 'mnView'
      end
      item
        Position = ipBeginsNewRow
        ItemName = 'mnEdit'
      end
      item
        ItemName = 'mnCopy'
      end
      item
        BeginGroup = True
        ItemName = 'mnGroupBox'
      end
      item
        ItemName = 'mnGroupingClear'
      end
      item
        ItemName = 'mnExpandAll'
      end
      item
        ItemName = 'mnCollapseAll'
      end
      item
        BeginGroup = True
        Position = ipBeginsNewRow
        ItemName = 'mnRefresh'
      end
      item
        BeginGroup = True
        Position = ipBeginsNewRow
        ItemName = 'mnMarkAsDeleted'
      end
      item
        Position = ipBeginsNewRow
        ItemName = 'mnDelete'
      end>
  end
  inherited TimerDetails: TTimer
    Left = 305
  end
end
