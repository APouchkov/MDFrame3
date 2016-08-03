inherited SBaseCxTreeFrm: TSBaseCxTreeFrm
  Left = 194
  Top = 75
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelMain: TcxGroupBox
    inherited PanelData: TcxGroupBox
      inherited PanelGrid: TcxGroupBox
        object cxDBTreeList: TcxMDDBTreeList
          Left = 0
          Top = 0
          Width = 1184
          Height = 394
          OnNodeAssignValues = cxDBTreeListNodeAssignValues
          Align = alClient
          DataController.DataSource = DataSource
          Navigator.Buttons.CustomButtons = <>
          ParentFont = True
          PopupMenu = PopupMenu
          TabOrder = 0
          OnCollapsing = cxDBTreeListCollapsing
          OnDataChanged = DBGridDataChanged
          OnDragOver = cxDBTreeListDragOver
          OnFocusedNodeChanged = cxDBTreeListFocusedNodeChanged
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
          ItemName = 'mnNewX'
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
          ItemName = 'mnExpandAll'
        end
        item
          ItemName = 'mnCollapseAll'
        end
        item
          BeginGroup = True
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
    inherited FilterBar: TdxBar
      Row = 0
    end
    inherited mnFitToWidth: TdxBarButton
      OnClick = mnFitToWidthClick
    end
    inherited mnFitToHeight: TdxBarButton
      OnClick = mnFitToHeightClick
    end
    inherited mnExpandAll: TdxBarButton
      Visible = ivAlways
      OnClick = mnExpandAllClick
    end
    inherited mnCollapseAll: TdxBarButton
      Visible = ivAlways
      OnClick = mnCollapseAllClick
    end
    inherited mnProperties: TdxBarButton
      OnClick = mnPropertiesClick
    end
    object mnNew2: TdxMDBarButton
      Category = 1
      Enabled = False
      Visible = ivAlways
      ShortCut = 16429
    end
    object mnNewX: TdxBarSubItem
      Category = 1
      Enabled = False
      Visible = ivAlways
      ShowCaption = False
      ItemLinks = <
        item
          ItemName = 'mnNew'
        end
        item
          ItemName = 'mnNew2'
        end>
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
        ItemName = 'mnNewX'
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
end
