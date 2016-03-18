inherited SBaseCxView2Frm: TSBaseCxView2Frm
  Left = 118
  Top = 67
  ClientWidth = 1212
  ExplicitWidth = 1228
  ExplicitHeight = 561
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TdxMDStatusBar
    Width = 1212
    ExplicitWidth = 1212
  end
  inherited PanelMain: TcxGroupBox
    ExplicitWidth = 1212
    Width = 1212
    inherited PanelData: TcxGroupBox
      ExplicitWidth = 1212
      Width = 1212
      inherited PanelDetails: TcxGroupBox
        ExplicitWidth = 1212
        Width = 1212
      end
      inherited SplitterDetails: TcxSplitter
        Width = 1212
        ExplicitWidth = 1212
      end
      inherited PanelGrid: TcxGroupBox
        ExplicitWidth = 1212
        Width = 1212
        inherited cxGrid: TcxMDGrid
          Width = 1212
          ExplicitWidth = 1212
          inherited cxGridLevel: TcxMDGridLevel
            object cxGridLevel2: TcxMDGridLevel
              Options.DetailFrameWidth = 0
              OnAddView = cxGridLevel2AddView
              OnViewChanged = cxGridLevel2ViewChanged
            end
          end
        end
      end
    end
  end
  inherited dxBarDockControl: TdxMDBarDockControl
    Width = 1212
    ExplicitWidth = 1212
  end
  inherited BarManager: TdxMDBarManager
    AllowCallFromAnotherForm = True
    Categories.Strings = (
      #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
      #1052#1077#1085#1102' '#1043#1083#1072#1074#1085#1086#1077
      #1052#1077#1085#1102' '#1042#1090#1086#1088#1086#1075#1086' '#1059#1088#1086#1074#1085#1103
      #1052#1077#1085#1102' '#1057#1091#1073#1083#1080#1084#1080#1088#1086#1074#1072#1085#1085#1086#1077)
    Categories.ItemsVisibles = (
      2
      2
      2
      3)
    Categories.Visibles = (
      True
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
          ItemName = 'mnNewX'
        end
        item
          BeginGroup = True
          ItemName = 'mnViewX'
        end
        item
          ItemName = 'mnEditX'
        end
        item
          BeginGroup = True
          ItemName = 'mnMarkAsDeletedX'
        end
        item
          ItemName = 'mnDeleteX'
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
        end
        item
          ItemName = 'mnSubmit'
        end>
    end
    inherited mnView: TdxMDBarButton
      ShortCut = 0
    end
    inherited mnEdit: TdxMDBarButton
      ShortCut = 0
    end
    inherited mnDelete: TdxBarButton
      ShortCut = 0
    end
    object mnNew2: TdxBarButton
      Category = 2
      Enabled = False
      Visible = ivAlways
      ShortCut = 16429
    end
    object mnView2: TdxBarButton
      Category = 2
      Enabled = False
      Visible = ivAlways
    end
    object mnEdit2: TdxBarButton
      Category = 2
      Enabled = False
      Visible = ivAlways
    end
    object mnCopy2: TdxBarButton
      Category = 2
      Enabled = False
      Visible = ivAlways
    end
    object mnRefreshRecord2: TdxBarButton
      Category = 2
      Enabled = False
      Visible = ivAlways
    end
    object mnDelete2: TdxBarButton
      Category = 2
      Enabled = False
      Visible = ivAlways
    end
    object mnMarkAsDeleted2: TdxBarButton
      Category = 2
      Enabled = False
      Visible = ivAlways
    end
    object mnNewX: TdxBarSubItem
      Category = 3
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
    object mnViewX: TdxBarButton
      Category = 3
      Enabled = False
      Visible = ivAlways
      ShortCut = 114
    end
    object mnEditX: TdxBarButton
      Category = 3
      Enabled = False
      Visible = ivAlways
      ShortCut = 115
    end
    object mnCopyX: TdxBarButton
      Category = 3
      Enabled = False
      Visible = ivAlways
      ShortCut = 16451
    end
    object mnRefreshRecordX: TdxBarButton
      Category = 3
      Visible = ivAlways
    end
    object mnDeleteX: TdxBarButton
      Category = 3
      Enabled = False
      Visible = ivAlways
      ShortCut = 16430
    end
    object mnMarkAsDeletedX: TdxBarButton
      Category = 3
      Enabled = False
      Visible = ivAlways
    end
  end
  inherited PopupMenu: TdxBarPopupMenu
    ItemLinks = <
      item
        ItemName = 'mnSelect'
      end
      item
        BeginGroup = True
        ItemName = 'mnNewX'
      end
      item
        ItemName = 'mnViewX'
      end
      item
        ItemName = 'mnEditX'
      end
      item
        ItemName = 'mnCopyX'
      end
      item
        BeginGroup = True
        ItemName = 'mnRefresh'
      end
      item
        BeginGroup = True
        ItemName = 'mnMarkAsDeleted'
      end
      item
        ItemName = 'mnDeleteX'
      end>
  end
  inherited DetailSetProvider: TSQLDataSetProvider
    Level = 4
  end
  object DataSource2: TDataSource
    DataSet = DataSetProvider2
    Left = 408
    Top = 147
  end
  object DataSetProvider2: TSQLDataSetProvider
    Level = 3
    AutoCalcFields = False
    Params = <>
    FilterOptions = [foCaseInsensitive]
    Left = 438
    Top = 144
  end
end
