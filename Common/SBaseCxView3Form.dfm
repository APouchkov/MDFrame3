inherited SBaseCxView3Frm: TSBaseCxView3Frm
  Left = 142
  Top = 64
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelMain: TcxGroupBox
    inherited PanelData: TcxGroupBox
      inherited PanelGrid: TcxGroupBox
        inherited cxGrid: TcxMDGrid
          inherited cxGridLevel: TcxMDGridLevel
            inherited cxGridLevel2: TcxMDGridLevel
              object cxGridLevel3: TcxMDGridLevel
                Options.DetailFrameWidth = 0
                OnAddView = cxGridLevel3AddView
                OnViewChanged = cxGridLevel3ViewChanged
              end
            end
          end
        end
      end
    end
  end
  inherited BarManager: TdxMDBarManager
    Categories.Strings = (
      #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
      #1052#1077#1085#1102' '#1043#1083#1072#1074#1085#1086#1077
      #1052#1077#1085#1102' '#1042#1090#1086#1088#1086#1075#1086' '#1059#1088#1086#1074#1085#1103
      #1052#1077#1085#1102' '#1057#1091#1073#1083#1080#1084#1080#1088#1086#1074#1072#1085#1085#1086#1077
      #1052#1077#1085#1102' '#1058#1088#1077#1090#1100#1077#1075#1086' '#1059#1088#1086#1074#1085#1103)
    Categories.ItemsVisibles = (
      2
      2
      2
      3
      3)
    Categories.Visibles = (
      True
      True
      True
      True
      True)
    DockControlHeights = (
      0
      0
      0
      0)
    inherited mnNew2: TdxBarButton
      ShortCut = 0
    end
    inherited mnNewX: TdxBarSubItem
      ItemLinks = <
        item
          ItemName = 'mnNew'
        end
        item
          ItemName = 'mnNew2'
        end
        item
          ItemName = 'mnNew3'
        end>
    end
    object mnNew3: TdxBarButton
      Category = 4
      Enabled = False
      Visible = ivAlways
    end
    object mnView3: TdxBarButton
      Category = 4
      Enabled = False
      Visible = ivAlways
    end
    object mnEdit3: TdxBarButton
      Category = 4
      Enabled = False
      Visible = ivAlways
    end
    object mnCopy3: TdxBarButton
      Category = 4
      Enabled = False
      Visible = ivAlways
    end
    object mnRefreshRecord3: TdxBarButton
      Category = 4
      Enabled = False
      Visible = ivAlways
    end
    object mnDelete3: TdxBarButton
      Category = 4
      Enabled = False
      Visible = ivAlways
    end
    object mnMarkAsDeleted3: TdxBarButton
      Category = 4
      Enabled = False
      Visible = ivAlways
    end
  end
  inherited DetailSetProvider: TSQLDataSetProvider
    Level = 5
  end
  inherited DataSetProvider2: TSQLDataSetProvider
    Top = 147
  end
  object DataSetProvider3: TSQLDataSetProvider
    Level = 4
    AutoCalcFields = False
    Params = <>
    FilterOptions = [foCaseInsensitive]
    Left = 438
    Top = 186
  end
  object DataSource3: TDataSource
    DataSet = DataSetProvider3
    Left = 408
    Top = 187
  end
end
