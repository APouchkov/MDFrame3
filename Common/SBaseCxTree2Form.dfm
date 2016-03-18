inherited SBaseCxTree2Frm: TSBaseCxTree2Frm
  Left = 164
  Top = 56
  ClientWidth = 843
  ExplicitWidth = 859
  ExplicitHeight = 561
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TdxMDStatusBar
    Width = 843
    ExplicitWidth = 843
  end
  inherited PanelMain: TcxGroupBox
    ExplicitWidth = 843
    Width = 843
    object SplitterLeft: TSplitter [0]
      Left = 260
      Top = 0
      Height = 477
      ExplicitLeft = 1000
      ExplicitHeight = 449
    end
    inherited PanelData: TcxGroupBox
      Left = 263
      ExplicitLeft = 263
      ExplicitWidth = 580
      Width = 580
      inherited PanelDetails: TcxGroupBox
        ExplicitWidth = 580
        Width = 580
      end
      inherited SplitterDetails: TcxSplitter
        Width = 580
        ExplicitWidth = 580
      end
      inherited PanelGrid: TcxGroupBox
        Top = 24
        OnResize = PanelGridResize
        ExplicitTop = 24
        ExplicitWidth = 580
        ExplicitHeight = 370
        Height = 370
        Width = 580
        inherited cxDBTreeList: TcxMDDBTreeList
          Width = 580
          Height = 370
          ExplicitWidth = 580
          ExplicitHeight = 370
        end
      end
      object PanelMainCaption: TPanel
        Left = 0
        Top = 0
        Width = 580
        Height = 24
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        ParentBackground = False
        TabOrder = 3
        DesignSize = (
          580
          24)
        object LbTreeNode: TLabel
          Left = 10
          Top = 1
          Width = 565
          Height = 23
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoSize = False
          Caption = #1057#1087#1080#1089#1086#1082
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
        end
        object PanelMainTopGray: TPanel
          Left = 0
          Top = 0
          Width = 580
          Height = 1
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
        end
      end
    end
    object PanelLeft: TcxGroupBox
      Left = 0
      Top = 0
      Align = alLeft
      PanelStyle.Active = True
      Style.BorderStyle = ebsNone
      TabOrder = 1
      Height = 477
      Width = 260
      object cxTree: TcxMDDBTreeList
        Left = 0
        Top = 24
        Width = 260
        Height = 453
        Align = alClient
        Bands = <
          item
          end>
        DataController.DataSource = TreeSource
        DataController.ImageIndexField = 'image'
        DataController.ParentField = 'parent_id'
        DataController.KeyField = 'id'
        Navigator.Buttons.CustomButtons = <>
        OptionsView.GridLines = tlglNone
        ParentFont = True
        RootValue = -1
        TabOrder = 0
        OnCanFocusNode = cxTreeCanFocusNode
        OnCollapsing = cxTreeCollapsing
        OnCustomDrawDataCell = cxTreeCustomDrawDataCell
        OnFocusedNodeChanged = cxTreeFocusedNodeChanged
        OnGetNodeImageIndex = cxTreeGetNodeImageIndex
        object cxTreeStandardColumn: TcxMDDBTreeListColumn
          PropertiesClassName = 'TcxTextEditProperties'
          Properties.AutoSelect = False
          Caption.AlignHorz = taCenter
          DataBinding.FieldName = 'Text'
          Options.AutoWidthSizable = True
          Options.Customizing = False
          Options.Editing = False
          Options.Moving = False
          Options.Sorting = False
          Width = 240
          Position.ColIndex = 0
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object cxTreeFocusedImage: TcxMDDBTreeListColumn
          Visible = False
          Caption.AlignHorz = taCenter
          DataBinding.FieldName = 'ImageFocused'
          Options.CellEndEllipsis = False
          Options.Footer = False
          Options.GroupFooter = False
          Options.Hidden = True
          Options.Sizing = False
          Options.VertSizing = False
          Options.Customizing = False
          Options.Editing = False
          Options.Focusing = False
          Options.IncSearch = False
          Options.Moving = False
          Options.Sorting = False
          Options.TabStop = False
          Position.ColIndex = -1
          Position.RowIndex = -1
          Position.BandIndex = -1
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
      end
      object PanelLeftCaption: TPanel
        Left = 0
        Top = 0
        Width = 260
        Height = 24
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        ParentBackground = False
        TabOrder = 1
        DesignSize = (
          260
          24)
        object LbGroups: TLabel
          Left = 20
          Top = 1
          Width = 238
          Height = 23
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoSize = False
          Caption = #1043#1088#1091#1087#1087#1099
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWhite
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          Layout = tlCenter
        end
        object btnTreeTypeChange: TSpeedButton
          Left = 0
          Top = 4
          Width = 17
          Height = 17
          Hint = #1048#1085#1086#1081' '#1074#1072#1088#1080#1072#1085#1090' '#1076#1077#1088#1077#1074#1072
          Flat = True
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00008080000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000808000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
            C000C0C0C000C0C0C000C0C0C00000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00C0C0C00000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000808000FFFFFF0000000000000000000000000080000000800000008000
            0000C0C0C000FFFFFF00C0C0C00000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000808000FFFFFF0000000000C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
            C000FFFFFF00FFFFFF00C0C0C00000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000808000FFFFFF0000000000FFFFFF00C0C0C00080000000800000008000
            0000C0C0C000FFFFFF00C0C0C00000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000808000FFFFFF0000000000C0C0C000FFFFFF0080000000FFFFFF00C0C0
            C000FFFFFF00FFFFFF00C0C0C00000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000808000FFFFFF0000000000000000000000000000000000800000008000
            0000C0C0C000FFFFFF00C0C0C00000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000808000FFFFFF0000000000C0C0C000FFFFFF00C0C0C000FFFFFF00C0C0
            C000FFFFFF00FFFFFF00C0C0C00000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000808000FFFFFF0000000000000000000000000080000000C0C0C000FFFF
            FF00FFFFFF00FFFFFF00C0C0C00000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000808000FFFFFF0000000000C0C0C000FFFFFF00C0C0C000FFFFFF00FFFF
            FF0000808000808080008080800000000000FF00FF00FF00FF00FF00FF00FF00
            FF0000808000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF0000808000C0C0C00080808000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF0000808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF000080800080808000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00008080000080800000808000008080000080800000808000008080000080
            800000808000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
          Margin = 0
          ParentShowHint = False
          ShowHint = True
          Spacing = 0
        end
        object PanelLeftTopGray: TPanel
          Left = 0
          Top = 0
          Width = 260
          Height = 1
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
        end
      end
    end
  end
  inherited dxBarDockControl: TdxMDBarDockControl
    Width = 843
    ExplicitWidth = 843
  end
  inherited BarManager: TdxMDBarManager
    Categories.Strings = (
      #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
      #1052#1077#1085#1102' '#1043#1083#1072#1074#1085#1086#1077
      #1052#1077#1085#1102' '#1042#1072#1088#1080#1072#1085#1090#1086#1074' '#1044#1077#1088#1077#1074#1072)
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
          BeginGroup = True
          ItemName = 'mnTree'
        end
        item
          ItemName = 'mnTreeChildsIncluded'
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
        end
        item
          ItemName = 'mnSubmit'
        end>
    end
    object mnTree: TdxBarButton [13]
      Category = 0
      Visible = ivAlways
      ButtonStyle = bsChecked
      Down = True
      OnClick = mnTreeClick
    end
    object mnTreeChildsIncluded: TdxBarButton [14]
      Category = 0
      Enabled = False
      Visible = ivNever
      ButtonStyle = bsChecked
      Down = True
      OnClick = mnTreeChildsIncludedClick
    end
    object mnFindInTree: TdxBarButton [15]
      Category = 0
      Visible = ivNever
    end
    object mnGroups: TdxBarListItem
      Caption = #1042#1072#1088#1080#1072#1085#1090#1099' '#1044#1077#1088#1077#1074#1072
      Category = 2
      Visible = ivAlways
      OnClick = mnGroupsClick
    end
  end
  inherited ActionList: TActionList
    Left = 336
    Top = 112
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
        ItemName = 'mnTree'
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
        ItemName = 'mnRefresh'
      end
      item
        BeginGroup = True
        ItemName = 'mnMarkAsDeleted'
      end
      item
        ItemName = 'mnDelete'
      end>
  end
  inherited DataSetProvider: TSQLDataSetProvider
    Level = 3
  end
  inherited DetailSetProvider: TSQLDataSetProvider
    Level = 4
  end
  object PopupMenuGroups: TdxBarPopupMenu
    BarManager = BarManager
    ItemLinks = <
      item
        ItemName = 'mnGroups'
      end>
    UseOwnFont = False
    Left = 222
    Top = 24
  end
  object TreeSource: TDataSource
    DataSet = TreeSetProvider
    Left = 144
    Top = 144
  end
  object TreeSetProvider: TSQLDataSetProvider
    Level = 2
    AutoCalcFields = False
    Params = <>
    FilterOptions = [foCaseInsensitive]
    AutoGenerateUpdateScripts = True
    DataSetObject = bdsoCDS
    OnPrepareParams = DataSetProviderPrepareParams
    Left = 174
    Top = 144
  end
  object GroupSetProvider: TSQLDataSetProvider
    Level = 1
    AutoCalcFields = False
    Params = <>
    FilterOptions = [foCaseInsensitive]
    DataSetObject = bdsoCDSCached
    AfterOpen = GroupSetProviderAfterOpen
    OnPrepareParams = DataSetProviderPrepareParams
    Left = 174
    Top = 112
  end
end
