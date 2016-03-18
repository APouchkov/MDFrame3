inherited SBaseCxTreeViewImportFrm: TSBaseCxTreeViewImportFrm
  Left = 312
  Top = 87
  ClientWidth = 1003
  ExplicitLeft = 312
  ExplicitTop = 87
  ExplicitWidth = 1019
  ExplicitHeight = 561
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TdxMDStatusBar
    Width = 1003
    ExplicitWidth = 1003
  end
  inherited PanelMain: TcxGroupBox
    Padding.Left = 5
    Padding.Top = 3
    Padding.Right = 5
    Padding.Bottom = 5
    ExplicitWidth = 1003
    Width = 1003
    object SplitterLeft: TSplitter [0]
      Left = 265
      Top = 3
      Height = 469
    end
    inherited PanelData: TcxGroupBox
      Left = 268
      Top = 3
      Padding.Left = 5
      Padding.Right = 5
      Caption = ' '#1056#1077#1079#1091#1083#1100#1090#1072#1090#1099' '#1089#1074#1077#1088#1082#1080' '
      PanelStyle.Active = False
      ParentFont = False
      Style.BorderStyle = ebsFlat
      Style.Font.Style = [fsBold]
      Style.IsFontAssigned = True
      ExplicitLeft = 268
      ExplicitTop = 3
      ExplicitWidth = 730
      ExplicitHeight = 469
      Height = 469
      Width = 730
      inherited PanelDetails: TcxGroupBox
        Left = 7
        Top = 387
        ExplicitLeft = 5
        ExplicitTop = 369
        ExplicitWidth = 716
        Width = 716
      end
      inherited SplitterDetails: TcxSplitter
        Left = 7
        Top = 384
        Width = 716
        ExplicitLeft = 5
        ExplicitTop = 366
        ExplicitWidth = 582
      end
      inherited PanelGrid: TcxGroupBox
        Left = 7
        Top = 16
        ExplicitLeft = 5
        ExplicitTop = 71
        ExplicitWidth = 716
        ExplicitHeight = 368
        Height = 368
        Width = 716
        object ReportTabControl: TSBaseReportTabControl [0]
          Left = 0
          Top = 0
          Width = 716
          Height = 368
          Items = <>
          Grid = cxGrid
          Caption = ''
          Info = 'Info'
          Footer = #1054#1073#1085#1072#1088#1091#1078#1077#1085#1086' '#1088#1072#1089#1093#1086#1078#1076#1077#1085#1080#1081':'
          Align = alClient
          Focusable = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          LookAndFeel.Kind = lfUltraFlat
          LookAndFeel.NativeStyle = False
          TabSlants.Positions = [spLeft, spRight]
          ClientRectBottom = 368
          ClientRectRight = 716
          ClientRectTop = 0
        end
        inherited cxGrid: TcxMDGrid
          Width = 716
          Height = 368
          TabOrder = 1
          ExplicitWidth = 716
          ExplicitHeight = 302
        end
      end
    end
    object PanelLeft: TcxGroupBox
      Left = 5
      Top = 3
      Align = alLeft
      Padding.Left = 5
      Padding.Right = 5
      Caption = ' '#1048#1089#1090#1086#1095#1085#1080#1082#1080' '#1076#1072#1085#1085#1099#1093' '
      ParentFont = False
      Style.BorderStyle = ebsFlat
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = [fsBold]
      Style.IsFontAssigned = True
      TabOrder = 1
      Height = 469
      Width = 260
      object cxTree: TcxMDTreeList
        Left = 7
        Top = 16
        Width = 246
        Height = 451
        Align = alClient
        Bands = <
          item
          end>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        LookAndFeel.NativeStyle = True
        Navigator.Buttons.CustomButtons = <>
        OptionsBehavior.AlwaysExpandedLevel = -1
        OptionsCustomizing.ColumnHorzSizing = False
        OptionsSelection.CellSelect = False
        OptionsView.ColumnAutoWidth = True
        OptionsView.CheckGroups = True
        ParentColor = False
        PopupMenu = PopupMenuTree
        TabOrder = 0
        ExplicitTop = 3
        ExplicitHeight = 464
        object cxTreeColumnName: TcxTreeListColumn
          PropertiesClassName = 'TcxTextEditProperties'
          Properties.AutoSelect = False
          DataBinding.ValueType = 'String'
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
        object cxTreeColumnMask: TcxTreeListColumn
          Visible = False
          DataBinding.ValueType = 'String'
          Options.Hidden = True
          Position.ColIndex = 1
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object cxTreeColumnFile: TcxTreeListColumn
          Visible = False
          DataBinding.ValueType = 'String'
          Options.Hidden = True
          Position.ColIndex = 3
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object cxTreeColumnSource: TcxTreeListColumn
          Visible = False
          DataBinding.ValueType = 'Integer'
          Position.ColIndex = 2
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object cxTreeColumnMethod: TcxTreeListColumn
          Visible = False
          DataBinding.ValueType = 'String'
          Options.Hidden = True
          Width = 20
          Position.ColIndex = 4
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object cxTreeColumnTemplate_Id: TcxTreeListColumn
          Visible = False
          DataBinding.ValueType = 'Integer'
          Options.Hidden = True
          Position.ColIndex = 5
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
      end
    end
  end
  inherited BarManager: TdxBarManager
    Categories.Strings = (
      #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
      #1052#1077#1085#1102' '#1043#1083#1072#1074#1085#1086#1077
      #1052#1077#1085#1102' '#1042#1072#1088#1080#1072#1085#1090#1086#1074' '#1044#1077#1088#1077#1074#1072
      #1052#1077#1085#1102' '#1044#1077#1088#1077#1074#1072)
    Categories.ItemsVisibles = (
      2
      2
      3
      3)
    Categories.Visibles = (
      True
      True
      True
      True)
    DockControlHeights = (
      0
      0
      26
      0)
    inherited ToolBar: TdxBar
      ItemLinks = <
        item
          ItemName = 'mnGroupBox'
        end
        item
          ItemName = 'mnGroupingClear'
        end
        item
          BeginGroup = True
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
    object mnAddFileOrMask: TdxBarSubItem
      Category = 3
      Visible = ivAlways
      ItemLinks = <
        item
          ItemName = 'mnAddMask'
        end
        item
          ItemName = 'mnAddFile'
        end
        item
          ItemName = 'mnAddFolder'
        end>
    end
    object mnAddMask: TdxBarButton
      Category = 3
      Visible = ivAlways
      OnClick = mnAddMaskClick
    end
    object mnAddFile: TdxBarButton
      Category = 3
      Visible = ivAlways
      OnClick = mnAddFileClick
    end
    object mnAddFolder: TdxBarButton
      Category = 3
      Visible = ivAlways
      OnClick = mnAddFileClick
    end
    object mnSetFileOrMask: TdxBarSubItem
      Category = 3
      Visible = ivAlways
      ItemLinks = <
        item
          ItemName = 'mnSetMask'
        end
        item
          ItemName = 'mnSetFile'
        end
        item
          ItemName = 'mnSetFolder'
        end>
    end
    object mnSetMask: TdxBarButton
      Category = 3
      Visible = ivAlways
      OnClick = mnSetMaskClick
    end
    object mnSetFile: TdxBarButton
      Category = 3
      Visible = ivAlways
      OnClick = mnSetFileClick
    end
    object mnSetFolder: TdxBarButton
      Category = 3
      Visible = ivAlways
      OnClick = mnSetFileClick
    end
    object mnRefreshTree: TdxBarButton
      Category = 3
      Visible = ivAlways
      OnClick = mnRefreshTreeClick
    end
    object mnDeleteFileOrMask: TdxBarButton
      Category = 3
      Visible = ivAlways
      OnClick = mnDeleteFileOrMaskClick
    end
  end
  inherited ActionList: TActionList
    Left = 24
    Top = 72
  end
  object TreeSetProvider: TSQLDataSetProvider
    ReadOnly = True
    Params = <>
    DataSetObject = bdsoCDS
    KeyFieldNames = 'Source'
    AfterOpen = TreeSetProviderAfterOpen
    AfterClose = TreeSetProviderAfterOpen
    OnPrepareParams = DataSetProviderPrepareParams
    Left = 103
    Top = 168
    object TreeSetProviderSource: TIntegerField
      FieldName = 'Source'
    end
    object TreeSetProviderParent: TIntegerField
      FieldName = 'Parent'
    end
    object TreeSetProviderName: TWideStringField
      FieldName = 'Name'
      Size = 100
    end
    object TreeSetProviderMethod: TStringField
      FieldName = 'Method'
      Size = 1
    end
    object TreeSetProviderTemplate_Id: TWordField
      FieldName = 'Template_Id'
    end
    object TreeSetProviderTemplateType_Id: TWordField
      FieldName = 'TemplateType_Id'
    end
    object TreeSetProviderMask: TWideStringField
      FieldName = 'Mask'
      Size = 256
    end
    object TreeSetProviderImageIndex: TIntegerField
      FieldName = 'ImageIndex'
    end
    object TreeSetProviderParams: TWideMemoField
      FieldName = 'Params'
      BlobType = ftWideMemo
    end
  end
  object PopupMenuTree: TdxBarPopupMenu
    BarManager = BarManager
    ItemLinks = <
      item
        ItemName = 'mnAddFileOrMask'
      end
      item
        ItemName = 'mnSetFileOrMask'
      end
      item
        BeginGroup = True
        ItemName = 'mnRefreshTree'
      end
      item
        BeginGroup = True
        ItemName = 'mnDeleteFileOrMask'
      end>
    UseOwnFont = False
    OnPopup = PopupMenuTreePopup
    Left = 101
    Top = 221
  end
  object DataSetSources: TSQLDataSetProvider
    Params = <>
    CachedUpdates = False
    DataSetObject = bdsoCDS
    OnPrepareParams = DataSetProviderPrepareParams
    Left = 431
    Top = 192
    object DataSetSourcesIndex: TIntegerField
      FieldName = 'Index'
    end
    object DataSetSourcesSource: TIntegerField
      FieldName = 'Source'
    end
    object DataSetSourcesPath: TStringField
      FieldName = 'Path'
      Size = 200
    end
    object DataSetSourcesParams: TWideMemoField
      FieldName = 'Params'
      BlobType = ftWideMemo
    end
  end
  object TypesSetProvider: TSQLDataSetProvider
    Params = <>
    DataSetObject = bdsoCDSCached
    KeyFieldNames = 'Id'
    OnPrepareParams = DataSetProviderPrepareParams
    Left = 151
    Top = 168
  end
  object DataSourceSources: TDataSource
    DataSet = DataSetSources
    Left = 403
    Top = 192
  end
end
