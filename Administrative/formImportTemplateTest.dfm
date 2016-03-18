inherited frmImportTemplateTest: TfrmImportTemplateTest
  Left = 158
  Top = 55
  ExplicitLeft = 158
  ExplicitTop = 55
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelMain: TcxGroupBox
    ExplicitTop = 28
    ExplicitWidth = 1302
    ExplicitHeight = 475
    inherited PanelFilter: TPanel
      Height = 140
      ExplicitHeight = 140
      object pnlInputParams: TPanel
        Left = 1
        Top = 1
        Width = 270
        Height = 138
        Align = alLeft
        BevelOuter = bvNone
        Padding.Left = 5
        Padding.Right = 5
        Padding.Bottom = 5
        TabOrder = 0
        object lbInputParams: TcxLabel
          Left = 5
          Top = 0
          Align = alTop
          Caption = #1042#1093#1086#1076#1085#1099#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1099
          ParentFont = False
          Style.Font.Charset = RUSSIAN_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'MS Sans Serif'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
        end
        object cxGridInputParams: TcxGrid
          Left = 5
          Top = 17
          Width = 260
          Height = 116
          Align = alClient
          TabOrder = 1
          object cxGridInputParamsTableView: TcxGridTableView
            Synchronization = False
            Navigator.Buttons.CustomButtons = <>
            DataController.Filter.Options = [fcoCaseInsensitive]
            DataController.Options = [dcoCaseInsensitive, dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding]
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            Filtering.ColumnMRUItemsList = False
            OptionsBehavior.CellHints = True
            OptionsBehavior.ImmediateEditor = False
            OptionsBehavior.IncSearch = True
            OptionsBehavior.ExpandMasterRowOnDblClick = False
            OptionsCustomize.ColumnFiltering = False
            OptionsCustomize.ColumnGrouping = False
            OptionsCustomize.ColumnHidingOnGrouping = False
            OptionsCustomize.ColumnMoving = False
            OptionsCustomize.ColumnSorting = False
            OptionsCustomize.ColumnsQuickCustomizationReordering = qcrDisabled
            OptionsData.Appending = True
            OptionsData.CancelOnExit = False
            OptionsData.DeletingConfirmation = False
            OptionsSelection.HideSelection = True
            OptionsSelection.InvertSelect = False
            OptionsView.CellEndEllipsis = True
            OptionsView.ColumnAutoWidth = True
            OptionsView.ExpandButtonsForEmptyDetails = False
            OptionsView.GridLineColor = clSilver
            OptionsView.GroupByBox = False
            OptionsView.HeaderEndEllipsis = True
            object cxGridInputParamsColumnName: TcxGridColumn
              Caption = #1048#1084#1103
              Options.AutoWidth = False
              Width = 100
            end
            object cxGridInputParamsColumnValue: TcxGridColumn
              Caption = #1047#1085#1072#1095#1077#1085#1080#1077
              DataBinding.ValueType = 'Variant'
              PropertiesClassName = 'TcxButtonEditProperties'
              Properties.OnButtonClick = cxGridInputParamsColumnValuePropertiesButtonClick
              Properties.Buttons = <
                item
                  Default = True
                  Kind = bkEllipsis
                end>
              Width = 130
            end
          end
          object cxGridInputParamsLevel: TcxGridLevel
            GridView = cxGridInputParamsTableView
          end
        end
      end
      object pnlOutputParams: TPanel
        Left = 271
        Top = 1
        Width = 270
        Height = 138
        Align = alLeft
        BevelOuter = bvNone
        Padding.Left = 5
        Padding.Right = 5
        Padding.Bottom = 5
        TabOrder = 1
        object lbOutputParams: TcxLabel
          Left = 5
          Top = 0
          Align = alTop
          Caption = #1042#1099#1093#1086#1076#1085#1099#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1099
          ParentFont = False
          Style.Font.Charset = RUSSIAN_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'MS Sans Serif'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
        end
        object cxGridOutputParams: TcxGrid
          Left = 5
          Top = 17
          Width = 260
          Height = 116
          Align = alClient
          TabOrder = 1
          object cxGridTableViewOutputParams: TcxGridTableView
            Synchronization = False
            Navigator.Buttons.CustomButtons = <>
            DataController.Filter.Options = [fcoCaseInsensitive]
            DataController.Options = [dcoCaseInsensitive, dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding]
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            Filtering.ColumnMRUItemsList = False
            OptionsBehavior.CellHints = True
            OptionsBehavior.ImmediateEditor = False
            OptionsBehavior.IncSearch = True
            OptionsBehavior.ExpandMasterRowOnDblClick = False
            OptionsCustomize.ColumnFiltering = False
            OptionsCustomize.ColumnGrouping = False
            OptionsCustomize.ColumnHidingOnGrouping = False
            OptionsCustomize.ColumnMoving = False
            OptionsCustomize.ColumnSorting = False
            OptionsCustomize.ColumnsQuickCustomizationReordering = qcrDisabled
            OptionsData.CancelOnExit = False
            OptionsData.Deleting = False
            OptionsData.DeletingConfirmation = False
            OptionsData.Editing = False
            OptionsData.Inserting = False
            OptionsSelection.HideSelection = True
            OptionsSelection.InvertSelect = False
            OptionsView.CellEndEllipsis = True
            OptionsView.ColumnAutoWidth = True
            OptionsView.ExpandButtonsForEmptyDetails = False
            OptionsView.GridLineColor = clSilver
            OptionsView.GroupByBox = False
            OptionsView.HeaderEndEllipsis = True
            object cxGridOutputParamsColumnName: TcxGridColumn
              Caption = #1048#1084#1103
              Options.AutoWidth = False
              Width = 100
            end
            object cxGridOutputParamsColumnValue: TcxGridColumn
              Caption = #1047#1085#1072#1095#1077#1085#1080#1077
              DataBinding.ValueType = 'Variant'
              Width = 130
            end
          end
          object cxGridLevelOutputParams: TcxGridLevel
            GridView = cxGridTableViewOutputParams
          end
        end
      end
      object pnlMessages: TPanel
        Left = 541
        Top = 1
        Width = 642
        Height = 138
        Align = alClient
        BevelOuter = bvNone
        Padding.Left = 5
        Padding.Right = 5
        Padding.Bottom = 5
        TabOrder = 2
        object lbMessages: TcxLabel
          Left = 5
          Top = 0
          Align = alTop
          Caption = #1057#1086#1086#1073#1097#1077#1085#1080#1103' '#1087#1072#1088#1089#1077#1088#1072
          ParentFont = False
          Style.Font.Charset = RUSSIAN_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'MS Sans Serif'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
        end
        object cxGridMessages: TcxGrid
          Left = 5
          Top = 17
          Width = 632
          Height = 116
          Align = alClient
          TabOrder = 1
          object cxGridTableViewMessages: TcxGridTableView
            Synchronization = False
            Navigator.Buttons.CustomButtons = <>
            DataController.Filter.Options = [fcoCaseInsensitive]
            DataController.Options = [dcoCaseInsensitive, dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding]
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            Filtering.ColumnMRUItemsList = False
            OptionsBehavior.CellHints = True
            OptionsBehavior.ImmediateEditor = False
            OptionsBehavior.IncSearch = True
            OptionsBehavior.ExpandMasterRowOnDblClick = False
            OptionsCustomize.ColumnFiltering = False
            OptionsCustomize.ColumnGrouping = False
            OptionsCustomize.ColumnHidingOnGrouping = False
            OptionsCustomize.ColumnMoving = False
            OptionsCustomize.ColumnSorting = False
            OptionsCustomize.ColumnsQuickCustomizationReordering = qcrDisabled
            OptionsData.CancelOnExit = False
            OptionsData.Deleting = False
            OptionsData.DeletingConfirmation = False
            OptionsData.Editing = False
            OptionsData.Inserting = False
            OptionsSelection.HideSelection = True
            OptionsSelection.InvertSelect = False
            OptionsView.CellEndEllipsis = True
            OptionsView.ColumnAutoWidth = True
            OptionsView.ExpandButtonsForEmptyDetails = False
            OptionsView.GridLineColor = clSilver
            OptionsView.GroupByBox = False
            OptionsView.HeaderEndEllipsis = True
            object cxGridTableViewMessagesTime: TcxGridColumn
              Caption = #1042#1088#1077#1084#1103
              Options.AutoWidth = False
              Width = 65
            end
            object cxGridTableViewMessagesText: TcxGridColumn
              Caption = #1057#1086#1086#1073#1097#1077#1085#1080#1077
              Width = 543
            end
          end
          object cxGridLevelMessages: TcxGridLevel
            GridView = cxGridTableViewMessages
          end
        end
      end
    end
    inherited SplitterFilter: TPanel
      Top = 140
      ExplicitTop = 102
    end
    inherited PanelGrid: TcxGroupBox
      Top = 147
      ExplicitTop = 109
      ExplicitHeight = 247
      Height = 247
      inherited cxGrid: TcxGrid
        Height = 247
        ExplicitWidth = 1300
        ExplicitHeight = 279
      end
    end
    inherited PanelDetails: TcxGroupBox
      ExplicitTop = 393
      inherited cxGridDetails: TcxGrid
        ExplicitWidth = 1300
      end
    end
    inherited SplitterDetails: TcxSplitter
      ExplicitTop = 361
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
