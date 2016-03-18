inherited SBaseReportFrm: TSBaseReportFrm
  Left = 111
  Top = 42
  ClientHeight = 344
  ClientWidth = 516
  ExplicitLeft = 111
  ExplicitTop = 42
  ExplicitWidth = 532
  ExplicitHeight = 382
  PixelsPerInch = 96
  TextHeight = 13
  object OnPrepareParams: TButton [0]
    Left = 448
    Top = 80
    Width = 0
    Height = 0
    TabOrder = 1
    Visible = False
  end
  object PanelMain: TcxGroupBox [1]
    Left = 0
    Top = 0
    Align = alClient
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    TabOrder = 0
    Height = 344
    Width = 516
    object PanelForm: TGroupPanel
      Left = 0
      Top = 0
      Width = 516
      Height = 344
      Style = lgsFormInner
      TabOrder = 0
      object pTsMain: TGroupPanel
        Left = 6
        Top = 6
        Width = 504
        Height = 313
        Style = lgsTabSheetInner
        TabOrder = 0
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 516
        object SplitterLog: TSplitter
          Left = 6
          Top = 248
          Width = 492
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          Visible = False
          OnCanResize = SplitterLogCanResize
          ExplicitLeft = 0
          ExplicitTop = 202
        end
        object PanelLog: TPanel
          Left = 6
          Top = 251
          Width = 492
          Height = 56
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          OnResize = PanelLogResize
          ExplicitLeft = 0
          ExplicitTop = 257
          ExplicitWidth = 516
          object cxGridLog: TcxGrid
            Left = 0
            Top = 0
            Width = 492
            Height = 56
            Align = alClient
            PopupMenu = pmLog
            TabOrder = 0
            LookAndFeel.NativeStyle = True
            ExplicitWidth = 516
            object cxGridLogDBTableView: TcxGridDBTableView
              Navigator.Buttons.CustomButtons = <>
              DataController.DataSource = LogDataSource
              DataController.Summary.DefaultGroupSummaryItems = <>
              DataController.Summary.FooterSummaryItems = <>
              DataController.Summary.SummaryGroups = <>
              OptionsBehavior.CopyCaptionsToClipboard = False
              OptionsBehavior.DragHighlighting = False
              OptionsBehavior.DragOpening = False
              OptionsBehavior.DragScrolling = False
              OptionsBehavior.ImmediateEditor = False
              OptionsBehavior.ColumnHeaderHints = False
              OptionsBehavior.CopyPreviewToClipboard = False
              OptionsBehavior.ExpandMasterRowOnDblClick = False
              OptionsCustomize.ColumnFiltering = False
              OptionsCustomize.ColumnGrouping = False
              OptionsCustomize.ColumnHidingOnGrouping = False
              OptionsCustomize.ColumnHorzSizing = False
              OptionsCustomize.ColumnMoving = False
              OptionsCustomize.ColumnSorting = False
              OptionsData.CancelOnExit = False
              OptionsData.Deleting = False
              OptionsData.DeletingConfirmation = False
              OptionsData.Editing = False
              OptionsData.Inserting = False
              OptionsSelection.CellSelect = False
              OptionsSelection.UnselectFocusedRecordOnExit = False
              OptionsView.CellEndEllipsis = True
              OptionsView.ScrollBars = ssVertical
              OptionsView.ColumnAutoWidth = True
              OptionsView.GridLines = glNone
              OptionsView.GroupByBox = False
              OptionsView.Header = False
              object cxGridLogType: TcxGridDBColumn
                DataBinding.FieldName = 'Type'
                PropertiesClassName = 'TcxImageComboBoxProperties'
                Properties.Items = <>
                Width = 20
              end
              object cxGridLogText: TcxGridDBColumn
                DataBinding.FieldName = 'Text'
                Width = 10000
              end
            end
            object cxGridLogLevel: TcxGridLevel
              GridView = cxGridLogDBTableView
            end
          end
        end
        object gbParams: TcxLayoutGroupBox
          Left = 6
          Top = 3
          Align = alTop
          Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099
          Style.BorderStyle = ebsFlat
          Style.Edges = [bLeft, bTop, bRight, bBottom]
          Style.Font.Charset = RUSSIAN_CHARSET
          Style.Font.Color = clNavy
          Style.Font.Height = -11
          Style.Font.Name = 'MS Sans Serif'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
          TabOrder = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          Height = 46
          Width = 492
        end
      end
      object PanelButtons: TGroupPanel
        Left = 6
        Top = 319
        Width = 504
        Style = lgsCustomBottom
        TabOrder = 1
        ExplicitLeft = 0
        ExplicitTop = 313
        ExplicitWidth = 516
        DesignSize = (
          504
          23)
        object btnPrint: TcxButton
          Tag = -1
          Left = 0
          Top = 1
          Width = 95
          Height = 22
          Anchors = [akLeft, akBottom]
          Caption = #1055#1077#1095#1072#1090#1100
          Default = True
          DropDownMenu = pmPrint
          Kind = cxbkDropDownButton
          LookAndFeel.Kind = lfStandard
          LookAndFeel.NativeStyle = True
          OptionsImage.Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00808080008080800080808000808080008080800080808000808080008080
            8000808080008080800080808000FF00FF00FF00FF00FF00FF00FF00FF008080
            8000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
            C000C0C0C00080808000C0C0C00080808000FF00FF00FF00FF00808080008080
            8000808080008080800080808000808080008080800080808000808080008080
            8000808080008080800080808000C0C0C00080808000FF00FF0080808000C0C0
            C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C00000FFFF0000FFFF0000FF
            FF00C0C0C000C0C0C000808080008080800080808000FF00FF0080808000C0C0
            C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
            C000C0C0C000C0C0C00080808000C0C0C00080808000FF00FF00808080008080
            8000808080008080800080808000808080008080800080808000808080008080
            8000808080008080800080808000C0C0C000C0C0C0008080800080808000C0C0
            C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
            C000C0C0C00080808000C0C0C00080808000C0C0C00080808000FF00FF008080
            8000808080008080800080808000808080008080800080808000808080008080
            800080808000C0C0C00080808000C0C0C0008080800080808000FF00FF00FF00
            FF0080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF0080808000C0C0C00080808000C0C0C00080808000FF00FF00FF00
            FF00FF00FF0080808000FFFFFF00808080008080800080808000808080008080
            8000FFFFFF0080808000808080008080800080808000FF00FF00FF00FF00FF00
            FF00FF00FF0080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF0080808000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF0080808000FFFFFF008080800080808000808080008080
            800080808000FFFFFF0080808000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF0080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00FFFFFF0080808000FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00808080008080800080808000808080008080
            800080808000808080008080800080808000FF00FF00FF00FF00}
          OptionsImage.ImageIndex = 15
          TabOrder = 0
          OnClick = PrintPrintExecute
        end
        object btnClose: TcxButton
          Tag = -1
          Left = 101
          Top = 1
          Width = 75
          Height = 22
          Anchors = [akLeft, akBottom]
          Cancel = True
          Caption = #1047#1072#1082#1088#1099#1090#1100
          LookAndFeel.Kind = lfStandard
          LookAndFeel.NativeStyle = True
          TabOrder = 1
          OnClick = btnCloseClick
        end
        object lpFormat: TLayoutPanel
          Left = 254
          Top = 0
          Width = 250
          Align = alRight
          Caption.Text = #1054#1090#1095#1077#1090' '#1074' '#1092#1086#1088#1084#1072#1090#1077':'
          Caption.Width = 100
          Caption.Font.Charset = DEFAULT_CHARSET
          Caption.Font.Color = clWindowText
          Caption.Font.Height = -11
          Caption.Font.Name = 'Tahoma'
          Caption.Font.Style = []
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          TabOrder = 2
          Visible = False
          ExplicitLeft = 266
          object lcbFormat: TcxImageLookupComboBox
            Left = 105
            Top = 1
            Align = alClient
            Properties.ImmediateDropDownWhenActivated = True
            Properties.ListSource = FormatsDataSource
            Properties.KeyField = 'Code'
            Properties.ImageField = 'ImageIndex'
            Properties.ListField = 'Name'
            Anchors = [akRight, akBottom]
            ParentFont = False
            TabOrder = 0
            Width = 145
          end
        end
      end
    end
  end
  inherited BarManager: TdxMDBarManager
    Left = 484
    DockControlHeights = (
      0
      0
      0
      0)
    object mnPrint: TdxBarButton
      Caption = #1055#1077#1095#1072#1090#1100
      Category = 0
      Visible = ivAlways
      OnClick = PrintPrintExecute
    end
    object mnSaveAs: TdxBarButton
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Category = 0
      Visible = ivAlways
      OnClick = PrintSaveExecute
    end
    object mnLogCopy: TdxBarButton
      Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
      Category = 0
      Visible = ivAlways
      OnClick = mnLogCopyClick
    end
    object mnLogDelete: TdxBarButton
      Caption = #1059#1076#1072#1083#1080#1090#1100
      Category = 0
      Visible = ivAlways
      OnClick = mnLogDeleteClick
    end
    object mnLogSave: TdxBarButton
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1092#1072#1081#1083
      Category = 0
      Visible = ivAlways
      OnClick = mnLogSaveClick
    end
    object mnLogClear: TdxBarButton
      Caption = #1054#1095#1080#1089#1090#1080#1090#1100
      Category = 0
      Visible = ivAlways
      OnClick = mnLogClearClick
    end
  end
  inherited ActionList: TActionList
    OnExecute = ActionListExecute
    Left = 382
    Top = 171
    object PrintPrint: TAction
      Tag = 1
      Category = 'Print'
      Caption = #1055#1077#1095#1072#1090#1100
      ImageIndex = 15
      OnExecute = PrintPrintExecute
    end
    object PrintSave: TAction
      Tag = 2
      Category = 'Print'
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      ImageIndex = 8
      OnExecute = PrintSaveExecute
    end
    object PrintMail: TAction
      Tag = 3
      Category = 'Print'
      Caption = #1054#1090#1087#1088#1072#1074#1080#1090#1100
      ImageIndex = 129
      OnExecute = PrintSaveExecute
    end
  end
  object FormatsDataSource: TDataSource
    Left = 379
    Top = 4
  end
  object LogDataSource: TDataSource
    Left = 347
    Top = 4
  end
  object pmPrint: TdxBarPopupMenu
    BarManager = BarManager
    ItemLinks = <
      item
        ItemName = 'mnPrint'
      end
      item
        ItemName = 'mnSaveAs'
      end>
    UseOwnFont = False
    Left = 414
    Top = 110
  end
  object pmLog: TdxBarPopupMenu
    BarManager = BarManager
    ItemLinks = <
      item
        ItemName = 'mnLogCopy'
      end
      item
        ItemName = 'mnLogDelete'
      end
      item
        ItemName = 'mnLogSave'
      end
      item
        ItemName = 'mnLogClear'
      end>
    UseOwnFont = False
    Left = 446
    Top = 110
  end
end
