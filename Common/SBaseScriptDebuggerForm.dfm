inherited SBaseScriptDebuggerFrm: TSBaseScriptDebuggerFrm
  Left = 535
  Top = 70
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Script Debugger'
  ClientHeight = 800
  ClientWidth = 899
  FormStyle = fsStayOnTop
  OnDestroy = FormDestroy
  ExplicitWidth = 915
  ExplicitHeight = 834
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit: TSynEdit [0]
    Left = 0
    Top = 26
    Width = 899
    Height = 571
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 5
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Highlighter = MainDataModule.SynPasSyn
    Lines.Strings = (
      'SynEdit')
    ReadOnly = True
    SearchEngine = MainDataModule.SynEditSearch
    OnGutterClick = SynEditGutterClick
    OnGutterPaint = SynEditGutterPaint
    OnSpecialLineColors = SynEditSpecialLineColors
    OnStatusChange = SynEditStatusChange
    FontSmoothing = fsmNone
  end
  object dxStatusBar: TdxMDStatusBar [1]
    Left = 0
    Top = 780
    Width = 899
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Fixed = False
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end
      item
        PanelStyleClassName = 'TdxStatusBarKeyboardStatePanelStyle'
        PanelStyle.CapsLockKeyAppearance.ActiveCaption = 'CAPS'
        PanelStyle.CapsLockKeyAppearance.InactiveCaption = 'CAPS'
        PanelStyle.NumLockKeyAppearance.ActiveCaption = 'NUM'
        PanelStyle.NumLockKeyAppearance.InactiveCaption = 'NUM'
        PanelStyle.ScrollLockKeyAppearance.ActiveCaption = 'SCRL'
        PanelStyle.ScrollLockKeyAppearance.InactiveCaption = 'SCRL'
        PanelStyle.InsertKeyAppearance.ActiveCaption = 'OVR'
        PanelStyle.InsertKeyAppearance.InactiveCaption = 'INS'
      end>
    DefaultPaintStyle = stpsOffice11
  end
  object pcDebugger: TcxPageControl [2]
    Left = 0
    Top = 600
    Width = 899
    Height = 180
    Align = alBottom
    TabOrder = 7
    Visible = False
    Properties.ActivePage = tsVariables
    Properties.CustomButtons.Buttons = <>
    OnChange = pcDebuggerChange
    ClientRectBottom = 180
    ClientRectRight = 899
    ClientRectTop = 24
    object tsVariables: TcxTabSheet
      Padding.Top = 3
      Caption = #1055#1077#1088#1077#1084#1077#1085#1085#1099#1077
      ImageIndex = 0
      object GridVairables: TcxMDGrid
        Left = 0
        Top = 3
        Width = 899
        Height = 153
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        TabOrder = 0
        object GridVairablesDBTableView: TcxMDGridDBTableView
          Navigator.Buttons.CustomButtons = <>
          DataController.DataSource = DataSourceVariables
          DataController.Filter.Options = [fcoCaseInsensitive]
          DataController.KeyFieldNames = 'VarName'
          DataController.Options = [dcoAnsiSort, dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding]
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          object ColumnVariableName: TcxGridDBColumn
            Caption = #1055#1077#1088#1077#1084#1077#1085#1085#1072#1103
            DataBinding.FieldName = 'VarName'
            SortIndex = 0
            SortOrder = soAscending
            Width = 160
          end
          object ColumnVariableType: TcxGridDBColumn
            Caption = #1058#1080#1087
            DataBinding.FieldName = 'VarType'
            Width = 140
          end
          object ColumnVariableClass: TcxGridDBColumn
            Caption = #1050#1083#1072#1089#1089
            DataBinding.FieldName = 'VarClassType'
            Width = 140
          end
          object ColumnVariableValue: TcxGridDBColumn
            Caption = #1047#1085#1072#1095#1077#1085#1080#1077
            DataBinding.FieldName = 'VarValueStr'
            Width = 320
          end
        end
        object GridVairablesLevel: TcxMDGridLevel
          GridView = GridVairablesDBTableView
          Options.DetailFrameWidth = 0
        end
      end
    end
    object tsWatch: TcxTabSheet
      Padding.Top = 3
      Caption = #1055#1088#1086#1089#1084#1086#1090#1088
      ImageIndex = 1
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GridWatch: TcxMDGrid
        Left = 0
        Top = 3
        Width = 899
        Height = 153
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        TabOrder = 0
        object GridWatchDBTableView: TcxMDGridDBTableView
          PopupMenu = pmWatches
          OnDragDrop = GridWatchDBTableViewDragDrop
          OnDragOver = GridWatchDBTableViewDragOver
          Navigator.Buttons.CustomButtons = <>
          DataController.DataSource = DataSourceWatch
          DataController.Filter.Options = [fcoCaseInsensitive]
          DataController.KeyFieldNames = 'VarName'
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          object ColumnWatchName: TcxGridDBColumn
            Caption = #1055#1077#1088#1077#1084#1077#1085#1085#1072#1103
            DataBinding.FieldName = 'VarName'
            Width = 160
          end
          object ColumnWatchType: TcxGridDBColumn
            Caption = #1058#1080#1087
            DataBinding.FieldName = 'VarType'
            Options.Editing = False
            Width = 140
          end
          object ColumnWatchClass: TcxGridDBColumn
            Caption = #1050#1083#1072#1089#1089
            DataBinding.FieldName = 'VarClassType'
            Options.Editing = False
            Width = 140
          end
          object ColumnWatchValue: TcxGridDBColumn
            Caption = #1047#1085#1072#1095#1077#1085#1080#1077
            DataBinding.FieldName = 'VarValueStr'
            Options.Editing = False
            Width = 320
          end
        end
        object GridWatchLevel: TcxMDGridLevel
          GridView = GridWatchDBTableView
          Options.DetailFrameWidth = 0
        end
      end
    end
  end
  object splDebugger: TcxSplitter [3]
    Left = 0
    Top = 597
    Width = 899
    Height = 3
    AlignSplitter = salBottom
    Visible = False
  end
  inherited BarManager: TdxMDBarManager
    Categories.Strings = (
      #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
      'Watch')
    Categories.ItemsVisibles = (
      3
      3)
    Categories.Visibles = (
      True
      True)
    Left = 84
    Top = 128
    DockControlHeights = (
      0
      0
      26
      0)
    object ToolBar: TdxBar [0]
      Store = False
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      AllowReset = False
      Caption = 'ToolBar'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 424
      FloatTop = 238
      FloatClientWidth = 23
      FloatClientHeight = 22
      ItemLinks = <
        item
          ItemName = 'mnRun'
        end
        item
          ItemName = 'mnPause'
        end
        item
          ItemName = 'mnStep'
        end
        item
          BeginGroup = True
          ItemName = 'mnSetBreakpoint'
        end
        item
          BeginGroup = True
          ItemName = 'mnWatch'
        end>
      MultiLine = True
      NotDocking = [dsNone, dsLeft, dsTop, dsRight, dsBottom]
      OldName = 'ToolBar'
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      UseRestSpace = True
      Visible = True
      WholeRow = False
    end
    object mnRun: TdxBarButton
      Caption = 'Run'
      Category = 0
      Visible = ivAlways
      PaintStyle = psCaptionGlyph
      ShortCut = 120
      OnClick = mnRunClick
    end
    object dxBarSubItem: TdxBarSubItem
      Caption = 'New SubItem'
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
    end
    object mnPause: TdxBarButton
      Caption = 'Pause'
      Category = 0
      Visible = ivAlways
      PaintStyle = psCaptionGlyph
      OnClick = mnPauseClick
    end
    object mnStep: TdxBarButton
      Caption = 'Step'
      Category = 0
      Visible = ivAlways
      PaintStyle = psCaptionGlyph
      ShortCut = 119
      OnClick = mnStepClick
    end
    object dxBar: TdxBarStatic
      Category = 0
      Visible = ivAlways
    end
    object mnSetBreakpoint: TdxBarButton
      Caption = 'BreakPoint'
      Category = 0
      Visible = ivAlways
      ShortCut = 116
      OnClick = mnSetBreakpointClick
    end
    object mnResetBreakpoint: TdxBarButton
      Caption = '-BreakPoint'
      Category = 0
      Visible = ivAlways
      ImageIndex = 9
    end
    object mnWatch: TdxBarButton
      Caption = 'Watch'
      Category = 0
      Visible = ivAlways
      ButtonStyle = bsChecked
      PaintStyle = psCaptionGlyph
      ShortCut = 49239
      OnClick = mnWatchClick
    end
    object mnWatchAdd: TdxBarButton
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      Category = 1
      Visible = ivAlways
      ShortCut = 45
    end
    object mnWatchDelete: TdxBarButton
      Caption = #1059#1076#1072#1083#1080#1090#1100
      Category = 1
      Visible = ivAlways
      ShortCut = 46
    end
    object mnWatchEdit: TdxBarButton
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100
      Category = 1
      Visible = ivAlways
      ShortCut = 13
    end
  end
  inherited ActionList: TActionList
    Left = 86
    Top = 63
    object ActionFind: TAction
      ShortCut = 16454
    end
    object ActionFindNext: TAction
      ShortCut = 114
    end
  end
  object VTableVariables: TVirtualTable
    Options = [voPersistentData, voStored, voSkipUnSupportedFieldTypes]
    FieldDefs = <
      item
        Name = 'VarName'
        DataType = ftString
        Size = 256
      end
      item
        Name = 'VarType'
        DataType = ftString
        Size = 256
      end
      item
        Name = 'VarClassType'
        DataType = ftString
        Size = 256
      end
      item
        Name = 'VarValueStr'
        DataType = ftString
        Size = 2000
      end>
    Left = 88
    Top = 224
    Data = {
      0300040007005661724E616D6501000001000000000700566172547970650100
      0001000000000C00566172436C6173735479706501000001000000000B005661
      7256616C75655374720100D00700000000000000000000}
  end
  object DataSourceVariables: TDataSource
    DataSet = VTableVariables
    Left = 160
    Top = 248
  end
  object VTableWatch: TVirtualTable
    Options = [voPersistentData, voStored, voSkipUnSupportedFieldTypes]
    BeforePost = VTableWatchBeforePost
    FieldDefs = <
      item
        Name = 'VarName'
        DataType = ftString
        Size = 256
      end
      item
        Name = 'VarType'
        DataType = ftString
        Size = 256
      end
      item
        Name = 'VarClassType'
        DataType = ftString
        Size = 256
      end
      item
        Name = 'VarValueStr'
        DataType = ftString
        Size = 2000
      end>
    Left = 88
    Top = 344
    Data = {
      0300040007005661724E616D6501000001000000000700566172547970650100
      0001000000000C00566172436C6173735479706501000001000000000B005661
      7256616C75655374720100D00700000000000000000000}
  end
  object DataSourceWatch: TDataSource
    DataSet = VTableWatch
    Left = 152
    Top = 368
  end
  object pmWatches: TdxBarPopupMenu
    BarManager = BarManager
    ItemLinks = <
      item
        ItemName = 'mnWatchAdd'
      end
      item
        ItemName = 'mnWatchEdit'
      end
      item
        ItemName = 'mnWatchDelete'
      end>
    UseOwnFont = False
    Left = 80
    Top = 488
  end
end
