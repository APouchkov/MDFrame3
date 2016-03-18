inherited SBaseXXViewFrm: TSBaseXXViewFrm
  Left = 319
  Top = 51
  ClientHeight = 523
  ClientWidth = 1184
  Constraints.MinHeight = 200
  Constraints.MinWidth = 400
  ExplicitWidth = 1200
  ExplicitHeight = 561
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TdxMDStatusBar [0]
    Left = 0
    Top = 503
    Width = 1184
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Fixed = False
        Width = 500
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 140
      end>
    DefaultPaintStyle = stpsOffice11
  end
  object PanelMain: TcxGroupBox [1]
    Left = 0
    Top = 26
    Align = alClient
    PanelStyle.Active = True
    ParentFont = False
    Style.BorderStyle = ebsNone
    TabOrder = 5
    Height = 477
    Width = 1184
    object PanelData: TcxGroupBox
      Left = 0
      Top = 0
      Align = alClient
      PanelStyle.Active = True
      Style.BorderStyle = ebsNone
      TabOrder = 0
      Height = 477
      Width = 1184
      object PanelDetails: TcxGroupBox
        Left = 0
        Top = 397
        Align = alBottom
        PanelStyle.Active = True
        ParentColor = False
        ParentFont = False
        Style.BorderStyle = ebsNone
        TabOrder = 0
        OnResize = PanelDetailsResize
        Height = 80
        Width = 1184
      end
      object SplitterDetails: TcxSplitter
        Left = 0
        Top = 394
        Width = 1184
        Height = 3
        Cursor = crVSplit
        AlignSplitter = salBottom
        Control = PanelDetails
        Color = clBtnFace
        ParentColor = False
      end
      object PanelGrid: TcxGroupBox
        Left = 0
        Top = 0
        Align = alClient
        PanelStyle.Active = True
        ParentColor = False
        ParentFont = False
        Style.BorderStyle = ebsNone
        TabOrder = 2
        Height = 394
        Width = 1184
      end
    end
  end
  object dxBarDockControl: TdxMDBarDockControl [2]
    Left = 0
    Top = 0
    Width = 1184
    Height = 26
    Align = dalTop
    BarManager = BarManager
  end
  inherited BarManager: TdxMDBarManager
    Categories.Strings = (
      #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
      #1052#1077#1085#1102' '#1043#1083#1072#1074#1085#1086#1077)
    Categories.ItemsVisibles = (
      2
      2)
    Categories.Visibles = (
      True
      True)
    Left = 28
    Top = 105
    DockControlHeights = (
      0
      0
      0
      0)
    object ToolBar: TdxBar [0]
      Store = False
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      AllowReset = False
      Caption = 'ToolBar'
      CaptionButtons = <>
      DockControl = dxBarDockControl
      DockedDockControl = dxBarDockControl
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 424
      FloatTop = 238
      FloatClientWidth = 23
      FloatClientHeight = 22
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
      NotDocking = [dsNone, dsLeft, dsTop, dsRight, dsBottom]
      OldName = 'ToolBar'
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      UseRestSpace = True
      Visible = True
      WholeRow = False
    end
    object FilterBar: TdxBar [1]
      Store = False
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      AllowReset = False
      Caption = 'FilterBar'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 26
      DockingStyle = dsTop
      FloatLeft = 351
      FloatTop = 75
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <>
      NotDocking = [dsNone, dsLeft, dsTop, dsRight, dsBottom]
      OldName = 'FilterBar'
      OneOnRow = True
      Row = 1
      UseOwnFont = False
      UseRestSpace = True
      Visible = False
      WholeRow = False
    end
    inherited mnExcel: TdxBarButton
      OnClick = mnExcelClick
    end
    object mnPrint: TdxBarButton
      Category = 0
      Visible = ivAlways
    end
    object mnFitToWidth: TdxBarButton
      Category = 0
      Visible = ivNever
      ButtonStyle = bsChecked
    end
    object mnFitToHeight: TdxBarButton
      Category = 0
      Visible = ivNever
      ButtonStyle = bsChecked
    end
    object mnFitToScreen: TdxBarButton
      Category = 0
      Visible = ivNever
      OnClick = mnFitToScreenClick
    end
    object mnFilter: TdxBarButton
      Category = 0
      Visible = ivNever
      ButtonStyle = bsChecked
      OnClick = mnFilterClick
    end
    object mnGroupBox: TdxBarButton
      Category = 0
      Visible = ivNever
      ShortCut = 119
    end
    object mnGroupingClear: TdxBarButton
      Category = 0
      Enabled = False
      Visible = ivNever
    end
    object mnExpandAll: TdxBarButton
      Category = 0
      Visible = ivNever
    end
    object mnCollapseAll: TdxBarButton
      Category = 0
      Visible = ivNever
    end
    object mnDetails: TdxBarButton
      Category = 0
      Visible = ivNever
      ButtonStyle = bsChecked
      DropDownMenu = PopupMenuDetailsPosition
      OnClick = mnDetailsClick
    end
    object mnDetailsBottom: TdxBarButton
      Category = 0
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      OnClick = mnDetailsBottomClick
    end
    object mnDetailsRight: TdxBarButton
      Category = 0
      Visible = ivAlways
      ButtonStyle = bsChecked
      GroupIndex = 1
      OnClick = mnDetailsRightClick
    end
    object mnProperties: TdxBarButton
      Category = 0
      Visible = ivNever
    end
    object mnSubmit: TdxMDBarButton
      Align = iaRight
      Category = 0
      Enabled = False
      Visible = ivNever
      PaintStyle = psCaptionGlyph
      OnClick = mnSubmitClick
    end
    object mnFormClose: TdxBarButton
      Category = 0
      Enabled = False
      Visible = ivNever
      ShortCut = 27
      OnClick = mnFormCloseClick
    end
    object mnSelect: TdxBarButton
      Category = 1
      Enabled = False
      Visible = ivNever
      PaintStyle = psCaptionGlyph
      ShortCut = 13
    end
    object mnTake: TdxBarButton
      Category = 1
      Enabled = False
      Visible = ivAlways
      ShortCut = 16397
    end
    object mnNew: TdxMDBarButton
      Category = 1
      Enabled = False
      Visible = ivAlways
      ShortCut = 45
    end
    object mnView: TdxMDBarButton
      Category = 1
      Enabled = False
      Visible = ivAlways
      ShortCut = 114
    end
    object mnEdit: TdxMDBarButton
      Category = 1
      Enabled = False
      Visible = ivAlways
      ShortCut = 115
    end
    object mnCopy: TdxMDBarButton
      Category = 1
      Enabled = False
      Visible = ivNever
    end
    object mnRefresh: TdxBarButton
      Category = 1
      Visible = ivAlways
      ShortCut = 116
    end
    object mnRefreshRecord: TdxBarButton
      Category = 1
      Enabled = False
      Visible = ivAlways
    end
    object mnMarkAsDeleted: TdxBarButton
      Category = 1
      Enabled = False
      Visible = ivNever
    end
    object mnDelete: TdxBarButton
      Category = 1
      Enabled = False
      Visible = ivAlways
      ShortCut = 16430
    end
  end
  inherited ActionList: TActionList
    Left = 56
    Top = 24
  end
  object PopupMenu: TdxBarPopupMenu
    BarManager = BarManager
    ItemLinks = <
      item
        Position = ipBeginsNewRow
        ItemName = 'mnSelect'
      end
      item
        ItemName = 'mnTake'
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
    UseOwnFont = False
    Left = 473
    Top = 112
  end
  object DataSource: TDataSource
    DataSet = DataSetProvider
    Left = 403
    Top = 112
  end
  object DetailSource: TDataSource
    DataSet = DetailSetProvider
    Left = 344
    Top = 351
  end
  object RefreshTimer: TTimer
    Enabled = False
    Interval = 60000
    OnTimer = RefreshTimerTimer
    Left = 370
    Top = 112
  end
  object TimerDetails: TTimer
    Enabled = False
    Interval = 300
    OnTimer = TimerTimerDetails
    Left = 315
    Top = 351
  end
  object DataSetProvider: TSQLDataSetProvider
    Level = 2
    AutoCalcFields = False
    Params = <>
    FilterOptions = [foCaseInsensitive]
    CachedUpdates = False
    Thread.OnError = DataSetProviderThreadOnError
    Thread.OnPrint = DataSetProviderThreadOnPrint
    Thread.OnValidate = DataSetProviderThreadOnValidate
    Thread.AfterComplete = DataSetProviderThreadAfterComplete
    BeforeOpen = DataSetBeforeOpen
    AfterOpen = DataSetAfterOpen
    AfterClose = DataSetAfterClose
    AfterRefresh = DataSetAfterRefresh
    OnBeginUpdate = DataSetProviderOnBeginUpdate
    OnEndUpdate = DataSetProviderOnEndUpdate
    Left = 438
    Top = 112
  end
  object DetailSetProvider: TSQLDataSetProvider
    Level = 3
    AutoCalcFields = False
    Params = <>
    FilterOptions = [foCaseInsensitive]
    CachedUpdates = False
    Left = 374
    Top = 352
  end
  object PopupMenuDetailsPosition: TdxBarPopupMenu
    BarManager = BarManager
    ItemLinks = <
      item
        ItemName = 'mnDetailsRight'
      end
      item
        ItemName = 'mnDetailsBottom'
      end>
    UseOwnFont = False
    Left = 408
    Top = 353
  end
end
