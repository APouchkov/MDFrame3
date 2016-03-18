inherited SBaseDataSetInspectorDlg: TSBaseDataSetInspectorDlg
  Left = 221
  Top = 73
  ClientHeight = 428
  ClientWidth = 984
  ExplicitLeft = 221
  ExplicitTop = 73
  ExplicitWidth = 1000
  ExplicitHeight = 466
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TdxMDStatusBar
    Top = 408
    Width = 984
    ExplicitTop = 408
    ExplicitWidth = 984
  end
  inherited PanelMain: TcxGroupBox
    Top = 3
    ExplicitTop = 3
    ExplicitWidth = 984
    ExplicitHeight = 405
    Height = 405
    Width = 984
    inherited PanelData: TcxGroupBox
      ExplicitWidth = 984
      ExplicitHeight = 405
      Height = 405
      Width = 984
      inherited PanelDetails: TcxGroupBox
        Top = 402
        ExplicitTop = 402
        ExplicitWidth = 984
        ExplicitHeight = 0
        Height = 0
        Width = 984
      end
      inherited SplitterDetails: TcxSplitter
        Top = 402
        Width = 984
        ExplicitTop = 402
        ExplicitWidth = 984
      end
      inherited PanelGrid: TcxGroupBox
        ExplicitWidth = 984
        ExplicitHeight = 402
        Height = 402
        Width = 984
        inherited cxGrid: TcxMDGrid
          AlignWithMargins = True
          Left = 5
          Top = 5
          Width = 974
          Height = 392
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          ExplicitLeft = 7
          ExplicitTop = 7
          ExplicitWidth = 970
          ExplicitHeight = 493
        end
      end
    end
  end
  inherited dxBarDockControl: TdxMDBarDockControl
    Width = 984
    ExplicitWidth = 984
    ExplicitHeight = 3
  end
  inherited BarManager: TdxMDBarManager
    DockControlHeights = (
      0
      0
      0
      0)
    inherited ToolBar: TdxBar
      Visible = False
    end
  end
  inherited DataSetProvider: TSQLDataSetProvider
    AutoCalcFields = True
    FilterOptions = []
    DataSetObject = bdsoMemory
  end
end
