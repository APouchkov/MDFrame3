inherited MainForm: TMainForm
  Left = 165
  Top = 73
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  ClientHeight = 694
  ClientWidth = 1015
  Color = clAppWorkSpace
  Font.Color = clBlack
  Font.Name = 'Default'
  FormStyle = fsMDIForm
  Position = poDefault
  ExplicitWidth = 1031
  ExplicitHeight = 732
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TdxMDStatusBar [0]
    Left = 0
    Top = 674
    Width = 1015
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
      end>
    DefaultPaintStyle = stpsOffice11
  end
  inherited BarManager: TdxMDBarManager
    DockControlHeights = (
      0
      0
      0
      0)
  end
  object Timer: TTimer
    Enabled = False
    Interval = 120000
    OnTimer = TimerTimer
    Left = 144
    Top = 168
  end
end
