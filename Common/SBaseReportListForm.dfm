inherited SBaseReportListFrm: TSBaseReportListFrm
  Left = 490
  Top = 43
  ExplicitLeft = 490
  ExplicitTop = 43
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelMain: TcxGroupBox
    inherited PanelForm: TGroupPanel
      inherited pTsMain: TGroupPanel
        inherited PanelLog: TPanel
          OnResize = nil
        end
        inherited gbParams: TcxLayoutGroupBox
          Style.IsFontAssigned = True
        end
        object gbList: TcxLayoutGroupBox
          Left = 6
          Top = 49
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 5
          Align = alClient
          Padding.Left = 5
          Padding.Right = 5
          Padding.Bottom = 5
          Caption = ' '#1060#1080#1083#1100#1090#1088#1099' '
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
          TabOrder = 2
          ExplicitLeft = 0
          ExplicitTop = 46
          ExplicitWidth = 516
          ExplicitHeight = 208
          Height = 199
          Width = 492
          object rfList: TSBaseReportFilter
            Left = 7
            Top = 16
            TabStop = False
            Align = alClient
            FilterItems = <>
            PanelStyle.Active = True
            Style.BorderStyle = ebsNone
            ParentFont = False
            ExplicitTop = 3
            ExplicitWidth = 502
            ExplicitHeight = 198
            Height = 176
            Width = 478
          end
        end
      end
    end
  end
  inherited BarManager: TdxBarManager
    DockControlHeights = (
      0
      0
      0
      0)
  end
end
