inherited frmPrepareToExecuteDialog: TfrmPrepareToExecuteDialog
  Left = 241
  Top = 61
  ClientHeight = 116
  ClientWidth = 415
  ExplicitWidth = 431
  ExplicitHeight = 154
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelMain: TcxGroupBox
    ExplicitWidth = 415
    ExplicitHeight = 116
    Height = 116
    Width = 415
    inherited PanelForm: TGroupPanel
      Width = 415
      Height = 116
      ExplicitWidth = 415
      ExplicitHeight = 116
      inherited PanelButtons: TGroupPanel
        Top = 91
        Width = 403
        ExplicitTop = 91
        ExplicitWidth = 403
        inherited btnPrint: TcxButton
          Left = 258
          ExplicitLeft = 258
        end
        inherited ViewID: TcxTextEdit
          Left = 333
          ExplicitLeft = 333
        end
      end
      inherited pcMain: TcxPageControl
        Width = 403
        Height = 85
        Properties.HideTabs = True
        ExplicitWidth = 403
        ExplicitHeight = 85
        ClientRectBottom = 85
        ClientRectRight = 403
        ClientRectTop = 0
        inherited tsMain: TcxTabSheet
          ExplicitTop = 0
          ExplicitWidth = 403
          ExplicitHeight = 85
          inherited pTsMain: TGroupPanel
            Width = 403
            Height = 79
            ExplicitWidth = 403
            ExplicitHeight = 79
            object cbShow: TcxUserCheckBox
              Left = 6
              Top = 3
              Align = alTop
              Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1086#1096#1080#1073#1082#1080' '#1074' '#1086#1073#1097#1077#1084' '#1089#1087#1080#1089#1082#1077' '#1089#1086#1086#1073#1097#1077#1085#1080#1081' '#1087#1088#1086#1075#1088#1072#1084#1084#1099
              Checked = False
              TabOrder = 0
              ExplicitWidth = 329
            end
            object cbSave: TcxUserCheckBox
              Left = 6
              Top = 24
              Align = alTop
              Caption = #1057#1086#1093#1088#1072#1085#1103#1090#1100' '#1086#1096#1080#1073#1082#1080' '#1087#1086' '#1091#1082#1072#1079#1072#1085#1085#1086#1084#1091' '#1087#1091#1090#1080':'
              Checked = False
              TabOrder = 1
              ExplicitWidth = 229
            end
            object beFilePath: TcxButtonEdit
              AlignWithMargins = True
              Left = 10
              Top = 48
              Margins.Left = 4
              Align = alTop
              Properties.AutoSelect = False
              Properties.Buttons = <
                item
                  Default = True
                  Kind = bkEllipsis
                end>
              Properties.ReadOnly = False
              TabOrder = 2
              Width = 384
            end
          end
        end
      end
    end
  end
  inherited BarManager: TdxMDBarManager
    Left = 356
    DockControlHeights = (
      0
      0
      0
      0)
  end
  inherited ActionList: TActionList
    Left = 366
    Top = 65524
  end
end
