inherited SBaseDialogFrm: TSBaseDialogFrm
  Left = 396
  Top = 85
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 173
  ClientWidth = 376
  ExplicitWidth = 382
  ExplicitHeight = 201
  PixelsPerInch = 96
  TextHeight = 13
  object PanelMain: TcxGroupBox [0]
    Left = 0
    Top = 0
    Align = alClient
    PanelStyle.Active = True
    ParentFont = False
    Style.BorderStyle = ebsNone
    TabOrder = 0
    Height = 173
    Width = 376
    object PanelForm: TGroupPanel
      Left = 0
      Top = 0
      Width = 376
      Height = 173
      Style = lgsDialogFormInner
      TabOrder = 0
      object PanelButtons: TGroupPanel
        Left = 6
        Top = 147
        Width = 364
        Height = 25
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Style = lgsCustomBottom
        TabOrder = 0
        object btnOk: TcxButton
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 75
          Height = 23
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alLeft
          Default = True
          LookAndFeel.Kind = lfOffice11
          TabOrder = 0
          OnClick = btnOkClick
        end
        object btnCancel: TcxButton
          AlignWithMargins = True
          Left = 80
          Top = 0
          Width = 75
          Height = 23
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alLeft
          Cancel = True
          LookAndFeel.Kind = lfOffice11
          ModalResult = 2
          TabOrder = 1
          OnClick = btnCancelClick
        end
      end
      object PanelParams: TPanel
        Left = 6
        Top = 2
        Width = 364
        Height = 145
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 7
        ExplicitTop = 16
        ExplicitWidth = 350
        ExplicitHeight = 119
        object lgbParams: TcxLayoutGroupBox
          AlignWithMargins = True
          Left = 0
          Top = 0
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Align = alClient
          Caption = ''
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -11
          Style.Font.Name = 'Tahoma'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
          TabOrder = 0
          ExplicitLeft = 6
          ExplicitTop = 2
          Height = 142
          Width = 364
        end
      end
    end
  end
  inherited BarManager: TdxMDBarManager
    Left = 308
    Top = 24
    DockControlHeights = (
      0
      0
      0
      0)
  end
  inherited ActionList: TActionList
    Left = 336
    Top = 24
  end
end
