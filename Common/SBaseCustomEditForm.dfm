inherited SBaseCustomEditFrm: TSBaseCustomEditFrm
  Left = 411
  Top = 54
  BorderIcons = [biSystemMenu, biMinimize]
  ClientHeight = 284
  ClientWidth = 520
  ExplicitWidth = 536
  ExplicitHeight = 322
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
    Height = 284
    Width = 520
    object PanelForm: TGroupPanel
      Left = 0
      Top = 0
      Width = 520
      Height = 284
      Style = lgsFormInner
      TabOrder = 0
      object PanelButtons: TGroupPanel
        Left = 6
        Top = 259
        Width = 508
        Style = lgsCustomBottom
        TabOrder = 1
        object btnOk: TcxButton
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 75
          Height = 23
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 5
          Margins.Bottom = 0
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
          Margins.Bottom = 0
          Align = alLeft
          Cancel = True
          LookAndFeel.Kind = lfOffice11
          ModalResult = 2
          TabOrder = 1
          OnClick = btnCancelClick
        end
        object btnApply: TcxButton
          AlignWithMargins = True
          Left = 160
          Top = 0
          Width = 75
          Height = 23
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 5
          Margins.Bottom = 0
          Align = alLeft
          LookAndFeel.Kind = lfOffice11
          TabOrder = 2
          Visible = False
          OnClick = btnApplyClick
        end
        object btnActions: TcxButton
          Tag = -1
          AlignWithMargins = True
          Left = 413
          Top = 0
          Width = 95
          Height = 23
          Margins.Left = 5
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alRight
          Enabled = False
          LookAndFeel.Kind = lfStandard
          LookAndFeel.NativeStyle = True
          TabOrder = 3
          Visible = False
        end
        object ViewID: TcxTextEdit
          AlignWithMargins = True
          Left = 338
          Top = 1
          Margins.Left = 0
          Margins.Top = 1
          Margins.Right = 0
          Margins.Bottom = 1
          Align = alRight
          ParentFont = False
          Properties.ReadOnly = True
          Style.Color = clBtnFace
          TabOrder = 4
          Width = 70
        end
      end
      object pcMain: TcxPageControl
        Left = 6
        Top = 6
        Width = 508
        Height = 253
        Align = alClient
        TabOrder = 0
        TabStop = False
        Properties.ActivePage = tsMain
        Properties.CustomButtons.Buttons = <>
        ClientRectBottom = 253
        ClientRectRight = 508
        ClientRectTop = 24
        object tsMain: TcxTabSheet
          Padding.Top = 3
          Padding.Bottom = 3
          ImageIndex = 0
          object pTsMain: TGroupPanel
            Left = 0
            Top = 3
            Width = 508
            Height = 223
            Style = lgsTabSheetInner
            TabOrder = 0
          end
        end
      end
    end
  end
  inherited BarManager: TdxMDBarManager
    DockControlHeights = (
      0
      0
      0
      0)
  end
  inherited ActionList: TActionList
    Left = 462
    Top = 4
    object ActionPgUp: TAction
      ShortCut = 16417
      OnExecute = ActionPageChangeExecute
    end
    object ActionPgDown: TAction
      ShortCut = 16418
      OnExecute = ActionPageChangeExecute
    end
    object ActionApply: TAction
      Enabled = False
      ShortCut = 16467
      OnExecute = ActionApplyExecute
    end
  end
end
