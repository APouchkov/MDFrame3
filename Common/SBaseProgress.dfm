object SBaseProgressFrm: TSBaseProgressFrm
  Left = 462
  Top = 89
  ActiveControl = PanelMain
  AutoSize = True
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 82
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 304
    Height = 52
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 0
    ExplicitWidth = 298
    ExplicitHeight = 90
    object LabelTitle: TLabel
      Left = 5
      Top = 5
      Width = 294
      Height = 13
      Align = alTop
      ExplicitLeft = 6
      ExplicitTop = 6
      ExplicitWidth = 3
    end
  end
  object PanelCancel: TPanel
    Left = 0
    Top = 52
    Width = 304
    Height = 30
    Align = alTop
    BevelEdges = [beLeft, beRight, beBottom]
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 90
    ExplicitWidth = 298
    object Cancel: TButton
      Left = 108
      Top = 3
      Width = 75
      Height = 25
      Cursor = crArrow
      Default = True
      TabOrder = 0
      OnClick = CancelClick
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 8
    Top = 8
  end
end
