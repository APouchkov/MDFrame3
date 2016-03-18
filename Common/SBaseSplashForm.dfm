object SBaseSplashFrm: TSBaseSplashFrm
  Left = 214
  Top = 50
  BorderStyle = bsNone
  ClientHeight = 221
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDeactivate = FormDeactivate
  DesignSize = (
    501
    221)
  PixelsPerInch = 96
  TextHeight = 13
  object Img: TImage
    Left = 0
    Top = 0
    Width = 501
    Height = 221
    Align = alClient
  end
  object Version: TLabel
    Left = 453
    Top = 198
    Width = 3
    Height = 14
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = 12376051
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
end
