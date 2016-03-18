object SBaseGridColumnsDlg: TSBaseGridColumnsDlg
  Left = 139
  Top = 52
  BorderStyle = bsDialog
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1090#1072#1073#1083#1080#1094#1099
  ClientHeight = 311
  ClientWidth = 245
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    245
    311)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 4
    Top = 5
    Width = 237
    Height = 270
    ActivePage = TabShAccount
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    TabStop = False
    object TabShAccount: TTabSheet
      Caption = #1050#1086#1083#1086#1085#1082#1080
      ImageIndex = 3
      DesignSize = (
        229
        242)
      object Bevel: TBevel
        Left = 0
        Top = 0
        Width = 229
        Height = 242
        Align = alClient
        Shape = bsFrame
      end
      object sbUp: TSpeedButton
        Left = 193
        Top = 86
        Width = 25
        Height = 25
        BiDiMode = bdLeftToRight
        Glyph.Data = {
          DE010000424DDE01000000000000760000002800000024000000120000000100
          04000000000068010000C40E0000C40E00001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFF77777FFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFF444447FFFFFFFFFFFF77777FFFFFFF0000FFFF
          FFCCCC47FFFFFFFFFFFF77777FFFFFFF0000FFFFFFCCCC47FFFFFFFFFFFF7777
          7FFFFFFF0000FFFFFFCCCC47FFFFFFFFFFFF77777FFFFFFF0000FFFFFFCCCC47
          FFFFFFFFFFFF77777FFFFFFF0000FFFFFFCCCC4777FFFFFFFFFF77777FFFFFFF
          0000FFFFCCCCCCCCCFFFFFFFFF777777777FFFFF0000FFFFFCCCCCCCFFFFFFFF
          FFF7777777FFFFFF0000FFFFFFCCCCCFFFFFFFFFFFFF77777FFFFFFF0000FFFF
          FFFCCCFFFFFFFFFFFFFFF777FFFFFFFF0000FFFFFFFFCFFFFFFFFFFFFFFFFF7F
          FFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000}
        NumGlyphs = 2
        ParentBiDiMode = False
        OnClick = sbUpClick
      end
      object sbDown: TSpeedButton
        Left = 193
        Top = 126
        Width = 25
        Height = 25
        BiDiMode = bdLeftToRight
        Glyph.Data = {
          DE010000424DDE01000000000000760000002800000024000000120000000100
          04000000000068010000C40E0000C40E00001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFF7FFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFF477FFFFFFFFFFFFFFF7FFFFFFFFF0000FFFF
          FFFCC477FFFFFFFFFFFFF777FFFFFFFF0000FFFFFFCCCC477FFFFFFFFFFF7777
          7FFFFFFF0000FFFFFCCCCCC477FFFFFFFFF7777777FFFFFF0000FFFFCCCCCC44
          4FFFFFFFFF777777777FFFFF0000FFFFFFCCCC47FFFFFFFFFFFF77777FFFFFFF
          0000FFFFFFCCCC47FFFFFFFFFFFF77777FFFFFFF0000FFFFFFCCCC47FFFFFFFF
          FFFF77777FFFFFFF0000FFFFFFCCCC47FFFFFFFFFFFF77777FFFFFFF0000FFFF
          FFCCCC47FFFFFFFFFFFF77777FFFFFFF0000FFFFFFCCCC4FFFFFFFFFFFFF7777
          7FFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000}
        NumGlyphs = 2
        ParentBiDiMode = False
        OnClick = sbDownClick
      end
      object ListViewInfo: TListView
        Left = 8
        Top = 9
        Width = 182
        Height = 224
        Anchors = [akLeft, akTop, akRight, akBottom]
        Checkboxes = True
        Columns = <
          item
            Caption = #1053#1072#1079#1074#1072#1085#1080#1077
            Width = 150
          end>
        DragMode = dmAutomatic
        HideSelection = False
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnDragDrop = ListViewInfoDragDrop
        OnDragOver = ListViewInfoDragOver
      end
    end
  end
  object ButtonOk: TButton
    Left = 4
    Top = 283
    Width = 76
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Ok'
    Default = True
    TabOrder = 1
    OnClick = ButtonOkClick
  end
  object ButtonClose: TButton
    Left = 89
    Top = 283
    Width = 76
    Height = 22
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 2
    OnClick = ButtonCloseClick
  end
end
