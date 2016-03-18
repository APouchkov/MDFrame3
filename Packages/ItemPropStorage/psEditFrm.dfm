object psEditForm: TpsEditForm
  Left = 101
  Top = 80
  BorderIcons = [biSystemMenu]
  BorderWidth = 5
  ClientHeight = 252
  ClientWidth = 374
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000010000000000000000402
    840004FEFC0084020400FC02FC00200011000B0001000E130000000000008206
    01000000000000000000FFFFFF000000C20000000B000000370000000000FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF0F00F0FFFFFFF
    FF00000000FFFFFFFFF001100FFFFFFF20001111000FFF2F20001111000FF222
    222001100FFFFF221100000000FF22211110200F0FFF22211112200FFFFFFF22
    1122FFFFFFFFF22222222FFFFFFFFF2F22F2FFFFFFFFFFFF22FFFFFFFFFFFFFF
    0000FFFF0000FF9F0000FE970000FC030000FE070000F0010000D00100008007
    0000C003000000170000001F0000C0FF0000807F0000D2FF0000F3FF0000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnlCat: TPanel
    Left = 0
    Top = 0
    Width = 146
    Height = 252
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 263
    object lbCat: TListBox
      Left = 1
      Top = 21
      Width = 144
      Height = 230
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
      OnClick = lbCatClick
      ExplicitHeight = 241
    end
    object TPanel
      Left = 1
      Top = 1
      Width = 144
      Height = 20
      Align = alTop
      BevelOuter = bvLowered
      Caption = #1050#1072#1090#1077#1075#1086#1088#1080#1080
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
  end
  object TPanel
    Left = 146
    Top = 0
    Width = 228
    Height = 252
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 236
    ExplicitHeight = 263
    object lbItems: TCheckListBox
      Left = 1
      Top = 21
      Width = 226
      Height = 230
      OnClickCheck = lbItemsClickCheck
      Align = alClient
      DragMode = dmAutomatic
      Style = lbOwnerDrawVariable
      TabOrder = 1
      OnClick = lbItemsClick
      OnDragDrop = lbItemsDragDrop
      OnDragOver = lbItemsDragOver
      OnDrawItem = lbItemsDrawItem
      OnStartDrag = lbItemsStartDrag
      ExplicitWidth = 234
      ExplicitHeight = 241
    end
    object TPanel
      Left = 1
      Top = 1
      Width = 226
      Height = 20
      Align = alTop
      BevelOuter = bvLowered
      Caption = #1069#1083#1077#1084#1077#1085#1090#1099
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      ExplicitWidth = 234
    end
  end
end
