inherited SearchBar: TSBaseSearchBar
    Align = alBottom
    object EditBox: TcxTextEdit [1]
      AlignWithMargins = True
      Left = 5
      Top = 1
      Margins.Left = 5
      Margins.Top = 1
      Margins.Bottom = 1
      Align = alLeft
      TabOrder = 0
      ExplicitLeft = -33
      Width = 160
      OnKeyDown = EditBoxKeyDown
    end
    object BackButton: TcxButton [2]    
      AlignWithMargins = True
      Width = 28
      Height = 19
      Margins.Left = 5
      Margins.Top = 1
      Margins.Bottom = 1
      Align = alLeft
      Caption = '<<'
      TabOrder = 1
      Left = 166
      OnClick = BackButtonClick
    end
    object ForwardButton: TcxButton [3]
      AlignWithMargins = True
      Width = 28
      Height = 19
      Margins.Top = 1
      Margins.Bottom = 1
      Align = alLeft
      Caption = '>>'
      TabOrder = 2
      Left = 195
      OnClick = ForwardButtonClick
    end
    object MatchCaseCheckBox: TcxCheckBox [4]
      AlignWithMargins = True
      Margins.Left = 10
      Margins.Top = 1
      Margins.Bottom = 1
      Align = alLeft
      Caption = #1059#1095#1080#1090#1099#1074#1072#1090#1100' '#1088#1077#1075#1080#1089#1090#1088
      TabOrder = 3
      Width = 149
      Left = 224
    end
    object WholeWordCheckBox: TcxCheckBox [5]
      AlignWithMargins = True
      Left = 248
      Top = 1
      Margins.Left = 10
      Margins.Top = 1
      Margins.Bottom = 1
      Align = alLeft
      Caption = #1057#1083#1086#1074#1086' '#1094#1077#1083#1080#1082#1086#1084
      TabOrder = 4
      Width = 125
      Left = 374
    end
    object CloseButton: TcxButton [6]
      AlignWithMargins = True
      Width = 26
      Margins.Top = 1
      Margins.Right = 10
      Margins.Bottom = 1
      Spacing = 0
      Align = alRight
      OptionsImage.Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000001000000010000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD3D3D3565656F4F4F4FF
        FFFFFFFFFFF4F4F4565656D3D3D3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFCACACA0B0B0B0000003F3F3FF4F4F4F4F4F43F3F3F0000000B0B0BCACA
        CAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1A1A1A0000000000000000003A
        3A3A3A3A3A0000000000000000001A1A1AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFD1D1D10C0C0C0000000000000000000000000000000000000C0C0CD1D1
        D1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD1D1D123232300000000
        0000000000000000232323D3D3D3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFF0F0F0303030000000000000000000000000303030F0F0F0FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEEEEEE2D2D2D00000000000000
        00000000000000000000002D2D2DEEEEEEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFF2727270000000000000000000808080808080000000000000000002727
        27FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF707070010101000000070707C8
        C8C8C8C8C8070707000000010101707070FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF6D6D6D0B0B0BC8C8C8FFFFFFFFFFFFC8C8C80B0B0B6D6D6DFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      }
      OptionsImage.Spacing = 0
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 5
      OnClick = CloseButtonClick
      Left = 500
    end
end