object MainDataModule: TMainDataModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 308
  Width = 373
  object FileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 24
    Top = 8
  end
  object FileSaveDialog: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 64
    Top = 8
  end
  object SynDfmSyn: TSynDfmSyn
    CommentAttri.Foreground = clTeal
    CommentAttri.Style = []
    NumberAttri.Style = [fsBold]
    StringAttri.Foreground = clBlue
    Left = 144
    Top = 8
  end
  object SynPasSyn: TSynPasSyn
    CommentAttri.Foreground = clGreen
    DirectiveAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    FloatAttri.Foreground = clBlue
    HexAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    CharAttri.Foreground = clBlue
    Left = 184
    Top = 8
  end
  object SynXMLSyn: TSynXMLSyn
    WantBracesParsed = False
    Left = 224
    Top = 8
  end
  object SynEditSearch: TSynEditSearch
    Left = 144
    Top = 64
  end
end
