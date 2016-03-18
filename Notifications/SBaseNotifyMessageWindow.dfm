object SBaseNotifyMessageWindow: TSBaseNotifyMessageWindow
  Height            = 120
  Width             = 280

  object FCaptionLabel: TLabel
    Top             = 17
    Left            = 50
    Font.Size       = 10
    Font.Name       = 'Arial'
    Font.Style      = [fsBold]
  end

  object FTextLabel: TLabel
    Top             = 50
    Left            = 50
    Font.Size       = 8
    Font.Name       = 'Arial'
  end

  object FNumeratorLabel: TLabel
    Alignment       = taCenter
    Font.Size       = 8
    Font.Style      = []
    Font.Name       = 'Arial'
    Transparent     = True
    AutoSize        = False
  end

  object FCloseButton: TBaseDrawButton
    OnClick             = DoCloseClick
    Visible             = True
    DrawType            = dtClose
    Position.AlignType  = atTopRight
    Position.Width      = 12
    Position.Height     = 13
    Position.DistanceX  = 5
    Position.DistanceY  = 13
  end

  object FNextButton: TBaseDrawButton
    OnClick             = DoNextClick
    DrawType            = dtRight
    Position.AlignType  = atBottomRight
    Position.Width      = 18
    Position.Height     = 18
    Position.DistanceX  = 105
    Position.DistanceY  = 10
  end

  object FPrevButton: TBaseDrawButton
    OnClick             = DoPrevClick
    DrawType            = dtLeft
    Position.AlignType  = atBottomLeft
    Position.Width      = 18
    Position.Height     = 18
    Position.DistanceX  = 105
    Position.DistanceY  = 10
  end
end
