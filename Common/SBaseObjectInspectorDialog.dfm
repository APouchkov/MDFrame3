object SBaseObjectInspectorDlg: TSBaseObjectInspectorDlg
  Left = 270
  Top = 40
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Property Inspector'
  ClientHeight = 646
  ClientWidth = 304
  Color = clBtnFace
  Constraints.MinWidth = 295
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 12
  object gbComponents: TcxGroupBox
    Left = 0
    Top = 0
    Align = alTop
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    TabOrder = 0
    Height = 20
    Width = 304
    object ltcbComponents: TcxMDLookupTreeComboBox
      Left = 0
      Top = 0
      Align = alClient
      Properties.ImmediateDropDownWhenActivated = True
      Properties.ImmediateDropDownWhenKeyPressed = False
      Properties.DropDownWidth = 300
      Properties.DropDownHeight = 500
      Properties.DropDownListStyle = lsFixedList
      Properties.OnEditValueChanged = ltcbComponentsPropertiesEditValueChanged
      Properties.ListSource = DataSource
      Properties.KeyField = 'Id'
      Properties.ParentField = 'Parent_Id'
      Properties.CanSelectField = 'CanSelect'
      Properties.ListField = 'Name'
      Properties.ListColumns = <>
      Properties.CanSelectParents = True
      TabOrder = 0
      Width = 264
    end
    object btnObjectList: TcxButton
      Left = 284
      Top = 0
      Width = 20
      Height = 20
      Align = alRight
      PaintStyle = bpsGlyph
      TabOrder = 1
      OnClick = btnObjectListClick
    end
    object btnFocusedControl: TcxButton
      Left = 264
      Top = 0
      Width = 20
      Height = 20
      Align = alRight
      PaintStyle = bpsGlyph
      TabOrder = 2
    end
  end
  object pButtons: TPanel
    Left = 0
    Top = 622
    Width = 304
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    Visible = False
    object btnFastScript: TcxButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 80
      Height = 18
      Align = alLeft
      Caption = 'Fast Script'
      TabOrder = 0
    end
    object btnDfmCurrent: TcxButton
      AlignWithMargins = True
      Left = 175
      Top = 3
      Width = 80
      Height = 18
      Align = alLeft
      Caption = 'Dfm:Current'
      TabOrder = 1
    end
    object btnDfmLoaded: TcxButton
      AlignWithMargins = True
      Left = 89
      Top = 3
      Width = 80
      Height = 18
      Align = alLeft
      Caption = 'Dfm:Loaded'
      TabOrder = 2
    end
    object btnHelper: TcxButton
      AlignWithMargins = True
      Left = 283
      Top = 3
      Width = 18
      Height = 18
      Align = alRight
      Caption = '?'
      OptionsImage.Margin = 3
      TabOrder = 3
      Visible = False
    end
  end
  object lcbComponents: TcxLookupComboBox
    Left = 8
    Top = 26
    Align = alCustom
    Properties.DropDownListStyle = lsFixedList
    Properties.DropDownRows = 30
    Properties.ImmediateDropDownWhenActivated = True
    Properties.IncrementalFilterContainMode = True
    Properties.KeyFieldNames = 'Id'
    Properties.ListColumns = <
      item
        FieldName = 'Name'
      end>
    Properties.ListOptions.GridLines = glNone
    Properties.ListOptions.ShowHeader = False
    Properties.OnEditValueChanged = lcbComponentsPropertiesEditValueChanged
    TabOrder = 2
    Visible = False
    Width = 145
  end
  object DataSet: TVirtualTable
    Options = [voPersistentData, voStored, voSkipUnSupportedFieldTypes]
    Filter = 'Id > 0 or Id < -10'
    IndexFieldNames = 'Name'
    Left = 232
    Top = 32
    Data = {03000000000000000000}
    object DataSetId: TIntegerField
      FieldName = 'Id'
    end
    object DataSetParent_Id: TIntegerField
      FieldName = 'Parent_Id'
    end
    object DataSetName: TStringField
      FieldName = 'Name'
      Size = 256
    end
    object DataSetCanSelect: TBooleanField
      FieldName = 'CanSelect'
    end
  end
  object DataSource: TDataSource
    DataSet = DataSet
    Left = 192
    Top = 32
  end
end
