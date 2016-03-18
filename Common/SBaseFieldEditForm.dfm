inherited SBaseFieldEditFrm: TSBaseFieldEditFrm
  Left = 218
  Top = 73
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = #1042#1074#1077#1076#1080#1090#1077' '#1085#1086#1074#1086#1077' '#1079#1085#1072#1095#1077#1085#1080#1077' '#1087#1086#1083#1103
  ClientHeight = 75
  ClientWidth = 203
  Constraints.MinWidth = 205
  Position = poOwnerFormCenter
  ExplicitLeft = 218
  ExplicitTop = 73
  ExplicitWidth = 219
  ExplicitHeight = 113
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox: TcxGroupBox [0]
    Left = 0
    Top = 0
    Align = alClient
    AutoSize = True
    Constraints.MinHeight = 21
    PanelStyle.Active = True
    PanelStyle.OfficeBackgroundKind = pobkStyleColor
    Style.BorderStyle = ebsSingle
    TabOrder = 0
    Height = 51
    Width = 203
    object pType: TLayoutPanel
      Left = 2
      Top = 2
      Width = 199
      Caption.Text = #1058#1080#1087
      Caption.Left = 10
      Caption.Width = 80
      Caption.Font.Charset = DEFAULT_CHARSET
      Caption.Font.Color = clWindowText
      Caption.Font.Height = -11
      Caption.Font.Name = 'Tahoma'
      Caption.Font.Style = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TabOrder = 0
      Visible = False
      object cbType: TcxComboBox
        Left = 90
        Top = 1
        Align = alClient
        Properties.DropDownListStyle = lsFixedList
        Properties.ImmediatePost = True
        Properties.Items.Strings = (
          'String'
          'Text'
          'Integer'
          'Float'
          'Date'
          'Time'
          'DateTime'
          'Boolean')
        Properties.OnChange = cbTypePropertiesChange
        TabOrder = 0
        Width = 109
      end
    end
    object pValue: TLayoutPanel
      Left = 2
      Top = 25
      Width = 199
      Caption.Text = #1047#1085#1072#1095#1077#1085#1080#1077
      Caption.Left = 10
      Caption.Width = 80
      Caption.Font.Charset = DEFAULT_CHARSET
      Caption.Font.Color = clWindowText
      Caption.Font.Height = -11
      Caption.Font.Name = 'Tahoma'
      Caption.Font.Style = []
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Constraints.MinHeight = 23
      TabOrder = 1
    end
    object eSpinEdit: TcxSpinEdit
      Left = 0
      Top = 2
      Properties.EditFormat = '0,'
      TabOrder = 2
      Visible = False
      Width = 109
    end
    object eMemoEdit: TcxMemo
      Left = 2
      Top = 48
      Properties.ScrollBars = ssVertical
      TabOrder = 3
      Visible = False
      Height = 0
      Width = 199
    end
    object eCalcEdit: TcxCalcEdit
      Left = 0
      Top = 2
      EditValue = 0.000000000000000000
      Properties.ImmediatePost = True
      TabOrder = 4
      Visible = False
      Width = 164
    end
    object eDateEdit: TcxDateEdit
      Left = 0
      Top = 2
      Properties.ShowTime = False
      TabOrder = 5
      Visible = False
      Width = 164
    end
    object eTimeEdit: TcxTimeEdit
      Left = 0
      Top = 2
      EditValue = 0d
      TabOrder = 6
      Visible = False
      Width = 164
    end
    object eStringEdit: TcxTextEdit
      Left = 0
      Top = 2
      TabOrder = 7
      Visible = False
      Width = 164
    end
    object eBooleanEdit: TcxUserCheckBox
      Left = 0
      Top = 2
      Properties.AllowGrayed = True
      Properties.DisplayGrayed = 'Null'
      Properties.ImmediatePost = True
      Properties.OnChange = eBooleanEditPropertiesChange
      Style.LookAndFeel.NativeStyle = True
      StyleDisabled.LookAndFeel.NativeStyle = True
      Caption = 'False'
      Checked = False
      Constraints.MinHeight = 14
      StyleFocused.LookAndFeel.NativeStyle = True
      StyleHot.LookAndFeel.NativeStyle = True
      TabOrder = 8
      Visible = False
    end
  end
  object pButtons: TPanel [1]
    Left = 0
    Top = 51
    Width = 203
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 1
    Padding.Top = 1
    Padding.Right = 1
    Padding.Bottom = 1
    TabOrder = 5
    object btnOk: TButton
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 75
      Height = 22
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 0
      Align = alLeft
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 81
      Top = 1
      Width = 75
      Height = 22
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 0
      Align = alLeft
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1072
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  inherited BarManager: TdxMDBarManager
    Left = 148
    Top = 16
    DockControlHeights = (
      0
      0
      0
      0)
  end
  inherited ActionList: TActionList
    Left = 104
  end
end
