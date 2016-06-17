inherited SBaseEditFrmDh: TSBaseEditFrmDh
  Left = 329
  Top = 65
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelMain: TcxGroupBox
    inherited PanelForm: TGroupPanel
      inherited PanelButtons: TGroupPanel
        inherited btnApply: TcxButton
          Visible = True
        end
      end
    end
  end
  object DataSetMain: TSQLDataSetProvider [1]
    Params = <>
    Left = 131
    Top = 106
  end
  inherited BarManager: TdxMDBarManager
    Font.Charset = DEFAULT_CHARSET
    Categories.Strings = (
      'DataHandlerMain')
    Categories.ItemsVisibles = (
      255)
    Style = bmsOffice11
    Left = 459
    Top = 4
    DockControlHeights = (
      0
      0
      0
      0)
  end
  inherited ActionList: TActionList
    Top = 52
  end
  object DataHandlerMain: TDataHandler
    DataSet = DataSetMain
    Caption = 'Main DataSet'
    OnBeforeValidate = DataHandlerMainBeforeValidate
    OnBeforeApply = DataHandlerMainBeforeApply
    OnBeforeCommit = DataHandlerMainBeforeCommit
    OnBeforeRollBack = DataHandlerMainBeforeRollBack
    OnAfterCommit = DataHandlerMainAfterCommit
    OnLockError = DataHandlerMainLockError
    PropStorage = PropStorageDh
    SupportClassName = 'TBaseDataHandlerSupport'
    OnModifiedChanged = DataHandlerMainModifiedChanged
    OnApplyUpdates = DataHandlerMainApplyUpdates
    Left = 93
    Top = 106
  end
  object PropStorageDh: TdhItemPropStorage
    DefaultImages.Copy = 0
    Left = 55
    Top = 106
  end
end
