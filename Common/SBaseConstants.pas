unit SBaseConstants;

interface

uses
  Classes, Menus, SysUtils;

type
  TXMLMode = (xmlmAttributes, xmlmRecords);

const
  SConst_CR                          = #13#10;
  SConst_Application        : String = 'Application';
  SConst_ObjectInspector    : String = 'ObjectInspector';
  SConst_Open               : String = 'Open';
  SConst_Stream             : String = 'Stream';
  SConst_Separator          : String = 'Separator';
  SConst_Focused            : String = 'Focused';
  SConst_Options            : String = 'Options';
  SConst_Visible            : String = 'Visible';
  SConst_Count              : String = 'Count';
  SConst_Item               : String = 'Item';
  SConst_ItemIndex          : String = 'ItemIndex';
  SConst_Styles             : String = 'Styles';
  SConst_Row                : String = 'Row';
  SConst_Id                 : String = 'Id';
  SConst_KeyValue           : String = 'KeyValue';
  SConst_RecNo              : String = 'RecNo';
  SConst_RecordCount        : String = 'RecordCount';
  SConst_IDs                : String = 'IDs';
  SConst_Items              : String = 'Items';
  SConst_Parent             : String = 'Parent';
  SConst_Parents            : String = 'Parents';
  SConst_Parent_Id          : String = 'Parent_Id';
  SConst_MasterField_Id     : String = 'MasterField_Id';
  SConst_MasterFields       : String = 'MasterFields';
  SConst_DetailFields       : String = 'DetailFields';
  SConst_Root               : String = 'Root';
  SConst_Name               : String = 'Name';
  SConst_Value              : String = 'Value';
  SConst_Values             : String = 'Values';
  SConst_List               : String = 'List';
  SConst_EditValue          : string = 'EditValue';
  SConst_Length             : String = 'Length';
  SConst_Font               : String = 'Font';
  SConst_Precision          : String = 'Precision';
  SConst_Scale              : String = 'Scale';
  SConst_Language           : String = 'Language';
  SConst_Date               : String = 'Date';
  SConst_DateTime           : String = 'DateTime';
  SConst_Today              : String = 'Today';
  SConst_Now                : String = 'Now';
  SConst_ActiveDate         : String = 'ActiveDate';
  SConst_NewGUId            : String = 'NewGUId';
  SConst_Version            : String = 'Version';
  SConst_FieldName          : String = 'FieldName';
  SConst_Params             : String = 'Params';
  SConst_Parameters         : String = 'Parameters';
  SConst_AParameters        : String = 'AParameters';
  SConst_ParamName          : String = 'ParamName';
  SConst_Collisions         : String = 'Collisions';
  SConst_ACollisions        : String = 'ACollisions';
  SConst_Index              : String = 'Index';
  SConst_Line               : String = 'Line';

  SConst_SyncMode           : String = 'SyncMode';
  SConst_Indicator          : String = 'Indicator';
  SConst_QuickCustomization : String = 'QuickCustomization';
  SConst_Bands              : String = 'Bands';
  SConst_Headers            : String = 'Headers';
  SConst_AutoHeight         : String = 'AutoHeight';
  SConst_AutoWidth          : String = 'AutoWidth';
  SConst_Sorting            : String = 'Sorting';
  SConst_Filtering          : String = 'Filtering';
  SConst_Grouping           : String = 'Grouping';
  SConst_AlwaysExpanded     : String = 'AlwaysExpanded';
  SConst_AlwaysExpandedLevel: String = 'AlwaysExpandedLevel';
  SConst_Categorized        : String = 'Categorized';
  SConst_HideFocusRect      : String = 'HideFocusRect';
  SConst_HideSelection      : String = 'HideSelection';

  SConst_Paused             : String = 'Paused';
  SConst_Down               : String = 'Down';
  SConst_Default            : String = 'Default';
  SConst_Command            : String = 'Command';
  SConst_CommandText        : String = 'CommandText';
  SConst_SQLText            : String = 'SQL.Text';
  SConst_Link               : String = 'Link';
  SConst_DisplayFields      : String = 'DisplayFields';
  SConst_Subject            : String = 'Subject';
  SConst_Action             : String = 'Action';
  SConst_Unique             : String = 'Unique';
  SConst_Url                : String = 'URL';
  SConst_Text               : String = 'Text';
  SConst_Number             : String = 'Number';
  SConst_Numeration         : String = 'Numeration';
  SConst_Header             : String = 'Header';
  SConst_Message            : String = 'Message';
  SConst_Info               : String = 'Info';
  SConst_Exists             : String = 'Exists';
  SConst_Timer              : String = 'Timer';
  SConst_Request            : String = 'Request';
  SConst_Result             : String = 'Result';
  SConst_Sign               : String = 'Sign';
  SConst_Constants          : String = 'Constants';
  SConst_Dependencies       : String = 'Dependencies';
  SConst_Type               : String = 'Type';
  SConst_Description        : String = 'Description';
  SConst_Type_Id            : String = 'Type_Id';
  SConst_Mask               : String = 'Mask';
  SConst_AnsiString         : String = 'AnsiString';
  SConst_WideString         : String = 'WideString';
  SConst_Param              : String = 'Param';
  SConst_PARAM_             : String = 'PARAM_';
  SConst_PARAM_OUT_         : String = 'PARAM_OUT_';
  SConst_PARAM_INOUT_       : String = 'PARAM_INOUT_';
  SConst_From               : String = 'From';
  SConst_AND                : String = 'AND';
  SConst_OR                 : String = 'OR';
  SConst_LIKE               : String = 'LIKE';
  SConst_Class              : String = 'Class';
  SConst_Classes            : String = 'Classes';
  SConst_ClassName          : String = 'ClassName';
  SConst_ParentClassName    : String = 'ParentClassName';
  SConst_ClassConstants     : String = 'ClassConstants';
  SConst_ClassParams        : String = 'ClassParams';
  SConst_OnSelect           : String = 'OnSelect';
  SConst_AutoPostCount      : String = 'AutoPostCount';
  SConst_ShowModal          : String = 'ShowModal';
  SConst_ShowProgress       : String = 'ShowProgress';

  SConst_Done               : String = 'Done';
  SConst_Found              : String = 'Found';
  SConst_Allow              : String = 'Allow';
  SConst_Mode               : String = 'Mode';
  SConst_Edit               : String = 'Edit';
  SConst_Position           : String = 'Position';
  SConst_NativeStyle        : String = 'NativeStyle';
  SConst_StandardStyle      : String = 'StandardStyle';
  SConst_SkinStyle          : String = 'SkinStyle';
  SConst_Encoding           : String = 'Encoding';
  SConst_Unicode            : String = 'Unicode';

  SConst_Thread             : String = 'Thread';
  SConst_Source             : String = 'Source';
  SConst_Target             : String = 'Target';
  SConst_Targets            : String = 'Targets';
  SConst_OpenDialog         : String = 'OpenDialog';
  SConst_Output             : String = 'Output';
  SConst_FreeOnTerminate    : String = 'FreeOnTerminate';
  SConst_ClearFields        : String = 'ClearFields';
  SConst_ClearTables        : String = 'ClearTables';
  SConst_Native             : String = 'Native';
  SConst_Closed             : String = 'Closed';
  SConst_DataBinding        : String = 'DataBinding';
  SConst_Skin               : String = 'Skin';
  SConst_Node               : String = 'Node';
  SConst_Columns            : String = 'Columns';
  SConst_ImageIndex         : String = 'ImageIndex';
  SConst_ShortCut           : String = 'ShortCut';
  SConst_CanClose           : String = 'CanClose';
  SConst_Accept             : String = 'Accept';
  SConst_Selected           : String = 'Selected';
  SConst_SelectedNode       : String = 'SelectedNode';
  SConst_DragNode           : String = 'DragNode';
  SConst_State              : String = 'State';
  SConst_Offset             : String = 'Offset';
  SConst_Password           : String = 'Password';
  SConst_IncludeTrailingPathDelimiter: String = 'IncludeTrailingPathDelimiter';
  SConst_Embeded            : String = 'Embeded';
  SConst_IconIndex          : String = 'IconIndex';

  SConst_NotificationTableChangesUpdatedObjects: String = '#Notification#TableChanges#UpdatedObjects';

  SQLtypeBit                : String = 'Bit';
  SQLtypeTinyInt            : String = 'TinyInt';
  SQLtypeSmallInt           : String = 'SmallInt';
  SQLtypeInt                : String = 'Int';
  SQLtypeBigInt             : String = 'BigInt';
  SQLtypeFloat              : String = 'Float';
  SQLtypeVarChar            : String = 'VarChar';
  SQLtypeNVarChar           : String = 'NVarChar';
  SQLtypeChar               : String = 'Char';
  SQLtypeNumeric            : String = 'Numeric';
  SQLtypeDecimal            : String = 'Decimal';
  SQLtypeCurrency           : String = 'TMoney';
  SQLtypeSmallDateTime      : String = 'SmallDateTime';
  SQLtypeDateTime           : String = 'DateTime';
  SQLtypeDateTimeOffset     : String = 'DateTimeOffset';
  SQLtypeDate               : String = 'Date';
  SQLtypeTime               : String = 'Time';

  SQLQuerySelectFrom        : String = 'SELECT %s FROM %s';

  DelphiTypeBoolean         : String = 'Boolean';
  DelphiTypeTinyByte        : String = 'Byte';
  DelphiTypeWord            : String = 'Word';
  DelphiTypeSmallInt        : String = 'SmallInt';
  DelphiTypeShortInt        : String = 'ShortInt';
  DelphiTypeInteger         : String = 'Integer';
  DelphiTypeBigInteger      : String = 'BigInteger';
  DelphiTypeLongWord        : String = 'LongWord';
  DelphiTypeFloat           : String = 'Float';
  DelphiTypeExtended        : String = 'Extended';
  DelphiTypeBcd             : String = 'Bcd';
  DelphiTypeFMTBcd          : String = 'FMTBcd';
  DelphiTypeString          : String = 'String';
  DelphiTypeText            : String = 'Text';
  DelphiTypeChar            : String = 'Char';
  DelphiTypeDateTime        : String = 'DateTime';
  DelphiTypeDate            : String = 'Date';
  DelphiTypeTime            : String = 'Time';
  DelphiTypeVariant         : String = 'Variant';
  DelphiTypeGUId            : String = 'GUId';

  SConst_Window             : String = 'Window';
  SConst_Sender             : String = 'Sender';
  SConst_ReadOnly           : String = 'ReadOnly';
  SConst_Icon               : String = 'Icon';
  SConst_Color              : String = 'Color';
  SConst_Enabled            : String = 'Enabled';
  SConst_ItemName           : String = 'ItemName';
  SConst_Tag                : String = 'Tag';
  SConst_Limit              : String = 'Limit';
  SConst_BarManager         : String = 'BarManager';

  SConst_Path               : String = 'Path';
  SConst_InitialDir         : String = 'InitialDir';

  SConst_Excel              : String = 'Excel';
  SConst_FastReport         : String = 'FastReport';

  SConst_Provider           : String = 'Provider';
  SConst_Server             : String = 'Server';
  SConst_DataBase           : String = 'DataBase';
  SConst_Port               : String = 'Port';
  SConst_User               : String = 'User';

  SConst_RETURN_VALUE       : String = 'RETURN_VALUE';
  SConst_Object_Id          : String = 'Object_Id';
  SConst_ObjectName         : String = 'ObjectName';
  SConst_Owner_Id           : String = 'Owner_Id';
  SConst_User_Id            : String = 'User_Id';
  SConst_Message_Id         : String = 'Message_Id';
  SConst_Folder_Id          : String = 'Folder_Id';
  SConst_Action_Id          : String = 'Action_Id';
  SConst_Recipients         : String = 'Recipients';
  SConst_Select             : String = 'Select';
  SConst_Exec               : String = 'Exec';
  SConst_Execute            : String = 'Execute';
  SConst_Call               : String = 'Call';
  SConst_Status             : String = 'Status';
  SConst_Refresh            : String = 'Refresh';
  SConst_Insert             : String = 'Insert';
  SConst_Copy               : String = 'Copy';
  SConst_Modify             : String = 'Modify';
  SConst_Delete             : String = 'Delete';
  SConst_Table              : String = 'Table';
  SConst_Tables             : String = 'Tables';
  SConst_Field              : String = 'Field';
  SConst_Group              : String = 'Group';
  SConst_Groups             : String = 'Groups';
  SConst_Title              : String = 'Title';
  SConst_View               : String = 'View';
  SConst_Caption            : String = 'Caption';
  SConst_CaptionFormat      : String = 'CaptionFormat';
  SConst_Active             : String = 'Active';
  SConst_DataSetActive      : String = 'DataSetActive';
  SConst_Prefix             : String = 'Prefix';
  SConst_Expand             : String = 'Expand';
  SConst_Expanded           : String = 'Expanded';
  SConst_Fields             : String = 'Fields';
  SConst_Level              : String = 'Level';
  SConst_ALevel             : String = 'ALevel';
  SConst_Method             : String = 'Method';
  SConst_Base               : String = 'Base';
  SConst_Code               : String = 'Code';
  SConst_Data               : String = 'Data';
  SConst_SQL                : String = 'SQL';
  SConst_SQL_Text           : String = 'SQL.Text';
  SConst_Synonym            : String = 'Synonym';
  SConst_Object             : String = 'Object';
  SConst_Key                : String = 'Key';
  SConst_TableName          : String = 'TableName';
  SConst_Dir                : String = 'Dir';

  SConst_Archive            : String = 'Archive';
  SConst_File               : String = 'File';
  SConst_Files              : String = 'Files';
  SConst_FileName           : String = 'FileName';
  SConst_FileNames          : String = 'FileNames';
  SConst_FileMask           : String = 'FileMask';
  SConst_FileSource         : String = 'FileSource';
  SConst_FileType           : String = 'FileType';
  SConst_FileDescription    : String = 'FileDescription';
//  SConst_FileFilter         : String = 'FileFilter';
  SConst_FilePath           : String = 'FilePath';
  SConst_FileExtention      : String = 'FileExtention';

  SConst_DisplayText        : String = 'DisplayText';
  SConst_CanSelect          : String = 'CanSelect';
  SConst_Binary             : String = 'Binary';
  SConst_WideChar           : String = 'WideChar';
  SConst_Master             : String = 'Master';
  SConst_Details            : String = 'Details';
  SConst_Properties         : String = 'Properties';
  SConst_Procedure          : String = 'Procedure';
  SConst_ProcedureName      : String = 'ProcedureName';
  SConst_ProcedureParams    : String = 'ProcedureParams';
  SConst_Connection         : String = 'Connection';
  SConst_DataSet            : String = 'DataSet';
  SConst_DataSets           : String = 'DataSets';
  SConst_DataSetCount       : String = 'DataSetCount';
  SConst_DataSet_Index      : String = 'DataSet_Index';
  SConst_MemoryDataSet      : String = 'MemoryDataSet';
  SConst_DataSource         : String = 'DataSource';
  SConst_DataSources        : String = 'DataSources';
  SConst_Controllers        : String = 'Controllers';
  SConst_DataController     : String = 'DataController';
//  SConst_MemoryDataSet      : String = 'MemoryDataSet';
  SConst_Attribute          : String = 'Attribute';
  SConst_Signed             : String = 'Signed';
  SConst_DataModule         : String = 'DataModule';
  SConst_Self               : String = 'Self';
  SConst_Form               : String = 'Form';
  SConst_Filter             : String = 'Filter';
  SConst_Sort               : String = 'Sort';
  SConst_Document           : String = 'Document';
  SConst_Report             : String = 'Report';
  SConst_Reports            : String = 'Reports';
  SConst_Privileges         : String = 'Privileges';
  SConst_DFM                : String = 'DFM';
  SConst_Script             : String = 'Script';
  SConst_ScriptProcedure    : String = 'ScriptProcedure';
  SConst_ScriptConstants    : String = 'ScriptConstants';
  SConst_ScriptParams       : String = 'ScriptParams';

  SConst_Confirmation       : String = 'Confirmation';
  SConst_Identity           : String = 'Identity';
  SConst_Left               : String = 'Left';
  SConst_Right              : String = 'Right';
  SConst_Top                : String = 'Top';
  SConst_Width              : String = 'Width';
  SConst_Height             : String = 'Height';
  SConst_FitToWidth         : String = 'FitToWidth';
  SConst_FitToHeight        : String = 'FitToHeight';
  SConst_Style              : String = 'Style';
  SConst_Column             : String = 'Column';
  SConst_Tree               : String = 'Tree';
  SConst_BeginDate          : String = 'BeginDate';
  SConst_EndDate            : String = 'EndDate';
  SConst_DateBegin          : String = 'DateBegin';
  SConst_DateEnd            : String = 'DateEnd';
  SConst_DatesPeriod        : String = 'DatesPeriod';
  SConst_DecimalPlaces      : String = 'DecimalPlaces';
  SConst_NullAble           : String = 'NullAble';
  SConst_SpinEdit           : String = 'SpinEdit';
  SConst_Interval           : String = 'Interval';
  SConst_BeginGroup         : String = 'BeginGroup';

  SConst_Login              : String = 'Login';
  SConst_Xml                : String = 'Xml';
  SConst_MD5                : String = 'MD5';

  SConst_Template           : String = 'Template';
  SConst_ConnectionString   : String = 'ConnectionString';
  SConst_LoginPrompt        : String = 'LoginPrompt';
  SConst_ConnectionPrompt   : String = 'ConnectionPrompt';

  SConst_True               : String = 'True';
  SConst_False              : String = 'False';
  SConst_NULL               : String = 'NULL';
  SFilterFalseCondition     : String = '1=0';

  SRegistryPath_Software    : String = '\Software';

  SConst_Year               : String = 'Year';
  SConst_Years              : String = 'Years';
  SConst_Month              : String = 'Month';
  SConst_Months             : String = 'Months';
  SConst_Day                : String = 'Day';
  SConst_Days               : String = 'Days';

  TConst_TColor             : String = 'TColor';
  TConst_TFont              : String = 'TFont';

  SCondition_IS_NOT_NULL    : String = ' IS NOT NULL';

  XMLConst_RECORD           : String = 'RECORD';
  XMLConst_NAME             : String = 'NAME';
  XMLConst_VALUE            : String = 'VALUE';
  XMLConst_PARAM            : String = 'PARAM';
  XMLConst_TYPE             : String = 'TYPE';

  SAbstractError            : String = 'Abstract Error';

  SUnSupportedObject        : String = 'Object you are selected is not supported';
  MUnSupportedMethod        : String = 'Method "%s" unsupported by this object';
  MFunctionNotFound         : String = 'Function "%s" not found';
  MDataSetNotFound          : String = 'DataSet "%s" not found';

  SError_ControlMustHaveValue       : String = 'Не указано значение поля.';
  SError_ControlHaveIncorrentValue  : String = 'Значение поля указано неверно.';
  MError_ColumnByFieldNotFound      : String = 'Не удалось найти колонку для поля '#171'%s'#187' указано неверно.';

  MError_DataSetParamNameNotFound   : String = 'В датасете '#171'%s'#187' не удалось найти параметр '#171'%s'#187'.';
  MError_ParamNameNotFound          : String = 'Не удалось найти параметр '#171'%s'#187'.';

  SConfirm_ControlHaveNoValue       : String = 'Не указано значение поля. Это нормально?';
  SError_RecordNoFound              : String = 'Запись не найдена (не существует). Возможно она была удалена, либо оформлена с ошибкой.';
  MError_UnknownFormatType          : String = 'Неизвестный тип форматирования строки: '#171'%s'#187;
  MError_ConfirmSaveChanges         : String = 'Сохранить все сделанные изменения?';

  SDebugTime_Exec                   : String = 'Выполнение на сервере: ';
  SDebugTime_Buld                   : String = 'Построение отчета: ';
  SDebugTime_Total                  : String = 'Общее время: ';
  SDebugTime_Format                 : String = 'hh:nn:ss.zzz';

var
  ShortCut_F3             : TShortCut;
  ShortCut_F4             : TShortCut;
  ShortCut_F5             : TShortCut;
  ShortCut_F6             : TShortCut;
  ShortCut_Ins            : TShortCut;
  ShortCut_Enter          : TShortCut;
  ShortCut_Ctrl_Enter     : TShortCut;
  ShortCut_Del            : TShortCut;
  ShortCut_Ctrl_Del       : TShortCut;
  ShortCut_Shift_Del      : TShortCut;
  ShortCut_Ctrl_C         : TShortCut;
  ShortCut_Ctrl_V         : TShortCut;
  ShortCut_Ctrl_X         : TShortCut;
  ShortCut_Ctrl_A         : TShortCut;


implementation

initialization
  ShortCut_F3           := TextToShortCut('F3');
  ShortCut_F4           := TextToShortCut('F4');
  ShortCut_F5           := TextToShortCut('F5');
  ShortCut_F6           := TextToShortCut('F6');

  ShortCut_Ins          := TextToShortCut('Ins');
  ShortCut_Enter        := TextToShortCut('Enter');
  ShortCut_Ctrl_Enter   := TextToShortCut('Ctrl+Enter');

  ShortCut_Del          := TextToShortCut('Del');
  ShortCut_Ctrl_Del     := TextToShortCut('Ctrl+Del');
  ShortCut_Shift_Del    := TextToShortCut('Shift+Del');

  ShortCut_Ctrl_C       := TextToShortCut('Ctrl+C');
  ShortCut_Ctrl_V       := TextToShortCut('Ctrl+V');
  ShortCut_Ctrl_X       := TextToShortCut('Ctrl+X');
  ShortCut_Ctrl_A       := TextToShortCut('Ctrl+A');

end.
