unit dhDesignEditors;

interface

uses Classes, DB, ToolsAPI, StrEdit, DesignEditors,
  DesignIntf, Forms, DHEditFrm, psDesignEditors, ItemPropStorage;

type
  TDHComponentEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TDHEditFormProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDHFieldNameProperty = class(TStringProperty)
  public
    function GetDataSet: TDataSet; virtual;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDHMasterFieldNameProperty = class(TDHFieldNameProperty)
  public
    function GetDataSet: TDataSet; override;
  end;

implementation

uses
  SysUtils, DataHandler, TypInfo;

resourcestring
  SEditStruct = 'Редактировать структуру...';

const
  SDataSet = 'DataSet';  

{ TDHComponentEditor }

function TDHComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := SEditStruct;
end;

function TDHComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TDHComponentEditor.ExecuteVerb(Index: Integer);

  procedure ShowEditor();
  var
    i: Integer;
  begin
    for i := 0 to Pred(Screen.FormCount) do
      if (Screen.Forms[i] is TDHEditForm) and
        (TDHEditForm(Screen.Forms[i]).DataHandler = Component)
      then
        with Screen.Forms[i] do begin
          Show;
          BringToFront;
          exit;
        end;

    with TDHEditForm.Create(nil) do
      try
        DataHandler := Component as TCustomDataHandler;
        Designer := Self.Designer;
        Show;
        BringToFront;
      except
        Free;
        Raise;
      end;
  end;
  
begin
  if Index = 0 then
    ShowEditor();
end;

{ TDHEditFormProperty }

function TDHEditFormProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList];
end;

procedure TDHEditFormProperty.GetValues(Proc: TGetStrProc);
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  ModuleInfo: IOTAModuleInfo;
  ProjectGroup: IOTAProjectGroup;
  Project: IOTAProject;
  I, J: Integer;
begin
  if Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
    for I := 0 to Pred(ModuleServices.ModuleCount) do begin
      Module := ModuleServices.Modules[I];
      if Supports(Module, IOTAProjectGroup, ProjectGroup) then begin
        Project := ProjectGroup.GetActiveProject();
        if Assigned(Project) then
          for J := 0 to Pred(Project.GetModuleCount) do begin
            ModuleInfo := Project.GetModule(j);
            if (ModuleInfo.ModuleType = omtForm) and (ModuleInfo.FormName <> '') then
              Proc('T' + ModuleInfo.FormName);
          end;
      end;
    end;
end;

{ TDHFieldNameProperty }

function TDHFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList];
end;

procedure TDHFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStrings;
  ADataSet: TDataSet;
begin
  Values := TStringList.Create;
  try
    ADataSet := GetDataSet();
    if Assigned(ADataSet) and
      ((ADataSet.FieldList.Count > 0) or (ADataSet.FieldDefList.Count > 0))
    then
      ADataSet.GetFieldNames(Values);
    for I := 0 to Pred(Values.Count) do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

function TDHFieldNameProperty.GetDataSet: TDataSet;
begin
  Result := (GetComponent(0) as TCustomDataHandler).DataSet;
end;

{ TDHMasterFieldNameProperty }

function TDHMasterFieldNameProperty.GetDataSet: TDataSet;
var
  ADataHandler: TCustomDataHandler;
begin
  ADataHandler := (GetComponent(0) as TSubDataHandler).Parent;
  if Assigned(ADataHandler) then
    Result := ADataHandler.DataSet
  else
    Result := nil;
end;

end.
