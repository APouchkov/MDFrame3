unit fsFunction_TVCLZip;
{$I config.inc}

interface

uses fs_iinterpreter, VCLZip, VCLUnZip;
{$IFDEF FASTSCRIPT_RTTI}
type
  TFunctions_TVCLZip = class(TfsRTTIModule)
  private

    function  Call_TVCLZip_Method(
      Instance: TObject;
      ClassType: TClass;
      const MethodName: String;
      Caller: TfsMethodHelper
    ): Variant;

    function  Get_TVCLZip_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TVCLZip_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

implementation

uses Variants, SBaseConstants;

{ TFunctions_TVCLZip }

{$IFDEF FASTSCRIPT_RTTI}
function TFunctions_TVCLZip.Call_TVCLZip_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'ZIP' then
    Result := TVCLZip(Instance).Zip;
end;

constructor TFunctions_TVCLZip.Create(AScript: TfsScript);
const
  SCategory: String = 'TVCLZip';
begin
  inherited Create(AScript);

  with AScript.AddClass(TVCLZip, TVCLZip.ClassParent.ClassName) do begin
    AddMethod('fuction Zip(): Integer', Call_TVCLZip_Method);
    AddProperty('ZipName', DelphiTypeString, Get_TVCLZip_Property, Set_TVCLZip_Property);
  end;

end;

function TFunctions_TVCLZip.Get_TVCLZip_Property(Instance: TObject;
  ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'ZIPNAME' then
    Result := TVCLZip(Instance).ZipName;
end;

procedure TFunctions_TVCLZip.Set_TVCLZip_Property(Instance: TObject;
  ClassType: TClass; const PropName: String; Value: Variant);
begin
  if PropName = 'ZIPNAME' then
    TVCLZip(Instance).ZipName := VarToStr(Value);
end;
{$ENDIF}

initialization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TFunctions_TVCLZip);
{$ENDIF}
end.
