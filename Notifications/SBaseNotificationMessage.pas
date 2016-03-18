(******************************************************************************
                         -= —истема уведомлений =-
                                 Ќегаш ј.ƒ
                               v2.0 (—) 2012
*******************************************************************************)

{$I CONFIG.INC}

unit SBaseNotificationMessage;

interface

uses
  Types, Messages, Classes, Controls, StdCtrls, ExtCtrls, NamedVariables, SBaseForm,
  SBaseEvents, SBaseUtils;

type
  TSBaseMessageNotifyEvent = procedure(ASender: TObject; AParams: TNamedVariants) of object;

  (*
     омпонент позвол€ет получать и отправл€ть уведомлени€

    CheckParamNames - список имен параметров, которые провер€ютс€ при получении
    уведомлени€:
      - если параметр ParamName из уведомлени€ равен одноименному параметру
    формы (Variables), то ок, оначе уведомление игнорируетс€
  *)
  TSBaseFormNotificationTrap = class(TCustomSBaseNotificationTrap)
  strict private
    FOnNotify        : TSBaseMessageNotifyEvent;
    FCheckParamNames : String;
  protected
    function GetIdentifiersEnumerator: TBaseEnumeratorFactory<TNamedVariantsCustomEnumerator>; override;
    procedure DoNotification(AMessage: TSBaseEventMessage); override;
  public
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;

  published
    property CheckParamNames: string read FCheckParamNames write FCheckParamNames;
    property Enabled;

    property OnNotify: TSBaseMessageNotifyEvent read FOnNotify write FOnNotify;
  end;

  procedure NotificationInit;
//  procedure NotificationFree;

type
  // хранит разные параметры уведомлений
  // загружает их с сервера
  TSBaseNotificationProperties = class(TComponent)
  strict private
    FEnabled: Boolean;
    FObject_Terminate, FObject_DataChanged: Integer;
    FBase: TSBaseFrm;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;
  published
    property Enabled: Boolean read FEnabled write FEnabled;

    property Object_Terminate: Integer read FObject_Terminate write FObject_Terminate;
    property Object_DataChanged: Integer read FObject_DataChanged write FObject_DataChanged;
  end;

var
  SBaseNotificationProperties: TSBaseNotificationProperties;

implementation

uses
  Windows, SysUtils, Variants, Forms, Graphics, Dialogs, Math,
  SBaseVariantFunctions, SBaseDataModule, SBaseStringFunctions, SBaseConstants,
  DataModule,

{$IFDEF FASTSCRIPT_RTTI}
  fs_ievents, fs_iinterpreter
{$ENDIF}
  ;

type
  {$IFDEF FASTSCRIPT_RTTI}
  Tfs_TSBaseFormNotificationTrapEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ASender: TObject; AParams: TNamedVariants);
    function GetMethod: Pointer; override;
  end;

  Tfs_TSBaseNotificationObject = class(TfsRTTIModule)
  public
    constructor Create(AScript: TfsScript); override;
  end;
  {$ENDIF}

procedure NotificationInit;
begin
//  SBaseNotificationProperties := MainDataModule.FindComponent('SBaseNotificationProperties') as TSBaseNotificationProperties;
  SBaseNotificationProperties.Init;
end;

//procedure NotificationFree;
//begin
//  FreeAndNil(SBaseNotificationProperties);
//end;

{$REGION 'TSBaseNotificationProperties'}
constructor TSBaseNotificationProperties.Create(AOwner: TComponent);
begin
  inherited;
  Enabled := False;
end;

destructor TSBaseNotificationProperties.Destroy;
begin
  FreeAndNil(FBase);

  inherited;
end;

procedure TSBaseNotificationProperties.Init;
begin
  FBase := TSBaseFrm.CreateByParams
  (
    Application.MainForm,
    TNamedVariants.Create
    (
      [
        TNamedVariant.Create(SConst_ClassName, ClassName)
      ]
    )
  );

  FBase.FreeNotification(Self);
end;

procedure TSBaseNotificationProperties.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;

  if (AOperation = opRemove) and (AComponent = FBase) then
    FBase := nil;
end;

{$ENDREGION}

{$REGION 'TSBaseFormNotificationTrap'}
procedure TSBaseFormNotificationTrap.DoNotification(AMessage: TSBaseEventMessage);
var
  LParams: TNamedVariants;
begin
  LParams.AssignValues(AMessage.Params);
  LParams.AssignValues(AMessage.Props);

  if Assigned(FOnNotify) then
    FOnNotify(Self, LParams);
end;

{$ENDREGION}

{$REGION 'Tfs_TSBaseNotificationObject'}
{$IFDEF FASTSCRIPT_RTTI}

constructor Tfs_TSBaseNotificationObject.Create(AScript: TfsScript);
begin
  inherited Create(AScript);

  with AScript.AddClass(TSBaseFormNotificationTrap, TSBaseFormNotificationTrap.ClassParent.ClassName) do begin
    AddEvent('OnNotify', Tfs_TSBaseFormNotificationTrapEvent);
  end;
end;

{$ENDIF}
{$ENDREGION}

{$REGION 'Tfs_TSBaseFormNotificationTrapEvent'}
{$IFDEF FASTSCRIPT_RTTI}

procedure Tfs_TSBaseFormNotificationTrapEvent.DoEvent(ASender: TObject; AParams: TNamedVariants);
begin
  CallHandler([ASender, Integer(@AParams)]);
end;

function Tfs_TSBaseFormNotificationTrapEvent.GetMethod: Pointer;
begin
  Result := @Tfs_TSBaseFormNotificationTrapEvent.DoEvent;
end;

{$ENDIF}
{$ENDREGION}

function TSBaseFormNotificationTrap.GetIdentifiersEnumerator: TBaseEnumeratorFactory<TNamedVariantsCustomEnumerator>;
begin
//  if (Owner is TSBaseFrm) then
//  Result := TBaseEnumeratorFactory<TSBaseFormValuesEnumerator>.Create(TSBaseFormValuesEnumerator.Create(TSBaseFrm(Owner), FCheckParamNames))

  Result := TBaseEnumeratorFactory<TNamedVariantsCustomEnumerator>.
              Create
              (
                TSBaseFormValuesEnumerator.Create(TSBaseFrm(Owner), FCheckParamNames)
              )
end;

initialization
  SBaseNotificationProperties := TSBaseNotificationProperties.Create(Application);
  SBaseNotificationProperties.Name := 'SBaseNotificationProperties';

  Classes.RegisterClasses([TSBaseFormNotificationTrap, TSBaseNotificationProperties]);
  fsRTTIModules.Add(Tfs_TSBaseNotificationObject);

finalization
  Classes.UnRegisterClasses([TSBaseFormNotificationTrap, TSBaseNotificationProperties]);
  fsRTTIModules.Remove(Tfs_TSBaseNotificationObject);

end.
