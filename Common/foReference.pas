{******************************************************************************}
{                        -= Интерфейс справочников =-                          }
{                                                                              }
{                               v1.0 (С) 2006                                  }
{                                                                              }
{                                  -=NLT=-                                     }
{                                                                              }
{   Для преобразования формы к справочнику, нужно делать так:                  }
{                                                                              }
{   ШАГ 1:                                                                     }
{         Добавляем к БАЗОВОЙ форме интерфейс IReference (модуль foReference)  }
{   ШАГ 2:                                                                     }
{         Объявляем в форме переменную FCallBackProc: TRefCallBackProc;        }
{   ШАГ 3:                                                                     }
{         Реализуем методы :                                                   }
{            /* для инициализации справочника */                               }
{            ReferenceInit (const Params: array of Variant;                    }
{                           const DefaultValue: Variant;                       }
{                           CallBackProc: TRefCallBackProc)                    }
{         Пример:                                                              }
{           procedure TMyForm.ReferenceInit(const Params: array of Variant;    }
{            const DefaultValue: Variant; CallBackProc: TRefCallBackProc);     }
{           begin                                                              }
{             FCallBackProc := CallBackProc;                                   }
{             ...                                                              }
{           end;                                                               }
{            /* для возвращения значений */                                    }
{            GetReferenceValue(const FieldNames: String): Variant;             }
{         Пример:                                                              }
{           function TMyForm.GetReferenceValue(                                }
{                                    const FieldNames: String): Variant;       }
{           begin                                                              }
{             if MyDataSet.IsEmpty then                                        }
{               Result := Null                                                 }
{             else                                                             }
{             if FieldNames = '' then                                          }
{               Result := MyDataSetPrimaryKey.Value                            }
{             else                                                             }
{               Result := MyDataSet.FieldValues[FieldNames];                   }
{           end;                                                               }
{   ШАГ 4:                                                                     }
{         При выборе формы, если определена переменная FCallBackProc, то       }
{           вызываем её как метод с параметром rfSelect                        }
{                                                                              }
{         Пример:                                                              }
{                                                                              }
{           if Assigned(FCallBackProc) then                                    }
{             FCallBackProc(Self, rfSelect, [])                                }
{           else                                                               }
{             ModalResult := mrOk;                                             }
{   ШАГ 5:                                                                     }
{         При обновлении данных, если определена переменная FCallBackProc, то  }
{           вызываем её как метод с параметром rfRefresh                       }
{         Пример:                                                              }
{           if Assigned(FCallBackProc) then                                    }
{             FCallBackProc(Self, rfRefresh, []);                              }
{   ШАГ 6:                                                                     }
{         При изменении параметров, если определена переменная FCallBackProc,  }
{           то вызываем её как метод с параметром rfParams                     }
{         Пример:                                                              }
{           if Assigned(FCallBackProc) then                                    }
{             FCallBackProc(Self, rfParams, [параметр1, параметр2, ...]);      }
{                                                                              }
{******************************************************************************}
unit foReference;

interface

uses
  Classes, DB, SBaseForm;

type
  TRefEvent = (rfSelect, rfRefresh, rfParams);
  TRefEvents = procedure (Sender: TObject; const Value: Variant) of object;
  TRefCallBackProc = procedure (Sender: TObject; Event: TRefEvent; Params: Array of Variant) of object;

  IReference = interface
  ['{AF02D2E2-0A9B-4BFC-ABBA-C1948F1532AC}']
    function  GetReferenceValue(const FieldNames: String): Variant;
    procedure ReferenceInit(const Params: Array of Variant; const DefaultValue: Variant; CallBackProc: TRefCallBackProc);
  end;

  function SelectRef(ClassForm: TSBaseFrmClass; const Params: Array of Variant; const DefaultValue: Variant; CallBackProc: TRefEvents = nil; const FieldNames: String = ''): Variant;
  procedure ClearRefBuffer;

resourcestring
  SRefNoParam = 'Не указан обязателный пареметр справочника ''%s'' - ''%s''';

implementation

uses
  SysUtils, Windows, Variants, Controls, ExtCtrls, Forms, Dialogs, finOffice;

const
  ReferenceRefreshTime = 5; // Время обновления данных - (5 сек.)
  ReferenceFormActualTime =  300; // Время хранения справочника - (5 мин.)
  ReferenceFormRefreshTime = 3600; // Время обновления справочника - (1 ч.)

resourcestring
  SErrorNoInterface = 'Класс %s не поддерживает интерфейс справочников';

type
  TReferenceItem = class
  private
    FForm: TSBaseFrm;
    FLiveTime: Integer;
    FClassForm: TSBaseFrmClass;
    FParams: Array of Variant;
    FRefreshTime: Integer;
    function GetParamCount: Integer;
    function GetParams(Index: Integer): Variant;
    procedure SetParams(Index: Integer; const Value: Variant);
  protected
    function Equal(ClassForm: TSBaseFrmClass; const Params: array of Variant): Boolean;
    procedure AssignParams(const Params: array of Variant);
    property LiveTime: Integer read FLiveTime write FLiveTime;
    property RefreshTime: Integer read FRefreshTime write FRefreshTime;
    property Form: TSBaseFrm read FForm write FForm;
    property ClassForm: TSBaseFrmClass read FClassForm write FClassForm;
    property ParamCount: Integer read GetParamCount;
    property Params[Index: Integer]: Variant read GetParams write SetParams;
  public
    constructor Create(Form: TSBaseFrm; const Params: Array of Variant);
    destructor Destroy; override;
  end;

  TReferenceList = class
  private
    FList: TList;
    FTimer: TTimer;
    FFieldNames: String;
    FCallBackProc: TRefEvents;
    FOldOnFormClose: TCloseEvent;
    FActiveItem: TReferenceItem;
    function GetCount: Integer;
    function GetItems(Index: Integer): TReferenceItem;
    procedure SetItems(Index: Integer; const Value: TReferenceItem);
    procedure DoTimer(Sender: TObject);
    procedure DoCallBackProc(Sender: TObject; Event: TRefEvent; Params: Array of Variant);
    procedure DoOnFormClose(Sender: TObject; var Action: TCloseAction);
  protected
    function IndexOf(ClassForm: TSBaseFrmClass; const Params: Array of Variant): Integer;
    function AddItem(Form: TSBaseFrm; Params: Array of Variant): Integer;
    procedure DeleteItem(Index: Integer);
    procedure SetActiveItem(Index: Integer; CallBackProc: TRefEvents = nil; FieldNames: String = '');
    property ActiveItem: TReferenceItem read FActiveItem;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TReferenceItem read GetItems write SetItems; default;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  ReferenceList: TReferenceList;

// TODO: REMOVE !!! ONLY FOR DEBUG
{
procedure UpdateRefInfo;

  function GetParamInfo(const Params: Array of Variant): String;
  var
    i: Integer;
  begin
    Result := '[';
    for i := 0 to Pred(Length(Params)) do
    begin
      if Result <> '[' then Result := Result + ',';
      Result := Result + VarToStr(Params[i]);
    end;
    Result := Result + ']';
  end;

var
  i: Integer;
begin
  with MainForm.RefListBox do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      if Assigned(ReferenceList) then
      begin
        if Assigned(ReferenceList.ActiveItem) then
        with ReferenceList.ActiveItem.Form do
          Items.Add('Открыт справочник: ' + Caption + '(' + ClassName + ')' + ': ' + GetParamInfo(ReferenceList.ActiveItem.FParams));
        for i := 0 to Pred(ReferenceList.Count) do
        with ReferenceList[i], Form do
          Items.Add(Caption + '(' + ClassName + ') - остаток жизни: ' + IntToStr(LiveTime) + ' - до обновления: ' + IntToStr(RefreshTime) + ' - параметры: ' + GetParamInfo(FParams));
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;
}

function SelectRef(ClassForm: TSBaseFrmClass; const Params: Array of Variant; const DefaultValue: Variant; CallBackProc: TRefEvents; const FieldNames: String): Variant;
var
  AIndex: Integer;
  ARef: IReference;
  AForm: TSBaseFrm;
begin
  if not Supports(ClassForm, IReference) then
    raise Exception.CreateFmt(SErrorNoInterface, [ClassForm.ClassName]);
  if Assigned(ReferenceList) then
    AIndex := ReferenceList.IndexOf(ClassForm, Params)
  else
    begin
      ReferenceList := TReferenceList.Create;
      AIndex := -1;
    end;
  if AIndex < 0 then
    begin
      AForm  := ClassForm.CreateByParams(nil, []);
      AIndex := ReferenceList.AddItem(AForm, Params);
    end
  else
    AForm := ReferenceList[AIndex].FForm;
  ARef := AForm as IReference;
  ReferenceList.SetActiveItem(AIndex, CallBackProc, FieldNames);
  try
    if Assigned(CallBackProc) then
      ARef.ReferenceInit(Params, DefaultValue, ReferenceList.DoCallBackProc)
    else
      ARef.ReferenceInit(Params, DefaultValue, nil);
    if AForm.ShowModal = mrOk then
      Result := ARef.GetReferenceValue(FieldNames)
    else
      Result := Null;
  finally
    ReferenceList.SetActiveItem(-1);
  end;
end;

procedure ClearRefBuffer;
begin
  if Assigned(ReferenceList) then FreeAndNil(ReferenceList);
end;

{ TReferenceItem }

procedure TReferenceItem.AssignParams(const Params: array of Variant);
var
  i, ACount: Integer;
begin
  ACount := Length(Params);
  SetLength(FParams, ACount);
  for i := Pred(ACount) downto 0 do
    FParams[i] := Params[i];
end;

constructor TReferenceItem.Create(Form: TSBaseFrm; const Params: Array of Variant);
begin
  FLiveTime := ReferenceFormActualTime;
  FRefreshTime := ReferenceFormRefreshTime;
  FForm := Form;
  FClassForm := TSBaseFrmClass(Form.ClassType);
  AssignParams(Params);
end;

destructor TReferenceItem.Destroy;
begin
  FForm.Free;  
  inherited;
end;

function TReferenceItem.Equal(ClassForm: TSBaseFrmClass; const Params: array of Variant): Boolean;
var
  i, ACount: Integer;
begin
  Result := False;
  if ClassForm = FClassForm then
  begin
    ACount := ParamCount;
    if Length(Params) = ACount then
    begin
      for i := Pred(ACount) downto 0 do
        if Params[i] <> FParams[i] then exit;
      Result := True;
    end;
  end;
end;

function TReferenceItem.GetParamCount: Integer;
begin
  Result := Length(FParams);
end;

function TReferenceItem.GetParams(Index: Integer): Variant;
begin
  Result := FParams[Index];
end;

procedure TReferenceItem.SetParams(Index: Integer; const Value: Variant);
begin
  FParams[Index] := Value;
end;

{ TReferenceList }
constructor TReferenceList.Create;
begin
  FOldOnFormClose := nil;
  FList := TList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := DoTimer;
  FTimer.Interval := ReferenceRefreshTime * 1000;
  FTimer.Enabled := True;
end;

function TReferenceList.IndexOf(ClassForm: TSBaseFrmClass; const Params: array of Variant): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Pred(Count) downto 0 do
  if Items[i].Equal(ClassForm, Params) then
    begin
      Result := i;
      break;
    end;
end;

function TReferenceList.AddItem(Form: TSBaseFrm; Params: Array of Variant): Integer;
begin
  Result := FList.Add(TReferenceItem.Create(Form, Params));
end;

procedure TReferenceList.DeleteItem(Index: Integer);
begin
  Items[Index].Free;
  FList.Delete(Index);
end;

procedure TReferenceList.SetActiveItem(Index: Integer; CallBackProc: TRefEvents; FieldNames: String);
begin
  if Index < 0 then
    begin
      if Assigned(FActiveItem) then
        FActiveItem.FForm.OnClose := FOldOnFormClose;
      FActiveItem := nil;
      FCallBackProc := nil;
      FOldOnFormClose := nil;
      FFieldNames := '';
    end
  else
    begin
      FActiveItem := Items[Index];
      FOldOnFormClose := FActiveItem.FForm.OnClose;
      FActiveItem.FForm.OnClose := DoOnFormClose;
      FCallBackProc := CallBackProc;
      FFieldNames := FieldNames;
      FActiveItem.FLiveTime := ReferenceFormActualTime;
    end;
end;

destructor TReferenceList.Destroy;
var
  i: Integer;
begin
  FTimer.Free;
  for i := Pred(Count) downto 0 do DeleteItem(i);
  FList.Free;
  inherited;
end;

procedure TReferenceList.DoTimer(Sender: TObject);
var
  i: Integer;
  AItem: TReferenceItem;
begin
  if Count = 0 then
    ClearRefBuffer
  else
    begin
      for i := Pred(Count) downto 0 do
      begin
        AItem := Items[i];
        if AItem.FRefreshTime > 0 then
          Dec(AItem.FRefreshTime, ReferenceRefreshTime);
        if ActiveItem <> AItem then
        with AItem do
        begin
          Dec(FLiveTime, ReferenceRefreshTime);
          if (FLiveTime <= 0) or (FRefreshTime <= 0) then
           DeleteItem(i);
        end;
      end;
      // TODO: REMOVE !!! ONLY FOR DEBUG UpdateRefInfo;
    end;
end;

procedure TReferenceList.DoCallBackProc(Sender: TObject; Event: TRefEvent; Params: Array of Variant);

  function IsShiftCtrl: Boolean;
  var
   AKbrd: TKeyBoardState;
  begin
    GetKeyBoardState(AKbrd);
    Result := ((AKbrd[VK_SHIFT] and 128) <> 0) or ((AKbrd[VK_CONTROL] and 128) <> 0);
  end;

begin
  if Assigned(FActiveItem) then
  begin
    case Event of
      rfSelect: if Assigned(FCallBackProc) and IsShiftCtrl then
                  FCallBackProc(Sender, (ActiveItem.Form as IReference).GetReferenceValue(FFieldNames))
                else
                  FActiveItem.FForm.ModalResult := mrOk;
      rfRefresh: FActiveItem.FRefreshTime := ReferenceFormRefreshTime;
      rfParams: FActiveItem.AssignParams(Params);
    end;
  end;
end;

procedure TReferenceList.DoOnFormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(ActiveItem) and Assigned(FOldOnFormClose) then FOldOnFormClose(Sender, Action);
  Action := caHide;
end;

function TReferenceList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TReferenceList.GetItems(Index: Integer): TReferenceItem;
begin
  Result := FList.Items[Index];
end;

procedure TReferenceList.SetItems(Index: Integer; const Value: TReferenceItem);
begin
  FList.Items[Index] := Value;
end;

initialization
  ReferenceList := nil;

finalization
  ClearRefBuffer;

end.
