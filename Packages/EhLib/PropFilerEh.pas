{*******************************************************}
{                                                       }
{                      EhLib v5.2                       }
{          TPropWriter, TPropReader objects             }
{    Delphi / Kylix Cross-Platform Runtime Library      }
{                    (Build 5.2.00)                     }
{                                                       }
{   Copyright (c) 2002-2009 by Dmitry V. Bolshakov      }
{                                                       }
{*******************************************************}

//{$I EhLibClx.Inc}

unit PropFilerEh;
{$I config.inc}

interface

uses
  Windows, Forms, Controls, Types, SysUtils, Classes, TypInfo, Generics.Collections;

type
  TControlAlign = record
    SetOn         : TControl;
    DependedBy    : TControl;
    DependedAfter : Boolean;
  public
    constructor Create(ASetOn: TControl; ADependedBy: TControl; ADependedAfter: Boolean);
  end;

  TPropReaderEh = class(TReader)
  private
    procedure AddTopAlign(ASetOnControl: TControl; ADependedControl: TControl; ADependedAfter: Boolean);
    procedure AddTabOrderAlign(ASetOnControl: TWinControl; ADependedControl: TWinControl; ADependedAfter: Boolean);

    procedure AncestorNotFound(Reader: TReader; const ComponentName: string; ComponentClass: TPersistentClass; var Component: TComponent);
    procedure FindComponentInstance(Reader: TReader; const Name: string; var Instance: Pointer);

    procedure ReadTCollectionItemIndex(Reader: TReader);
    procedure ReadTCollectionAddItems(Reader: TReader);
    procedure ReadTCollectionLoadItems(Reader: TReader);

    procedure ReadTControlTopAfter(Reader: TReader);
    procedure ReadTControlTopBefore(Reader: TReader);

    procedure ReadTWinControlTabOrderAfter(Reader: TReader);
    procedure ReadTWinControlTabOrderBefore(Reader: TReader);
    procedure ReadTWinControlParent(Reader: TReader);

    procedure ReadTSynCompletionsItems(Reader: TReader);

    procedure ReadTStringsText(Reader: TReader);

    procedure ReadTdxBarItemShortCutText(Reader: TReader);
  protected
    function FindMethodInstance(Root: TComponent; const MethodName: string): TMethod; override;
    procedure DefinePersistentProperties(APersistent: TPersistent); override;
  public
    constructor Create(Stream: TStream; BufSize: Integer);
  end;

  procedure BeginGlobalLoadingEh;
  procedure NotifyGlobalLoadingEh{$IFDEF DEBUG}(ADebugInfo: TStrings = nil){$ENDIF};
  procedure EndGlobalLoadingEh;
  procedure FreeGlobalLoadingEh;


implementation

uses
  ConvUtils, RTLConsts, SBaseStringFunctions, SBaseConstants,
  SynCompletionProposal, SynUnicode, dxBar, Menus,
  SBaseUtils, SBaseFileUtils, Dialogs;

type
  TWinControlCrack = class(TWinControl);
  TComponentCrack = class(TComponent);

var
  FControlTopFixup: TList<TControlAlign>;
  FControlTabOrderFixup: TList<TControlAlign>;

procedure BeginGlobalLoadingEh;
begin
  FreeGlobalLoadingEh;
  BeginGlobalLoading;
end;

{$IFDEF DEBUG}
procedure NotifyGlobalLoading(ADebugInfo: TStrings = nil);
var
  I: Integer;
  G: TList<TComponent>;
  LTimeCounter: TTimeCounter;
begin
  G := Classes.GlobalLoaded;
  for I := 0 to G.Count - 1 do
  begin
    LTimeCounter.Start;
    TComponentCrack(G[I]).Loaded;
    if Assigned(ADebugInfo) then begin
      LTimeCounter.Stop;
      if LTimeCounter.IntervalMilliSeconds > 200 then
        ADebugInfo.Add('  >> ' + FormatDateTime(SDebugTime_Format, Now()) + ': ' + TComponent(G[I]).Name + '.Loaded: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
    end;
  end;
end;
{$ENDIF}

procedure NotifyGlobalLoadingEh{$IFDEF DEBUG}(ADebugInfo: TStrings = nil){$ENDIF};
var
  I: Integer;
begin
  NotifyGlobalLoading{$IFDEF DEBUG}(ADebugInfo){$ENDIF};

  if Assigned(FControlTopFixup) then begin
    for I := 0 to FControlTopFixup.Count - 1 do with FControlTopFixup.List[I] do begin
  {$IFDEF DEBUG}
      Assert(SetOn.Parent = DependedBy.Parent, 'Ќесовпадение при расстановки вертикального расположени€ контрола '#171 + SetOn.Name + #187': SetOn.Parent = '#171 + SetOn.Parent.Name + #187', DependedBy.Parent = '#171 + DependedBy.Parent.Name + #187);
  {$ENDIF}
      if DependedAfter then
        SetOn.Top := DependedBy.Top + DependedBy.Height
      else
        SetOn.Top := DependedBy.Top - 1;
    end;
  end;

  if Assigned(FControlTabOrderFixup) then begin
    for I := 0 to FControlTabOrderFixup.Count - 1 do with FControlTabOrderFixup[I] do begin
{$IFDEF DEBUG}
      Assert(SetOn.Parent = DependedBy.Parent, 'Ќесовпадение при расстановки пор€дка следовани€ контрола '#171 + SetOn.Name + #187': SetOn.Parent = '#171 + SetOn.Parent.Name + #187', DependedBy.Parent = '#171 + DependedBy.Parent.Name + #187);
{$ENDIF}
      if DependedAfter then
        TWinControl(SetOn).TabOrder := TWinControl(DependedBy).TabOrder + 1
      else
        TWinControl(SetOn).TabOrder := TWinControl(DependedBy).TabOrder
    end;
  end;
end;

procedure EndGlobalLoadingEh;
begin
  FreeGlobalLoadingEh;
  EndGlobalLoading;
end;

procedure FreeGlobalLoadingEh;
begin
  FreeAndNil(FControlTopFixup);
  FreeAndNil(FControlTabOrderFixup);
end;

{ TPropReaderEh }

constructor TPropReaderEh.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create(Stream, BufSize);
  OnFindComponentInstance := FindComponentInstance;
  OnAncestorNotFound := AncestorNotFound;
//  OnFindMethodInstance := FindMethodInstance;
end;

procedure TPropReaderEh.DefinePersistentProperties(APersistent: TPersistent);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Data := APersistent;

  if APersistent is TCollectionItem then begin
    LMethod.Code := @TPropReaderEh.ReadTCollectionItemIndex;
    DefineProperty(SConst_Index, TReaderProc(LMethod), nil, False);
  end else if APersistent is TCollection then begin
    LMethod.Code := @TPropReaderEh.ReadTCollectionAddItems;
    DefineProperty('Append', TReaderProc(LMethod), nil, False);
    LMethod.Code := @TPropReaderEh.ReadTCollectionLoadItems;
    DefineProperty('Load', TReaderProc(LMethod), nil, False);
  end else if APersistent is TControl then begin
    LMethod.Code := @TPropReaderEh.ReadTControlTopAfter;
    DefineProperty('TopAfter', TReaderProc(LMethod), nil, False);
    LMethod.Code := @TPropReaderEh.ReadTControlTopBefore;
    DefineProperty('TopBefore', TReaderProc(LMethod), nil, False);
    if APersistent is TWinControl then begin
      LMethod.Code := @TPropReaderEh.ReadTWinControlTabOrderAfter;
      DefineProperty('TabOrderAfter', TReaderProc(LMethod), nil, False);
      LMethod.Code := @TPropReaderEh.ReadTWinControlTabOrderBefore;
      DefineProperty('TabOrderBefore', TReaderProc(LMethod), nil, False);
      LMethod.Code := @TPropReaderEh.ReadTWinControlParent;
      DefineProperty('Parent', TReaderProc(LMethod), nil, False);
    end;
  end else if APersistent is TSynCompletionProposal then begin
    LMethod.Code := @TPropReaderEh.ReadTSynCompletionsItems;
    DefineProperty('LoadItems', TReaderProc(LMethod), nil, False);
  end else if APersistent is TStrings then begin
    LMethod.Code := @TPropReaderEh.ReadTStringsText;
    DefineProperty('Text', TReaderProc(LMethod), nil, False);
  end else if APersistent is TdxBarItem then begin
    LMethod.Code := @TPropReaderEh.ReadTdxBarItemShortCutText;
    DefineProperty('ShortCutText', TReaderProc(LMethod), nil, False);
  end
end;

procedure TPropReaderEh.AddTopAlign(ASetOnControl: TControl; ADependedControl: TControl; ADependedAfter: Boolean);
var
  I: Integer;
{$IFDEF DEBUG}
  LAdded: Boolean;
{$ENDIF}
begin
{$IFDEF DEBUG}
  LAdded := False;
{$ENDIF}
  if not Assigned(FControlTopFixup) then
    FControlTopFixup := TList<TControlAlign>.Create
  else begin
    for I := 0 to FControlTopFixup.Count - 1 do with FControlTopFixup.List[I] do
{$IFDEF DEBUG}
      if SetOn = ASetOnControl then
        Raise Exception.Create('ѕовторное определение вертикального расположени€ контрола '#171 + ASetOnControl.Name + #187)
      else if not LAdded then begin
{$ENDIF}
        if (DependedBy = ADependedControl) and (DependedAfter = ADependedAfter) then begin
          DependedBy        := ASetOnControl;
          FControlTopFixup.Insert(I, TControlAlign.Create(ASetOnControl, ADependedControl, ADependedAfter));
{$IFDEF DEBUG}
          LAdded := True;
{$ELSE}
          Exit;
{$ENDIF}
        end else if (SetOn = ADependedControl) and (DependedAfter <> ADependedAfter) then begin
          ADependedAfter    := DependedAfter;
          ADependedControl  := DependedBy;
          DependedBy        := ASetOnControl;
          FControlTopFixup.Insert(I, TControlAlign.Create(ASetOnControl, ADependedControl, ADependedAfter));
{$IFDEF DEBUG}
          LAdded := True;
{$ELSE}
          Exit;
{$ENDIF}
        end
{$IFDEF DEBUG}
        else
          Assert(DependedBy <> ASetOnControl)
      end;
{$ENDIF};
  end;

{$IFDEF DEBUG}
  if not LAdded then
{$ENDIF}
  FControlTopFixup.Add(TControlAlign.Create(ASetOnControl, ADependedControl, ADependedAfter));
end;

procedure TPropReaderEh.AddTabOrderAlign(ASetOnControl: TWinControl; ADependedControl: TWinControl; ADependedAfter: Boolean);
var
  I: Integer;
{$IFDEF DEBUG}
  LAdded: Boolean;
{$ENDIF}
begin
  if not Assigned(FControlTabOrderFixup) then
    FControlTabOrderFixup := TList<TControlAlign>.Create
  else begin
{$IFDEF DEBUG}
    LAdded := False;
{$ENDIF}
    for I := 0 to FControlTabOrderFixup.Count - 1 do with FControlTabOrderFixup.List[I] do
{$IFDEF DEBUG}
      if SetOn = ASetOnControl then
        Raise Exception.Create('ѕовторное определение пор€дка следовани€ контрола '#171 + ASetOnControl.Name + #187)
      else if not LAdded then begin
{$ENDIF}
        if (DependedBy = ADependedControl) and (DependedAfter = ADependedAfter) then begin
          DependedBy := ASetOnControl;
          FControlTabOrderFixup.Insert(I, TControlAlign.Create(ASetOnControl, ADependedControl, ADependedAfter));
{$IFDEF DEBUG}
          LAdded := True;
{$ELSE}
          Exit;
{$ENDIF}
        end else if (SetOn = ADependedControl) and (DependedAfter <> ADependedAfter) then begin
          ADependedAfter := DependedAfter;
          ADependedControl := TWinControl(DependedBy);
          DependedBy := ASetOnControl;
          FControlTabOrderFixup.Insert(I, TControlAlign.Create(ASetOnControl, ADependedControl, ADependedAfter));
{$IFDEF DEBUG}
          LAdded := True;
{$ELSE}
          Exit;
{$ENDIF}
        end
{$IFDEF DEBUG}
        else
          Assert(DependedBy <> ASetOnControl)
      end;
{$ENDIF};
  end;

  FControlTabOrderFixup.Add(TControlAlign.Create(ASetOnControl, ADependedControl, ADependedAfter));
end;

procedure TPropReaderEh.AncestorNotFound(Reader: TReader; const ComponentName: string; ComponentClass: TPersistentClass; var Component: TComponent);
begin
  if SameText(ComponentName, 'Self') then
    Component := Owner {Parent - Ќе работало внутри компонент}
  else if SameText(ComponentName, 'MainForm') then
    Component := Application.MainForm
  else if SameText(ComponentName, 'Application') then
    Component := Application
  else begin
    Component := Root.FindComponent(ComponentName);
    if Component = nil then begin
      Component := Application.FindComponent(ComponentName);
      if Component = nil then
        Component := TComponent(GetObjectProp(Root, ComponentName));
    end;
  end;
end;

procedure TPropReaderEh.FindComponentInstance(Reader: TReader; const Name: string; var Instance: Pointer);
var
  LItem: TStringItem;
  LObject, LSubObject: TObject;
  LComponent: TComponent absolute LObject;
begin
  LObject := nil;
  LSubObject := nil;

  for LItem in EnumStringItems(Name, '.') do begin
    if LItem.Index = 0 then begin
      AncestorNotFound(Reader, LItem.Value, nil, LComponent);
    end else begin
      if (LObject is TComponent) then
        LSubObject := LComponent.FindComponent(LItem.Value);

      if (not (LObject is TComponent)) or (LSubObject = nil) then
          LSubObject := GetObjectProp(LObject, LItem.Value);

      LObject := LSubObject;
    end;

    if not Assigned(LObject) then Exit;
  end;

  Instance := LObject;
end;

function TPropReaderEh.FindMethodInstance(Root: TComponent; const MethodName: string): TMethod;
var
  LItem: TStringItem;
  LComponent, LSubComponent: TComponent;
begin
  LComponent := Root;
  for LItem in EnumStringItems(MethodName, '.') do begin
    if LItem.Eof then begin
      Result.Code := FindMethod(LComponent, LItem.Value);
    end else begin
      if LItem.Index = 0 then
        AncestorNotFound(Self, LItem.Value, nil, LComponent)
      else begin
        LSubComponent := LComponent.FindComponent(LItem.Value);
        if LSubComponent = nil then
            LSubComponent := TComponent(GetObjectProp(LComponent, LItem.Value));

        LComponent := LSubComponent;
      end;

      if not Assigned(LComponent) then Break;
    end;
  end;

//  if Result.Code <> nil then
  Result.Data := LComponent;
end;

procedure TPropReaderEh.ReadTCollectionItemIndex(Reader: TReader);
begin
  TCollectionItem(Self).Index := Reader.ReadInteger;
end;

procedure TPropReaderEh.ReadTCollectionLoadItems(Reader: TReader);
var
  LCollection: TCollection absolute Self;
  LInstance: Pointer;
  LLoadCollection: TCollection;
begin
  TPropReaderEh(Reader).FindComponentInstance(Reader, Reader.ReadIdent, LInstance);
  LLoadCollection := TCollection(GetObjectProp(TComponent(LInstance), SConst_Items));
  LCollection.Assign(LLoadCollection);
end;

procedure TPropReaderEh.ReadTWinControlTabOrderAfter(Reader: TReader);
var
  LInstance: Pointer;
begin
  TPropReaderEh(Reader).FindComponentInstance(Reader, Reader.ReadIdent, LInstance);
  TPropReaderEh(Reader).AddTabOrderAlign(TWinControl(Self), TWinControl(LInstance), True);
end;

procedure TPropReaderEh.ReadTWinControlTabOrderBefore(Reader: TReader);
var
  LInstance: Pointer;
begin
  TPropReaderEh(Reader).FindComponentInstance(Reader, Reader.ReadIdent, LInstance);
  TPropReaderEh(Reader).AddTabOrderAlign(TWinControl(Self), TWinControl(LInstance), False);
end;

procedure TPropReaderEh.ReadTWinControlParent(Reader: TReader);
var
  LInstance: Pointer;
begin
  TPropReaderEh(Reader).FindComponentInstance(Reader, Reader.ReadIdent, LInstance);
  TWinControl(Self).Parent := TWinControl(LInstance);
end;

procedure TPropReaderEh.ReadTControlTopAfter(Reader: TReader);
var
  LInstance: Pointer;
begin
  TPropReaderEh(Reader).FindComponentInstance(Reader, Reader.ReadIdent, LInstance);
  TPropReaderEh(Reader).AddTopAlign(TControl(Self), TControl(LInstance), True);
end;

procedure TPropReaderEh.ReadTControlTopBefore(Reader: TReader);
var
  LInstance: Pointer;
begin
  TPropReaderEh(Reader).FindComponentInstance(Reader, Reader.ReadIdent, LInstance);
  TPropReaderEh(Reader).AddTopAlign(TControl(Self), TControl(LInstance), False);

{$IFDEF DEBUG}
  Assert(TControl(Self).Parent = TControl(LInstance).Parent);
{$ENDIF}
  TControl(Self).Top := TControl(LInstance).Top - 1
end;

procedure TPropReaderEh.ReadTdxBarItemShortCutText(Reader: TReader);
begin
  TdxBarItem(Self).ShortCut := TextToShortCut(TPropReaderEh(Reader).ReadString);
end;

procedure TPropReaderEh.ReadTStringsText(Reader: TReader);
begin
  TStrings(Self).Text := TPropReaderEh(Reader).ReadString;
end;

procedure TPropReaderEh.ReadTSynCompletionsItems(Reader: TReader);
var
  LSynCompletionProposal: TSynCompletionProposal absolute Self;
  LInstance: Pointer;
  LSynCompletionProposalItems: TUnicodeStrings absolute LInstance;
  I: Integer;
begin
  TPropReaderEh(Reader).FindComponentInstance(Reader, Reader.ReadIdent, LInstance);
  try
    LSynCompletionProposal.InsertList.BeginUpdate;
    try
      LSynCompletionProposal.ItemList.BeginUpdate;

      LSynCompletionProposal.InsertList.Clear;
      LSynCompletionProposal.ItemList.Clear;

      for I := 0 to LSynCompletionProposalItems.Count - 1 do begin
        LSynCompletionProposal.InsertList.Add(LSynCompletionProposalItems.Names[I]);
        LSynCompletionProposal.ItemList.Add(LSynCompletionProposalItems.ValueFromIndex[I]);
      end;
    finally
      LSynCompletionProposal.ItemList.EndUpdate;
    end;
  finally
    LSynCompletionProposal.InsertList.EndUpdate;
  end;
end;

procedure TPropReaderEh.ReadTCollectionAddItems(Reader: TReader);
var
  LItem: TPersistent;
  ACollection: TCollection absolute Self;
begin
  Reader.CheckValue(vaCollection);
  ACollection.BeginUpdate;
  try
    while not Reader.EndOfList do begin
      if Reader.NextValue in [vaInt8, vaInt16, vaInt32] then Reader.ReadInteger;
      LItem := ACollection.Add;
      Reader.ReadListBegin;
      while not Reader.EndOfList do TPropReaderEh(Reader).ReadProperty(LItem);
      Reader.ReadListEnd;
    end;
    Reader.ReadListEnd;
  finally
    ACollection.EndUpdate;
  end;
end;

{ TControlAlign }

constructor TControlAlign.Create(ASetOn: TControl; ADependedBy: TControl; ADependedAfter: Boolean);
begin
  SetOn         := ASetOn;
  DependedBy    := ADependedBy;
  DependedAfter := ADependedAfter;
end;

initialization

finalization
  FreeGlobalLoadingEh;

end.
