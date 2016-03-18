{******************************************************************************}
{                       -= Модуль контроля задач =-                            }
{                                                                              }
{                              v1.1 (С) 2012
{                                                                              }
{                                 -=MD=-                                      }
{                                                                              }
{******************************************************************************}
unit SBaseTaskWindow;
{$I config.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, pngimage, ExtCtrls, NamedVariables, DB, SBaseForm,
  dxCustomHint, dxScreenTip, SBaseDataModule, dxGDIPlusClasses, DateUtils,
  dxBar, System.Actions, ActnList, cxClasses, dxMDBar
{$IFDEF DELPHIXE3}
  , System.UITypes
{$ENDIF}
;

type
  ETaskError = class(Exception);
  TTaskItemNotify = procedure(Sender: TObject; TaskIndex: Integer) of object;

  TTask = packed record
    Id              : String;
    Parent_Id       : String;
    DefaultCaption  : String;
    DefaultInfo     : String;
    SQL             : String;
    ClassName       : String;
    Interval        : Integer;

    Caption         : String;
    Info            : String;
    Params          : TNamedVariants;

    ObjLabel    : TLabel;
    ObjInfo     : TLabel;

    Visible     : Boolean;
    New         : Boolean;
    RefreshTime : TDateTime;
  end;
  TTaskList = Array of TTask;
  PTaskList = ^TTaskList;

  TSBaseTaskWnd = class(TSBaseFrm)
    procedure TimerTimer(Sender: TObject);
    procedure LabelCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction); override;
  private
    FInternalBusy: Boolean;
    FFullRefreshInterval: Integer;
    FLebelLeft, FLebelTop, FLebelLeftOffset, FLabelTopOffset: Integer;

    function GetActiveTaskCount: Integer;
    function GetNewTaskCount: Integer;
    function IntervalNOD(const ATaskList: TTaskList): Integer;
    procedure UpdateBounds(var ABounds: TRect);
  protected
    FMenuButton: TdxBarItem;
    FTaskList   : TTaskList;
    FOpenIfPosible: Boolean;
    FLinkLabels : Array of TLabel;
    FRefreshTime: TDateTime;

    procedure OnButtonClick(Sender: TObject);
    procedure WMNCHitTest (var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMMoving(var Message: TWMMoving); message WM_MOVING;
  protected
    procedure RefreshLabels;
    function  GetTaskIndexById(ATaskId: String; ATaskList: PTaskList = nil): Integer;
    procedure RefreshTasks(var ATaskList: TTaskList);
    procedure RefreshActives(var ATaskList: TTaskList);
    function  AssignTasks(var ATaskList: TTaskList): Boolean;
  protected
    procedure InternalCreate; override;
    procedure InternalClose(AFinalization: Boolean); override;
  public
   procedure Show; override;
   property ActiveTaskCount: Integer read GetActiveTaskCount;
   property NewTaskCount: Integer read GetNewTaskCount;
  published
    Timer     : TTimer;
    DataSetList, DataSetTask: TSQLDataSetProvider;
    LabelTitle: TLabel;
    LabelClose: TLabel;

    property FullRefreshInterval: Integer read FFullRefreshInterval write FFullRefreshInterval default 600; //Seconds

    property LebelLeft: Integer read FLebelLeft write FLebelLeft default 30;
    property LebelTop: Integer read FLebelTop write FLebelTop default 25;
    property LebelLeftOffset: Integer read FLebelLeftOffset write FLebelLeftOffset default 15;
    property LabelTopOffset: Integer read FLabelTopOffset write FLabelTopOffset default 10;
  end;

implementation

{$R *.dfm}

uses
  Types, SBaseVariantFunctions, SBaseConstants, SBaseStringFunctions, SBaseCxUtils,
  SBaseUtils, SBaseFormDefinitions;

resourcestring
  STaskPrefix_Caption = '#.Caption';
  STaskPrefix_Info    = '#.Info';

{ TSBaseTaskWnd }

procedure TSBaseTaskWnd.InternalClose(AFinalization: Boolean);
begin
  inherited;

  if Assigned(DataSetList) then
    DataSetList.Thread.Stop;

  if Assigned(DataSetTask) then
    DataSetTask.Thread.Stop;

  FInternalBusy := True;
end;

procedure TSBaseTaskWnd.InternalCreate;
begin
  FOpenIfPosible := False;
  FRefreshTime := 0;
  FFullRefreshInterval := 600;

  FLebelLeft        := 30;
  FLebelTop         := 25;
  FLebelLeftOffset  := 15;
  FLabelTopOffset   := 10;

  inherited;

  if Owner is TWinControl then
    Parent := TWinControl(Owner);

  FMenuButton := VarToPointer(CreateFormParams[SConst_Sender]);
end;

function TSBaseTaskWnd.GetActiveTaskCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Pred(Length(FTaskList)) downto 0 do
    if FTaskList[I].Visible then
      Inc(Result);
end;

function TSBaseTaskWnd.GetNewTaskCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Pred(Length(FTaskList)) downto 0 do
    with FTaskList[i] do
      if Visible and New then
        Inc(Result);
end;

function TSBaseTaskWnd.IntervalNOD(const ATaskList: TTaskList): Integer;

  function NODXY(x, y: Integer): Integer;
  var
    n, m, p: Integer;
  begin
    n := x; m := y; p := n mod m;
    while p <> 0 do
    begin
      n := m; m := p; p := n mod m;
    end;
    Result := m;
  end;

var
  I: Integer;
begin
  if Length(ATaskList) = 0 then
    Result := 1
  else begin
    Result := ATaskList[0].Interval;
    for I := Pred(Length(ATaskList)) downto 1 do
      if ATaskList[I].Interval > 0 then
        Result := NODXY(Result, ATaskList[I].Interval);
  end;
end;

procedure TSBaseTaskWnd.RefreshTasks(var ATaskList: TTaskList);
var
  I, LIndex: Integer;
  LId: String;
begin
  LIndex := 0;

  SetLength(ATaskList, 0);
  if DataSetList.Active or DataSetList.Thread.Active then begin
    DataSetList.Thread.Stop;
    DataSetList.Close;
    DataSetList.UnPrepare;
  end;

  repeat
    DataSetList.Thread.OpenNext;
    DataSetList.Thread.Wait;
    if not DataSetList.Active then Exit;

//    LCount := FDataSet.RecordCount;
//    SetLength(ATaskList, LIndex + LCount);

    while not DataSetList.Eof do begin
      LId := DataSetList.FieldByName(SConst_Id).AsString;
      if GetTaskIndexById(LId, @ATaskList) = -1 then begin
        SetLength(ATaskList, Succ(LIndex));
        with ATaskList[LIndex] do begin
          Id              := LId;
          Parent_Id       := VarToStr(DataSetList.Fields.FindFieldValue(SConst_Parent_Id));
          DefaultCaption  := DataSetList.FieldByName(SConst_Caption).AsString;
          DefaultInfo     := VarToStr(DataSetList.Fields.FindFieldValue(SConst_Info));
          SQL             := VarToStr(DataSetList.Fields.FindFieldValue(SConst_SQL_Text));
          ClassName       := VarToStr(DataSetList.Fields.FindFieldValue(SConst_ClassName));
          Interval        := VarToIntDef(DataSetList.Fields.FindFieldValue(SConst_Interval), 0);

          I := GetTaskIndexById(Id);
          if I >= 0 then begin
            RefreshTime := FTaskList[I].RefreshTime;
            Visible     := FTaskList[I].Visible;
            Info        := FTaskList[I].Info;
            Caption     := FTaskList[I].Caption;
            Params      := FTaskList[I].Params;
          end else begin
            RefreshTime := 0;
            Visible     := False;
            if SQL.IsEmpty then begin
              Caption     := DefaultCaption;
              Info        := DefaultInfo;
            end;
            Params.Clear;
          end;
          New := True;
        end;
        Inc(LIndex);
      end;
      DataSetList.Next;
    end;
  until False;

//  SetLength(ATaskList, LIndex);
end;

function TSBaseTaskWnd.GetTaskIndexById(ATaskId: String; ATaskList: PTaskList): Integer;
var
  I: Integer;
begin
  Result := -1;
  if ATaskList = nil then
    ATaskList := @FTaskList;

  for I := Pred(Length(ATaskList^)) downto 0 do
    if ATaskList^[I].Id = ATaskId then begin
      Result := I;
      Break;
    end;
end;

function TSBaseTaskWnd.AssignTasks(var ATaskList: TTaskList): Boolean;
var
  I, LIndex: Integer;
begin
  Result := False;
  if (Length(ATaskList) <> Length(FTaskList)) then
    Result := True
  else
    for I := Pred(Length(ATaskList)) downto 0 do
      if (ATaskList[I].Id           <> FTaskList[I].Id)
        or (ATaskList[I].Parent_Id  <> FTaskList[I].Parent_Id)
        or (ATaskList[I].Caption    <> FTaskList[I].Caption)
        or (ATaskList[I].Info       <> FTaskList[I].Info)
        or (ATaskList[I].Visible    <> FTaskList[I].Visible)
      then begin
        Result := True;
        Break;
      end;

  if Result then begin
    for I := Pred(Length(ATaskList)) downto 0 do
      with ATaskList[I] do begin
        LIndex := GetTaskIndexById(ATaskList[I].Id);
        if LIndex >= 0 then
          New := FTaskList[LIndex].New or (Visible and (not FTaskList[LIndex].Visible))
        else
          New := True;
      end;

    SetLength(FTaskList, 0);
    FTaskList := ATaskList;
  end else begin
    for I := Pred(Length(FTaskList)) downto 0 do
      with FTaskList[I] do begin
        Info        := ATaskList[I].Info;
        Params      := ATaskList[I].Params;
        ClassName   := ATaskList[I].ClassName;
        RefreshTime := ATaskList[I].RefreshTime;
      end;
    //SetLength(ATaskList, 0);
  end;

  if Length(FTaskList) = 0 then
    Timer.Enabled := False
  else
    Timer.Interval := IntervalNOD(FTaskList) * 1000;
end;

procedure TSBaseTaskWnd.RefreshLabels;
var
  LTop, LCount, LIndex: Integer;

  function AddLabel(AIndex: Integer; ALeftOffset: Integer): TLabel;
  begin
    Result := TLabel.Create(Self);
    with Result do begin
      Tag         := AIndex;
      Parent      := Self;
//      AutoSize    := True;
      Font.Color  := clHighlight;
//      Font.Name   := 'Tahoma';
      Left        := LebelLeft + ALeftOffset;
      Top         := LTop;
      Caption     := FTaskList[AIndex].Caption;
      Hint        := FTaskList[AIndex].Info;
      ShowHint    := (Hint <> '');

      if FTaskList[AIndex].New or (FTaskList[AIndex].SQL.IsEmpty) then
        Font.Style := Font.Style + [fsBold];

      if Length(FTaskList[AIndex].ClassName) > 0 then begin
        Font.Style := Font.Style + [fsUnderline];
        Cursor  := crHandPoint;
        OnClick := OnButtonClick;
      end;
      Inc(LTop, Height);
    end;

  if FTaskList[AIndex].Info <> '' then begin
    FTaskList[AIndex].ObjInfo := TLabel.Create(Result);
    with FTaskList[AIndex].ObjInfo do begin
      Parent      := Self;
      AutoSize    := True;
      Font.Color  := clWindowText;
      Font.Style  := [];
      Hint        := '';
      ShowHint    := False;
      Font.Name   := 'Tahoma';
      Left        := LebelLeft + ALeftOffset;
      Top         := LTop;
      Inc(LTop, Height);
    end;
  end;
//  Inc(LTop, LabelSpaces);
end;

  procedure CreateLabels(AParent_Id: String; ALeftOffset: Integer = 0);
  var
    K: Integer;
  begin
    for K := 0 to Pred(LCount) do
      with FTaskList[K] do
        if (not Visible) then
          ObjLabel := nil
        else if (Parent_Id = AParent_Id) then begin
          FLinkLabels[LIndex] := AddLabel(K, ALeftOffset);
          ObjLabel := FLinkLabels[LIndex];
          with FLinkLabels[LIndex] do begin
            if BoundsRect.Right + LebelLeft > Self.Width then
              Self.Width := BoundsRect.Right + LebelLeft;
          end;
          Inc(LIndex);
          CreateLabels(Id, ALeftOffset + LebelLeftOffset);
          if AParent_Id = '' then
            Inc(LTop, LabelTopOffset);
        end
  end;

  procedure UpdatePos;
  var
    LRect, LOldRect: TRect;
  begin
    LRect := BoundsRect;
    LOldRect := LRect;
    UpdateBounds(LRect);
    if not EqualRect(LOldRect, LRect) then begin
      BoundsRect := LRect;
      Application.ProcessMessages;
      LRect := BoundsRect;
      UpdateBounds(LRect);
      BoundsRect := LRect;
    end;
  end;

var
  I: Integer;
begin
  LCount := Length(FTaskList);
  LIndex := 0;

  for I := Pred(Length(FLinkLabels)) downto 0 do begin
    if FLinkLabels[I].Tag < LCount then begin
      FTaskList[FLinkLabels[I].Tag].ObjLabel := nil;
      FTaskList[FLinkLabels[I].Tag].ObjInfo := nil;
    end;
    FLinkLabels[I].Free;
  end;

  SetLength(FLinkLabels, ActiveTaskCount);

  LTop := LabelTitle.BoundsRect.Bottom + LabelTopOffset;
  CreateLabels('');

  for I := 0 to Pred(Length(FLinkLabels)) do
    if FLinkLabels[I].ComponentCount > 0 then
      with TLabel(FLinkLabels[I].Components[0]) do begin
        Caption  := GetTextEndEllipsis(FTaskList[FLinkLabels[i].Tag].Info, Self.Canvas, Self.Width - (2 * LebelLeft));
      end;

  while Height < LTop + LebelTop do begin
    Height := Height + 1;
    UpdatePos;
  end;

  while Height > LTop + LebelTop do begin
    Height := Height - 1;
    UpdatePos;
  end;

  UpdatePos;
  UpdatePos;
  UpdatePos;
end;

procedure TSBaseTaskWnd.RefreshActives(var ATaskList: TTaskList);
var
  LParams: TNamedVariants;
  I, J: Integer;
  LParent_Id: String;
begin
  LParams.UnknownAsNull := True;

  if DataSetTask.Active or DataSetTask.Thread.Active then begin
    DataSetTask.Thread.Stop;
    DataSetTask.Close;
    DataSetTask.UnPrepare;
  end;

  try
    for I := Pred(Length(ATaskList)) downto 0 do
      with ATaskList[I] do
        if RefreshTime <= Now then begin
          Visible := False;

          if (not SQL.IsEmpty) then begin
            DataSetTask.Close;
            DataSetTask.UnPrepare;
            DataSetTask.SQL.Text := SQL;

            LParams.Clear;
            try
              while True do begin
                DataSetTask.Thread.OpenNext;
                DataSetTask.Thread.Wait;
                if not DataSetTask.Active then
                  Break
                else if (not DataSetTask.IsEmpty) then begin
                  LParams.AssignValues(DataSetTask.Fields, amOverride);
                  Visible := True;
                end;
              end;
            finally
              DataSetTask.Close;
              RefreshTime := IncSecond(Now, Interval);
            end;

            if Visible then begin
              Params := LParams;

              Caption := IsEmptyString(VarToStr(LParams[STaskPrefix_Caption]), DefaultCaption);
              Info    := IsEmptyString(VarToStr(LParams[STaskPrefix_Info]), DefaultInfo);
            end;
          end;
        end;
  finally
    LParams.Clear;
  end;

  for I := Pred(Length(ATaskList)) downto 0 do begin
    with ATaskList[I] do begin
      if (SQL.IsEmpty) or (not Visible) then
        Continue;
      LParent_Id := Parent_Id;
    end;

    if Length(LParent_Id) > 0 then begin
      repeat
        J := GetTaskIndexById(LParent_Id, @ATaskList);
        if J = -1 then begin
          ATaskList[I].Parent_Id := '';
          Break;
        end else
          with ATaskList[J] do begin
            if ATaskList[I].New then
              New := True;
            if Visible then
              Break;
            Visible := True;
            LParent_Id := Parent_Id;
          end;
      until (Length(LParent_Id) = 0);
    end;
  end;
end;

procedure TSBaseTaskWnd.Show;
begin
  FOpenIfPosible := True;
  if FRefreshTime = 0 then
    Timer.Enabled := True
  else
    TimerTimer(nil)
end;

procedure TSBaseTaskWnd.TimerTimer(Sender: TObject);
var
  LTaskList: TTaskList;
  LRect, LRectMax: Trect;
  LChanged: Boolean;
  LRefreshed: Boolean;
begin
  if FInternalBusy then Exit;
  Timer.Enabled := False;
  FInternalBusy := True;
  try
    LRefreshed := (FOpenIfPosible) or (FRefreshTime < Now);
    if LRefreshed then
      RefreshTasks(LTaskList)
    else
      LTaskList := FTaskList;

    if Length(LTaskList) > 0 then
      RefreshActives(LTaskList);
    LChanged := AssignTasks(LTaskList);

    if LRefreshed then
      FRefreshTime := IncSecond(Now, FullRefreshInterval);

    if Assigned(FMenuButton) then
      FMenuButton.Enabled := (Length(FTaskList) > 0);

    if (not Visible) and (ActiveTaskCount > 0) and (FOpenIfPosible or (NewTaskCount > 0)) then begin
      RefreshLabels;
      Windows.GetClientRect(Application.MainForm.ClientHandle, LRectMax);
      LRect := BoundsRect;
      OffsetRect(LRect, LRectMax.Right - LRect.Left, LRectMax.Bottom - LRect.Top);
      UpdateBounds(LRect);
      BoundsRect := LRect;
      AnimateWindow(Handle, 500, AW_SLIDE or AW_VER_NEGATIVE);
      inherited Show;
      FOpenIfPosible := False;
    end else if Visible and (ActiveTaskCount = 0) then
      Close
    else if LChanged then // !!! COMMENT LINE FOR DEBUG !!!
      RefreshLabels;
  finally
    FInternalBusy := False;
    Timer.Enabled   := True;
  end;
end;

procedure TSBaseTaskWnd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TSBaseTaskWnd.LabelCloseClick(Sender: TObject);
var
  I: Integer;
begin
  Close;

  for I := Pred(Length(FTaskList)) downto 0 do
    with FTaskList[I] do begin
      New := False;
      if (not SQL.IsEmpty) and Assigned(ObjLabel) then
        with TLabel(ObjLabel) do
          Font.Style := Font.Style - [fsBold];
    end;
end;

procedure TSBaseTaskWnd.WMNCHitTest(var Message: TWMNCHitTest);

  function PTInControl(AControl: TControl; P: TPoint): Boolean;
  begin
    Result := PTInRect(AControl.BoundsRect, P);
  end;

var
  i: Integer;
  P: TPoint;
  AisLink: Boolean;
begin
  P := ScreenToClient(Point(Message.XPos, Message.YPos));
  AisLink := False;
  for i := Pred(Length(FLinkLabels)) downto 0 do
  if PTInControl(FLinkLabels[i], P) or ((FLinkLabels[i].ComponentCount > 0) and PTInControl(TControl(FLinkLabels[i].Components[0]), P)) then
  begin
    AisLink := True;
    break;
  end;

  if AisLink or PTInControl(LabelClose, P) then
    inherited
  else
    Message.Result := HTCAPTION;
end;

procedure TSBaseTaskWnd.UpdateBounds(var ABounds: TRect);
var
  ARect: TRect;
begin
  Windows.GetClientRect(Application.MainForm.ClientHandle, ARect);
  OffsetRect(ARect, 0, 50);
  InflateRect(ARect, -10, -10);
  if ABounds.Left < ARect.Left then
     OffsetRect(ABounds, ARect.Left - ABounds.Left, 0);
  if ABounds.Top < ARect.Top then
    OffsetRect(ABounds, 0, ARect.Top - ABounds.Top);
  if ABounds.Right > ARect.right then
    OffsetRect(ABounds, ARect.Right - ABounds.Right, 0);
  if ABounds.Bottom > ARect.Bottom then
    OffsetRect(ABounds, 0, ARect.Bottom - ABounds.Bottom);
end;

procedure TSBaseTaskWnd.WMMoving(var Message: TWMMoving);
var
  ARect: TRect;
begin
  ARect := Message.DragRect^;
  ARect.TopLeft := Parent.ScreenToClient(ARect.TopLeft);
  ARect.BottomRight := Parent.ScreenToClient(ARect.BottomRight);
  UpdateBounds(ARect);
  ARect.TopLeft := Parent.ClientToScreen(ARect.TopLeft);
  ARect.BottomRight := Parent.ClientToScreen(ARect.BottomRight);
  Message.DragRect^ := ARect;
end;

procedure TSBaseTaskWnd.OnButtonClick(Sender: TObject);
var
  LComponent: TComponent;
  LParams: TNamedVariants;
begin
  with FTaskList[TLabel(Sender).Tag] do begin
    LParams := TNamedVariants.Create(Params, True);
    LParams[SConst_ClassName] := ClassName;

    LComponent := SBaseFormDefinitions.ICreateByParams(TSBaseFrm.ClassName, Owner, LParams);
    if (LComponent <> nil) then
      (LComponent as ISBaseInterface).IShow;
  end;
end;

initialization
  RegisterClasses([TSBaseTaskWnd]);

finalization
  UnRegisterClasses([TSBaseTaskWnd]);

end.
