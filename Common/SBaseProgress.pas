{******************************************************************************}
{                                                                              }
{                     -= Форма с индикатором процесса =-                       }
{                                                                              }
{                               v4.1 (С) 2012                                  }
{                                                                              }
{                                                                              }
{******************************************************************************}
unit SBaseProgress;
{$I config.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, cxControls, cxContainer, cxEdit,
  cxLabel, NamedVariables

{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, fs_ievents
{$ENDIF}
  ;

type
  TSBaseProgressFrm = class(TForm)
    Timer       : TTimer;

    PanelMain   : TPanel;
    LabelTitle  : TLabel;
    Cancel      : TButton;
    PanelCancel: TPanel;

    procedure CancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FProgressBar: TComponent;
    FActiveWindow: HWnd;
    FWindowList: Pointer;
    FTerminated: Boolean;
    FControls: Array of TControl;
    FOnCancelButtonClick: TNotifyEvent;
    FParentDisabled: Boolean;
    FText: String;
    FLastUpdateTextTime: TDateTime;

    function  GetProgressCount: Integer;
    procedure SetProgressCount(const AValue: Integer);
    function  GetProgressPos: Integer;
    procedure SetProgressPos(const Value: Integer);
    procedure SetText(const Value: String);
//    procedure SetVisible(const Value: Boolean);
  protected
    FShow: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Show(AParent: TWinControl; ATimer: Boolean = True); reintroduce;
    procedure Close;
    procedure UpdatePos;

  published
//    property Visible: Boolean write SetVisible;
    property Text: String read FText write SetText;
    property ProgressPos: Integer read GetProgressPos write SetProgressPos;
    property ProgressCount: Integer read GetProgressCount write SetProgressCount;
    property ParentDisabled: Boolean read FParentDisabled write FParentDisabled;

    property OnCancelButtonClick: TNotifyEvent read FOnCancelButtonClick write FOnCancelButtonClick;
  end;

{$IFDEF FASTSCRIPT_RTTI}
  TFunctions_TSBaseProgressFrm = class(TfsRTTIModule)
  private
    function Call_TSBaseProgressFrm_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

implementation

{$R *.dfm}

uses
  DateUtils, cxProgressBar, SBaseVariantFunctions, SBaseConstants, SBaseUtils;

resourcestring
  SReportProgressCaption = 'Операция выполняется ...';

const
  ProgressSize = 40;

type
  TSBaseProgressBar = class(TcxProgressBar)
  private
    FPos: Integer;
    FMin: Integer;
    FMax: Integer;
    FSize: Integer;
    FActive: Boolean;
    FTime: TTime;
    FTimer: TTimer;
    procedure SetPos(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetActive(const Value: Boolean);
  protected
    procedure DoTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Pos: Integer read FPos write SetPos;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Active: Boolean read FActive write SetActive;
  end;

{ TSBaseProgressBar }

constructor TSBaseProgressBar.Create(AOwner: TComponent);
begin
  inherited;

  FPos := 0;
  FMin := 0;
  FMax := 100;
  FTime := Now;
  FSize := ProgressSize;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 30;
  FTimer.Enabled  := False;
  FTimer.OnTimer  := DoTimer;

  Style.BorderStyle         := ebsFlat;
  StyleFocused.BorderStyle  := ebsFlat;
  StyleHot.BorderStyle      := ebsFlat;

  Properties.Text               := '';
  Properties.ShowText           := True;
  Properties.OverloadBeginColor := clNavy;
  Properties.OverloadEndColor   := clNavy;

  Active := False;
end;

procedure TSBaseProgressBar.DoTimer(Sender: TObject);
var
  ASec: Integer;
begin
  if (Position >= Properties.Max) then
    begin
      if FSize > 0 then
        Dec(FSize)
      else
        begin
          Position := 0;
          FSize := ProgressSize;
        end;
    end
  else
    Position := Position + 1;
  Properties.OverloadValue := Position - FSize;
  ASec := SecondsBetween(Now, FTime);
  if Properties.ShowTextStyle = cxtsText then
  begin
    If ASec > 0 then
      Properties.Text := VarToStr(IncSecond(0, ASec))
    else
      Properties.Text := '';
  end;
end;

procedure TSBaseProgressBar.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if FActive then
    Properties.ShowTextStyle := cxtsText
  else begin
    Properties.ShowTextStyle := cxtsPercent;
    Properties.Text := '';
  end;
  Properties.ShowOverload := FActive;
  FTimer.Enabled := FActive;

  if FActive then begin
    Position := 0;
    Properties.Min := 0;
    Properties.Max := 100;
    FSize := ProgressSize;
    Properties.BeginColor := clBtnFace;
  end else begin
    Position := Pos;
    Properties.Min := Min;
    Properties.Max := Max;
    Properties.BeginColor := clNavy;
  end;
end;

procedure TSBaseProgressBar.SetMax(const Value: Integer);
begin
  FMax := Value;
  if not FActive then
    Properties.Max := FMax;
end;

procedure TSBaseProgressBar.SetMin(const Value: Integer);
begin
  FMin := Value;
  if not FActive then
    Properties.Min := FMin;
end;

procedure TSBaseProgressBar.SetPos(const Value: Integer);
begin
  FPos := Value;
  if not FActive then Position := FPos;
end;

{ TfrmReportProgress }

constructor TSBaseProgressFrm.Create(AOwner: TComponent);
begin
  inherited;

  FShow := False;
  FText := SReportProgressCaption;
  Cancel.Caption := SBaseProperties.Text_MenuCancel;
  FProgressBar := TSBaseProgressBar.Create(Self);

  with TSBaseProgressBar(FProgressBar) do begin
    //Parent  := Self;
    Parent  := PanelMain;
    Top     := LabelTitle.BoundsRect.Bottom + 10;
    Left    := 10;
    Width   := PanelMain.ClientWidth - 20;
    Anchors := [akLeft, akTop, akRight];
  end;

  FWindowList     := nil;
  FParentDisabled := True;
  FActiveWindow   := 0;
  FTerminated     := False;
end;

destructor TSBaseProgressFrm.Destroy;
begin
  if FShow then Close;
  inherited;
end;

procedure TSBaseProgressFrm.CancelClick(Sender: TObject);
begin
  LabelTitle.Caption := SBaseProperties.Message_ProcessTerminating + ' ...';
  FTerminated := True;
  if Assigned(FOnCancelButtonClick) then FOnCancelButtonClick(Sender);
end;

procedure TSBaseProgressFrm.SetText(const Value: String);
begin
  FText := Value;
  if Timer.Enabled then
  else if (IncMilliSecond(FLastUpdateTextTime, Timer.Interval) > Now) then
    Timer.Enabled := True
  else begin
    LabelTitle.Caption  := FText;
    FLastUpdateTextTime := Now;
  end;
//  Application.ProcessMessages;
end;

procedure TSBaseProgressFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FTerminated then
    Action := caHide
  else
    Action := caNone;
  FTerminated := False;
end;

procedure TSBaseProgressFrm.UpdatePos;
var
  FForm: TForm;
begin
  if not Assigned(Parent) then
    FForm := Application.MainForm
  else
    FForm := (Parent as TForm);

  Left := ((FForm.Width - Width) div 2);
  Top := ((FForm.Height - Height) div 2);

  if Parent = nil then
  begin
    Left := FForm.Left + Left;
    Top := FForm.Top + Top;
  end;

  if Left < 0 then Left := 0;
  if Top < 0 then Top := 0;
end;

procedure TSBaseProgressFrm.Show(AParent: TWinControl; ATimer: Boolean = True);

  procedure DisableControls(Control: TControl);
  var
    I, ACount: Integer;
  begin
    if Control is TWinControl then
      with TWinControl(Control) do
        for I := Pred(ControlCount) downto 0 do
          if (Controls[I] <> Self) and Controls[I].Enabled then begin
            ACount := Length(FControls);
            SetLength(FControls, Succ(ACount));
            FControls[ACount]   := Controls[I];
            Controls[I].Enabled := False;
          end;
  end;

begin
//  PanelCancel.Visible

  if Assigned(AParent) then begin
    Parent := AParent;
    if ParentDisabled then
      DisableControls(AParent);
  end else begin
    FActiveWindow := GetActiveWindow;
    FWindowList   := DisableTaskWindows(0);
  end;
  FShow := True;

  inherited Show;

  if ATimer then begin
    TSBaseProgressBar(FProgressBar).Active := True;
    LabelTitle.Caption := SBaseProperties.Message_ProcessWaitForCompletion + ' ...';
  end;

//  Application.ProcessMessages;
end;

procedure TSBaseProgressFrm.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  FLastUpdateTextTime := Now;
  LabelTitle.Caption  := FText;
end;

procedure TSBaseProgressFrm.Close;
var
  i: Integer;
begin
  FShow := False;
  if not Assigned(Parent) then
    begin
      FTerminated := True;
      inherited Close;
      if (FWindowList <> nil) then EnableTaskWindows(FWindowList);
      if FActiveWindow <> 0 then SetActiveWindow(FActiveWindow);
      FWindowList := nil;
      FActiveWindow := 0;
    end
  else
    begin
      for i := Pred(Length(FControls)) downto 0 do
       if Assigned(FControls[i]) then
         FControls[i].Enabled := True;
      SetLength(FControls, 0);
      inherited Close;
    end;
end;

procedure TSBaseProgressFrm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Cancel.Click;
end;

procedure TSBaseProgressFrm.FormShow(Sender: TObject);
begin
  UpdatePos;
end;

function TSBaseProgressFrm.GetProgressCount: Integer;
begin
  if TSBaseProgressBar(FProgressBar).Active then
    Result := 0
  else
    Result := TSBaseProgressBar(FProgressBar).Max;
end;

procedure TSBaseProgressFrm.SetProgressCount(const AValue: Integer);
var
  LOldActive: Boolean;
begin
  with TSBaseProgressBar(FProgressBar) do begin
    LOldActive := Active;
    Active := (AValue = 0);
    if AValue = 0 then
      Max := 1000
    else
      Max := AValue;

    if LOldActive and (not Active) then
      Pos := 0;
  end;
end;

function TSBaseProgressFrm.GetProgressPos: Integer;
begin
  Result := TSBaseProgressBar(FProgressBar).Pos;
end;

procedure TSBaseProgressFrm.SetProgressPos(const Value: Integer);
begin
  TSBaseProgressBar(FProgressBar).Pos := Value;
//  Application.ProcessMessages;
end;

procedure TSBaseProgressFrm.FormResize(Sender: TObject);
begin
  Cancel.Left := (Width - Cancel.Width) shr 1;
end;

(*
procedure TSBaseProgressFrm.SetVisible(const Value: Boolean);
begin
  if Value then
    Show
  else
    Close;
end;
*)

{$IFDEF FASTSCRIPT_RTTI}

{ TFunctions_TSBaseProgressFrm }

constructor TFunctions_TSBaseProgressFrm.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseProgressFrm, TSBaseProgressFrm.ClassParent.ClassName) do begin
    AddMethod('procedure Show(AParent: TWinControl; ATimer: Boolean = True)', Call_TSBaseProgressFrm_Method);
    AddEvent('OnCancelButtonClick', TfsNotifyEvent);
  end;
end;

function TFunctions_TSBaseProgressFrm.Call_TSBaseProgressFrm_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'SHOW' then
    TSBaseProgressFrm(Instance).Show(VarToPointer(Caller.Params[0]), VarToBoolDef(Caller.Params[1], True));
end;
{$ENDIF}

initialization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TFunctions_TSBaseProgressFrm);
{$ENDIF}

  RegisterClass(TSBaseProgressFrm);

finalization
  UnRegisterClass(TSBaseProgressFrm);

end.
