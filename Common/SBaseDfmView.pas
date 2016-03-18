unit SBaseDfmView;
{$I config.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  SBaseForm, StdCtrls, dxBar, cxControls, cxContainer, Dialogs, PropFilerEh,
  cxEdit, cxTextEdit, cxMemo, ExtCtrls, dxStatusBar, dxMDStatusBar, cxLookAndFeels,
  cxLookAndFeelPainters, cxGraphics, cxMaskEdit, cxButtons,
  cxCheckBox, System.Actions, ActnList, RichEdit, Rtf2Html,
  cxClasses, SynEditHighlighter, SynHighlighterPas, SynEdit, SynHighlighterDfm, DataModule,
  SynEditMiscClasses, SynEditSearch, dxSkinsCore, dxSkinsDefaultPainters,
  dxMDBar;

type
  TSBaseDfmView = class(TSBaseFrm)
    dxStatusBar: TdxMDStatusBar;
    SynEdit: TSynEdit;
    ActionFind: TAction;
    ActionFindNext: TAction;

    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SynEditSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
  private
    FErrorLine  : Integer;

    const CPanelLine  : Integer = 1;
    const CPanelChar  : Integer = 2;
  protected
    procedure InternalCreate; override;
    procedure InternalInit; override;
  end;

  TDfmParser = class
  strict private
{$IFDEF DEBUG}
    procedure ErrorHandle(Sender: TObject; const AMessage: String; var AHandled: Boolean);
{$ENDIF}
  private
{$IFDEF DEBUG}
    FStringStream: TStringStream;

    class var FExceptionHandled: Boolean;
    class var FInstance: TDfmParser;
{$ENDIF}
  public
    class function Parse(const ADFM: String): TMemoryStream;
  end;

implementation

{$R *.dfm}

uses
  NamedVariables, SBaseVariantFunctions, SBaseConstants, hsDialogs,
  SynEditTypes, SBaseUtils, MDSynSearch;

//  KeyWords: array[0..3] of string = ('object', 'inherited', 'end', 'item');

{$REGION 'TSBaseDfmView'}

procedure TSBaseDfmView.InternalCreate;
begin
  inherited;

  IconIndex := SBaseFormProperties.IconIndex_Properties;

  with TMDSynSearch.Create(Self) do begin
    ActionFind      := Self.ActionFind;
    ActionFindNext  := Self.ActionFindNext;
    //ActionReplace   := Self.ActionReplace;
  end;
end;

procedure TSBaseDfmView.InternalInit;
var
  LIdx      : Integer;
  LPosition : Integer;
begin
  inherited;

  dxStatusBar.Panels[0].Text := VarToStr(CreateFormParams[SConst_Message]);

  if CreateFormParams.Find(SConst_DFM, LIdx) then begin
    SynEdit.Text := VarToStr(CreateFormParams.Items[LIdx].Value);
    CreateFormParams.Delete(LIdx);
    SynEditStatusChange(Self, [scAll]);
  end;

  FErrorLine := -1;
  LPosition := -1;
  if CreateFormParams.Find(SConst_Line, LIdx) then begin
    FErrorLine := CreateFormParams.Items[LIdx].Value;
  end;
  if CreateFormParams.Find(SConst_Position, LIdx) then begin
    LPosition := CreateFormParams.Items[LIdx].Value;
  end;
  if FErrorLine >= 0 then
    SynEdit.CaretY := FErrorLine;
  if (LPosition >= 0) and (LPosition <= SynEdit.Lines[FErrorLine-1].Length) then
    SynEdit.CaretX := LPosition;
end;

procedure TSBaseDfmView.SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if Changes * [scAll, scCaretX, scCaretY] <> [] then begin
    dxStatusBar.Panels[CPanelLine].Text := 'ÑÒÐ: ' + IntToStr(SynEdit.CaretY);
    dxStatusBar.Panels[CPanelChar].Text := 'ÏÎÇ: ' + IntToStr(SynEdit.CaretX);
  end;
end;

procedure TSBaseDfmView.SynEditSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if Line = FErrorLine then begin
    Special := True;
    FG := clWhite;
    BG := clRed;
  end;
end;


{$ENDREGION}

{$REGION 'TDfmParser'}

{$IFDEF DEBUG}
procedure TDfmParser.ErrorHandle(Sender: TObject; const AMessage: String; var AHandled: Boolean);
var
  LMessage: String;
begin
  FInstance.FExceptionHandled := True;
  LMessage := SBaseProperties.Text_Line + ' ' + IntToStr(TParser(Sender).SourceLine) + ': ' + AMessage;
  if hsDialogs.HSMessageDlg(LMessage, mtError, [mbOk, mbAbort], 0) = mrAbort then
    TSBaseDfmView.CreateByParams
        (
          Application.MainForm,
          TNamedVariants.Create
          (
            [
              TNamedVariant.Create(SConst_DFM, FStringStream.DataString),
              TNamedVariant.Create(SConst_Message, LMessage),
              TNamedVariant.Create(SConst_Line, TParser(Sender).SourceLine),
              TNamedVariant.Create(SConst_Position, TParser(Sender).LinePos)
            ],
            True
          )
        ).ShowModal;
end;
{$ENDIF}

class function TDfmParser.Parse(const ADFM: String): TMemoryStream;
var
  SS: TStringStream;
begin
  if ADFM.IsEmpty then
    Result := nil
  else begin
    SS := TStringStream.Create('inherited Self: TComponent' + SConst_CR + ADFM + SConst_CR + 'end' );
    try
      SS.Position := 0;
{$IFDEF DEBUG}
      if not Assigned(FInstance) then
        FInstance := TDfmParser.Create;
      FInstance.FExceptionHandled := False;
      FInstance.FStringStream := SS;
{$ENDIF}

      Result := TMemoryStream.Create;
      try
        ObjectTextToBinary(SS, Result{$IFDEF DEBUG}, FInstance.ErrorHandle{$ENDIF})
      except
        FreeAndNil(Result);
{$IFDEF DEBUG}
        if FInstance.FExceptionHandled then
          Abort
        else
          Raise;
{$ELSE}
        Raise;
{$ENDIF}
      end;
    finally
      SS.Free;
    end;
  end;
end;

{$ENDREGION}

initialization
  {$IFDEF DEBUG}
  TDfmParser.FInstance := nil;
  {$ENDIF}

finalization
  {$IFDEF DEBUG}
  FreeAndNil(TDfmParser.FInstance);
  {$ENDIF}

end.

