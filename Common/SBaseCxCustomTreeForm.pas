unit SBaseCxCustomTreeForm;
{$I config.inc}

interface

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SBaseCxTreeForm,

{$IFDEF FASTSCRIPT_RTTI}
  frxClass, fs_iinterpreter, fs_itools, fs_ievents,
{$ENDIF}

  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxCustomData, cxStyles, cxTL, cxTLdxBarBuiltInMenu, DB, SBaseDataModule,
  ImgList, ExtCtrls, dxBar, cxClasses, cxSplitter, cxInplaceContainer, cxTLData,
  cxDBTL, cxGroupBox, dxStatusBar, System.Actions, ActnList, dxMDBarButton,
  cxMDDBTreeList, dxMDStatusBar, dxSkinsCore, dxSkinsDefaultPainters, dxMDBar;

type
  TOnGetNodeColumnListEvent = procedure(ANode: TcxTreeListNode; var AColumns: TList) of Object;

  TSBaseCxCustomTreeFrm = class(TSBaseCxTreeFrm)
    procedure cxDBTreeListCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
  private
    { Private declarations }
    FOnGetNodeColumnList: TOnGetNodeColumnListEvent;
  protected
    { Protected declarations }
{
    procedure InternalCreate; override;
}
  public
    { Public declarations }
  published
    property OnGetNodeColumnList: TOnGetNodeColumnListEvent read FOnGetNodeColumnList write FOnGetNodeColumnList;
  end;

{$IFDEF FASTSCRIPT_RTTI}
  TFunctions_TSBaseCxCustomTreeFrm = class(TfsRTTIModule)
  private
  public
    constructor Create(AScript: TfsScript); override;
  end;

  TfsGetNodeColumnListEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ANode: TcxTreeListNode; var AColumns: TList);
    function GetMethod: Pointer; override;
  end;
{$ENDIF}

const
  IConst_Column_Left_Margin   = 3;
  IConst_Column_Right_Margin  = 1;
  IConst_GridLine_Size        = 1;

type
  TcxTreeListNodeCrack = class(TcxTreeListNode);

implementation

uses SBaseVariantFunctions, SBaseConstants, SBaseForm;

{$R *.dfm}

procedure TSBaseCxCustomTreeFrm.cxDBTreeListCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
var
  LcxDBTreeList: TcxDBTreeList absolute Sender;

  LIndentWidth: Integer;
  LBoundLeft, LBoundRight: Integer;

  LRect     : TRect;
  I         : Integer;
  LLastColumnIndex: Integer;

  procedure DrawCustomCell(ALeft, ARight: Integer; AColumn: TcxTreeListColumn);
  var
    LRectTemp: TRect;
  begin
    LRectTemp := LRect;
    LRectTemp.Left    := ALeft;
    LRectTemp.Right   := ARight;
{
    ACanvas.FillRect(LRectTemp, AViewInfo.ViewParams, [bLeft, bRight, bTop, bBottom], LcxDBTreeList.OptionsView.GridLineColor);
}
    ACanvas.FrameRect(LRectTemp, LcxDBTreeList.OptionsView.GridLineColor, IConst_GridLine_Size, [bLeft, bRight, bTop, bBottom]);
    LRectTemp.Inflate(-IConst_GridLine_Size, -IConst_GridLine_Size);
    ACanvas.FillRect(LRectTemp, AViewInfo.ViewParams);

    LRectTemp.Left    := ALeft + IConst_GridLine_Size + IConst_Column_Left_Margin;
    LRectTemp.Right   := ARight - IConst_GridLine_Size - IConst_Column_Right_Margin;

    //if VarIsPresent(AValue) then
    ACanvas.DrawText(VarToStr(AViewInfo.Node.Values[AColumn.ItemIndex]), LRectTemp, taLeftJustify, vaCenter, False, True);
  end;
var
  LList   : TList;
  LColumn : TcxTreeListColumn;
begin
  if Assigned(FOnGetNodeColumnList) then begin
    LList := nil;
    FOnGetNodeColumnList(AViewInfo.Node, LList);
    ADone := (LList <> nil);
    if ADone and (AViewInfo.Column.VisibleIndex = 0) then begin
      LRect := AViewInfo.BoundsRect;
      LRect.Right   := AViewInfo.Node.DisplayRect(False).Right;

      LIndentWidth := TcxTreeListNodeCrack(AViewInfo.Node).ViewData.IndentWidth;
      LBoundLeft := LRect.Left;

      LLastColumnIndex := LList.Count - 1;
      for I := 0 to LLastColumnIndex do begin
        LColumn := TcxDBTreeListColumn(LList[I]);
        LBoundRight := LBoundLeft + LColumn.Width;
        if (I = 0) then
          Dec(LBoundRight, LIndentWidth)
        else
          Inc(LBoundRight, IConst_GridLine_Size);
        if (LBoundRight > LRect.Right) or ((I = LLastColumnIndex) and (LBoundRight < LRect.Right)) then
          LBoundRight := LRect.Right;
        DrawCustomCell(LBoundLeft, LBoundRight, LColumn);
        if LBoundRight = LRect.Right then Break;
        LBoundLeft := LBoundRight - IConst_GridLine_Size;
      end;
    end;
  end;
end;

{$IFDEF FASTSCRIPT_RTTI}
constructor TFunctions_TSBaseCxCustomTreeFrm.Create(AScript: TfsScript);
begin
  with AScript.AddClass(TSBaseCxCustomTreeFrm, TSBaseCxCustomTreeFrm.ClassParent.ClassName) do begin
    AddEvent('OnGetNodeColumnList', TfsGetNodeColumnListEvent);
  end;
end;

{ TfsTreeListColumnChangedEvent }

procedure TfsGetNodeColumnListEvent.DoEvent(ANode: TcxTreeListNode; var AColumns: TList);
begin
  CallHandler([ANode, AColumns]);
  AColumns := VarToPointer(Handler.Params[1].Value);
end;

function TfsGetNodeColumnListEvent.GetMethod: Pointer;
begin
  Result := @TfsGetNodeColumnListEvent.DoEvent;
end;

{$ENDIF}

{
procedure TSBaseCxCustomTreeFrm.InternalCreate;
begin
  inherited;

  cxDBTreeList.Styles.Selection := SBaseFormProperties.DefaultStyles.HighlightRecord;
end;
}

initialization
  RegisterClasses([TSBaseCxCustomTreeFrm]);
  fsRTTIModules.Add(TFunctions_TSBaseCxCustomTreeFrm);

finalization
  fsRTTIModules.Remove(TFunctions_TSBaseCxCustomTreeFrm);
  RegisterClasses([TSBaseCxCustomTreeFrm]);

end.
