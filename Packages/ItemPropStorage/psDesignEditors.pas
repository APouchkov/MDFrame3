unit psDesignEditors;

interface

uses
  Classes, DesignEditors, VCLEditors, cxPropEditors, ImgList, Graphics,
  Types, SysUtils;

type
  TItemComponentEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TdxItemProperty  = class(TComponentProperty)
    procedure GetValues(Proc: TGetStrProc); override;
  end;

//  TdxImageIndexPropery = class(TImageIndexProperty, ICustomPropertyDrawing)
//    function GetImages: TCustomImageList; override;
//    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
//      ASelected: Boolean);
//    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
//      ASelected: Boolean);
//  end;

implementation
uses
  Forms, psEditFrm, ItemPropStorage, dxBar;

resourcestring
  SEdit = 'Редактировать ...';

procedure TItemComponentEditor.ExecuteVerb(Index: Integer);
var
  i: Integer;
begin
  for i := 0 to Pred(Screen.FormCount) do
    if (Screen.Forms[i] is TpsEditForm) and
     (TpsEditForm(Screen.Forms[i]).PropStorage = Component)
    then
      with Screen.Forms[i] do begin
        Show;
        BringToFront;
        exit;
      end;

  with TpsEditForm.Create(nil) do
    try
      Designer := Self.Designer;
      PropStorage := Component as TdxItemPropStorage;
      Show;
      BringToFront;
    except
      Free;
      raise;
    end;
end;

function TItemComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := SEdit;
end;

function TItemComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TdxItemProperty }

procedure TdxItemProperty.GetValues(Proc: TGetStrProc);
var
  ABarManager: TdxBarManager;
  i: Integer;
  ACategory: string;
begin
  if GetComponent(0) is TdxItemPropStorage then begin
    with TdxItemPropStorage(GetComponent(0)).Properties do begin
      ABarManager := BarManager;
      ACategory := Category;
    end;
    if Assigned(ABarManager) then
      for i := 0 to Pred(ABarManager.ItemCount) do
        if (ACategory = '') or SameText(ACategory, ABarManager.Items[i].Name) then
          Proc(ABarManager.Items[i].Name)
  end;
end;

//{TdxImageIndexProperty}
//
//function TdxImageIndexPropery.GetImages: TCustomImageList;
//begin
//  if (GetComponent(0) is TdxCustomImages) and
//    Assigned(TdxCustomImages(GetComponent(0)).BarManager)
//  then
//    Result := TdxCustomImages(GetComponent(0)).BarManager.Images
//  else
//    Result := nil;
//end;
//
//procedure TdxImageIndexPropery.PropDrawName(ACanvas: TCanvas;
//  const ARect: TRect; ASelected: Boolean);
//begin
//  DefaultPropertyDrawName(Self, ACanvas, ARect);
//end;
//
//procedure TdxImageIndexPropery.PropDrawValue(ACanvas: TCanvas;
//  const ARect: TRect; ASelected: Boolean);
//var
//  AImages: TCustomImageList;
//  AIndex: Integer;
//begin
//  AImages := GetImages;
//  AIndex := StrToIntDef(Value, -1);
//  if Assigned(AImages) and (AIndex >= 0) and
//    (AImages.Height <= Succ(ARect.Bottom - ARect.Top)) and
//    (AImages.Width <= (ARect.Right - ARect.Left))
//  then begin
//    ACanvas.FillRect(ARect);
//    TdxCustomImages(GetComponent(0)).BarManager.Images.Draw(
//      ACanvas, ARect.Left, ARect.Top, AIndex);
//
//    ACanvas.TextOut(ARect.Left + AImages.Width, ARect.Top, Format(' - %s', [Value]));
//  end else
//    DefaultPropertyDrawValue(Self, ACanvas, ARect);
//end;

end.
