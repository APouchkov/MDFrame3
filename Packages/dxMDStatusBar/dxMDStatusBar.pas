unit dxMDStatusBar;

interface

uses
  Classes, dxStatusBar;

type
  TdxMDStatusBar = class(TdxCustomStatusBar)
  strict private
    FDefaultPaintStyle: TdxStatusBarPaintStyle;
  public
    property PaintStyle;
  published
    property Images;
    property Panels;
    property DefaultPaintStyle: TdxStatusBarPaintStyle read FDefaultPaintStyle write FDefaultPaintStyle default stpsStandard;
    property SimplePanelStyle;
    property SizeGrip;
//    property LookAndFeel;
    property OnHint;
    property BorderWidth;
    { TcxControl properties}
    property Anchors;
//    property BiDiMode;
//    property DragCursor;
//    property DragKind;
//    property DragMode;
    property Enabled;
//    property Font;
//    property Color default clBtnFace;
    property Constraints;
    property ShowHint;
//    property ParentBiDiMode;
//    property ParentFont default False;
//    property ParentShowHint;
    property PopupMenu;
    property Visible;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
    property OnClick;
    property OnDblClick;
//    property OnDragDrop;
//    property OnDragOver;
//    property OnEndDock;
//    property OnEndDrag;
//    property OnMouseDown;
//    property OnMouseMove;
//    property OnMouseUp;
    property OnResize;
//    property OnStartDock;
//    property OnStartDrag;
  end;

implementation

initialization
  RegisterClass(TdxMDStatusBar);

finalization
  UnRegisterClass(TdxMDStatusBar);

end.
