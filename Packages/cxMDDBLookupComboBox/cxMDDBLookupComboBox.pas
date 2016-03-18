unit cxMDDBLookupComboBox;

interface

uses
  Classes, Windows, Controls, Messages, cxDBLookupComboBox, cxMultiFieldsDBDataBinding,
  cxEdit, cxDBEdit;

type
  TcxMDDBLookupEditDataBinding = class(TcxMDDBTextEditDataBinding)
  protected
    function IsLookupControl: Boolean; override;
  end;

  TcxMDDBLookupComboBox = class(TcxCustomLookupComboBox)
  private
    function GetDataBinding: TcxMDDBTextEditDataBinding;
    procedure SetDataBinding(Value: TcxMDDBTextEditDataBinding);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;

  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DataBinding: TcxMDDBTextEditDataBinding read GetDataBinding
      write SetDataBinding;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

{ TcxMDDBLookupComboBox }

procedure TcxMDDBLookupComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

function TcxMDDBLookupComboBox.GetDataBinding: TcxMDDBTextEditDataBinding;
begin
  Result := TcxMDDBTextEditDataBinding(FDataBinding);
end;

class function TcxMDDBLookupComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxMDDBLookupEditDataBinding;
end;

procedure TcxMDDBLookupComboBox.SetDataBinding(Value: TcxMDDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

{ TcxDBLookupEditDataBinding }

function TcxMDDBLookupEditDataBinding.IsLookupControl: Boolean;
begin
  Result := True;
end;

end.
