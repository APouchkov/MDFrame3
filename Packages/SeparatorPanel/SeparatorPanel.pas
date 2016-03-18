unit SeparatorPanel;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls, ExtCtrls,
  Forms, Math;

type
  { TCustomSeparatorPanel }

  TCustomSeparatorPanel = class(TCustomPanel)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TSeparatorPanel }

  TSeparatorPanel = class(TCustomSeparatorPanel)
  published
    property Height Default 2;
    property AlignWithMargins Default True;

    property Align Default alTop;
    property Anchors;
    property Constraints;
    property Enabled;
    property Visible;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TSeparatorPanel]);
end;

{ TCustomSeparatorPanel }

constructor TCustomSeparatorPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];

  BevelOuter      := bvNone;
  Margins.Left    := 5;
  Margins.Top     := 5;
  Margins.Right   := 0;
  Margins.Bottom  := 5;
  AlignWithMargins := True;

  BevelOuter := bvNone;
  Height := 2;

  with TBevel.Create(Self) do begin
    Align := alClient;
    Parent := Self;
  end;

  Align := alTop;
end;

initialization
  RegisterClasses([TSeparatorPanel]);

finalization
  UnRegisterClasses([TSeparatorPanel]);

end.
