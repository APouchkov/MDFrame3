unit cxStaticLabel;

interface

{$I cxVer.inc}

uses
  Classes, cxLabel, cxEdit, cxContainer
{$IFDEF DELPHI6}
  , Variants
{$ENDIF}
;

type
  TcxStaticLabel = class;

  TcxCustomStaticLabelProperties = class(TcxCustomLabelProperties)
  private
    FText: String;
    procedure SetText(const Value: String);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
  public
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue; AFullText: Boolean = False; AIsInplace: Boolean = True): WideString; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;

    property Text: String read FText write SetText;
  end;

  { TcxStaticLabelProperties }

  TcxStaticLabelProperties = class(TcxCustomStaticLabelProperties)
  published
    property Text;

    property Alignment;
    property Angle;
    property Depth;
    property Glyph;
    property LabelEffect;
    property LabelStyle;
    property LineOptions;
    property Orientation;
    property PenWidth;
    property ShadowedColor;
    property ShowAccelChar;
    property ShowEndEllipsis;
    property Transparent; // deprecated
    property WordWrap;
  end;

  TcxCustomStaticLabelViewData = class(TcxCustomLabelViewData)
  protected
    function InternalEditValueToDisplayText(AEditValue: TcxEditValue): string; override;
  end;

  TcxCustomStaticLabel = class(TcxCustomLabel)
  end;

  TcxStaticLabel = class(TcxCustomStaticLabel);

  TcxEditRepositoryStaticLabelItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxStaticLabelProperties;
    procedure SetProperties(Value: TcxStaticLabelProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxStaticLabelProperties read GetProperties write SetProperties;
  end;

implementation

const
  scxSEditRepositoryStaticLabel: String = 'StaticLabel|Represents a static label';

{ TcxCustomStaticLabelProperties }

procedure TcxCustomStaticLabelProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomStaticLabelProperties then
    with TcxCustomStaticLabelProperties(AProperties) do
    begin
      Self.Text := Text;
    end;
end;

function TcxCustomStaticLabelProperties.GetDisplayText(const AEditValue: TcxEditValue; AFullText, AIsInplace: Boolean): WideString;
begin
  Result := FText
end;

class function TcxCustomStaticLabelProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCustomStaticLabelViewData;
end;

class function TcxCustomStaticLabelProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxStaticLabel
end;

procedure TcxCustomStaticLabelProperties.PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean);
begin
  DisplayValue := FText
end;

procedure TcxCustomStaticLabelProperties.SetText(const Value: String);
begin
  if FText <> Value then begin
    FText := Value;
    Changed
  end;
end;

{ TcxCustomStaticLabelViewData }

function TcxCustomStaticLabelViewData.InternalEditValueToDisplayText(AEditValue: TcxEditValue): string;
begin
  Result := TcxCustomStaticLabelProperties(FProperties).Text
end;

{ TcxEditRepositoryStaticLabelItem }

class function TcxEditRepositoryStaticLabelItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxStaticLabelProperties
end;

function TcxEditRepositoryStaticLabelItem.GetProperties: TcxStaticLabelProperties;
begin
  Result := inherited Properties as TcxStaticLabelProperties
end;

procedure TcxEditRepositoryStaticLabelItem.SetProperties(Value: TcxStaticLabelProperties);
begin
  inherited Properties := Value;
end;

initialization
  RegisterClasses([TcxStaticLabelProperties, TcxEditRepositoryStaticLabelItem]);
  GetRegisteredEditProperties.Register(TcxStaticLabelProperties, scxSEditRepositoryStaticLabel);

finalization
  GetRegisteredEditProperties.Unregister(TcxStaticLabelProperties);
  UnRegisterClasses([TcxStaticLabelProperties, TcxEditRepositoryStaticLabelItem]);

end.
