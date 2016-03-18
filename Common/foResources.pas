{******************************************************************************}
{                           -= Модуль ресурсов =-                              }
{                                                                              }
{                               v1.0 (С) 2008                                  }
{                                                                              }
{                                  -=NLT=-                                     }
{                                                                              }
{******************************************************************************}
unit foResources;

interface

uses
  Graphics, pngimage;

function GetColorByName(const ColorName: String; DefaultColor: TColor = clNone): TColor;

function GetResource(AImage: TBitmap; const Name: String; const Default: String = ''): Boolean; overload;
function GetResource(AImage: TPNGImage; const Name: String; const Default: String = ''): Boolean; overload;

procedure RegisterResourceFile(const FileName: String);
procedure UnRegisterResourceFile(const FileName: String);

var
  ColorNames: Array [0..5] of String =
    ('Status:Accept', 'Status:ToExec', 'Status:Execute', 'Status:Reject', 'Status:Cancel', 'Status:Delete');

  ColorValues: Array [0..5] of TColor =
    (clNone, $00E0C592, $94DEB8, $9999FF, $9999FF, $9999FF);

const
  StaticColorNames: Array [0..6] of String =
    ('Grey', 'Red', 'Blue', 'Yellow', 'Green', 'Orange', 'Violet');

  StaticColorValues: Array [0..6] of TColor =
    ($00F4EFEF, $00605DCF, $00B4835C, $0060C1FF, $0067B48B, $005A8CFF, $007C58A5);

implementation

uses
  Windows, SysUtils, Classes;

const
  SResourceTypePNG = 'PNG';

var
  ResourceFiles: TStringList;

function FindColor(ColorArrayNames: Array of String; ColorArrayValues: Array of TColor; const ColorName: String; var Color: TColor): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Pred(Length(ColorArrayNames)) downto 0 do
  if SameText(ColorArrayNames[i], ColorName) then
  begin
    Color := ColorArrayValues[i];
    Result := True;
    break;
  end;
end;

function GetColorByName(const ColorName: String; DefaultColor: TColor): TColor;
begin
  if not FindColor(ColorNames, ColorValues, ColorName, Result) then
  if not FindColor(StaticColorNames, StaticColorValues, ColorName, Result) then Result := clNone;
  if Result = clNone then Result := DefaultColor;
end;

procedure RegisterResourceFile(const FileName: String);
var
  HLib: THandle;
  AIndex: Integer;
begin
  AIndex := ResourceFiles.IndexOf(FileName);
  if AIndex < 0 then
  begin
    if FileExists(FileName) then
    try
      HLib := LoadLibrary(PChar(FileName));
      if HLib > 0 then
        ResourceFiles.AddObject(FileName, TObject(HLib));
    except
    end;
  end;
end;

procedure UnRegisterResourceFile(const FileName: String);
var
  AIndex: Integer;
begin
  AIndex := ResourceFiles.IndexOf(FileName);
  if AIndex > 0 then
  begin
    FreeLibrary(THandle(ResourceFiles.Objects[AIndex]));
    ResourceFiles.Delete(AIndex);
  end;
end;

function ExistsResource(const Name: String; var HLib: THandle; ResType: PChar = RT_BITMAP): Boolean;
var
  i: Integer;
  HRes: THandle;
begin
  Result := False;
  if Name = '' then exit;
  HLib := 0; HRes := 0;
  for i := Pred(ResourceFiles.Count) downto 0 do
  begin
    HLib := THandle(ResourceFiles.Objects[i]);
    HRes := FindResource(HLib, PChar(Name), ResType);
    if HRes > 0 then break;
  end;
  if HRes = 0 then
  begin
    HLib := HInstance;
    HRes := FindResource(HLib, PChar(Name), ResType);
  end;
  Result := HRes <> 0;
  if not Result then HLib := 0;
end;

function GetResource(AImage: Graphics.TBitmap; const Name: String; const Default: String = ''): Boolean; overload;
var
  HLib: THandle;
begin
  Result := True;
  if ExistsResource(Name, HLib) then
    AImage.LoadFromResourceName(HLib, Name)
  else
  if ExistsResource(Default, HLib) then
    AImage.LoadFromResourceName(HLib, Default)
  else
    Result := False;
end;

function GetResource(AImage: TPNGImage; const Name: String; const Default: String = ''): Boolean; overload;

var
  HLib: THandle;

  procedure LoadFromResource(const Name: String);
  var
    ResStream: TResourceStream;
  begin
    try
      ResStream := TResourceStream.Create(HLib, Name, SResourceTypePNG);
    except
      exit;
    end;
    try
      AImage.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
  end;

begin
  Result := True;
  if ExistsResource(Name, HLib, SResourceTypePNG) then
    LoadFromResource(Name)
  else
  if ExistsResource(Default, HLib, SResourceTypePNG) then
    LoadFromResource(Default)
  else
    Result := False;
end;

procedure Initialize;
begin
  ResourceFiles := TStringList.Create;
  ResourceFiles.CaseSensitive := False;
end;

procedure Finalize;
var
  i: Integer;
begin
  for i := Pred(ResourceFiles.Count) downto 0 do
  begin
    FreeLibrary(THandle(ResourceFiles.Objects[i]));
    ResourceFiles.Delete(i);
  end;
  ResourceFiles.Free;
end;

initialization
  Initialize;

finalization
  Finalize;

end.
