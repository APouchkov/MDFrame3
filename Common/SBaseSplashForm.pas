unit SBaseSplashForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, jpeg, StdCtrls, IOUtils;

type
  TSBaseSplashFrm = class(TForm)
    Img: TImage;
    Version: TLabel;
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses DataModule, SBaseFileUtils;

const
  SConst_Splash : string = 'Splash';

constructor TSBaseSplashFrm.Create(AOwner: TComponent);
const
  Formats: array [0..2] of String = ('bmp', 'gif', 'jpg' );
var
  FileName, FileNameExt: string;
  I: Integer;
begin
  inherited;

  FileName := ChangeFileExt(ApplicationExeName, '.' + SConst_Splash);
  I := Pred(Length(Formats));
  while I >= 0 do begin
    FileNameExt := FileName + '.' + Formats[I];
    if TFile.Exists(FileNameExt) then Break;
    Dec(I);
  end;

  if I >= 0 then
    Img.Picture.LoadFromFile(FileNameExt);

  Version.Caption := MainDataModule.VersionInfo.ProductName + ' / ' + MainDataModule.VersionInfo.FileVersion
end;

procedure TSBaseSplashFrm.FormDeactivate(Sender: TObject);
begin
//  Free;
  BringToFront;
  Update
end;

initialization
  RegisterClass(TSBaseSplashFrm);

finalization
  UnRegisterClass(TSBaseSplashFrm);

end.
