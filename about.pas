unit About;

interface

uses
  Windows, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls,
  SysUtils, SBaseForm, dxBar, System.Actions, ActnList, cxClasses;

type
  TAboutBox = class(TSBaseFrm)
    Panel: TPanel;
    OKButton: TButton;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    pn: TLabel;
    ver: TLabel;
    cr: TLabel;
    com: TLabel;
  protected
    procedure InternalCreate; override;
  end;

implementation

uses DataModule;

{$R *.dfm}

procedure TAboutBox.InternalCreate;
begin
  with MainDataModule.VersionInfo do
    Ver.Caption := format('%d.%d.%d.%d', [FileLongVersion.All[2], FileLongVersion.All[1], FileLongVersion.All[4], FileLongVersion.All[3]])
{$IFDEF DEBUG}
  + ' (Debug)';
{$ENDIF}
end;

initialization
  RegisterClass(TAboutBox);

finalization
  UnRegisterClass(TAboutBox);

end.
