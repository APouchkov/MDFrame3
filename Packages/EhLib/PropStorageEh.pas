{*******************************************************}
{                                                       }
{                      EhLib v5.2                       }
{                    (Build 5.2.00)                     } 
{                                                       }
{   TPropStorageManagerEh, TIniPropStorageManEh,        }
{   TRegPropStorageManEh,  TPropStorageEh components    }
{                                                       }
{   Copyright (c) 2002-2006 by Dmitry V. Bolshakov      }
{                                                       }
{*******************************************************}

unit PropStorageEh;

interface

uses
  Windows, Forms, Controls, Registry, PropFilerEh, Dialogs, SysUtils,
  EhLibVCL,
  Classes, IniFiles, TypInfo;

type
{ TPropStorageEh }

  TPropStorageEh = class(TComponent)
  public
    procedure ReadPropValues(Stream: TStream);
  end;

implementation

{ TPropStorageEh }

procedure TPropStorageEh.ReadPropValues(Stream: TStream);
var
  pr: TPropReaderEh;
begin
  pr := TPropReaderEh.Create(Stream, 1024);

  try
    try
      pr.BeginReferences;
      pr.ReadOwnerProperties(Owner);
      pr.FixupReferences;
    finally
      pr.EndReferences;
    end;
  finally
    pr.Free;
  end;
end;

end.
