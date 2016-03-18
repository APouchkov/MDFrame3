{*******************************************************}
{                                                       }
{                       EhLib v5.2                      }
{            Specific routines for VCL.Win32            }
{                      Build 5.2.01                     }
{                                                       }
{     Copyright (c) 2004-10 by Dmitry V. Bolshakov      }
{                                                       }
{*******************************************************}

unit EhLibVCL;

interface

{$WARNINGS OFF}

uses
  Windows, Forms, SysUtils, Classes, DB, TypInfo, Controls, Graphics,
  RTLConsts, Variants,
  Messages;

type
  IntPtr = Pointer;

const
  NilBookmarkEh = nil;

function CharInSetEh(C: Char; const CharSet: TSysCharSet): Boolean;

// WindowsSetStdCursor

function NlsUpperCase(const S: String): String;
function NlsLowerCase(const S: String): String;
function NlsCompareStr(const S1, S2: String): Integer;
function NlsCompareText(const S1, S2: String): Integer;

function HexToBinEh(Text: Pointer; var Buffer: TBytes; Count: Integer): Integer;
procedure BinToHexEh(Buffer: TBytes; var Text: String; Count: Integer);

procedure StreamWriteBytes(Stream: TStream; Buffer: TBytes);
procedure StreamReadBytes(Stream: TStream; var Buffer: TBytes; Count: Integer);

function BytesOf(S: String): TBytes;

function PropInfo_getPropType(APropInfo: PPropInfo): PTypeInfo;
function PropInfo_getName(APropInfo: PPropInfo): String;
function PropType_getKind(APropType: PTypeInfo): TTypeKind;

type
  IInterface = IUnknown;

  IInterfaceComponentReference = interface
    ['{E28B1858-EC86-4559-8FCD-6B4F824151ED}']
    function GetComponent: TComponent;
  end;

implementation

function CharInSetEh(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := CharInSet(C, CharSet);
end;

function DrawTextEh(hDC: HDC; Text: String; nCount: Integer;
  var lpRect: TRect; uFormat: UINT): Integer;
begin
  Result := DrawText(hDC, PChar(Text), nCount, lpRect, uFormat);
end;

function NlsUpperCase(const S: String): String;
begin
  Result := AnsiUpperCase(S);
end;

function NlsLowerCase(const S: String): String;
begin
  Result := AnsiLowerCase(S);
end;

function NlsCompareStr(const S1, S2: String): Integer;
begin
  Result := AnsiCompareStr(S1, S2);
end;

function NlsCompareText(const S1, S2: String): Integer;
begin
  Result := AnsiCompareText(S1, S2);
end;

function HexToBinEh(Text: Pointer; var Buffer: TBytes; Count: Integer): Integer;
begin
  SetLength(Buffer, 0);
  SetLength(Buffer, Count div 2);
  Result := HexToBin(PAnsiChar(Text), PAnsiChar(Buffer), Count);
end;

procedure BinToHexEh(Buffer: TBytes; var Text: String; Count: Integer);
var
  AnsiText: AnsiString;
begin
  SetString(AnsiText, nil, Count*2);
  BinToHex(PAnsiChar(Buffer), PAnsiChar(AnsiText), Count);
  Text := AnsiText;
end;

procedure StreamWriteBytes(Stream: TStream; Buffer: TBytes);
begin
  Stream.Write(Pointer(Buffer)^, Length(Buffer));
end;

procedure StreamReadBytes(Stream: TStream; var Buffer: TBytes; Count: Integer);
var
  bs: AnsiString;
  i: Integer;
begin
  SetLength(Buffer, Count);
  SetString(bs, nil, Count);
  Stream.Read(Pointer(bs)^, Count);
  for i := 0 to Length(bs)-1 do
    Buffer[i] := Byte(bs[i+1]);
end;

function BytesOf(S: String): TBytes;
var
  i: Integer;
begin
  SetLength(Result, Length(S));
  for i := 0 to Length(S)-1 do
    Result[i] := Byte(AnsiChar(S[i+1]));
end;

function PropInfo_getPropType(APropInfo: PPropInfo): PTypeInfo;
begin
  Result := APropInfo^.PropType^;
end;

function PropInfo_getName(APropInfo: PPropInfo): String;
begin
  Result := APropInfo^.Name;
end;

function PropType_getKind(APropType: PTypeInfo): TTypeKind;
begin
  Result := APropType^.Kind;
end;

end.



