{ -=MD=-: Delphi XE TRegExpr used}

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditRegexSearch.pas, released 2002-07-26.

Original Code by Eduardo Mauro, Gerald Nunn and Fl�vio Etrusco.
Unicode translation by Ma�l H�rz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditRegexSearch.pas,v 1.5.2.2 2008/09/14 16:24:59 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITREGEXSEARCH}
unit SynEditRegexSearch;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QSynEditTypes,
  QSynEditMiscClasses,
  QSynUnicode,
{$ELSE}
  SynEditTypes,
  SynEditMiscClasses,
  SynUnicode,
{$ENDIF}
  RegularExpressions,
  Classes;

type
  TSynEditRegexSearch = class(TSynEditSearchCustom)
  private
{ -=MD=-
    fPositions: TList;
    fLengths: TList;
}
    FPattern: String;
    FOptions: TRegExOptions;
    fMatches: TMatchCollection;

  protected
    function GetPattern: UnicodeString; override;
    procedure SetPattern(const AValue: UnicodeString); override;
    procedure SetOptions(const Value: TSynSearchOptions); override;
    function GetLength(Index: Integer): Integer; override;
    function GetResult(Index: Integer): Integer; override;
    function GetResultCount: Integer; override;
  public
{ -=MD=-
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
}
    function FindAll(const NewText: UnicodeString): Integer; override;
    function Replace(const aOccurrence, aReplacement: UnicodeString): UnicodeString; override;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QConsts;
{$ELSE}
  Consts;
{$ENDIF}

{ TSynEditRegexSearch }

{ -=MD=-
constructor TSynEditRegexSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRegex := TRegEx.Create;
  fPositions := TList.Create;
  fLengths := TList.Create;
end;
}

{ -=MD=-
destructor TSynEditRegexSearch.Destroy;
begin
  inherited;
  fRegex.Free;
  fPositions.Free;
  fLengths.Free;
end;
}

function TSynEditRegexSearch.FindAll(const NewText: UnicodeString): Integer;
{ -=MD=-
  procedure AddResult(const aPos, aLength: Integer);
  begin
    fPositions.Add(Pointer(aPos));
    fLengths.Add(Pointer(aLength));
  end;
}
begin
{ -=MD=-
  fPositions.Clear;
  fLengths.Clear;

  if fRegex.Exec(NewText) then
  begin
    AddResult(fRegex.MatchPos[0], fRegex.MatchLen[0]);
    Result := 1;
    while fRegex.ExecNext do
    begin
      AddResult(fRegex.MatchPos[0], fRegex.MatchLen[0]);
      Inc(Result);
    end;
  end
  else
    Result := 0;
}
  fMatches := TRegEx.Matches(NewText, FPattern, FOptions);
  Result := fMatches.Count;
end;

function TSynEditRegexSearch.Replace(const aOccurrence, aReplacement: UnicodeString): UnicodeString;
begin
{ -=MD=-
  Result := fRegex.Replace(aOccurrence, aReplacement, True);
}
  Result := TRegEx.Replace(aOccurrence, FPattern, aReplacement, FOptions);
end;

function TSynEditRegexSearch.GetLength(Index: Integer): Integer;
begin
{ -=MD=-
  Result := Integer(fLengths[Index]);
}
  Result := fMatches[Index].Length;
end;

procedure TSynEditRegexSearch.SetPattern(const AValue: UnicodeString);
begin
{ -=MD=-
  fRegex.Expression := Value;
}
  FPattern := AValue;
end;

function TSynEditRegexSearch.GetPattern: UnicodeString;
begin
{ -=MD=-
  Result := fRegex.Expression;
}
  Result := FPattern;
end;

function TSynEditRegexSearch.GetResult(Index: Integer): Integer;
begin
{ -=MD=-
  Result := Integer(fPositions[Index]);
}
  Result := fMatches[Index].Index;
end;

function TSynEditRegexSearch.GetResultCount: Integer;
begin
{ -=MD=-
  Result := fPositions.Count;
}
  Result := fMatches.Count;
end;

procedure TSynEditRegexSearch.SetOptions(const Value: TSynSearchOptions);
begin
{ -=MD=-
  fRegex.ModifierI := not(ssoMatchCase in Value);
}
  if ssoMatchCase in Value then
    FOptions := FOptions - [roIgnoreCase]
  else
    FOptions := FOptions + [roIgnoreCase]
end;

end.

