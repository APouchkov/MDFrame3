unit XProc;

interface

uses Windows, Messages, Classes, Controls, MSODSAPI, Variants, SysUtils, DB,
      DateUtils;

const
  XP_MAX_PARAMS = 15;
  XP_MAX_FIELDS = 20;
  XP_MEM_DATA   = 7000;
type
  PSingle= ^Single;
  PReal  = ^Real48;
  PDouble= ^Double;

  TXParam = record
    Name: String;
    IsNull: Boolean;
    IsLocal: Boolean;
    Value: Variant;
    Data: String;
    DataType: TFieldType;
    DataSize: ULONG;
  end;
  TXField = record
    Name: String;
    Data: String;
    DataType: TFieldType;
    DataSize: Word;
  end;

  TXProc = class (TObject)
  private
    pSrvProc : SRV_PROC;
    FParamCount : Word;
    FFieldCount : Word;
    FParams: array of TXParam;
    FFields: array of TXField;
    FRowCount: Integer;
    FisSuccess: Boolean;
    procedure FloatToBcd(f: Extended; bcd: PDBNUMERIC);
  public
    constructor Create (pSrvProc : SRV_PROC; StringStoring: Boolean = True);
    destructor  Destroy; override;

    property ParamCount  : Word read FParamCount;
    property FieldCount  : Word read FFieldCount;
    property isSuccess   : Boolean read FisSuccess write FisSuccess;

    function  GetParam(ID: Byte): Variant;
    function  GetParamData(ID: Byte): Pointer;
    function  GetParamIsNull(ID: Byte): Boolean;
    function  GetParamLen(ID: Byte): Integer;
    function  GetParamName(ID: Byte): String;
    function  GetParamByName(const ParamName: String): Variant;
    function  GetParamType(ID: Byte): TFieldType;
    procedure SetParam(ID: Byte; v: Variant; len: Integer = 0);
    procedure SetParamByName(const ParamName: String; Value: Variant);

    procedure CreateField(ID: Byte; FieldName: String; ft: TFieldType; len: Word = 0; precision: Byte = 0; scale: Byte = 0);
    procedure SetField(ID: Byte; v: Variant);
    function  GetFieldName(ID: Byte): String;

    procedure Next;
    procedure RaisError(szErrorMsg : String);

    procedure DateTimeToSQLSmallDateTime(dt: TDateTime; sqldt: PDBDATETIM4);
    procedure DateTimeToSQLDateTime(dt: TDateTime; sqldt: PDBDATETIME);
  end;

implementation

//uses ShellAPI;

constructor TXProc.Create(pSrvProc : SRV_PROC; StringStoring: Boolean = True);
var
  fName: PChar;
  i,j,len,PParamCount: Integer;

  bType: Byte;
  fNull: Bool;
  cbMaxLen, cbActualLen: ULONG;

  eParam: Extended;
  dParam: TDateTime;
begin
  DecimalSeparator := '.';
  DateSeparator := '.';

  ShortDateFormat := 'dd.mm.yy';
  LongDateFormat := 'dd.mm.yyyy';
  TimeSeparator := ':';
  ShortTimeFormat := 'h:nn:ss';
  LongTimeFormat := 'hh:nn:ss';

  FisSuccess := True;
  FRowCount  := 0;

  inherited Create;
  self.pSrvProc := pSrvProc;
  FFieldCount  := 0;

  PParamCount  := srv_rpcparams(pSrvProc);
  if PParamCount < 0 then
    FParamCount := 0
  else if PParamCount > XP_MAX_PARAMS then
    FParamCount := XP_MAX_PARAMS
  else
    FParamCount := PParamCount;
  SetLength(FParams, FParamCount);

  try
    for i := Pred(FParamCount) downto 0 do with FParams[i] do begin
      srv_paraminfo(pSrvProc, Succ(i), @bType, @cbMaxLen, @cbActualLen, nil, @fNull);
      if not (fNull or (bType in [SRVVARCHAR,SRVCHAR,SRVBIGVARCHAR,SRVBIGCHAR,SRVNVARCHAR,SRVNCHAR,SRVBINARY,SRVVARBINARY,SRVBIGBINARY,SRVBIGVARBINARY,SRVTEXT,SRVNTEXT,SRVIMAGE])) then begin
        SetLength(Data, cbMaxLen);
        srv_paraminfo(pSrvProc, Succ(i), @bType, @cbMaxLen, @cbActualLen, Pointer(Data), @fNull);
      end;

      case bType of
        SRVVARCHAR,SRVCHAR,SRVBIGVARCHAR,SRVBIGCHAR,SRVNVARCHAR,SRVNCHAR,SRVBINARY,SRVVARBINARY,SRVBIGBINARY,SRVBIGVARBINARY:
        begin
          DataType := ftString;
          if fNull then
            DataSize := 0
          else begin
            Value    := Null;
            DataSize := cbActualLen;
            if StringStoring or (srv_paramlen(pSrvProc,i)<cbActualLen) then begin
              SetLength(Data, cbActualLen);
              IsLocal := True;
              srv_paraminfo(pSrvProc, Succ(i), @bType, @cbMaxLen, @cbActualLen, Pointer(Data), @fNull);
            end else begin
              IsLocal := False
            end;
          end;
        end;

        SRVINTN:
        begin
          DataType := ftInteger;
          if not fNull then case cbMaxLen of
            1: Value := PByte(Pointer(Data))^;
            2: Value := PSmallInt(Pointer(Data))^;
            4: Value := PLongint(Pointer(Data))^;
            else DataType := ftUnknown;
          end;
        end;

        SRVFLTN:
        begin
          DataType := ftFloat;
          if not fNull then case cbMaxLen of
            4: Value := PSingle(Pointer(Data))^;
            6: Value := PReal(Pointer(Data))^;
            8: Value := PDouble(Pointer(Data))^;
            10:Value := PExtended(Pointer(Data))^;
            else DataType := ftUnknown;
          end;
        end;

        SRVNUMERICN:
        begin
          DataType := ftBCD;
          if not fNull then begin
            eParam := 0;
            j := cbMaxLen-4;
            while (j>=0) and ((PDBNUMERIC(Pointer(Data))^).val[j]=0) do Dec(j);
            while (j>=0) do begin eParam := eParam * 256.0 + (PDBNUMERIC(Pointer(Data))^).val[j]; Dec(j) end;

            for j := (PDBNUMERIC(Pointer(Data))^).scale downto 1 do eParam := eParam / 10.0;
            if (PDBNUMERIC(Pointer(Data))^).sign <> 1 then eParam := -eParam;

            Value    := eParam;
            PDBNUMERIC(Pointer(Data)).precision:= PDBNUMERIC(Pointer(Data)).precision;
            PDBNUMERIC(Pointer(Data)).scale:= PDBNUMERIC(Pointer(Data)).scale;
          end else begin
            SetLength(Data, SizeOf(DBNUMERIC));
            PDBNUMERIC(Pointer(Data)).precision := 18;
            PDBNUMERIC(Pointer(Data)).scale:= 6;
          end;
        end;

        SRVDATETIMN:
        begin
          DataType := ftDateTime;
          if not fNull then begin
            case cbMaxLen of
              4: begin
                dParam := IncDay(EncodeDate(1900, 1, 1),(PDBDATETIM4(Pointer(Data))^).numdays);
                if (PDBDATETIM4(Pointer(Data))^).nummins<>0 then
                  dParam := IncMinute(dParam, (PDBDATETIM4(Pointer(Data))^).nummins)
              end;
              8: begin
                dParam := IncDay(EncodeDate(1900,1,1),(PDBDATETIME(Pointer(Data))^).dtdays);
                if (PDBDATETIME(Pointer(Data))^).dttime<>0 then begin
                  dParam := IncSecond(dParam, (PDBDATETIME(Pointer(Data))^).dttime div 300);
                end;
              end
              else DataType := ftUnknown;
            end;
            Value := dParam;
          end;
        end;

        SRVBITN:
        begin
          DataType := ftBoolean;
          if not fNull then Value    := PByte(Pointer(Data))^ <> 0;
        end;

        SRVMONEYN:
        begin
          DataType := ftCurrency;
          if not fNull then
            if cbMaxLen=8 then
              Value    := PCurrency(Pointer(Data))^
            else
              DataType := ftUnknown;
        end;

        SRVTEXT,SRVNTEXT,SRVIMAGE:
        begin
          DataType     := ftVarBytes;
          Value        := Null;
          IsLocal      := False;
          DataSize     := cbActualLen;
        end
      
        else
          DataType := ftUnknown;
      end;

      if not (DataType in [ftUnknown,ftVarBytes,ftString]) then begin
        IsLocal  := True;
        DataSize := cbMaxLen;
      end;

      if fNull or (DataType = ftUnknown) then begin
        IsNull := True;
        Value := Null;
      end else
        IsNull := False;

      if (cbMaxLen>0) and not (DataType in [ftBCD,ftUnknown,ftString,ftVarBytes]) then SetLength(Data,cbMaxLen);

      fName := srv_paramname(pSrvProc, Succ(i), len);
      if (fName = nil) or (len = 0) then
        Name := ''
      else begin
        if fName[0] = '@' then begin Inc(fName); Dec(len) end;
        SetLength(Name, len);
        MoveMemory(@Name[1], fName, len);
      end;
    end
  except on E:Exception do begin
    RaisError(E.Message);
    Abort
  end end;
end;

destructor TXProc.Destroy;
begin
  if FisSuccess then
    srv_senddone(pSrvProc,(SRV_DONE_COUNT or SRV_DONE_MORE), 0, FRowCount)
  else
    srv_senddone(pSrvProc,(SRV_DONE_ERROR), 0, 0);
  inherited Destroy;
end;

procedure TXProc.Next;
begin
  if FisSuccess then begin
    srv_sendrow(pSrvProc);
    Inc(FRowCount);
  end;
end;

// send szErrorMsg to client
procedure TXProc.RaisError (szErrorMsg : String);
begin;
  srv_sendmsg(pSrvProc, SRV_MSG_ERROR, 20001, SRV_FATAL_PROCESS {SRV_INFO}, 1,
    nil, 0, 0, @szErrorMsg[1], length(szErrorMsg));
  FisSuccess := False;
//  srv_senddone(pSrvProc, (SRV_DONE_ERROR) or (SRV_DONE_MORE), 0, 0);
//  Raise Exception.Create(szErrorMsg);
end;

function  TXProc.GetParam(ID: Byte): Variant;
begin
  if (ID = 0) or (ID > FParamCount) or FParams[Pred(ID)].IsNull then
    Result := Null
  else if FParams[Pred(ID)].DataType in [ftString, ftVarBytes] then
    Result := FParams[Pred(ID)].Data
  else
    Result := FParams[Pred(ID)].Value
end;

function  TXProc.GetParamByName(const ParamName: String): Variant;
var
  i: Integer;
begin
  Result := Null;
  for i := Pred(ParamCount) downto 0 do if CompareText(ParamName, FParams[i].Name) = 0 then begin
    if FParams[i].DataType in [ftString,ftVarBytes] then
      Result := FParams[i].Data
    else
      Result := FParams[i].Value;
    Exit
  end
end;

function  TXProc.GetParamIsNull(ID: Byte): Boolean;
begin
  if (ID = 0) or (ID > FParamCount) then
    Result := True
  else
    Result := FParams[Pred(ID)].IsNull
end;

function  TXProc.GetParamData(ID: Byte): Pointer;
begin
  if (ID = 0) or (ID > FParamCount) then
    Result := nil
  else if FParams[Pred(ID)].IsLocal then
    Result := @FParams[Pred(ID)].Data[1]
  else
    Result := srv_paramdata(pSrvProc, ID)
end;

function  TXProc.GetParamLen(ID: Byte): Integer;
begin
  if (ID = 0) or (ID > FParamCount) then
    Result := 0
  else
    Result := FParams[Pred(ID)].DataSize
end;

function  TXProc.GetParamName(ID: Byte): String;
begin
  if (ID = 0) or (ID > FParamCount) then
    Result := ''
  else begin
    Result := FParams[Pred(ID)].Name
  end;
end;

procedure TXProc.SetParamByName(const ParamName: String; Value: Variant);
var
  i: Integer;
begin
  for i := Pred(ParamCount) downto 0 do
    if CompareText(ParamName, FParams[i].Name) = 0 then begin
      SetParam(i, Value);
      Exit
    end
end;

function  TXProc.GetParamType(ID: Byte): TFieldType;
begin
  if (ID = 0) or (ID > FParamCount) then
    Result := ftUnknown
  else
    Result := FParams[Pred(ID)].DataType
end;

procedure TXProc.SetParam(ID: Byte; v: Variant; len: Integer = 0);
var
  i: Integer;
  function Hex2Dec(const Hex: String): Integer;
  var
    x: Integer;
  begin
    Result := 0;
    for x := 1 to Length(Hex) do
      if Hex[x] in ['0'..'9'] then Result := (Result shl 4) + Ord(Hex[x]) - Ord('0')
      else if Hex[x] in ['A'..'F'] then Result := (Result shl 4) + Ord(Hex[x]) - Ord('A') + 10
      else begin
        Result := 0;
        Exit
      end;
  end;
begin
  if (ID > 0) and (ID <= FParamCount) and ((srv_paramstatus(pSrvProc, ID) and SRV_PARAMRETURN) <> 0) then
    if VarIsNull(v) then
      srv_paramsetoutput(pSrvProc, ID, nil, 0, True)
    else with FParams[Pred(ID)] do case DataType of
      ftInteger:
        begin
          PInteger(@Data[1])^ := v;
          srv_paramsetoutput(pSrvProc, ID, @Data[1], DataSize, FALSE);
        end;
      ftFloat: case DataSize of
        4: begin
          PSingle(@Data[1])^ := v;
          srv_paramsetoutput(pSrvProc, ID, @Data[1], 4, FALSE);
        end;
        8: begin
          PDouble(@Data[1])^ := v;
          srv_paramsetoutput(pSrvProc, ID, @Data[1], 8, FALSE);
        end;
      end;
      ftCurrency: begin
          PCurrency(@Data[1])^ := v;
          srv_paramsetoutput(pSrvProc, ID, @Data[1], SizeOf(Currency), FALSE);
      end;
      ftDateTime: begin
        if VarType(v) = varString then begin
          if Length(v)=0 then begin
            srv_paramsetoutput(pSrvProc, ID, nil, 0, True);
            Exit;
          end else
            v := StrToDateTime(v);
        end;
        case DataSize of
        4: begin
            DateTimeToSQLSmallDateTime(v, @Data[1]);
            srv_paramsetoutput(pSrvProc, ID, @Data[1], 4, FALSE);
           end;
        8: begin
            DateTimeToSQLDateTime(v, @Data[1]);
            srv_paramsetoutput(pSrvProc, ID, @Data[1], 8, FALSE);
           end;
        end;
        end;
      ftString:
        begin
          if VarType(v) = varString then
            Data := v
          else
            Data := VarToStr(v);
          srv_paramsetoutput(pSrvProc, ID, @Data[1], Length(Data), FALSE);
        end;
      ftBoolean:
        begin
          if v = True then
            PByte(@Data[1])^ := 1
          else
            PByte(@Data[1])^ := 0;
          srv_paramsetoutput(pSrvProc, ID, @Data[1], 1, FALSE);
        end;
      ftBCD:
        begin
          FloatToBcd(v,@Data[1]);
          srv_paramsetoutput(pSrvProc, ID, @Data[1], DataSize, FALSE);
        end;
      ftVarBytes:
        begin
          if VarType(v) = varInteger then begin
            SetLength(Data, len); if Len > DataSize then Len := DataSize;
            MoveMemory(@Data[1], Pointer(Integer(v)), len);
          end else if VarType(v) = varString then begin
            Len := (Length(v) + 1) div 2; if Len > DataSize then Len := DataSize;
            for i := 1 to Len do
              PByte(@Data[i])^ := Hex2Dec(Copy(v, (i shl 1)-1, 2));
          end;

          srv_paramsetoutput(pSrvProc, ID, @Data[1], Len, False);
        end;
      end;
//  end;
end;

procedure TXProc.DateTimeToSQLSmallDateTime(dt: TDateTime; sqldt: PDBDATETIM4);
begin
  sqldt.numdays := DaysBetween(EncodeDate(1900,1,1),dt);
  sqldt.nummins := MinutesBetween(RecodeTime(dt,0,0,0,0),dt);
end;

procedure TXProc.DateTimeToSQLDateTime(dt: TDateTime; sqldt: PDBDATETIME);
begin
  sqldt.dtdays := DaysBetween(EncodeDate(1900,1,1),dt);
  sqldt.dttime := SecondsBetween(RecodeTime(dt,0,0,0,0),dt)*300;
end;

procedure TXProc.FloatToBcd(f: Extended; bcd: PDBNUMERIC);
var
  i,j,mult: Integer;
begin
  if f<0.0 then begin bcd.sign := 0; f := -f end else bcd.sign := 1;
  PInt(@bcd.val)^ := Trunc(f);
  f := Frac(f);
  for i := SizeOf(Integer) to MAXNUMERICLEN-1 do bcd.val[i] := 0;
  for i := bcd.scale downto 1 do begin
    f := f * 10.0;
    if i>1 then mult := Trunc(f) else mult := Round(f);
    f := Frac(f);
    for j := 0 to MAXNUMERICLEN-1 do begin
      mult := bcd.val[j] * 10 + mult;
      bcd.val[j] := mult mod 256;
      mult := mult div 256;
    end;
  end;
end;

procedure TXProc.CreateField(ID: Byte; FieldName: String; ft: TFieldType; len: Word = 0; precision: Byte = 0; scale: Byte = 0);
var
  sqltype: Integer;
begin
  if (ID = 0) or (ID > XP_MAX_FIELDS) or (ID <> Succ(FFieldCount)) or (FieldName = '') or (not (ft in [ftBoolean, ftCurrency, ftBcd]) and (len = 0)) then
    RaisError('Invalid CreateField parameters.')
  else begin
    Inc(FFieldCount);
    SetLength(FFields, FFieldCount);
    with FFields[Pred(FFieldCount)] do begin
      Name := FieldName;
      DataType := ft;
      case ft of
        ftString: sqltype := SRVBIGVARCHAR;
        ftVarBytes: sqltype := SRVBIGVARBINARY;
        ftSmallInt, ftWord: begin sqltype := SRVINTN; len := sizeof(SmallInt) end;
        ftInteger: sqltype := SRVINTN;
        ftLargeint: begin sqltype := SRVINT8; len := sizeof(Int64) end;
        ftFloat: sqltype := SRVFLTN;
        ftCurrency: begin sqltype:= SRVMONEYN; len := 8 end;
        ftDate, ftTime: begin sqltype := SRVDATETIMN; len := 8 end;
        ftDateTime: sqltype := SRVDATETIMN;
        ftBoolean: begin sqltype := SRVBITN; len := 1; end;
  //      ftBcd: begin sqltype := SRVNUMERICN; len := sizeof(DBNUMERIC); end;
        else begin
          RaisError('Invalid DataType parameter in CreateField procedure.');
          Exit
        end
      end;
      DataSize := len;
      SetLength(Data,len);
      srv_describe(pSrvProc, ID, PChar(name), SRV_NULLTERM,
        sqltype, len, sqltype, len, nil);
    end;
  end;
end;

function  TXProc.GetFieldName(ID: Byte): String;
begin
  if (ID = 0) or (ID > FFieldCount) then
    Result := ''
  else begin
    Result := FFields[Pred(ID)].Name
  end;
end;

procedure TXProc.SetField(ID: Byte; v: Variant);
begin
  if (ID > 0) and (ID <= FFieldCount) then
    if VarIsNull(v) then
      srv_setcoldata(pSrvProc, ID, nil)
    else with FFields[Pred(ID)] do begin
      case DataType of
        ftString, ftVarBytes:
          begin
            Data := VarToStr(v);
            srv_setcollen(pSrvProc, ID, Length(Data));
          end;
        ftWord:
          PWord(@Data[1])^ := v;
        ftSmallInt:
          PSmallInt(@Data[1])^ := v;
        ftInteger:
          PInteger(@Data[1])^ := v;
        ftLargeint:
          PInt64(@Data[1])^ := v;
        ftFloat:
          case DataSize of
            4: begin
              PSingle(@Data[1])^ := v;
            end;
            8: begin
              PDouble(@Data[1])^ := v;
            end;
          end;
        ftCurrency:
          PCurrency(@Data[1])^ := v;
{
        ftBcd:
          begin
            FloatToBcd(v,Pointer(@Data[1]));
            srv_setcoldata(pSrvProc, ID, @Data[1]);
          end;
}
        ftDateTime:
          case DataSize of
            4: DateTimeToSQLSmallDateTime(v, @Data[1]);
            8: DateTimeToSQLDateTime(v, @Data[1]);
          end;
        ftBoolean:
          begin
            if v = True then
              PByte(@Data[1])^ := 1
            else
              PByte(@Data[1])^ := 0;
          end;
        else
          Exit;
      end;
      srv_setcoldata(pSrvProc, ID, @Data[1]);
    end;
end;

end.
