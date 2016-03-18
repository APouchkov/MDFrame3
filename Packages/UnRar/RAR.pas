//  written by Philippe Wechsler 2008
//
//  web: www.PhilippeWechsler.ch
//  mail: contact@PhilippeWechsler.ch
//
//  please see license.txt and documentation.txt
//
//  changes in 1.2 stable
//   - support for delphi 2009
//   - support for unicode filenames (see TRARFileItem.FileNameW)
//   - dll name + path is custom
//   - fixed a memory leak (thanks to Claes Enskär)
//   - some small improvements in the demo
//  changes in 1.1 stable
//   - fixed problem with mySelf pointer - you can use now multiple TRAR instances
//   - "SFX" in archive informations
//   - code better commented
//   - bugfixing in reading multivolumes
//
//  known bugs:
//   - when extracting files that contains unicode characters there's no test if
//     the file exists allready
//   - open archives that contains unicode characters in the archive name fails

unit RAR;

interface

uses
  Classes, SysUtils, Windows, RAR_DLL, RegularExpressions;

type
  TRAROperation = (roInitArchive, roListFiles, roExtract, roTest);

type
  TRARProgressInfo = record
    FileBytesDone : LongInt;
    FileBytesTotal: LongInt;
    FileName      : WideString;
    TotalSize     : LongInt;
  end;

type
  TRARFileItem = record
    FileName        : AnsiString;
    FileNameW       : WideString;
    CompressedSize  : Cardinal;
    UnCompressedSize: Cardinal;
    HostOS          : String;
    CRC32           : String;
    Attributes      : Cardinal;
    Comment         : String;
    Time            : TDateTime;
    CompressionStrength : Cardinal;
    ArchiverVersion     : Cardinal;
    Encrypted       : Boolean;
  end;

type TRARReplaceData = record
  FileName  : String;
  Size      : Int64;
  Time      : TDateTime;
end;

TRARReplace = (rrCancel, rrOverwrite, rrSkip);

type
  TOnRARErrorNotifyEvent  = procedure(Sender: TObject; const ErrorCode:integer; const Operation: TRAROperation) of object;
  TOnRARListFile          = procedure(Sender: TObject; const FileInformation: TRARFileItem) of Object;
  TOnRARPasswordRequired  = procedure(Sender: TObject; const HeaderPassword: Boolean; const FileName: String; out NewPassword: String; out Cancel:Boolean) of object;
  TOnRARNextVolumeRequired= procedure(Sender: TObject; const RequiredFileName: String; out newFileName: String; out Cancel: Boolean) of Object;
  TOnRARProcess           = procedure(Sender: TObject; const FileName: WideString; const ArchiveBytesTotal, ArchiveBytesDone, FileBytesTotal, FileBytesDone: Cardinal) of Object;
  TOnRARReplace           = procedure(Sender: TObject; const ExistingData, NewData: TRARReplaceData; out Action:TRARReplace) of object;

type
  TRARArchiveInformation = class (TPersistent)
  private
    fOpened         : Boolean;
    fFileName       : String;
    fUseMask        : Boolean;
    fRegExpr        : TRegEx;
    fArchiverMajorVersion: Cardinal;
    fArchiverMinorVersion: Cardinal;
    fDictionarySize : Int64;
    fEncryption     : Boolean;
    fSolid          : Boolean;
    fHostOS         : String;
    fTotalFiles     : Integer;
    fCompressedSize   : Int64;
    fUnCompressedSize : Int64;
    fHeaderEncrypted: Boolean;
    fMultiVolume    : Boolean;
    fArchiveComment : Boolean;
    fFileComment    : Boolean;
    fComment        : String;
    fSigned         : Boolean;
    fLocked         : Boolean;
    fRecovery       : Boolean;
    fSFX            : Boolean;
    procedure Reset;
  protected
  public
  published
    property FileName: String read fFileName;
    property ArchiverMajorVersion: cardinal read fArchiverMajorVersion;
    property ArchiverMinorVersion: cardinal read fArchiverMinorVersion;
    property DictionarySize: int64 read fDictionarySize;
    property Encryption: Boolean read fEncryption;
    property Solid: Boolean read fSolid;
    property HostOS: String read fHostOS;
    property TotalFiles: integer read fTotalFiles;
    property CompressedSize: int64 read fCompressedSize;
    property UnCompressedSize: int64 read fUnCompressedSize;
    property HeaderEncrypted: Boolean read fHeaderEncrypted;
    property MultiVolume: Boolean read fMultiVolume;
    property ArchiveComment: Boolean read fArchiveComment;
    property FileComment: Boolean read fFileComment;
    property Comment: String read fComment;
    property Signed: Boolean read fSigned;
    property Locked: Boolean read fLocked;
    property Recovery: Boolean read fRecovery;
    property SFX: Boolean read fSFX;
  end;

type
  TRAR = class(TComponent)
  private
    RARDLLInstance:THandle;
    fAbort:Boolean;
    fProgressInfo:TRARProgressInfo;
    fReadMVToEnd:Boolean;
    fPackedSizeMVVolume:Cardinal;
    Password: String;
    Comment: PAnsiChar;
    CommentResult:Cardinal;
    fArchiveInformation   : TRARArchiveInformation;
    ArchiveData   : TRARArchiveDataEx;
    ArchiveHandle : Cardinal;
    hdrData : TRARHeaderDataEx;
    fDLLName: WideString;
    fOnError: TOnRARErrorNotifyEvent;
    fOnListFile           : TOnRARListFile;
    fOnPasswordRequired   : TOnRARPasswordRequired;
    fOnNextVolumeRequired : TOnRARNextVolumeRequired;
    fOnProcess  : TOnRARProcess;
    fOnReplace  : TOnRARReplace;
    MySelf: Pointer;
    FFilesList: TStrings;

    function InitArchive(Extract: Boolean):Boolean;
    function CloseArchive: Boolean;
    function OnUnRarCallBack(Msg: Cardinal; UserData, P1, P2:LongInt): Integer; stdcall;
    function GetVersion: String;

    procedure ProgressHeader;
    procedure Error(ErrorCode: Integer; Operation: TRAROperation);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OpenFile(const AFileName: String; const AMask: String = ''): Boolean;
    function List: Boolean;
    function Extract(Path: String; RestoreFolder: Boolean; AFiles: TStrings): Boolean;
    function Test: Boolean;
    procedure Abort;
    procedure LoadDLL;
    procedure UnloadDLL;
    function IsDLLLoaded  : Boolean;
    function GetDllVersion: Integer;
  published
    property Version: String read getVersion;
    property ReadMultiVolumeToEnd:Boolean read fReadMVToEnd write fReadMVToEnd; //if true, mv's will be read until last part of the file
    //pro:display correct crc + display all files in all parts
    //contra: all volumes required=to open you have to insert all disk if not all volumes in sam folder
    property DllName: WideString read fDLLName write fDLLName;
    property OnError: TOnRARErrorNotifyEvent read fOnError write fOnError;
    property OnListFile: TOnRARListFile read fOnListFile write fOnListFile;
    property OnPasswordRequired: TOnRARPasswordRequired read fOnPasswordRequired write fOnPasswordRequired;
    property OnNextVolumeRequired:TOnRARNextVolumeRequired read fOnNextVolumeRequired write fOnNextVolumeRequired;
    property OnProgress: TOnRARProcess read fOnProcess write fOnProcess;
    property OnReplace: TOnRARReplace read fOnReplace write fOnReplace;
    property ArchiveInformation:TRARArchiveInformation read fArchiveInformation;

    property FilesList: TStrings read FFilesList;
  end;

procedure Register;

implementation

uses
  IOUtils;

const
  fVersion='1.2';

procedure Register;
begin
  RegisterComponents('Philippe Wechsler', [TRAR]);
end;

function UnRarCallBack(msg:Cardinal; UserData, P1, P2:LongInt): integer; stdcall;
begin
  //Result:=TRAR(mySelf).OnUnRarCallBack(msg, UserData, P1, P2);
  Result := TRAR(UserData).OnUnRarCallBack(msg, UserData, P1, P2);
end;

function TRAR.OnUnRarCallBack(msg:Cardinal; UserData, P1, P2: LongInt):integer; stdcall;
var
  Password: String;
  PasswordFile, FileName: String;
  Cancel:Boolean;
begin
  Password := '';
  Cancel:=False;
  Result:=0;
  case msg of
    UCM_CHANGEVOLUME: begin
                        FileName := String(PAnsiChar(P1));
                        case P2 of
                          RAR_VOL_ASK:    begin
                                            if (not fArchiveInformation.fOpened) and (not fReadMVToEnd) then begin
                                              Result:=-1
                                            end else begin
                                              if assigned(fOnNextVolumeRequired) then
                                                fOnNextVolumeRequired(Self, String(PAnsiChar(P1)), FileName, Cancel);
                                              StrPCopy(PWideChar(P1), FileName); //todo: handle error if P1 has not enough space for FileName
                                              if fAbort or Cancel then
                                                Result:=-1
                                              else
                                                Result:=0;
                                            end;
                                          end;
                          RAR_VOL_NOTIFY: begin        //occurs when next volume required and next part was found
                                            Result:=0; //continue
                                          end;
                        end;

                      end;
    UCM_NEEDPASSWORD: begin
                        if not fArchiveInformation.fOpened then begin
                          fArchiveInformation.fHeaderEncrypted:=True;
                          PasswordFile := fArchiveInformation.FileName;
                        end else
                          PasswordFile := fProgressInfo.FileName;

                        if assigned(fOnPasswordRequired) then
                          fOnPasswordRequired(Self, not fArchiveInformation.fOpened, PasswordFile, Password, Cancel);

                        StrPCopy(Pointer(P1), Copy(Password, 1, P2));
                        if fAbort or Cancel then
                          Result := -1
                        else
                          Result := 0;
                      end;
    UCM_PROCESSDATA:  begin
                        fProgressInfo.FileBytesDone:=fProgressInfo.FileBytesDone+P2;
                        fProgressInfo.TotalSize:=fProgressInfo.TotalSize+P2;
                        if assigned(fOnProcess) then
                          fOnProcess(Self,fProgressInfo.FileName,
                          fArchiveInformation.UnCompressedSize, fProgressInfo.TotalSize,
                          fProgressInfo.FileBytesTotal,fProgressInfo.FileBytesDone);
                        if fAbort then
                          Result := -1
                        else
                          Result := 0;
                      end;
  end;
  if fAbort then
    Result:=-1;
end;

constructor TRAR.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fArchiveInformation := TRARArchiveInformation.Create;
  fReadMVToEnd        := False;
  mySelf              := Self;
  fDLLName            := 'unrar.dll';
  FFilesList          := TStringList.Create;
end;

destructor TRAR.Destroy;
begin
  if Assigned(comment) then FreeMem(comment);
  FreeAndNil(fArchiveInformation);
  FreeAndNil(FFilesList);

  UnLoadDLL;
  inherited Destroy;
end;

function TRAR.OpenFile(const AFileName: String; const AMask: String): Boolean;
  function FileMaskToRegExpr: String;
  var
    I: Integer;
    LChar: Char;
  begin
    Result := '';
    for I := 1 to Length(AMask) do begin
      LChar := AMask[I];
      if LChar = '*' then
        Result := Result + '.*'
      else if LChar = '?' then
        Result := Result + '.'
      else if LChar = '.' then
        Result := Result + '\.'
      else if LChar = #13 then
        Result := Result + '|'
      else if LChar = #10 then
      else
        Result := Result + LChar
    end;

  end;
begin
  fArchiveInformation.Reset;
  if not isDLLLoaded then
    LoadDLL;

  if not IsDLLLoaded then begin
    Error(ERAR_DLL_LOAD_ERROR, roInitArchive);
    Result := False;
    Exit;
  end;

  fArchiveInformation.fFileName := AFileName;
  if AMask.IsEmpty then
    fArchiveInformation.fUseMask := False
  else begin
    fArchiveInformation.fUseMask := True;
    fArchiveInformation.fRegExpr := TRegEx.Create(FileMaskToRegExpr);
  end;

  Result := List;
  fArchiveInformation.fOpened := True;
end;

function TRAR.InitArchive(Extract: Boolean): Boolean;
begin
  Result := True;
  CommentResult := RAR_SUCCESS;
  with ArchiveData do begin
    OpenResult := RAR_SUCCESS;
    if Extract then
      OpenMode := RAR_OM_EXTRACT
    else
      if fReadMVToEnd then
        OpenMode  := RAR_OM_LIST_INCSPLIT
      else
        OpenMode  := RAR_OM_LIST;
    ArcNameW := PWideChar(fArchiveInformation.FileName);

    if not Assigned(Comment) then
      GetMem(Comment, MAX_RAR_COMMENTSIZE);
    CmtBuf    := PAnsiChar(Comment);
    CmtBufSize:= MAX_RAR_COMMENTSIZE;
    CmtSize   := Length(Comment);
    CmtState  := CommentResult;
  end;

  ArchiveHandle := RAROpenArchiveEx(@ArchiveData);
  if ArchiveHandle = 0 then begin           //handle incorrect=failed to load dll
    Error(ERAR_DLL_LOAD_ERROR, roInitArchive);
    Result := False;
    Exit;
  end;
  //((ArchiveData.Flags and $00000100)=$00000100)=first volume
  //((ArchiveData.Flags and $00000001)=$00000001)=Volume attribute (archive volume)
  //((ArchiveData.Flags and $00000010)=$00000010)=New volume naming scheme ('volname.partN.rar')

  if ((ArchiveData.Flags and $00000004)=$00000004) then    //set archive info
    fArchiveInformation.fLocked := True;
  if ((ArchiveData.Flags and $00000020)=$00000020) then
    fArchiveInformation.fSigned := True;
  if ((ArchiveData.Flags and $00000040)=$00000040) then
    fArchiveInformation.fRecovery := True;
  if ((ArchiveData.Flags and $00000008)=$00000008) then
    fArchiveInformation.fSolid := True;
  if ((ArchiveData.Flags and $00000002)=$00000002) then
    fArchiveInformation.fArchiveComment := True;
  if ((ArchiveData.Flags and $00000080)=$00000080) then
    fArchiveInformation.fHeaderEncrypted := True;
  fArchiveInformation.fSFX := isSFX(fArchiveInformation.FileName);

  case ArchiveData.CmtState of                              //read archive comment
    ERAR_COMMENTS_EXISTS: begin
                            fArchiveInformation.fComment := String(Comment);
                            fArchiveInformation.fArchiveComment:=True;
                          end;
    ERAR_NO_COMMENTS:     begin
                            fArchiveInformation.fComment := '';
                            fArchiveInformation.fArchiveComment := False;
                          end;
    ERAR_NO_MEMORY:       Error(ERAR_NO_MEMORY,roInitArchive);
    ERAR_BAD_DATA:        Error(ERAR_BAD_DATA,roInitArchive);
    ERAR_UNKNOWN_FORMAT:  Error(ERAR_UNKNOWN_FORMAT,roInitArchive);
    ERAR_SMALL_BUF:       Error(ERAR_SMALL_BUF,roInitArchive);
  end;
  if (ArchiveData.CmtState<>ERAR_NO_COMMENTS) and (ArchiveData.CmtState<>ERAR_COMMENTS_EXISTS) then
    result:=False;      //error reading comment
end;

function TRAR.CloseArchive: Boolean;
var
  CloseResult: Integer;
begin
  CloseResult := RARCloseArchive(ArchiveHandle);
  if CloseResult = ERAR_ECLOSE then
    Error(ERAR_ECLOSE, roInitArchive);
  Result := (CloseResult = RAR_SUCCESS);
end;

procedure TRAR.ProgressHeader;     //write data to archiv information and  list file
var
  FileItem: TRARFileItem;
  ft: _FILETIME;
  st: TSystemTime;
  OS: String;
begin
  if (fReadMVToEnd) and (not ((hdrData.Flags and $00000001)=$00000001)) and  //first part of the file
    (((hdrData.Flags and $00000002)=$00000002))
  then
    fPackedSizeMVVolume:=hdrData.PackSize;

  if (fReadMVToEnd) and (((hdrData.Flags and $00000001)=$00000001)) and  //not last, not first part
    (((hdrData.Flags and $00000002)=$00000002)) then begin
    fPackedSizeMVVolume:=fPackedSizeMVVolume+hdrData.PackSize;
    exit;
  end;

  if (fReadMVToEnd) and (((hdrData.Flags and $00000001)=$00000001)) and  //last part
    (not ((hdrData.Flags and $00000002)=$00000002))
  then
    hdrData.PackSize:=hdrData.PackSize+fPackedSizeMVVolume;

  if (fReadMVToEnd) and ((hdrData.Flags and $00000002)=$00000002) then //not last part
    exit;

  if fArchiveInformation.fArchiverMajorVersion*10+fArchiveInformation.fArchiverMinorVersion<hdrData.UnpVer then begin
    fArchiveInformation.fArchiverMinorVersion:=hdrData.UnpVer mod 10;
    fArchiveInformation.fArchiverMajorVersion:=(hdrData.UnpVer-fArchiveInformation.fArchiverMinorVersion) div 10;
  end;

  if ((hdrData.Flags and $00000004)=$00000004) then
    fArchiveInformation.fEncryption := True;

  if ((hdrData.Flags and $00000010)=$00000010) then
    fArchiveInformation.fSolid := True;

  OS := 'unknown';
  case hdrData.HostOS of
    0: OS := 'DOS';
    1: OS := 'IBM OS/2';
    2: OS := 'Windows';
    3: OS := 'Unix';
  end;

  fArchiveInformation.fHostOS := OS;

  if (not ((hdrData.Flags and $00000070)=$00000070)) and (hdrData.FileAttr<>faDirectory) then begin//not a directory
    fArchiveInformation.fTotalFiles := fArchiveInformation.fTotalFiles+1;
    case (hdrData.Flags shl 24 shr 29) of
      0: fArchiveInformation.fDictionarySize:=65536;
      1: fArchiveInformation.fDictionarySize:=131072;
      2: fArchiveInformation.fDictionarySize:=262144;
      3: fArchiveInformation.fDictionarySize:=524288;
      4: fArchiveInformation.fDictionarySize:=1048576;
      5: fArchiveInformation.fDictionarySize:=2097152;
      6: fArchiveInformation.fDictionarySize:=4194304;
    end;
  end;

  fArchiveInformation.fCompressedSize  := fArchiveInformation.fCompressedSize+hdrData.PackSize;
  fArchiveInformation.fUnCompressedSize:= fArchiveInformation.fUnCompressedSize+hdrData.UnpSize;
  if ((hdrData.Flags and $00000001)=$00000001) or ((hdrData.Flags and $00000002)=$00000002) then    //file continued in last or next part
    fArchiveInformation.fMultiVolume := True;

  if hdrData.CmtSize>0 then
    fArchiveInformation.fFileComment := True;
  
  with FileItem do begin
    FileName  := hdrData.FileName;
    FileNameW := hdrData.FileNameW;
    CompressedSize  := hdrData.PackSize;
    UnCompressedSize:= hdrData.UnpSize;
    HostOS := OS;
    CRC32  := Format('%x',[hdrData.FileCRC]);
    Attributes  := hdrData.FileAttr;
    Comment     := String(hdrData.CmtBuf);
    DosDateTimeToFileTime(HiWord(hdrData.FileTime), LoWord(hdrData.FileTime), ft);
    FileTimeToSystemTime(ft, st);
    Time := SystemTimeToDateTime(st);

    CompressionStrength := hdrData.Method;
    ArchiverVersion     := hdrData.UnpVer;

    Encrypted := ((hdrData.Flags and $00000004) = $00000004);
  end;

  if (not fArchiveInformation.fUseMask) or fArchiveInformation.fRegExpr.IsMatch(FileItem.FileNameW) then
    FFilesList.Add(FileItem.FileNameW);

  if Assigned(fOnListFile) then
    fOnListFile(Self, FileItem);
end;

procedure TRARArchiveInformation.Reset;
begin
  fOpened:=False;

  fFileName:='';
  fTotalFiles:=0;
  fArchiverMajorVersion:=0;
  fArchiverMinorVersion:=0;
  fDictionarySize:=0;
  fEncryption:=False;
  fSolid:=False;
  fHostOS:='';
  fTotalFiles:=0;
  fCompressedSize:=0;
  fUnCompressedSize:=0;
  fHeaderEncrypted:=False;
  fMultiVolume:=False;
  fArchiveComment:=False;
  fFileComment:=False;
  fComment:='';
  fLocked:=False;
  fSigned:=False;
  fRecovery:=False;
  fSFX:=False;
end;

function TRAR.List: Boolean;
var
  ReadFileHeaderResult: Integer;
begin
  FFilesList.Clear;
  Assert(FileExists(fArchiveInformation.FileName));

  fAbort := False;
  Result := InitArchive(False);
  if fAbort or (not Result) then Exit;

  try
    mySelf := Self;
    RARSetCallback(Archivehandle, UnRarCallBack, Integer(mySelf));
    if not Password.IsEmpty then
      RARSetPassword(ArchiveHandle, PAnsiChar(AnsiString(Password)));

    ReadFileHeaderResult := RAR_SUCCESS;
    while (ReadFileHeaderResult = RAR_SUCCESS) and Result do begin
      ReadFileHeaderResult := RARReadHeaderEx(ArchiveHandle, @hdrData);

      if ReadFileHeaderResult = ERAR_END_ARCHIVE then Break;
      if ReadFileHeaderResult <> RAR_SUCCESS then Result := False;
      Error(ReadFileHeaderResult, roListFiles);

      ProgressHeader; //fOnListFile + writte data to farchiveInformation
      ReadFileHeaderResult := RARProcessFileW(ArchiveHandle, RAR_SKIP, nil, nil);

      if ReadFileHeaderResult <> RAR_SUCCESS then
        Result := False;
      Error(ReadFileHeaderResult, roListFiles);
    end;
  finally
    CloseArchive;
  end;
end;

function ExtractFile(FileName: String; Files: TStrings): Boolean;   //returns if the actual file should be extracted or not
var
  i:integer;
begin
  if Files = NIL then
    Result := True
  else
    begin
      Result := False;
      for i := 0 to Files.Count - 1 do          //check if actual file is in the filelist
        if Files[i] = FileName then begin
          Result := True;
          break;
        end;
    end;
end;

function TRAR.Extract(Path: String; RestoreFolder: Boolean; AFiles: TStrings): Boolean;
var
  ReadFileHeaderResult: integer;
  ExistentFile, ArchiveFile: TRARReplaceData;
  ft: _FILETIME;
  st: TSystemTime;
  ReplaceResult: TRARReplace;
begin
  Assert(FileExists(fArchiveInformation.FileName));
  fAbort  := False;
  Result  := InitArchive(True);

  if fAbort or not (Result) then
    Exit;

  Path := IncludeTrailingPathDelimiter(Path);
  try
    mySelf := Self;
    RARSetCallback(Archivehandle, UnRarCallBack, Integer(mySelf));
    if not Password.IsEmpty then
      RARSetPassword(ArchiveHandle, PAnsiChar(AnsiString(Password)));

    readFileHeaderResult := RAR_SUCCESS;
    fProgressInfo.TotalSize := 0;

    while (ReadFileHeaderResult=RAR_SUCCESS) and Result do begin
      ReadFileHeaderResult := RARReadHeaderEx(ArchiveHandle,@hdrData);

      if ReadFileHeaderResult=ERAR_END_ARCHIVE then
        Break;

      if ReadFileHeaderResult <> RAR_SUCCESS then begin
        Result:=False;
        Error(ReadFileHeaderResult, roListFiles);
      end;

      fProgressInfo.FileBytesDone:=0;
      fProgressinfo.FileBytesTotal:=hdrData.UnpSize;
      fProgressInfo.FileName:=hdrData.FileNameW;
      ReplaceResult := rrOverWrite;

      if ExtractFile(StrPas(hdrData.FileNameW), AFiles) then begin    //todo: UniCode FileName
        if RestoreFolder then
          ExistentFile.FileName := Path + StrPas(hdrData.FileNameW)
        else
          ExistentFile.FileName := Path + ExtractFileName(StrPas(hdrData.FileNameW));

        if RestoreFolder then
          ArchiveFile.FileName := StrPas(hdrData.FileNameW)
        else
          ArchiveFile.FileName := ExtractFileName(StrPas(hdrData.FileNameW));

        ArchiveFile.Size := hdrData.UnpSize;
        DosDateTimeToFileTime(HiWord(hdrData.FileTime), LoWord(hdrData.FileTime), ft);
        FileTimeToSystemTime(ft, st);
        ArchiveFile.Time := SystemTimeToDateTime(st);

        if FileExists(ExistentFile.FileName) then begin
          if Assigned(fOnReplace) then begin
            ExistentFile.Size := GetFileSize(ExistentFile.FileName);
            ExistentFile.Time := TFile.GetLastWriteTime(ExistentFile.FileName);

            fOnReplace(Self, ExistentFile, ArchiveFile, ReplaceResult);
          end;
        end;

        case ReplaceResult of
          rrCancel    : fAbort:=True;
          rrOverwrite : if RestoreFolder then
                        ReadFileHeaderResult := RARProcessFileW(ArchiveHandle, RAR_EXTRACT, PWideChar(Path), NIL)
                      else
                        if (not ((hdrData.Flags and $00000070)=$00000070)) and (hdrData.FileAttr<>faDirectory) then
                          ReadFileHeaderResult := RARProcessFileW(ArchiveHandle, RAR_EXTRACT, nil, PWideChar(ExistentFile.FileName));
          rrSkip      : begin
                          ReadFileHeaderResult := RARProcessFileW(ArchiveHandle, RAR_SKIP, PWideChar(Path), NIL);
                          {$WARN COMBINING_SIGNED_UNSIGNED OFF}
                          fProgressInfo.FileBytesDone:=fProgressInfo.FileBytesDone+hdrData.UnpSize;
                          {$WARN COMBINING_SIGNED_UNSIGNED ON}
                        end;
        end;

      end else
        ReadFileHeaderResult := RARProcessFileW(ArchiveHandle, RAR_SKIP, nil, nil); //select next file without extracting

      if ReadFileHeaderResult <> RAR_SUCCESS then begin
        Result:=False;
        Error(ReadFileHeaderResult, roListFiles);
      end;

      if fAbort then
        Result:=False;
    end;
  finally
    CloseArchive;
  end;

  if fAbort then
    Result := False;
end;

function TRAR.Test:Boolean;
var
  ReadFileHeaderResult: integer;
begin
  assert(FileExists(fArchiveInformation.FileName));
  fAbort:=False;
  Result:=InitArchive(True);
  if fAbort or (not Result) then exit;
  try
    mySelf:=Self;
    RARSetCallback(Archivehandle, UnRarCallBack, Integer(mySelf));
    if not Password.IsEmpty then
      RARSetPassword(ArchiveHandle, PAnsiChar(AnsiString(Password)));
    fProgressInfo.TotalSize := 0;
    readFileHeaderResult := RAR_SUCCESS;
    while (ReadFileHeaderResult=RAR_SUCCESS) and Result do begin
      ReadFileHeaderResult:=RARReadHeaderEx(ArchiveHandle,@hdrData);

      if ReadFileHeaderResult=ERAR_END_ARCHIVE then
        break;

      if ReadFileHeaderResult<>RAR_SUCCESS then begin
        Result:=False;
        Error(ReadFileHeaderResult, roListFiles);
      end;

      fProgressInfo.FileBytesDone:=0;
      fProgressinfo.FileBytesTotal:=hdrData.UnpSize;
      fProgressInfo.FileName:=hdrData.FileNameW;

      ReadFileHeaderResult := RARProcessFileW(ArchiveHandle, RAR_TEST, NIL, NIL);

      if ReadFileHeaderResult<>RAR_SUCCESS then begin
        Result:=False;
        Error(ReadFileHeaderResult, roListFiles);
      end;

      if fAbort then
        Result:=False;
    end;
  finally
    CloseArchive;
  end;
end;

procedure TRAR.LoadDLL;
begin
  RARDLLInstance := LoadLibraryW(PWideChar(fDLLName));
  if RARDLLInstance <> 0 then begin
    DllLoaded := True;
//    @RAROpenArchive := GetProcAddress(RARDLLInstance, 'RAROpenArchive');
    @RAROpenArchiveEx       := GetProcAddress(RARDLLInstance, 'RAROpenArchiveEx');
    @RARCloseArchive        := GetProcAddress(RARDLLInstance, 'RARCloseArchive');
//    @RARReadHeader := GetProcAddress(RARDLLInstance, 'RARReadHeader');
    @RARReadHeaderEx        := GetProcAddress(RARDLLInstance, 'RARReadHeaderEx');
//    @RARProcessFile := GetProcAddress(RARDLLInstance, 'RARProcessFile');
    @RARProcessFileW        := GetProcAddress(RARDLLInstance, 'RARProcessFileW');
    @RARSetCallback         := GetProcAddress(RARDLLInstance, 'RARSetCallback');
    @RARSetChangeVolProc    := GetProcAddress(RARDLLInstance, 'RARSetChangeVolProc');
    @RARSetProcessDataProc  := GetProcAddress(RARDLLInstance, 'RARSetProcessDataProc');
    @RARSetPassword         := GetProcAddress(RARDLLInstance, 'RARSetPassword');
    @RARGetDllVersion       := GetProcAddress(RARDLLInstance, 'RARGetDllVersion');
    if
      // (@RAROpenArchive = nil) or
      (@RAROpenArchiveEx = nil) or (@RARCloseArchive = nil)
    // or (@RARReadHeader = nil)
    or (@RARReadHeaderEx = nil)
    //or (@RARProcessFile = nil)
    or (@RARProcessFileW = nil)
    or (@RARSetCallback = nil) or (@RARSetChangeVolProc = nil) or (@RARSetProcessDataProc = nil)
    or (@RARSetPassword = nil) or (@RARGetDllVersion = nil) then begin
      RARDLLInstance:=0;
      UnloadDLL;
    end;

    if RARGetDllVersion < MIN_RAR_VERSION then
      MessageBox(0, 'please download the newest "unrar.dll" file. See www.rarlabs.com', 'error', 0);
  end;
end;

procedure TRAR.UnloadDLL;
begin
  if DllLoaded then begin
    FreeLibrary(RARDLLInstance);
    RARDLLInstance := 0;
  end;
end;

function TRAR.isDLLLoaded: Boolean;
begin
  Result:=RARDLLInstance<>0;
end;

function TRAR.GetDllVersion:integer;
begin
  if not isDLLLoaded then
    LoadDLL;
  if not isDLLLoaded then begin
    Error(ERAR_DLL_LOAD_ERROR,roInitArchive);
    Result:=0;
    Exit;
  end;
  Result:=RARGetDllVersion;
end;

procedure TRAR.Abort;
begin
  fAbort:=True;
end;

procedure TRAR.Error(ErrorCode:integer;Operation:TRAROperation);
begin
  if (ErrorCode=ERAR_DLL_LOAD_ERROR) or
     //(ErrorCode=ERAR_END_ARCHIVE) or
     (ErrorCode=ERAR_NO_MEMORY) or
     (ErrorCode=ERAR_BAD_DATA) or
     (ErrorCode=ERAR_UNKNOWN_FORMAT) or
     (ErrorCode=ERAR_EOPEN) or
     (ErrorCode=ERAR_ECREATE) or
     (ErrorCode=ERAR_ECLOSE) or
     (ErrorCode=ERAR_EREAD) or
     (ErrorCode=ERAR_EWRITE) or
     (ErrorCode=ERAR_SMALL_BUF) or
     (ErrorCode=ERAR_UNKNOWN) then
    fAbort := True;

  if Assigned(fOnError) then
    fOnError(Self, ErrorCode, Operation);
end;

function TRAR.getVersion:String;
begin
  result:=fVersion;
end;

end.
