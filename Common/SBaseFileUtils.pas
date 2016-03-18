unit SBaseFileUtils;
{$I config.inc}

interface

uses
  SysUtils, Dialogs, Windows, Forms, WinInet, Classes, IOUtils, FileCtrl, Math,
  Winapi.ShellAPI, Data.DB
{$IFDEF UNZIP}
  , VCLUnZip
{$ENDIF}
{$IFDEF UNRAR}
  , Rar
{$ENDIF}
  , RegularExpressions
{$IFDEF BASEFORM_SUPPORT}
  , SBaseFormDefinitions, SBaseForm, SBaseControls
{$ENDIF}

  , NamedVariables
{$IFNDEF PACKAGE}
  , Excel_TLB, Variants, ComObj
  , RxVerInf, FileUtil
{$ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, fs_idbrtti, fs_itools
{$ENDIF}
  ;

const
  SFileExt_Doc: String  = 'doc';
  SFileExt_Frp: String  = 'frp';
  SFileExt_Frf: String  = 'frf';
  SFileExt_Fp3: String  = 'fp3';
  SFileExt_Fr3: String  = 'fr3';
  SFileExt_Xls: String  = 'xls';
  SFileExt_Xlsx: String = 'xlsx';
  SFileExt_Txt: String  = 'txt';
  SFileExt_Xml: String  = 'xml';
  SFileExt_Xsl: String  = 'xsl';
  SFileExt_Dbf: String  = 'dbf';
  SFileExt_Html: String = 'html';
  SFileMask_All: String = '*.*';

  SFileExt_Zip: String  = 'zip';
  SFileExt_Rar: String  = 'rar';

  procedure SetApplicationExeName(AExeName: String);

  function  ExtractFileExt(const AFileName: String): String;
  function  GetTempFile(AExtension: String = ''): String;
  function  GetTempPath: String;
{$IFNDEF PACKAGE}
  function  ClearDirectory(const ADirectory: String; ADeleteSelf: Boolean = True): Boolean;
{$ENDIF}

  function  CompileFilePathByDate(const AOldPath: String; AMask: String; ADateTime: TDateTime): String;
  function  FinalizeFilePath(const AFilePath: String; const ADefaultPath: String = ''): String;
  function  PrepareFileName(const AFileName: String; const AExt: String = ''): String;

{$IFNDEF PACKAGE}
  function  ExcelFileReSave(const AFileName: String): String;
{$ENDIF}

  function  GetFileList(APath: String; AMask: String): TArray<String>;
  procedure SaveTextFile(const AFileName: String; const AText: String; AEncoding: TEncoding = nil);
  function  ReadTextFile(const AFileName: String): String;
  procedure ReadFile(const AFileName: String; var AData: TBytes); overload;
  procedure DownloadInternetFile(const AFileName, URL: String);

  procedure ShellExecute(const ACommandText: String; const AParams: String = '');
  procedure OpenFileWithShell(AFileName: String); inline;
  procedure OpenDataWithShell(const ABuffer: TBytes; AFileName: String); overload;
  procedure OpenDataWithShell(const AField: TField; AFileName: String); overload;
  procedure OpenDataWithShell(const AParam: TParam; AFileName: String); overload;

{$IFDEF ARCHIVES}
  function  ExtractFiles(const AArchName: String; const AFileMask: String = ''; const ADestDir: String = ''): String;
  function  ExtractFileIfNeeded(const AFileName: String; const AFileMask: String = ''; const ADestDir: String = ''): String;
{$ENDIF}

type
  TFileTypesComponent = class(TComponent)
  private
    FItems: TFileTypeItems;
    procedure SetItems(const Value: TFileTypeItems);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TFileTypeItems read FItems write SetItems;
  end;

  TFindResult = (frFound, frNotFound, frMultiple);

  TFilePath = packed record
    FFilePath: String;
    FURLComponents: TURLComponents;
  public
    constructor Create(const AFilePath: String);
    procedure Extract(const AFilePath: String);

    function URL: String;
    function Protocol: TInternetScheme;
    function Scheme: String;
    function Host: String;
    function Port: INTERNET_PORT;
    function Path: String;
    function FileName: String;
//    function IsFile: String;
    function IsInternetUrl: Boolean;
  end;

{$IFDEF BASEFORM_SUPPORT}
  TSelectDirectoryDlg = class(TComponent, ISBaseInterface)
//  public
//    class function CreateByParams(AOwner: TComponent; AParams: TNamedVariants): TComponent;
  published
    {$IFNDEF REPORT_EXTERNAL}
    { Реализация интерфейса ICreateByParams. Пропустить не глядя. }
    function  IVirtualClassOwner(var AVirtualClassName: String; var AParams: TNamedVariants): TPersistentClass;
    function  ICreateByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants): TComponent;
    function  ISelectByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants; ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;

    function  IGetCreateParams: TNamedVariants;
    procedure IShow;
    {$ENDIF}
  end;
  TSelectDirectoryDlgClass = class of TSelectDirectoryDlg;
{$ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
type
  TFunctions_SBaseFileUtils = class(TfsRTTIModule)
  private
    function Call_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

var
  ApplicationParams   : Array of String;
  ApplicationDir      : String;
  ApplicationExeName  : String;
  TEMPLATES_DIR       : String;
  TEMP_DIR            : String;
{$IFDEF ARCHIVES}
  ArchivesFileTypeItems : TFileTypeItems = nil;
{$ENDIF}
  ArchivesFileTypes     : TFileTypesComponent;

implementation

uses
  SBaseVariantFunctions, SBaseStringFunctions, SBaseConstants;

const
  ReservedFileNameChars: Set Of AnsiChar = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];

var
  FArchivesExtensions: String = '';

procedure GetApplicationParams;
var
  LItem: TStringItem;
  LIsApplication: Boolean;
begin
  LIsApplication := False;
  SetLength(ApplicationParams, 0);
  for LItem in EnumStringItems(String(GetCommandLine), ' ', True) do
    if LIsApplication then begin
      SetApplicationExeName(LItem.Value);
      LIsApplication := False;
    end else if SameText(LItem.Value, '/' + SConst_Application) then
      LIsApplication := True
    else begin
      SetLength(ApplicationParams, Succ(LItem.Index));
      ApplicationParams[LItem.Index] := LItem.Value;
    end;
end;

procedure SetApplicationExeName(AExeName: String);
begin
  ApplicationExeName  := AExeName;
  ApplicationDir      := IncludeTrailingPathDelimiter(ExtractFilePath(ApplicationExeName));
  TEMPLATES_DIR       := ApplicationDir + 'Templates\';
end;

function ExtractFileExt(const AFileName: String): String;
begin
  Result := SysUtils.ExtractFileExt(AFileName);
  if (Length(Result) > 0) and (Result[1] = '.') then
    Delete(Result, 1, 1);
end;

function GetTempFile(AExtension: String = ''): String;
var
  LFileName: String;
begin
  if not SysUtils.DirectoryExists(TEMP_DIR) then
    SysUtils.ForceDirectories(TEMP_DIR);

  if (AExtension <> '') and (AExtension[1] <> '.') then
    AExtension := '.' + AExtension;

  repeat
    Result := TEMP_DIR + #0;
    SetLength(Result, MAX_PATH);
    GetTempFileName(@Result[1], '~', 0, @Result[1]);
    SetLength(Result, Pos(#0, Result) - 1);

    if AExtension <> '' then begin
      LFileName := ChangeFileExt(Result, AExtension);
      RenameFile(Result, LFileName);
      Result := LFileName;
    end;
  until FileExists(Result);
end;

function GetTempPath: String;
var
  LFileName: String;
begin
  if not SysUtils.DirectoryExists(TEMP_DIR) then
    SysUtils.ForceDirectories(TEMP_DIR);

  repeat
    Result := TEMP_DIR + #0;
    SetLength(Result, MAX_PATH);
    GetTempFileName(@Result[1], '#', 0, @Result[1]);
    SetLength(Result, Pos(#0, Result) - 1);

    LFileName := ChangeFileExt(Result, '');
    TFile.Delete(Result);
    Result := LFileName;
    SysUtils.ForceDirectories(Result);
  until SysUtils.DirectoryExists(Result);

  Result := IncludeTrailingPathDelimiter(Result);
end;

{$IFNDEF PACKAGE}
{$WARN SYMBOL_DEPRECATED OFF}
function ClearDirectory(const ADirectory: String; ADeleteSelf: Boolean): Boolean;
const
{$IFDEF WIN32}
  FileNotFound = 18;
{$ELSE}
  FileNotFound = -18;
{$ENDIF}
var
  LFileInfo: TSearchRec;
  LDosCode: Integer;
  LDirectory: String;
begin
  Result := DirExists(ADirectory);
  if not Result then Exit;

  LDirectory := NormalDir(ADirectory);
  LDosCode := SysUtils.FindFirst(LDirectory + SFileMask_All, faAnyFile, LFileInfo);
  try
    while LDosCode = 0 do begin
//      if (FileInfo.Name[1] <> '.') and (FileInfo.Attr <> faVolumeID) then
//      !!! BUG !!!

      if (LFileInfo.Name <> '.') and (LFileInfo.Name <> '..') and (LFileInfo.Attr <> faVolumeID) then begin
        if (LFileInfo.Attr and faDirectory = faDirectory) then
          Result := ClearDirectory(LDirectory + LFileInfo.Name, True) and Result
        else //if (FileInfo.Attr and faVolumeID <> faVolumeID) then
        begin
          if (LFileInfo.Attr and SysUtils.faReadOnly = SysUtils.faReadOnly) then
            FileSetAttr(LDirectory + LFileInfo.Name, faArchive);
          try
            Result := SysUtils.DeleteFile(LDirectory + LFileInfo.Name) and Result;
          except
            Result := True;
          end;
        end;
      end;
      LDosCode := SysUtils.FindNext(LFileInfo);
    end;
  finally
    SysUtils.FindClose(LFileInfo);
  end;

  if ADeleteSelf and Result and (LDosCode = FileNotFound) and
    not ((Length(ADirectory) = 2) and (ADirectory[2] = ':'))
  then begin
    try
      RmDir(ADirectory);
    except
      Result := False;
    end;
    // Result := Result and (IOResult = 0);
  end;
end;
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}

procedure ShellExecute(const ACommandText: String; const AParams: String);
begin
  Winapi.ShellAPI.ShellExecute(0, PWideChar(SConst_Open), PWideChar(ACommandText), PWideChar(AParams), '', SW_SHOWNORMAL);
end;

procedure OpenFileWithShell(AFileName: String);
begin
  ShellExecute(AFileName);
end;

procedure OpenDataWithShell(const ABuffer: TBytes; AFileName: string);
var
  LFileStream: TFileStream;
  LFileName: string;
begin
  LFileName := GetTempFile(ExtractFileExt(AfileName));
  LFileStream := TFileStream.Create(LFileName, fmOpenWrite);
  try
    LFileStream.Write(ABuffer, Length(ABuffer));
  finally
    LFileStream.Free;
  end;
  OpenFileWithShell(LFileName);
end;

procedure OpenDataWithShell(const AField: TField; AFileName: string);
begin
  OpenDataWithShell(AField.AsBytes, AFileName);
end;

procedure OpenDataWithShell(const AParam: TParam; AFileName: string);
begin
  OpenDataWithShell(AParam.AsBytes, AFileName);
end;

function CompileFilePathByDate(const AOldPath: String; AMask: String; ADateTime: TDateTime): String;
var
  i, len: Integer;
begin
  Result := AOldPath;
  if AMask <> '' then begin
    repeat
      i := Pos('%', AMask);
      if i > 0 then begin
        Inc(i); len := 0;
        while (i + len <= length(AMask)) and CharInSet(AMask[i + len], ['d', 'm', 'y']) do Inc(len);
        AMask := Copy(AMask, 1, i-2) + FormatDateTime(Copy(AMask, i, Len), ADateTime) + Copy(AMask, i + len, Length(AMask) - i - len + 1)
      end;
    until i = 0;
    if (Length(AMask) > 2) and ( ((AMask[1] = '\') and (AMask[2] = '\')) or ((AMask[2] = ':') and (AMask[3] = '\')) ) then
      Result := AMask
    else begin
      if AMask[1] = '\' then
        AMask := Copy(AMask, 2, Length(AMask) - 1);
      len := 1;
      for i := Length(AMask) downto 1 do
        if AMask[i] = '\' then
          Inc(len);

      Result := AMask;
      for i := Length(AOldPath) downto 1 do
        if AOldPath[i] = '\' then begin
          Dec(len);
          if len = 0 then begin
            Result := Copy(AOldPath, 1, i) + AMask;
            Exit
          end;
        end;
    end;
  end
end;

function FinalizeFilePath(const AFilePath: String; const ADefaultPath: String): String;
begin
  if (Length(AFilePath) > 0) and CharInSet(AFilePath[1], ['/', '\']) then
    Result := AFilePath
  else if (Length(AFilePath) >= 2) and (AFilePath[2] = ':') then
    Result := AFilePath
  else if not ADefaultPath.IsEmpty then
    Result := ADefaultPath + AFilePath
  else
    Result := ApplicationDir + AFilePath
end;

function PrepareFileName(const AFileName: String; const AExt: String = ''): String;
const
  ReplaceChar: Char = ' ';
var
  SkipRezervedChar: Boolean;
  I, J: Integer;
begin
  SkipRezervedChar := False;
  J := 1;

  Result := AFileName;
  for I := 1 to Length(Result) do
    if CharInSet(Result[I], ReservedFileNameChars) then begin
      if not SkipRezervedChar then begin
        Result[I] := ReplaceChar;
        SkipRezervedChar := True;
      end
    end else begin
      Result[J] := Result[I];
      Inc(J);
      SkipRezervedChar := (Result[I] = ReplaceChar);
    end;

  if (J < Length(Result)) or SkipRezervedChar then
    SetLength(Result, J - IfThen(SkipRezervedChar, 1, 0));

  if (AExt <> '') and (ExtractFileExt(AFileName) <> AExt) then
    Result := Result + '.' + AExt;
end;

{$IFNDEF PACKAGE}
function ExcelFileReSave(const AFileName: String): String;
var
  IExcel: Excel_TLB._Application;
  IWorkbook: Excel_TLB._Workbook;
begin
  Result := GetTempFile(SFileExt_Xls);

  IWorkBook := nil;
  IExcel := CreateComObject(Excel_TLB.CLASS_ExcelApplication) as Excel_TLB._Application;
  try
    IExcel.Visible[LOCALE_USER_DEFAULT] := False;
    IExcel.DisplayAlerts[LOCALE_USER_DEFAULT] := False;
    IWorkBook := IExcel.Workbooks.Open(AFileName, EmptyParam, EmptyParam,
        EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
        EmptyParam, EmptyParam, EmptyParam, EmptyParam, False, EmptyParam, EmptyParam, LOCALE_USER_DEFAULT);

    IWorkBook.SaveAs(Result, xlExcel7, EmptyParam, False,
        False, False, xlNoChange, EmptyParam, False, EmptyParam, EmptyParam, False, LOCALE_USER_DEFAULT);
  finally
    if Assigned(IWorkBook) then begin
      IWorkBook.Close(False, EmptyParam, EmptyParam, LOCALE_USER_DEFAULT);
      IWorkBook := nil;
    end;
    IExcel := nil;
  end;
end;
{$ENDIF}

function GetFileList(APath: String; AMask: String): TArray<String>;
var
  LSearchRec: TSearchRec;
  LFileAttrs: Integer;
  LCount: Integer;
begin
  APath   := IncludeTrailingPathDelimiter(APath);
  SetLength(Result, 0); LCount := 0;
  LFileAttrs := faAnyFile;
{
  if not FilesOnly then
    FileAttrs := FileAttrs + faDirectory;
}
  if SysUtils.FindFirst(APath + AMask, LFileAttrs, LSearchRec) = 0 then begin
    repeat
      if {(not FilesOnly) or }((LSearchRec.Attr and faDirectory) = 0) then begin
        Inc(LCount);
        SetLength(Result, LCount);
        Result[Pred(LCount)] := APath + LSearchRec.Name;
      end;
    until SysUtils.FindNext(LSearchRec) <> 0;
    SysUtils.FindClose(LSearchRec);
  end;
end;

procedure SaveTextFile(const AFileName: String; const AText: String; AEncoding: TEncoding);
begin
  if AEncoding = nil then
    AEncoding := TEncoding.Default;

  with TStringStream.Create(AText, AEncoding) do try
    SaveToFile(AFileName)
  finally
    Free;
  end;
end;

function ReadTextFile(const AFileName: String): String;
begin
  with TStringStream.Create do try
    LoadFromFile(AFileName);
    Result := DataString
  finally
    Free;
  end;
end;

procedure ReadFile(const AFileName: String; var AData: TBytes);
var
  LFileHandle: Integer;
  LFileLength: Integer;
begin
  if TFile.Exists(AFileName) then begin
    LFileHandle := FileOpen(AFileName, fmOpenRead or fmShareDenyNone);
    if LFileHandle = -1 then
      Raise Exception.Create('Невозможно открыть файл "' + AFileName + '" для чтения.')
    else try
      LFileLength := FileSeek(LFileHandle, 0, 2);
      FileSeek(LFileHandle, 0, 0);
      SetLength(AData, LFileLength);
      if FileRead(LFileHandle, AData[1], LFileLength) <> LFileLength then
        Raise Exception.Create('Ошибка чтения файла "' + AFileName + '".');
    finally
      FileClose(LFileHandle);
    end;
  end else
    Raise Exception.Create('Файл "' + AFileName + '" не существует.')
end;

procedure DownloadInternetFile(const AFileName, URL: String);
const
  bInitalRequest: Bool = True;
type
  TBuf = array [1..1024] of char;
var
  hConnect, InetHandle: HInternet;
  F: File;
//  b : bool;
  dwCode: DWORD;
  dwSize: DWORD;
  DUMMY : DWORD;
  dwLength, dWritten : DWORD;
  szBuffer: ^TBuf;
begin
  InetHandle := InternetOpen
                (
                  PChar(Application.Title),
                  INTERNET_OPEN_TYPE_PRECONFIG,
                  nil,
                  nil,
                  INTERNET_FLAG_KEEP_CONNECTION
                );

  New(szBuffer);
  try
    hConnect := InternetOpenUrl
                (
                  InetHandle,
                  PChar(URL),
                  nil,
                  0,
                  INTERNET_FLAG_DONT_CACHE or
                  INTERNET_FLAG_KEEP_CONNECTION or
                  INTERNET_FLAG_RELOAD,
                  0
                );
    try
      if not bInitalRequest then
        if not HttpSendRequest (hConnect, nil, 0, nil, 0) then
          Raise Exception.Create('Error: HttpSendRequest');

      dwSize := SizeOf(dwCode);

      if not InternetQueryOption(hConnect, INTERNET_OPTION_HANDLE_TYPE, @dwCode, dwSize) then
        Raise Exception.Create('Error: InternetQueryOption');

      if (
          (dwCode = INTERNET_HANDLE_TYPE_HTTP_REQUEST)
          or
          (dwCode = INTERNET_HANDLE_TYPE_CONNECT_HTTP)
         )
      then begin
        dwSize := sizeof (DWORD) ;
        if not HttpQueryInfo(hConnect, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @dwCode, dwSize, DUMMY) then
          if dwCode = HTTP_STATUS_PROXY_AUTH_REQ then
            Raise Exception.Create('Auth Required!');
      end;

      if dwCode <> HTTP_STATUS_PROXY_AUTH_REQ then begin
        AssignFile(F, AFileName);
        Rewrite(F, 1);
        while True do begin
           dwLength := SizeOf(szBuffer^);
           if not WinInet.InternetReadFile (hConnect, szBuffer, dwLength, dWritten) then begin
             CloseFile(f);
             Windows.DeleteFile(PChar(AFileName));
             Raise Exception.Create('ERROR: InternetReadFile');
           end;

           if dWritten = 0 then
             Break
           else
             BlockWrite(F, szBuffer^, dWritten);
        end;
        CloseFile(f);
      end;
    finally
      InternetCloseHandle(InetHandle);
    end
  finally
    Dispose(szBuffer);
  end;
end;

{$IFDEF ARCHIVES}
function ExtractFiles(const AArchName: String; const AFileMask: String = ''; const ADestDir: String = ''): String;
var
{$IFDEF UNZIP}
  LUnZip: TVCLUnZip;
{$ENDIF}
{$IFDEF UNRAR}
  LUnRar: TRar;
  LDestDir: String;
  LFileMask: String;
{$ENDIF}
  LFileCount: Integer;
  LFileExt: String;
begin
  LFileExt := TrimLeft(ExtractFileExt(AArchName), '.');

{$IFDEF UNZIP}
  if SameText(LFileExt, SFileExt_Zip) then begin
    LUnZip := TVCLUnZip.Create(nil);
    try
      LUnZip.OverwriteMode := Always;
      LUnZip.ReplaceReadOnly := True;
      LUnZip.ZipName := AArchName;
      if (not ADestDir.IsEmpty) then
        LUnZip.DestDir := ADestDir
      else
        LUnZip.DestDir := GetTempPath;

      LUnZip.ReadZip;
      if LUnZip.Count = 0 then
        Raise Exception.Create('Zip archive "' + AArchName + '" does not contains any files');

      if not (AFileMask.IsEmpty) then
        LUnZip.FilesList.Text := StringReplace(AFileMask, '|', SConst_CR, [rfReplaceAll, rfIgnoreCase])
      else
        LUnZip.FilesList.Text := SFileMask_All;

      LFileCount := LUnZip.UnZip;
      if LFileCount = 0 then
        Raise Exception.Create('Zip archive "' + AArchName + '" does not contains any files requested by mask "' + LUnZip.FilesList.Text + '"');

      Result := IncludeTrailingPathDelimiter(LUnZip.DestDir);
    finally
      LUnZip.ClearZip;
      LUnZip.Free;
    end;
  end
{$ENDIF}
{$IF DEFINED(UNZIP) AND DEFINED(UNRAR)}
  else
{$IFEND}
{$IFDEF UNRAR}
  if SameText(LFileExt, SFileExt_Rar) then begin
    LUnRar := TRar.Create(nil);
    try
      //LUnRar.OverwriteMode := Always;
      //LUnRar.ReplaceReadOnly := True;
      if (not ADestDir.IsEmpty) then
        LDestDir := ADestDir
      else
        LDestDir := GetTempPath;

      if ((AFileMask.IsEmpty) or (AFileMask = '*') or (AFileMask = SFileMask_All)) then
        LFileMask := ''
      else
        LFileMask := AFileMask;

      LUnRar.OpenFile(AArchName, LFileMask);
      if LUnRar.FilesList.Count = 0 then
        Raise Exception.Create('Rar archive "' + AArchName + '" does not contains any files');

      LUnRar.Extract(LDestDir, False, LUnRar.FilesList);
//      LFileCount := LUnRar.UnRar;
//      if LFileCount = 0 then
//        Raise Exception.Create('Zip archive "' + AArchName + '" does not contains any files requested by mask "' + AFileMask + '"');

      Result := IncludeTrailingPathDelimiter(LDestDir);
    finally
      LUnRar.Free;
    end;
  end
{$ENDIF}
  else
    Raise Exception.Create('File extension '#171 + LFileExt + #187' is unknown!');
end;

function ExtractFileIfNeeded(const AFileName: String; const AFileMask: String; const ADestDir: string): String;
var
{$IFDEF UNZIP}
  LUnZip: TVCLUnZip;
{$ENDIF}
  LFileName: String;
  LFileCount: Integer;
  LFileExt: String;
{$IFDEF UNRAR}
  LUnRar: TRar;
  LDestDir: String;
  LFileMask: String;
{$ENDIF}
begin
  LFileExt := TrimLeft(ExtractFileExt(AFileName), '.');
{$IFDEF UNZIP}
  if SameText(LFileExt, SFileExt_Zip) then begin
    LUnZip := TVCLUnZip.Create(nil);
    try
      LUnZip.OverwriteMode := Always;
      LUnZip.ReplaceReadOnly := True;
      LUnZip.ZipName := AFileName;
      if (not ADestDir.IsEmpty) then
        LUnZip.DestDir := ADestDir
      else
        LUnZip.DestDir := GetTempPath;

      LUnZip.ReadZip;
      if LUnZip.Count = 0 then
        Raise Exception.Create('Zip archive "' + AFileName + '" does not contains any files');

      if (AFileMask.IsEmpty) or (AFileMask = '*') then begin
        LFileCount := LUnZip.Count;
        if LFileCount = 1 then begin
          LUnZip.FilesList.Text := SFileMask_All;
          LFileCount := LUnZip.UnZip;
        end;
      end else begin
        LUnZip.FilesList.Text := StringReplace(AFileMask, '|', SConst_CR, [rfReplaceAll, rfIgnoreCase]);
        LFileCount := LUnZip.UnZip;
      end;

      if LFileCount = 0 then
        Raise Exception.Create('Zip archive "' + AFileName + '" does not contains any files requested by mask "' + AFileMask + '"')
      else if LFileCount > 1 then
        Raise Exception.Create('Zip archive "' + AFileName + '" contains ' + IntToStr(LFileCount) + ' file(s) requested by mask "' + AFileMask + '"');

      LFileName := LUnZip.GetMatchedFileName(0);
      Result := IncludeTrailingPathDelimiter(LUnZip.DestDir) + '\' + LFileName;
    finally
      LUnZip.ClearZip;
      LUnZip.Free;
    end;
  end
{$ENDIF}
{$IF DEFINED(UNZIP) AND DEFINED(UNRAR)}
  else
{$IFEND}
{$IFDEF UNRAR}
  if SameText(LFileExt, SFileExt_Rar) then begin
    LUnRar := TRar.Create(nil);
    try
      //LUnRar.OverwriteMode := Always;
      //LUnRar.ReplaceReadOnly := True;
      if (not ADestDir.IsEmpty) then
        LDestDir := ADestDir
      else
        LDestDir := GetTempPath;

      if ((AFileMask.IsEmpty) or (AFileMask = '*') or (AFileMask = SFileMask_All)) then
        LFileMask := ''
      else
        LFileMask := AFileMask;

      LUnRar.OpenFile(AFileName, LFileMask);
      if LUnRar.FilesList.Count = 0 then
        Raise Exception.Create('Rar archive "' + AFileName + '" does not contains any files')
      else if LUnRar.FilesList.Count > 1 then
        Raise Exception.Create('Zip archive "' + AFileName + '" contains ' + IntToStr(LUnRar.FilesList.Count) + ' file(s) requested by mask "' + AFileMask + '"');

      LUnRar.Extract(LDestDir, False, LUnRar.FilesList);
//      LFileCount := LUnRar.UnRar;
//      if LFileCount = 0 then
//        Raise Exception.Create('Zip archive "' + AArchName + '" does not contains any files requested by mask "' + AFileMask + '"');

      Result := IncludeTrailingPathDelimiter(LDestDir) + LUnRar.FilesList[0];
    finally
      LUnRar.Free;
    end;
  end
{$ENDIF}
  else
    Result := AFileName;
end;
{$ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
{ TFunctions_SBaseFileUtils }

constructor TFunctions_SBaseFileUtils.Create(AScript: TfsScript);
begin
  inherited;

  with AScript do begin
    AddClass(TVersionInfo, TVersionInfo.ClassParent.ClassName);
    AddMethod('function GetFileList(APath: String; AMask: String): Variant;', Call_Method);
    AddMethod('function CompileFilePathByDate(AOldPath: String; AMask: String; ADateTime: TDateTime): String;', Call_Method);
    AddMethod('procedure OpenDataWithShell(AData: TBytes; AFileName: String);', Call_Method);
    AddMethod('procedure OpenFieldWithShell(AField: TField; AFileName: String);', Call_Method);
    AddMethod('procedure OpenParamWithShell(AParam: TParam; AFileName: String);', Call_Method);
  end;
end;

function TFunctions_SBaseFileUtils.Call_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'GETFILELIST' then
    Result := Variant(GetFileList(Caller.Params[0], Caller.Params[1]))
  else if MethodName = 'COMPILEFILEPATHBYDATE' then
    Result := CompileFilePathByDate(Caller.Params[0], Caller.Params[1], Caller.Params[2])
  else if MethodName = 'OPENFIELDWITHSHELL' then
    OpenDataWithShell(TField(VarToPointer(Caller.Params[0])), Caller.Params[1])
  else if MethodName = 'OPENPARAMWITHSHELL' then
    OpenDataWithShell(TParam(VarToPointer(Caller.Params[0])), Caller.Params[1])
  else if MethodName = 'OPENDATAWITHSHELL' then
    OpenDataWithShell(TBytes(Caller.Params[0]), Caller.Params[1]);
end;
{$ENDIF}

{ TFilePath }

constructor TFilePath.Create(const AFilePath: String);
begin
  Extract(AFilePath);
end;

procedure TFilePath.Extract(const AFilePath: String);
begin
  FFilePath := AFilePath;

  FillChar(FURLComponents, SizeOf(FURLComponents), 0);
  FURLComponents.dwStructSize      := SizeOf(FURLComponents);
  FURLComponents.dwSchemeLength    := 1;
  FURLComponents.dwHostNameLength  := 1;
  FURLComponents.dwURLPathLength   := 1;

  InternetCrackUrl(PChar(FFilePath), 0, 0, FURLComponents);
end;

function TFilePath.Protocol: TInternetScheme;
begin
  if not FFilePath.IsEmpty and (FURLComponents.nScheme in [INTERNET_SCHEME_FIRST..INTERNET_SCHEME_LAST]) then
    Result := FURLComponents.nScheme
  else
    Result := INTERNET_SCHEME_UNKNOWN
end;

function TFilePath.Scheme: String;
begin
  if not FFilePath.IsEmpty and (FURLComponents.dwSchemeLength > 0) then
    Result := Copy(FURLComponents.lpszScheme, 0, FURLComponents.dwSchemeLength)
  else
    Result := '';
end;

function TFilePath.URL: String;
begin
  Result := String(FFilePath);
end;

function TFilePath.Host: String;
begin
  if not FFilePath.IsEmpty and (FURLComponents.dwHostNameLength > 0) then
    Result := Copy(FURLComponents.lpszHostName, 0, FURLComponents.dwHostNameLength)
  else
    Result := '';
end;

function TFilePath.IsInternetUrl: Boolean;
begin
  Result := (FURLComponents.nScheme in [INTERNET_SCHEME_FILE, INTERNET_SCHEME_FTP, INTERNET_SCHEME_HTTP, INTERNET_SCHEME_HTTPS]);
end;

function TFilePath.Port: INTERNET_PORT;
begin
  if not FFilePath.IsEmpty and (FURLComponents.nPort > 0) then
    Result := FURLComponents.nPort
  else
    Result := 0
end;

function TFilePath.Path: String;
begin
  if not FFilePath.IsEmpty and (FURLComponents.dwUrlPathLength > 0) then
    Result := Copy(FURLComponents.lpszUrlPath, 0, FURLComponents.dwUrlPathLength)
  else
    Result := '';
end;

function TFilePath.FileName: String;
var
  L, I: Integer;
begin
  Result := Path;
  L := Length(Result);
  I := L;
  while I > 0 do begin
    if CharInSet(Result[I], ['/', '\']) then begin
      Result := Copy(Result, I + 1, L - I);
      Exit;
    end else if CharInSet(Result[I], ReservedFileNameChars) then begin
      Result := '';
      Exit;
    end;
    Dec(I);
  end;
  Result := '';
end;

{$IFDEF BASEFORM_SUPPORT}
{ TSelectDirectoryDlg }


//class function TSelectDirectoryDlg.CreateByParams(AOwner: TComponent; AParams: TNamedVariants): TComponent;
//begin
//  Result := nil;
//end;

function TSelectDirectoryDlg.IVirtualClassOwner(var AVirtualClassName: String; var AParams: TNamedVariants): TPersistentClass;
begin
  Result := nil;
end;

function TSelectDirectoryDlg.ICreateByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants): TComponent;
begin
  Result := nil;
end;

function TSelectDirectoryDlg.IGetCreateParams: TNamedVariants;
begin
  Assert(False);
end;

function TSelectDirectoryDlg.ISelectByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants; ACallBackProcedure: TCallBackSelectFunction): TNamedVariants;
var
  LDirectory, LCallBackScript: String;
  LIdx: Integer;
  LParams: TNamedVariants;
begin
  Result := TNamedVariants.Create(True);

  with TFileOpenDialog.Create(nil) do try
    Options := [fdoPickFolders];
    FileNameLabel := VarToStr(AParams[SConst_Caption]);
    DefaultFolder := VarToStr(AParams[SConst_Default]);

    if Execute then begin
      LDirectory := FileName;
      if AParams[SConst_IncludeTrailingPathDelimiter] = True then
        LDirectory := IncludeTrailingPathDelimiter(LDirectory);

      LCallBackScript := VarToStr(AParams[SBFSelectCallBackScript]);
      if AParams.Find(SConst_ResultFields, LIdx) then begin
        LParams.UnknownAsNull := True;
        LParams[SConst_Value] := LDirectory;
        with AParams.Items[LIdx] do
          Result[Value] := LParams[Value];
      end else
        Result[SConst_Value] := LDirectory;

      TSBaseFrm.ExecuteSelectCallBackProcedures(cbsaSingleRecord, Result, AOwner, ACallBackProcedure, LCallBackScript);
    end;
  finally
    Free;
  end;
end;

procedure TSelectDirectoryDlg.IShow;
begin
  Assert(False);
end;
{$ENDIF}

{ TFileTypesComponent }

constructor TFileTypesComponent.Create(AOwner: TComponent);
begin
  FItems := TFileTypeItems.Create(TFileTypeItem);
  inherited;
end;

destructor TFileTypesComponent.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TFileTypesComponent.SetItems(const Value: TFileTypeItems);
begin
  if Assigned(Value) then
    FItems.Assign(Value);
end;

initialization
  GetApplicationParams;
  if ApplicationExeName.IsEmpty then
    SetApplicationExeName(Application.ExeName);

  TEMP_DIR        := IOUtils.TPath.GetTempPath;

{$IFDEF ARCHIVES}
  ArchivesFileTypeItems := TFileTypeItems.Create(TFileTypeItem);
{$IFDEF UNZIP}
  FArchivesExtensions := ConcatIfNotEmpty(FArchivesExtensions, ';', '*.' + SFileExt_Zip);
  with ArchivesFileTypeItems.Add do begin
    DisplayName := 'WinZip archive';
    FileMask    := '*.' + SFileExt_Zip;
  end;
{$ENDIF}
{$IFDEF UNZIP}
  FArchivesExtensions := ConcatIfNotEmpty(FArchivesExtensions, ';', '*.' + SFileExt_Rar);
  with ArchivesFileTypeItems.Add do begin
    DisplayName := 'WinRar archive';
    FileMask    := '*.' + SFileExt_Rar;
  end;
{$ENDIF}
  with TFileTypeItem(ArchivesFileTypeItems.Insert(0)) do begin
    DisplayName := 'All archives';
    FileMask    := FArchivesExtensions;
  end;
{$ENDIF}
  ArchivesFileTypes := TFileTypesComponent.Create(Application);
  ArchivesFileTypes.Name := 'ArchivesFileTypes';
  ArchivesFileTypes.Items := ArchivesFileTypeItems;

  RegisterClasses([TFileOpenDialog, TFileSaveDialog{$IFDEF BASEFORM_SUPPORT}, TSelectDirectoryDlg{$ENDIF}, TFileTypeItems, TFileTypeItem]);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TFunctions_SBaseFileUtils);
{$ENDIF}

finalization
  UnRegisterClasses([TFileOpenDialog, TFileSaveDialog{$IFDEF BASEFORM_SUPPORT}, TSelectDirectoryDlg{$ENDIF}, TFileTypeItems, TFileTypeItem]);
{$IFDEF ARCHIVES}
  FreeAndNil(ArchivesFileTypeItems);
{$ENDIF}

end.
