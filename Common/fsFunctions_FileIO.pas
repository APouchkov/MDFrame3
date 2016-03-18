unit fsFunctions_FileIO;

interface

implementation

uses
  FileUtil, SysUtils, Types, Classes, DateUtils, frxClass, fs_iinterpreter, fs_itools, Variants, FileCtrl, Windows, Dialogs,
  SBaseFileUtils, SBaseVariantFunctions, SBaseConstants;

type
  TfsFunctions_FileIO = class(TfsRTTIModule)
  private
    function Call_FileIO_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function Get_TFileOpenDialog_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function Call_TFileOpenDialog_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

{ TfsFunctions_FileIO }

constructor TfsFunctions_FileIO.Create(AScript: TfsScript);
const
  SCategory: String = 'Работа с файлами';
begin
  inherited Create(AScript);

  with AScript do begin
    AddMethod('function SelectDirectory(var ADirectory: string; ADefault: String = ''''): Boolean', Call_FileIO_Method, SCategory);
    AddMethod('function IncludeTrailingPathDelimiter(const Path : String) : String', Call_FileIO_Method, SCategory);
    AddMethod('function ForceDirectories(const Path : String) : Boolean', Call_FileIO_Method, SCategory);
    AddMethod('function ExtractFilePath(const FileName: String): String', Call_FileIO_Method, SCategory);
    AddMethod('function ExtractFileDir(const FileName: String): String', Call_FileIO_Method, SCategory);
    AddMethod('function ExtractShortPathName(const FileName: string): string', Call_FileIO_Method, SCategory);
    AddMethod('function ExtractFileName(const FileName: String): String', Call_FileIO_Method, SCategory);
    AddMethod('function ExtractFileExt(const FileName: String): String', Call_FileIO_Method, SCategory);
    AddMethod('function FileExists(const FileName: String): Boolean', Call_FileIO_Method, SCategory);
    AddMethod('function IncludeTrailingPathDelimiter(const DirName: String): String', Call_FileIO_Method, SCategory);

    AddMethod('function CopyFile(const SourceFile : String; const DestFile : String; FailIfExist: Boolean) : Boolean', Call_FileIO_Method, SCategory);
    AddMethod('function DeleteFile(const FileName: String): Boolean', Call_FileIO_Method, SCategory);
    AddMethod('procedure SaveTextFile(const AFileName: String; const AText: String; const AEncoding: TEncoding = nil)', Call_FileIO_Method, SCategory);
    AddMethod('procedure ShellExecute(const CmdText: String; const CmdParams: String)', Call_FileIO_Method, SCategory);
  end;

  AScript.AddEnum('TFileDialogOptions', 'fdoOverWritePrompt, fdoStrictFileTypes,' +
    'fdoNoChangeDir, fdoPickFolders, fdoForceFileSystem,' +
    'fdoAllNonStorageItems, fdoNoValidate, fdoAllowMultiSelect,' +
    'fdoPathMustExist, fdoFileMustExist, fdoCreatePrompt,' +
    'fdoShareAware, fdoNoReadOnlyReturn, fdoNoTestFileCreate,' +
    'fdoHideMRUPlaces, fdoHidePinnedPlaces, fdoNoDereferenceLinks,' +
    'fdoDontAddToRecent, fdoForceShowHidden, fdoDefaultNoMiniMode,' +
    'fdoForcePreviewPaneOn)');


  AScript.AddEnum('TShellExecuteOptions',
    'SW_SHOWNORMAL,SW_NORMAL,SW_SHOWMINIMIZED,SW_SHOWMAXIMIZED,SW_MAXIMIZE,'+
    'SW_SHOWNOACTIVATE,SW_SHOW,SW_MINIMIZE,SW_SHOWMINNOACTIVE,SW_SHOW,SW_MINIMIZE'+
    'SW_SHOWMINNOACTIVE,SW_SHOWNA,SW_RESTORE,SW_SHOWDEFAULT,SW_FORCEMINIMIZE,SW_MAX');
  with AScript.AddClass(TFileOpenDialog, TFileOpenDialog.ClassParent.ClassName) do begin
    AddProperty('Files', 'TStrings', Get_TFileOpenDialog_Property);
    AddMethod('function Execute: Boolean', Call_TFileOpenDialog_Method);
  end;

  AScript.AddClass(TFileTypeItem, TFileTypeItem.ClassParent.ClassName);
end;

function TfsFunctions_FileIO.Call_FileIO_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
//var
//  LStringParam: String;
begin
  if MethodName = 'SELECTDIRECTORY' then begin
    with TFileOpenDialog.Create(nil) do
    try
      Options := [fdoPickFolders];
      DefaultFolder := VarToStr(Caller.Params[1]);
      if Execute then begin
        Result := True;
        Caller.Params[0] := FileName;
      end else
        Result := False;
    finally
      Free;
    end;
  end else if MethodName = 'INCLUDETRAILINGPATHDELIMITER' then
    Result := SysUtils.IncludeTrailingPathDelimiter(Caller.Params[0])
  else if MethodName = 'FORCEDIRECTORIES' then
    Result := SysUtils.ForceDirectories(Caller.Params[0])
  else if MethodName = 'COPYFILE' then
    Result := Windows.CopyFile(PChar(VarToStr(Caller.Params[0])), PChar(VarToStr(Caller.Params[1])), Caller.Params[2] = True)
  else if MethodName = 'DELETEFILE' then
    Result := SysUtils.DeleteFile(Caller.Params[0])
  else if MethodName = 'EXTRACTFILEPATH' then
    Result := SysUtils.ExtractFilePath(Caller.Params[0])
  else if MethodName = 'EXTRACTFILEDIR' then
    Result := SysUtils.ExtractFileDir(Caller.Params[0])
  else if MethodName = 'EXTRACTSHORTPATHNAME' then
    Result := SysUtils.ExtractShortPathName(Caller.Params[0])
  else if MethodName = 'EXTRACTFILENAME' then
    Result := SysUtils.ExtractFileName(Caller.Params[0])
  else if MethodName = 'EXTRACTFILEEXT' then
    Result := SysUtils.ExtractFileExt(Caller.Params[0])  
  else if MethodName = 'FILEEXISTS' then
    Result := SysUtils.FileExists(Caller.Params[0])
  else if MethodName = 'INCLUDETRAILINGPATHDELIMITER' then
    Result := IncludeTrailingPathDelimiter(Caller.Params[0])
  else if MethodName = 'SAVETEXTFILE' then
    SBaseFileUtils.SaveTextFile(Caller.Params[0], Caller.Params[1], VarToPointer(Caller.Params[2]))
  else if MethodName = 'SHELLEXECUTE' then
    ShellExecute(Caller.Params[0], Caller.Params[1]);
end;

function TfsFunctions_FileIO.Get_TFileOpenDialog_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'FILES' then
    Result := Integer(TFileOpenDialog(Instance).Files);
end;

function TfsFunctions_FileIO.Call_TFileOpenDialog_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'EXECUTE' then
    Result := TFileOpenDialog(Instance).Execute;
end;

initialization
  fsRTTIModules.Add(TfsFunctions_FileIO);

finalization
  fsRTTIModules.Remove(TfsFunctions_FileIO);

end.
