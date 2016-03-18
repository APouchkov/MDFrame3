unit DropFiles;

interface

uses Types, Windows, Messages, Classes, Controls;

type
  TDragDropInfo = class (TObject)
  private
    FCount : UINT;
    FDropPoint : TPoint;
    FFileList : TStringList;
  public
    constructor Create (ANumFiles : UINT);
    destructor Destroy; override;

    property Count : UINT read FCount;
    property DropPoint : TPoint
    read FDropPoint;
    property Files : TStringList
    read FFileList;
  end;

  TFMDDEvent = procedure (DDI : TDragDropInfo) of object;

procedure AcceptDroppedFiles (Control : TWinControl; AOnDrop : TFMDDEvent);
procedure UnacceptDroppedFiles(Control : TWinControl);

implementation

uses ShellAPI;

type
  {
    � TSubclassItem �������� ����������
    � ����������������� ����
  }
  TSubclassItem = class (TObject)
  private
    Handle : HWND;
    { ���������� ����� ���� }
    WindowProc : TFNWndProc;
    { ������ ������� ��������� }
    FOnDrop : TFMDDEvent;
    { ���������� ������� OnFMDragDrop �������� }
  public
    constructor Create (AHandle: HWND; AWndProc: TFNWndProc; AOnDrop: TFMDDEvent);
  end;

var
  SubclassList : TList;

constructor TSubclassItem.Create (AHandle: HWND; AWndProc: TFNWndProc; AOnDrop: TFMDDEvent);
begin
  inherited Create;
  Handle := AHandle;
  WindowProc := AWndProc;
  FOnDrop := AOnDrop;
end;

{
  WMDragDrop ������� ������ TDragDropInfo
  � ��������
  ���������� FOnDrop.
}
procedure WMDragDrop (hDrop : THandle; FOnDrop: TFMDDEvent);
var
  DragDropInfo : TDragDropInfo;
  pszFileName: array [0..255] of char;
  TotalNumberOfFiles,
  nFileLength : Integer;
  i : Integer;
//  InClientArea: Boolean;
begin
  if not assigned (FOnDrop) then
    exit;
  {
    hDrop - ���������� ����� ����������
    ��������� ������ Windows,
    ���������� ���������� � ��������� ������.
  }
  {
    ���������� ����� ���������� ���������
    ������, ���������
    DragQueryFile ��������� �������� -1
  }
  TotalNumberOfFiles := DragQueryFile (hDrop, $FFFFFFFF, Nil, 0);

  DragDropInfo := TDragDropInfo.Create
  (TotalNumberOfFiles);

  { ���������, ���� �� ����� ������� � ���������� ������� }
  {
  InClientArea :=
    DragQueryPoint (hDrop,
    DragDropInfo.FDropPoint);
  }

  for i := 0 to TotalNumberOfFiles - 1 do begin
    {
      ���������� ����� ����� ����� ?
      �������� DragQueryFile
      � ���, ����� ���� ��� ���������� ( i ),
      � �������� Nil
      ������ ����� ������.  ������������
      �������� ����� �����
      ����� �����.
    }
    nFileLength := DragQueryFile (hDrop, i ,
    Nil, 0) + 1;

    {
      �������� ��� ����� ?  ��������
      DragQueryFile � ���,
      ����� ���� ��� ���������� ( i ), �
      �������� ����� ������.
      ���������: ���������� �� ���, �����
      ������ ������ �� 1 ����
      �������� ����� �����, ����� ��������
      ����� ��� ������������
      ������ �������� �������!
    }
    if nFileLength < sizeof(pszFileName) then begin
      DragQueryFile (hDrop , i, pszFileName, nFileLength);

    { ������� ���� � ������ }
      DragDropInfo.FFileList.Add (pszFileName);
    end;

    { ����������� ���������� ������... }
//    FreeMem (pszFileName, nFileLength);
  end;
  {
    �������� DragFinish, ����� ���������� ������,
    ���������� Shell
    ��� ������� ����������� ������.
    ���������: �� ���� ���� ������� ��������,
    � ���������� ���������
    ������ ������, � ���������
    �������� ��������� ��������.
  }
  DragFinish (hDrop);

  { �������� ���������� �������... }
  FOnDrop (DragDropInfo);

  { ...� ���������� ������ TDragDropInfo }
  DragDropInfo.Free;
end;


{
  FindItemInList ������� � ���������� �������
  ������, ���������������
  ������������� ����������� ������ ����
}
function FindItemInList (Handle : HWND): TSubclassItem;
var
  i : Integer;
  Item : TSubclassItem;
begin
  for i := 0 to SubclassList.Count - 1 
  do begin
    Item := SubclassList.Items[i];
    if Item.Handle = Handle then begin
      Result := Item;
      exit;
    end;
  end;
  Result := Nil;
end;

{
  FMDDWndProc ������������ ��������� 
  WM_DROPFILES, ������� WMDragDrop.
  ��� ������ ��������� ���������� ������ 
  ������� ���������.
}
function FMDDWndProc (
    Handle : HWND; Msg : UINT;
    wparam: WPARAM; lparam: LPARAM) 
    : LRESULT; stdcall;
var
  Item : TSubclassItem;
begin
  Item := FindItemInList (Handle);
  if Item <> Nil then begin
    if Msg = WM_DROPFILES then begin
      WMDragDrop (wparam, Item.FOnDrop);
      Result := 0;
    end
    else
      Result := CallWindowProc (Item.WindowProc,
          Handle, Msg, wparam, lparam)
  end
  else
    Result := 0;
end;

{
  AcceptDroppedFiles ������������� ���� �������� 
  � ��������� ���������� ��� 
  ������������ �������������.
}
procedure AcceptDroppedFiles (Control 
: TWinControl;
            AOnDrop : TFMDDEvent);
var
  WndProc : TFNWndProc;
begin
  DragAcceptFiles (Control.Handle, True);
  { �������� ������ ������� ��������� }
  WndProc :=
    TFNWndProc(GetWindowLong (Control.Handle, 
    GWL_WNDPROC));
  { ���������� ����� ������� ���������... }
  SetWindowLong (Control.Handle,
    GWL_WNDPROC, Longint (@FMDDWndProc));
  { ... � ��������� �� � ������ }
  SubclassList.Add (
    TSubclassItem.Create (Control.Handle, 
    WndProc, AOnDrop));
end;

{
  UnacceptDroppedFiles ���������� 
  ���������������� ����
  � ������� ��� �� ������.
}
procedure UnacceptDroppedFiles (Control 
: TWinControl);
var
  Item : TSubclassItem;
begin
  { ���������� ����� ������ }
  DragAcceptFiles (Control.Handle, False);

  Item := FindItemInList (Control.Handle);
  if Item <> Nil then begin
    { ��������������� ������ ������� ��������� }
    SetWindowLong (Control.Handle, GWL_WNDPROC,
      Longint (Item.WindowProc));
    { ������� ������� �� ������... }
    SubclassList.Remove (Item);
    { ... � ���������� ��� }
    Item.Free;
  end;
end;

{ TDragDropInfo }
constructor TDragDropInfo.Create(ANumFiles : UINT);
begin
  inherited Create;
  FCount := ANumFiles;
  FFileList := TStringList.Create;
end;

destructor TDragDropInfo.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

initialization
  SubclassList := TList.Create;

finalization
  SubclassList.Free;

end.
