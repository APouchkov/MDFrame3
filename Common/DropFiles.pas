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
    В TSubclassItem хранится информация
    о субклассированном окне
  }
  TSubclassItem = class (TObject)
  private
    Handle : HWND;
    { Логический номер окна }
    WindowProc : TFNWndProc;
    { Старая оконная процедура }
    FOnDrop : TFMDDEvent;
    { Обработчик события OnFMDragDrop элемента }
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
  WMDragDrop создает объект TDragDropInfo
  и вызывает
  обработчик FOnDrop.
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
    hDrop - логический номер внутренней
    структуры данных Windows,
    содержащей информацию о брошенных файлах.
  }
  {
    Определяем общее количество брошенных
    файлов, передавая
    DragQueryFile индексный параметр -1
  }
  TotalNumberOfFiles := DragQueryFile (hDrop, $FFFFFFFF, Nil, 0);

  DragDropInfo := TDragDropInfo.Create
  (TotalNumberOfFiles);

  { Проверяем, были ли файлы брошены в клиентской области }
  {
  InClientArea :=
    DragQueryPoint (hDrop,
    DragDropInfo.FDropPoint);
  }

  for i := 0 to TotalNumberOfFiles - 1 do begin
    {
      Определяем длину имени файла ?
      сообщаем DragQueryFile
      о том, какой файл нас интересует ( i ),
      и передаем Nil
      вместо длины буфера.  Возвращаемое
      значение равно длине
      имени файла.
    }
    nFileLength := DragQueryFile (hDrop, i ,
    Nil, 0) + 1;

    {
      Копируем имя файла ?  сообщаем
      DragQueryFile о том,
      какой файл нас интересует ( i ), и
      передаем длину буфера.
      ЗАМЕЧАНИЕ: Проследите за тем, чтобы
      размер буфера на 1 байт
      превышал длину имени, чтобы выделить
      место для завершающего
      строку нулевого символа!
    }
    if nFileLength < sizeof(pszFileName) then begin
      DragQueryFile (hDrop , i, pszFileName, nFileLength);

    { Заносим файл в список }
      DragDropInfo.FFileList.Add (pszFileName);
    end;

    { Освобождаем выделенную память... }
//    FreeMem (pszFileName, nFileLength);
  end;
  {
    Вызываем DragFinish, чтобы освободить память,
    выделенную Shell
    для данного логического номера.
    ЗАМЕЧАНИЕ: Об этом шаге нередко забывают,
    в результате возникает
    утечка памяти, а программа
    начинает медленнее работать.
  }
  DragFinish (hDrop);

  { Вызываем обработчик события... }
  FOnDrop (DragDropInfo);

  { ...и уничтожаем объект TDragDropInfo }
  DragDropInfo.Free;
end;


{
  FindItemInList находит и возвращает элемент
  списка, соответствующий
  передаваемому логическому номеру окна
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
  FMDDWndProc обрабатывает сообщения 
  WM_DROPFILES, вызывая WMDragDrop.
  Все прочие сообщения передаются старой 
  оконной процедуре.
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
  AcceptDroppedFiles субклассирует окно элемента 
  и сохраняет информацию для 
  последующего использования.
}
procedure AcceptDroppedFiles (Control 
: TWinControl;
            AOnDrop : TFMDDEvent);
var
  WndProc : TFNWndProc;
begin
  DragAcceptFiles (Control.Handle, True);
  { Получаем старую оконную процедуру }
  WndProc :=
    TFNWndProc(GetWindowLong (Control.Handle, 
    GWL_WNDPROC));
  { Подключаем новую оконную процедуру... }
  SetWindowLong (Control.Handle,
    GWL_WNDPROC, Longint (@FMDDWndProc));
  { ... и добавляем ее в список }
  SubclassList.Add (
    TSubclassItem.Create (Control.Handle, 
    WndProc, AOnDrop));
end;

{
  UnacceptDroppedFiles прекращает 
  субклассирование окна
  и удаляет его из списка.
}
procedure UnacceptDroppedFiles (Control 
: TWinControl);
var
  Item : TSubclassItem;
begin
  { Прекращаем прием файлов }
  DragAcceptFiles (Control.Handle, False);

  Item := FindItemInList (Control.Handle);
  if Item <> Nil then begin
    { Восстанавливаем старую оконную процедуру }
    SetWindowLong (Control.Handle, GWL_WNDPROC,
      Longint (Item.WindowProc));
    { Удаляем элемент из списка... }
    SubclassList.Remove (Item);
    { ... и уничтожаем его }
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
