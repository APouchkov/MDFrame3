unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ActiveX, ShellApi, ShlObj, Buttons, ExtCtrls, DropSource,
  StdCtrls, CommCtrl, FileCtrl;

type

  TForm1 = class(TForm)
    ListView1: TListView;
    Panel1: TPanel;
    Button1: TButton;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    DropFileSource1: TDropFileSource;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure DropFileSource1Drop(Sender: TObject; DragType: TDragType;
      var ContinueDrop: Boolean);
  private
    DragPoint: TPoint;
    AlreadyDragging: boolean;
    TempPath: string; //path to temp folder
    procedure ExtractFile(ListviewIndex: integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;


implementation

{$R *.DFM}

//---------------------------------------------------------------------
// Local functions ...
//---------------------------------------------------------------------

procedure MakeBlankFile(const Name: string);
var
  tf: textfile;
  path: string;
begin
  path := extractfilepath(name);
  if (path <> '') then
    ForceDirectories(path);
  assignfile(tf,Name);
  rewrite(tf);
  closefile(tf);
end;
//---------------------------------------------------------------------

function AddSlash(var str: string): string;
begin
  if (str <> '') and (str[length(str)] <> '\') then
    appendstr(str,'\');
  result := str;
end;
//---------------------------------------------------------------------

function GetTempPath: string;
var
  Res: DWORD;
begin
  SetLength (Result, MAX_PATH);
  Res := windows.GetTempPath(MAX_PATH, PChar(Result));
  SetLength (Result, Res);
  AddSlash(Result); //append a slash if needed
end;

//---------------------------------------------------------------------
// TForm1 class ...
//---------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
begin
 TempPath := GetTempPath;
end;
//---------------------------------------------------------------------

procedure TForm1.ListView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DragPoint.X := X;
  DragPoint.Y := Y;
end;

//---------------------------------------------------------------------
// Start a Drag and Drop ...
//---------------------------------------------------------------------

procedure TForm1.ListView1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  i,j: integer;
  tmpStr: string;
begin
  if AlreadyDragging then exit;
  //Filter out all but the mouse buttons from 'Shift'...
  Shift := (Shift * [ssLeft,ssRight]);
  //Make sure mouse has moved at least 10 pixels
  //and a mouse button pressed before starting drag ...
  //(DragPoint is set in the ListView1MouseDown event)
  if  (Shift = []) or
      ((abs(DragPoint.X - X) <10) and (abs(DragPoint.Y - Y) <10)) then exit;
  //If no files selected then exit...
  if Listview1.SelCount = 0 then exit;

  //clear any filenames left from a previous drag operation...
  DropFileSource1.Files.clear;

  //OK, 'extracting' files here would be much simpler but often
  //very inefficient as many drag ops are cancelled before the
  //files are actually dropped. Delay the extracting until we
  //know the user really wants the files, but load the filenames
  //into DropFileSource1.Files as if they already exist...

  //add root files and top level subfolders...
  for i := 0 to Listview1.items.Count-1 do
    if (Listview1.items.item[i].Selected) then
    begin
      j := pos('\',Listview1.items.item[i].caption);
      if j > 0 then
      begin
        //file in a subfolder...
        //get the top level subfolder...
        tmpStr := copy(Listview1.items.item[i].caption,1,j-1);
        //if the subfolder hasn't already been added...
        if DropFileSource1.Files.IndexOf(TempPath + tmpStr) = -1 then
          DropFileSource1.Files.Add(TempPath + tmpStr);
      end
      else
        //file in root folder...
        DropFileSource1.Files.Add(TempPath + Listview1.items.item[i].caption);
    end;

  //Do the dragdrop...
  DropFileSource1.execute;

end;
//---------------------------------------------------------------------

procedure TForm1.DropFileSource1Drop(Sender: TObject; DragType: TDragType;
  var ContinueDrop: Boolean);
var
  i: integer;
begin
  //OK, if we get this far, the user really does want the files...
  //'Extract' all the selected files into the temporary folder tree...
  for i := 0 to Listview1.items.Count-1 do
    if (Listview1.items.item[i].Selected) then ExtractFile(i);
  //As soon as this method returns, the (Explorer's) DropTarget.OnDrop
  //event will trigger.
end;
//---------------------------------------------------------------------

procedure TForm1.ExtractFile(ListviewIndex: integer);
begin
  //Of course, this is a demo so we'll just make phoney files here...
  MakeBlankFile(TempPath+ Listview1.items.item[ListviewIndex].caption);
end;
//---------------------------------------------------------------------

procedure TForm1.Button1Click(Sender: TObject);
begin
  close;
end;
//---------------------------------------------------------------------
//---------------------------------------------------------------------

end.

