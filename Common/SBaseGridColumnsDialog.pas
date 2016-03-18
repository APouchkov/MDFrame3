unit SBaseGridColumnsDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, DBGridEh, Buttons, Mask, DBCtrlsEh,
  cxGridDBTableView, Grids, cxGridDBBandedTableView,
  SBaseForm;

type
  TSBaseGridColumnsDlg = class(TSBaseFrm)
    PageControl1: TPageControl;
    TabShAccount: TTabSheet;
    ButtonOk: TButton;
    ButtonClose: TButton;
    Bevel: TBevel;
    ListViewInfo: TListView;
    sbUp: TSpeedButton;
    sbDown: TSpeedButton;

    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure sbUpClick(Sender: TObject);
    procedure sbDownClick(Sender: TObject);
    procedure ListViewInfoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListViewInfoDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    { Private declarations }
    Grid: TComponent;
    bInactiveOffset: Byte;
    CanColumnMoving: Boolean;
  public
    { Public declarations }
    constructor CreateDialog(Grid: TComponent; bInactiveOffset: Byte = 0); reintroduce;
  end;

implementation

uses SBaseConstants;

{$R *.dfm}

constructor TSBaseGridColumnsDlg.CreateDialog(Grid: TComponent; bInactiveOffset: Byte = 0);
var
  i : integer;
  ListItem: TListItem;
begin
  inherited CreateByParams(Grid, []);
  Self.Grid := Grid;
  Self.bInactiveOffset := bInactiveOffset;

  if Grid is TcxGridDBTableView then with Grid as TcxGridDBTableView do begin
    CanColumnMoving := True;
    for i:=bInactiveOffset to ColumnCount-1 do begin
      ListItem:=ListViewInfo.Items.Add;
      ListItem.Caption := ' ' + Columns[i].Caption;
      ListItem.Checked := Columns[i].Visible;
      ListItem.Indent := Columns[i].ID;
    end;
  end else if Grid is TcxGridDBBandedTableView then with Grid as TcxGridDBBandedTableView do begin
    CanColumnMoving := False;
    for i:=bInactiveOffset to ColumnCount-1 do begin
      ListItem:=ListViewInfo.Items.Add;
      ListItem.Caption := ' ' + Columns[i].Caption;
      ListItem.Checked := Columns[i].Visible;
      ListItem.Indent := Columns[i].ID;
    end;
  end else if Grid is TDBGridEh then with Grid as TDBGridEh do begin
    CanColumnMoving := True;
    for i:=bInactiveOffset to Columns.Count-1 do begin
      ListItem:=ListViewInfo.Items.Add;
      ListItem.Caption := ' ' + StringReplace(Columns[i].Title.Caption,'|',' :: ',[rfReplaceAll]);
      ListItem.Checked := Columns[i].Visible;
      ListItem.Indent := Columns[i].ID;
    end;
  end else
    Raise Exception.Create(SUnSupportedObject);


  if not CanColumnMoving then begin
    sbUp.Enabled := False;
    sbDown.Enabled := False;
    ListViewInfo.DragMode := dmManual;
  end;
end;
{--------------------------------------------------------------------------}

procedure TSBaseGridColumnsDlg.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TSBaseGridColumnsDlg.ButtonOkClick(Sender: TObject);
var
  i,j : integer;
  flgErr : Boolean;
begin

  flgErr := True;
  for j:=0 to ListViewInfo.Items.Count-1 do
    if ListViewInfo.Items[j].Checked then begin
      flgErr := False;
      Break;
    end;

  if flgErr then
    WarningMessage('Укажите как минимум одну колонку для отображения в списке');


  if Grid is TcxGridDBTableView then begin with Grid as TcxGridDBTableView do
    for i:=0 to Pred(ListViewInfo.Items.Count) do
      for j:=bInactiveOffset to Pred(ColumnCount) do
        if ListViewInfo.Items[i].Indent=Columns[j].Id then begin
          Columns[j].Visible:= ListViewInfo.Items[i].Checked;
          Columns[j].Index  := i + bInactiveOffset;
          break;
        end
  end else if Grid is TcxGridDBBandedTableView then begin with Grid as TcxGridDBBandedTableView do
      for j:=bInactiveOffset to Pred(ColumnCount) do
        Columns[j].Visible:= ListViewInfo.Items[j-bInactiveOffset].Checked;
  end else if Grid is TDBGridEh then begin with Grid as TDBGridEh do
    for i:=0 to Pred(ListViewInfo.Items.Count) do
      for j:=bInactiveOffset to Pred(Columns.Count) do
        if ListViewInfo.Items[i].Indent=Columns[j].Id then begin
          Columns[j].Visible:= ListViewInfo.Items[i].Checked;
          Columns[j].Index  := i + bInactiveOffset;
          break;
        end
  end;

  Close;
  ModalResult:=mrOk;

end;

procedure TSBaseGridColumnsDlg.sbUpClick(Sender: TObject);
var
  idx,Statex : integer;
  Title : string;
  Check : Boolean;
begin
  if ListViewInfo.Selected<>nil then begin
   idx:=ListViewInfo.Selected.Index;
   if idx>0 then begin
      Title:=ListViewInfo.Items[idx-1].Caption;
      Check:=ListViewInfo.Items[idx-1].Checked;
      Statex:=ListViewInfo.Items[idx-1].StateIndex;

      ListViewInfo.Items[idx-1].Selected:=True;

      ListViewInfo.Items[idx-1].Caption    := ListViewInfo.Items[idx].Caption;
      ListViewInfo.Items[idx-1].Checked    := ListViewInfo.Items[idx].Checked;
      ListViewInfo.Items[idx-1].StateIndex := ListViewInfo.Items[idx].StateIndex;

      ListViewInfo.Items[idx].Caption    := Title;
      ListViewInfo.Items[idx].Checked    := Check;
      ListViewInfo.Items[idx].StateIndex := Statex;
    end;
  end;
end;

procedure TSBaseGridColumnsDlg.sbDownClick(Sender: TObject);
var
  idx,Statex : integer;
  Title : string;
  Check : Boolean;
begin
  if ListViewInfo.Selected<>nil then begin
    idx:=ListViewInfo.Selected.Index;
    if idx<ListViewInfo.Items.Count-1 then begin
      Title :=ListViewInfo.Items[idx+1].Caption;
      Check :=ListViewInfo.Items[idx+1].Checked;
      Statex:=ListViewInfo.Items[idx+1].StateIndex;

      ListViewInfo.Items[idx+1].Selected:=True;
      ListViewInfo.Items[idx+1].Focused :=True;

      ListViewInfo.Items[idx+1].Caption    := ListViewInfo.Items[idx].Caption;
      ListViewInfo.Items[idx+1].Checked    := ListViewInfo.Items[idx].Checked;
      ListViewInfo.Items[idx+1].StateIndex := ListViewInfo.Items[idx].StateIndex;

      ListViewInfo.Items[idx].Caption:=Title;
      ListViewInfo.Items[idx].StateIndex := Statex;
      ListViewInfo.Items[idx].Checked:=Check;
    end;
  end;
end;

procedure TSBaseGridColumnsDlg.ListViewInfoDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var 
  DragItem, DropItem, CurrentItem, NextItem: TListItem; 
begin
  if Sender = Source then 
    with TListView(Sender) do
    begin 
      DropItem    := GetItemAt(X, Y);
      CurrentItem := Selected;

      if DropItem <> CurrentItem then begin
        while CurrentItem <> nil do
        begin
          NextItem := GetNextItem(CurrentItem, SdAll, [IsSelected]);
          if DropItem = nil then DragItem := Items.Add
          else
            DragItem := Items.Insert(DropItem.Index);
          DragItem.Assign(CurrentItem);
          CurrentItem.Free;
          CurrentItem := NextItem;
        end;

        if DropItem = nil then with Items[0] do begin
          Selected := True;
          Focused := True;
        end else with Items[Pred(DropItem.Index)] do begin
          Selected := True;
          Focused := True;
        end;
      end;
    end;
end;

procedure TSBaseGridColumnsDlg.ListViewInfoDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Sender = ListViewInfo;
end;

end.

