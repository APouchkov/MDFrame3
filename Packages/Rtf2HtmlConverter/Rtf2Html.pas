(*
  Негаш А.
  2012г
  Перевод текста из TRichEdit в HTML формат c ооочень сильно ограниченным
  количеством тэгов. Для ускорения используется TOM (Text Object Model)
  интерфейс к RichEdit-у.
*)

unit Rtf2Html;

interface

uses Classes, SysUtils, ComCtrls, Graphics, Windows, ActiveX, Richedit, tom_TLB;

{$REGION 'IRichEditOle'}
{*
 *	RICHOLE.H
 *
 *	Purpose:
 *		OLE Extensions to the Rich Text Editor
 *
 *	Copyright (c) 1985-1996, Microsoft Corporation
 *}

{$MINENUMSIZE 4}
const
  // Flags to specify which interfaces should be returned in the structure above
  REO_GETOBJ_NO_INTERFACES	= $00000000;
  REO_GETOBJ_POLEOBJ			  = $00000001;
  REO_GETOBJ_PSTG				    = $00000002;
  REO_GETOBJ_POLESITE			  = $00000004;
  REO_GETOBJ_ALL_INTERFACES	= $00000007;

  // Place object at selection
  REO_CP_SELECTION          = $FFFFFFFF;

  // Use character position to specify object instead of index
  REO_IOB_SELECTION         = $FFFFFFFF;
  REO_IOB_USE_CP            = $FFFFFFFE;

  // Object flags
  REO_NULL			            = $00000000;	// No flags
  REO_READWRITEMASK	        = $0000003F;	// Mask out RO bits
  REO_DONTNEEDPALETTE	      = $00000020;	// Object doesn't need palette
  REO_BLANK			            = $00000010;	// Object is blank
  REO_DYNAMICSIZE		        = $00000008;	// Object defines size always
  REO_INVERTEDSELECT	      = $00000004;	// Object drawn all inverted if sel
  REO_BELOWBASELINE	        = $00000002;	// Object sits below the baseline
  REO_RESIZABLE		          = $00000001;	// Object may be resized
  REO_LINK			            = $80000000;	// Object is a link (RO)
  REO_STATIC			          = $40000000;	// Object is static (RO)
  REO_SELECTED		          = $08000000;	// Object selected (RO)
  REO_OPEN			            = $04000000;	// Object open in its server (RO)
  REO_INPLACEACTIVE	        = $02000000;	// Object in place active (RO)
  REO_HILITED			          = $01000000;	// Object is to be hilited (RO)
  REO_LINKAVAILABLE	        = $00800000;	// Link believed available (RO)
  REO_GETMETAFILE		        = $00400000;	// Object requires metafile (RO)

  // flags for IRichEditOle::GetClipboardData(),
  // IRichEditOleCallback::GetClipboardData() and
  // IRichEditOleCallback::QueryAcceptData()
  RECO_PASTE			          = $00000000;	// paste from clipboard
  RECO_DROP			            = $00000001;	// drop
  RECO_COPY			            = $00000002;	// copy to the clipboard
  RECO_CUT			            = $00000003;	// cut to the clipboard
  RECO_DRAG			            = $00000004;	// drag

// Structure passed to GetObject and InsertObject
type
  TREOBJECT = packed record
    cbStruct: DWORD;			// Size of structure
    cp: longint;					// Character position of object
    clsid: TCLSID;				// Class ID of object
    oleobj: IOleObject;			// OLE object interface
    stg: IStorage;				// Associated storage interface
    olesite: IOLEClientSite;			// Associated client site interface
    sizel: TSize;				// Size of object (may be 0,0)
    dvaspect: DWORD;			// Display aspect to use
    dwFlags: DWORD;			// Object status flags
    dwUser: DWORD;				// Dword for user's use
  end;

{*
 *	IRichEditOle
 *
 *	Purpose:
 *		Interface used by the client of RichEdit to perform OLE-related
 *		operations.
 *
 *	//$ REVIEW:
 *		The methods herein may just want to be regular Windows messages.
 *}
  IRichEditOle = interface(IUnknown)
    ['{00020D00-0000-0000-C000-000000000046}']
    function GetClientSite(out lplpolesite: IOLECLIENTSITE): HResult; stdcall;
    function GetObjectCount: longint; stdcall;
    function GetLinkCount: longint; stdcall;
    function GetObject(iob: longint; out reobject: TREOBJECT; dwFlags: DWORD): HRESULT; stdcall;
    function InsertObject(const reobject: TREOBJECT): HResult; stdcall;
    function ConvertObject(iob: longint; const clsidNew: TCLSID;
       lpStrUserTypeNew: POleStr): HRESULT; stdcall;
    function ActivateAs(const clsid, clsidAs: TCLSID): HRESULT; stdcall;
    function SetHostNames(lpstrContainerApp, lpstrContainerObj: POleStr): HRESULT; stdcall;
    function SetLinkAvailable(iob: longint; fAvailable: BOOL): HRESULT; stdcall;
    function SetDvaspect(iob: longint; dvaspect: DWORD): HRESULT; stdcall;
    function HandsOffStorage(iob: longint): HRESULT; stdcall;
    function SaveCompleted(iob: longint; stg: IStorage): HRESULT; stdcall;
    function InPlaceDeactivate: HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
         out dataobj: IDataObject): HRESULT; stdcall;
    function ImportDataObject(dataobj: IDataObject; cf: TClipFormat;
         hMetaPict: HGLOBAL): HRESULT; stdcall;
  end;

{*
 *	IRichEditOleCallback
 *
 *	Purpose:
 *		Interface used by the RichEdit to get OLE-related stuff from the
 *		application using RichEdit.
 *}
  IRichEditOleCallback = interface(IUnknown)
    ['{00020D03-0000-0000-C000-000000000046}']
    function GetNewStorage(out stg: IStorage): HRESULT; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
         out Doc: IOleInPlaceUIWindow; var FrameInfo: TOleInPlaceFrameInfo): HRESULT; stdcall;
    function ShowContainerUI(fShow: BOOL): HRESULT; stdcall;
    function QueryInsertObject(const clsid: TCLSID; stg: IStorage; cp: longint): HRESULT; stdcall;
    function DeleteObject(oleobj: IOLEObject): HRESULT; stdcall;
    function QueryAcceptData(dataobj: IDataObject; var cfFormat: TClipFormat;
         reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
         out dataobj: IDataObject): HRESULT; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
         var dwEffect: DWORD): HRESULT; stdcall;
    function GetContextMenu(seltype: Word; oleobj: IOleObject;
         const chrg: TCharRange; var menu: HMENU): HRESULT; stdcall;
  end;

function RichEdit_SetOleCallback(RichEdit: HWnd; OleInterface: IRichEditOleCallback): BOOL;
function RichEdit_GetOleInterface(RichEdit: HWnd; out OleInterface: IRichEditOle): BOOL;

{$ENDREGION}

type
  TFontProperty = record
    Color: TColor;
    Name: TFontName;
    Size: Single;
    Style: TFontStyles;

    class operator Equal(AFirst, ASecond: TFontProperty): Boolean;
    class operator NotEqual(AFirst, ASecond: TFontProperty): Boolean;
    class operator Implicit(ATextAttribute: TTextAttributes): TFontProperty;
    class operator Implicit(AFont: ITextFont): TFontProperty;
  end;

  TRtf2HtmlConverter = class
  strict private
    class function GetCloseTag(AAttribute: TFontProperty): string;
    class function GetOpenTag(AAttribute: TFontProperty): string;
  public
    class function Convert(ARichEdit: TRichEdit): String;
  end;

  function TextToHtml(AText: string): string;
implementation

function _CharToHtml(C: Char): string;
begin
  case C of
    '<': Result := '&lt;';
    '>': Result := '&gt;';
    '&': Result := '&nbsp;';
    '"': Result := '&quot;';
    else Result := C;
  end;
end;

function TextToHtml(AText: string): string;
  var LChar: Char;
begin
  Result := '';
  for LChar in AText do
    Result := Result + _CharToHtml(LChar);
end;

{$REGION 'IRichEditOle implementation'}

function RichEdit_SetOleCallback(RichEdit: HWnd; OleInterface: IRichEditOleCallback): BOOL;
begin
  Result:= BOOL(SendMessage(RichEdit, EM_SETOLECALLBACK, 0, LongInt(OleInterface)));
end;

function RichEdit_GetOleInterface(RichEdit: HWnd; out OleInterface: IRichEditOle): BOOL;
begin
  Result:= BOOL(SendMessage(RichEdit, EM_GETOLEINTERFACE, 0, LongInt(@OleInterface)));
end;

{$ENDREGION}

{$REGION 'TRtf2HtmlConverter'}

class function TRtf2HtmlConverter.Convert(ARichEdit: TRichEdit): String;
  var
    i: Integer;
    LOldAttribute, LAttribute: TFontProperty;
    LText: string;

    LRichEdit: IRichEditOle;
    LDoc: ITextDocument;
    LRange: ITextRange;
    LLength: Integer;
    LDefaultColor: TColor;
begin
  {$REGION 'Create TOM Document'}
  if not RichEdit_GetOleInterface(ARichEdit.Handle, LRichEdit) then
    raise Exception.Create('IRichEditOle create');

  if LRichEdit.QueryInterface(IID_ITextDocument, LDoc) <> 0 then
    raise Exception.Create('ITextDocument create');
  {$ENDREGION}

  LDoc.Freeze;
  try
    LLength := ARichEdit.GetTextLen;

    LOldAttribute := ARichEdit.DefAttributes;
    LDefaultColor := LOldAttribute.Color;
    Result  := '';

    for i := 0 to LLength - 1 do
    begin
      LRange := LDoc.Range(i, i + 1);
      LText := LRange.Text;

      // пропускаем непечатаемые символы
      if (Length(LText) > 0) and ((Ord(LText[1]) >= 32) or (LText = #13)) then begin
        if LText = #13 then
          Result := Result + '<BR/>'
        else begin
          LAttribute := LRange.Font;
          if LongWord(LAttribute.Color) = LongWord(tomAutocolor) then
            LAttribute.Color := LDefaultColor;

          if LAttribute <> LOldAttribute then
          begin
            if Result <> '' then
              Result := Result + GetCloseTag(LOldAttribute);

            Result := Result + GetOpenTag(LAttribute);
            LOldAttribute := LAttribute;
          end;

          Result := Result + _CharToHtml(LText[1]);
        end;
      end;
    end;
  finally
    LDoc.Unfreeze;
  end;
end;

class function TRtf2HtmlConverter.GetOpenTag(AAttribute: TFontProperty): string;
  var LColor: LongInt;
begin
  Result := '';

  if fsStrikeOut in AAttribute.Style then Result := Result + '<S>';
  if fsUnderline in AAttribute.Style then Result := Result + '<U>';
  if fsItalic in AAttribute.Style then Result := Result + '<I>';
  if fsBold in AAttribute.Style then Result := Result + '<B>';

  LColor := ColorToRGB(AAttribute.Color);
  Result := Result + Format
  (
    '<FONT face="%s" size="%g" color="#%.2x%.2x%.2x">',
    [
      AAttribute.Name,
      AAttribute.Size,
      GetRValue(LColor),
      GetGValue(LColor),
      GetBValue(LColor)
    ]
  )
end;

class function TRtf2HtmlConverter.GetCloseTag(AAttribute: TFontProperty): string;
begin
  Result := '</FONT>';

  if fsBold in AAttribute.Style       then Result := Result + '</B>';
  if fsItalic in AAttribute.Style     then Result := Result + '</I>';
  if fsUnderline in AAttribute.Style  then Result := Result + '</U>';
  if fsStrikeOut in AAttribute.Style  then Result := Result + '</S>';
end;

{$ENDREGION}

{$REGION 'TFontProperty'}

class operator TFontProperty.Equal(AFirst, ASecond: TFontProperty): Boolean;
begin
  Result := (AFirst.Color = ASecond.Color)
    and (AFirst.Name = ASecond.Name)
    and (AFirst.Size = ASecond.Size)
    and (AFirst.Style = ASecond.Style);
end;

class operator TFontProperty.Implicit(ATextAttribute: TTextAttributes): TFontProperty;
begin
  Result.Color := ATextAttribute.Color;
  Result.Name  := ATextAttribute.Name;
  Result.Size  := ATextAttribute.Size;
  Result.Style := ATextAttribute.Style;
end;

class operator TFontProperty.Implicit(AFont: ITextFont): TFontProperty;
begin
  Result.Color := AFont.ForeColor;
  Result.Size := AFont.Size;
  Result.Name := AFont.Name;

  Result.Style := [];

  if AFont.Weight = FW_BOLD         then Result.Style := Result.Style + [fsBold];
  if AFont.Italic <> tomNone        then Result.Style := Result.Style + [fsItalic];
  if AFont.StrikeThrough <> tomNone then Result.Style := Result.Style + [fsStrikeOut];
  if AFont.Underline <> tomNone     then Result.Style := Result.Style + [fsUnderline];
end;

class operator TFontProperty.NotEqual(AFirst, ASecond: TFontProperty): Boolean;
begin
  Result := not (AFirst = ASecond);
end;

{$ENDREGION}

end.
