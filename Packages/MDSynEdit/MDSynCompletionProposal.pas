unit MDSynCompletionProposal;

interface

uses
  System.RTTI,
  System.TypInfo,
  SynCompletionProposal,
  SysUtils,
  System.Classes;


type
  TMDSynCompletionProposalDfm = class(TSynCompletionProposal)
  private
    FRttiContext      : TRttiContext;
    FStartInsertList  : TStringList;
    FStartItemList    : TStringList;
    FClassesList      : TStringList;
    FMacroInsertList  : TStringList;
    FMacroItemList    : TStringList;

    procedure ClassFinded(AClass: TPersistentClass);
  public
    procedure ExecuteEx(s: UnicodeString; x, y: Integer; Kind : SynCompletionType
      {$IFDEF SYN_COMPILER_4_UP} = ctCode {$ENDIF}); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Types, Forms, Controls, ImgList, fs_iinterpreter, Graphics, LayoutPanel,
  StdCtrls, ExtCtrls, Buttons,
  ActnList, fs_ievents, Fs_iextctrlsrtti, ComCtrls, Variants,
  SynEdit,

  Messages, Windows, DB, Math, Excel_TLB, ComObj, StrUtils,

  cxTextEdit, cxDBEdit;

{ TMDSynCompletionProposalDfm }

procedure TMDSynCompletionProposalDfm.ClassFinded(AClass: TPersistentClass);
begin
  FClassesList.AddObject(AClass.ClassName, TObject(AClass));
end;

constructor TMDSynCompletionProposalDfm.Create(AOwner: TComponent);
var
  LClassFinder : TClassFinder;
begin
  inherited;
  FRttiContext := TRttiContext.Create;

  FStartInsertList  := TStringList.Create;
  FStartItemList    := TStringList.Create;
  FClassesList      := TStringList.Create;
  FMacroInsertList  := TStringList.Create;
  FMacroItemList    := TStringList.Create;

  Options := Options + [scoUsePrettyText, scoUseInsertList, scoLimitToMatchedText];

  FStartInsertList.Add('inherited');
  FStartItemList.Add('\style{+B}inherited\style{-B} AComponent: TComponent');

  FStartInsertList.Add('object');
  FStartItemList.Add('\style{+B}object\style{-B} AComponent: TComponent');

  FStartInsertList.Add('end');
  FStartItemList.Add('\style{+B}end\style{-B}');

  // -----
  FMacroInsertList.Add('{$IF }');
  FMacroItemList.Add('{\style{+B}$IF\style{-B}}');

  FMacroInsertList.Add('{$ELSE}');
  FMacroItemList.Add('{\style{+B}$ELSE\style{-B}}');

  FMacroInsertList.Add('{$END}');
  FMacroItemList.Add('{\style{+B}$END\style{-B}}');

  FMacroInsertList.Add('{$I }');
  FMacroItemList.Add('{\style{+B}$INCLUDE\style{-B}}');

  FMacroInsertList.Add('{$SELECT }');
  FMacroItemList.Add('{\style{+B}$SELECT\style{-B}}');

  FMacroInsertList.Add('{$SET}');
  FMacroItemList.Add('{\style{+B}$SET\style{-B}}');

  FMacroInsertList.Add('{$T <1>}');
  FMacroItemList.Add('{\style{+B}$TRANSLATE\style{-B}}');

  LClassFinder := TClassFinder.Create(nil);
  try
    LClassFinder.GetClasses(ClassFinded);
  finally
    LClassFinder.Free;
  end;

  FClassesList.Sort;
end;

destructor TMDSynCompletionProposalDfm.Destroy;
begin
  FreeAndNil(FStartInsertList);
  FreeAndNil(FStartItemList);
  FreeAndNil(FClassesList);

  FreeAndNil(FMacroInsertList);
  FreeAndNil(FMacroItemList);

  FRttiContext.Free;

  inherited;
end;

procedure TMDSynCompletionProposalDfm.ExecuteEx(s: UnicodeString; x, y: Integer;  Kind: SynCompletionType);

var
  LCurrentPos     : TBufferCoord;   // ѕозици€ картеки
  LRttiTPersistent: TRttiType;

  function _RttiIsInherits(AType, ABaseRttiType: TRttiType): Boolean;
  begin
    repeat
      Result := (AType = ABaseRttiType);
      AType := AType.BaseType;
    until (Result) or (not Assigned(AType));
end;

  procedure _PropArrayToStrings(AStrings : TStringList; APropsArray : TArray<TRttiProperty>);
  var
    LPropery : TRttiProperty;
  begin
    for LPropery in APropsArray do
      AStrings.AddObject(LPropery.Name, TObject(LPropery));
    AStrings.Sort;
  end;

  procedure _StringArrayToStrings(AStrings : TStringList; AStringsArray : TArray<String>);
  var
    LStr : String;
  begin
    for LStr in AStringsArray do
      AStrings.Add(LStr);
    AStrings.Sort;
  end;

  procedure _FillDropListFromPropList(ARttiType : TRttiType);
  var
    I           : Integer;
    LProperties : TArray<TRttiProperty>;
    LPropsList  : TStringList;
    LProperty   : TRttiProperty;
  begin
    // ¬ыпадающий список формируем из списка свойств класса
    // (используем отсортированный список APropList)
    LProperties := ARttiType.GetProperties;
    LPropsList := TStringList.Create;
    try

      LPropsList.Sorted := True;
      for LProperty in LProperties do
        if LPropsList.IndexOf(LProperty.Name) < 0 then
          LPropsList.AddObject(LProperty.Name, TObject(LProperty));

      if LPropsList.IndexOf('TopAfter') < 0 then
        LPropsList.AddObject('TopAfter', nil);
      if LPropsList.IndexOf('TabOrderAfter') < 0 then
        LPropsList.AddObject('TabOrderAfter', nil);

      for I:= 0 to LPropsList.Count - 1 do begin
        LProperty := TRttiProperty(LPropsList.Objects[I]);
        if Assigned(LProperty) then begin
          InsertList.Add(LProperty.Name);
          ItemList.Add('\style{+B}' + LProperty.Name + '\style{-B}\column{} : ' + LProperty.PropertyType.Name);
        end else begin
          InsertList.Add(LPropsList.Strings[I]);
          ItemList.Add('\style{+B}' + LPropsList.Strings[I] + '\style{-B}\column{}');
        end;
      end;

    finally
      LPropsList.Free;
    end;
  end;

  function _SeekPropertyByChainLine(ARttiType : TRttiType; ALine, ACharFrom, ACharTo: Integer): TRttiProperty;
  var
    LWord           : String;
    LPos            : TBufferCoord;
    LPropertyType   : TRttiType;
  begin
    LPos.Char := ACharFrom;
    LPos.Line := ALine;

    Result := nil;
    LWord := Editor.GetWordAtRowCol(LPos);
    while (LWord.IsEmpty) do begin
      Inc(LPos.Char);
      if LPos.Char >= ACharTo then
        Exit;
      LWord := Editor.GetWordAtRowCol(LPos);
    end;

    Result := ARttiType.GetProperty(LWord);
    if Result = nil then
      Exit;

    LPos := Editor.NextWordPosEx(LPos);
    if (LPos.Line > ALine) or ((LPos.Line = ALine) and (LPos.Char >= ACharTo)) then
      Exit;

    LPropertyType := Result.PropertyType;
    if _RttiIsInherits(LPropertyType, LRttiTPersistent) then
      Result := _SeekPropertyByChainLine(LPropertyType, ALine, LPos.Char, ACharTo);
  end;

  procedure _FillDropListByClass(ARttiType : TRttiType; ALine: Integer);
  var
    LProperty       : TRttiProperty;
    LEnumNames      : TArray<String>;
    LEnumList       : TStringList;
    // LIsChainProps   : Boolean;        // True - значит ожидаем, что работаем с цепочкой свойств
  begin
    LProperty := _SeekPropertyByChainLine(ARttiType, LCurrentPos.Line, 1, LCurrentPos.Char);
    // Ќашли свойсво внутри текущего класса
    if Assigned(LProperty) then begin

      if LProperty.PropertyType is TRttiEnumerationType then begin
        LEnumNames := (LProperty.PropertyType as TRttiEnumerationType).GetNames;
        LEnumList := TStringList.Create;
        try
          _StringArrayToStrings(LEnumList, LEnumNames);
          InsertList.AddStrings(LEnumList);
          ItemList.AddStrings(LEnumList);
        finally
          LEnumList.Free;
        end;

      end else if _RttiIsInherits(LProperty.PropertyType, LRttiTPersistent) then begin
        // —войство имеет субствойства, вот их и надо показать
        _FillDropListFromPropList(LProperty.PropertyType);
      end;

    end else begin
      // ¬ыпадающий список формируем из списка свойств класса
      _FillDropListFromPropList(ARttiType);
    end;
  end;

var
  LCBPos          : TBufferCoord;   // ѕозици€ каретки, потом смещаем в цикле на начало слов
  LWord           : String;         // —лово у смещаемой "картеки"
  LWord1          : String;         // слово следущее помле LWord
  LWord2          : String;         // слово следущее помле LWord1
  LPrevNo         : Integer;        // кол-во слов, которое перебрали двига€сь вверх
  LClassIndex     : Integer;
  LClass          : TComponentClass;

  LType           : TRttiType;
//  I               : Integer;
begin
  InsertList.Clear;
  ItemList.Clear;

  LRttiTPersistent :=  FRttiContext.GetType(TPersistent);
  try
    LPrevNo := 0;
    LCurrentPos := Editor.WordStart;
    LCBPos := LCurrentPos;
    while True do begin
      LWord2 := LWord1;
      LWord1 := LWord;
      if (LCBPos.Line >= Editor.Lines.Count - 1) then
        Break;
      if LCBPos.Char >= Editor.Lines[LCBPos.Line - 1].Length then
        LCBPos.Char := Editor.Lines[LCBPos.Line - 1].Length - 2;
      if LCBPos.Char < 1 then
        LCBPos.Char := 1;
      LWord := Editor.GetWordAtRowCol(LCBPos);

      if  ((LPrevNo = 1) or (LPrevNo = 2))
      and ((LWord = 'object') or (LWord = 'inherited')) then begin
          // ¬ выпадающий список добавл€ем список классов
        InsertList.AddStrings(FClassesList);
        ItemList.AddStrings(FClassesList);
        Exit; // Ѕез добавлени€ inherited + object + end (которые в FStartInsertList)
      end;

      if  (LPrevNo > 1)
      and ((LWord = 'object') or (LWord = 'inherited')) then begin
        LClassIndex := FClassesList.IndexOf(LWord2);
        if LClassIndex >= 0 then begin
          // јга! ћы находимс€ внутри класса LWord2
          // ¬ список свойства компонента
          LClass := TComponentClass(FClassesList.Objects[LClassIndex]);
          if Assigned(LClass) then begin
            LType := FRttiContext.GetType(LClass);
            _FillDropListByClass(LType, LCurrentPos.Line);
          end;
          Break;
        end;
      end;

      if LWord = 'end' then
        Break;

      if (LCBPos.Char <= 1) and (LCBPos.Line <= 1) then
        Break;

      LCBPos := Editor.PrevWordPosEx(LCBPos);
      if (LCBPos.Line > Editor.Lines.Count - 1) then
        Break;
      Inc(LPrevNo);
    end;

    InsertList.AddStrings(FStartInsertList);
    ItemList.AddStrings(FStartItemList);

    InsertList.AddStrings(FMacroInsertList);
    ItemList.AddStrings(FMacroItemList);

  finally
    if NbLinesInWindow > 28 then
      NbLinesInWindow := 28;

    inherited;
  end;
end;

initialization
  RegisterClasses([TMDSynCompletionProposalDfm]);

finalization
  UnRegisterClasses([TMDSynCompletionProposalDfm]);

end.

