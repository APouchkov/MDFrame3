unit SBaseStorage;

interface

uses
  Classes, Controls;

type
  TSBaseStorage_StoredProcedures = class(TComponent)
  private
    FSaveUserConfig: String;
    FLoadUserConfig: String;
  public
//    constructor Create(AOwner: TComponent); override;
  published
    property SaveUserConfig: String read FSaveUserConfig write FSaveUserConfig;
    property LoadUserConfig: String read FLoadUserConfig write FLoadUserConfig;
  end;

  TSBaseStorage_Images_Document = class(TComponent)
  private
    FNew    : Integer;
    FView   : Integer;
    FEdit   : Integer;
    FDelete : Integer;
    FMarkAsDeleted, FUnMarkDeleted: Integer;
    FNew2   : Integer;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property New            : Integer read FNew           write FNew;
    property View           : Integer read FView          write FView;
    property Edit           : Integer read FEdit          write FEdit;
    property Delete         : Integer read FDelete        write FDelete;
    property MarkAsDeleted  : Integer read FMarkAsDeleted write FMarkAsDeleted;
    property UnMarkDeleted  : Integer read FUnMarkDeleted write FUnMarkDeleted;
    property New2           : Integer read FNew2          write FNew2;
  end;

  TSBaseStorage_Images = class(TComponent)
  private
    FList: TImageList;

    FDocument: TSBaseStorage_Images_Document;
  published
    property List: TImageList read FList write FList;

    property Document: TSBaseStorage_Images_Document read FDocument write FDocument;
  end;

  TSBaseStorage = class(TComponent)
  private
    FStoredProcedures: TSBaseStorage_StoredProcedures;
    FImages: TSBaseStorage_Images;
  published
    property StoredProcedures: TSBaseStorage_StoredProcedures read FStoredProcedures write FStoredProcedures;
    property Images: TSBaseStorage_Images read FImages write FImages;
  end;

implementation

{ TSBaseStorage_StoredProcedures }

{
constructor TSBaseStorage_StoredProcedures.Create(AOwner: TComponent);
begin
  inherited;
//
end;
}

{ TSBaseStorage_Images_Document }

constructor TSBaseStorage_Images_Document.Create(AOwner: TComponent);
begin
  inherited;

  FNew            := -1;
  FView           := -1;
  FEdit           := -1;
  FDelete         := -1;
  FMarkAsDeleted  := -1;
  FUnMarkDeleted  := -1;

  FNew2           := -1;
end;

end.
