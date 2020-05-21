{$include lem_directives.inc}
unit LemSteel;

interface

uses
  LemPiece;

type
  TSteelClass = class of TSteel;
  TSteel = class(TSizedPiece)
  end;

type
  TSteels = class(TPieces)
  private
    function GetItem(Index: Integer): TSteel;
    procedure SetItem(Index: Integer; const Value: TSteel);
  protected
  public
    constructor Create(aItemClass: TSteelClass);
    function Add: TSteel;
    function Insert(Index: Integer): TSteel;
    property Items[Index: Integer]: TSteel read GetItem write SetItem; default;
  end;

implementation


{ TSteels } 

function TSteels.Add: TSteel; 
begin 
  Result := TSteel(inherited Add); 
end; 

constructor TSteels.Create(aItemClass: TSteelClass);
begin
  inherited Create(aItemClass);
end; 

function TSteels.GetItem(Index: Integer): TSteel; 
begin 
  Result := TSteel(inherited GetItem(Index)) 
end; 

function TSteels.Insert(Index: Integer): TSteel; 
begin 
  Result := TSteel(inherited Insert(Index)) 
end; 

procedure TSteels.SetItem(Index: Integer; const Value: TSteel);
begin
  inherited SetItem(Index, Value);
end;


end.

