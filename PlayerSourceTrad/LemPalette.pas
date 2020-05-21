{$include lem_directives.inc}
unit LemPalette;

interface

uses
  GR32;

type
  TLemmixPalette = class
  private
    fColorArray: TArrayOfColor32;
    fColorCount: Integer;
    procedure SetColorCount(Value: Integer);
    function GetColorS(Index: Integer): TColor32;
  protected
  public
    property ColorS[Index: Integer]: TColor32 read GetColorS;
    property ColorArray: TArrayOfColor32 read fColorArray;
    property ColorCount: Integer read fColorCount write SetColorCount;
  end;

implementation

{ TLemmixPalette }

function TLemmixPalette.GetColorS(Index: Integer): TColor32;
begin
  if Index < fColorCount then
    Result := fColorArray[Index]
  else
    Result := 0;  
end;

procedure TLemmixPalette.SetColorCount(Value: Integer);
begin
  if fColorCount = Value then
    Exit;
  SetLength(fColorArray, Value);
  fColorCount := Value;
end;

end.

