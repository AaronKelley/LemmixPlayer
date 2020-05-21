{$include lem_directives.inc}

unit LemDosMisc;

interface

uses
  GR32,
  LemDosStructures;

procedure DosVgaPalette8ToLemmixPalette(DosPal: TDosVGAPalette8;
  var LemmixPal: TArrayOfColor32);

implementation

procedure DosVgaPalette8ToLemmixPalette(DosPal: TDosVGAPalette8;
  var LemmixPal: TArrayOfColor32);
// 6 --> 8 bit color conversions
var
  i: Integer;
  E: PColor32Entry;
  D: PDosVgaColorRec;
begin
  SetLength(LemmixPal, 8);
  for i := 0 to 7 do
  begin
    E := @LemmixPal[i];
    D := @DosPal[i];
    with TColor32Entry(LemmixPal[i]) do
    begin
      E^.A := 0;
      E^.R := (Integer(D^.R) * 255) div 63;
      E^.G := (Integer(D^.G) * 255) div 63;
      E^.B := (Integer(D^.B) * 255) div 63;
    end;
  end;
end;


end.

