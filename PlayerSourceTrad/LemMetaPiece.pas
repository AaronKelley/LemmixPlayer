{$include lem_directives.inc}
unit LemMetaPiece;

interface

uses
  Classes;

type
  TMetaPiece = class
  private
  protected
    fWidth  : Integer;
    fHeight : Integer;
    procedure Error(const S: string); virtual;
  public
    procedure LoadFromStream(S: TStream); virtual;
  published
    property Width  : Integer read fWidth write fWidth;
    property Height : Integer read fHeight write fHeight;
  end;

implementation

uses
  Lemstrings, LemMisc;

{ TMetaPiece }

procedure TMetaPiece.Error(const S: string);
begin
  raise ELemmixError.Create(S);
end;

procedure TMetaPiece.LoadFromStream(S: TStream);
begin
  Error(SMetaPieceLoadError);
end;

end.

