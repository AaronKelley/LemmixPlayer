{$include lem_directives.inc}
unit LemEdit;

interface

uses
  Classes,
  LemLevel;

type
  TLevelEditor = class(TComponent)
  private
    fLevel: TLevel;
    procedure SetLevel(const Value: TLevel);
  protected
  public
    property Level: TLevel read fLevel write SetLevel;
  published
  end;

implementation

{ TLevelEditor }

procedure TLevelEditor.SetLevel(const Value: TLevel);
begin
  fLevel := Value;
end;

end.

