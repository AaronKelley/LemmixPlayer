{$include lem_directives.inc}

unit LemMusicSystem;

interface

uses
  SysUtils;

type
  TBaseMusicSystem = class
  private
  protected
  public
    function GetMusicFileName(aPack, aSection, aLevel: Integer): string; virtual;
  end;

implementation

{ TBaseMusicSystem }

function TBaseMusicSystem.GetMusicFileName(aPack, aSection, aLevel: Integer): string;
begin
  raise Exception.Create('abstract music system');
end;

end.

