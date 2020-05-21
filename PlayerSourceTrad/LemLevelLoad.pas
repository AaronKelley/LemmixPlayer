{$include lem_directives.inc}
unit LemLevelLoad;

interface

uses
  Classes, SysUtils,
  LemLevel;

type
  // tlevelloadinfo = style, file etc.


  TLevelLoader = class
  private
  protected
  public
    class procedure LoadLevelFromFile(const aFileName: string; aLevel: TLevel);
    class procedure SaveLevelToFile(aLevel: TLevel; const aFileName: string);
    class procedure LoadLevelFromStream(aStream: TStream; aLevel: TLevel; OddLoad: Boolean = false); virtual; abstract;
    class procedure StoreLevelInStream(aLevel: TLevel; aStream: TStream); virtual; abstract;

//    class procedure LoadLevel(LoadInfo: ILevelLoadInfo); virtual;
  published
  end;

implementation

{ TLevelLoader }

class procedure TLevelLoader.LoadLevelFromFile(const aFileName: string; aLevel: TLevel);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFileName, fmOpenRead);
  try
    LoadLevelFromStream(F, aLevel);
  finally
    F.Free;
  end;
end;

class procedure TLevelLoader.SaveLevelToFile(aLevel: TLevel; const aFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(aFileName, fmCreate);
  try
    StoreLevelInStream(aLevel, F);
  finally
    F.Free;
  end;
end;


end.

