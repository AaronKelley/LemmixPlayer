unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
(*ulog,*) umisc, ucodes, uwintools,  Dialogs,uzip, StdCtrls,ufiles;

type
  TForm1 = class(TForm)
    BtnDataOrig: TButton;
    BtnDataOhNo: TButton;
    BtnDataHoliday94: TButton;
    BtnDataCust: TButton;

    BtnMusicOrig: TButton;
    BtnMusicOhno: TButton;
    BtnMusicCust: TButton;
    BtnMusicHoliday94: TButton;


    BtnSoundsOrig: TButton;
    BtnDataCovox: TButton;
    BtnDataPrima: TButton;
    BtnMusicCovox: TButton;
    BtnMusicPrima: TButton;
    BtnDataXmas: TButton;
    BtnMusicXmas: TButton;
    BtnDataExtra: TButton;
    BtnMusicExtra: TButton;
    btnFlexiData: TButton;
    btnFlexiMusic: TButton;
    btnBassDLL: TButton;

    procedure BtnBassmodClick(Sender: TObject);

    procedure BtnDataClick(Sender: TObject);

    procedure BtnSoundsOrigClick(Sender: TObject);

    procedure BtnMusicOrigClick(Sender: TObject);
    procedure BtnMusicOhNoClick(Sender: TObject);
    procedure BtnMusicH94Click(Sender: TObject);

    procedure BtnRandomCodesOrigClick(Sender: TObject);
    procedure BtnRandomCodesOhNoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnExplodeArrayClick(Sender: TObject);
  private
    procedure CreateDatResource(const aSourcePath: string);

    procedure GenCodes(SectionCount, LevelCount: Integer; const ArrayName: string);
    procedure GenCodes2(SectionCount, LevelCount: Integer; const ArrayName: string);
    procedure CreateParticleResource(const aSourcePath: string);


//    procedure DoCreateArcResource(const aResName, aResType: string)
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;


implementation

{$R *.dfm}

const
  TAG_ORIG = 1;
  TAG_OHNO = 2;
  TAG_H94  = 3;
  TAG_CUST = 4;

  TAG_COVOX = 9;
  TAG_PRIMA = 10;
  TAG_XMAS = 11;
  TAG_EXTRA = 12;
  TAG_FLEXI = 13;


procedure TForm1.FormCreate(Sender: TObject);
begin
  //setdebproc(ulog.deb);
  BtnDataOrig.Tag      := TAG_ORIG;
  BtnDataOhNo.Tag      := TAG_OHNO;
  BtnDataHoliday94.Tag := TAG_H94;
  BtnDataCust.Tag      := TAG_CUST;

  BtnDataCovox.Tag     := TAG_COVOX;
  BtnDataPrima.Tag     := TAG_PRIMA;
  BtnDataXmas.Tag      := TAG_XMAS;
  BtnDataExtra.Tag     := TAG_EXTRA;
  BtnFlexiData.Tag     := TAG_FLEXI;



  BtnMusicOrig.Tag     := TAG_ORIG;
  BtnMusicOhno.Tag     := TAG_OHNO;
  BtnMusicHoliday94.Tag := TAG_H94;
  BtnMusicCust.Tag     := TAG_CUST;

  BtnMusicCovox.Tag     := TAG_COVOX;
  BtnMusicPrima.Tag     := TAG_PRIMA;
  BtnMusicXmas.Tag      := TAG_XMAS;
  BtnMusicExtra.Tag     := TAG_EXTRA;

end;

procedure TForm1.CreateDatResource(const aSourcePath: string);
{-------------------------------------------------------------------------------
  creates a zipped resourcefile of all *.dat files in <aSourcePath>
-------------------------------------------------------------------------------}
var
  Z: TArchive;
  Command, Param, ZipFileName, ScriptFileName: string;
begin
  ZipFileName    := GetApplicationPath + aSourcePath + 'lemdata.arc';
  ScriptFileName := GetApplicationPath + aSourcePath + 'lemdata.rc';

  DeleteFile(ZipFileName);
  DeleteFile(ScriptFileName);
  DeleteFile(ReplaceFileExt(ZipFileName, '.res'));

  Z := TArchive.Create;
  Z.OpenArchive(ZipFileName, amCreate);
  Z.ZipOptions := [];
  Z.AddFiles(IncludeTrailingBackslash(GetApplicationPath + aSourcePath) + '*.dat');
  Z.Free;

  StringToFile('LANGUAGE 9, 5' + CrLf + 'LEMDATA' + ' ' + 'ARCHIVE ' + '"' + ZipFileName + '"', ScriptFileName);

  // compileer tot resource met brcc32.exe
  DoShellExecute('C:\Program Files (x86)\Borland\Delphi7\Bin\brcc32.exe', '"' + ScriptFileName + '"', '');
end;

procedure TForm1.BtnDataClick(Sender: TObject);
var
  Path: string;
  But: TButton absolute Sender;
begin
  if not (Sender is TButton) then
    Exit;
  Path := '';
  case But.Tag of
    TAG_ORIG : Path := 'data\orig\';
    TAG_OHNO : Path := 'data\ohno\';
    TAG_H94  : Path := 'data\h94\';
    TAG_CUST : Path := 'data\cust\';

    TAG_COVOX : Path := 'data\covox\';
    TAG_PRIMA : Path := 'data\prima\';
    TAG_XMAS : Path := 'data\xmas\';
    TAG_EXTRA : Path := 'data\extra\';
    TAG_FLEXI : Path := 'data\flexi\';

  else
    raise Exception.Create('unknown button tag')
  end;
  CreateDatResource(Path);
end;

procedure TForm1.BtnSoundsOrigClick(Sender: TObject);
var
  Z: TArchive;
  Command, Param, ZipFileName, ScriptFileName: string;
begin
  ZipFileName    := GetApplicationPath + 'lemsounds.arc';
  ScriptFileName := GetApplicationPath + 'lemsounds.rc';

  DeleteFile(ZipFileName);
  DeleteFile(ScriptFileName);
  DeleteFile(ReplaceFileExt(ZipFileName, '.res'));

  Z := TArchive.Create;
  Z.OpenArchive(ZipFileName, amCreate);
  Z.ZipOptions := [];
  Z.AddFiles('sounds\win\*.wav');
  Z.Free;

  StringToFile('LANGUAGE 9, 5' + CrLf + 'LEMSOUNDS' + ' ' + 'ARCHIVE ' + '"' + ZipFileName + '"', ScriptFileName);

  // compileer tot resource met brcc32.exe
  DoShellExecute('C:\Program Files (x86)\Borland\Delphi7\Bin\brcc32.exe', '"' +
                 ScriptFileName + '"', '');

end;

procedure TForm1.BtnMusicOrigClick(Sender: TObject);
var
  Z: TArchive;
  Command, Param, Path, ZipFileName, ScriptFileName: string;
  But: TButton absolute Sender;
  SRec: TSearchRec;
begin

  if not (Sender is TButton) then
    Exit;
  Path := '';
  case But.Tag of
    TAG_ORIG : Path := 'music\orig\';
    TAG_OHNO : Path := 'music\ohno\';
    TAG_H94  : Path := 'music\h94\';
    TAG_CUST : Path := 'music\cust\';

    TAG_COVOX : Path := 'music\covox\';
    TAG_PRIMA : Path := 'music\prima\';
    TAG_XMAS : Path := 'music\xmas\';
    TAG_EXTRA : Path := 'music\extra\';
    TAG_FLEXI : Path := 'music\flexi\';

  else
    raise Exception.Create('unknown button tag')
  end;

  ZipFileName    := GetApplicationPath + Path + 'lemmusic.arc';
  ScriptFileName := GetApplicationPath + Path + 'lemmusic.rc';
  Z := TArchive.Create;
  Z.OpenArchive(ZipFileName, amCreate);
  Z.ZipOptions := [];
  if FindFirst(GetApplicationPath + Path + 'track_??.ogg', 0, SRec) = 0 then
  begin
    Z.AddFiles(GetApplicationPath + Path + 'track_??.ogg');
    FindClose(SRec);
  end;
  if FindFirst(GetApplicationPath + Path + 'track_??.it', 0, SRec) = 0 then
  begin
    Z.AddFiles(GetApplicationPath + Path + 'track_??.it');
    FindClose(SRec);
  end;
  //if FileExists(Path + 'frenzy.it') then Z.AddFiles(Path + 'frenzy.it');
  //if FileExists(Path + 'gimmick.it') then Z.AddFiles(Path + 'gimmick.it');
  Z.Free;

  StringToFile('LANGUAGE 9, 5' + CrLf + 'LEMMUSIC' + ' ' + 'ARCHIVE ' + '"' + ZipFileName + '"', ScriptFileName);

  // compileer tot resource met brcc32.exe
  DoShellExecute('C:\Program Files (x86)\Borland\Delphi7\Bin\brcc32.exe', '"' +
                 ScriptFileName + '"', '');
end;

procedure TForm1.BtnMusicOhNoClick(Sender: TObject);
var
  Z: TArchive;
  Command, Param, ZipFileName, ScriptFileName: string;
begin
  ZipFileName    := GetApplicationPath + 'lemmusic.arc';
  ScriptFileName := GetApplicationPath + 'lemmusic.rc';

  DeleteFile(ZipFileName);
  DeleteFile(ScriptFileName);
  DeleteFile(ReplaceFileExt(ZipFileName, '.res'));

  Z := TArchive.Create;
  Z.OpenArchive(ZipFileName, amCreate);
  Z.ZipOptions := [];
  Z.AddFiles('music\ohno\track_??.it');
  Z.Free;

  StringToFile('LANGUAGE 9, 5' + CrLf + 'LEMMUSIC' + ' ' + 'ARCHIVE ' + '"' + ZipFileName + '"', ScriptFileName);

  // compileer tot resource met brcc32.exe
  DoShellExecute('C:\Program Files (x86)\Borland\Delphi7\Bin\brcc32.exe', '"' +
                 ScriptFileName + '"', '');
end;

procedure TForm1.BtnMusicH94Click(Sender: TObject);
var
  Z: TArchive;
  Command, Param, ZipFileName, ScriptFileName: string;
begin
  ZipFileName    := GetApplicationPath + 'lemmusic.arc';
  ScriptFileName := GetApplicationPath + 'lemmusic.rc';

  DeleteFile(ZipFileName);
  DeleteFile(ScriptFileName);
  DeleteFile(ReplaceFileExt(ZipFileName, '.res'));

  Z := TArchive.Create;
  Z.OpenArchive(ZipFileName, amCreate);
  Z.ZipOptions := [];
  Z.AddFiles('music\h93\track_??.it');
  Z.Free;

  StringToFile('LANGUAGE 9, 5' + CrLf + 'LEMMUSIC' + ' ' + 'ARCHIVE ' + '"' + ZipFileName + '"', ScriptFileName);

  // compileer tot resource met brcc32.exe
  DoShellExecute('C:\Program Files (x86)\Borland\Delphi7\Bin\brcc32.exe', '"' +
                 ScriptFileName + '"', '');
end;

procedure TForm1.BtnBassmodClick(Sender: TObject);
// this one is not zipped
var
  Z: TArchive;
  Command, Param, ZipFileName, ScriptFileName: string;
begin
  ZipFileName    := GetApplicationPath + 'bass.arc';
  ScriptFileName := GetApplicationPath + 'bass.rc';
  {
  Z := TArchive.Create;
  Z.OpenArchive(ZipFileName, amCreate);
  Z.ZipOptions := [];
  Z.AddFile('bassmod.dll');
  Z.Free;
  }

  StringToFile('LANGUAGE 9, 5' + CrLf +
               'bass ARCHIVE "bass.dll"',
               ScriptFileName);
  // compileer tot resource met brcc32.exe
  DoShellExecute('C:\Program Files (x86)\Borland\Delphi7\Bin\brcc32.exe', '"' +
                 ScriptFileName + '"', '');

end;

procedure TForm1.BtnRandomCodesOrigClick(Sender: TObject);
begin
//  GenCodes(4, 30, 'DosOrigLevelCodes');
  GenCodes2(4, 30, 'DosOrigLevelCodes');
end;

procedure TForm1.GenCodes(SectionCount, LevelCount: Integer; const ArrayName: string);
{-------------------------------------------------------------------------------
  generates random codes
-------------------------------------------------------------------------------}
var
  L: TStringList;
  Sec, Lev, i: Integer;
  r: Integer;
  c : Char;
  S: string;
begin
  Randomize;
//  randseed := -1207816797; // so we do not need consts
  S := StringOfChar(' ', 10);
  L := TStringlist.Create;
  L.Add('// randseed = ' + i2s(randseed));
  L.Add('var');
  L.Add(ArrayName + ': array[0..3, 0..29] of string = ');
  L.Add('  (');
  for Sec := 1 to SectionCount do
  begin
    L.Add ('    (');
    for Lev := 1 to LevelCount do
    begin
      for i := 1 to 10 do
      begin
        r := Random(26);
        c := Chr(r + ord('A'));
        S[i] := c;
      end;
      L.Add('    ' + '''' + S + '''' + IIF(Lev < LevelCount, ',', ''));
    end;
    L.Add ('    )' + IIF(Sec < SectionCount, ',', ''));
  end;
  L.Add('  );');

  L.SaveToFile('codes.txt');
  L.Free;
end;

procedure TForm1.GenCodes2(SectionCount, LevelCount: Integer; const ArrayName: string);
{-------------------------------------------------------------------------------
  generates random codes with alternating nouns non-nouns
-------------------------------------------------------------------------------}
const
  klinkers: array[0..4] of char = ('A','E','I','O','U');
  medeklinkers: array[0..20] of char = ('B','C','D','F','G','H','J','K','L','M','N','P','Q','R',
   'S','T','V','W','X','Y','Z');

  function RndChar(aMedeklinker: Boolean): Char;
  begin
    if aMedeklinker then
      Result := Medeklinkers[Random(21)]
    else
      Result := Klinkers[Random(5)];
  end;

var
  L: TStringList;
  Sec, Lev, i: Integer;
  r: Integer;
  c : Char;
  S: string;
  DoMedeKlinker: Boolean;
begin
  Randomize;
//  randseed := -1207816797; // so we do not need consts
  S := StringOfChar(' ', 10);
  L := TStringlist.Create;
  L.Add('// randseed = ' + i2s(randseed));
  L.Add('var');
  L.Add(ArrayName + ': array[0..3, 0..29] of string = ');
  L.Add('  (');
  for Sec := 1 to SectionCount do
  begin
    L.Add ('    (');
    for Lev := 1 to LevelCount do
    begin
      DoMedeKlinker := Boolean(Random(2)); // init on random
      for i := 1 to 10 do
      begin
        r := Random(26);
        //c := Chr(r + ord('A'));
        C := RndChar(DoMedeKlinker);
        DoMedeKlinker := not DoMedeKlinker;
        S[i] := c;
      end;
      L.Add('    ' + '''' + S + '''' + IIF(Lev < LevelCount, ',', ''));
    end;
    L.Add ('    )' + IIF(Sec < SectionCount, ',', ''));
  end;
  L.Add('  );');

  L.SaveToFile('codes.txt');
  L.Free;
end;

procedure TForm1.BtnRandomCodesOhNoClick(Sender: TObject);
begin
  GenCodes(5, 20, 'DosOhnoLevelCodes');
end;

procedure TForm1.CreateParticleResource(const aSourcePath: string);
{-------------------------------------------------------------------------------
  creates a zipped resourcefile of all *.dat files in <aSourcePath>
-------------------------------------------------------------------------------}
var
  Z: TArchive;
  Command, Param, ZipFileName, ScriptFileName: string;
begin
  ZipFileName    := GetApplicationPath + 'explode.arc';
  ScriptFileName := GetApplicationPath + 'explode.rc';

  DeleteFile(ZipFileName);
  DeleteFile(ScriptFileName);
  DeleteFile(ReplaceFileExt(ZipFileName, '.res'));

  Z := TArchive.Create;
  Z.OpenArchive(ZipFileName, amCreate);
  Z.ZipOptions := [];
  Z.AddFile(IncludeTrailingBackslash(aSourcePath) + 'explode.dat');
  Z.Free;

  StringToFile('LANGUAGE 9, 5' + CrLf + 'LEMPARTICLES' + ' ' + 'ARCHIVE ' + '"' + ZipFileName + '"', ScriptFileName);

  // compileer tot resource met brcc32.exe
  DoShellExecute('C:\Program Files (x86)\Borland\Delphi7\Bin\brcc32.exe', '"' + ScriptFileName + '"', '');
end;

procedure TForm1.BtnExplodeArrayClick(Sender: TObject);
var
  F: TFileStream;
  T: TextFile;
  i, p, x, y: Integer;
  S: string;//ShortInt;
type
  TPixRec = packed record
    DX, DY: ShortInt
  end;
  TPixels = packed array[0..79] of TPixRec;
  TTable = packed array[0..50] of TPixels;

var
  Table: TTable;

begin

  createparticleresource('data\');

  Exit;

  AssignFile(T, 'explodearray.txt');
  Rewrite(T);
  F := TFileStream.Create('explode.dat', fmOpenRead);
  try
    Log([F.Size, SizeOf(Table)]);
    if F.Size = SizeOf(Table) then
      F.Read(Table, SizeOf(Table));

    writeln(t, 'const');
    writeln(t, '  GlobalParticles: TParticleTable = (');

    for i := 0 to 50 do
    begin
      writeln(T, '  // particles for frame ' + leadzerostr(i,2) );
      for p := 0 to 79 do
      begin
        S := '  (DX:' + PadL(i2s(Table[p][i].DX), 5) + '; ' + 'DY:' + PadL(i2s(Table[p][i].DY), 5) + '), ';
        write(T,S);

//        if p > 0 then
          if (p + 1) mod 5 = 0 then
            writeln(T);
      end;
      writeln(T);
      if i < 50 then
        writeln(T);
    end;

    writeln(t, ');');




    (*
    for i := 0 to F.Size - 1 do
    begin
      F.Read(S, 1);
      Write(T, IntToStr(S), ',');
      if i > 0 then
        if i mod 32 = 0 then
          WriteLn(T);
    end;
    *)
  finally
    F.Free;
    CloseFile(T);
  end;
  windlg('ready');
end;

end.

