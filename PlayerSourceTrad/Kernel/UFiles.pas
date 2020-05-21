unit UFiles;

interface

(*
  2007-07-06
  o ReadDir and CreateFileList adjusted: The List parameter may be nil now.
  o Added easy-access properties to TSearchRecObject
  2005-11-11
  o DirectorySynchronizer weggegooid

//2003-06-12: ReadDir aangepast: FindClose toegevoegd
*)

uses
  Windows, Classes, Sysutils, Contnrs,
  UMisc, UTools;

{ extra file-attributen sinds windows NT, zie ook windows.pas en de help bij
  windows.getfileattributes }
const

(*
  FILE_ATTRIBUTE_READONLY             = $00000001;
  FILE_ATTRIBUTE_HIDDEN               = $00000002;
  FILE_ATTRIBUTE_SYSTEM               = $00000004;
  FILE_ATTRIBUTE_DIRECTORY            = $00000010;
  FILE_ATTRIBUTE_ARCHIVE              = $00000020;
  FILE_ATTRIBUTE_NORMAL               = $00000080;
  FILE_ATTRIBUTE_TEMPORARY            = $00000100;
  FILE_ATTRIBUTE_COMPRESSED           = $00000800;
  FILE_ATTRIBUTE_OFFLINE              = $00001000;
*)

  { sysutils
  faReadOnly   = $00000001; = bit0
  faHidden     = $00000002; = bit1
  faSysFile    = $00000004; = bit2
  faVolumeID   = $00000008; = bit3
  faDirectory  = $00000010; = bit4
  faArchive    = $00000020; = bit5
  faAnyFile    = $0000003F; = de vorige bij elkaar opgeteld
  }

  faNormal     = $00000080; { = bit 7 }
  faTemporary  = $00000100; { = bit 8 }
  faCompressed = $00000800; { = bit 11 }
  faOffLine    = $00001000; { = bit 12 }
  faNoIndex    = $00002000; { = bit 13 }
  faAllFiles   = faAnyFile + faNormal + faTemporary + faCompressed + faOffLine + faNoIndex;

  faFileMask = faAllFiles and not faDirectory;
  faDirMask  = faDirectory + faReadOnly + faHidden + faSysFile + faArchive +
    faNormal + faTemporary + faCompressed + faOffLine + faNoIndex;


type
  TSearchRecObject = class
  private
    fSearchRec: TSearchRec;
  public
    constructor Create(const ASearchRec: TSearchRec);
    property SearchRec: TSearchRec read fSearchRec;
  { easy access }
    property Time: Integer read fSearchRec.Time;
    property Size: Integer read fSearchRec.Size;
    property Attr: Integer read fSearchRec.Attr;
    property Name: TFileName read fSearchRec.Name;
    property ExcludeAttr: Integer read fSearchRec.ExcludeAttr;
    property FindHandle: THandle read fSearchRec.FindHandle;
    property FindData: TWin32FindData read fSearchRec.FindData;
  { }
    function IsDirectory: Boolean;
  end;

type
  { stringlist met TSearchRecObjecten eraan vast }
  TFileList = class(TStringList)
  public
//    procedure AddFile(const aFileName: string);
  end;


{ file functies }
type
  TAddSearchRecEvent = procedure(const ASearchRec: TSearchRec; const aFullName: string; out DoAdd: boolean;
   out AObject: TObject) of object;

function ValidFileName(const S: string): boolean;

function IsRel(const R: TSearchRec): boolean;
function IsDir(const R: TSearchRec): boolean;
function IsDirectory(const aFileName: string): boolean;

procedure CreateFileList(AList: TStrings;//List;
                         const Path: string;
                         Attribs: integer;
                         FullFileName: boolean = True;
                         Recursive: boolean = False;
                         OnAddSearchRec: TAddSearchRecEvent = nil);
    { Maakt een stringlist van de aanwezige files, geef <path> inclusief wildcard
      Voegt TSearchRecObjects toe als AddObjects = True }

function AttribStr(Attrib: integer; Points: boolean = False): string;

function GetSearchRec(const aFileName: string): TSearchRec;

implementation

{ TSearchRecObject }

constructor TSearchRecObject.Create(const ASearchRec: TSearchRec);
begin
  inherited Create;
  FSearchRec := ASearchRec;
end;

function TSearchRecObject.IsDirectory: Boolean;
begin
  Result := Attr and faDirectory <> 0;
end;

function GetSearchRec(const aFileName: string): TSearchRec;
begin
  FillChar(Result, SizeOf(Result), 0);
  try
    FindFirst(aFileName, faAllFiles, Result);
  finally
    FindClose(Result)
  end;
end;

function ValidFileName(const S: string): boolean;
const
  InvChars: string = '\/:*?"<>|';
var
  i: integer;
  F: string;
begin
  Result := False;

  if Length(S) = 0 then Exit;
  for i := 4 to Length(InvChars) do
    if Pos(InvChars[i], S) > 0 then
      Exit;

  F := ExtractFileName(S);
  if Length(F) = 0 then Exit;
  for i := 1 to Length(InvChars) do
    if Pos(InvChars[i], F) > 0 then
      Exit;

  Result := True;
end;

function IsRel(const R: TSearchRec): boolean;
begin
  Result := (R.Name = '.') or (R.Name = '..');
end;

function IsDir(const R: TSearchRec): boolean;
begin
  Result := (R.Attr and faDirectory <> 0) and not IsRel(R)
end;

function IsDirectory(const aFileName: string): boolean;
var
  sr: TSearchRec;
begin
  Result := False;
  if FindFirst(aFileName, faAllFiles, sr) = 0 then
  begin
    Result := IsDir(sr);
    FindClose(sr);
  end;
end;

procedure ReadDir(const Path: string;
                  Attribs: integer;
                  FullFileNames: boolean;
                  Recursive: boolean;
                  OnAddSearchRec: TAddSearchRecEvent;
                  List: TStrings);

{-------------------------------------------------------------------------------
  2007-07-06: List parameter can be nil. Then this procedure can be used to
  scan files only.
-------------------------------------------------------------------------------}

var
  sr: TSearchRec;
  LFN, LFP, WildCard: string;
  O: TObject;
  DoAdd: boolean;

    procedure AddFile;

        function GetName: string;
        begin
          if FullFileNames then Result := LFP + sr.Name else Result := sr.Name;
        end;

    begin
      if not Assigned(OnAddSearchRec) then
      begin
        if Assigned(List) then
          if (sr.Attr and Attribs <> 0) then // ### toegevoegd 2005-01-10
             List.Add(GetName)
      end
      else begin
        if (sr.Attr and Attribs <> 0) then // ### toegevoegd 2005-01-10
        begin
          DoAdd := True;
          OnAddSearchRec(sr, LFP + sr.Name, DoAdd, O); // fire event
          if Assigned(List) then
            if DoAdd then
              List.AddObject(GetName, O);
        end;
      end;
      //Log([GetName]);
    end;

begin
  LFN := ExpandFileName(Path);
  LFP := ExtractFilePath(LFN); { is altijd met backslash }
  WildCard := ExtractFileName(LFN);
//  DirAttribs := Attribs;

  { recursief inlezen }
  // ## EL veranderd 2005-01-10: faDirectory ipv faAllFiles
  // ## EL veranderd 2005-01-14: weer terugveranderd in faAllFiles
  if Recursive and (FindFirst(LFP + '*.*', faAllFiles, sr) = 0) then
  try
    repeat
      if IsDir(sr) then
      begin
//        if (faDirectory and Attribs <> 0) then
  //        AddFile;
        ReadDir(LFP + sr.Name + '\' + WildCard, Attribs, FullFileNames, Recursive, OnAddSearchRec, List);
      end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr); // 2003-06-12: toegevoegd, dit was een bug!
  end;

  { toevoegen }
  if FindFirst(LFN, Attribs, sr) = 0 then
  try //2003-06-12 try finally statement toegevoegd
    repeat
      if not IsRel(sr) then
        AddFile
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;


{ file functions }
procedure CreateFileList(aList: TStrings;
                         const Path: string;
                         Attribs: integer;
                         FullFileName: boolean = True;
                         Recursive: boolean = False;
                         OnAddSearchRec: TAddSearchRecEvent = nil);
begin
  ReadDir(Path, Attribs, FullFileName, Recursive, OnAddSearchRec, aList);
end;

function AttribStr(Attrib: integer; Points: boolean = False): string;
//var
  //i: integer;
begin
  Result := '';
  case Points of
    True:
      begin
        Result := Result + IIF(Attrib and faReadOnly   = 0, '.', 'R');
        Result := Result + IIF(Attrib and faHidden     = 0, '.', 'H');
        Result := Result + IIF(Attrib and faSysfile    = 0, '.', 'S');
        Result := Result + IIF(Attrib and faVolumeID   = 0, '.', 'V');
        Result := Result + IIF(Attrib and faDirectory  = 0, '.', 'D');
        Result := Result + IIF(Attrib and faArchive    = 0, '.', 'A');
        Result := Result + IIF(Attrib and faNormal     = 0, '.', 'N');
        Result := Result + IIF(Attrib and faTemporary  = 0, '.', 'T');
        Result := Result + IIF(Attrib and faCompressed = 0, '.', 'C');
        Result := Result + IIF(Attrib and faOffLine    = 0, '.', 'O');
        Result := Result + IIF(Attrib and faNoIndex    = 0, '.', 'X');
      end;
    False:
      begin
        Result := Result + IIF(Attrib and faReadOnly   = 0, '', 'R');
        Result := Result + IIF(Attrib and faHidden     = 0, '', 'H');
        Result := Result + IIF(Attrib and faSysfile    = 0, '', 'S');
        Result := Result + IIF(Attrib and faVolumeID   = 0, '', 'V');
        Result := Result + IIF(Attrib and faDirectory  = 0, '', 'D');
        Result := Result + IIF(Attrib and faArchive    = 0, '', 'A');
        Result := Result + IIF(Attrib and faNormal     = 0, '', 'N');
        Result := Result + IIF(Attrib and faTemporary  = 0, '', 'T');
        Result := Result + IIF(Attrib and faCompressed = 0, '', 'C');
        Result := Result + IIF(Attrib and faOffLine    = 0, '', 'O');
        Result := Result + IIF(Attrib and faNoIndex    = 0, '', 'X');
      end;
  end;

{  for i := 0 to 31 do
   Result := Result + IIF(attrib and (1 shl i) > 0, '1', '.');
   exit;}
end;

end.


