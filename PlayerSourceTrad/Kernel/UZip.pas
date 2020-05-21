
{**************************************************************}
{                                                              }
{    (c) Eric Langedijk                                        }
{                                                              }
{    TArchive classen                                          }
{                                                              }
{**************************************************************}

unit UZip;

interface

{ Wijzigingen (zie ook UZipLib.pas)

  2007-12-07
  o GetTotalFileBytes toegevoegd t.b.v. juiste OnProgress statistiek
  o Alle Extract routines nagelopen. Bij het starten van extractie wordt nu
    altijd het juiste aantal totaalbytes (originele bestandsgrootte) ge-initialiseerd
    in het progress-record. In de extractfile-routines wordt nu altijd het aantal geschreven
    bytes aan DoProgress toegevoegd

  2005-03-17
  o Twee methods toegevoegd om een extract naar een stream te kunnen doen.
    -InternalExtractFile(aIndex: Integer; aDestStream: TStream); overload;
    -ExtractFile(aIndex: Integer; aDestStream: TStream); overload;
    Bij eerste test lijkt het goed te werken. Moet dus nog beter

  2005-11-11
  o Begonnen met het slopen van iedere referentie naar Forms.pas
    om de DLL kleiner te krijgen. dat is uiteindlijk gelukt.
    Dit is gelukt. De DLL is nog 148 KB ipv 396 KB.
  o Confirm event toegevoegd. DoConfirm aangepast.
  o Extra ziperror toegevoegd: ERR_ABORTED (wanneer DoConfirm mrAbort teruggeeft)

  2005-11-10
  o Laden van archief uit resource mogelijk gemaakt:
    TZipStream flexibeler gemaakt door hem een TVolatileStream als ancestor
    te geven.
  o Als gevolg hiervan in TArchive wijzigingen aangebracht. De belangrijkste
    toevoeging is de method OpenResource.

  2005-09-11
  o Form FOverWrite weggehaald: DoConfirm moet nog aangepast
  o Begonnen met preciezere progress

  2003-07-06
  o Bestandsstructuur aangepast om flexibeler te zijn voor toekomstige
    compatibiliteit. TZipHeader en TFileHeader aangevuld met reserved bytes.

  2003-06-09:
  o TArchive.InternalExtractFile aangepast:
    Ook het bestandsattribuut hidden erbij gezet (zie ook 2003-06-05).

  2003-06-07:
  o TArchive.FileName wordt genegeerd in InternalAddFile.
    Bij AddFiles(Wildcard) evt. warning.

  2003-06-05:
  o TArchive.InternalExtractFile aangepast: Wanneer een bestand overschreven
    moet worden faalt OutputStream.Create, wanneer het al bestaande bestand
    read-only is. Oplossing: eerst de attributen veranderen.
  o Warningsysteem beter + zoErrorOnWarning toegevoegd

  2003-04-01:
  o gestart met unit

}

uses
  Windows, Classes, Contnrs, SysUtils,
  UMisc, UTools, UFiles, ZLibEx;

// copy van controls
const
  mrNone     = 0;
  mrOk       = idOk;
  mrCancel   = idCancel;
  mrAbort    = idAbort;
  mrRetry    = idRetry;
  mrIgnore   = idIgnore;
  mrYes      = idYes;
  mrNo       = idNo;           // 7
  mrAll      = mrNo + 1;       // 8
  mrNoToAll  = mrAll + 1;      // 9
  mrYesToAll = mrNoToAll + 1;  // 10

type
  TZipError =
  (
    ERR_NONE,                            // 0
    { non fatal }
    ERR_ZIP_NO_MATCHING_FILES,           // 1

    ERR_SEEK,                            // 2
    { read }
    ERR_READ_FILE_EMPTY,                 // 4
    ERR_READ_ZIPHEADER,                  // 5
    ERR_READ_FILENAME,                   // 6
    ERR_READ_FILEHEADER,                 // 7
    ERR_READ_DATAHEADER,                 // 8
    ERR_READ_DATA,                       // 9
    ERR_READ_NUMFILES,
    { write }
    ERR_WRITE_ZIPHEADER,                 // 10
    ERR_WRITE_FILENAME,                  // 11
    ERR_WRITE_FILEHEADER,                // 12
    ERR_WRITE_DATAHEADER,                // 13
    ERR_WRITE_DATA,                      // 14
    ERR_WRITE_NUMFILES,                  // 15
    ERR_WRITE_TERMINATOR,                // 16
    { file fouten }
    ERR_NEXT_POINTER,                    // 17

    { zipheader fouten }
    ERR_INV_ZIPHEADER_ID,                // 18
    ERR_INV_ZIPHEADER_VERSION,           // 19
    ERR_INV_ZIPHEADER_NUMFILES,          // 20
    ERR_INV_ZIPHEADER_FILESIZE,          // 21
    ERR_INV_ZIPHEADER_OPTIONS,           // 22

    ERR_INV_FILEHEADER_CHECK,            // 23
    ERR_CANNOT_CREATE_DIR,               // 24

    { extract fouten }
    ERR_EXTRACT_INV_FILESIZE,            // 25
    ERR_EXTRACT_FILE_NOT_FOUND,          // 26
    ERR_ARCHIVE_NOT_OPEN,                // 27

    { zip fouten }
    ERR_FILE_NOT_FOUND,                  // 28
    ERR_WARNING,                         // 29

    { error abort }
    ERR_ABORTED                          // 30
  );

resourcestring
  { header }
  SInvalidID = 'ID klopt niet (%s)';
  SInvalidVersion = 'Versienummer klopt niet (%d)';
  SInvalidNumFiles = 'Ongeldig aantal bestanden (%d)';
  { write errors }
  SWriteError = 'Schrijffout';
  { read errors }
  SReadError = 'Leesfout op positie %d';
  SReadLenError = 'Lengte bestandsnaam fout op positie %d';
  SUnzipError = 'Onbekende fout';
  SNextError = 'Foute verwijzing';
  SSeekError = 'Seek fout op positie (%d)';
  { input }
  SFileNameNotSpecified = 'Filenaam is leeg';
  SCannotMakeDir = 'Kan directory "%s" niet maken';
  SInvalidFileSize = 'Opgeslagen bestandsgrootte (%d) klopt niet met werkelijke grootte (%d)';
  SInvFileHeaderCheck = 'Controle karakter fileheader klopt niet (%s)';
  SInvExtractFileSize = 'Opgeslagen bestandsgrootte (%d) klopt niet met uitgepakte grootte (%d)';
  SCannotCreateDir = 'Map "%s" kan niet aangemaakt worden';
  SZipNoMatchingFiles = 'Geen bestanden gevonden die voldoen aan de criteria (%s)';
  SArchiveNotOpen = 'Archief is niet geopend';
  SFileNotFound = 'Bestand "%s" niet gevonden';
  SExtractFileNoFound = 'Bestand "%s" staat niet in archief';
  { warnings }
  SCannotOpenfile = 'Kan bestand "%s" niet openen';
  SCannotCreateFile = 'Kan bestand "%s" niet aanmaken';
  { aborted }
  SAborted = 'Operatie afgebroken';

const
  ZIPHEADER_VERSION = 1; // zolang de zip-structuur niet wijzigt kan dit blijven
  ZIP_VERSION = 1;

{******************************************************************************}

// archiefstructuur types

type
  TZipHeader = packed record
    ID       : array[0..1] of Char;
    Version  : byte;
    NumFiles : integer;
    FileSize : integer;
    Options  : Byte;

    // + reserved 32 bytes : ALTIJD 0 als niet gevuld
    Reserved : array[0..31] of Byte;
  end;

  TFileHeader = packed record
    Check            : Char; { verplichte waarde = 'F'}
    UnCompressedSize : integer;
    CompressedSize   : integer;
    FileTime         : integer;
    FileAttr         : integer;
    Delta            : integer; { delta bytes naar volgende filename }
    CreationTime     : TFileTime;
    LastAccessTime   : TFileTime;
    LastWriteTime    : TFileTime;
    Reserved         : array[0..3] of Byte;
    // + reserved: 4 bytes : ALTIJD 0 als niet gevuld
  end;

  TDataHeader = packed record
    UnCompressedSize : integer;
    CompressedSize   : integer;
    { COMPRESSEDDATA = UnCompressedSize bytes }
  end;

  { TERMINATOR = byte = 0 }

{******************************************************************************}

  TArchive = class;

  TPathInfo = (piRelative, piFull);

  EZipError = class(Exception)
  private
    FCode: TZipError;
  public
    constructor Create(Code: TZipError; const Msg: string);
    property Code: TZipError read FCode;
  end;

  TZipStateMode = (
    zmNone,
    zmStart,
    zmBusy,
    zmReady
  );

  TZipState = (
    zsNone,
    zsRead,
    zsZip,
    zsUnzip,
    zsRemove,
    zsCopy,
    zsUnzipDenied
  );

  TProgress = record
    Finished       : boolean;
    Mode           : TZipStateMode;
    State          : TZipState;
    CurrentFile    : string;
    Max            : Int64;
    Done           : Int64;
  end;

  TProgressEvent = procedure(Sender: TArchive; const Progress: TProgress) of object;
  TConfirmOverwriteEvent = procedure(Sender: TArchive; const aFileName: string; var aResult: Word) of object;

  TStreamClass = class of TStream;

  TVolatileStream = class(TStream)
  private
    FMyStream: TStream;
    //FStreamClass: TStreamClass;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
    property MyStream: TStream read FMyStream;
  public
    constructor Create(const FN: string; AMode: word); overload;
        { constructor voor filestream }
    constructor Create(Instance: THandle; const ResName: string; ResType: PChar); overload;
        { constructor voor resourcestream }
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    //property StreamClass: TStreamClass read FStreamClass;
  published
  end;

  TZipStreamEx = class(TVolatileStream)
  private
    FArchive : TArchive;
  { check }
    procedure CheckRead(Error: TZipError; Needed, Done: integer);
    procedure CheckWrite(Error: TZipError;Needed, Done: integer);
  protected
  public
    constructor Create(Arc: TArchive; const FN: string; AMode: word); overload;
    constructor Create(Arc: TArchive; Instance: THandle; const ResName: string; ResType: PChar); overload;
    destructor Destroy; override;
  { check }
    procedure CheckZipHeader(const Header: TZipHeader);
    procedure CheckFileHeader(const FileHeader: TFileHeader);
  { read }
    function ReadZipHeader(var Header: TZipHeader): integer;
    function ReadFileName(var Name: ShortString): integer;
    function ReadLongFileName(out Name: string): integer;
    function ReadFileHeader(var FileHeader: TFileHeader): integer;
    function ReadDataHeader(var DataHeader: TDataHeader): integer;
    function ReadData(var Buffer; Count: integer): integer;
  { write }
    function WriteZipHeader(const Header: TZipHeader): integer;
    function WriteFileName(const Name: ShortString): integer;
    function WriteLongFileName(const Name: string): integer;
    function WriteFileHeader(const FileHeader: TFileHeader): integer;
    function WriteDataHeader(const DataHeader: TDataHeader): integer;
    function WriteData(var Buffer; Count: integer): integer;
    function WriteTerminator: integer;
  { update }
    procedure UpdateZipHeader(const Header: TZipHeader);
  end;

  TArchiveObject = class
    FileName    : string;
    FileTime    : integer;
    FileSize    : integer;
    FileAttr    : integer;
    ArcPosition : integer;
    ArcSize     : integer;
    ArcDelta    : integer;
    function Ratio: integer;
    constructor CreateClone(const AO: TArchiveObject);
  end;

  TArchiveList = class(TStringList)
  private
    function GetArchiveObject(N: integer): TArchiveObject;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property ArchiveObjects[N: integer]: TArchiveObject read GetArchiveObject;
  end;

  TArchiveMode = (
    amCreate,
    amOpen
  );

  TZipOption = (
    {  1} zoPathInfo,
    {  2} zoFileAttr,
    {  4} zoFileTime,
    {  8} zoLogFile,            // not used
    { 16} zoOverwrite,
    { 32} zoProgress,
    { 64} zoErrorOnWarning,
    {128} zoRefreshFiles,       // not used
    {256} zoDeleteAfterZip,     // not used
    {512} zoConfirmOverwrite
  );
  TZipOptions = set of TZipOption;

  TArchive = class
  private
    FStream              : TZipStreamEx;
    FBuffer              : pointer;
    FBufferSize          : integer;
    FFileName            : string;
    FTempFileName        : string;
    FArchiveList         : TArchiveList;
    FZipHeader           : TZipHeader;
    FChangeFlag          : integer;
    FOnChange            : TNotifyEvent;
    FProgress            : TProgress;
    FDirectAddList       : boolean;
    FIsOpen              : boolean;
    FBeforeChange        : TNotifyEvent;
    FZipOptions          : TZipOptions;
    fCompressionLevel    : TZCompressionLevel;
    FExceptionList       : TObjectList;
    FIgnoreConfirm       : boolean;
    FOnProgress          : TProgressEvent;
    FOnConfirmOverwrite  : TConfirmOverwriteEvent;
    procedure AddException(const E: Exception);
    //procedure InternalValidate;
    procedure InternalOpenRead;

    procedure InternalAddFile(const AFileName: string; const ABaseDir: string = ''); { internal key method }
    //procedure InternalAddFiles(const ANameList: TStringList; const ABaseDir: string = '');
    // deleted this method because the compiler told me to :)

    procedure InternalExtractFile(AIndex: integer; const ADir: string); overload; { internal key method }
    procedure InternalExtractFile(const AFileName: string; const ADir: string); overload;
    //procedure InternalExtractFiles(const ExtractList: TIntegerList; const ADir: string);
    // deleted...

    procedure InternalExtractFile(aIndex: Integer; aDestStream: TStream); overload; { internal key method reading to stream }
    procedure InternalExtractFile(const AFileName: string; aDestStream: TStream); overload;
    procedure InternalRemove(DeleteList: TIntegerList); { internal key method }
    procedure InternalRemoveFile(const AFileName: string);
    procedure InternalHandleException(Err: TZipError; const Msg: string);
    //function CalcUnzipBytes(Sel: TIntegerList = nil; Compressed: Boolean = False): Int64;
    // and here again deleted because of the hint :)
        { Sel = nil --> alles }
    function GetFileCount: integer;
    procedure ClearArchiveList;
    procedure AddSearchEvent(const ASearchRec: TSearchRec; const aFullName: string; out DoAdd: boolean; out AObject: TObject);
    procedure Changing;
    procedure Changed;
    function GetSize: integer;
    procedure ProgressStart;
    procedure DoProgress(AMode: TZipStateMode; AState: TZipState; const AFileName: string;
      AMax: Int64; ADelta: Int64; aAbsolute: Boolean = False);
    procedure ProgressFinished;
    procedure SetZipOptions(const Value: TZipOptions);
    function GetWarnings: integer;
    //procedure SetOption(const Value: TZipOption);
    function DoConfirm(const AFileName: string): word;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckOpen;
    procedure OpenArchive(const AFileName: string; AMode: TArchiveMode); { open file }
    procedure OpenResource(Instance: THandle; const ResName: string; ResType: PChar); { open resource }
    procedure CloseArchive;
    procedure AddFiles(const ANameList: TStringList; const ABaseDir: string = ''); overload; { key method }
      { moet internal worden }
    procedure AddFiles(const AWildCard: string); overload;
    procedure AddFile(const AFileName: string); overload;
    procedure RemoveFile(const AFileName: string); overload;
    procedure RemoveFiles(const IX: TIntegerList);
    procedure ExtractFiles(const ANameList: TStringList; const ADir: string); overload; { key method }
    procedure ExtractFiles(const ExtractList: TIntegerList; const ADir: string); overload; { key method }
    procedure ExtractFiles(const AWildCard: string; const ADir: string); overload;
    procedure ExtractAll(const ADir: string);
    procedure ExtractFile(const AFileName: string; const ADir: string); overload;
    procedure ExtractFile(AIndex: integer; const ADir: string); overload;
    procedure ExtractFile(AIndex: integer; aDestStream: TStream); overload;
    procedure ExtractFile(const AFileName: string; aDestStream: TStream); overload;
    procedure DeleteArchive;
    procedure SaveLogFile;
    procedure ClearWarnings;
  {}
    function GetTotalFileBytes(aIndexList: TIntegerList = nil; Compressed: Boolean = False): Int64; 

    property FileName: string read FFileName;
    property Size: integer read GetSize;
    property ArchiveList: TArchiveList read FArchiveList;
    property FileCount: integer read GetFileCount;
    property BeforeChange: TNotifyEvent read FBeforeChange write FBeforeChange;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property IsOpen: boolean read FIsOpen;
    property ZipOptions: TZipOptions read FZipOptions write SetZipOptions;
    property CompressionLevel: TZCompressionLevel read fCompressionLevel write fCompressionLevel;
    property Warnings: integer read GetWarnings;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnConfirmOverwrite: TConfirmOverwriteEvent read FOnConfirmOverwrite write FOnConfirmOverwrite;
  published
  end;

const
  AllZipOptions = [zoPathInfo, zoFileAttr, zoFileTime, zoLogFile, zoOverwrite,
    zoProgress, zoErrorOnWarning, zoRefreshFiles];

implementation

uses
  TypInfo, Masks,
  Math, UFastStrings;


const
  MAX_FILES = 1024 * 32;
  OFFSET_NUMFILES = 3;

const
  MaxBufSize = MegaByte;
  CreateHeader: TZipHeader = (
    ID        : 'EL';
    Version   : ZIPHEADER_VERSION;
    NumFiles  : 0;
    FileSize  : SizeOf(TZipHeader) + 1; { + 1 voor terminator }
    Options   : 0;
    Reserved  : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  );

procedure ClearFileHeader(var aFileHeader: TFileHeader);
begin
  FillChar(aFileHeader, SizeOf(TFileHeader), 0);
end;

{ EZipError }

constructor EZipError.Create(Code: TZipError; const Msg: string);
begin
  inherited Create(Msg);
  FCode := Code;
end;

function ErrStr(Err: TZipError): string;
begin
  Result := GetEnumName(TypeInfo(TZipError), Integer(Err));
end;

procedure ZipError(Arc: TArchive; Err: TZipError; const Msg: string);
begin
  if Arc <> nil then
    Arc.InternalHandleException(Err, Msg);
  raise EZipError.Create(Err, Msg);
end;

procedure ZipErrorFmt(Arc: TArchive; Err: TZipError; const Msg: string; const Args: array of const);
var
  S: string;
begin
  try S := Format(Msg, Args); except S := Msg; end;
  ZipError(Arc, Err, S);
end;

{ TVolatileStream }

constructor TVolatileStream.Create(const FN: string; AMode: word);
begin
  inherited Create;
  FMyStream := TFileStream.Create(FN, aMode);
end;

constructor TVolatileStream.Create(Instance: THandle; const ResName: string; ResType: PChar);
begin
  inherited Create;
  FMyStream := TResourceStream.Create(Instance, ResName, ResType);
end;

destructor TVolatileStream.Destroy;
begin
  FMyStream.Free;
  inherited;
end;

function TVolatileStream.GetSize: Int64;
begin
  Result := FMyStream.Size;
end;

function TVolatileStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FMyStream.Read(Buffer, Count);
end;

function TVolatileStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := FMyStream.Seek(Offset, Origin);
end;

function TVolatileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FMyStream.Seek(Offset, Origin);
end;

procedure TVolatileStream.SetSize(NewSize: Integer);
begin
  FMyStream.Size := NewSize;
end;

procedure TVolatileStream.SetSize(const NewSize: Int64);
begin
  FMyStream.Size := NewSize;
end;

function TVolatileStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FMyStream.Write(Buffer, Count);
end;


{ TZipStreamEx }

constructor TZipStreamEx.Create(Arc: TArchive; const FN: string; AMode: word);
begin
  inherited Create(FN, aMode);
  FArchive := Arc;
end;

constructor TZipStreamEx.Create(Arc: TArchive; Instance: THandle; const ResName: string; ResType: PChar);
begin
  inherited Create(Instance, ResName, ResType);
  FArchive := Arc;
end;

destructor TZipStreamEx.Destroy;
begin
  inherited;
end;

procedure TZipStreamEx.CheckFileHeader(const FileHeader: TFileHeader);
begin
  with FileHeader do
  begin
    if Check <> 'F' then ZipErrorFmt(FArchive, ERR_INV_FILEHEADER_CHECK, SInvFileHeaderCheck, [Check]);
  end;
end;

procedure TZipStreamEx.CheckRead(Error: TZipError; Needed, Done: integer);
begin
  if Needed <> Done then ZipErrorFmt(FArchive, Error, SReadError, [Position]);
end;

procedure TZipStreamEx.CheckWrite(Error: TZipError; Needed, Done: integer);
begin
  if Needed <> Done then ZipErrorFmt(FArchive, Error, SWriteError, [Position]);
end;

procedure TZipStreamEx.CheckZipHeader(const Header: TZipHeader);
begin
  with Header do
  begin
    if ID <> 'EL' then ZipErrorFmt(FArchive, ERR_INV_ZIPHEADER_ID, SInvalidID, [string(ID)]);
    if Version <> 1 then ZipErrorFmt(FArchive, ERR_INV_ZIPHEADER_VERSION, SInvalidVersion, [Version]);
    if NumFiles < 0 then ZipErrorFmt(FArchive, ERR_INV_ZIPHEADER_NUMFILES, SInvalidNumFiles, [NumFiles]);
    if FileSize <> Size then ZipErrorFmt(FArchive, ERR_INV_ZIPHEADER_FILESIZE, SInvalidFileSize, [FileSize, Size]);
  end;
end;

function TZipStreamEx.ReadZipHeader(var Header: TZipHeader): integer;
begin
  Result := Read(Header, SizeOf(TZipHeader));
  CheckRead(ERR_READ_ZIPHEADER, SizeOf(TZipHeader), Result);
  CheckZipHeader(Header);
end;

function TZipStreamEx.ReadFileName(var Name: ShortString): integer;
var
  L: byte;
begin
  Result := Read(L, 1);
  CheckRead(ERR_READ_FILENAME, 1, Result);
  Seek(-1, soFromCurrent);
  Result := Read(Name, L + 1);
  CheckRead(ERR_READ_FILENAME, L + 1, Result);
end;

function TZipStreamEx.ReadLongFileName(out Name: string): integer;
var
  L: word;
begin
  Result := Read(L, 2);
  CheckRead(ERR_READ_FILENAME, 2, Result);
  SetLength(Name, L);
  Result := Read(Name[1], L);
  CheckRead(ERR_READ_FILENAME, L, Result);
  Result := 2 + L;
end;

function TZipStreamEx.ReadFileHeader(var FileHeader: TFileHeader): integer;
begin
  Result := Read(FileHeader, SizeOf(TFileHeader));
  CheckRead(ERR_READ_FILEHEADER, SizeOf(TFileHeader), Result);
  CheckFileHeader(FileHeader);
end;

function TZipStreamEx.ReadDataHeader(var DataHeader: TDataHeader): integer;
begin
  Result := Read(DataHeader, SizeOf(TDataHeader));
  CheckRead(ERR_READ_DATAHEADER, SizeOf(TDataHeader), Result);
end;

function TZipStreamEx.ReadData(var Buffer; Count: integer): integer;
begin
  Result := Read(Buffer, Count);
  CheckRead(ERR_READ_DATA, Count, Result);
end;

function TZipStreamEx.WriteZipHeader(const Header: TZipHeader): integer;
begin
  Result := Write(Header, SizeOf(TZipHeader));
  CheckWrite(ERR_WRITE_ZIPHEADER, SizeOf(TZipHeader), Result);
end;

function TZipStreamEx.WriteFileName(const Name: ShortString): integer;
begin
  Result := Write(Name, Length(Name) + 1);
  CheckWrite(ERR_WRITE_FILENAME, Length(Name) + 1, Result);
end;

function TZipStreamEx.WriteLongFileName(const Name: string): integer;
var
  L: Word;
begin
  L := Length(Name);
  Result := Write(L, 2);
  CheckWrite(ERR_WRITE_FILENAME, 2, Result);
  Result := Write(Name[1], L);
  CheckWrite(ERR_WRITE_FILENAME, L, Result);
  Result := 2 + L;
end;

function TZipStreamEx.WriteFileHeader(const FileHeader: TFileHeader): integer;
begin
  Result := Write(FileHeader, SizeOf(TFileHeader));
  CheckWrite(ERR_WRITE_FILEHEADER, SizeOf(TFileHeader), Result);
end;

function TZipStreamEx.WriteDataHeader(const DataHeader: TDataHeader): integer;
begin
  Result := Write(DataHeader, SizeOf(TDataHeader));
  CheckWrite(ERR_WRITE_DATAHEADER, SizeOf(TDataHeader), Result);
end;

function TZipStreamEx.WriteData(var Buffer; Count: integer): integer;
begin
  Result := Write(Buffer, Count);
  CheckWrite(ERR_WRITE_DATA, Count, Result);
end;

function TZipStreamEx.WriteTerminator: integer;
var
  B: Byte;
begin
  B := 0;
  Result := Write(B, 1);
  CheckWrite(ERR_WRITE_TERMINATOR, 1, Result);
end;

procedure TZipStreamEx.UpdateZipHeader(const Header: TZipHeader);
var
  P: integer;
begin
  P := Position;
  Seek(0, soFromBeginning);
  WriteZipHeader(Header);
  Seek(P, soFromBeginning);
end;

procedure FreeStringListWithObjects(AList: TStringList);
var
  i: integer;
  O: TObject;
begin
  if AList = nil then Exit;
  with AList do
  begin
    for i := 0 to Count - 1 do
    begin
      O := Objects[i];
      if O <> nil then O.Free;
    end;
  end;
  FreeAndNil(AList);
end;

procedure ClearStringListWithObjects(AList: TStringList);
var
  i: integer;
  O: TObject;
begin
  if AList = nil then Exit;
  with AList do
  begin
    for i := 0 to Count - 1 do
    begin
      O := Objects[i];
      if O <> nil then O.Free;
    end;
    Clear;
  end;
end;

procedure ClearStringListItemWithObject(AList: TStringList; Index: integer);
var
  O: TObject;
begin
  if AList = nil then Exit;
  with AList do
  begin
      O := Objects[Index];
      if O <> nil then O.Free;
      Delete(Index);
  end;
end;

{ TArchiveObject }

constructor TArchiveObject.CreateClone(const AO: TArchiveObject);
begin
  inherited Create;
  FileName    := AO.FileName;
  FileTime    := AO.FileTime;
  FileSize    := AO.FileSize;
  FileAttr    := AO.FileAttr;
  ArcPosition := AO.ArcPosition;
  ArcSize     := AO.ArcSize;
  ArcDelta    := AO.ArcDelta;
end;

function TArchiveObject.Ratio: integer;
begin
  Result := Percentage(FileSize, ArcSize);
end;

{ TArchiveList }

constructor TArchiveList.Create;
begin
  inherited Create;
end;

destructor TArchiveList.Destroy;
begin
  inherited Destroy;
end;

function TArchiveList.GetArchiveObject(N: integer): TArchiveObject;
begin
  Result := TArchiveObject(GetObject(N));
end;

{ TArchive }

constructor TArchive.Create;
begin
  inherited Create;
//  FLog := TStringList.Create;
  FExceptionList := TObjectList.Create;
  FBufferSize := MaxBufSize;
  GetMem(FBuffer, FBufferSize);
  FArchiveList := TArchiveList.Create;
  FArchiveList.Duplicates := dupError;
  FZipOptions := [zoPathInfo, zoFileAttr, zoFileTime, zoLogFile, zoOverwrite, zoProgress];
  FCompressionLevel := zcMax;
end;

destructor TArchive.Destroy;
begin
  CloseArchive;
  FArchiveList.Free;
  FreeMem(FBuffer, FBufferSize);
  FExceptionList.Free;
//  FLog.Free;
  inherited Destroy;
end;

procedure TArchive.AddException(const E: Exception);
var
  NewExc: Exception;
begin
  NewExc := Exception.Create(E.Message);
  FExceptionList.Add(NewExc);
end;

procedure TArchive.OpenArchive(const AFileName: string; AMode: TArchiveMode);
var
  Pad: string;
begin
  Changing;
  try
    try
      CloseArchive;
      FFileName := ExpandFileName(AFileName);
      Pad := ExtractFilePath(FFileName);
      if Pad <> '' then Pad := IncludeTrailingBackslash(Pad);
      FTempFileName := Pad + 'Z' + DtoS(Date()) + '_' + IntToStr(Milliseconds) + '.tmp';
      case AMode of
        amCreate:
          begin
            FStream := TZipStreamEx.Create(Self, FFileName, fmCreate);
//            FStream.OpenFile(FFileName, fmCreate);
            FStream.WriteZipHeader(CreateHeader);
            FStream.WriteTerminator;
          end;
        amOpen:
          begin
            FStream := TZipStreamEx.Create(Self, FFileName, fmOpenReadWrite);
            //FStream.OpenFile(FFilename, fmOpenReadWrite);
          end;
      end;
      InternalOpenRead;
      FIsOpen := True;
    except
      CloseArchive;
      raise;
    end;
  finally
    Changed;
  end;
end;

procedure TArchive.OpenResource(Instance: THandle; const ResName: string; ResType: PChar);
begin
  Changing;
  try
    try
      CloseArchive;
      FStream := TZipStreamEx.Create(Self, Instance, ResName, ResType);
      InternalOpenRead;
      FIsOpen := True;
    except
      CloseArchive;
      raise;
    end;
  finally
    Changed;
  end;
end;

procedure TArchive.CloseArchive;
begin
  FIsOpen := False;
  FFileName := '';
  Changing;
  try
    ClearArchiveList;
    FreeAndNil(FStream);
  finally
    Changed;
  end;
end;

procedure TArchive.InternalAddFile(const AFileName: string; const ABaseDir: string = '');
var
  InputStream: TFileStream;
  InSize, OutSize, FileNamePosition, FileHeaderPosition: integer;
  LFN: string;
  FileHeader: TFileHeader;
  DataHeader: TDataHeader;
  OutBuf: pointer;

    procedure AddToArchiveList;
    var
      A: TArchiveObject;
    begin
      A := TArchiveObject.Create;
      A.FileName    := ExtractFileName(LFN);
      A.FileTime    := FileHeader.FileTime;
      A.FileSize    := FileHeader.UnCompressedSize;
      A.FileAttr    := FileHeader.FileAttr;
      A.ArcPosition := FileNamePosition;
      A.ArcSize     := FileHeader.CompressedSize;
      A.ArcDelta    := FileHeader.Delta;
      ArchiveList.AddObject(LFN, A);
    end;

    function GetName: string;
    var
      Drive: string;
    begin
      { volledige pad }
      if zoPathInfo in ZipOptions then
      begin
        Result := ExtractRelativePath(ABaseDir, AFileName);
        Drive := ExtractFileDrive(Result);
        if Drive = '' then
          Exit;
        Result := FastReplace(AFileName, Drive, '');
        while (Length(Result) > 0) and (Result[1] = '\') do
        begin
          Result := Copy(Result, 2, Length(Result));
        end;
      end
      { alleen filenaam }
      else
        Result := ExtractFileName(AFileName);
    end;


begin
  LFN := GetName;
  if ArchiveList.IndexOf(LFN) >= 0 then
  begin
    Exit;
  end;
  if CompareText(AFileName, fFileName) = 0 then Exit;

  ClearFileHeader(FileHeader); { initialiseer FileHeader, vul met nullen }
//  if zoLogFile in FZipOptions then FLog.Add('Inpakken ' + AFileName);

  { inputstream openen }
  try
    InputStream := TFileStream.Create(AFileName, fmOpenRead{ or fmShareDenyNone});
    { vul datum-tijd velden van FileHeader }
    with FileHeader do
      GetFileTime(InputStream.Handle, @CreationTime, @LastAccessTime, @LastWriteTime);
  except
    on E: Exception do
    begin
//      if zoLogFile in FZipOptions then FLog.Add('  kan niet geopend worden');
      AddException(E);
      if zoErrorOnWarning in fZipOptions then ZipErrorFmt(Self, ERR_WARNING, SCannotOpenfile, [aFilename]);
      Exit;
    end;
  end;

  { zippen }
  try
    Inc(FZipHeader.NumFiles);
    { schrijf over oude terminator heen }
    FStream.Seek(-1, soFromEnd);
    //LFN := GetName; {### kan weg, is al gedaan}
    { bewaar filenamepositie }
    FileNamePosition := FStream.Position;
    FStream.WriteLongFileName(LFN);
    FileHeader.Check := 'F';
    FileHeader.CompressedSize := 0; { ### kan ook inputstream.size gezet }
    FileHeader.UnCompressedSize := 0;
    FileHeader.FileTime := FileGetDate(InputStream.Handle);
    FileHeader.FileAttr := FileGetAttr(AFileName);
    FileHeader.Delta := 0;
    { bewaar headerpositie }
    FileHeaderPosition := FStream.Position;
    FileHeader.Delta := FileHeaderPosition - FileNamePosition + SizeOf(TFileHeader);
    { schrijf fileheader }
    FStream.WriteFileHeader(FileHeader);
    repeat
      InSize := InputStream.Read(FBuffer^, MaxBufSize);
      if InSize > 0 then //#optimalisatie bij een leeg bestand alleen filenaam opslaan
      begin
        Inc(FileHeader.UnCompressedSize, InSize);
        ZCompress(FBuffer, InSize, OutBuf, OutSize, fCompressionLevel{zcMax});
        { schrijf dataheader }
        DataHeader.UnCompressedSize := InSize;
        DataHeader.CompressedSize := OutSize;
        FStream.WriteDataHeader(DataHeader);
        { schrijf gecomprimeerde data }
        FStream.WriteData(OutBuf^, OutSize);
        FreeMem(OutBuf);
        Inc(FileHeader.CompressedSize, OutSize + SizeOf(TDataHeader));
        Inc(FileHeader.Delta, OutSize + SizeOf(TDataHeader));
      end;
      DoProgress(zmBusy, zsZip, aFileName, -1, InSize);
      if InSize = 0 then Break;
    until False;
    AddToArchiveList;
    { herschrijf fileheader }
    FStream.Seek(FileHeaderPosition, soFromBeginning);
    FStream.WriteFileHeader(FileHeader);
    FStream.Seek(0, soFromEnd);
    { schrijf terminator }
    FStream.WriteTerminator;
    FZipHeader.FileSize := FStream.Size;
    { herschrijf header }
    FStream.UpdateZipHeader(FZipHeader);
//    if zoLogFile in FZipOptions then FLog.Add('OK');
  finally
    InputStream.Free;
  end;

end;
(*
procedure TArchive.InternalAddFiles(const ANameList: TStringList; const ABaseDir: string);
var
  i: integer;
begin
  Changing;
  try
    ProgressStart;
    with ANameList do
      for i := 0 to Count - 1 do
        InternalAddFile(Strings[i], ABaseDir);
    ProgressFinished;
  finally
    Changed;
  end;
end;
*)

procedure TArchive.InternalExtractFile(AIndex: integer; const ADir: string);
var
  AFileName: string;
  OutputStream: TFileStream;
  OutBuf: pointer;
  OutSize: integer;
  OutputDir: string;
  CompleteFileName: string;
  DirPart: string;
  FilePart: string;
  LFN: string;
  FileHeader: TFileHeader;
  DataHeader: TDataHeader;
  Att: DWORD;


  {P, }StartPos, EndPos, Rd, Wr, BlockBytesWritten, AR, BR{, CR}: integer;

    function GetRelName: string;
    var
      Drive: string;
    begin
      { deze routine moet eigenlijk aangepast, in een archief zit deze informatie nooit }
      Drive := ExtractFileDrive(AFileName); { is het volledige pad opgeslagen? }
      if Drive = '' then
        Result := AFileName
      else
        Result := FastReplace(AFileName, Drive, '');
      { haal}
      while (Length(Result) > 0) and (Result[1] = '\') do
      begin
        Result := Copy(Result, 2, Length(Result));
      end;
      if not (zoPathInfo in FZipOptions) then
        Result := ExtractFileName(Result);
    end;

begin
  AFileName := ArchiveList[AIndex];
//  if zoLogFile in FZipOptions then FLog.Add('Uitpakken ' + AFileName);
  { zet namen }
  OutputDir := IncludeTrailingBackslash(ExpandFileName(ADir));
  CompleteFileName := OutputDir + GetRelName;

  if FileExists(CompleteFileName) then
    case DoConfirm(CompleteFileName) of
      mrNo:
        begin
          DoProgress(zmBusy, zsUnzipDenied, CompleteFileName, -1, 0);
          Exit;
        end;
      mrAbort: ZipError(Self, ERR_ABORTED, SAborted);
    else begin
      // we willen het bestand overschrijven. readonly of hidden mislukt
      // dus veranderen we het attr.
      Att :=  GetFileAttributes(PChar(CompleteFileName));
      if (FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_HIDDEN) and Att <> 0 then
      begin
        SetFileAttributes(PChar(CompleteFileName), Att and not (FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_HIDDEN));
      end;
    end;
  end;

  DirPart := ExtractFilePath(CompleteFileName);
  FilePart := ExtractFileName(CompleteFileName);

  with FArchiveList.ArchiveObjects[AIndex] do
    if FStream.Seek(ArcPosition, soFromBeginning) <> ArcPosition then
      ZipErrorFmt(Self, ERR_SEEK, SSeekError, [FStream.Position]);

  StartPos := FStream.Position; // leg startpos vast
  AR := FStream.ReadLongFileName(LFN);

  BR := FStream.ReadFileHeader(FileHeader);
  EndPos := StartPos + FileHeader.Delta; // bepaal initiele eindpositie file

  if not ForceDirectories(DirPart) then
    ZipErrorFmt(Self, ERR_CANNOT_CREATE_DIR, SCannotCreateDir, [DirPart]);

//  DoProgress(zmBusy, zsUnzip, CompleteFileName{LFN}, -1, AR + BR);

  try
    OutputStream := TFileStream.Create(CompleteFileName, fmCreate);
  except
    on E: Exception do
    begin
//      if zoLogFile in FZipOptions then
  //      FLog.Add('  kan bestand niet aanmaken');
      AddException(E);
      if zoErrorOnWarning in fZipOptions then
        ZipErrorFmt(Self, ERR_WARNING, SCannotCreateFile, [CompleteFilename]);
      Exit;
    end;
  end;

  try
    BlockBytesWritten := 0;
    repeat
      Rd := 0;
      //CR := 0;
      if FileHeader.UnCompressedSize > 0 then //#optimalisatie alleen niet lege bestanden schrijven
      begin
        {CR := }FStream.ReadDataHeader(DataHeader); //#add progress!
        if FBufferSize < DataHeader.CompressedSize then
        begin
          FBufferSize := DataHeader.CompressedSize;
          ReAllocMem(FBuffer, DataHeader.CompressedSize);
        end;
        Rd := FStream.ReadData(FBuffer^, DataHeader.CompressedSize);
        { decomprimeer block }
        ZDecompress(FBuffer, Rd, OutBuf, OutSize, DataHeader.UnCompressedSize);
        { schrijf data }
        Wr := OutputStream.Write(OutBuf^, OutSize);
        FreeMem(OutBuf);
        { check Wr }
        Inc(BlockBytesWritten, Wr);
        DoProgress(zmBusy, zsUnzip, CompleteFileName, -1, Wr);
      end;
      if BlockBytesWritten >= FileHeader.UnCompressedSize then Break;
      if FStream.Position >= EndPos then Break;
    until False;
    { check size }
    if OutputStream.Size <> FileHeader.UncompressedSize then
      ZipErrorFmt(Self, ERR_EXTRACT_INV_FILESIZE, SInvExtractFileSize,
        [FileHeader.UncompressedSize, OutputStream.Size]);
    { zet attr }
    if zoFileAttr in FZipOptions then
      FileSetAttr(CompleteFileName, FileHeader.FileAttr); { of windows? }
    { zet tijd }
    if zoFileTime in fZipOptions then
      with FileHeader do
        SetFileTime(OutputStream.Handle, @CreationTime, @LastAccessTime, @LastWriteTime);
//    if zoLogFile in FZipOptions then
  //    FLog.Add('OK');
  finally
    OutputStream.Free;
  end;
end;

procedure TArchive.InternalExtractFile(aIndex: Integer; aDestStream: TStream);
var
 // AFileName: string;
  OutBuf: pointer;
  OutSize: integer;
//  FilePart: string;
  LFN: string;
  FileHeader: TFileHeader;
  DataHeader: TDataHeader;
  StartPos, EndPos, Rd, Wr, BlockBytesWritten, AR, BR: integer;

begin
  with FArchiveList.ArchiveObjects[AIndex] do
    if FStream.Seek(ArcPosition, soFromBeginning) <> ArcPosition then
      ZipErrorFmt(Self, ERR_SEEK, SSeekError, [FStream.Position]);

  StartPos := FStream.Position; // leg startpos vast
  AR := FStream.ReadLongFileName(LFN);

  BR := FStream.ReadFileHeader(FileHeader);
  EndPos := StartPos + FileHeader.Delta; // bepaal initiele eindpositie file

  BlockBytesWritten := 0;
  repeat
    Rd := 0;
    if FileHeader.UnCompressedSize > 0 then //#optimalisatie alleen niet lege bestanden schrijven
    begin
      FStream.ReadDataHeader(DataHeader); //#add progress!
      if FBufferSize < DataHeader.CompressedSize then
      begin
        FBufferSize := DataHeader.CompressedSize;
        ReAllocMem(FBuffer, DataHeader.CompressedSize);
      end;
      Rd := FStream.ReadData(FBuffer^, DataHeader.CompressedSize);
      { decomprimeer block }
      ZDecompress(FBuffer, Rd, OutBuf, OutSize, DataHeader.UnCompressedSize);
      { schrijf data }
      Wr := aDestStream.Write(OutBuf^, OutSize);
      FreeMem(OutBuf);
      { check Wr }
      Inc(BlockBytesWritten, Wr);
    end;
    DoProgress(zmBusy, zsUnzip, LFN, -1, BlockBytesWritten {Rd} {+CR});
    if BlockBytesWritten >= FileHeader.UnCompressedSize then Break;
    if FStream.Position >= EndPos then Break;
  until False;
  { check size }
  if aDestStream.Size <> FileHeader.UncompressedSize then
    ZipErrorFmt(Self, ERR_EXTRACT_INV_FILESIZE, SInvExtractFileSize,
      [FileHeader.UncompressedSize, aDestStream.Size]);
end;

procedure TArchive.InternalExtractFile(const AFileName: string; aDestStream: TStream);
var
  i: integer;
begin
  i := ArchiveList.IndexOf(AFileName);
  if i = -1 then
    ZipErrorFmt(Self, ERR_EXTRACT_FILE_NOT_FOUND, SExtractFileNoFound, [AFileName]);
  InternalExtractFile(i, aDestStream);
end;

procedure TArchive.InternalExtractFile(const AFileName, ADir: string);
var
  i: integer;
begin
  i := ArchiveList.IndexOf(AFileName);
  if i = -1 then
    ZipErrorFmt(Self, ERR_EXTRACT_FILE_NOT_FOUND, SExtractFileNoFound, [AFileName]);
  InternalExtractFile(i, ADir);
end;

(*
procedure TArchive.InternalExtractFiles(const ExtractList: TIntegerList; const ADir: string);
var
  i: integer;
begin
  with ExtractList do
    for i := 0 to Count - 1 do
      InternalExtractFile(Integers[i], ADir);
end;
*)

// kopieert (gedeeltes van) archive naar temparchive en terug.
procedure TArchive.InternalRemove(DeleteList: TIntegerList);
var
  StartPos, NextPos, Bytes, NewStartPos: integer;
  MustCopy: boolean;
  FNR: integer;
  FN: string;
  TempStream: TZipStreamEx;
  FileHeader: TFileHeader;
  i: integer;
  TempAO, AO: TArchiveObject;
  TempList: TArchiveList;

    procedure ReplaceOld;
    var
      Rd: integer;
    begin
      TempStream.Seek(0, soFromBeginning);
      FStream.Seek(0, soFromBeginning);
      repeat
        Rd := TempStream.Read(FBuffer^, FBufferSize);
        FStream.WriteData(FBuffer^, Rd); // overschrijf
        DoProgress(zmBusy, zsCopy, '', -1, Rd);
        if TempStream.Position >= TempStream.Size - 1 then Break;
      until False;
      FStream.SetSize(TempStream.Size); // truncate
    end;


begin
  Changing;
  ProgressStart;
  FN := ExtractFilePath(FFilename) + 'Z' + DtoS(Date) + '.tmp';
  DoProgress(zmStart, zsRemove, '', ArchiveList.Count, 0);
  TempStream := TZipStreamEx.Create(Self, FN, fmCreate);

  try
    TempList := TArchiveList.Create;
    TempList.Capacity := FArchiveList.Count - DeleteList.Count;
    TempStream.WriteZipHeader(CreateHeader);
    TempStream.WriteTerminator;
    TempStream.Seek(-1, soFromCurrent);
    for i := 0 to FArchiveList.Count - 1 do
    begin
      AO := FArchiveList.ArchiveObjects[i];
      MustCopy := DeleteList.IndexOf(i) = -1;
      StartPos := AO.ArcPosition;
      NextPos := StartPos + AO.ArcDelta; //#aanp
      Bytes := NextPos - StartPos;
      NewStartPos := TempStream.Position;
      FStream.Seek(AO.ArcPosition, soFromBeginning);
      FNR := FStream.ReadLongFileName(FN);
      FStream.ReadFileHeader(FileHeader);
      case MustCopy of
        False:
          DoProgress(zmBusy, zsRemove, FN, -1, 1);
        True:
          begin
            DoProgress(zmBusy, zsRemove, '', -1, 1);
            { update templist }
            TempAO := TArchiveObject.CreateClone(AO);
            TempList.AddObject(FN, TempAO);
            TempAO.ArcPosition := NewStartPos;
            TempAO.ArcDelta := Bytes;
            FileHeader.Delta := Bytes;
            { update tempfile }
            TempStream.WriteLongFileName(FN);
            TempStream.WriteFileHeader(FileHeader);
            TempStream.CopyFrom(FStream, Bytes - FNR - SizeOf(TFileHeader)); { beter in blokken met progressevent }
          end;
      end;
    end;

    TempStream.WriteTerminator;
    FZipHeader.NumFiles := FArchiveList.Count - DeleteList.Count;
    FZipHeader.FileSize := TempStream.Size;
    TempStream.UpdateZipHeader(FZipHeader);
    DoProgress(zmReady, zsRemove, '', -1, 0);
    DoProgress(zmStart, zsCopy, ''{'Kopieren'}{##lullig}, TempStream.Size, 0);
    ReplaceOld;
    FreeStringListWithObjects(FArchiveList);
    FArchiveList := TempList;
    ProgressFinished;
  finally
    TempStream.Free;
    DeleteFile(ExtractFilePath(FFilename)+ '\temp.tmp');
  end;
  Changed;
end;

procedure TArchive.InternalRemoveFile(const AFileName: string);
var
  P: integer;
  IX: TIntegerList;
begin
  IX := TIntegerList.Create;
  try
    P := FArchiveList.IndexOf(AFileName);
    if P = -1 then Exit;
    IX.Add(P);
    InternalRemove(IX);
  finally
    IX.Free;
  end;
end;

procedure TArchive.InternalOpenRead;
var
  LFN: string;
  FileHeader: TFileHeader;
  Arc: TArchiveObject;
  P: integer;
begin
  ProgressStart;
  if FStream.Size = 0 then
    ZipError(Self, ERR_READ_FILE_EMPTY, 'bestand is leeg');
  FStream.Seek(0, soFromBeginning);
  FStream.ReadZipHeader(FZipHeader);
  DoProgress(zmStart, zsRead, '', FZipHeader.NumFiles, 0);
  FArchiveList.Capacity := FZipHeader.NumFiles;
  while True do
  begin
    if FZipHeader.NumFiles = 0 then Break;
    P := FStream.Position;
    FStream.ReadLongFileName(LFN);
    FStream.ReadFileHeader(FileHeader);
    Arc := TArchiveObject.Create;
    with Arc do
    begin
      FileName    := ExtractFileName(LFN);
      FileTime    := FileHeader.FileTime;
      FileSize    := FileHeader.UnCompressedSize;
      FileAttr    := FileHeader.FileAttr;
      ArcPosition := P;
      ArcSize     := FileHeader.CompressedSize;
      ArcDelta    := FileHeader.Delta;
    end;
    FArchiveList.AddObject(LFN, Arc);
    DoProgress(zmBusy, zsRead, LFN, -1, 1);
    if FStream.Seek(P + FileHeader.Delta, soFromBeginning) <> P + FileHeader.Delta then
      ZipErrorFmt(Self, ERR_NEXT_POINTER, SNextError, []);
    if FStream.Position >= FZipHeader.FileSize - 1 then
      Break;
  end;
  ProgressFinished;
end;

{procedure TArchive.InternalValidate;
begin
end;}

procedure TArchive.AddFiles(const ANameList: TStringList; const ABaseDir: string = '');
var
  i: integer;
  CompleteFileList: TStringList;
  Bytes: Int64;
//  RelPathList: TStringList;

    { kopieer alles en haal ook subdirectories op }
    procedure GetAll;
    var
      i: integer;
      Sr: TSearchRec;
    begin

      for i := 0 to ANameList.Count - 1 do
      begin
        if FindFirst(ANameList[i], faAllFiles, Sr) = 0 then
        try //2003-06-12 try finally toegevoegd
          if IsDir(Sr) then   
            CreateFileList(CompleteFileList, ANameList[i] + '\*.*', faAllFiles and not faDirectory, True, True, AddSearchEvent)
          else
            CreateFileList(CompleteFileList, ANameList[i], faAllFiles and not faDirectory, True, False, AddSearchEvent);
        finally
          FindClose(Sr);
        end;
      end;

    end;

begin
  CheckOpen;
  if ANameList.Count = 0 then Exit;

//  RelPathList := TStringList.Create;
  //if not FDirectAddList then
  Changing;
  try
    if not FDirectAddList then { tricky }
    begin
      //PROGRESS
      CompleteFileList := TStringList.Create;
      GetAll;
    end
    else
      CompleteFileList := ANameList;

    { check self }
    i := CompleteFileList.IndexOf(FFileName);
    if i <> -1 then
    begin
      ClearStringListItemWithObject(CompleteFileList, i);
    end;

//    Log(['addfiles', completefilelist.count]);

    if CompleteFileList.Count = 0 then Exit;

    Bytes := 0;
  //  windlg([completefilelist.count]);
    { bepaal en check size }
    with CompleteFileList do
    for i := 0 to Count - 1 do
    begin
      //Log([strings[i], tsearchrecobject(objects[i]).searchrec.size]);
      Inc(Bytes, TSearchRecObject(Objects[i]).SearchRec.Size);
      //if Bytes > MaxInt
    end;
    ProgressStart;
    DoProgress(zmStart, zsZip, '', Bytes, 0);
    for i := 0 to CompleteFileList.Count - 1 do
      InternalAddFile(CompleteFileList[i], ABaseDir);
    ProgressFinished;
  finally
    if not FDirectAddList then
      FreeStringListWithObjects(CompleteFileList);
    Changed;
  end;
end;

procedure TArchive.AddFiles(const AWildCard: string);
var
  TempList: TStringList;
  FullDir: string;
  BaseDir: string;
  RelDir: string;
  i: integer;
begin
  CheckOpen;
  FullDir := ExpandFileName(AWildCard);
  if zoPathInfo in FZipOptions then
    BaseDir := ExtractFilePath(FullDir)
  else
    BaseDir := '';
  RelDir := ExtractRelativePath(BaseDir, ExtractFilePath(FullDir));
  TempList := TStringList.Create;
  FDirectAddList := True; { vlaggetje zie overloaded AddFiles }
  try
    { zoek files en voeg objecten toe aan templist }
    CreateFileList(TempList, AWildCard, faAllFiles and not faDirectory, True, True, AddSearchEvent);
    { haal zipfile zelf uit de lijst }
    i := TempList.IndexOf(fFileName);
    if i <> -1 then
      ClearStringListItemWithObject(TempList, i);
    if TempList.Count = 0 then
      ZipErrorFmt(Self, ERR_ZIP_NO_MATCHING_FILES, SZipNoMatchingFiles, [AWildCard]);
    TempList.Sorted := True;
    AddFiles(TempList, BaseDir);
{    if zoDeleteAfterZip in fZipOptions then
      if not RemoveDir(ExtractFilePath(FullDir)) then
        windlg('kan dir niet verwijderen'); }
  finally
    FDirectAddList := False;
    FreeStringListWithObjects(TempList);
    if zoLogFile in FZipOptions then SaveLogFile;//(ExtractFilePath(FFileName) + 'Zozip.log')
  end;
end;

procedure TArchive.AddFile(const AFileName: string);
var
  TempList: TStringList;
  FullFileName: string;
//  BaseDir: string;
begin
  CheckOpen;
  Changing;
  FullFileName := ExpandFileName(AFileName);
//  BaseDir := ExtractFilePath(FullFileName);
  if not FileExists(FullFileName) then
    ZipErrorFmt(Self, ERR_FILE_NOT_FOUND, SFileNotFound, [AFileName]);
  TempList := TStringList.Create;
  try
    TempList.Add(FullFileName);
    AddFiles(TempList);
  finally
    TempList.Free;
  end;
  Changed;
end;

procedure TArchive.Changed;
begin
  if FChangeFlag > 0 then
  begin
    Dec(FChangeFlag);
  end;
  if FChangeFlag = 0 then
    if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TArchive.Changing;
begin
  if FChangeFlag = 0 then
  begin
    if Assigned(FBeforeChange) then
      FBeforeChange(Self);
  end;
  Inc(FChangeFlag);
end;

function TArchive.GetFileCount: integer;
begin
  CheckOpen;
  Result := FZipHeader.NumFiles;
end;

procedure TArchive.ClearArchiveList;
begin
  ClearStringListWithObjects(FArchiveList);
end;

procedure TArchive.AddSearchEvent(const ASearchRec: TSearchRec; const aFullName: string; out DoAdd: boolean; out AObject: TObject);
begin
  //Log([ASearchRec.Name]);
  DoAdd := True;
  AObject := TSearchRecObject.Create(ASearchRec);
end;

procedure TArchive.ProgressStart;
begin
  FillChar(FProgress, SizeOf(TProgress), 0);
  Finalize(FProgress); { HIER ZAT GVD EEN MEMORY LEK }
  FIgnoreConfirm := False;
  ClearWarnings;
end;

procedure TArchive.DoProgress(AMode: TZipStateMode;
                              AState: TZipState;
                              const AFileName: string;
                              AMax: Int64;
                              ADelta: Int64;
                              aAbsolute: Boolean = False);
begin
  with FProgress do
  begin
    Mode := AMode;
    State := AState;
    if AFilename <> '' then CurrentFile := AFileName; { --> HIER ZAT EEN MEMORY LEK!!?? }
    //  UniqueString(CurrentFile);
    if AMax <> -1 then
      Max := AMax;
    if not aAbsolute then
      Inc(Done, ADelta)
    else
      Done := aDelta;
    if (State <> zsNone) and (zoProgress in fZipOptions) and Assigned(FOnProgress) then
      FOnProgress(Self, FProgress);
  end;
end;

procedure TArchive.ProgressFinished;
begin
  FProgress.Finished := True;
  if (FProgress.State <> zsNone) and (zoProgress in fZipOptions) then
    if Assigned(FOnProgress) then FOnProgress(Self, FProgress);
  Finalize(FProgress); { HIER ZAT EEN MEMORY LEK!!?? }
end;

procedure TArchive.RemoveFile(const AFileName: string);
begin
  CheckOpen;
  InternalRemoveFile(AFileName);
end;

procedure TArchive.RemoveFiles(const IX: TIntegerList);
begin
  CheckOpen;
  InternalRemove(IX);
end;

procedure TArchive.ExtractFiles(const ANameList: TStringList; const ADir: string);
var
  i, p: integer;
  IX: TIntegerList;
  S: string;
  aMax: Int64;
begin
  CheckOpen;

  IX := TIntegerList.Create;
  try

    for i := 0 to aNameList.Count - 1 do
    begin
      S := aNameList[i];
      p := ArchiveList.IndexOf(S);
      if p = -1 then
        ZipErrorFmt(Self, ERR_EXTRACT_FILE_NOT_FOUND, SExtractFileNoFound, [S]);
      IX.Add(p);
    end;

    if IX.Count > 0 then
    begin
      aMax := GetTotalFileBytes(IX);
      ProgressStart;
      DoProgress(zmStart, zsUnzip, '', aMax, 0);
      for i := 0 to IX.Count - 1 do
        InternalExtractFile(IX[i], ADir);
      DoProgress(zmBusy, zsUnzip, '', -1, aMax, True); // voor de zekerheid
      ProgressFinished;
    end;

  finally
    IX.Free;
  end;
end;

procedure TArchive.ExtractFiles(const ExtractList: TIntegerList; const ADir: string);
var
  i: integer;
  aMax: Int64;
begin
  CheckOpen;
  ProgressStart;
  aMax := GetTotalFileBytes(ExtractList); // 2007-12-07
  DoProgress(zmStart, zsUnzip, '', aMax, 0);
  for i := 0 to ExtractList.Count - 1 do
    InternalExtractFile(ExtractList[i], ADir);
  DoProgress(zmBusy, zsUnzip, '', -1, aMax, True);
  ProgressFinished;
end;

procedure TArchive.ExtractFiles(const AWildCard: string; const ADir: string);
var
  IX: TIntegerList;
  i: integer;
  B: Boolean;
  aMax: Int64;
begin
  CheckOpen;
  IX := TIntegerList.Create;
  try

    for i := 0 to FArchiveList.Count - 1 do
    begin
      B := MatchesMask(FArchiveList[i]{.ArchiveObjects[i].FileName}, AWildCard);
      if B then
        IX.Add(i);
    //  end;
    end;

    if IX.Count > 0 then
    begin
      ProgressStart;
      aMax := GetTotalFileBytes(IX); // 2007-12-07
      DoProgress(zmStart, zsUnzip, '', aMax, 0);
      for i := 0 to IX.Count - 1 do
        InternalExtractFile(IX[i], ADir);
      DoProgress(zmBusy, zsUnzip, '', -1, aMax, True); // voor de zekerheid
      ProgressFinished;
    end;

  finally
    IX.Free;
  end;
end;

procedure TArchive.ExtractAll(const ADir: string);
var
  i: integer;
  aMax: Int64;
begin
  CheckOpen;
  ProgressStart;
  try
    aMax := GetTotalFileBytes(); // toegevoegd 2007-12-07
    DoProgress(zmStart, zsUnzip, '', aMax, 0);
    with FArchiveList do
      for i := 0 to Count - 1 do
        InternalExtractFile(i, ADir);
    DoProgress(zmBusy, zsUnzip, '', -1, aMax, True);
  finally
    if zoLogFile in FZipOptions then SaveLogFile;
  end;
  ProgressFinished;
end;

procedure TArchive.ExtractFile(const AFileName: string; const ADir: string);
var
  List: TStringList;
begin
  CheckOpen;
  List := TStringList.Create;
  List.Add(AFileName);
  try
    ExtractFiles(List, ADir)
  finally
    List.Free;
  end;
end;

procedure TArchive.ExtractFile(aIndex: integer; const ADir: string);
var
  Bytes: integer;
begin
  CheckOpen;
  ProgressStart;
  Bytes := ArchiveList.ArchiveObjects[aIndex].FileSize;
  DoProgress(zmStart, zsUnzip, '', Bytes, 0);
  InternalExtractFile(aIndex, ADir);
  DoProgress(zmBusy, zsUnzip, '', -1, Bytes, True);
  ProgressFinished;
end;

procedure TArchive.ExtractFile(AIndex: integer; aDestStream: TStream);
begin
  InternalExtractFile(aIndex, aDestStream);
end;

procedure TArchive.ExtractFile(const AFileName: string; aDestStream: TStream);
begin
  InternalExtractFile(aFileName, aDestStream);
end;

procedure TArchive.DeleteArchive;
var
  FN: string;
begin
  CheckOpen;
//  if not FIsOpen then Exit;
  Changing;
  FN := FFileName;
  CloseArchive;
  DeleteFile(FN);
  Changed;
end;

procedure TArchive.InternalHandleException(Err: TZipError; const Msg: string);
begin

  ProgressFinished;

(*  if zoLogFile in fZipOptions then
  begin
    try
      FLog.Add(Msg + '(foutcode ' + IntToStr(Ord(Err)));
      //SaveLogFile;//(ExtractFilePath(FFileName) + 'Zozip.log')
    except
    end;
  end; *)
end;

function TArchive.GetSize: integer;
begin
  if FStream <> nil then
    Result := FStream.Size else Result := 0;
end;

procedure TArchive.SetZipOptions(const Value: TZipOptions);
begin
  FZipOptions := Value;
//  Exclude(fZipOptions, zoLogFile);
//  if zoLogFile in fZipOptions
end;

function TArchive.GetWarnings: integer;
begin
  Result := FExceptionList.Count;
end;

procedure TArchive.SaveLogFile;//(const AFileName: string);
//var
  //i: integer;
//var
  //S: string;
begin
(*
//  for i := 0 to FLog.Count - 1 do
  try
    S := 'Zozip Logbestand, aangemaakt op ' + DateTimeToStr(Date);
//    windlg(afilename);
    FLog.Insert(0, S);
    FLog.SaveToFile('c:\zozip.log');
  except
//    Windows.MessageBox(0, PChar('logfile "' + 'c:\zozip.log' + '" niet aanmaken'), PChar('ok'), mb_OK);
    windlg('Kan logbestand "' + 'c:\zozip.log' + '" niet aanmaken');
  end;    //udebug
  *)
end;

{procedure TArchive.SetOption(const Value: TZipOption);
begin

end;}

(*
function TArchive.CalcUnzipBytes(Sel: TintegerList = nil; Compressed: Boolean = False): Int64;
var
  i: integer;
begin
  Result := 0;
  if Sel = nil then
  begin
    with FArchiveList do
      for i := 0 to Count - 1 do
      begin
        if Compressed then
          Inc(Result, ArchiveObjects[i].ArcSize)
        else
          Inc(Result, ArchiveObjects[i].FileSize);
      end;
  end
  else begin
    with FArchiveList do
      for i := 0 to Sel.Count - 1 do
      begin
        if Compressed then
          Inc(Result, ArchiveObjects[Sel[i]].ArcSize)
        else
          Inc(Result, ArchiveObjects[Sel[i]].FileSize);
      end;
  end;
end;
*)

procedure TArchive.CheckOpen;
begin
  if not FIsOpen then
    ZipError(Self, ERR_ARCHIVE_NOT_OPEN, SArchiveNotOpen);
end;

function TArchive.DoConfirm(const AFileName: string): word;
begin

  // standaard overschrijven
  Result := mrYesToAll;

  // check
  if (FIgnoreConfirm) or (zoOverwrite in fZipOptions) then
    Exit;

  // als overschrijven standaard uit dan niet
  if not (zoOverwrite in fZipOptions) then
    Result := mrNo;

  // als confirm optie aan
  if zoConfirmOverwrite in fZipOptions then
  begin
    // event
    if Assigned(FOnConfirmOverwrite) then
    begin
      Result := 0;
      FOnConfirmOverwrite(Self, aFileName, Result);
    end;
  end;

  // voor deze uitpak sessie ignore aanzetten als gewenst
  if Result = mrYesToAll then
    FIgnoreConfirm := True;


{  Result := FOverWrite.ConfirmOverwite(AFileName);
  if Result = mrYesToAll then
    FIgnoreConfirm := True; }
end;

procedure TArchive.ClearWarnings;
begin
  FExceptionList.Clear;
end;

function TArchive.GetTotalFileBytes(aIndexList: TIntegerList = nil; Compressed: Boolean = False): Int64;
// retourneert totale grootte van alle files of een gedeelte daarvan, wanneer aIndexList gevuld is
var
  i: Integer;
begin
  Result := 0;
  if aIndexList = nil then
    case Compressed of
      False:
        for i := 0 to ArchiveList.Count - 1 do
          Inc(Result, ArchiveList.ArchiveObjects[i].FileSize);
      True:
        for i := 0 to ArchiveList.Count - 1 do
          Inc(Result, ArchiveList.ArchiveObjects[i].ArcSize);
    end
  else begin
    case Compressed of
      False:
        for i := 0 to aIndexList.Count - 1 do
          Inc(Result, ArchiveList.ArchiveObjects[aIndexList[i]].FileSize);
      True:
        for i := 0 to aIndexList.Count - 1 do
          Inc(Result, ArchiveList.ArchiveObjects[aIndexList[i]].ArcSize);
    end
  end;
end;

end.


