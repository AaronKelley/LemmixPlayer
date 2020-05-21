(*    dagberekening

jaar, bv 1935. neem 35
deel 35 door 4 en rond af naar beneden (dus 8)
dag, bv de 28e
maandgetal,  voor november4 (zie beneden)
tel op: 35+8+28 (dag) +4(maand) deze 4 getallen optellen
uitkomst delen door 7. het restgetal geldt en geeft de dag aan.
 voor jan t/m dec achtereenvolgens 1-4-4-0-2-5-0-3-6-1-4-6 (dus elke maand een ander getal
[20:51:21] elsjedewolf zegt: data na 2000.....
[20:51:43] elsjedewolf zegt: teller van de breuk in de formule met 1 verminderen
*)

{**************************************************************}
{                                                              }
{    Eric Langedijk                                            }
{                                                              }
{    Algemene types en functies                                }
{                                                              }
{**************************************************************}

unit UMisc;

interface

uses
  Windows, Classes, Sysutils, TypInfo, Math,
  UFastStrings;

{ Algemene constanten en types }

const
  { getallen }
  KiloByte    = 1024;
  MegaByte    = KiloByte * KiloByte;
  GigaByte    = KiloByte * MegaByte;
  KB64        = 64 * KiloByte;
  MinInt      = Low(Integer);
  CrLf        = Chr(13) + Chr(10);

  { bits }
  Bit0  = 1;         {1}
  Bit1  = 1 shl 1;   {2}
  Bit2  = 1 shl 2;   {4}
  Bit3  = 1 shl 3;   {8}
  Bit4  = 1 shl 4;   {16}
  Bit5  = 1 shl 5;   {32}
  Bit6  = 1 shl 6;   {64}
  Bit7  = 1 shl 7;   {128}
  Bit8  = 1 shl 8;   {256}
  Bit9  = 1 shl 9;   {512}
  Bit10 = 1 shl 10;  {1024}
  Bit11 = 1 shl 11;  {2048}
  Bit12 = 1 shl 12;  {4096}
  Bit13 = 1 shl 13;  {8192}
  Bit14 = 1 shl 14;  {16384}
  Bit15 = 1 shl 15;  {32768}
  Bit16 = 1 shl 16;  // Etc :)
  Bit17 = 1 shl 17;
  Bit18 = 1 shl 18;
  Bit19 = 1 shl 19;
  Bit20 = 1 shl 20;
  Bit21 = 1 shl 21;
  Bit22 = 1 shl 22;
  Bit23 = 1 shl 23;
  Bit24 = 1 shl 24;
  Bit25 = 1 shl 25;
  Bit26 = 1 shl 26;
  Bit27 = 1 shl 27;
  Bit28 = 1 shl 28;
  Bit29 = 1 shl 29;
  Bit30 = 1 shl 30;
  Bit31 = 1 shl 31;

type
  String1   = string[1];
  String2   = string[2];
  String3   = string[3];
  String4   = string[4];
  String5   = string[5];
  String6   = string[6];
  String7   = string[7];
  String8   = string[8];
  String9   = string[9];
  String10  = string[10];
  String11  = string[11];
  String12  = string[12];
  String13  = string[13];
  String14  = string[14];
  String15  = string[15];
  String16  = string[16];
  String20  = string[20];
  String31  = string[31];
  String32  = string[32];
  String40  = string[40];
  String63  = string[63];
  String64  = string[64];
  String79  = string[79];
  String80  = string[80];
  String127 = string[127];
  String128 = string[128];

{ pointers naar basische types }
type
//  PByte = T
//  pByte          = ^byte; // declared in Types and System (starting from delphi 7 I think)
  pShortint      = ^shortint;
//  pInteger       = ^integer; defined in types unit since delphi 7
  pWord          = ^word;
  pLongint       = ^longint;
  pReal          = ^real;
  //  pShortString   = ^ShortString;  staat al in delphi system unit
  pBoolean       = ^boolean;
  PPointer       = ^Pointer;

const
  MaxBytes = MaxListSize * 4; // MaxListSize = Maxint div 16;

type
  PBytes = ^TBytes;
  TBytes = array[0..MaxBytes - 1] of Byte;

type
  TWordRec = packed record
  case Byte of
    0: (AsWord: Word);
    1: (Lo, Hi: Byte);
    2: (B0, B1: Byte)
  end;

{ sets }
type
  TCharSet = set of char;
  PCharSet = ^TCharSet;
  TByteSet = set of byte;
  PByteSet = ^TByteSet;

const
  AlphaCharSet = ['a'..'z', 'A'..'Z'];
  DigitCharSet = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

type
  TIntArray = array[0..MaxListSize - 1] of integer;
  PIntArray = ^TIntArray;
  TPointArray = array[0..MaxListSize div 2 - 1] of TPoint;
  PPointArray = ^TPointArray;
  TOpenBooleanArray = array of boolean;
  TOpenByteArray = array of Byte;
  TOpenStringArray = array of string;
  TOpenIntegerArray = array of integer;
  TOpenPointerArray = array of Pointer;
  TOpenVarRecArray = array of TVarRec;
  TOpenExtendedArray = array of Extended;
  TOpenVariantArray = array of Variant;
  TOpenVariantPointerArray = array of PVariant;
  TOpenRectArray = array of TRect;

{ Chars en Strings }

function AllDigits(const S: string; Count: Integer = 0): Boolean;
    { checkt of alle letters cijfers zijn, met een optionele count. als count = 0 dan }
function Bit8Str(B: Byte; Plain: Boolean = True): string;
    { geeft een string van een 8 bits getal byte signed of unsigned }
function Bit16Str(W: Word; Plain: Boolean = True): string;
    { geeft een string van een 16 bits getal signed of unsigned }
function Bit32Str(L: LongWord; Plain: Boolean = True): string;
    { geeft een string van een 32 bits getal signed of unsigned }
function BlancStr(const S: string): boolean;
    { geeft true als string leeg is of alleen maar uit spaties bestaat }
function BoolStr(B: boolean): string;
    { converteert een boolean naar een string }
function CharPos(C: Char; const S: string): Integer;
    { sneller dan Pos wanneer het een char betreft }
function CharPosR(C: Char; const S: string): Integer;
    { sneller dan Pos wanneer het een char betreft }
function CharSetToString(const aCharSet: TCharSet): string;
    { geeft alle letters uit set }
function CharsToStr(const aChars: array of char): string;
    { converteert een array of char naar eene string }
function CountChars(C: Char; const S: string): integer;
    { telt aantal chars in string }
function GetChar(const S: string; N: Integer): Char;
    { returns the nth character of S if there is one, if not it returns #0}
function CutFileName(const S: string; Max: integer = 128): string;
    { maakt evt. van een erg lang pad een afgekort pad en maakt van het begin bijv. "C:\..\eric.pas"}
function CutLeft(const S: string; aLen: integer): string;
    { haal aan begin van string chars weg }
function CutRight(const S: string; aLen: integer): string;
    { haal aan eind van string chars weg }
function ThousandStr(Int: Integer): string;
    { string met punten tussen de duizendtallen }
function EmptyStr(const S: string): boolean;
    { true als string leeg is of alleen uit spaties bestaat }
function FirstNice(const S: string): string;
    { eerste letter een hoofdletter, rest klein }
function AllNice(const S: string): string;
    { alle eerste letters van een woord hoofdletter, rest van woord klein }
function ElfProef(const Banknummer: String): Boolean;
    { checkt een bankrekeningnummer (NLDelphi PsychoMark) }
function FastReplacePart(const aSource: String; const aStart, aLength: Integer; const aReplace: String): String;
    { replace als je positie al weet (NLDelphi PsychoMark)}
function FilterStr(const S: string; const aFilter: TCharSet; Inverted: boolean = false): string;
    { haalt alleen geldige letters uit string, als inverted alleen letters die *niet* in aFilter zitten }
function GetStrAllocSize(const S: string): Longint;
  { GetStrAllocSize returns the memory allocation size of a given string. NB: alleen voor longstrings! }
function GetStrRefCount(const S: string): Longint;
  { GetStrRefCount returns the reference count of a given string. NB: alleen voor longstrings!}
function HexPtr(P: pointer): string;
    { geeft hexadecimale notatie van pointer }
function HexStr(I: integer): string;
    { geeft hexadecimale notatie van integer }
function HexStr64(I: Int64{; Mode: Byte = 0}): string;
    { geeft hexadecimale notatie van integer }
function IntPointStr(N: integer): string;
    { geeft integer met duizend seperators }
function InvertStr(const S: string): string;
    { draait string om }
function i2s(i: integer): string;
    { str }
function f2s(const D: Double): string;
    { float naar string }
function s2f(const S: string): Double;
    { string naar float }
function KeepChars(const S: string; aCharSet: TCharSet): string;
    { haalt alle chars weg die niet in de set zitten }
function LastChar(const S: string): Char;
    { laatste letter van string}
function LeadZeroStr(Int, Len: integer): string;
    { lijdende nullen voor getal }
function LoCase(C: char): char;
    { tegenhanger van upcase }
function Lstr(const S: string; aLen: integer): string;
    { plakt evt. spaties achter string, of geeft stukje van string }
function MatchingChars(const A, B: string; CaseSensitive: boolean = False): integer;
    { kijkt hoeveel karakters van A en B overeenkomen van karakter 1 }
function PadC(const S: string; aLen: integer; PadChar: char = ' '): string;
    { = Centerstring }
function PadL(const S: string; aLen: integer; PadChar: char = ' '): string;
    { = RStr met een te kiezen karakter }
function PadR(const S: string; aLen: integer; PadChar: char = ' '): string;
    { = Lstr met een te kiezen karakter }
function PassWordStr(const S: string): string;
    { '*' ipv char }
function Prime(N: Integer): Boolean;
    { priemgetal }
function RandomAlphaChar: Char;
    { geeft een random char 'a'..'z' }
function RandomStr(M: integer; Fixed: boolean = False): string;
    { eenvoudige random string (alleen a..z, A..Z) }
function RandomStrAdv(M: integer; RandomLineFeeds: Integer = 0): string;
    { geavanceerde randomstring }
function RectStr(const R: TRect; Aligned: boolean = False): string;
    { geeft coordinaten string }
procedure RemoveLeadingZeros(var S: string);
    { verwijderd lijdende nullen }
function ReplaceChar(const S: string; aFrom, aTo: Char): string;
    { vervangt karakters in string }
function ReplaceFileExt(const aFileName, aNewExt: string): string;
    { vervangt fileext door nieuwe ext, als niet gevonden dan geen verandering }
function RStr(const S: string; aLen: integer): string;
    { plakt evt. spaties voor string, of geeft rechterstuk van string }
function s2i(const S: string): integer;
    { als StrToInt }
function SafeFormatStr(const S: string; const Args: array of const): string;
    { Als format maar dan zonder exception }
function SetToStr(const aSetTypeInfo: PTypeInfo): string;
    { parameter moet typeinfo(aSet) zijn }
function ShiftStateString(Shift: TShiftState): string;
function ShortUpperCase(const S: ShortString): ShortString;
    { speciaal voor shortstrings: veel sneller dan sysutils.uppercase }
function ShortLowerCase(const S: ShortString): ShortString;
    { speciaal voor shortstrings: veel sneller dan sysutils.uppercase }
function SplitString(const S: string; Index: integer; Seperator: Char; DoTrim: Boolean = True): string;
    { haal string uit Seperated string }
function SplitStringCount(const S: string; Seperator: Char): integer;
    { geeft aan hoeveel splitstrings er in S zitten }
function SplitString_To_StringList(const S: string; Seperator: Char;
  DoTrim: boolean = False): TStringList; overload;
    { sloopt <S>, gescheiden door een <Seperator> uit elkaar in meerdere strings }
procedure SplitString_To_StringList(const S: string; AList: TStringList; Seperator: Char;
  DoTrim: boolean = False); overload;
    { sloopt <S>, gescheiden door een <Seperator> uit elkaar in meerdere strings }
function SplitString_To_StringArray(const S: string; Seperator: Char; DoTrim: boolean = False): TOpenStringArray;
    { sloopt <S>, gescheiden door een <Seperator> uit elkaar in meerdere strings }
function StringArray_To_SplitString(const Ar: array of string; Seperator: Char): string;
    { maakt splitstring van een stringarray}
function StringList_To_SplitString(const AList: TStrings; Seperator: Char): string;
    { de omgekeerde van SplitString_To_StringList }
function StringArray_To_StringList(const Ar: array of string): TStringList; overload;
    { functie creeert een stringlist van Ar}
procedure StringArray_To_StringList(const Ar: array of string; List: TStringList); overload;
    { vult List met Ar}
function StringArray_To_VariantArray(const Ar: array of string): TOpenVariantArray;
    { conversie strings naar varianten }
procedure StringToFile(const aString, aFileName: string);
    { bewaar string als file }    
function SetStringArray(const Ar: array of string): TOpenStringArray;
    { procedure om snel even wat waarden in een array te gooien }
function SetIntegerArray(const Ar: array of integer): TOpenIntegerArray;
    { procedure om snel even wat waarden in een array te gooien }
function SetVariantArray(const Ar: array of Variant): TOpenVariantArray;
    { procedure om snel even wat waarden in een array te gooien }
function StrAllInSet(const S: string; const aCharSet: TCharSet): boolean;
    { kijkt of alle letters van string in aCharset zitten }
function Transform(const AVarRec: TVarRec): string;
    { transformeert een varrec naar string }
function StrToFloatDef(const S: string; const DefValue: Extended = 0): Extended; overload;
    { = StrToFloat, maar geeft DefValue terug bij exception. NB StrToIntDef staat in sysutils }
function StringToCharSet(const S: string; const Keep: TCharSet = []): TCharSet;
    { geeft alle letters uit set }


    //function QuotedStr(const S: string): string;
  //  { haalt dubbele of enkele aanhalingstekens om S weg }
function UnQuotedStr(const S: string): string;
    { haalt dubbele of enkele aanhalingstekens om S weg }
function VTypeStr(const AType: word): string;
function VarTypeStr(const V: Variant): string;

{ Getallen }

function Percentage(Max, N: Double): Double; overload;
function Percentage(Max, N: integer): integer; overload;
function Percentage(Max, N: Int64): integer; overload;
function GetKB(N: integer): integer;
function RandomDouble(const Max: Double): Double;
    { random floating point }

function Wrap(aValue, aMax: Integer): Integer; //overload;
//function Wrap(var aValue: Integer; aMax: Integer): Boolean; overload;
                                    //math
{ Datum en Tijd }

function MilliSeconds: integer;
    { geeft milliseconden sinds middernacht }

function DaysInMonth(AYear, AMonth: Integer): Integer;
    { hoeveel dagen in deze maand }

function FirstOfHalfYear(const D: TDateTime): TDateTime;
function FirstOfQuarter(const D: TDateTime): TDateTime;
function FirstOfMonth(const D: TDateTime): TDateTime;
function DtoS(const aDate: TDateTime): ShortString;
    { output: 20011231 }
function TtoS(const ATime: TDateTime; const aSeparator: string = ''): ShortString;
    { output: 20011231 + aSeparator + 124559 }
function GetEncodeDate(Y, M, D: word): TDateTime;
    { DecodeDate als function }
//function EncodeDateTime(): TDateTime    
function GetDay(const aDate: TDateTime): Word;
function GetMonth(const aDate: TDateTime): Word;
function GetYear(const aDate: TDateTime): Word;
function StoD(const aDateStr: ShortString; const Def: TDateTime = 0): TDateTime;
    { input: 20011231 geen check! }
function UserStrToDate(const DateStr: string): TDateTime;
    { slimme string naar datum. EConvertError exception bij fout }
function WeekOfYear(const aDate: TDateTime) : Integer;
    { geeft weeknummer van het jaar van een datum }
function IncYears(const ADate: TDateTime; Count: integer): TDateTime;
//function StrToDateDef(const S, ShortFormat: string; const ErrorDate: TDateTime = 0): TDateTime;
    { = StrToDate uit SysUtils, maar geeft 0 terug bij exception }

{ Memory }

procedure FillDWord(var Dest; Count, Value: Integer);
    { vult een stuk geheugen met DWords }
procedure FastMove(const Source; var Dest; Count: Integer);
function MemEmpty(var A; Size: integer): boolean;
function MemAllocated: Cardinal;
procedure SetPointerListSize(var PointerList: TPointerList; ASize: integer);
function EqualRects(const R1, R2: TRect): boolean;
function EqualPoints(const P1, P2: TPoint): boolean;

{ Files }

function CompareFiles(const FileName1, FileName2: string; out DiffPos: Integer): boolean;
    { reraised exception als één van de files niet geopend kan worden }

{ Bits }

function TestBit8(B, Bit: Byte): Boolean;
procedure SetBit8(var B: byte; Bit: Byte);
procedure ClearBit8(var B: byte; Bit: Byte);

function TestBit64(const B: Int64; Bit: Byte): Boolean;
procedure SetBit64(var B: Int64; Bit: Byte);
procedure ClearBit64(var B: Int64; Bit: Byte);

function Test_UL_Bit(UL: LongWord; Bit: Byte): Boolean; //unsigned long

{ TRect TPoint }

procedure RectGrow(var aRect: TRect; DeltaX, DeltaY: integer);
  { kanweg, deze staat ook in Windows.pas: InflateRect }
function RectHeight(const aRect: TRect): integer;
function RectWidth(const aRect: TRect): integer;
procedure RectMove(var R: TRect; X, Y: integer); {windows.offsetrect}
procedure RectAssign(var ARect: TRect; X, Y, W, H: integer); { en staat deze dan niet in windows? }
function ZeroTopLeftRect(const aRect: TRect): TRect;
function CenterRect(const Child, Parent: TRect): TRect;

//function ClientRectToScreenRect(const aControl: TControl): TRect;

{ Encrypt }

procedure EncryptMem(var Mem; ASize: integer);
procedure DecryptMem(var Mem; ASize: integer);
function EncryptStr(const S: string): string;
function DecryptStr(const S: string): string;
function CheckSum(var Mem; ASize: integer): LongWord; overload;
function CheckSum(aStream: TStream): LongWord; overload;

{ Range }

function Between(X, A, B: char): boolean; overload;
function Between(X, A, B: integer): boolean; overload;
function Between(const X, A, B: TDateTime): boolean; overload;
function Between(const S, A, B: string): boolean; overload;
function Limit(AValue, AMin, AMax: integer): integer;
procedure Restrict(var aValue: integer; aMin, aMax: integer);
function RestrictValue(aValue: integer; aMin, aMax: integer): Integer;
function Restricted(var aValue: integer; aMin, aMax: integer): Boolean;

{ IIFS }

function IIF(aExpr: boolean; const aTrue, aFalse: pointer): pointer; overload;
function IIF(aExpr, aTrue, aFalse: boolean): boolean; overload;
function IIF(aExpr: boolean; const aTrue, aFalse: integer): integer; overload;
function IIF(aExpr: boolean; const aTrue, aFalse: string): string; overload;
function IIF(aExpr: boolean; const aTrue, aFalse: Char): Char; overload;
function IIF(aExpr: boolean; const aTrue, aFalse: Currency): Currency; overload;
function IIFSTRING(aExpr: boolean; const aTrue, aFalse: string): string;

{ Zoeken, vergelijken  }

{ De InStrList routines retourneren de index van S in AList of -1 als niet gevonden.
  Er kan niet met default parameters gewerkt worden wegens een bug in delphi 5.
  Zie readme.txt in delphi 5}
function InStrList(const S: string; const AList: array of string; CaseSensitive: boolean): integer; //overload;
function InIntList(I: integer; const aList: array of integer): integer;
function IntArraysEqual(A, B: TOpenIntegerArray): boolean;
procedure AllocIntArray(var Ar: PIntArray; Count: integer);

{ Conversie }

function StrListToArray(AList: TStringList): TOpenStringArray;
    { converteert stringlist naar stringarray }
//function StringArray_To_StringList(const Ar: AList: TStringList): TOpenStringArray;

{ Sorteren }

function SortStr(const S: string): string;
procedure SortStrings(var Strings: array of string; CaseSensitive: boolean = True);

{ Swaps }

procedure Swap(var A, B; Size: integer);
procedure SwapBytes(var A, B: byte);
procedure SwapShortInts(var A, B: ShortInt);
procedure SwapChars(var A, B: char);
procedure SwapInts(var A, B: integer);
procedure SwapWords(var A, B: word);
procedure SwapStrings(var A, B: string);

{ nibble swap }
function SwapWord(W: Word): Word;
//procedure SwapPoints(var )

{ Toetsenbord }

function GetKeyBoardShiftState: TShiftState;

{ Components }

function ComponentToString(Component: TComponent): string;
procedure StringToComponent(Value: string; C: TComponent);
procedure ComponentToFile(C: TComponent; const AFileName: string);
procedure FileToComponent(const AFileName: string; C: TComponent);
procedure ComponentToTextFile(C: TComponent; const AFileName: string);
procedure TextFileToComponent(const aFileName: string; C: TComponent);
function UniqueComponentName(const aBaseName: string; aOwner: TComponent): string;

type
  TComponentMethod = procedure(Com: TComponent) of object;

procedure ForEachComponent(aComponent: TComponent; Method: TComponentMethod;
  SubComponents: boolean; const Filter: array of TComponentClass;
  IncludeMain: Boolean = False); //overload;

{ procedure LocalProc(const aComponent: TComponent); far;}
//procedure ForEachComponent(aComponent: TComponent; LocalProc: pointer;
  //SubComponents: boolean); overload;

//procedure SetDisplayDragImage(Parent: TControl);
    { zorgt ervoor dat control + subcontrols dragimages laten zien }

{ Exception handling algemeen }

type
  EAppError = class(Exception);

procedure AppError(const Msg: string; Sender: TObject = nil);
procedure AppErrorFmt(const Msg: string;  const Args: array of const; Sender: TObject = nil);
procedure ShowError(ExceptObject: TObject; ExceptAddr: Pointer);

procedure WinDlg(const S: string); overload;
procedure WinDlg(const AValues: array of const); overload;

{ diversen }
function GetHardDiskSerialNumber: DWORD;
procedure GetProjectVersionInfo(AVersionList: TStrings; AFileName: string = '');
function GetBuildInfo(var V1, V2, V3, V4: Word; AFileName: string = ''): Boolean;
function GetBuildInfoAsString(AFileName: string = ''): string;

function GetApplicationPath: string;
function GetApplicationName: string;


const
  EmptyRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

type
  TDebProc = procedure(const aValues: array of const);

procedure SetDebProc(P: TDebProc);
procedure Deb(const aValues: array of const);
procedure Log(const aValues: array of const);

resourcestring
// origineel uit sysconst:  SMyException = 'Fout %s in module %s at %p.'#$0A'%s%s';
  SMyException = 'Fout: %s in module %s.' + Chr(13) +
                 'Adres: %p.' + Chr(13) + Chr(13) +
                 '%s%s';
  SMyExceptTitle = 'Applicatie Fout';


implementation

var
  _DebProc: TDebProc = nil;

procedure SetDebProc(P: TDebProc);
begin
  _DebProc := P;
end;

procedure Deb(const aValues: array of const);
begin
  if Assigned(_DebProc) then
    _DebProc(aValues);    //ulog
end;

procedure Log(const aValues: array of const);
begin
  Deb(aValues);
end;


(*type
  PTextFile = ^TextFile;
var
  _LogTxt: PTextFile; verhuisd naar ulog *)

function AllDigits(const S: string; Count: Integer = 0): Boolean;
var
  i: Integer;
  C: Char;
begin
  Result := False;
  if Count = 0 then
    Count := Length(S);
  for i := 1 to Count do
  begin
    C := S[i];
    if not (C in DigitCharSet) then
      Exit;
  end;
  Result := True;
end;

function Bit8Str(B: Byte; Plain: Boolean = True): string;
var
  i: Integer;
begin
  Result := StringOfChar('0', 8);
  case Plain of
    False: for i := 0 to 7 do if B and (1 shl i) <> 0 then Result[(7 - i) + 1] := '1';
    True : for i := 0 to 7 do if B and (1 shl i) <> 0 then Result[i + 1] := '1';
  end;
end;

function Bit16Str(W: Word; Plain: Boolean = True): string;
var
  i: Integer;
begin
  Result := StringOfChar('0', 16);
  case Plain of
    False: for i := 0 to 15 do if W and (1 shl i) <> 0 then Result[(15 - i) + 1] := '1';
    True : for i := 0 to 15 do if W and (1 shl i) <> 0 then Result[i + 1] := '1';
  end;
end;

function Bit32Str(L: LongWord; Plain: Boolean = True): string;
var
  i: Integer;
begin
  Result := StringOfChar('0', 32);
  case Plain of
    False: for i := 0 to 31 do if L and (1 shl i) <> 0 then Result[(7 - i) + 1] := '1';
    True : for i := 0 to 31 do if L and (1 shl i) <> 0 then Result[i + 1] := '1';
  end;
end;

{function Bit_UB_Str(UB: Byte): string;
var
  i: Byte;
begin
  Result := '00000000';
  for i := 0 to 7 do
    if UB and (1 shl i) <> 0 then
      Result[(7 - i) + 1] := '1'
end;}

function BlancStr(const S: string): boolean;
begin
  Result := (S = '') or (S = StringOfChar(' ', Length(S)));
end;

function BoolStr(B: boolean): string;
begin
 if B then Result := 'TRUE' else Result := 'FALSE';
end;

function CharPos(C: Char; const S: string): Integer;
begin
  for Result := 1 to Length(S) do
    if S[Result] = C then Exit;
  Result := 0;
end;

function CharPosR(C: Char; const S: string): Integer;
begin
  for Result := Length(S) downto 1 do
    if S[Result] = C then Exit;
  Result := 0;
end;

function CharSetToString(const aCharSet: TCharSet): string;
var
  C: Char;
begin
  Result := '';
  for C := #0 to #255 do
    if C in aCharset then
      Result := Result + C;
end;

function CharsToStr(const aChars: array of char): string;
var
  i: integer;
begin
  Result := '';
  for i := Low(aChars) to High(aChars) do
    Result := Result + aChars[i];
{  SetLength(Result, Length(AChars));
  Move(AChars, Result[1], Length(AChars)); kan dit? moet nog testen }
end;

function CountChars(C: Char; const S: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then inc(Result);
end;

function GetChar(const S: string; N: Integer): Char;
begin
  if N <= 0 then
    Result := #0
  else if Length(S) >= N then
    Result := S[N]
  else
    Result := #0;  
end;

function CutFileName(const S: string; Max: integer = 128): string;
begin
  if Max < 12 then Max := 12;
  if Length(S) <= Max then
    Result := S
  else
    Result := Copy(S, 1, 3) + '...' + Copy(S, Length(S) - 125, 125) ;
end;

function CutLeft(const S: string; aLen: integer): string;
begin
{  if aLen + 1 >= Length(S)
  then Result := ''
  else }
  Result := Copy(S, aLen + 1, Length(S));
end;

function CutRight(const S: string; aLen: integer): string;
begin
  Result := Copy(S, 1, Length(S) - aLen);
end;

function ThousandStr(Int: Integer): string;
begin
  Result := FloatToStrF(Int/1, ffNumber, 15, 0);
  //  Result := Format('%.0n', [Int/1]);
end;

function EmptyStr(const S: string): boolean;
begin
  Result := (S = '') or (S = StringOfChar(' ', Length(S)));
end;

function FirstNice(const S: string): string;
var
  i: integer;
begin
  Result := S;
  if Length(S) = 0 then
    Exit;
  Result[1] := UpCase(Result[1]);
  for i := 2 to Length(Result) do
    Result[i] := LoCase(Result[i]);
end;

function AllNice(const S: string): string;
var
  i: integer;
  Up: boolean;
begin
  Result := S;
  Up := True;
  Result := S;
  for i := 1 to Length(Result) do
  begin
    if Result[i] <> ' ' then
      Result[i] := IIF(Up, Upcase(Result[i]), {Result[i]}LoCase(Result[i]));
    Up := Result[i] = ' ';
  end;
end;

function ElfProef(const Banknummer: String): Boolean;
var
  sFiltered:      String;
  iCount:         Integer;
  iChar:          Integer;
  iValue:         Integer;

begin
  Result  := False;

  // Controleer minimum lengte
  if Length(Banknummer) >= 9 then begin
    // Filter alle niet-getallen
    SetLength(sFiltered, Length(Banknummer));
    iCount  := 0;

    for iChar := 1 to Length(Banknummer) do
      if Banknummer[iChar] in ['0'..'9'] then begin
        Inc(iCount);
        sFiltered[iCount] := Banknummer[iChar];
      end;

    SetLength(sFiltered, iCount);

    // Controleer nieuwe lengte
    if Length(sFiltered) = 9 then begin
      iCount  := 0;

      // Tel alle getallen op
      //
      //   Elfproef theorie:
      //      Getal1 * 9 + Getal2 * 8 + Getal3 * 7, enz. moet deelbaar
      //      zijn door 11...
      for iChar := 1 to Length(sFiltered) do begin
        iValue  := Ord(sFiltered[iChar]) - Ord('0');
        iCount  := iCount + (iValue * (10 - iChar));
      end;

      // Elfproef
      if (iCount mod 11) = 0 then
        Result  := True;
    end;
  end;
end;


function FastReplacePart(const ASource: String; const AStart, ALength: Integer;
                         const AReplace: String): String;
var
  iSrcLength: Integer;
  iLength:    Integer;
  iDiff:      Integer;
  iDest:      Integer;

begin
  iSrcLength  := Length(ASource);
  iLength     := Length(AReplace);
  iDiff       := iLength - ALength;
  iDest       := 1;

  SetLength(Result, iSrcLength + iDiff);

  // Write first part
  CopyMemory(@Result[iDest], @ASource[1], AStart - 1);
  Inc(iDest, AStart - 1);

  // Write replacement
  CopyMemory(@Result[iDest], @AReplace[1], iLength);
  Inc(iDest, iLength);

  // Write last part
  CopyMemory(@Result[iDest], @ASource[AStart + ALength],
             iSrcLength - AStart - (ALength - 1));
end;

function FilterStr(const S: string; const aFilter: TCharSet;
  Inverted: boolean = false): string;
var
  i: integer;
begin
  Result := '';
  case Inverted of
    False:
      for i := 1 to Length(S) do
        if S[i] in aFilter then
          Result := Result + S[i];
    True:
      for i := 1 to Length(S) do
        if not (S[i] in aFilter) then
          Result := Result + S[i];
  end;
end;

function GetStrAllocSize(const S: string): Longint;
var
  P: ^Longint;
begin
  Result := 0;
  if Length(S) = 0 then Exit;

  P := Pointer(S);                        // pointer to string structure
  dec(P, 3);                              // 12-byte negative offset
  Result := P^ and not $80000000 shr 1;   // ignore bits 0 and 31
end;

function GetStrRefCount(const S: string): Longint;
var
  P: ^Longint;
begin
  Result := 0;
  if Length(S) = 0 then Exit;
  
  P := Pointer(S);                        // pointer to string structure
  dec(P, 2);                              // 8-byte negative offset
  Result := P^;
end;

function HexPtr(P: pointer): string;
begin
  Result := '$' + Format('%p', [P]);
end;

function HexStr(I: integer): string;
begin
  Result := '$' + Format('%x', [i]);
end;

function HexStr64(I: Int64{; Mode: Byte = 0}): string;
// 0 = $ at the beginning
// 1 = h at the end
// otherwise nothing
begin
(*function Hex64(const B: Int64; Prefix: Boolean = True): string;
begin
  Result := IIF(Prefix, '$', '') + IntToHex(B, 16);
end; *)
  Result := '$' + IntToHex(I, 16);
  //Format('%x', [i]);
end;

function IntPointStr(N: integer): string;
//var
//  i, j: integer;
//  PointAr: array of integer;
begin
  Result := IntToStr(N);
  //if Length(Result) <= 3 then Exit;
{
  for i := Length(Result) downto 1 do
  begin
    Inc(j);
    if (J mod 3 = 0) and (i <> 1) then
    begin
      SetLength(PointAr, Length(PointAr) + 1);
      Insert('.', Result, i);      sysutils
    end;
  end;}

end;

function InvertStr(const S: string): string;
var
  L, i: integer;
begin
  L := Length(S);
  SetLength(Result, L);
  if L = 0 then Exit;
  for i := 1 to L do
  begin
    Result[L - i + 1] := S[i];
  end;
end;

function i2s(i: integer): string;
begin
  Str(i, Result);
end;

function f2s(const D: Double): string;
begin
  Result := FloatToStr(D);
//  Str(D, Result);
end;

function s2f(const S: string): Double;
begin
  //Result := StrToFloat(S);
  if not TextToFloat(PChar(S), Result, fvExtended) then
    Result := 0;
//    ConvertErrorFmt(@SInvalidFloat, [S]);

end;

function KeepChars(const S: string; aCharSet: TCharSet): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if S[i] in aCharSet then
      Result := Result + S[i];
end;

function LastChar(const S: string): Char;
begin
  Result := #0;
  if Length(S) > 0 then
    Result := S[Length(S)];
end;

function Lstr(const S: string; aLen: integer): string;
begin
  { de compiler checkt intern al op negatieve waarden, daar maken we gebruik van }
  Result := Copy(S, 1, aLen) + StringOfChar(' ', aLen - Length(S));
end;

function MatchingChars(const A, B: string; CaseSensitive: boolean = False): integer;
var
  i: integer;
begin
  Result := 0;
  case
    CaseSensitive of
      False:
        for i := 1 to Min(Length(A), Length(B)) do
          if UpCase(A[i]) = UpCase(B[i]) then Inc(Result);
      True:
        for i := 1 to Min(Length(A), Length(B)) do
          if A[i] = B[i] then Inc(Result);
  end;
end;

function PadC(const S: string; aLen: integer; PadChar: char = ' '): string;
var
  i, N: Integer;
begin
  if aLen < Length(S) then
    Result := S
  else begin
    Result := StringOfChar(' ', aLen);
    N := (aLen - Length(S)) div 2;
    for i := 1 to Length(S) do
      Result[i + N] := S[i];
  end;
end;

function PadL(const S: string; aLen: integer; PadChar: char = ' '): string;
begin
  Result := StringOfChar(PadChar, aLen - Length(S)) + Copy(S, Length(S) - aLen + 1, aLen);
end;

function PadR(const S: string; aLen: integer; PadChar: char = ' '): string;
begin
  { de compiler checkt intern al op negatieve waarden, daar maken we gebruik van }
  Result := Copy(S, 1, aLen) + StringOfChar(PadChar, aLen - Length(S));
end;

function PassWordStr(const S: string): string;
begin
  Result := StringOfChar('*', Length(S));
{  if S = '' then
    Result := S
  else
    Result := '********'; }
end;

Function Prime(N: Integer): Boolean; // van internet. volstrekt onleesbaar
//Determines if argument is prime
var
  C: Integer;
  S: Real;
  X: Boolean;
Begin;
 N:=ABS(N);
 S:=SQRT(N);
 X:=( (N<=2) OR (ODD(N)) AND (S <> INT(S) ) );
 If X then Begin
  C:=3;
  While (X AND (C < Int(S))) do Begin
   X:=((N Mod C) > 0);
   C:=C+2;
  End; //While
 End; //If X
 Prime:=X;
End;

function ReplaceChar(const S: string; aFrom, aTo: Char): string;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to length(Result) do
    if Result[i] = aFrom then
      Result[i] := aTo;
end;

function ReplaceFileExt(const aFileName, aNewExt: string): string;
var
  Ext: string;
begin
  Ext := ExtractFileExt(aFileName);
  if Ext <> '' then
  begin
    Result := CutRight(aFileName, Length(Ext)) + aNewExt;
  end;
end;

function RStr(const S: string; aLen: integer): string;
begin
  { de compiler checkt intern al op negatieve waarden, daar maken we gebruik van }
  Result := StringOfChar(' ', aLen - Length(S)) + Copy(S, Length(S) - aLen + 1, aLen);
end;

function s2i(const S: string): integer;
var
  Code: integer;
begin
  {$I-}
  Val(S, Result, Code);
  {$I+}
  if Code <> 0 then
    Result := 0;
end;

function RandomAlphaChar: Char;
begin
  Result := Chr(Random(ord('z') - ord('a')));
end;

function RandomStr(M: integer; Fixed: boolean = False): string;
var
  i: integer;
  RandomLength: integer;
begin
  if Fixed then
    RandomLength := M
  else
    RandomLength := Random(M) + 1;
  SetLength(Result, RandomLength);
  for i := 1 to RandomLength do
    Result[i] := Chr(Random(26) + Ord('a') - Random(2) * (Ord('a') - Ord('A')));
end;

function RandomStrAdv(M: integer; RandomLineFeeds: Integer = 0): string;
var
  i: integer; k: byte;
  C: Integer;
  multicount: integer;
begin
  Result := '';

  multicount := random(randomlinefeeds);
  for c := 0 to multicount do
  begin

    for i:=1 to random(m)+1 do
    begin
      k:=random(40);
      case k of
        0..9:
          Result := Result + chr(ord('0')+k);
        10..36:
          if random(2)=1 then
            Result := Result +chr(ord('@')+k-9)
          else
            Result := Result + lowercase(chr(ord('@')+k-9));
        else  Result:=Result+' ';
      end;
    end;

    if c <> MultiCount then
    Result := Result + chr(13) + Chr(10);

  end;
//  if MultiLine
end;

function RectStr(const R: TRect; Aligned: boolean = False): string;
begin
  if not Aligned then
    Result := i2s(R.Left) + ', ' + i2s(R.Top) + ', ' + i2s(R.Right)+ ', ' + i2s(R.Bottom)
  else
    Result := PadL(i2s(R.Left), 3) + ', ' + PadL(i2s(R.Top), 3) + ', ' + PadL(i2s(R.Right), 3) + ', ' + PadL(i2s(R.Bottom), 3);
end;

procedure RemoveLeadingZeros(var S: string);
var
  i, Count: Integer;
begin
  if  Length(S) = 0 then
    Exit;
  Count := 0;
  i := 1;
  while S[i] = '0' do
  begin
    Inc(Count);
    Inc(i);
    if i > Length(S) then Break;
  end;
  S := CutLeft(S, Count);
end;

function LeadZeroStr(Int, Len: integer): string;
var
  i: byte;
begin
  Str(Int:Len, Result);
  i := 1;
  while Result[i] = ' ' do
  begin
    Result[i] := '0';
    Inc(i);
  end;
end;

function InStrList(const S: string; const aList: array of string; CaseSensitive: boolean): integer;
begin
  if CaseSensitive then
    for Result := 0 to Length(aList) - 1 do
    begin
      if S = aList[Result] then Exit;
    end
  else
    for Result := 0 to Length(aList) - 1 do
    begin
      if CompareText(S, aList[Result]) = 0 then Exit;
    end;
  Result := -1;
end;

(*function InStrList(const S: string; const AList: TStringList; CaseSensitive: boolean): integer; overload;
begin
  if AList <> nil then
    with AList do
      if CaseSensitive then
        for Result := 0 to Count - 1 do
        begin
          if S = aList[Result] then Exit;
        end
      else
        for Result := 0 to Count - 1 do
        begin
          if CompareText(S, aList[Result]) = 0 then Exit;
        end;
  Result := -1;
end;*)

function InIntList(I: integer; const aList: array of integer): integer; overload;
begin
  for Result := 0 to Length(aList) - 1 do
    if I = aList[Result] then
      Exit;
  Result := -1;
end;

function IntArraysEqual(A, B: TOpenIntegerArray): boolean;
var
  LenA, LenB: integer;
  i: integer;
begin
  Result := False;
  LenA := Length(A);
  LenB := Length(B);
  if LenA <> LenB then Exit;
  for i := 0 to LenA - 1 do
    if A[i]<> B[i] then Exit;
  Result := True;
end;

procedure AllocIntArray(var Ar: PIntArray; Count: integer);
begin
  if Ar = nil then
  begin
    GetMem(Ar, Count * SizeOf(Integer));
    FillChar(Ar^, Count * SizeOf(Integer), 0);
    Exit;
  end;
end;


function StrListToArray(AList: TStringList): TOpenStringArray;
var
  i: integer;
begin
  if AList <> nil then
    with AList do
      begin
        SetLength(Result, Count);
        for i := 0 to Count - 1 do
          Result[i] := AList[i];
      end;
end;

{ True als alle karakters uit S in aCharSet zitten }
function StrAllInSet(const S: string; const aCharSet: TCharSet): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 1 to Length(S) do
    if not (S[i] in aCharSet) then
    begin
      Result := False;  Exit;
    end;
end;

function Transform(const AVarRec: TVarRec): string;
begin
  with AVarRec do
    case VType of
      vtInteger     : Result := IntToStr(VInteger);
      vtBoolean     : Result := BoolStr(VBoolean);
      vtChar        : if VChar = #0 then Result := ' ' else Result := VChar;
      vtExtended    : Result := FloatToStrF(VExtended^, ffFixed, 15, 4);
      vtString      : Result := VString^;
      vtPChar       : Result := VPChar;
      vtObject      :  if VObject = nil then Result := 'NIL' else VObject.ClassName;
      vtClass       : Result := VClass.ClassName;
      vtAnsiString  : Result := string(VAnsiString);
      vtCurrency    : Result := CurrToStr(VCurrency^);
      vtVariant     : Result := string(VVariant^);
      vtInt64       : Result := IntToStr(VInt64^);
      else Result := '';
    end;
    (* UIT DELPHI HELP
        vtInteger:    Result := Result + IntToStr(VInteger);
        vtBoolean:    Result := Result + BoolChars[VBoolean];
        vtChar:       Result := Result + VChar;
        vtExtended:   Result := Result + FloatToStr(VExtended^);

        vtString:     Result := Result + VString^;
        vtPChar:      Result := Result + VPChar;
        vtObject:     Result := Result + VObject.ClassName;
        vtClass:      Result := Result + VClass.ClassName;
        vtAnsiString: Result := Result + string(VAnsiString);
        vtCurrency:   Result := Result + CurrToStr(VCurrency^);
        vtVariant:    Result := Result + string(VVariant^);
        vtInt64:      Result := Result + IntToStr(VInt64^);*)
end;

function StrToFloatDef(const S: string; const DefValue: Extended): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended) then
    Result := DefValue;
end;

function StringToCharSet(const S: string; const Keep: TCharSet = []): TCharSet;
var
  i: Integer;
begin
  Result := [];
  for i := 1 to Length(S) do
    if (Keep = []) or (S[i] in Keep) then
      Include(Result, S[i]);
end;

function UnQuotedStr(const S: string): string;
var
  L: integer;
begin
  Result := S;
  L := Length(S);
  if L < 2 then Exit;
  if (S[1] in ['"', '''']) and (S[L] in ['"', '''']) then
    Result := Copy(S, 2, L - 2);
end;

function VTypeStr(const AType: word): string;
begin
  case AType of
    varEmpty    : Result :=  'Empty';
    varNull     : Result :=  'Null';
    varSmallint : Result :=  'Smallint';
    varInteger  : Result :=  'Integer';
    varSingle   : Result :=  'Single';
    varDouble   : Result :=  'Double';
    varCurrency : Result :=  'Currency';
    varDate     : Result :=  'Date';
    varOleStr   : Result :=  'OleStr';
    varDispatch : Result :=  'Dispatch';
    varError    : Result :=  'Error';
    varBoolean  : Result :=  'Boolean';
    varVariant  : Result :=  'Variant';
    varUnknown  : Result :=  'Unknown';
    varByte     : Result :=  'Byte';
    varStrArg   : Result :=  'StrArg';
    varString   : Result :=  'String';
    varAny      : Result :=  'Any';
    varTypeMask : Result :=  'TypeMask';
    varArray    : Result :=  'Array';
    varByRef    : Result :=  'ByRef';
    else Result := '???'
  end;
end;

function VarTypeStr(const V: Variant): string;
begin
  Result := VTypeStr(TVarData(V).VType);
end;

function SafeFormatStr(const S: string; const Args: array of const): string;
begin
  try
    Result := Format(S, Args);
  except
    Result := S;
  end;
end;

function SetToStr(const aSetTypeInfo: PTypeInfo): string;
begin

end;

function ShiftStateString(Shift: TShiftState): string;
begin
  Result := '';
  if ssShift in Shift then Result := Result + 'ssShift ';
  if ssAlt in Shift then Result := Result + 'ssAlt ';
  if ssCtrl in Shift then Result := Result + 'ssCtrl ';
  if ssLeft in Shift then Result := Result + 'ssLeft ';
  if ssRight in Shift then Result := Result + 'ssRight ';
  if ssMiddle in Shift then Result := Result + 'ssMiddle ';
  if ssDouble in Shift then Result := Result + 'ssDouble ';
  Result := StringReplace(Result, ' ', ',', [rfReplaceAll]);
  if Result <> '' then Result := CutRight(Result, 1); 
end;

function ShortUpperCase(const S: ShortString): ShortString;
var
  i: integer;
begin
  Result[0] := S[0];
  for i := 1 to Length(S) do
    Result[i] := UpCase(S[i]);
end;

function ShortLowerCase(const S: ShortString): ShortString;
var
  i: integer;
begin
  Result[0] := S[0];
  for i := 1 to Length(S) do
    Result[i] := LoCase(S[i]);
end;

function SplitString(const S: string; Index: integer; Seperator: Char; DoTrim: Boolean = True): string;
var
  L: TStringList;
begin
  Result := '';
  L := SplitString_To_StringList(S, Seperator, DoTrim);
  try
    if Index < L.Count then
      Result := L[Index];
  finally
    L.Free;
  end;
end;

function SplitStringCount(const S: string; Seperator: Char): integer;
var
  Start, P: integer;
begin
  //windlg('splitstringcount: kijken of deze functie goed is');
  Result := 0;
  Start := 1;
  repeat
    P := FastCharPos(S, Seperator, Start);
    if P = 0 then Break;
    Inc(Result);
    Start := P + 1;
  until P = 0;
  { evt laatste }
  if (Result > 0) and (Start <= Length(S)) then
    inc(Result);
end;

(*function GetNextToken(const aSeparators: TCharSet; var CharPtr: PChar; var aToken: string): boolean;
var
  P: PChar;
begin
  Result := True;
  P := CharPtr;
  if P^ = #0 then
  begin
    Result := False;
    Exit;
  end;
  if P^ <> #0 then
  begin
    if P^ in aSeparators then
    begin
      Inc(P);
      Inc(CharPtr);
    end;
    while not (P^ in aSeparators) do
      Inc(P);
    SetString(aToken, CharPtr, P - CharPtr);
  end;
  CharPtr := P;
end;*)

//function DoSplit(const S: string)

function SplitString_To_StringList(const S: string; Seperator: Char; DoTrim: boolean = False): TStringList;
var
  Cnt, Start, P: integer;
begin

  (* CODE VERVANGEN DOOR SNELLERE
  H := S;
  Result := TStringList.Create;
  if H = '' then Exit;
  repeat
    P := Pos(Seperator, H);
    if P = 0 then Break;
    Result.Add(Copy(H, 1, P - 1));
    H := Copy(H, P + 1, Length(H)); { copy kapt zelf goed af }
  until False;
  if H <> '' then Result.Add(H);
  *)

  Result := TStringList.Create;
  Start := 1;
  Cnt := 0;
  repeat
    P := FastCharPos(S, Seperator, Start);
    if P = 0 then Break;
    case DoTrim of
      False: Result.Add(Copy(S, Start, P - Start));
      True: Result.Add(Trim(Copy(S, Start, P - Start)));
    end;
    Inc(Cnt);
    Start := P + 1;
  until P = 0;
  { evt laatste }
  if (Cnt = 0) and (P = 0) and (S <> '') then
    case DoTrim of
      False: Result.Add(S);
      True: Result.Add(Trim(S));
    end
  else if (Cnt > 0) and (Start <= Length(S)) then
    case DoTrim of
      False: Result.Add(Copy(S, Start, Length(S) - Start + 1));
      True: Result.Add(Trim(Copy(S, Start, Length(S) - Start + 1)));
    end;
end;

procedure SplitString_To_StringList(const S: string; AList: TStringList; Seperator: Char;
  DoTrim: boolean = False);
var
  Cnt, Start, P: integer;
begin
  if not Assigned(AList) then Exit;
  AList.Clear;
  Start := 1;
  Cnt := 0;
  repeat
    P := FastCharPos(S, Seperator, Start);
    if P = 0 then Break;
    case DoTrim of
      False:
//        if not MakeUpper
         AList.Add(Copy(S, Start, P - Start));
  //      else AList.Add(UpperCase(Copy(S, Start, P - Start)));
      True:
    //    if not MakeUpper
         AList.Add(Trim(Copy(S, Start, P - Start)));
      //  else AList.Add(UpperCase(Trim(Copy(S, Start, P - Start))));
    end;
    Inc(Cnt);
    Start := P + 1;
  until P = 0;
  { evt laatste }
  if (Cnt = 0) and (P = 0) and (S <> '') then
    case DoTrim of
      False:
        //if not MakeUpper
         AList.Add(S);
        //else aList.Add(UpperCase(S));
      True:
        //if not MakeUpper
         AList.Add(Trim(S));
        //else AList.Add(Trim(UpperCase(S)))
    end
  else if (Cnt > 0) and (Start <= Length(S)) then
    case DoTrim of
      False:
        //if not MakeUpper
         AList.Add(Copy(S, Start, Length(S) - Start + 1));
        //else AList.Add(UpperCase(Copy(S, Start, Length(S) - Start + 1)));
      True:
        //if not MakeUpper
         AList.Add(Trim(Copy(S, Start, Length(S) - Start + 1)));
        //else AList.Add(UpperCase(Trim(Copy(S, Start, Length(S) - Start + 1))))
    end;
end;

function SplitString_To_StringArray(const S: string; Seperator: Char; DoTrim: boolean = False): TOpenStringArray; overload;
var
  List: TStringList;
  i: integer;
begin
  List := SplitString_To_StringList(S, Seperator, DoTrim);
  try
    SetLength(Result, List.Count);
    for i := 0 to List.Count - 1 do
      Result[i] := List[i];
  finally
    List.Free;
  end;
end;

function StringArray_To_SplitString(const Ar: array of string; Seperator: Char): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Length(Ar) - 1 do
    Result := Result + Ar[i] + IIF(i < Length(Ar) - 1, Seperator, '');
end;

function StringList_To_SplitString(const AList: TStrings; Seperator: Char): string;
var
  i: integer;
begin
  Result := '';
  if AList = nil then Exit;
  with AList do
    for i := 0 to Count - 1 do
      Result := Result + Strings[i] + Seperator;
end;

function StringArray_To_StringList(const Ar: array of string): TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to Length(Ar) - 1 do
    Result.Add(Ar[i]);
end;

procedure StringArray_To_StringList(const Ar: array of string; List: TStringList);
var
  i: integer;
begin
  if List = nil then
    Exit;
  for i := 0 to Length(Ar) - 1 do
    List.Add(Ar[i]);
end;

function StringArray_To_VariantArray(const Ar: array of string): TOpenVariantArray;
var
  i: integer;
begin
  SetLength(Result, Length(Ar));
  for i := 0 to Length(Ar) - 1 do
    Result[i] := Ar[i];
end;

procedure StringToFile(const aString, aFileName: string);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.Add(aString);
    L.SaveToFile(aFileName);
  finally
    L.Free;
  end;
end;

function SetStringArray(const Ar: array of string): TOpenStringArray;
var
  i: integer;
begin
  SetLength(Result, Length(Ar));
  for i := 0 to Length(Ar) - 1 do Result[i] := Ar[i];
end;

function SetIntegerArray(const Ar: array of integer): TOpenIntegerArray;
var
//  TAr: TOpenIntegerArray absolute Ar;
  i: integer;
begin
  SetLength(Result, Length(Ar));
//  Result := Copy(TAr);
  for i := 0 to Length(Ar) - 1 do Result[i] := Ar[i];
end;

function SetVariantArray(const Ar: array of Variant): TOpenVariantArray;
var
  i: integer;
begin
  SetLength(Result, Length(Ar));
  for i := 0 to Length(Ar) - 1 do Result[i] := Ar[i];
end;

function LoCase(C: char): char;
asm
    CMP    AL, 'A'
    JB     @@Exit
    CMP    AL, 'Z'
    JA @@Exit
    ADD    AL, 'a' - 'A'
  @@Exit:
end;

{function LowerCase(const S: string): string;
var
  i: integer;
begin
  SetLength(Result, Length(S));
  Result := S;
  for i := 1 to Length(S) do
    if S[i] in ['A'..'Z'] then
      Result[i] := Chr(Ord(S[i]) + 32);
end;}

function Between(X, A, B: integer): boolean;
begin
  Result := (X >= A) and (X <= B);
end;

function Between(X, A, B: char): boolean; overload;
begin
  Result := (X >= A) and (X <= B);
end;

function Percentage(Max, N: Double): Double;
begin
  if Max = 0 then
    Result := 0
  else
    Result := (N/Max) * 100;
end;

function Percentage(Max, N: integer): integer;
begin
  if Max = 0 then
    Result := 0
  else
    Result := Trunc((N/Max) * 100);
end;

function Percentage(Max, N: Int64): integer; overload;
begin
  if Max = 0 then
    Result := 0
  else
    Result := Round((N/Max) * 100);
end;

function GetKB(N: integer): integer;
begin
  Result := N div KiloByte;
  if N mod KiloByte > 0 then Inc(Result);
end;

function RandomDouble(const Max: Double): Double;
begin
  Result := Random(Trunc(Max)) + (1/(Random(600) + 1));
  if Random(2) = 1 then Result := -Result;
end;

function Wrap(aValue, aMax: Integer): Integer; overload;
begin
  Result := aValue;
  if Result > aMax then
    Result := 0;
end;

{function Wrap(var aValue: Integer; aMax: Integer): Boolean; overload;
begin
  Result := aValue >= aMax;
  if Result then
    aValue := 0;
end;}

function Between(const X, A, B: TDateTime): boolean; overload;
begin
  Result := (X >= A) and (X <= B);
end;

function Between(const S, A, B: string): boolean; overload;
begin
  Result := (S > A){ and (S <= B)};
end;

function Limit(AValue, AMin, AMax: integer): integer;
begin
  if AMin > AMax then SwapInts(AMin, AMax);
  if AValue < AMin then
    Result := AMin
  else if AValue > AMax then
    Result := AMax
  else
    Result := AValue;
end;

procedure Restrict(var aValue: integer; aMin, aMax: integer);
begin
  if aMin > aMax then SwapInts(aMin, aMax);
  if aValue < aMin then
    aValue := aMin else
  if aValue > aMax then
    aValue := aMax
{  else
    aValue := aMin; }
end;

function RestrictValue(aValue: integer; aMin, aMax: integer): Integer;
begin
  Result := aValue;
  Restrict(Result, aMin, aMax);
//  if aMin > aMax then SwapInts(aMin, aMax);
//  if aValue < aMin then aValue := aMin else if aValue > aMax then aValue := aMax;
end;

function Restricted(var aValue: integer; aMin, aMax: integer): Boolean;
begin
  Result := False;
  if aMin > aMax then SwapInts(aMin, aMax);
  if aValue < aMin then
  begin
    Result := True;
    aValue := aMin
  end
  else if aValue > aMax then begin
    Result := True;
    aValue := aMax;
  end;
end;

{ retourneert aantal milliseconden sinds middernacht }
function MilliSeconds: integer;
var
  SysTime: TSystemTime;
begin
  DateTimeToSystemTime(Now, SysTime);
  with SysTime do
    Result := (wHour * 60 * 60 + wMinute * 60 + wSecond) * 1000 + wMilliSeconds;
end;

const
  DaysPerWeek = 7;
  WeeksPerFortnight = 2;
  MonthsPerYear = 12;
  YearsPerDecade = 10;
  YearsPerCentury = 100;
  YearsPerMillennium = 1000;

  DayMonday = 1;
  DayTuesday = 2;
  DayWednesday = 3;
  DayThursday = 4;
  DayFriday = 5;
  DaySaturday = 6;
  DaySunday = 7;


procedure DivMod(Dividend: Integer; Divisor: Word;
  var Result, Remainder: Word);
asm
        PUSH    EBX
        MOV     EBX,EDX
        MOV     EDX,EAX
        SHR     EDX,16
        DIV     BX
        MOV     EBX,Remainder
        MOV     [ECX],AX
        MOV     [EBX],DX
        POP     EBX
end;

function DecodeDateFully(const DateTime: TDateTime; var Year, Month, Day, DOW: Word): Boolean;
const
  D1 = 365;
  D4 = D1 * 4 + 1;
  D100 = D4 * 25 - 1;
  D400 = D100 * 4 + 1;
var
  Y, M, D, I: Word;
  T: Integer;
  DayTable: PDayTable;
begin
  T := DateTimeToTimeStamp(DateTime).Date;
  if T <= 0 then
  begin
    Year := 0;
    Month := 0;
    Day := 0;
    DOW := 0;
    Result := False;
  end else
  begin
    DOW := T mod 7 + 1;
    Dec(T);
    Y := 1;
    while T >= D400 do
    begin
      Dec(T, D400);
      Inc(Y, 400);
    end;
    DivMod(T, D100, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D100);
    end;
    Inc(Y, I * 100);
    DivMod(D, D4, I, D);
    Inc(Y, I * 4);
    DivMod(D, D1, I, D);
    if I = 4 then
    begin
      Dec(I);
      Inc(D, D1);
    end;
    Inc(Y, I);
    Result := IsLeapYear(Y);
    DayTable := @MonthDays[Result];
    M := 1;
    while True do
    begin
      I := DayTable^[M];
      if D < I then Break;
      Dec(D, I);
      Inc(M);
    end;
    Year := Y;
    Month := M;
    Day := D + 1;
  end;
end;

function DayOfTheWeek(const AValue: TDateTime): Word;
begin
  Result := (DateTimeToTimeStamp(AValue).Date - 1) mod 7 + 1;
end;

const
  CDayMap: array [1..7] of Word = (7, 1, 2, 3, 4, 5, 6);

procedure DecodeDateWeek(const AValue: TDateTime; out AYear, AWeekOfYear,
  ADayOfWeek: Word);
var
  LDayOfYear: Integer;
  LMonth, LDay: Word;
  LStart: TDateTime;
  LStartDayOfWeek, LEndDayOfWeek: Word;
  LLeap: Boolean;
begin
  LLeap := DecodeDateFully(AValue, AYear, LMonth, LDay, ADayOfWeek);
  ADayOfWeek := CDayMap[ADayOfWeek];
  LStart := EncodeDate(AYear, 1, 1);
  LDayOfYear := Trunc(AValue - LStart + 1);
  LStartDayOfWeek := DayOfTheWeek(LStart);
  if LStartDayOfWeek in [DayFriday, DaySaturday, DaySunday] then
    Dec(LDayOfYear, 8 - LStartDayOfWeek)
  else
    Inc(LDayOfYear, LStartDayOfWeek - 1);
  if LDayOfYear <= 0 then
    DecodeDateWeek(LStart - 1, AYear, AWeekOfYear, LDay)
  else
  begin
    AWeekOfYear := LDayOfYear div 7;
    if LDayOfYear mod 7 <> 0 then
      Inc(AWeekOfYear);
    if AWeekOfYear > 52 then
    begin
      LEndDayOfWeek := LStartDayOfWeek;
      if LLeap then
      begin
        if LEndDayOfWeek = DaySunday then
          LEndDayOfWeek := DayMonday
        else
          Inc(LEndDayOfWeek);
      end;
      if LEndDayOfWeek in [DayMonday, DayTuesday, DayWednesday] then
      begin
        Inc(AYear);
        AWeekOfYear := 1;
      end;
    end;
  end;
end;

function WeekOfYear(const aDate: TDateTime): Integer;

{var
  FirstDayOfYear: TDateTime;
  Y, M, D: word;
  DiffDays: integer; }
var
  ResultWeek: Word;
  LYear, LDOW: Word;
begin
{  DecodeDate(aDate, Y, M, D);
  FirstDayOfYear := EncodeDate(Y, 1, 1);
  DiffDays := Trunc(aDate - FirstDayOfYear);
  Result := DiffDays div 7 + 1; }

//  Result := Trunc(ADate - EncodeDate(GetYear(ADate), 1, 1)) div 7 + 1;


  DecodeDateWeek(aDate, LYear, ResultWeek, LDOW);
  Result := ResultWeek;


end;

function IncYears(const ADate: TDateTime; Count: integer): TDateTime;
begin
  Result := IncMonth(ADate, 12 * Count)
end;

function GetYear(const aDate: TDateTime): Word;
var
  M, D: Word;
begin
  DecodeDate(aDate, Result, M, D);
end;

function GetMonth(const aDate: TDateTime): Word;
var
  Y, D: Word;
begin
  DecodeDate(aDate, Y, Result, D);
end;

function GetDay(const aDate: TDateTime): Word;
var
  Y, M: Word;
begin
  DecodeDate(aDate, Y, M, Result);
end;

function DtoS(const aDate: TDateTime): ShortString;
var
  Y, M, D: word;
begin
  DecodeDate(aDate, Y, M, D);
  Result := IntToStr(Y) + LeadZeroStr(M, 2) + LeadZeroStr(D, 2);
end;

function TtoS(const ATime: TDateTime; const aSeparator: string = ''): ShortString;
var
  {Y, M, D, }H, Min, S, MS: word;
begin
//  Result := DtoS(ATime) + IntToStr(MilliSeconds)
  DecodeTime(aTime, H, Min, S, MS);
  Result := //IntToStr(Y) + LeadZeroStr(M, 2) + LeadZeroStr(D, 2)
    DtoS(aTime)
    + aSeparator + LeadZeroStr(H, 2) + LeadZeroStr(Min, 2) + LeadZeroStr(S, 2);

//  DecodeDate(aDate, Y, M, D);
//  Result := IntToStr(Y) + LeadZeroStr(M, 2) + LeadZeroStr(D, 2);
end;

function StrToDateDef(const S, ShortFormat: string; const ErrorDate: TDateTime = 0): TDateTime;
var
  Old: string;
begin
  Old := ShortDateFormat;
  ShortDateFormat := ShortFormat;
  try
    try
      Result := StrToDate(S)
    except
      Result := ErrorDate;
    end;
  finally
    ShortDateFormat := Old;
  end;
end;

function GetEncodeDate(Y, M, D: word): TDateTime;
begin
  //WinDlg('GetEncodeDate is niet geimplementeerd!');
  Result := 0;
//  Result := EncodeDate(Result, Y, M, D);
end;

function StoD(const aDateStr: ShortString; const Def: TDateTime = 0): TDateTime;
var
  //E: Integer;
  Y, M, D: word;
begin
  try
    //Val(S, Result, E);
    Y := StrToInt(Copy(aDateStr, 1, 4));
    M := StrToInt(Copy(aDateStr, 5, 2));
    D := StrToInt(Copy(aDateStr, 7, 2));
    Result := EncodeDate(Y, M, D);
  except
    Result := Def;
    Exit;
  end;
end;

function UserStrToDate(const DateStr: string): TDateTime;
var
  S: string;
  P, Y, M, D: integer;
begin
  { we zouden hier ook wel een delphi strtodate met format of zoiets kunnen gebruiken }
//  Result := 0;
//  try
    S := StringReplace(DateStr, ' ', '', [rfReplaceAll]);
    { luxe }
    if Length(S) = 1 then
      case UpCase(S[1]) of
        'V': Result := Date();
        'G': Result := Date() - 1;
        'M': Result := Date() + 1;
        else raise EConvertError.Create('Foute datum invoer');
      end
    else
    begin
      { probeer dag te pakken }
      P := Pos('-', S);
      if not Between(P, 2, 3) then raise EConvertError.Create('Foute datum invoer');
      D := StrToInt(Copy(S, 1, P - 1)); { StrToInt raised exception bij fout }
      { probeer maand te pakken }
      S := CutLeft(S, P);
      P := Pos('-', S);
      if not Between(P, 2, 3) then raise EConvertError.Create('Foute datum invoer');
      M := StrToInt(Copy(S, 1, P - 1)); { StrToInt raised exception bij fout }
      { probeer jaar te pakken }
      S := CutLeft(S, P);
      if Length(S) <> 4 then raise EConvertError.Create('Foute datum invoer');
      Y := StrToInt(Copy(S, 1, 4));
      Result := EncodeDate(Y, M, D);
    end;
//  except
    //raise// EConvertError.Create('Foute datum invoer');
//  end;
//  Result := ToDay;
end;


function DaysInMonth(AYear, AMonth: Integer): Integer;
const
  DaysInMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result); { leap-year Feb is special }
end;

function FirstOfQuarter(const D: TDateTime): TDateTime;
var
  Y, M, Day: word;
begin
  DecodeDate(D, Y, M, Day);
  case M of
    1,2,3    : M := 1;
    4,5,6    : M := 2;
    7,8,9    : M := 3;
    10,11,12 : M := 4;
  end;
  Result := EncodeDate(Y, M, 1);
end;

function FirstOfHalfYear(const D: TDateTime): TDateTime;
var
  Y, M, Day: word;
begin
  DecodeDate(D, Y, M, Day);
  case M of
    1..6     : M := 1;
    7..12    : M := 7;
  end;
  Result := EncodeDate(Y, M, 1);
end;

function FirstOfMonth(const D: TDateTime): TDateTime;
var
  Y, M, Day: word;
begin
  DecodeDate(D, Y, M, Day);
  Result := EncodeDate(Y, M, 1);
end;

procedure Swap(var A, B; Size: integer);
var
  Temp: pointer;
begin
  GetMem(Temp, Size);
  Move(A, Temp^, Size);
  Move(B, A, Size);
  Move(Temp^, B, Size);
  FreeMem(Temp, Size);
end;

function SwapWord(W: Word): Word;
begin
  Result := System.Swap(W);
end;

procedure FillDWord(var Dest; Count, Value: Integer); register;
asm
  XCHG  EDX, ECX
  PUSH  EDI
  MOV   EDI, EAX
  MOV   EAX, EDX
  REP   STOSD
  POP   EDI
end;

procedure FastMove(const Source; var Dest; Count: Integer);
asm
      cmp eax, edx
      je  @exit

      push esi
      push edi

      mov esi, eax
      mov edi, edx
      shr ecx, 2
      jz  @MoveRest

   @LongLoop:
      mov eax, [esi]
      mov [edi], eax
      dec ecx
      jz  @MoveRest4
      mov eax, [esi + 4]
      mov [edi + 4], eax
      add esi, 8
      add edi, 8
      dec ecx
      jne @LongLoop
      jmp @MoveRest

   @MoveRest4:
      add esi, 4
      add edi, 4

   @moveRest:
      mov ecx, Count
      and ecx, 3
      rep movsb

      pop edi
      pop esi
   @exit:
end;

function MemEmpty(var A; Size: integer): boolean; // kan geoptimaliseerd of misschien zit ie standaard in delphi
var
  B: pByte;
  i: integer;
begin
  Result := False;
  B := @A;
  for i := 0 to Size - 1 do
  begin
    if B^ <> 0 then
      Exit;
    Inc(B);
  end;
  Result := True;
end;

function MemAllocated: Cardinal;
begin
  Result := GetHeapStatus.TotalAllocated;
end;

procedure SetPointerListSize(var PointerList: TPointerList; ASize: integer);
begin
//  ReallocMem(@PointerList, ASize * SizeOf(Pointer));
end;

function EqualRects(const R1, R2: TRect): boolean;
begin
  Result := CompareMem(@R1, @R2, SizeOf(TRect));
end;

function EqualPoints(const P1, P2: TPoint): boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);//CompareMem(@P1, @P2, SizeOf(TPoint));
end;

procedure SwapBytes(var A, B: byte);
var
  C: byte;
begin
  C := A;  A := B;  B := C;
end;

procedure SwapShortInts(var A, B: ShortInt);
var
  C: ShortInt;
begin
  C := A;  A := B;  B := C;
end;

procedure SwapChars(var A, B: char); 
var
  C: char;
begin
  C := A;  A := B;  B := C;
end;

procedure SwapWords(var A, B: word);
begin
  a := a xor b;
  b := b xor a;
  a := a xor b;
end;

procedure SwapInts(var A, B: integer);
//var
  //C: integer;
begin
  a := a xor b;
  b := b xor a;
  a := a xor b;
//  C := A;  A := B;  B := C;
end;

procedure SwapStrings(var A, B: string);
var
  C: string;
begin
  C := A;  A := B;  B := C;
end;

function CompareFiles(const FileName1,FileName2: string; out DiffPos: Integer): boolean;
var
  F1, F2: File;
  P1, P2: pointer;
  Read1, Read2: integer;
  P: integer;
  i: Integer;
type
  PBytes = ^TBytes;
  TBytes = array[0..1023] of Byte;
begin
  DiffPos := -1;
  { probeer te openen }
  try AssignFile(F1, FileName1);  Reset(F1, 1);
  except raise;
  end;
  try AssignFile(F2, FileName2);  Reset(F2, 1);
  except raise;
  end;
  { mem }
  GetMem(P1, 1024);  GetMem(P2, 1024);
  { vergelijk }
  P := 0;
  Result := True;
  if FileSize(F1) = FileSize(F2) then
    while Result and not Eof(F1) and not Eof(F2) do
    begin
      BlockRead(F1, P1^, 1024, Read1);
      BlockRead(F2, P2^, 1024, Read2);
      Result := (Read1 = Read2) and CompareMem(P1, P2, Read1);
      if not Result then
        if Read1 = Read2 then
        begin
          for i := 0 to Read1 - 1 do
            if TBytes(P1^)[i] <> TBytes(P2^)[i] then
            begin
              DiffPos := P + i;
              Break;
            end;
        end;
//      if not Result then WinDlg('verschil op positie ' + i2s(P));
      Inc(P, 1024);
    end
  else Result := False;
  CloseFile(F1);  CloseFile(F2);
  FreeMem(P1, 1024);  FreeMem(P2, 1024);
end;

function TestBit8(B, Bit: Byte): Boolean;
begin
  Result := B and (1 shl Bit) <> 0;
end;

procedure SetBit8(var B: byte; Bit: byte);
begin
  B := B or (1 shl Bit);
end;

procedure ClearBit8(var B: byte; Bit: byte);
begin
  B := B and not (1 shl Bit);
end;

function TestBit64(const B: Int64; Bit: Byte): Boolean;
begin
  Result := B and (Int64(1) shl Bit) <> 0;
end;

procedure SetBit64(var B: Int64; Bit: Byte);
begin
  B := B or (Int64(1) shl Bit);
end;

procedure ClearBit64(var B: Int64; Bit: Byte);
begin
  B := B and not (Int64(1) shl Bit);
end;

function Test_UL_Bit(UL: LongWord; Bit: Byte): Boolean; //unsigned long
begin
  Result := UL and (1 shl Bit) <> 0;
end;

{ intern }
procedure QuickSortStr (var v_arr : string; p_left, p_right : word);
var
  min : word; max : word; mid : char; temp : char;
begin

  min:=p_left; max:=p_right; mid:=v_arr[(p_left+p_right) div 2];

  repeat
  while v_arr[min]<mid do inc(min); while v_arr[max]>mid do dec(max);
  if min<=max then
    begin
    temp:=v_arr[min]; v_arr[min]:=v_arr[max]; v_arr[max]:=temp; inc(min); dec(max);
    end;
  until min>max;

  if p_left<max then quicksortstr(v_arr,p_left,max);
  if min<p_right then quicksortstr(v_arr,min,p_right);

end;

function SortStr (const S : string) : string;  { retourneert op alfabet gesorteerde string }
begin
  Result := S;
  if Length(S) > 1 then
    QuickSortStr(Result, 1, length (S)); 
end;

(* RANGE CHECK ERROR
function SortStr(const S: string): string;

    procedure QuickSortStr (L, R : integer);
    var
      min,  max : integer;
      mid : char;
    begin
      min := L; max := R; mid:= Result[(L + R) div 2];
      repeat
      while S[min] < mid do inc(min);
      while S[max] > mid do dec(max);
      if min <= max then
        begin
        SwapChars(Result[Min], Result[Max]);
        Inc(min);
        Dec(max);
        end;
      until min > max;
      if L < max then QuickSortStr(L, max);
      if min<R then QuickSortStr(min, R);
    end;

begin
  Result := S;
  QuickSortStr(1, Length (Result));
end; *)

procedure SortStrings(var Strings: array of string; CaseSensitive: boolean = True);

    procedure DoSortCaseSensitive(First, Last: integer);
    var
      Min, Max: integer;
      Mid: string;
    begin
      Min := First;
      Max := Last;
      Mid := Strings[(Min + Max) shr 1];
      repeat
      while Strings[Min] < Mid do Inc(Min);
      while Strings[Max] > Mid do Dec(Max);
      if Min <= Max then
        begin
        SwapStrings(Strings[Min], Strings[Max]);
        inc(Min); dec(Max);
        end;
      until Min > Max;
      if First < Max then DoSortCaseSensitive(First, Max);
      if Min < Last then DoSortCaseSensitive(Min, Last);
    end;

    procedure DoSortCaseInsensitive(First, Last: integer);
    var
      Min, Max: integer;
      Mid, Temp: string;
    begin
      Min := First;
      Max := Last;
      Mid := Strings[(Min + Max) shr 1];
      repeat
      while CompareText(Strings[Min], Mid) < 0 do Inc(Min);
      while CompareText(Strings[Max], Mid) > 0 do Dec(Max);
      if Min <= Max then
        begin
        Temp := Strings[Min]; Strings[Min] := Strings[Max]; Strings[Max] := Temp;
        inc(Min); dec(Max);
        end;
      until Min > Max;
      if First < Max then DoSortCaseInsensitive(First, Max);
      if Min < Last then DoSortCaseInsensitive(Min, Last);
    end;

begin
  if Length(Strings) > 1 then
    case CaseSensitive of
      True:
        DoSortCaseSensitive(0, Length(Strings) - 1);
      False:
        DoSortCaseInsensitive(0, Length(Strings) - 1);
    end;
end;

function IIF(aExpr: boolean; const aTrue, aFalse: pointer): pointer; overload;
begin
  if aExpr then Result := aTrue else Result := aFalse;
end;

function IIF(aExpr: boolean; const aTrue, aFalse: integer): integer; overload;
begin
  if aExpr then Result := aTrue else Result := aFalse;
end;

function IIF(aExpr, aTrue, aFalse: boolean): boolean; overload;
begin
  if aExpr then Result := aTrue else Result := aFalse;
end;

function IIF(aExpr: boolean; const aTrue, aFalse: string): string; overload;
begin
  if aExpr then Result := aTrue else Result := aFalse;
end;

function IIFSTRING(aExpr: boolean; const aTrue, aFalse: string): string;
begin
  if aExpr then Result := aTrue else Result := aFalse;
end;

function IIF(aExpr: boolean; const aTrue, aFalse: Char): Char; overload;
begin
  if aExpr then Result := aTrue else Result := aFalse;
end;
(*asm
  { al = aExpr, dl = aTrue, cl = aFalse }
    cmp al, False
    je @False
    mov al, dl
    jmp @Exit
  @False:
    mov al, cl
  @Exit:
end;*)

function IIF(aExpr: boolean; const aTrue, aFalse: Currency): Currency; overload;
begin
  if aExpr then Result := aTrue else Result := aFalse;
end;

procedure RectGrow(var aRect: TRect; DeltaX, DeltaY: integer);
begin
  with aRect do
  begin
    Inc(Right, DeltaX); Dec(Left, DeltaX);
    Inc(Bottom, DeltaY);  Dec(Top, DeltaY);
  end;
end;

function RectHeight(const aRect: TRect): integer;
begin
  with aRect do Result := Bottom - Top + 1;
end;

function RectWidth(const aRect: TRect): integer;
begin
  with aRect do Result := Right - Left + 1;
end;

procedure RectMove(var R: TRect; X, Y: integer);
begin
  with R do
  begin
    if X <> 0 then
    begin
      Inc(R.Left, X);
      Inc(R.Right, X);
    end;
    if Y <> 0 then
    begin
      Inc(R.Top, Y);
      Inc(R.Bottom, Y);
    end;
  end;
end;

procedure RectAssign(var ARect: TRect; X, Y, W, H: integer);
begin
  with ARect do
  begin
    Left := X;
    Top := Y;
    Right := X + W;
    Bottom := Y + H;
  end;
end;

function ZeroTopLeftRect(const aRect: TRect): TRect;
begin
  Result := aRect;
  with Result do
  begin
    Dec(Right, Left);
    Dec(Bottom, Top);
    Left := 0;
    Top := 0;
  end;
end;

{function CenterRect(const Source, Dest: TRect): TRect;
begin
  Result := Source;
  Result.Left := Dest.Left + (Source.Right - Source.Left) div 2;
  Result.Top := Dest.Top + (Source.Bottom - Source.Top) div 2;
  Result.Right := Result.Left + (Source.Right - Source.Left);
  Result.Bottom := Result.Top + (Source.Bottom - Source.Top); 
end;        }

function CenterRect(const Child, Parent: TRect): TRect;
begin
  Result.Left   := (RectWidth(Parent) - RectWidth(Child)) div 2;
  Result.Top    := (RectHeight(Parent) - RectHeight(Child)) div 2;
  Result.Right  := Result.Left + RectWidth(Child);
  Result.Bottom := Result.Top + RectHeight(Child);

//  Result := Child;                                      extctrls
{  Result.Left := Child.Left + (Parent.Right - Parent.Left) div 2;
  Result.Top := Dest.Top + (Source.Bottom - Source.Top) div 2;
  Result.Right := Result.Left + (Source.Right - Source.Left);
  Result.Bottom := Result.Top + (Source.Bottom - Source.Top); }



//    Result := Bounds((Width - Picture.Width) div 2, (Height - Picture.Height) div 2,
  //    Picture.Width, Picture.Height)


end;

(*function ClientRectToScreenRect(const aControl: TControl): TRect;
begin
  with aControl do
  begin
    Result := ClientRect;
    Result.TopLeft := ClientToScreen(Result.TopLeft);
    Result.BottomRight := ClientToScreen(Result.BottomRight);
  end;
end;*)

{ nooit veranderen, nooit weghalen }
const
  ECode: ShortString =
    '8Á²ë¢¸Ýp(ÓÄ×CÌË÷áCªøòý°y…†Ç³‹£ìÉ¾ÊèRl„*¡YH“Y‘¼ç?¶ä¼„Ö¯'; // xorstring voor encryptie

procedure EncryptMem(var Mem; ASize: integer);
var
  p: PByte;
  i: integer;
  b: byte;
  m: byte;
  c: integer;
begin
  P := @Mem;  Inc(P, ASize - 1);
  for i := ASize - 1 downto 0 do
  begin
    b := P^;
    m := i mod 64;
    b := b xor m;
    P^ := b;
    Dec(P);
  end;
  P := @Mem;
  c := 1;
  for i := 0 to ASize - 1 do
  begin
    P^ := P^ xor Ord(ECode[c]);
    if c < Length(ECode) then Inc(c) else c := 1;
  end;
end;

procedure DecryptMem(var Mem; ASize: integer);
begin
  EncryptMem(Mem, ASize); { 2x encrypten = decrypten }
end;

function EncryptStr(const S: string): string;
var
  i: integer;
  b: byte;
  m: byte;
  c: integer;
begin
  SetLength(Result, Length(S));
  for i := Length(S) downto 1 do
  begin
    b := Ord(S[i]);
    m := i mod 64;
    b := b xor m;
    Result[i] := Chr(b);
  end;
  c := 1;
  for i := 1 to Length(S) do
  begin
    Result[i] := Chr(Ord(S[i]) xor Ord(ECode[c]));
    if c < Length(ECode) then inc(c) else c := 1;
  end;
end;

function DecryptStr(const S: string): string;
begin
  Result := EncryptStr(S); { 2x encrypten = decrypten }
end;

{ kan 32 bits geoptimaliseerd en ASM }
function CheckSum(var Mem; ASize: integer): LongWord;
var
  P: PByte;
  i: integer;
  Shift: integer;
  CRC: LongWord;
  PECode: ^LongWord;
begin
  { standaard startwaarde genereren, is gelijk aan de eerste 4 letters van ECode }
  PECode := @ECode[1];
  Result := PECode^;
  P := @Mem;
  for i := 0 to ASize - 1 do
  begin
    Shift := (i mod 4) * 8;  { bepaal shiftleftfactor }
    CRC := P^;  { LongWord CRC = byte P^ }
    CRC := CRC shl Shift;  { En schuif de bytes op voor goede xor }
    Result := Result xor CRC;  { en xor }
    Inc(P);  { volgende byte }
  end;
end;

function CheckSum(aStream: TStream): LongWord;
var
  P: Byte;
  i: integer;
  Shift: integer;
  CRC: LongWord;
  PECode: ^LongWord;
begin
  aStream.Position := 0;
  { standaard startwaarde genereren, is gelijk aan de eerste 4 letters van ECode }
  PECode := @ECode[1];
  Result := PECode^;
//  P := @Mem;
  with aStream do
  for i := 0 to Size - 1 do
  begin
    Read(P, 1);
    Shift := (i mod 4) * 8;  { bepaal shiftleftfactor }
    CRC := P;  { LongWord CRC = byte P^ }
    CRC := CRC shl Shift;  { En schuif de bytes op voor goede xor }
    Result := Result xor CRC;  { en xor }
//    Inc(P);  { volgende byte }
  end;
end;

function KeyboardStateToShiftState(const KeyboardState: TKeyboardState): TShiftState;
begin
  Result := [];
  if KeyboardState[VK_SHIFT] and $80 <> 0 then Include(Result, ssShift);
  if KeyboardState[VK_CONTROL] and $80 <> 0 then Include(Result, ssCtrl);
  if KeyboardState[VK_MENU] and $80 <> 0 then Include(Result, ssAlt);
  if KeyboardState[VK_LBUTTON] and $80 <> 0 then Include(Result, ssLeft);
  if KeyboardState[VK_RBUTTON] and $80 <> 0 then Include(Result, ssRight);
  if KeyboardState[VK_MBUTTON] and $80 <> 0 then Include(Result, ssMiddle);
end;

function GetKeyBoardShiftState: TShiftState;
var
  KeyState : TKeyBoardState;
begin
  GetKeyboardState(KeyState);
  Result := KeyboardStateToShiftState(KeyState);
end;


(*
*)


{ Component Streaming }

function ComponentToString(Component: TComponent): string;

var
  BinStream: TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(s);
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result:= StrStream.DataString;
    finally
      StrStream.Free;

    end;
  finally
    BinStream.Free
  end;
end;

procedure StringToComponent(Value: string; C: TComponent);
var
  StrStream: TStringStream;
  BinStream: TMemoryStream;
begin
  StrStream := TStringStream.Create(Value);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      BinStream.ReadComponent(C);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

procedure ComponentToFile(C: TComponent; const AFileName: string);
var
  Stream : TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Stream.WriteComponent(C);
  finally
    Stream.Free;
  end;
end;

procedure FileToComponent(const AFileName: string; C: TComponent);
var
  Stream : TFileStream;
  //i: integer;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead);
  try
    Stream.ReadComponent(C);
  finally
    Stream.Free;
  end;
end;

procedure ComponentToTextFile(C: TComponent; const AFileName: string);
var
  S: string;
  P: PChar;
  Stream: TFileStream;
begin
  S := ComponentToString(C);
  P := PChar(S);
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Stream.Write(P^, Length(S));
  finally
    Stream.Free;
  end;
end;

(*procedure ComponentToTextFile(C: TComponent; const AFileName: string);
var
  StrStream: TFileStream;
  BinStream: TMemoryStream;
  aFormat: TStreamOriginalFormat;
begin
  StrStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    BinStream := TMemoryStream.Create;
    try
      BinStream.ReadComponent(C);
      ObjectBinaryToText(BinStream, StrStream, aFormat);
      StrStream.Seek(0, soFromBeginning);
      StrStream.WriteComponent(C);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;*)

procedure TextFileToComponent(const aFileName: string; C: TComponent);
var
  StrStream: TFileStream;
  BinStream: TMemoryStream;
  aFormat: TStreamOriginalFormat;
begin
  StrStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream, aFormat);
      BinStream.Seek(0, soFromBeginning);
      BinStream.ReadComponent(C);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

procedure ForEachComponent(aComponent: TComponent; Method: TComponentMethod;
  SubComponents: boolean; const Filter: array of TComponentClass;
  IncludeMain: Boolean = False);

    function FilterOk(C: TComponent): Boolean;
    var
      i: Integer;
    begin
      Result := False;
      if Length(Filter) = 0 then
      begin
        Result := True;
        Exit;
      end;
      for i := Low(Filter) to High(Filter) do
        if C is Filter[i] then
        begin
          Result := True;
          Exit;
        end;
    end;

    procedure DoScan(Com: TComponent);
    var
      i: integer;
      C: TComponent;
    begin
      for i := 0 to Com.ComponentCount - 1 do
      begin
        C := Com.Components[i];
        if FilterOK(C) then
          Method(C);
        if SubComponents then
          DoScan(C);
      end;
    end;

begin
  if IncludeMain then
    if FilterOK(aComponent) then
      Method(aComponent);
  DoScan(aComponent);
end;

function UniqueComponentName(const aBaseName: string; aOwner: TComponent): string;
var
   ix: Integer;
  //N: string;
begin
  ix := 0;
  if aOwner <> nil then
    with aOwner do
      while FindComponent(aBaseName + i2s(ix)) <> nil do
        Inc(ix);
  if ix = 0 then
    Result := aBaseName
  else
    Result := aBaseName + i2s(ix);
end;

{ Exception handling }

procedure AppError(const Msg: string; Sender: TObject = nil);
var
  ErrorStr: string;
begin
  if Sender <> nil then
  begin
    ErrorStr := 'Fout in ' + Sender.ClassName;
    ErrorStr := ErrorStr + ': ' + Chr(13) + Msg;
  end
  else ErrorStr := Msg;
  raise EAppError.Create(ErrorStr);
end;

procedure AppErrorFmt(const Msg: string;  const Args: array of const; Sender: TObject = nil);
var
  S: string;
begin
  try S := Format(Msg, Args) except S := Msg; end;
  AppError(S, Sender);
end;

{ -------- sysutilscode maar dan nederlands -------- }

{ Convert physical address to logical address }

function ConvertAddr(Address: Pointer): Pointer; assembler;
asm
        TEST    EAX,EAX         { Always convert nil to nil }
        JE      @@1
        SUB     EAX, $1000      { offset from code start; code start set by linker to $1000 }
@@1:
end;

{ Format and return an exception error message }

function ExceptionErrorMessage(ExceptObject: TObject; ExceptAddr: Pointer;
  Buffer: PChar; Size: Integer): Integer;
var
  MsgPtr: PChar;
  MsgEnd: PChar;
  MsgLen: Integer;
  ModuleName: array[0..MAX_PATH] of Char;
  Temp: array[0..MAX_PATH] of Char;
  Format: array[0..255] of Char;
  Info: TMemoryBasicInformation;
  ConvertedAddress: Pointer;
begin
  // to do: vertaling van exceptionnamen?
  // vertaling van messages?
  VirtualQuery(ExceptAddr, Info, sizeof(Info));
  if (Info.State <> MEM_COMMIT) or
    (GetModuleFilename(THandle(Info.AllocationBase), Temp, SizeOf(Temp)) = 0) then
  begin
    GetModuleFileName(HInstance, Temp, SizeOf(Temp));
    ConvertedAddress := ConvertAddr(ExceptAddr);
  end
  else
    Integer(ConvertedAddress) := Integer(ExceptAddr) - Integer(Info.AllocationBase);
  StrLCopy(ModuleName, AnsiStrRScan(Temp, '\') + 1, SizeOf(ModuleName) - 1);
  MsgPtr := '';
  MsgEnd := '';
  if ExceptObject is Exception then
  begin
    MsgPtr := PChar(Exception(ExceptObject).Message);
    MsgLen := StrLen(MsgPtr);
    if (MsgLen <> 0) and (MsgPtr[MsgLen - 1] <> '.') then MsgEnd := '.';
  end;
  LoadString(FindResourceHInstance(HInstance),
    PResStringRec(@SMyException).Identifier, Format, SizeOf(Format));
  StrLFmt(Buffer, Size, Format, [ExceptObject.ClassName, ModuleName,
    ConvertedAddress, MsgPtr, MsgEnd]);
  Result := StrLen(Buffer);
end;

{ Display exception message box }

procedure ShowError(ExceptObject: TObject; ExceptAddr: Pointer);
var
  Title: array[0..63] of Char;
  Buffer: array[0..1023] of Char;
begin
  ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer, SizeOf(Buffer));
  if IsConsole then
    WriteLn(Buffer)
  else
  begin
    LoadString(FindResourceHInstance(HInstance), PResStringRec(@SMyExceptTitle).Identifier,
      Title, SizeOf(Title));
    MessageBox(0, Buffer, Title, MB_OK or MB_ICONEXCLAMATION or MB_TASKMODAL);
  end;
end;

procedure WinDlg(const S: string);
begin
//  P := PChar(S);
//  showmessage(S);
  MessageBox(0, PChar(S), PChar('ok'), MB_OK + {MB_APPLMODAL}MB_TASKMODAL);
end;

procedure WinDlg(const AValues: array of const);

    function SetStr: string;
    var
      i: integer;
    begin
      Result := '';
      for i := Low (AValues) to High (AValues) do
      begin
        try
          Result := Result + '[' + TransForm(AValues[i]) + ']' + Chr(13);
        except
          Result := Result + '[¿NULL¿]' + Chr(13);
        end;
      end;
      Result := FastReplace(Result, '#0', ' '{, [rfReplaceAll]});
    end;

var
  S: string;
begin
//  P := PChar(S);
//  showmessage(S);

  S := SetStr;

  MessageBox(0, PChar(S), PChar('ok'), MB_OK + MB_TASKMODAL);
end;

function GetHardDiskSerialNumber: DWORD;
var
  MaximumComponentLength, FileSystemFlags: Cardinal;
begin
  if not GetVolumeInformation(nil, nil, 0, @Result, MaximumComponentLength, FileSystemFlags, nil, 0) then
    Result := 0;
end;

type
  PTransBuffer = ^TTransBuffer;
  TTransBuffer = array[1..13] of smallint;

const
  CInfoStr : array[1..13] of string =
    ('FileVersion',
     'CompanyName',
     'FileDescription',
     'InternalName',
     'LegalCopyright',
     'LegalTradeMarks',
     'OriginalFileName',
     'ProductName',
     'ProductVersion',
     'Comments',
     'CurrentProgramVersion',
     'CurrentDatabaseVersion',
     'VersionDetails');

procedure GetProjectVersionInfo(AVersionList: TStrings; AFileName: string = '');
{
 This procedure returns ALL of the version information as separate
 string entries of a TString list. Each element can then be accessed
 by indexing the TString list thus: AVersionList[0], AVersionList[1] etc..
}
var
  I: Integer;
  InfoSize: DWORD;
  pTrans: PTransBuffer;
  TransStr: string;
  TypeStr: string;
  Value: PChar;
  VerBuf: pointer;
  VerSize: DWORD;
  Wnd: DWORD;
begin
  AVersionList.Clear;
  if AFileName = '' then
    AFileName := ParamStr(0);
  InfoSize := GetFileVersioninfoSize(PChar(AFileName), Wnd);

  if (InfoSize <> 0) then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(AFileName), Wnd, InfoSize, VerBuf) then
      begin
        VerQueryValue(VerBuf, PChar('\VarFileInfo\Translation'),
                      Pointer(pTrans), VerSize);

        TransStr := IntToHex(pTrans^[1], 4) + IntToHex(pTrans^[2], 4);

        for i := Low(CInfoStr) to High(CInfoStr) do
        begin
          TypeStr := 'StringFileInfo\' + TransStr + '\' + CInfoStr[I];

          if VerQueryvalue(VerBuf, PChar(TypeStr),
                           Pointer(Value), VerSize) then
            AVersionList.Add(CInfoStr[I] + '=' + Value);
        end
      end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

function GetBuildInfo(var V1, V2, V3, V4: Word; AFileName: string = ''): Boolean;
{
 This procedure returns the individual Major/Minor/Release/Build
 values of the version information.
}
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  Result := True;
  if AFileName = '' then
    AFileName := ParamStr(0);
  VerInfoSize := GetFileVersionInfoSize(PChar(AFileName), Dummy);
  if VerInfoSize = 0 then
  begin
    Result := False;
    Exit;
  end;
  GetMem(VerInfo, VerInfoSize);
  try
    GetFileVersionInfo(PChar(AFileName), 0, VerInfoSize, VerInfo);
    VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);

    with VerValue^ do
    begin
      V1 := dwFileVersionMS shr 16;
      V2 := dwFileVersionMS and $FFFF;
      V3 := dwFileVersionLS shr 16;
      V4 := dwFileVersionLS and $FFFF;
    end;
  finally
    FreeMem(VerInfo, VerInfoSize);
  end;
end;

function GetBuildInfoAsString(AFileName: string = ''): string;
var
  V1: Word;
  V2: Word;
  V3: Word;
  V4: Word;
begin
  if GetBuildInfo(V1, V2, V3, V4) then
    Result := Format('%d.%d.%d.%d', [V1, V2, V3, V4])
  else
    Result := '';
end;


function GetApplicationPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function GetApplicationName: string;
begin
  Result := ParamStr(0);
end;

end.


