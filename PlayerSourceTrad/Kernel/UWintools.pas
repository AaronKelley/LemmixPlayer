unit UWinTools;

interface

uses
  Classes, Forms, Windows, Graphics, ShellApi, SysUtils, Controls, StdCtrls,
  Registry, Messages,
  UMisc;

type
  EShellExecuteError = class(Exception);

  TOperationParam = (opNone, opOpen, opPrint, opExplore);

procedure SelectBackwards(Edit: TEdit; SelStart, SelLength: Integer);

procedure SetTransparentForm(AHandle: THandle; AValue: byte = 0);
    { maakt een form transparant }
procedure SetDisplayDragImage(Parent: TControl);
    { zorgt ervoor dat image altijd op alle controls en subcontrols weergegeven wordt }
function GetBestPopupPosition(const aCenterRect: TRect; aWidth, aHeight: integer): TPoint;
    { berekent de beste popup positie }
function DoShellExecute(const FileName: string; const Parameters: string = '';
  const Directory: string = ''; Operation: TOperationParam = opOpen; ShowCmd: Integer = SW_SHOWNORMAL): HINST;
    { wrapper rond ShellExecute }
function MailTo(const Address: string): HINST;
    { shellexecute wrapper url mailto }
function ForceForegroundWindow(hwnd: THandle): Boolean;
    { forceert een app op de voorgrond, gebruik Application.Handle }
procedure AssociateFileExtension(const IconPath, ProgramName, Path, Extension: string);

implementation

const
 WS_EX_LAYERED = $80000;
 LWA_COLORKEY = 1;
 LWA_ALPHA    = 2;

type
 TSetLayeredWindowAttributes = function (
     hwnd : HWND;         // handle to the layered window
     crKey : TColor;      // specifies the color key
     bAlpha : byte;       // value for the blend function
     dwFlags : DWORD      // action
     ): BOOL; stdcall;

var
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes;

procedure SelectBackwards(Edit: TEdit; SelStart, SelLength: Integer);
var KS: TKeyboardState;
    OrgShift: Byte;
    I: Integer;
begin
  Edit.SelStart := SelStart;
  GetKeyboardState(KS);
  OrgShift := KS[VK_SHIFT];
  KS[VK_SHIFT] := $80;
  SetKeyboardState(KS);
  for I := SelStart downto SelStart - SelLength + 1 do
    Edit.Perform(WM_KEYDOWN, VK_LEFT, 0);
  Edit.Perform(WM_KEYUP, VK_LEFT, 0);
  KS[VK_SHIFT] := OrgShift;
  SetKeyboardState(KS);
end;

procedure SetTransparentForm(AHandle : THandle; AValue : byte = 0);
begin
  if Assigned(SetLayeredWindowAttributes) then
  begin
    SetWindowLong(AHandle, GWL_EXSTYLE, GetWindowLong(AHandle, GWL_EXSTYLE) or WS_EX_LAYERED);
    //Make form transparent
    SetLayeredWindowAttributes(AHandle, 0, AValue, LWA_ALPHA);
  end;
end;

procedure SetDisplayDragImage(Parent: TControl);
var
  I: Integer;
begin
  Parent.ControlStyle := Parent.ControlStyle + [csDisplayDragImage];
  if Parent is TWinControl then
    with TWinControl(Parent) do
      for I := 0 to ControlCount - 1 do
        SetDisplayDragImage(Controls[I]);
end;

function GetBestPopupPosition(const aCenterRect: TRect; aWidth, aHeight: integer): TPoint;
var
  C: TRect;
  P: TPoint;
  MaxRight, MaxBottom: integer;
  AppBar: TAppBarData;
  AppBarHeight: integer;
//  res:integer;
begin

  { haal hoogte taskbar op }
  FillChar(AppBar, SizeOf(AppBar), 0);
  SHAppBarMessage(ABM_GETTASKBARPOS, AppBar);
  begin
    AppBarHeight := AppBar.Rc.Bottom - AppBar.Rc.Top;
    if AppBarHeight > 300 then AppBarHeight := 0;
  end;

  if CompareMem(@aCenterRect, @EmptyRect, SizeOf(TRect)) then
  begin
    Result.X := Screen.Width div 2 - aWidth div 2;
    Result.Y := (Screen.Height - 1 - AppBarHeight) div 2 - aHeight div 2;
    Exit;
  end;

  MaxRight := Screen.Width - 1;
  MaxBottom := Screen.Height - 1 - AppBarHeight;
//  Log([appbarheight]);

  C := aCenterRect;
  { 1e keus: rechts-onder }
  P.X := C.Right + 1;
  P.Y := C.Bottom + 1;

  { als niet OK dan ergens rechts }
  if P.Y + aHeight > MaxBottom then P.Y := MaxBottom - aHeight;
  if P.Y < 0 then P.Y := 0;

  Result := P;
  if (P.X + aWidth < MaxRight) and (P.X >= 0) then Exit;

  { 2e keus: links-onder }
  P.Y := C.Bottom + 1;
  P.X := C.Right - aWidth;

  { als niet OK dan ergens links }
  if P.Y + aHeight > MaxBottom then P.Y := MaxBottom - aHeight;
  if P.Y < 0 then P.Y := 0;

  if P.X + aWidth > MaxRight then P.X := MaxRight - aWidth;
  if P.X < 0 then P.X := 0;

  Result := P;
end;

procedure InitUnit;
var
  Info: TOSVersionInfo;
begin
 //Check Windows version
 Info.dwOSVersionInfoSize := SizeOf(Info);
 GetVersionEx(Info);
 if (Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and (Info.dwMajorVersion >= 5) then
   SetLayeredWindowAttributes := GetProcAddress(GetModulehandle(user32), 'SetLayeredWindowAttributes');
end;

function DoShellExecute(const FileName: string;
                        const Parameters: string = '';
                        const Directory: string = '';
                        Operation: TOperationParam = opOpen;
                        ShowCmd: Integer = SW_SHOWNORMAL): HINST;

//function DoShellExecute(const Operation, FileName, Parameters, Directory: string; ShowCmd: Integer): HINST;
const
  Op: array[TOperationParam] of string = ('', 'open', 'print', 'explore');
begin
  Result := 0;
  if FileName = '' then
    Exit;

  if not Between(ShowCmd, 0, 10) then
    ShowCmd := SW_SHOWNORMAL;
  Result := ShellExecute(HINSTANCE,
                         PChar(Op[Operation]),
                         PChar(FileName),
                         PChar(Parameters),
                         PChar(Directory),
                         ShowCmd);
  if Result <= 32 then
    raise EShellExecuteError.CreateFmt('ShellExcecute fout %d', [Result]);
end;

function MailTo(const Address: string): HINST;
begin
  Result := DoShellExecute(Pchar('mailto:' + Address))
end;

(*
function ExecuteWaitFile( ExeFile, Parameters: string): Boolean;

  procedure WaitFor(processHandle: THandle);
  var
    AMessage : TMsg;
    Result   : DWORD;
  begin
    repeat
      Result := MsgWaitForMultipleObjects(1,
                                          processHandle,
                                          False,
                                          INFINITE,
                                          QS_PAINT or
                                          QS_SENDMESSAGE);
      if Result = WAIT_FAILED then
        Exit;
      if Result = ( WAIT_OBJECT_0 + 1 ) then
      begin
        while PeekMessage(AMessage, 0, WM_PAINT, WM_PAINT, PM_REMOVE) do
          DispatchMessage(AMessage);
      end;
    until result = WAIT_OBJECT_0;
  end;

var
  ExecuteCommand: array[0 .. 512] of Char;
  StartUpInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
begin
  StrPCopy(ExecuteCommand , ExeFile + ' ' + Parameters);
  FillChar(StartUpInfo, SizeOf(StartUpInfo), #0);
  StartUpInfo.cb  := SizeOf(StartUpInfo);
  StartUpInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartUpInfo.wShowWindow := SW_MINIMIZE;
  if CreateProcess(nil,
                   ExecuteCommand,
                   nil,
                   nil,
                   False,
                   CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                   nil,
                   PChar(ExtractFileDir(ExeFile)),
                   StartUpInfo,
                   ProcessInfo) then
  begin
    WaitFor(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    result := true;
  end
  else
    result := false;
end;
*)

{  Windows 98/2000 doesn't want to foreground a window when
  some other window has the keyboard focus.
  ForceForegroundWindow is an enhanced SetForeGroundWindow/bringtofront
  function to bring a window to the front.
}


{
  Manchmal funktioniert die SetForeGroundWindow Funktion
  nicht so, wie sie sollte; besonders unter Windows 98/2000,
  wenn ein anderes Fenster den Fokus hat.
  ForceForegroundWindow ist eine "verbesserte" Version von
  der SetForeGroundWindow API-Funktion, um ein Fenster in
  den Vordergrund zu bringen.
}


function ForceForegroundWindow(hwnd: THandle): Boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(hwnd) then ShowWindow(hwnd, SW_RESTORE);

  if GetForegroundWindow = hwnd then Result := True
  else
  begin
    // Windows 98/2000 doesn't want to foreground a window when some other
    // window has keyboard focus

    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
      ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
      ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and
      (Win32MinorVersion > 0)))) then
    begin
      // Code from Karl E. Peterson, www.mvps.org/vb/sample.htm
      // Converted to Delphi by Ray Lischner
      // Published in The Delphi Magazine 55, page 16

      Result := False;
      ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
      ThisThreadID := GetWindowThreadPRocessId(hwnd, nil);
      if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then
      begin
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
        AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
        Result := (GetForegroundWindow = hwnd);
      end;
      if not Result then
      begin
        // Code by Daniel P. Stasinski
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0),
          SPIF_SENDCHANGE);
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hWnd);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
      end;
    end
    else
    begin
      BringWindowToTop(hwnd); // IE 5.5 related hack
      SetForegroundWindow(hwnd);
    end;

    Result := (GetForegroundWindow = hwnd);
  end;
end; { ForceForegroundWindow }


(*
// 2. Way:
//**********************************************

procedure ForceForegroundWindow(hwnd: THandle);
  // (W) 2001 Daniel Rolf
  // http://www.finecode.de
  // rolf@finecode.de
var
  hlp: TForm; 
begin 
  hlp := TForm.Create(nil); 
  try 
    hlp.BorderStyle := bsNone; 
    hlp.SetBounds(0, 0, 1, 1); 
    hlp.FormStyle := fsStayOnTop; 
    hlp.Show;
    mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0); 
    mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0); 
    SetForegroundWindow(hwnd);
  finally 
    hlp.Free; 
  end; 
end; 

// 3. Way: 
//********************************************** 
// by Thomas Stutz 

{ 
  As far as you know the SetForegroundWindow function on Windows 98/2000 can
  not force a window to the foreground while the user is working with another window.
  Instead, SetForegroundWindow will activate the window and call the FlashWindowEx
  function to notify the user. However in some kind of applications it is necessary
  to make another window active and put the thread that created this window into the
  foreground and of course, you can do it using one more undocumented function from
  the USER32.DLL.

  void SwitchToThisWindow (HWND hWnd,  // Handle to the window that should be activated
  BOOL bRestore // Restore the window if it is minimized
);

}

procedure SwitchToThisWindow(h1: hWnd; x: bool); stdcall;
  external user32 Name 'SwitchToThisWindow';
         {x = false: Size unchanged, x = true: normal size}


procedure TForm1.Button2Click(Sender: TObject);
begin
  SwitchToThisWindow(FindWindow('notepad', nil), True);
end;
*)

procedure RebuildIconCache;
var
  Dummy: DWORD;
begin
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS,
    Longint(PChar('WindowMetrics')), SMTO_NORMAL or SMTO_ABORTIFHUNG, 10000, Dummy);
end;

procedure AssociateFileExtension(const IconPath, ProgramName, Path, Extension: string);
const
  RC_DefaultIcon = 'DefaultIcon';
begin

  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey(ProgramName, True);
    WriteString('', ProgramName);
    if IconPath <> '' then
    begin
      OpenKey(RC_DefaultIcon, True);
      WriteString('', IconPath);
    end;
    CloseKey;
    OpenKey(ProgramName, True);
    OpenKey('shell', True);
    OpenKey('open', True);
    OpenKey('command', True);
    WriteString('', '"' + Path + '" "%1"');
    Free;
  end;

  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('.' + extension, True);
    WriteString('', ProgramName);
    Free;
  end;

  RebuildIconCache;
end;


initialization
  InitUnit;

(* als <= 32
    procedure CheckExecError;
    var
      Msg: string;
    begin
      Msg := '';
      case ExecError of
        0                      : Msg := 'Het besturingssysteem heeft te weinig geheugen of resources';
        ERROR_FILE_NOT_FOUND   : Msg := 'Het opgegeven bestand is niet gevonden';
        ERROR_PATH_NOT_FOUND   : Msg := 'Het opgegeven pad is niet gevonden';
        ERROR_BAD_FORMAT       : Msg := 'Het .EXE bestand is ongeldig (niet-Win32 .EXE of fout in .EXE image)';
        SE_ERR_ACCESSDENIED    : Msg := 'Het besturingssysteem geeft geen toegang tot het opgegeven bestand';
        SE_ERR_ASSOCINCOMPLETE : Msg := 'De bestandsnaam associatie is niet compleet of ongeldig';
        SE_ERR_DDEBUSY         : Msg := 'DDE transactie kon niet worden voltooid omdat andere DDE transacties bezig waren';
        SE_ERR_DDEFAIL         : Msg := 'DDE transactie mislukt';
        SE_ERR_DDETIMEOUT      : Msg := 'DDE transactie kom niet worden voltooid wegens een timeout';
        SE_ERR_DLLNOTFOUND     : Msg := 'De opgegeven dynamic-link library werd niet gevonden';
        SE_ERR_NOASSOC         : Msg := 'Er is geen applicatie geassocieerd met de opgegeven bestandsextensie';
        SE_ERR_OOM             : Msg := 'Er is niet genoeg geheugen om de operatie te voltooien';
        SE_ERR_SHARE           : Msg := 'Er trad een sharing violation op';
      end;
      if Msg <> '' then
        raise EShellExecuteError.Create(Msg);
    end;
*)
{    ShellExecute(HINSTANCE, nil, PChar(S), nil, nil, SW_SHOWNORMAL);
function ShellExecute(hWnd: HWND; Operation, FileName, Parameters,
  Directory: PChar; ShowCmd: Integer): HINST; stdcall; }
//function DoShellExecute(const Operation, Parameters, Directory): HINST;
(*
SW_HIDE	Hides the window and activates another window.
SW_MAXIMIZE	Maximizes the specified window.
SW_MINIMIZE	Minimizes the specified window and activates the next top-level window in the Z order.
SW_RESTORE	Activates and displays the window. If the window is minimized or maximized, Windows restores it to its original size and position. An application should specify this flag when restoring a minimized window.
SW_SHOW	Activates the window and displays it in its current size and position.
SW_SHOWDEFAULT	Sets the show state based on the SW_ flag specified in the STARTUPINFO structure passed to the CreateProcess function by the program that started the application. An application should call ShowWindow with this flag to set the initial show state of its main window.
SW_SHOWMAXIMIZED	Activates the window and displays it as a maximized window.
SW_SHOWMINIMIZED	Activates the window and displays it as a minimized window.
SW_SHOWMINNOACTIVE	Displays the window as a minimized window. The active window remains active.
SW_SHOWNA	Displays the window in its current state. The active window remains active.
SW_SHOWNOACTIVATE	Displays a window in its most recent size and position. The active window remains active.
SW_SHOWNORMAL	Activates and displays a window. If the window is minimized or maximized, Windows restores it to its original size and position. An application should specify this flag when displaying the window for the first time.


  SW_HIDE = 0;
  SW_SHOWNORMAL = 1;
  SW_NORMAL = 1;
  SW_SHOWMINIMIZED = 2;
  SW_SHOWMAXIMIZED = 3;
  SW_MAXIMIZE = 3;
  SW_SHOWNOACTIVATE = 4;
  SW_SHOW = 5;
  SW_MINIMIZE = 6;
  SW_SHOWMINNOACTIVE = 7;
  SW_SHOWNA = 8;
  SW_RESTORE = 9;
  SW_SHOWDEFAULT = 10;
  SW_MAX = 10;



"open"	The function opens the file specified by lpFile. The file can be an executable file or a document file. The file can be a folder to open.
"print"	The function prints the file specified by lpFile. The file should be a document file. If the file is an executable file, the function opens the file, as if "open" had been specified.
"explore"	The function explores the folder specified by lpFile.

*)

{    ShellExecute(HINSTANCE, nil, PChar(S), nil, nil, SW_SHOWNORMAL);
function ShellExecute(hWnd: HWND; Operation, FileName, Parameters,
  Directory: PChar; ShowCmd: Integer): HINST; stdcall; }
end.

