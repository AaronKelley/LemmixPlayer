unit UClasses;

interface

uses
  Classes;

type
  TOwnedPersistent = class(TPersistent)
  private
    fOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
    procedure CheckOwnerType(aOwner: TPersistent; const Allowed: array of TClass);
    function FindOwner(aClass: TClass): TPersistent;
    function GetMaster: TPersistent;
  public
    constructor Create(aOwner: TPersistent);
    property Owner: TPersistent read fOwner;
  end;

implementation

uses
  (*ULog,*) UMisc;

{ TOwnedPersistent }

procedure TOwnedPersistent.CheckOwnerType(aOwner: TPersistent; const Allowed: array of TClass);
var
  i: integer;
  OK: boolean;
  S: string;
begin
  OK := False;
  for i := 0 to Length(Allowed) - 1 do
  begin
    if Allowed[i] <> nil then
      OK := OK or (aOwner is Allowed[i])
    else
      OK := True;
    if OK then
      Break;  
  end;

  if not OK then
  begin
    if aOwner <> nil then S := aOwner.ClassName else S := 'nil';
    AppErrorFmt('CheckOwnerType: %s is een ongeldig Owner type',[S], Self);
  end;
end;

constructor TOwnedPersistent.Create(aOwner: TPersistent);
begin
  fOwner := aOwner;
end;

function TOwnedPersistent.GetOwner: TPersistent;
begin
  Result := fOwner;
end;

type
  THackedPersistent = class(TPersistent);

function TOwnedPersistent.FindOwner(aClass: TClass): TPersistent;
var
  O: TPersistent;
begin
  O := Self;
  Result := nil;
  while O <> nil do
  begin
    O := THackedPersistent(O).GetOwner;
    if O = nil then
      Exit;
    if O is aClass then
    begin
      Result := O;
      Exit;
    end;
  end;
end;

function TOwnedPersistent.GetMaster: TPersistent;
var
  O: TPersistent;
begin
  Result := nil;
  O := Self;
  while O <> nil do
  begin
    O := THackedPersistent(O).GetOwner;
    if O <> nil then
      Result := O
    else
      Exit;
  end;
  Result := nil;
end;

{procedure TOwnedPersistent.Changed;
begin
  if Assigned(fOnChange) then fOnChange(Self);
end;

procedure TOwnedPersistent.Change(Sender: TObject);
begin
  Changed;
end;}

end.

