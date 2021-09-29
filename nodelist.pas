{$IFDEF VER70}
{$DEFINE BPC}
{$ENDIF}

{$IFDEF BPC}
{$F+}
{$ENDIF}

{$IFDEF VirtualPascal}
{&Delphi+,Use32+}
{$ENDIF}

{$IFDEF FPC}
{$IFNDEF NO_SMART_LINK}
{$SMARTLINK ON}
{$ENDIF}
{$MODE objfpc}
{$ENDIF}

{$I-}
unit NodeList;

interface

procedure InitNodeList;
procedure DoneNodeList;
procedure ParseLists;
function WriteNodeDefs: Boolean;

implementation

uses
  Dos,
  Misc,
  Types,
  Config;

const
  ExcludedCount : Longint = 0;
  TotalCount    : Longint = 0;

var
  NodeDefs: PNodeDefCollection;

procedure InitNodeList;
begin
  New(NodeDefs, Init(1000, 500));
  NodeDefs^.Duplicates := False;
end;

procedure DoneNodeList;
begin
  Dispose(NodeDefs, Done);
end;

procedure AddNodeDef(const Address, Domain, INA: String);
var
  P: PNodeDef;
begin
  if not InExcludeList(Address) then
  begin
    New(P);
    P^.Address := NewPString(Address);
    P^.Domain  := NewPString(Domain);
    P^.INA     := NewPString(INA);
    NodeDefs^.Insert(P);
  end else
    Inc(ExcludedCount);
end;

function WriteNodeDefs: Boolean;
var
  F: Text;

  procedure WriteNodeDef(P: Pointer);
  var
    S: String;
  begin
    with PNodeDef(P)^ do
    begin
      S := Cfg.Format;
      StReplaceEx(S, '%address%', Address^);
      StReplaceEx(S, '%domain%', Domain^);
      StReplaceEx(S, '%hostname%', INA^);
      WriteLn(F, S);
    end;
  end;

begin
  Assign(F, Cfg.NodeFile);
  Rewrite(F);
  if IOResult = 0 then
    WriteNodeDefs := True
  else
  begin
    WriteNodeDefs := False;
    WriteLn('! cannot write to "' + Cfg.NodeFile + '"');
    exit;
  end;

  NodeDefs^.ForEach(@WriteNodeDef);

  Close(F);
end;

function GetNodeListFileName(const Mask: String; var Name: String): Boolean;
var
  SR    : SearchRec;
  Time  : Longint;
  {$IFDEF BPC}
  Result: Boolean;
  {$ENDIF}
begin
  if IsWildcard(Mask) then
  begin
    Time := Low(Longint);
    FindFirst(Cfg.NodeListPath + Mask, AnyFile xor Directory xor VolumeID, SR);
    Result := DosError = 0;
    while DosError = 0 do
    begin
      if SR.Time >= Time then
      begin
        Time := SR.Time;
        Name := Cfg.NodeListPath + SR.Name;
      end;
      FindNext(SR);
    end;
    {$IFNDEF BPC}
    FindClose(SR);
    {$ENDIF}
  end else
  begin
    Name := Cfg.NodeListPath + Mask;
    Result := True;
  end;
  {$IFDEF BPC}
  GetNodeListFileName := Result;
  {$ENDIF}
end;

function ExtractNodePart(const Prefix, S: String): String;
var
  I, N: Longint;
  {$IFDEF BPC}
  Result: String;
  {$ENDIF}
begin
  I := Pos(Prefix, S);
  if I <> 0 then
  begin
    N := Length(Prefix);
    Result := Copy(S, I+N, Length(S)-N);
    I := Pos(',', Result);
    if I <> 0 then
      SetLength(Result, I-1);
  end else
    Result := '';
  {$IFDEF BPC}
  ExtractNodePart := Result;
  {$ENDIF}
end;

function CheckAddress(const S: String): Boolean;
begin
  if Pos('.', S) > 1 then
    CheckAddress := StConsists(S, ['0'..'9', 'A'..'Z', 'a'..'z', '.', '-'])
  else
  if (S[1] = '[') and (Pos(':', S) > 2) and (S[Length(S)] = ']') then
    CheckAddress := StConsists(S, ['0'..'9', 'A'..'F', 'a'..'f', ':', '[', ']'])
  else
    CheckAddress := False;
end;

function CheckPort(const S: String): Boolean;
var
  I: Longint;
  E: Integer;
begin
  Val(S, I, E);
  CheckPort := (E = 0) and (I > 0) and (I <= 65535);
end;

function AppendPort(const Address, Port: String): String;
begin
  if Length(Port) = 0 then
    AppendPort := Address
  else
    AppendPort := Address + ':' + Port;
end;

procedure AppendAddress(var AddrStr: String; const Address: String);
begin
  if Length(AddrStr) = 0 then
    AddrStr := Address
  else
    AddrStr := AddrStr + ';' + Address;
end;

function GetIpFromPhone(const S: String; var IP: String): Boolean;
var
  S1: String;
  I, N, Err: Integer;
begin
  GetIpFromPhone := False;

  IP := ExtractWord(S, 5, ',', True);

  if IP[1] <> '0' then
    exit;

  if StConsists(IP, ['0', '.', '-']) then
    exit;

  IP := StReplace(IP, '-', '.');

  S1 := ExtractWord(IP, 0, '.', True);
  if not StConsists(S1, ['0']) then
    exit;

  for I := 1 to 4 do
  begin
    S1 := ExtractWord(IP, I, '.', True);
    Val(S1, N, Err);
    if (N < 0) or (N > 255) or (Err <> 0) then
      exit;
  end;

  if ExtractWord(IP, 5, '.', False) <> '' then
    exit;

  Delete(IP, 1, Pos('.', IP));

  GetIpFromPhone := True;
end;

function GetAddressFromFlag(S: String; const Flag, Port: String; var Address: String): Boolean;
var
  Tmp: String;
  FlagLen: Byte;
begin
  GetAddressFromFlag := False;

  Address := '';
  FlagLen := Length(Flag);

  repeat
    Tmp := ExtractNodePart(Flag, S);
    if Tmp = '' then
      break;
    Delete(S, 1, Pos(Flag, S)+FlagLen-1+Length(Tmp));
    if CheckAddress(Tmp) then
      AppendAddress(Address, AppendPort(Tmp, Port));
  until Length(S) <= FlagLen;

  GetAddressFromFlag := Length(Address) <> 0;
end;

function GetAddressFromSystemField(const S: String; var Address: String): Boolean;
begin
  Address := ExtractWord(S, 2, ',', True);

  GetAddressFromSystemField := CheckAddress(Address);
end;

function ProcessFlags(const S: String; var Addresses: String; ProcessSystemField, AllowAsterisks: Boolean): Boolean;
var
  Flags, IBN, Port, TmpFlags, TmpAddr, Tmp: String;
  NormalPort: Boolean;
  I, J: Integer;
begin
  ProcessFlags := False;

  Flags := ExtractWord(S, 7, ',', False);
  TmpFlags := Flags;
  Addresses := '';

  repeat
    I := Pos('IBN', TmpFlags);
    if I = 0 then
      break;

    NormalPort := True;
    Port := '';
    J := Pos('IBN:', TmpFlags);

    if J = I then
    begin
      IBN := ExtractNodePart('IBN:', TmpFlags);
      Delete(TmpFlags, 1, I+3+Length(IBN));
      I := Pos(']:', IBN);
      if I > 0 then
      begin
        Tmp  := Copy(IBN, 1, I);
        Port := Copy(IBN, I+2, Length(IBN)-I-1);
      end else
      begin
        if Pos('[', IBN) <> 1 then
          I := Pos(':', IBN);
        if I > 0 then
        begin
          Tmp  := ExtractWord(IBN, 0, ':', True);
          Port := ExtractWord(IBN, 1, ':', False);
        end else
          Tmp  := IBN;
      end;
      if CheckAddress(Tmp) then
      begin
        if (I > 0) and not CheckPort(Port) then
          continue;
        NormalPort := False;
      end else
      begin
        if I > 0 then
          continue;
        if not CheckPort(Tmp) then
          continue;
        Port := Tmp;
      end;
    end else
      Delete(TmpFlags, 1, I+2);

    TmpAddr := '';

    if not NormalPort then
      TmpAddr := IBN
    else
    if GetAddressFromFlag(Flags, 'INA:', Port, Tmp) then
      TmpAddr := Tmp
    else
    if Cfg.AddressInPhone and GetIpFromPhone(S, Tmp) then
      TmpAddr := AppendPort(Tmp, Port)
    else
    if Cfg.AddressInIpFlag and GetAddressFromFlag(Flags, 'IP:', Port, Tmp) then
      TmpAddr := Tmp
    else
    if ProcessSystemField and GetAddressFromSystemField(S, Tmp) then
      TmpAddr := AppendPort(Tmp, Port)
    else
    if AllowAsterisks then
      TmpAddr := AppendPort('*', Port);

    if TmpAddr <> '' then
      AppendAddress(Addresses, TmpAddr);
  until Length(TmpFlags) < 3;

  ProcessFlags := Addresses <> '';
end;

procedure ParseNodeList(const FileMask, Domain: String; Zone: String);
var
  F: Text;
  S, FileName, Host, Node, INA: String;
  S1: String[4];
  NormalNode: Boolean;
  Count: Longint;
begin
  if not GetNodeListFileName(FileMask, FileName) then
  begin
    WriteLn('? cannot find "' + FileMask + '" in "' + Cfg.NodeListPath + '"');
    exit;
  end;

  Assign(F, FileName);
  Reset(F);
  if IOResult <> 0 then
  begin
    WriteLn('? cannot open nodelist: "' + FileName + '"');
    exit;
  end;

  Write('* parsing nodelist  : "' + FileName + '"');

  if Zone = '' then
    Zone := '0';
  Host   := Zone;
  Count  := 0;

  while not Eof(F) do
  begin
    ReadLn(F, S);

    if (S[1] = ';') or (S = '') then
      continue;

    S1 := StUpcase(Copy(S, 1, 4));

    if (S1 = 'HOLD') or (S1 = 'DOWN') then
      continue;

    NormalNode := True;

    if S[1] <> ',' then
    begin
      if (S1 = 'HOST') or (S1 = 'REGI') then
      begin
        Host := ExtractWord(S, 1, ',', True);
        NormalNode := False;
      end else
      if S1 = 'ZONE' then
      begin
        Host := ExtractWord(S, 1, ',', True);
        Zone := Host;
        NormalNode := False;
      end;
    end;

    if not ProcessFlags(S, INA, Cfg.AddressInSystem, Cfg.UseAsterisks) then
      continue;

    if NormalNode then
      Node := ExtractWord(S, 1, ',', True)
    else
      Node := '0';

    AddNodeDef(Zone + ':' + Host + '/' + Node, Domain, INA);
    Inc(Count);
  end;
  Close(F);
  WriteLn(' [', Count, ']');
  Inc(TotalCount, Count);
end;

procedure ParsePointList(const FileMask, Domain: String);
var
  F: Text;
  S, FileName, Boss, Point, INA: String;
  S1: String[4];
  Count: Longint;
begin
  if not GetNodeListFileName(FileMask, FileName) then
  begin
    WriteLn('? cannot find "' + FileMask + '" in "' + Cfg.NodeListPath + '"');
    exit;
  end;

  Assign(F, FileName);
  Reset(F);
  if IOResult <> 0 then
  begin
    WriteLn('? cannot open pointlist: "' + FileName + '"');
    exit;
  end;

  Write('* parsing pointlist : "' + FileName + '"');
  Count := 0;

  while not Eof(F) do
  begin
    ReadLn(F, S);

    if (S[1] = ';') or (S = '') then
      continue;

    S1 := Copy(S, 1, 4);

    if S1 = 'Boss' then
    begin
      Boss := ExtractWord(S, 1, ',', True);
      continue;
    end;

    if not ProcessFlags(S, INA, Cfg.AddressInSystemPnt, Cfg.UseAsterisksPnt) then
      continue;

    Point := ExtractWord(S, 1, ',', True);

    AddNodeDef(Boss + '.' + Point, Domain, INA);
    Inc(Count);
  end;
  Close(F);
  WriteLn(' [', Count, ']');
  Inc(TotalCount, Count);
end;

procedure ParseLists;

  procedure ParseList(const P: Pointer);
  begin
    with PNodeList(P)^ do
      if Zone <> nil then
        ParseNodeList(Path^, Domain^, Zone^)
      else
        ParsePointList(Path^, Domain^);
  end;

begin
  NodeLists^.ForEach(@ParseList);
  WriteLn;
  WriteLn('* total nodes     : ', TotalCount);
  WriteLn('* excluded nodes  : ', ExcludedCount);
  WriteLn('* duplicate nodes : ', TotalCount - ExcludedCount - NodeDefs^.Count);
  WriteLn('* written nodes   : ', NodeDefs^.Count);
end;

end.
