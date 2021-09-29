{$IFDEF VER70}
{$DEFINE BPC}
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
unit Config;

interface

uses
  Types;

type
  TConfigRec = record
    NodeListPath, Format, NodeFile: String;
    AddressInSystem, AddressInSystemPnt, AddressInPhone, AddressInIpFlag,
    UseAsterisks, UseAsterisksPnt: Boolean;
  end;

var
  Cfg       : TConfigRec;
  NodeLists : PNodeListCollection;

procedure InitConfig;
procedure DoneConfig;
function ReadConfig(const FileName: String): Boolean;
function InExcludeList(const Address: String): Boolean;

implementation

uses
  Misc;

var
  ExcludeList: PExcludeCollection;

procedure InitConfig;
begin
  FillChar(Cfg, SizeOf(TConfigRec), #0);
  NodeLists := New(PNodeListCollection, Init(10, 10));
  ExcludeList := New(PExcludeCollection, Init(10, 10));
end;

procedure DoneConfig;
begin
  Dispose(NodeLists, Done);
  Dispose(ExcludeList, Done);
end;

function InExcludeList(const Address: String): Boolean;

  function Matches(P: Pointer): Boolean; {$IFDEF BPC} far; {$ENDIF}
  begin
    if P <> nil then
    begin
      if IsWildcard(String(P^)) then
        Matches := MatchWildCard(Address, String(P^))
      else
        Matches := Address = String(P^);
    end else
      Matches := False;
  end;

begin
  InExcludeList := ExcludeList^.FirstThat(@Matches) <> nil;
end;

function ReadConfig(const FileName: String): Boolean;

  function GetKeyName(var S: String): String;
  var
    I: Longint;
  begin
    TrimEx(S, S, tmLeft);

    I := Pos(' ', S);
    if I <> 0 then
    begin
      GetKeyName := Copy(S, 1, I-1);
      Delete(S, 1, I);
    end else
    begin
      GetKeyName := S;
      SetLength(S, 0);
    end;
  end;

var
  F        : Text;
  S, Key   : String;
  I        : Longint;
  NodeList : PNodeList;

begin
  Assign(F, FileName);
  Reset(F);
  if IOResult <> 0 then
  begin
    WriteLn('! cannot open configuration file: "' + FileName + '"');
    ReadConfig := False;
    exit;
  end;

  while not Eof(F) do
  begin
    ReadLn(F, S);

    TrimEx(S, S, tmRight or tmLeft);

    if (S[1] = '#') or (S = '') then
      continue;

    I := Pos('#', S);
    if I <> 0 then
    begin
      SetLength(S, I-1);
      TrimEx(S, S, tmRight);
    end;

    StUpcaseEx(Key, GetKeyName(S));
    if Key = 'NODELIST' then
    begin
      New(NodeList);
      NodeList^.Path   := NewPString(GetKeyName(S));
      NodeList^.Domain := NewPString(GetKeyName(S));
      NodeList^.Zone   := NewPString(LTrim(S));
      NodeLists^.Insert(NodeList);
    end else
    if Key = 'POINTLIST' then
    begin
      New(NodeList);
      NodeList^.Path   := NewPString(GetKeyName(S));
      NodeList^.Domain := NewPString(LTrim(S));
      NodeList^.Zone   := nil;
      NodeLists^.Insert(NodeList);
    end else
    if Key = 'EXCLUDE' then
      ExcludeList^.Insert(NewPString(LTrim(S)))
    else
    if Key = 'NODELISTPATH' then
      Cfg.NodeListPath := AddTrailPathSep(LTrim(S))
    else
    if Key = 'FORMAT' then
      Cfg.Format := LTrim(S)
    else
    if Key = 'NODEFILE' then
      Cfg.NodeFile := LTrim(S)
    else
    if Key = 'ADDRESSINIPFLAG' then
      Cfg.AddressInIpFlag := True
    else
    if Key = 'ADDRESSINPHONE' then
      Cfg.AddressInPhone := True
    else
    if Key = 'ADDRESSINSYSTEM' then
      Cfg.AddressInSystem := True
    else
    if Key = 'ADDRESSINSYSTEMPNT' then
      Cfg.AddressInSystemPnt := True
    else
    if Key = 'USEASTERISKS' then
      Cfg.UseAsterisks := True
    else
    if Key = 'USEASTERISKSPNT' then
      Cfg.UseAsterisksPnt := True
    else
      WriteLn('? unknown configuration keyword: ' + Key);
  end;

  Close(F);
  ReadConfig := True;
end;

end.
