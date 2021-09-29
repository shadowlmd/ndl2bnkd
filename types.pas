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
unit Types;

interface

uses
  Objects;

type
  PExcludeCollection = ^TExcludeCollection;
  TExcludeCollection = object(TCollection)
    procedure FreeItem(Item: Pointer); virtual;
  end;

  PNodeList = ^TNodeList;
  TNodeList = packed record
    Path, Domain, Zone: PString;
  end;

  PNodeListCollection = ^TNodeListCollection;
  TNodeListCollection = object(TCollection)
    procedure FreeItem(Item: Pointer); virtual;
  end;

  PNodeDef = ^TNodeDef;
  TNodeDef = packed record
    Address, Domain, INA: PString;
  end;

  PNodeDefCollection = ^TNodeDefCollection;
  TNodeDefCollection = object(TSortedCollection)
    procedure Insert(Item: Pointer); virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function Compare(Key1, Key2: Pointer): Integer; virtual;
  end;

function NewPString(const S: String): PString;

implementation

uses
  Misc;

type
  TAddressRec = array [0..3] of String[25];

function NewPString(const S: String): PString;
{$IFDEF BPC}
var
  Result: PString;
{$ENDIF}
begin
  GetMem(Result, Length(S)+1);
  Result^ := S;
  {$IFDEF BPC}
  NewPString := Result;
  {$ENDIF}
end;

procedure TExcludeCollection.FreeItem(Item: Pointer);
begin
  DisposeStr(Item);
end;

procedure TNodeListCollection.FreeItem(Item: Pointer);
begin
  if Item <> nil then
  begin
    with PNodeList(Item)^ do
    begin
      DisposeStr(Path);
      DisposeStr(Domain);
      DisposeStr(Zone);
    end;
    Dispose(PNodeList(Item));
  end;
end;

procedure TNodeDefCollection.Insert(Item: Pointer);
var
  I: Integer;
begin
  if not Search(KeyOf(Item), I) or Duplicates then
    AtInsert(I, Item)
  else
    FreeItem(Item);
end;

procedure TNodeDefCollection.FreeItem(Item: Pointer);
begin
  if Item <> nil then
  begin
    with PNodeDef(Item)^ do
    begin
      DisposeStr(Address);
      DisposeStr(Domain);
      DisposeStr(INA);
    end;
    Dispose(PNodeList(Item));
  end;
end;

function CompareString(const S1, S2: String): Integer;
begin
  if S1 < S2 then CompareString := -1 else
  if S1 > S1 then CompareString :=  1 else
    CompareString := 0;
end;

function CompareNumber(const S1, S2: String): Integer;
var
  N1, N2, Err1, Err2: Integer;
begin
  Val(S1, N1, Err1);
  Val(S2, N2, Err2);
  if (Err1 <> 0) or (Err2 <> 0) then CompareNumber := CompareString(S1, S2) else
  if N1 < N2                    then CompareNumber := -1                    else
  if N1 > N2                    then CompareNumber :=  1                    else
    CompareNumber := 0;
end;

procedure ParseAddress(var AR: TAddressRec; A: PString);
begin
  AR[0] := ExtractWord(A^, 0, ':', True);
  AR[1] := ExtractWord(A^, 1, ':', False);
  AR[2] := ExtractWord(AR[1], 1, '/', False);
  AR[3] := ExtractWord(AR[2], 1, '.', False);
  AR[1] := ExtractWord(AR[1], 0, '/', True);
  AR[2] := ExtractWord(AR[2], 0, '.', True);
end;

function CompareAddress(A1, A2: PString): Integer;
var
  AR1, AR2: TAddressRec;
  I: Byte;
  {$IFDEF BPC}
  Result: Integer;
  {$ENDIF}
begin
  ParseAddress(AR1, A1);
  ParseAddress(AR2, A2);
  for I := 0 to 3 do
  begin
    Result := CompareNumber(AR1[I], AR2[I]);
    if Result <> 0 then
      break;
  end;
  {$IFDEF BPC}
  CompareAddress := Result;
  {$ENDIF}
end;

function TNodeDefCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  if PNodeDef(Key1)^.Domain^  < PNodeDef(Key2)^.Domain^  then Compare := -1 else
  if PNodeDef(Key1)^.Domain^  > PNodeDef(Key2)^.Domain^  then Compare :=  1 else
    Compare := CompareAddress(PNodeDef(Key1)^.Address, PNodeDef(Key2)^.Address);
end;

end.
