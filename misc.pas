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
unit Misc;

interface

const
  {$IFDEF FPC}
  PathSep = DirectorySeparator;
  {$ELSE}
  PathSep = '\';
  {$ENDIF}

  tmRight = $01;
  tmLeft  = $02;

type
 TCharSet = set of Char;

{$IFDEF BPC}
procedure SetLength(var S: String; const NewLength: Byte);
{$ENDIF}
procedure StUpcaseEx(var Dest: String; const Source: String);
function StUpcase(const S: String): String;
procedure TrimEx(var Dest: String; const Source: String; const Mode: Longint);
function Trim(const S: String): String;
function LTrim(const S: String): String;
function RTrim(const S: String): String;
procedure TrimSetEx(var Dest: String; const Source: String; const Mode: Longint; const CharSet: TCharSet);
function TrimSet(const S: String; const CharSet: TCharSet): String;
function LTrimSet(const S: String; const CharSet: TCharSet): String;
function RTrimSet(const S: String; const CharSet: TCharSet): String;
function StConsists(const S: String; const CharSet: TCharSet): Boolean;
procedure StReplaceEx(var S: String; A: String; const B: String);
function StReplace(const S, A, B: String): String;
function AddTrailPathSep(const Path: String): String;
function RemoveTrailPathSep(const Path: String): String;
function IsWildcard(const Mask: String): Boolean;
function MatchWildcard(const Src, Mask: String): Boolean;
function ExtractWord(const S: String; Position: Longint; Delim: Char; Exact: Boolean): String;

implementation

{$IFDEF BPC}
procedure SetLength(var S: String; const NewLength: Byte);
begin
  Byte(S[0]) := NewLength;
end;
{$ENDIF}

procedure StUpcaseEx(var Dest: String; const Source: String);
var
  K, L: Longint;
begin
  L := Length(Source);
  for K := 1 to L do
    Dest[K] := Upcase(Source[K]);
  SetLength(Dest, L);
end;

function StUpcase(const S: String): String;
{$IFDEF BPC}
var
  Result: String;
{$ENDIF}
begin
  StUpcaseEx(Result, S);
  {$IFDEF BPC}
  StUpcase := Result;
  {$ENDIF}
end;

procedure TrimEx(var Dest: String; const Source: String; const Mode: Longint);
var
  K, L: Longint;
begin
  K := 1;
  L := Length(Source);

  if Mode and tmLeft <> 0 then
    while (K <= L) and (Source[K] = ' ') do
      Inc(K);

  if Mode and tmRight <> 0 then
    while (L <> 0) and (Source[L] = ' ') do
      Dec(L);

  Dec(L, K);
  Inc(L);

  Dest := Copy(Source, K, L);
end;

function Trim(const S: String): String;
{$IFDEF BPC}
var
  Result: String;
{$ENDIF}
begin
  TrimEx(Result, S, tmRight or tmLeft);
  {$IFDEF BPC}
  Trim := Result;
  {$ENDIF}
end;

function LTrim(const S: String): String;
{$IFDEF BPC}
var
  Result: String;
{$ENDIF}
begin
  TrimEx(Result, S, tmLeft);
  {$IFDEF BPC}
  LTrim := Result;
  {$ENDIF}
end;

function RTrim(const S: String): String;
{$IFDEF BPC}
var
  Result: String;
{$ENDIF}
begin
  TrimEx(Result, S, tmRight);
  {$IFDEF BPC}
  RTrim := Result;
  {$ENDIF}
end;

procedure TrimSetEx(var Dest: String; const Source: String; const Mode: Longint; const CharSet: TCharSet);
var
  K, L: Longint;
begin
  K := 1;
  L := Length(Source);

  if Mode and tmLeft <> 0 then
    while (K <= L) and (Source[K] in CharSet) do
      Inc(K);

  if Mode and tmRight <> 0 then
    while (L <> 0) and (Source[L] in CharSet) do
      Dec(L);

  Dec(L, K);
  Inc(L);

  Dest := Copy(Source, K, L);
end;

function TrimSet(const S: String; const CharSet: TCharSet): String;
{$IFDEF BPC}
var
  Result: String;
{$ENDIF}
begin
  TrimSetEx(Result, S, tmRight or tmLeft, CharSet);
  {$IFDEF BPC}
  TrimSet := Result;
  {$ENDIF}
end;

function LTrimSet(const S: String; const CharSet: TCharSet): String;
{$IFDEF BPC}
var
  Result: String;
{$ENDIF}
begin
  TrimSetEx(Result, S, tmLeft, CharSet);
  {$IFDEF BPC}
  LTrimSet := Result;
  {$ENDIF}
end;

function RTrimSet(const S: String; const CharSet: TCharSet): String;
{$IFDEF BPC}
var
  Result: String;
{$ENDIF}
begin
  TrimSetEx(Result, S, tmRight, CharSet);
  {$IFDEF BPC}
  RTrimSet := Result;
  {$ENDIF}
end;

function StConsists(const S: String; const CharSet: TCharSet): Boolean;
var
  I, L: Longint;
begin
  StConsists := False;
  L := Length(S);

  if L = 0 then
    exit;

  for I := 1 to L do
    if not (S[I] in CharSet) then
      exit;

  StConsists := True;
end;

procedure StReplaceEx(var S: String; A: String; const B: String);
var
  K, LA, LB, I : Longint;
  S1           : String;

begin
  StUpcaseEx(S1, S);
  StUpcaseEx(A, A);

  K := Pos(A, S1);

  if K = 0 then
    exit;

  LA := Length(A);
  LB := Length(B);

  I := K;
  Inc(I, LA);
  Dec(I);

  repeat
    Delete(S, K, LA);
    Insert(B, S, K);

    Delete(S1, 1, I);
    I := Pos(A, S1);

    if I <> 0 then
    begin
      Inc(K, LB);
      Inc(K, I);
      Dec(K);

      Inc(I, LA);
      Dec(I);
    end;
  until I = 0;
end;

function StReplace(const S, A, B: String): String;
{$IFDEF BPC}
var
  Result: String;
{$ENDIF}
begin
  Result := S;
  StReplaceEx(Result, A, B);
  {$IFDEF BPC}
  StReplace := Result;
  {$ENDIF}
end;

function AddTrailPathSep(const Path: String): String;
begin
  if Path[Length(Path)] = PathSep then
    AddTrailPathSep := Path
  else
    AddTrailPathSep := Path + PathSep;
end;

function RemoveTrailPathSep(const Path: String): String;
var
  L: Longint;
  {$IFDEF BPC}
  Result: String;
  {$ENDIF}
begin
  Result := Path;
  L := Length(Result);
  if Result[L] = PathSep then
    SetLength(Result, L-1);
  {$IFDEF BPC}
  RemoveTrailPathSep := Result;
  {$ENDIF}
end;

function IsWildcard(const Mask: String): Boolean;
begin
  IsWildcard := (Pos('*', Mask) <> 0) or (Pos('?', Mask) <> 0);
end;

{
  MatchWildcard (WildEqu)
  (c) by Vladimir S. Lokhov <vsl@tula.net> <2:5022/18.14>, 1994-2000.
}

type
  TMatchWildcardStack = packed record
    Src, Mask: Byte;
  end;

function MatchWildcard(const Src, Mask: String): Boolean;
var
  Stack                      : array [1..128] of TMatchWildcardStack;
  StackPointer,
  SrcPosition, MaskPosition,
  SrcLength, MaskLength      : Byte;
begin
  MatchWildcard := False;

  if (Mask = '') and (Src <> '') then
    exit;

  MaskLength := Length(Mask);
  SrcLength := Length(Src);

  if Mask[MaskLength] <> '*' then
    while (MaskLength > 1) and (SrcLength > 1) do
    begin
      if (Mask[MaskLength] = '*') or (Mask[MaskLength] = '?') then
        break;

      if Mask[MaskLength] <> Src[SrcLength] then
        exit;

      Dec(MaskLength);
      Dec(SrcLength);
    end;

  if Mask[MaskLength] = '*' then
    while (Mask[MaskLength - 1] = '*') and (MaskLength > 1) do
      Dec(MaskLength);

  StackPointer := 0;

  SrcPosition := 1;
  MaskPosition := 1;

  while (SrcPosition <= SrcLength) and (MaskPosition <= MaskLength) do
  begin
    case Mask[MaskPosition] of
      '?':
      begin
        Inc(SrcPosition);
        Inc(MaskPosition);
      end;
      '*':
      begin
        if (MaskPosition = 1) or (Mask[MaskPosition - 1] <> '*') then
        Inc(StackPointer);

        Stack[StackPointer].Mask := MaskPosition;

        Inc(MaskPosition);

        if MaskPosition <= MaskLength then
          if (Mask[MaskPosition] <> '?') and (Mask[MaskPosition] <> '*') then
            while (SrcPosition <= Length(Src)) and (Src[SrcPosition] <> Mask[MaskPosition]) do
              Inc(SrcPosition);

        Stack[StackPointer].Src := SrcPosition + 1;
      end;
    else
      if Src[SrcPosition] = Mask[MaskPosition] then
      begin
        Inc(SrcPosition);
        Inc(MaskPosition);
      end else
      begin
        if StackPointer = 0 then
          exit;

        SrcPosition := Stack[StackPointer].Src;
        MaskPosition := Stack[StackPointer].Mask;

        Dec(StackPointer);
      end;
    end;

    while not ((SrcPosition <= SrcLength) xor (MaskPosition > MaskLength)) do
    begin
      if (MaskPosition >= MaskLength) and (Mask[MaskLength] = '*') then
        break;

      if StackPointer = 0 then
        exit;

      SrcPosition := Stack[StackPointer].Src;
      MaskPosition := Stack[StackPointer].Mask;

      Dec(StackPointer);
    end;
  end;

  MatchWildcard := True;
end;

function ExtractWord(const S: String; Position: Longint; Delim: Char; Exact: Boolean): String;
var
  I, L, K: Longint;
  {$IFDEF BPC}
  Result: String;
  {$ENDIF}
begin
  I := 0;
  K := 0;
  L := Length(S);

  while (I < Position) and (K < L) do
  begin
    Inc(K);
    if S[K] = Delim then
      Inc(I);
  end;

  Dec(L, K);
  Inc(K);
  Result := Copy(S, K, L);

  if Exact then
  begin
    I := Pos(Delim, Result);
    if I <> 0 then
      SetLength(Result, I-1);
  end;

  {$IFDEF BPC}
  ExtractWord := Result;
  {$ENDIF}
end;

end.
