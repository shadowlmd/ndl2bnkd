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
program ndl2bnkd;

uses
  Config,
  NodeList;

const
  ConfigName : String = 'ndl2bnkd.cfg';
  ErrCode    : Word   = 0;

var
  I: Longint;
  S: String;

begin
  WriteLn('ndl2bnkd 0.8 (C) 2018 Alexey Fayans, 2:5030/1997@fidonet');
  WriteLn;

  I := 1;
  while I <= ParamCount do
  begin
    S := ParamStr(I);
    if S = '-c' then
    begin
      if I <> ParamCount then
      begin
        Inc(I);
        ConfigName := ParamStr(I);
      end;
    end else
    if (S[2] = '?') or (Pos('help', S) <> 0) then
    begin
      WriteLn('usage: ', ParamStr(0), ' [-c config]');
      Halt(1);
    end else
      WriteLn('? unknown command line parameter: ', S);

    Inc(I);
  end;

  InitConfig;

  if not ReadConfig(ConfigName) then
    ErrCode := 2;

  if ErrCode = 0 then
  begin
    InitNodeList;
    ParseLists;
    if not WriteNodeDefs then
      ErrCode := 3;
    DoneNodeList;
  end;

  DoneConfig;

  if ErrCode <> 0 then
    Halt(ErrCode);
end.
