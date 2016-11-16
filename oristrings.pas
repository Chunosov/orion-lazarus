unit OriStrings;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TStringArray = array of String;

function SplitStr(const S, Delims: String): TStringArray;
function SplitStr(const S, Delims: String; var Parts: TStringArray): Integer;
procedure SplitString(const S, Delims: String; var Parts: TStringArray);
function ReplaceChar(const AStr: String; const AFrom, ATo: Char): String;
function CharPos(const Str: String; Ch: Char): Integer; overload;
function LeftTill(const Source: String; Ch: Char): String;
function StartsWith(const Source, SubStr: String): Boolean;
function StartsWithI(const Source, SubStr: String): Boolean; inline;
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload; inline;
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload; inline;

function FormatEx(const Format: String; const Args: array of const): String;

implementation

uses
  Strings, LazUTF8;

function SplitStr(const S, Delims: String): TStringArray;
begin
  Result := nil;
  SplitString(S, Delims, Result);
end;

function SplitStr(const S, Delims: String; var Parts: TStringArray): Integer;
begin
  SplitString(S, Delims, Parts);
  Result := Length(Parts);
end;

procedure SplitString(const S, Delims: String; var Parts: TStringArray);
var
  L, I, Len, Index: Integer;
begin
  Parts := nil;
  I := 1;
  Index := 1;
  Len := Length(S);
  while I <= Len do
  begin
    if StrScan(PChar(Delims), S[I]) <> nil then
    begin
      if I-Index > 0 then
      begin
        L := Length(Parts);
        SetLength(Parts, L+1);
        Parts[L] := Copy(S, Index, I-Index);
      end;
      Index := I+1;
    end;
    Inc(I);
  end;
  if I-Index > 0 then
  begin
    L := Length(Parts);
    SetLength(Parts, L+1);
    Parts[L] := Copy(S, Index, I-Index);
  end;
end;

function ReplaceChar(const AStr: String; const AFrom, ATo: Char): String;
var
  chFrom, chTo: PChar;
begin
  SetLength(Result, Length(AStr));
  chFrom := PChar(AStr);
  chTo := PChar(Result);
  while chFrom^ <> #0 do
  begin
    if chFrom^ = AFrom
      then chTo^ := ATo
      else chTo^ := chFrom^;
    Inc(chFrom);
    Inc(chTo);
  end;
end;

function CharPos(const Str: String; Ch: Char): Integer;
var i: Integer;
begin
  for i := 1 to Length(Str) do
    if Str[i] = Ch then
    begin
      Result := i;
      Exit;
    end;
  Result := 0;
end;

function LeftTill(const Source: String; Ch: Char): String;
var
  I: Integer;
begin
  Result := Source;
  for I := 1 to Length(Result) do
    if Result[I] = Ch then
    begin
      SetLength(Result, I-1);
      Break;
    end;
end;

function StartsWith(const Source, SubStr: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(Source) < Length(SubStr) then Exit;
  for I := 1 to Length(SubStr) do
    if Source[I] <> SubStr[I] then Exit;
  Result := True;
end;

function StartsWithI(const Source, SubStr: String): Boolean; inline;
begin
  Result := SameText(Copy(Source, 1, Length(SubStr)), SubStr);
end;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;

function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := (C < #$0100) and (AnsiChar(C) in CharSet);
end;

function FormatEx(const Format: String; const Args: array of const): String;
var
  IsBraket: Boolean;
  I, Index, Param: Integer;
begin
  Result := '';
  Index := 1;
  IsBraket := False;
  for I := 1 to Length(Format) do
    case Format[I] of
      '{':
        begin
          Result := Result + Copy(Format, Index, I-Index);
          IsBraket := True;
          Index := I;
        end;
      '}':
        begin
          if IsBraket then
          begin
            IsBraket := False;
            Param := StrToIntDef(Copy(Format, Index+1, I-Index-1), -1);
            if (Param >= Low(Args)) and (Param <= High(Args)) then
              case Args[Param].VType of
                vtInteger     : Result := Result + IntToStr(Args[Param].VInteger);
                vtBoolean     : Result := Result + BoolToStr(Args[Param].VBoolean, True);
                vtChar        : Result := Result + String(Args[I].VChar);
                vtExtended    : Result := Result + FloatToStr(Args[Param].VExtended^);
                vtString      : Result := Result + String(Args[Param].VString^);
                vtPChar       : Result := Result + String(Args[Param].VPChar^);
                vtAnsiString  : Result := Result + String(Args[Param].VAnsiString);
                vtWideString  : Result := Result + String(Args[Param].VWideString);
                vtWideChar    : Result := Result + String(Args[Param].VWideChar);
                else            Result := Result + Copy(Format, Index, I-Index+1);
              end
            else Result := Result + Copy(Format, Index, I-Index+1);
          end
          else Result := Result + Copy(Format, Index, I-Index+1);
          Index := I+1;
        end;
    end;
  if Index <= Length(Format) then Result := Result + Copy(Format, Index, MaxInt);
end;

end.

