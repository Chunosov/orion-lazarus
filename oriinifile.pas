unit OriIniFile;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, IniFiles, Graphics, Forms;

type
  TOriIniFile = class(TIniFile)
  private
    FSection: String;
    FFormat: TFormatSettings;
    Parts: array of String;

    function ResembleString(const S: String): Integer;
    function ResolutionSufix: String;

  public
    constructor Create; reintroduce;
    constructor Create(const AFileName: String); reintroduce;
    destructor Destroy; override;

    function KeyExists(const Key: String): Boolean; overload; inline;
    function KeyExists(const Section, Key: String): Boolean; overload; inline;

    procedure WriteString(const Key, Value: String); overload;
    procedure WriteInteger(const Key: string; Value: Longint); overload;
    procedure WriteBool(const Key: string; Value: Boolean); overload;
    procedure WriteFloat(const Key: string; Value: Extended); overload;
    procedure WriteDate(const Key: string; Value: TDateTime); overload;
    procedure WriteTime(const Key: string; Value: TDateTime); overload;
    procedure WriteDateTime(const Key: string; Value: TDateTime); overload;

    function ReadString(const Key, Value: String): String; overload;
    function ReadInteger(const Key: string; Value: Longint): Longint; overload;
    function ReadBool(const Key: string; Value: Boolean): Boolean; overload;
    function ReadFloat(const Key: string; Value: Extended): Extended; overload;
    function ReadDate(const Key: string; Value: TDateTime): TDateTime; overload;
    function ReadTime(const Key: string; Value: TDateTime): TDateTime; overload;
    function ReadDateTime(const Key: string; Value: TDateTime): TDateTime; overload;

    procedure ReadText(const Key: String; Value: TStrings; const Default: String); overload;
    procedure WriteText(const Key: String; Value: TStrings); overload;
    function ReadText(const Key: String; const Default: String = ''): String; overload;
    procedure WriteText(const Key: String; const Value: String); overload;

    procedure WriteRect(const Key: String; const Value: TRect);
    function  ReadRect(const Key: String; const Value: TRect): TRect;
    procedure WritePoint(const Key: String; const Value: TPoint); overload;
    function  ReadPoint(const Key: String; const Value: TPoint): TPoint; overload;
    procedure WritePoint(const Key: String; X, Y: Integer); overload;
    function  ReadPoint(const Key: String; X, Y: Integer): TPoint; overload;

    procedure WriteFont(const Key: String; Font: TFont);
    procedure ReadFont(const Key: String; Font: TFont);

    procedure WriteFormPos(Form: TCustomForm; const Key: String = '');
    procedure WriteFormSize(Form: TCustomForm; const Key: String = '');
    procedure WriteFormSizePos(Form: TCustomForm; const Key: String = '');
    procedure ReadFormSizePos(Form: TCustomForm; const Key: String = ''; DefaultLeft: Integer = 0; DefaultTop: Integer = 0);
    procedure ReadFormPos(Form: TCustomForm; const Key: String = ''; DefaultLeft: Integer = 0; DefaultTop: Integer = 0);
    procedure ReadFormSize(Form: TCustomForm; const Key: String = '');

    function GetMaxSectionIndex(const Prefix: String): Integer;
    function NextSection(const Prefix: String): String;

    procedure GetSectionKeysOnly(Strings: TStrings); overload;
    procedure GetSectionKeysOnly(const Section: string; Strings: TStrings); overload;

    procedure Init;
    procedure Save;
    procedure UpdateFile; override;

    property Section: String read FSection write FSection;
  end;

  TOriMemIniFile = class(TOriIniFile)

  end;

  TPropSaver = TOriIniFile deprecated 'Use class TOriIniFile';

{ Pointer to a function returning path to application ini-file.
  It is used by TOriIniFile when its constructor is called without parameters.
  The GetIniFileName function is used by default.
}
var GetIniName: function(): String;

{ The flag defines if application is portable. Portable application stores all
  its settings, templates and various user-specific infomation near the program
  executable file. So application must have write access to a folder where it
  is located. Non-portable (normal) application stores its settings in user
  profile catalog (~/.config/appname/ on Linux, $(APPDATA)\appname\ on Windows).
}
var IsPortable: Boolean;

const CMainSection = 'PREFERENCES';

procedure StoreFormGeometry(AForm: TCustomForm);
procedure RestoreFormGeometry(AForm: TCustomForm);

implementation

uses
  Math, LazFileUtils, LazUTF8;

{%region Quick helpers}
procedure StoreFormGeometry(AForm: TCustomForm);
begin
  with TOriIniFile.Create do
  try
    WriteFormSizePos(AForm);
	finally
    Free;
	end;
end;

procedure RestoreFormGeometry(AForm: TCustomForm);
begin
  with TOriIniFile.Create do
  try
    ReadFormSizePos(AForm);
	finally
    Free;
	end;
end;
{%endregion}

{%region Ini File Location}
var IniFileName: String;

function GetLocalIniFileName: String;
begin
  Result := ChangeFileExt(ParamStrUTF8(0), '.cfg');
end;

function GetIniFileName: String;
begin
  if IniFileName = '' then
  begin
    if IsPortable then
      IniFileName := GetLocalIniFileName
    else
    begin
      // Windows: c:\Users\<user name>\AppData\Local\spectrum\spectrum.cfg
      IniFileName := GetAppConfigFileUTF8(False, {$ifdef WINDOWS}False{$else}True{$endif});
    end;
  end;
  Result := IniFileName;
end;
{%endregion}

{%region TOriIniFile}
constructor TOriIniFile.Create;
begin
  FSection := CMainSection;

  if Assigned(GetIniName)
    then inherited Create(GetIniName())
    else inherited Create(GetIniFileName);

  Init;
end;

constructor TOriIniFile.Create(const AFileName: String);
begin
  FSection := CMainSection;
  inherited Create(AFileName);
  Init;
end;


destructor TOriIniFile.Destroy;
begin
  if Dirty then UpdateFile;
  inherited;
end;

procedure TOriIniFile.Init;
begin
  FFormat.DecimalSeparator := '.';
  FFormat.DateSeparator := '.';
  FFormat.TimeSeparator := ':';
  FFormat.ShortDateFormat := 'dd.mm.yyyy';
  FFormat.ShortTimeFormat := 'hh:nn';
  FFormat.LongDateFormat := 'dd mmmm yyyy';
  FFormat.LongTimeFormat := 'hh:nn:ss.zzzz';

  CaseSensitive := True;
  CacheUpdates := True;
end;

procedure TOriIniFile.Save;
begin
  UpdateFile;
end;

procedure TOriIniFile.UpdateFile;
var
  Dir: String;
begin
  if not Dirty then Exit;
  Dir := TrimFilename(ExtractFilePath(FileName));
  if not DirectoryExistsUTF8(Dir) then
    if not CreateDirUTF8(Dir) then
      raise Exception.CreateFmt('Unable to create directory "%s"', [Dir]);
  inherited;
end;

// разбивает строку на части, которые содержатся в StringArray,
// разделитель - точка с запятой используется для загрузки массивов
function TOriIniFile.ResembleString(const S: String): Integer;
var
  L, I, Index: Integer;
begin
  Index := 1;
  Parts := nil;
  I := 1;
  while I <= Length(S) do
  begin
    if S[I] = ';' then
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
  // от последнего разделителя до конца строки
  if I-Index > 0 then
  begin
    L := Length(Parts);
    SetLength(Parts, L+1);
    Parts[L] := Copy(S, Index, I-Index);
  end;
  Result := Length(Parts);
end;

function TOriIniFile.KeyExists(const Key: string): Boolean;
begin
  Result := ValueExists(Section, Key);
end;

function TOriIniFile.KeyExists(const Section, Key: string): Boolean;
begin
  Result := ValueExists(Section, Key);
end;

{%region Common Types}
procedure TOriIniFile.WriteString(const Key, Value: String);
begin
  WriteString(FSection, Key, Value);
end;

function TOriIniFile.ReadString(const Key, Value: String): String;
begin
  Result := ReadString(FSection, Key, Value);
end;

procedure TOriIniFile.WriteInteger(const Key: string; Value: Longint);
begin
  WriteInteger(FSection, Key, Value);
end;

function TOriIniFile.ReadInteger(const Key: string; Value: Longint): Longint;
begin
  Result := ReadInteger(FSection, Key, Value);
end;

procedure TOriIniFile.WriteBool(const Key: string; Value: Boolean);
begin
  WriteBool(FSection, Key, Value);
end;

function TOriIniFile.ReadBool(const Key: string; Value: Boolean): Boolean;
begin
  Result := ReadBool(FSection, Key, Value);
end;

procedure TOriIniFile.WriteFloat(const Key: string; Value: Extended);
begin
  WriteString(FSection, Key, FloatToStr(Value, FFormat));
end;

function TOriIniFile.ReadFloat(const Key: string; Value: Extended): Extended;
var S: String;
begin
  S := UpperCase(Trim(ReadString(FSection, Key, '')));
  if S = 'INF' then
    Result := Infinity
  else if S = '-INF' then
    Result := -Infinity
  else
    Result := StrToFloatDef(S, Value, FFormat);
end;
{%endregion}

{%region Date and Time}
procedure TOriIniFile.WriteDate(const Key: string; Value: TDateTime);
begin
  WriteString(FSection, Key, DateToStr(Value, FFormat));
end;

procedure TOriIniFile.WriteTime(const Key: string; Value: TDateTime);
begin
  WriteString(FSection, Key, TimeToStr(Value, FFormat));
end;

procedure TOriIniFile.WriteDateTime(const Key: string; Value: TDateTime);
begin
  WriteString(FSection, Key, DateTimeToStr(Value, FFormat));
end;

function TOriIniFile.ReadDate(const Key: string; Value: TDateTime): TDateTime;
begin
  if not TryStrToDate(ReadString(FSection, Key, ''), Result, FFormat) then Result := Value;
end;

function TOriIniFile.ReadTime(const Key: string; Value: TDateTime): TDateTime;
begin
  if not TryStrToTime(ReadString(FSection, Key, ''), Result, FFormat) then Result := Value;
end;

function TOriIniFile.ReadDateTime(const Key: string; Value: TDateTime): TDateTime;
begin
  if not TryStrToDateTime(ReadString(FSection, Key, ''), Result, FFormat) then Result := Value;
end;
{%endregion}

{%region Text}
procedure TOriIniFile.WriteText(const Key: String; const Value: String);
var
  Strs: TStrings;
begin
  Strs := TStringList.Create;
  try
    Strs.Text := Value;
    WriteText(Key, Strs);
  finally
    Strs.Free;
  end;
end;

function TOriIniFile.ReadText(const Key: String; const Default: String): String;
var
  Strs: TStrings;
begin
  if not KeyExists(Key) then
  begin
    Result := Default;
    Exit;
  end;
  Strs := TStringList.Create;
  try
    ReadText(Key, Strs, Default);
    Result := Strs.Text;
  finally
    Strs.Free;
  end;
end;

// Stores several lines of text into single string parameter.
procedure TOriIniFile.WriteText(const Key: String; Value: TStrings);
var OldDelimiter, OldQuoteChar: Char;
begin
  OldDelimiter := Value.Delimiter;
  OldQuoteChar := Value.QuoteChar;
  try
    Value.Delimiter := ',';
    Value.QuoteChar := '"';
    WriteString(Key, Value.DelimitedText);
  finally
    Value.Delimiter := OldDelimiter;
    Value.QuoteChar := OldQuoteChar;
  end;
end;

// Value of parameter Default should be in delimited text format, i.e.:
// "string1 string1","string 2 string2","string3 string3".
// Elsewise string will be splitted - each word will be at different line.
procedure TOriIniFile.ReadText(const Key: String; Value: TStrings; const Default: String);
var OldDelimiter, OldQuoteChar: Char;
begin
  OldDelimiter := Value.Delimiter;
  OldQuoteChar := Value.QuoteChar;
  try
    Value.Delimiter := ',';
    Value.QuoteChar := '"';
    Value.DelimitedText := ReadString(Key, Default);
  finally
    Value.Delimiter := OldDelimiter;
    Value.QuoteChar := OldQuoteChar;
  end;
end;
{%endregion}

{%region Point and Rect}
procedure TOriIniFile.WriteRect(const Key: String; const Value: TRect);
begin
  with Value do WriteString(Key, Format('%d;%d;%d;%d', [Left, Top, Right, Bottom]));
end;

function TOriIniFile.ReadRect(const Key: String; const Value: TRect): TRect;
begin
  if ResembleString(ReadString(Key, '')) > 3 then
  begin
    Result.Left   := StrToIntDef(Parts[0], Value.Left);
    Result.Top    := StrToIntDef(Parts[1], Value.Top);
    Result.Right  := StrToIntDef(Parts[2], Value.Right);
    Result.Bottom := StrToIntDef(Parts[3], Value.Bottom);
  end
  else Result := Value;
end;

procedure TOriIniFile.WritePoint(const Key: String; const Value: TPoint);
begin
  WriteString(Key, Format('%d;%d', [Value.X, Value.Y]));
end;

procedure TOriIniFile.WritePoint(const Key: String; X, Y: Integer);
begin
  WriteString(Key, Format('%d;%d', [X, Y]));
end;

function TOriIniFile.ReadPoint(const Key: String; const Value: TPoint): TPoint;
begin
  if ResembleString(ReadString(Key, '')) > 1 then
  begin
    Result.X := StrToIntDef(Parts[0], Value.X);
    Result.Y := StrToIntDef(Parts[1], Value.Y);
  end
  else Result := Value;
end;

function TOriIniFile.ReadPoint(const Key: String; X, Y: Integer): TPoint;
begin
  if ResembleString(ReadString(Key, '')) > 1 then
  begin
    Result.X := StrToIntDef(Parts[0], X);
    Result.Y := StrToIntDef(Parts[1], Y);
  end
  else
  begin
    Result.X := X;
    Result.Y := Y;
  end;
end;
{%endregion}

{%region Font}
procedure TOriIniFile.WriteFont(const Key: String; Font: TFont);
begin
  WriteString(Key, Format('%s;%d;$%s;%d;%d;%d;%d;%d',
    [Font.Name, Font.Size, IntToHex(Font.Color, 8), Ord(Font.Charset),
    Ord(fsBold in Font.Style), Ord(fsItalic in Font.Style),
    Ord(fsUnderline in Font.Style), Ord(fsStrikeOut in Font.Style)]));
end;

procedure TOriIniFile.ReadFont(const Key: String; Font: TFont);
begin
  if ResembleString(ReadString(Key, '')) > 7 then
    with Font do
    begin
      Name    := Parts[0];
      Size    := StrToIntDef(Parts[1], Size);
      Color   := StrToIntDef(Parts[2], Color);
      Charset := StrToIntDef(Parts[3], Charset);
      Style   := [];
      if StrToBool(Parts[4]) then Style := Style + [fsBold];
      if StrToBool(Parts[5]) then Style := Style + [fsItalic];
      if StrToBool(Parts[6]) then Style := Style + [fsUnderline];
      if StrToBool(Parts[7]) then Style := Style + [fsStrikeOut];
    end;
end;
{%endregion}

// Rerurns maximum number of a section whose name like 'PrefixXXX'
function TOriIniFile.GetMaxSectionIndex(const Prefix: String): Integer;
var
  I, Index: Integer;
  Strs: TStrings;
begin
  Result := 0;
  Strs := TStringList.Create;
  try
    ReadSections(Strs);
    for I := 0 to Strs.Count-1 do
      if Copy(Strs[I], 1, Length(Prefix)) = Prefix then
      begin
        Index := StrToIntDef(Copy(Strs[I], Length(Prefix)+1, MaxInt), 0);
        if Index > Result then Result := Index;
      end;
  finally
    Strs.Free;
  end;
end;

function TOriIniFile.NextSection(const Prefix: String): String;
begin
  Result := Prefix + IntToStr(GetMaxSectionIndex(Prefix) + 1);
end;

// Копирует из текущей секции только ключи, без значений (Key1)
procedure TOriIniFile.GetSectionKeysOnly(Strings: TStrings);
begin
  GetSectionKeysOnly(Section, Strings);
end;

// Копирует из заданной секции только ключи, без значений (Key1)
procedure TOriIniFile.GetSectionKeysOnly(const Section: string; Strings: TStrings);
begin
  ReadSection(Section, Strings);
end;
{%endregion}

{%region Forms}
function TOriIniFile.ResolutionSufix: String;
begin
  Result := Format('%dx%d', [Screen.Width, Screen.Height]);
end;

procedure TOriIniFile.WriteFormPos(Form: TCustomForm; const Key: String = '');
var _key: String;
begin
  if Key = '' then _key := Form.Name else _key := Key;
  WriteString(_key + 'Pos' + ResolutionSufix, Format('%d;%d', [Form.Left, Form.Top]));
end;

procedure TOriIniFile.WriteFormSize(Form: TCustomForm; const Key: String = '');
var _key: String;
begin
  if Key = '' then _key := Form.Name else _key := Key;
  WriteString(_key + 'Size' + ResolutionSufix, Format('%d;%d', [Form.Width, Form.Height]));
end;

procedure TOriIniFile.WriteFormSizePos(Form: TCustomForm; const Key: String = '');
var _key: String;
begin
  if Key = '' then _key := Form.Name else _key := Key;
  WriteString(_key + 'SizePos'+ ResolutionSufix, Format('%d;%d;%d;%d',
    [Form.Width, Form.Height, Form.Left, Form.Top]));
end;

procedure TOriIniFile.ReadFormPos(Form: TCustomForm; const Key: String = '';
  DefaultLeft: Integer = 0; DefaultTop: Integer = 0);
const SafeRange = 20;
var
  _key: String;
  W, H, Left, Top, DefLeft, DefTop: Integer;
begin
  if Key = '' then _key := Form.Name else _key := Key;
  if DefaultLeft = 0 then DefLeft := Form.Left else DefLeft := DefaultLeft;
  if DefaultTop = 0 then DefTop := Form.Top else DefTop := DefaultTop;
  if ResembleString(ReadString(_key + 'Pos' + ResolutionSufix, '')) > 1 then
  begin
    Left := StrToIntDef(Parts[0], DefLeft);
    Top := StrToIntDef(Parts[1], DefTop);
    W := Screen.DesktopWidth - SafeRange;
    H := Screen.DesktopHeight - SafeRange;
    if Left > W then Left := W
    else if Left < SafeRange-Form.Width then Left := SafeRange;
    if Top > H then Top := H
    else if Top < SafeRange-Form.Height then Top := SafeRange;
    Form.SetBounds(Left, Top, Form.Width, Form.Height);
  end
  else Form.SetBounds(DefLeft, DefTop, Form.Width, Form.Height);
end;

procedure TOriIniFile.ReadFormSize(Form: TCustomForm; const Key: String = '');
var _key: String;
begin
  if Key = '' then _key := Form.Name else _key := Key;
  if ResembleString(ReadString(_key + 'Size' + ResolutionSufix, '')) > 1 then
    Form.SetBounds(Form.Left, Form.Top,
      StrToIntDef(Parts[0], Form.Width), StrToIntDef(Parts[1], Form.Height));
end;

{ Предполагается, что у формы Position = poDesigned.
  Если для текущего разрешения параметр не найден, то форма сохраняет размеры
  и помещается в положение по умолчанию. Если по умолчанию заданы 0, то форма
  остается там же, где была.
}
procedure TOriIniFile.ReadFormSizePos(Form: TCustomForm; const Key: String = '';
  DefaultLeft: Integer = 0; DefaultTop: Integer = 0);
const SafeRange = 20;
var
  _key: String;
  W, H, Left, Top, Width, Height, DefLeft, DefTop: Integer;
begin
  if Key = '' then _key := Form.Name else _key := Key;
  if DefaultLeft = 0 then DefLeft := Form.Left else DefLeft := DefaultLeft;
  if DefaultTop = 0 then DefTop := Form.Top else DefTop := DefaultTop;
  if ResembleString(ReadString(_key + 'SizePos' + ResolutionSufix, '')) > 3 then
  begin
    Width := StrToIntDef(Parts[0], Form.Width);
    Height := StrToIntDef(Parts[1], Form.Height);
    Left := StrToIntDef(Parts[2], DefLeft);
    Top := StrToIntDef(Parts[3], DefTop);
    W := Screen.DesktopWidth - SafeRange;
    H := Screen.DesktopHeight - SafeRange;
    if Left > W then Left := W
    else if Left < SafeRange-Width then Left := SafeRange;
    if Top > H then Top := H
    else if Top < SafeRange-Height then Top := SafeRange;
    Form.SetBounds(Left, Top, Width, Height);
  end
  else Form.SetBounds(DefLeft, DefTop, Form.Width, Form.Height);
end;
{%endregion}

initialization
  GetIniName := @GetIniFileName;
  IsPortable := FileExistsUTF8(GetLocalIniFileName);

end.

