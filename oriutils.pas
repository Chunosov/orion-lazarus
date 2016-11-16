unit OriUtils;

interface

uses
  FGL;

type
  TIntegerList = specialize TFPGList<Integer>;

procedure Unused(const UnusedObject);

{%region Paths and FileNames}
function EnsurePath(const APath: String): String;
function ExtractFileExtNoDot(const FileName: String): String;
{%endregion}

{%region Log}
procedure WriteLogString(const LogString: String; Params: array of const); overload;
procedure WriteLogString(const FileName: String; const LogString: String; Params: array of const); overload;
procedure WriteLogString(const FileName: String; const LogString: String); overload;
{%endregion}

{%region Containers}
procedure FreeAndNilList(var AList: TFPSList);
procedure FreeAndClearList(AList: TFPSList);
{%endregion}

implementation

uses
  SysUtils, LazFileUtils, LazUTF8;

procedure Unused(const UnusedObject);
begin
end;

{%region Log}
procedure WriteLogString(const LogString: String; Params: array of const);
{$ifndef ORI_DISABLE_LOG}
var
  FileName: String;
{$endif}
begin
{$ifndef ORI_DISABLE_LOG}
  FileName := ChangeFileExt(ParamStrUTF8(0), '.log');
  WriteLogString(FileName, Format(LogString, Params));
{$endif}
end;

procedure WriteLogString(const FileName: String; const LogString: String; Params: array of const);
begin
{$ifndef ORI_DISABLE_LOG}
  WriteLogString(FileName, Format(LogString, Params));
{$endif}
end;

procedure WriteLogString(const FileName: String; const LogString: String);
{$ifndef ORI_DISABLE_LOG}
var
  FileOut: TextFile;
{$endif}
begin
{$ifndef ORI_DISABLE_LOG}
  if not FileExistsUTF8(FileName) then
    FileClose(FileCreateUTF8(FileName));
  AssignFile(FileOut, UTF8ToSys(FileName));
  Append(FileOut);
  WriteLn(FileOut, Format('%s : %s', [
    FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', Now), LogString]));
  Flush(FileOut);
  CloseFile(FileOut);
{$endif}
end;
{%endregion}

{%region Paths and FileNames}
// Procedure checks if a path exists and substitutes an existed if not.
function EnsurePath(const APath: String): String;
begin
  if (APath = '') or not (DirectoryExistsUTF8(APath))
    then Result := ExtractFilePath(ParamStrUTF8(0))
    else Result := APath;
end;

function ExtractFileExtNoDot(const FileName: String): String;
begin
  Result := Copy(ExtractFileExt(FileName), 2, MaxInt);
end;
{%endregion}

{%region Containers}
procedure FreeAndNilList(var AList: TFPSList);
var I: Integer;
begin
  if Assigned(AList) then
  begin
    for I := 0 to AList.Count-1 do
      TObject(AList[I]^).Free;
    FreeAndNil(AList);
  end;
end;

procedure FreeAndClearList(AList: TFPSList);
var I: Integer;
begin
  if Assigned(AList) then
  begin
    for I := 0 to AList.Count-1 do
      TObject(AList[I]^).Free;
    AList.Clear;
  end;
end;
{%endregion}

end.