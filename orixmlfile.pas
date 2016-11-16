unit OriXmlFile;

{$mode objfpc}{$H+}
{-$define TRACE_READING}

interface

uses
  SysUtils, DOM, FGL;

type
  TOriXmlFile = class
  protected
    FDoc: TXMLDocument;
    FNode: TDOMNode;
    FFormat: TFormatSettings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close; virtual;
  end;

  TOriXmlFileWriter = class(TOriXmlFile)
  private
    FFileName: String;
    procedure SetAttribute(const AName, AValue: String);
    procedure SetBoolAttribute(const AName: String; AValue: Boolean);
    procedure SetTextNode(const AName, AValue: String);
    procedure SetValueNode(const AName: String; const AValue: Double);
  public
    constructor Create(const AFileName: String); reintroduce;
    destructor Destroy; override;
    procedure Open(const AName: String);
    property Attribute[const AName: String]: String write SetAttribute;
    property BoolAttribute[const AName: String]: Boolean write SetBoolAttribute;
    property Text[const AName: String]: String write SetTextNode;
    property Value[const AName: String]: Double write SetValueNode;
  end;

  TNodeListers = specialize TFPGMap<DOMString, TDOMNode>;

  TOriXmlFileReader = class(TOriXmlFile)
  private
    FListers: TNodeListers;
    FListed: TDOMNode;
    function GetCurrentNode: TDOMNode;
    function GetAttribute(const AName: String): String;
    function GetBoolAttribute(const AName: String): Boolean;
    function GetTextNode(const AName: String): String;
    function GetValueNode(const AName: String): Double;
    function GetListing(const AName: DOMString): TDOMNode;
    procedure RemoveListing(const AName: DOMString);
    procedure AppendListing(const AName: DOMString; ANode: TDOMNode);
  public
    constructor Create(const AFileName: String); reintroduce;
    destructor Destroy; override;
    procedure Open(const AName: String);
    function TryOpen(const AName: String): Boolean;
    procedure Close; override;
    function List(const AName: String): Boolean;
    function HasNode(const AName: String): Boolean;
    property Attribute[const AName: String]: String read GetAttribute;
    property BoolAttribute[const AName: String]: Boolean read GetBoolAttribute;
    property Text[const AName: String]: String read GetTextNode;
    property Value[const AName: String]: Double read GetValueNode;
    property CurrentNode: TDOMNode read GetCurrentNode;
  end;

  EOriXmlFile = class(Exception)
  end;

implementation

uses
{$ifdef TRACE_READING}
  OriDebugConsole, StrUtils,
{$endif}
  Math, LazUTF8, XMLRead, XMLWrite;

resourcestring
  SFileNotFound = 'File "%s" does not exist.';
  SNodeNotFound = 'Node "%s" not found in the parent "%s".';
  SNotTextNode = '"%s" is not a text node.';

{%region Helpers}
function NotNull(AFirst, ASecond: TDOMNode): TDOMNode; inline;
begin
  if Assigned(AFirst) then Result := AFirst else Result := ASecond;
end;

function GetPath(ANode: TDOMNode): DOMString;
var node: TDOMNode;
begin
  Result := '';
  node := ANode;
  repeat
    Result := '\' + node.NodeName + Result;
    node := node.ParentNode;
  until not Assigned(node) or (node.NodeType = DOCUMENT_NODE);
end;

function NodeToStr(ANode: TDOMNode): String;
begin
  Result := Format('%s %d %s', [ANode.NodeName, ANode.NodeType, ANode.NodeValue]);
end;

{$ifdef TRACE_READING}
procedure Trace(const Msg: String); overload;
begin
  DebugPrint(Msg);
end;

procedure Trace(const Msg: String; Args: array of const); overload;
begin
  DebugPrint(Msg, Args);
end;
{$endif}
{%endregion}

{%region TOriXmlFile}
constructor TOriXmlFile.Create;
begin
  FFormat := DefaultFormatSettings;
  FFormat.DecimalSeparator := '.';
  FFormat.ThousandSeparator := #0;
end;

destructor TOriXmlFile.Destroy;
begin
  FDoc.Free;
end;

procedure TOriXmlFile.Close;
begin
  FNode := FNode.ParentNode;
  if FNode = FDoc then FNode := nil;
end;
{%endregion}

{%region TOriXmlFileWriter}
constructor TOriXmlFileWriter.Create(const AFileName: String);
begin
  inherited Create;
  FFileName := AFileName;
  FDoc := TXMLDocument.Create;
end;

destructor TOriXmlFileWriter.Destroy;
begin
  if FFileName <> '' then
  try
    WriteXMLFile(FDoc, UTF8ToSys(FFileName));
  finally
    inherited;
  end;
end;

procedure TOriXmlFileWriter.Open(const AName: String);
begin
  FNode := NotNull(FNode, FDoc).AppendChild(FDoc.CreateElement(UTF8ToUTF16(AName)));
end;

procedure TOriXmlFileWriter.SetAttribute(const AName, AValue: String);
var attr: TDOMAttr;
begin
  if Assigned(FNode) then
  begin
    attr := FDoc.CreateAttribute(UTF8ToUTF16(AName));
    attr.NodeValue := UTF8ToUTF16(AValue);
    FNode.Attributes.SetNamedItem(attr);
  end;
end;

procedure TOriXmlFileWriter.SetBoolAttribute(const AName: String; AValue: Boolean);
begin
  SetAttribute(AName, BoolToStr(AValue, True));
end;

procedure TOriXmlFileWriter.SetTextNode(const AName, AValue: String);
begin
  if Assigned(FNode) then
    FNode.AppendChild(FDoc.CreateElement(UTF8ToUTF16(AName))).TextContent := UTF8ToUTF16(AValue);
end;

procedure TOriXmlFileWriter.SetValueNode(const AName: String; const AValue: Double);
var S: String;
begin
  if IsNaN(AValue) then S := '' else S := FloatToStr(AValue, FFormat);
  SetTextNode(AName, S);
end;
{%endregion}

{%region TOriXmlFileReader}
constructor TOriXmlFileReader.Create(const AFileName: String);
begin
  inherited Create;
  if not FileExists(UTF8ToSys(AFileName)) then
    raise EOriXmlFile.CreateFmt(SFileNotFound, [AFileName]);
  ReadXMLFile(FDoc, UTF8ToSys(AFileName));
  FListers := TNodeListers.Create;
end;

destructor TOriXmlFileReader.Destroy;
begin
  FListers.Free;
  inherited;
end;

function TOriXmlFileReader.GetCurrentNode: TDOMNode;
begin
  Result := NotNull(FListed, NotNull(FNode, FDoc));
end;

procedure TOriXmlFileReader.Open(const AName: String);
var node: TDOMNode;
begin
  node := CurrentNode.FindNode(UTF8ToUTF16(AName));
  if not Assigned(node) then
    raise EOriXmlFile.CreateFmt(SNodeNotFound, [AName, GetPath(CurrentNode)]);
  FNode := node;
  FListed := nil;
{$ifdef TRACE_READING}
  Trace('OPEN: %s (%s)', [AName, GetPath(CurrentNode)]);
{$endif}
end;

function TOriXmlFileReader.TryOpen(const AName: String): Boolean;
var node: TDOMNode;
begin
  node := CurrentNode.FindNode(UTF8ToUTF16(AName));
  if not Assigned(node) then Exit(False);
  FNode := node;
  FListed := nil;
  Result := True;
{$ifdef TRACE_READING}
  Trace('OPEN: %s (%s)', [AName, GetPath(CurrentNode)]);
{$endif}
end;

procedure TOriXmlFileReader.Close;
begin
{$ifdef TRACE_READING}
  Trace('CLOSE: ' + GetPath(CurrentNode));
{$endif}
  if Assigned(FListed) then
  begin
    RemoveListing(FListed.NodeName);
    FListed := nil
  end
  else inherited;
  // Previously opened node could be obtained as result of list()
  // but not of open() method, so return previous listing state.
  FListed := GetListing(GetPath(CurrentNode));
  if Assigned(FListed) then FNode := FListed.ParentNode;
{$ifdef TRACE_READING}
  Trace('Current: ' + GetPath(CurrentNode));
{$endif}
end;

function TOriXmlFileReader.GetListing(const AName: DOMString): TDOMNode;
var index: Integer;
begin
  index := FListers.IndexOf(AName);
  if index >= 0 then Result := FListers.Data[index] else Result := nil;
{$ifdef TRACE_READING}
  Trace('Listing %sfound: %s', [IfThen(index < 0, 'not '), AName]);
{$endif}
end;

procedure TOriXmlFileReader.RemoveListing(const AName: DOMString);
var index: Integer;
begin
  index := FListers.IndexOf(AName);
  if index > -1 then FListers.Delete(index);
{$ifdef TRACE_READING}
  Trace('Listing %sremoved: %s', [IfThen(index < 0, 'not found to be '), AName]);
{$endif}
end;

procedure TOriXmlFileReader.AppendListing(const AName: DOMString; ANode: TDOMNode);
begin
{$ifdef TRACE_READING}
  Trace('Listing %s: %s', [IfThen(FListers.IndexOf(AName) < 0, 'added', 'updated'), AName]);
{$endif}
  FListers[AName] := ANode;
end;

function TOriXmlFileReader.List(const AName: String): Boolean;
var
  node: TDOMNode;
  key: DOMString;
begin
{$ifdef TRACE_READING}
  Trace('LIST: ' + AName);
  Trace('Current: ' + GetPath(CurrentNode));
{$endif}
  key := GetPath(NotNull(FNode, FDoc)) + '\' + UTF8ToUTF16(AName);
  node := GetListing(key);
  if Assigned(node) then
  begin
    FListed := node.NextSibling;
    if Assigned(FListed)
      then AppendListing(key, FListed)
      else RemoveListing(key);
  end
  else
  begin
    FListed := CurrentNode.FindNode(UTF8ToUTF16(AName));
    if Assigned(FListed) then
      AppendListing(key, FListed)
  end;
  Result := Assigned(FListed);
end;

function TOriXmlFileReader.GetAttribute(const AName: String): String;
var node: TDOMNode;
begin
  node := CurrentNode.Attributes.GetNamedItem(UTF8ToUTF16(AName));
  if Assigned(node)
    then Result := UTF16ToUTF8(node.NodeValue)
    else Result := '';
end;

function TOriXmlFileReader.GetBoolAttribute(const AName: String): Boolean;
begin
  if not TryStrToBool(GetAttribute(AName), Result) then Result := False;
end;

function TOriXmlFileReader.GetTextNode(const AName: String): String;
var node, txt: TDOMNode;
begin
  node := CurrentNode.FindNode(UTF8ToUTF16(AName));
  if not Assigned(node) then
    raise EOriXmlFile.CreateFmt(SNodeNotFound, [AName, GetPath(CurrentNode)]);
  txt := node.FirstChild;
  if not Assigned(txt) then Exit(''); // case: <node/>
  if txt.NodeType <> TEXT_NODE then
    raise EOriXmlFile.CreateFmt(SNotTextNode, [GetPath(node)]);
  Result := UTF8Encode(txt.NodeValue);
end;

function TOriXmlFileReader.GetValueNode(const AName: String): Double;
var S: String;
begin
  S := Trim(GetTextNode(AName));
  if S = '' then Result := NaN else Result := StrToFloat(S, FFormat);
end;

function TOriXmlFileReader.HasNode(const AName: String): Boolean;
begin
  Result := Assigned(CurrentNode.FindNode(UTF8ToUTF16(AName)));
end;
{%endregion}

end.

