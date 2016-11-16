unit OriDialogs;

{$mode objfpc}{$H+}

{$ifndef WINDOWS}
  {$define CORRECT_COLORS} // for Gtk widgetset
{$endif}

interface

uses
  Classes, SysUtils, Forms, FGL;

type
  TOriDialog = class(TForm)
  protected
    procedure DoFirstShow; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
    function CanClose: Boolean; virtual;
    procedure Populate; virtual;
    procedure Collect; virtual;
    {$ifdef CORRECT_COLORS}
    procedure CorrectColors;
    {$endif}
  public
    constructor Create; reintroduce;
    function CloseQuery: Boolean; override;
    function Accepted: Boolean;
    function Exec: Boolean;
	end;

function ShowDialog(ADialog: TOriDialog): Boolean;

implementation

uses
  Controls,
  {$ifdef CORRECT_COLORS} ButtonPanel, Graphics, {$endif}
  OriUtils_Gui;

type
  TDialogPropsMap = specialize TFPGMap<string, Longword>;
  TDialogProps = class
  private
    FSize: TDialogPropsMap;
    FPos: TDialogPropsMap;
    function GetProp(AMap: TDialogPropsMap; const AKey: String): Longword;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Store(ADlg: TOriDialog);
    procedure Restore(ADlg: TOriDialog);
	end;

var
  SavedProps: TDialogProps;

{%region TDialogProps}
constructor TDialogProps.Create;
begin
  FSize := TDialogPropsMap.Create;
  FPos := TDialogPropsMap.Create;
end;

destructor TDialogProps.Destroy;
begin
  FSize.Free;
  FPos.Free;
  inherited;
end;

function TDialogProps.GetProp(AMap: TDialogPropsMap; const AKey: String): Longword;
begin
  if AMap.IndexOf(AKey) >= 0
    then Result := AMap.KeyData[AKey]
    else Result := 0;
end;

procedure TDialogProps.Store(ADlg: TOriDialog);
var
  Size, Pos: Longword;
begin
  Size := 0; Pos := 0; // warning supress
  SaveFormSizePos(ADlg, Size, Pos);
  FSize.KeyData[ADlg.ClassName] := Size;
  FPos.KeyData[ADlg.ClassName] := Pos;
end;

procedure TDialogProps.Restore(ADlg: TOriDialog);
begin
  RestoreFormSizePos(ADlg,
    GetProp(FSize, ADlg.ClassName),
    GetProp(FPos, ADlg.ClassName));
end;
{%endregion}

{%region TOriDialog}
function ShowDialog(ADialog: TOriDialog): Boolean;
begin
  Result := ADialog.ShowModal = mrOk;
end;

constructor TOriDialog.Create;
begin
  inherited Create(Application.MainForm);
  BorderIcons := [biSystemMenu];
  PopupMode := pmAuto;
end;

function TOriDialog.Accepted: Boolean;
begin
  Result := ModalResult = mrOk;
end;

function TOriDialog.Exec: Boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TOriDialog.DoFirstShow;
begin
  inherited;
  Populate;
  {$ifdef CORRECT_COLORS}
  CorrectColors;
  {$endif}
  SavedProps.Restore(Self);
end;

procedure TOriDialog.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);

  if Accepted then Collect;

  SavedProps.Store(Self);
  CloseAction := caFree;
end;

function TOriDialog.CloseQuery: Boolean;
begin
  Result := inherited;
  if Result and Accepted then
    Result := CanClose;
end;

procedure TOriDialog.Populate;
begin
end;

procedure TOriDialog.Collect;
begin
end;

function TOriDialog.CanClose: Boolean;
begin
  Result := True;
end;

{$ifdef CORRECT_COLORS}
procedure TOriDialog.CorrectColors;
var I: Integer;
begin
  for I := 0 to ControlCount-1 do
    if Controls[I] is TCustomButtonPanel then
    begin
      Controls[I].Color := clForm;
      break;
    end;
end;
{$endif}
{%endregion}

initialization
  SavedProps := TDialogProps.Create;

finalization
  SavedProps.Free;
end.

