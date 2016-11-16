{*******************************************************************************

  Orion VCL Package
  Chunosov N.I. (c) 2006-2009. Contact: rezonator_info at mail.ru
  Copyrights on some code used as base for this package belogns to their owners.

  OriButtons
  Set of different button controls.

  An idea of TDialogPanel component was taken from free-pascal Lazarus IDE

2009/02/13: TBXColorItem and mates was removed to TBX2000 package
2009/05/16: + TOriDialogPanel option dpoSizeGrip added
2009/08/26: + TOriDialogPanel option dpoSeparator added

*******************************************************************************}

{
  If DK Language Manager is used ($ifdef USE_DKLANG)
  these constants should be added in proect constants:
		OriDialog_Apply=Apply
		OriDialog_Cancel=Cancel
		OriDialog_Help=Help
		OriDialog_OK=OK
}

unit OriButtons;

interface

uses
  Windows, SysUtils, Types, Classes, Messages, Controls, Graphics, ImgList,
  Menus, StdCtrls, Forms, LCLType,
  OriControlsBase//, OriUtils
  ;

type

  { TOriCustomButton }

  TOriButtonOption = (boAllowAllUnchecked, boNoDrawFocus, boNoAutoDropDownMenu,
    boDropDownAlignRight, boDropDownArrow, boDropDownAsEllipsis, boDropDownHoriz);
  TOriButtonOptions = set of TOriButtonOption;
  TOriButtonLook = (blDefault, blDefaultSys, blFlat, blSoft, blTransparent,
    blTransparentSys);

  TOriCustomButton = class(TButton)
  private
    IFocused: Boolean;
    IMouseOver: Boolean;
    FCanvas: TCanvas;
    FChecked: Boolean;
    FPaintInfo: TButtonPaintInfo;
    FGroupIndex: Integer;
    FOptions: TOriButtonOptions;
    FLook: TOriButtonLook;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FImageChangeLink: TChangeLink;
    FDropDownMenu: TPopupMenu;
    procedure SetOptions(Value: TOriButtonOptions);
    procedure SetLook(Value: TOriButtonLook);
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: TImageIndex);
    procedure ImageListChange(Sender: TObject);
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetButtonStyle(ADefault: Boolean);
    procedure SetChecked(Value: Boolean); override;
    function GetChecked: Boolean; override;
    procedure PaintShape; virtual;
    procedure PaintImage; virtual;
    procedure PaintText; virtual;
    procedure PaintFocus; virtual;
    procedure PaintDropDownArrow; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
  published
    property GroupIndex: Integer read FGroupIndex write FGroupIndex default 0;
    property Options: TOriButtonOptions read FOptions write SetOptions default [];
    property Look: TOriButtonLook read FLook write SetLook default blDefault;
    property Action;
    property Anchors;
    property BiDiMode;
    property Cancel;
    property Checked;
    property Constraints;
    property Default;
    property Enabled;
    property ModalResult;
    property ParentShowHint;
    property ParentBiDiMode;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property OnExit;     
  end;

  TOriImgButton = class(TOriCustomButton)
  private
    FMonoGlyph: TOriButtonMonoGlyph;
    procedure SetMonoGlyph(Value: TOriButtonMonoGlyph);
  protected
    procedure PaintImage; override;
    procedure PaintText; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MonoGlyph: TOriButtonMonoGlyph read FMonoGlyph write SetMonoGlyph default bgNone;
    property DropDownMenu;
    property Images;
    property ImageIndex;
    property Height default 25;
    property Width default 25;
  end;

  { TOriButton }

  TOriButton = class(TOriCustomButton)
  published
    property DropDownMenu;
    property Images;
    property ImageIndex;
    property Caption;
  end;

  { TOriRadioCheckBox }

  TOriRadioCheckBoxOption = (rcoRadioButton, rcoRadioAsCheck);
  TOriRadioCheckBoxOptions = set of TOriRadioCheckBoxOption;
  TOriRadioCheckBoxLook = (rclDefault, rclFlat);

  TOriRadioCheckPaintText = procedure(Sender: TObject; Canvas: TCanvas;
    const AText: String; var ARect: TRect; Flags: Cardinal;
    var PaintDefault: Boolean) of object;

  TOriRadioCheckBox = class(TButtonControl)
  private
    _MouseOver: Boolean;
    FCanvas: TCanvas;
    FLayout: TTextLayout;
    FAlignment: TAlignment;
    FCheckPosition: TLeftRight;
    FAutoSize: Boolean;
    FGroupIndex: Integer;
    FState: TCheckBoxState;
    FAllowGrayed: Boolean;
    FLook: TOriRadioCheckBoxLook;
    FOptions: TOriRadioCheckBoxOptions;
    FOnPaintText: TOriRadioCheckPaintText;
    procedure SetLook(Value: TOriRadioCheckBoxLook);
    procedure SetOptions(Value: TOriRadioCheckBoxOptions);
    procedure SetCheckPosition(Value: TLeftRight);
    procedure SetLayout(Value: TTextLayout);
    procedure SetAlignment(Value: TAlignment);
    procedure SetState(Value: TCheckBoxState);
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure AdjustBounds;
    procedure UncheckSiblings;
    procedure DoDrawText(var Rect: TRect; Flags: Longint);
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNCtlColorBtn(var Message: TWMCtlColorBtn); message CN_CTLCOLORBTN;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure SetAutoSize(Value: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Toggle;
    procedure Loaded; override;
    procedure Click; override;
    procedure SetChecked(Value: Boolean); override;
    function GetChecked: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ClicksDisabled;
  published
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    property CheckPosition: TLeftRight read FCheckPosition write SetCheckPosition default taLeftJustify;
    property Look: TOriRadioCheckBoxLook read FLook write SetLook default rclDefault;
    property Options: TOriRadioCheckBoxOptions read FOptions write SetOptions default [];
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property OnPaintText: TOriRadioCheckPaintText read FOnPaintText write FOnPaintText;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex default 0;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Align;
    property Action;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TOriDialogPanel }

  TDialogButton = (dbutOk, dbutCancel, dbutApply, dbutHelp);
  TDialogButtons = set of TDialogButton;
  TDialogPanelOption = (dpoSizeGrip, dpoSeparator, dpoHelpLabel, dpoHelpLabelOpposite,
    dpoCatchF1);
  TDialogPanelOptions = set of TDialogPanelOption;

  TOriDlgButton = TButton;

  TOriDialogPanel = class(TCustomControl)
  private
    FButtonOk: TOriDlgButton;
    FButtonCancel: TOriDlgButton;
    FButtonApply: TOriDlgButton;
    FButtonHelp: TOriDlgButton;
    FHorzMargin: Integer;
    FVertMargin: Integer;
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FButtonSpace: Integer;
    FButtons: TDialogButtons;
    FOnClickOK: TNotifyEvent;
    FOnClickCancel: TNotifyEvent;
    FOnClickApply: TNotifyEvent;
    FOnClickHelp: TNotifyEvent;
    FOptions: TDialogPanelOptions;
    FHelpLabel: TLabel;
    FHelpTopic: String;
    procedure SetSize(Index: Integer; Value: Integer);
    procedure SetButtons(Value: TDialogButtons);
    procedure SetEvent(Index: Integer; Value: TNotifyEvent);
    procedure SetOptions(const Value: TDialogPanelOptions);
    procedure PaintDesignTime;
    procedure PaintSizeGrip;
    procedure PaintSeparator;
    procedure RemoveButtons;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure RecreateButtons;
    procedure RecreateHelpLabel;
    procedure ArrangeButtons; virtual;
    function GetButtonCaption(Value: TDialogButton): String; virtual;
    procedure InternalHelpClick(Sender: TObject);
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ButtonOk: TOriDlgButton read FButtonOk;
    property ButtonCancel: TOriDlgButton read FButtonCancel;
    property ButtonApply: TOriDlgButton read FButtonApply;
    property ButtonHelp: TOriDlgButton read FButtonHelp;
  published
    property HelpTopic: String read FHelpTopic write FHelpTopic;
    property HorzMargin: Integer index 0 read FHorzMargin write SetSize default 6;
    property VertMargin: Integer index 1 read FVertMargin write SetSize default 6;
    property ButtonWidth: Integer index 2 read FButtonWidth write SetSize default 75;
    property ButtonHeight: Integer index 3 read FButtonHeight write SetSize default 25;
    property ButtonSpace: Integer index 4 read FButtonSpace write SetSize default 6;
    property Buttons: TDialogButtons read FButtons write SetButtons default [dbutOk, dbutCancel];
    property Options: TDialogPanelOptions read FOptions write SetOptions default [];
    property OnClickOK: TNotifyEvent index 0 read FOnClickOK write SetEvent;
    property OnClickCancel: TNotifyEvent index 1 read FOnClickCancel write SetEvent;
    property OnClickApply: TNotifyEvent index 2 read FOnClickApply write SetEvent;
    property OnClickHelp: TNotifyEvent index 3 read FOnClickHelp write SetEvent;
    //property Align; TODO, see ArrangeButtons
    property BiDiMode;
    property BorderWidth;
    property Color default clBtnFace;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

{$ifdef USE_TBX}
type
  TTBXComboItem = class(TTBXItem)
  protected
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  end;

  // нужен только чтобы подогнать ширину меню к ширине кнопки
  TTBXComboItemViewer = class(TTBXItemViewer)
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer); override;
  end;

  TOriComboButton = class(TOriCustomButton)
  private
    FItemIndex: Integer;
    FOnChange: TNotifyEvent;
    procedure ItemClick(Sender: TObject);
    procedure SetItemIndex(Value: Integer);
    function GetItemsCount: Integer;

  protected
    procedure DoChange;

  public
    constructor Create(AOwner: TComponent); override;

    procedure AddItem(const Value: TOriString; Tag: Integer = 0);

    property ItemsCount: Integer read GetItemsCount;

  published
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Options default [boDropDownArrow];
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TOriBrushButton = class;

  TOriSelectPenBrush = (bstBrush, bstPen);

  TOriColorButton = class(TOriCustomButton)
  private
    FItemRoot: TTBXPopupMenu;
    FShowCaption: Boolean;
    FColorSelector: TTBXOriColorSelector;
    FColorPosition: TLeftRight;
    procedure SetShowCaption(Value: Boolean);
    procedure SetColorPosition(Value: TLeftRight);
    procedure SetSelectedColor(Value: TColor);
    function GetSelectedColor: TColor;
    procedure SetOnColorChange(Value: TNotifyEvent);
    function GetOnColorChange: TNotifyEvent;
  protected
    procedure DoPopup(Sender: TObject);
    procedure PaintText; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
  published
    property ColorSelector: TTBXOriColorSelector read FColorSelector write FColorSelector;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default False;
    property ColorPosition: TLeftRight read FColorPosition write SetColorPosition default taLeftJustify;
    property OnColorChange: TNotifyEvent read GetOnColorChange write SetOnColorChange;
    property Caption;
    property Options default [boDropDownArrow];
  end;

  TOriBrushButton = class(TOriCustomButton)
  private
    FButtonStyle: TOriSelectPenBrush;
    FPenStyle: TPenStyle;
    FBrushStyle: TBrushStyle;
    FSmallDots: Boolean; // for TeeChartPen
    FUseSmallDots: Boolean; // for TeeChartPen
    FItemRoot: TTBXPopupMenu;
    FOnStyleChanged: TNotifyEvent;
    FImageList: TCustomImageList;
    FPopupWidth: Integer;
  protected
    procedure SetPenStyle(Value: TPenStyle);
    procedure SetBrushStyle(Value: TBrushStyle);
    procedure SetSmallDots(Value: Boolean);
    procedure ItemPenClick(Sender: TObject);
    procedure ItemBrushClick(Sender: TObject);
    procedure DoPopup(Sender: TObject);
    procedure DrawPenImage(Item: TTBCustomItem; Viewer: TTBItemViewer;
      Canvas: TCanvas; ImageRect: TRect; ImageOffset: TPoint; StateFlags: Integer);
    procedure DrawBrushImage(Item: TTBCustomItem; Viewer: TTBItemViewer;
      Canvas: TCanvas; ImageRect: TRect; ImageOffset: TPoint; StateFlags: Integer);
    procedure PaintText; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PenStyle: TPenStyle read FPenStyle write SetPenStyle default psSolid;
    property BrushStyle: TBrushStyle read FBrushStyle write SetBrushStyle default bsSolid;
    property SmallDots: Boolean read FSmallDots write SetSmallDots default False;
    property OnStyleChanged: TNotifyEvent read FOnStyleChanged write FOnStyleChanged;
    property UseSmallDots: Boolean read FUseSmallDots write FUseSmallDots default False;   // for design-time (or before first click)
    property ButtonStyle: TOriSelectPenBrush read FButtonStyle write FButtonStyle nodefault; // for design-time (or before first click)
    property PopupWidth: Integer read FPopupWidth write FPopupWidth nodefault;             // for design-time (or before first click)
  end;
{$endif}

{$ifdef USE_DKLANG}
{$ifdef UNIT_TEST}
procedure UnitTest_OriButtons_DKLang;
{$endif}
{$endif}


implementation

uses
  Themes, Math, Dialogs
  {$ifdef USE_DKLANG}, DKLang{$endif}
  {$ifdef USE_TBX}, TBXUtils {$ifdef TBX_22A_UP}, TBXGraphics{$endif}{$endif};

{$ifdef USE_DKLANG}
{$ifdef UNIT_TEST}
// Проверяет наличие всех DKLang-констант, необходимых для работы модуля.
procedure UnitTest_OriButtons_DKLang;
begin
  LangManager.ConstantValue['OriDialog_OK'];
  LangManager.ConstantValue['OriDialog_Cancel'];
  LangManager.ConstantValue['OriDialog_Apply'];
  LangManager.ConstantValue['OriDialog_Help'];
end;
{$endif}
{$endif}

//------------------------------------------------------------------------------
{ TOriCustomButton }

constructor TOriCustomButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReflector];
  DoubleBuffered := True;
  FCanvas := TCanvas.Create;

  FLook := blDefault;
  FImageIndex := -1;
  FImages := nil;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
end;

destructor TOriCustomButton.Destroy;
begin
  FCanvas.Free;
  FImageChangeLink.Free;
  inherited;
end;

procedure TOriCustomButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end;

procedure TOriCustomButton.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IFocused then
  begin
    IFocused := ADefault;
    Refresh;
  end;
end;

procedure TOriCustomButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TOriCustomButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;
    
procedure TOriCustomButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;

procedure TOriCustomButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  case FLook of
    blDefault:
      if IsThemesEnabled and not IMouseOver then
      begin
        IMouseOver := True;
        Repaint;
      end;
    blTransparent, blTransparentSys:
      if not IMouseOver then
      begin
        IMouseOver := True;
        Repaint;
      end;
  end;
end;

procedure TOriCustomButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  case FLook of
    blDefault:
      if IsThemesEnabled and IMouseOver then
      begin
        IMouseOver := False;
        Repaint;
      end;
    blTransparent, blTransparentSys:
      if IMouseOver then
      begin
        IMouseOver := False;
        Repaint;
      end;
  end;
end;

procedure TOriCustomButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;

procedure TOriCustomButton.CNDrawItem(var Message: TWMDrawItem);
begin
  with Message.DrawItemStruct^ do
  begin
    FCanvas.Handle := hDC;
    FCanvas.Font.Assign(Self.Font);

    FPaintInfo.Canvas := FCanvas;
    FPaintInfo.Parent := Self.Parent;
    FPaintInfo.Window := Self.Handle;
    FPaintInfo.Rect := ClientRect;
    FPaintInfo.Pressed := itemState and ODS_SELECTED <> 0;
    FPaintInfo.Default := itemState and ODS_FOCUS <> 0;
    FPaintInfo.Disabled := itemState and ODS_DISABLED <> 0;
    FPaintInfo.Focused := IFocused;
    FPaintInfo.Hover := IMouseOver;
    FPaintInfo.Checked := FChecked;
  end;

  PaintShape;

  if boDropDownArrow in FOptions then PaintDropDownArrow;

  PaintImage;

  PaintText;

  with FPaintInfo do
    if Default and Focused and not (boNoDrawFocus in FOptions) then
      PaintFocus;

  FPaintInfo.Canvas.Handle := 0;
end;

procedure TOriCustomButton.PaintShape;
begin
  case FLook of
    blDefault:
      if IsThemesEnabled
        then PaintButtonShape_Themed(FPaintInfo)
        else PaintButtonShape_Default(FPaintInfo);
    blDefaultSys:
      if IsThemesEnabled
        then PaintButtonShape_ThemedSys(FPaintInfo)
        else PaintButtonShape_DefaultSys(FPaintInfo);
    blFlat: PaintButtonShape_Flat(FPaintInfo);
    blSoft: PaintButtonShape_Soft(FPaintInfo);
    blTransparent: PaintButtonShape_Transparent(FPaintInfo);
    blTransparentSys: PaintButtonShape_TransparentSys(FPaintInfo);
  end;
end;

procedure TOriCustomButton.PaintFocus;
begin
  case FLook of
    blDefault: PaintButtonFocus_Default(FPaintInfo);
    blFlat: PaintButtonFocus_Flat(FPaintInfo);
    blSoft: PaintButtonFocus_Default(FPaintInfo);
    blTransparent, blTransparentSys: PaintButtonFocus_Default(FPaintInfo);
  end;
end;

procedure TOriCustomButton.PaintImage;
begin
  if (FImages <> nil) and (FImageIndex > -1)
    and (FImageIndex < FImages.Count) then
    with FPaintInfo, FPaintInfo.Rect do
    begin
      FImages.Draw(Canvas,
        Left + 4 + Ord(Pressed),
        (Top + Bottom - FImages.Height) div 2 + Ord(Pressed),
        FImageIndex, Enabled);
      Left := Left + FImages.Width + 6;
    end;
end;

procedure TOriCustomButton.PaintText;
var
  Flags: Cardinal;
  Text: TOriString;
begin
  Text := Caption;
  Flags := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
  FCanvas.Brush.Style := bsClear;
  if Enabled
    then FCanvas.Font.Color := clBtnText
    else FCanvas.Font.Color := clBtnShadow;
  DrawText(FCanvas.Handle, POriChar(Text), Length(Text), FPaintInfo.Rect, DrawTextBiDiModeFlags(Flags));
end;

procedure TOriCustomButton.PaintDropDownArrow;
var
  C: TColor;

  procedure PaintDropDownArrowNormal(X, Y: Integer);
  begin
    with FCanvas do
    begin
      Pixels[X, Y] := C;
      Pixels[X+1, Y] := C;
      Pixels[X+2, Y] := C;
      Pixels[X+3, Y] := C;
      Pixels[X+4, Y] := C;
      Pixels[X+5, Y] := C;
      Pixels[X+6, Y] := C;

      Pixels[X+1, Y+1] := C;
      Pixels[X+2, Y+1] := C;
      Pixels[X+3, Y+1] := C;
      Pixels[X+4, Y+1] := C;
      Pixels[X+5, Y+1] := C;

      Pixels[X+2, Y+2] := C;
      Pixels[X+3, Y+2] := C;
      Pixels[X+4, Y+2] := C;

      Pixels[X+3, Y+3] := C;
    end;
  end;

  procedure PaintDropDownArrowHoriz(X, Y: Integer);
  begin
    with FCanvas do
    begin
      Pixels[X, Y] := C;
      Pixels[X, Y+1] := C;
      Pixels[X, Y+2] := C;
      Pixels[X, Y+3] := C;
      Pixels[X, Y+4] := C;
      Pixels[X, Y+5] := C;
      Pixels[X, Y+6] := C;

      Pixels[X+1, Y+1] := C;
      Pixels[X+1, Y+2] := C;
      Pixels[X+1, Y+3] := C;
      Pixels[X+1, Y+4] := C;
      Pixels[X+1, Y+5] := C;

      Pixels[X+2, Y+2] := C;
      Pixels[X+2, Y+3] := C;
      Pixels[X+2, Y+4] := C;

      Pixels[X+3, Y+3] := C;
    end;
  end;

  procedure PaintDropDownArrowEllipsis(X, Y: Integer);
  begin
    with FCanvas do
    begin
      Pixels[X, Y+1] := C;  Pixels[X+1, Y+1] := C;
      Pixels[X, Y+2] := C;  Pixels[X+1, Y+2] := C;

      Pixels[X+3, Y+1] := C;  Pixels[X+4, Y+1] := C;
      Pixels[X+3, Y+2] := C;  Pixels[X+4, Y+2] := C;

      Pixels[X+6, Y+1] := C;  Pixels[X+7, Y+1] := C;
      Pixels[X+6, Y+2] := C;  Pixels[X+7, Y+2] := C;
    end;
  end;

var
  X, Y: Integer;
begin
  X := FPaintInfo.Rect.Right - 7 - 4;
  Y := (FPaintInfo.Rect.Top + FPaintInfo.Rect.Bottom) div 2 - 2;
  FPaintInfo.Rect.Right := FPaintInfo.Rect.Right - 12;
  if Enabled
    then C := clBtnText
    else C := clBtnShadow;
  if boDropDownHoriz in FOptions then
    PaintDropDownArrowHoriz(X, Y-2)
  else if boDropDownAsEllipsis in FOptions then
    PaintDropDownArrowEllipsis(X-1, Y)
  else PaintDropDownArrowNormal(X, Y);
end;

procedure TOriCustomButton.SetChecked(Value: Boolean);
begin
  if Value <> FChecked then
  begin
    FChecked := Value;
    Invalidate;
  end;
end;

function TOriCustomButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TOriCustomButton.SetOptions(Value: TOriButtonOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    Invalidate;
  end;
end;

procedure TOriCustomButton.Click;
var
  i: Integer;
  pt: TPoint;
begin
  if GroupIndex > 0 then
  begin
    if not Checked then
    begin
      Checked := True;
      with Parent do
        for i := 0 to ControlCount-1 do
          if (Controls[i] is TOriCustomButton) and (Controls[i] <> Self) then
            with TOriCustomButton(Controls[i]) do
              if Checked and (GroupIndex = Self.GroupIndex) then
                Checked := False;
    end
    else
      if boAllowAllUnchecked in Options then Checked := False;
  end;
  if Assigned(FDropDownMenu) and not (boNoAutoDropDownMenu in Options) then
  begin
    if boDropDownAlignRight in Options then pt.X := Width else pt.X := 0;
    pt.Y := Height;
    pt := ClientToScreen(pt);
    FDropDownMenu.PopupComponent := Self;
    FDropDownMenu.Popup(pt.X, pt.Y);
  end;
  inherited;
end;

procedure TOriCustomButton.SetLook(Value: TOriButtonLook);
begin
  if FLook <> Value then
  begin
    FLook := Value;
    Invalidate;
  end;
end;

procedure TOriCustomButton.SetImages(Value: TCustomImageList);
begin
  if FImages <> nil then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  Invalidate;
end;

procedure TOriCustomButton.SetImageIndex(Value: TImageIndex);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TOriCustomButton.ImageListChange(Sender: TObject);
begin
  if Sender = Images then Invalidate;
end;

//------------------------------------------------------------------------------
{ TOriImgButton }

constructor TOriImgButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  Width := 25;
  Height := 25;
  FMonoGlyph := bgNone;
end;

procedure TOriImgButton.PaintImage;
var OffsetX, OffsetY: Integer;
begin
  if FMonoGlyph <> bgNone then
    with FPaintInfo, FPaintInfo.Rect do
    begin
      if Pressed and (Width > MonoGlyphW * 2) then OffsetX := 1 else OffsetX := 0;
      if Pressed and (Height > MonoGlyphH * 2) then OffsetY := 1 else OffsetY := 0;
      DrawMonoGlyph(Canvas.Handle,
        (Left + Right - MonoGlyphW) div 2 + OffsetX,
        (Top + Bottom - MonoGlyphH) div 2 + OffsetY,
        FMonoGlyph, IfThen(Enabled, clWindowText, clBtnShadow));
    end
  else // paint image centered
    if (FImages <> nil) and (FImageIndex > -1)
      and (FImageIndex < FImages.Count) then
      with FPaintInfo, FPaintInfo.Rect do
        FImages.Draw(Canvas,
          (Left + Right - FImages.Width) div 2 + Ord(Pressed),
          (Top + Bottom - FImages.Height) div 2 + Ord(Pressed),
          FImageIndex, Enabled);
end;

procedure TOriImgButton.PaintText;
begin
  // do nothing
end;

procedure TOriImgButton.SetMonoGlyph(Value: TOriButtonMonoGlyph);
begin
  if Value <> FMonoGlyph then
  begin
    FMonoGlyph := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
{ TOriRadioCheckBox }

const
  CheckSize = 13;
  CheckIndent = 5;

constructor TOriRadioCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks];
  FCanvas := TCanvas.Create;
  DoubleBuffered := True;
  TabStop := True;

  FLayout := tlCenter;
  FLook := rclDefault;
  FCheckPosition := taLeftJustify;
  FAlignment := taLeftJustify;
  FAutoSize := True;
  FState := cbUnchecked;
end;

destructor TOriRadioCheckBox.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TOriRadioCheckBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'BUTTON');
  with Params do
  begin
    Style := Style or BS_OWNERDRAW;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TOriRadioCheckBox.Loaded;
begin
  inherited Loaded;
  if FAutoSize then AdjustBounds;
end;

procedure TOriRadioCheckBox.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(Parent) and FAutoSize then AdjustBounds;
end;

procedure TOriRadioCheckBox.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TOriRadioCheckBox.SetOptions(Value: TOriRadioCheckBoxOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;

    if (rcoRadioButton in FOptions) and (FState = cbGrayed) then
      FState := cbUnchecked; // radio button can't be grayed

    AdjustBounds;
    Invalidate;
  end;
end;

procedure TOriRadioCheckBox.SetLook(Value: TOriRadioCheckBoxLook);
begin
  if Value <> FLook then
  begin
    FLook := Value;
    Invalidate;
  end;
end;

procedure TOriRadioCheckBox.SetCheckPosition(Value: TLeftRight);
begin
  if FCheckPosition <> Value then
  begin
    FCheckPosition := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TOriRadioCheckBox.SetLayout(Value: TTextLayout);
begin
  if Value <> FLayout then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TOriRadioCheckBox.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TOriRadioCheckBox.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;

procedure TOriRadioCheckBox.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;

procedure TOriRadioCheckBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (FLook = rclDefault) and IsThemesEnabled and
    not _MouseOver and not (csDesigning in ComponentState) then
  begin
    _MouseOver := True;
    Invalidate;
  end;
end;

procedure TOriRadioCheckBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (FLook = rclDefault) and IsThemesEnabled and _MouseOver then
  begin
    _MouseOver := False;
    Invalidate;
  end;
end;

procedure TOriRadioCheckBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;
end;

procedure TOriRadioCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TOriRadioCheckBox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
  Invalidate;
end;

procedure TOriRadioCheckBox.WMSize(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TOriRadioCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      SetFocus;
      if Focused then Toggle;
      Result := 1;
    end
    else inherited;
end;

procedure TOriRadioCheckBox.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    BN_CLICKED,
    BN_DOUBLECLICKED: Toggle;
  end;
end;

procedure TOriRadioCheckBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if IsThemesEnabled then
    Message.Result := 1
  else
    DefaultHandler(Message);
end;

procedure TOriRadioCheckBox.CNCtlColorBtn(var Message: TWMCtlColorBtn);
begin
  with OriGraphics.ThemeServices do
    if IsThemesEnabled then
    begin
      DrawParentBackground(Handle, Message.ChildDC, nil, False);
      // Return an empty brush to prevent Windows
      // from overpainting we just have created.
      Message.Result := GetStockObject(NULL_BRUSH);
    end
    else
      inherited;
end;

procedure TOriRadioCheckBox.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
  VAlignments: array[Boolean] of Word = (DT_VCENTER, DT_TOP);
var
  DC: HDC;
  Rect: TRect;
//  Align: TAlignment;
begin
  if not (csReading in ComponentState) and FAutoSize and Assigned(Parent) then
  begin
    DC := GetDC(0);
    try
      FCanvas.Handle := DC;
      Rect := ClientRect;
      Inc(Rect.Right, CheckSize + CheckIndent + 3);
      DoDrawText(Rect, DT_EXPANDTABS or DT_CALCRECT or WordWraps[WordWrap] or VAlignments[WordWrap]);
      FCanvas.Handle := 0;
    finally
      ReleaseDC(0, DC);
    end;
//    Align := FAlignment;
//    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(Align);
    SetBounds(Left, Top, Rect.Right + CheckSize + CheckIndent + 3, Rect.Bottom + 4);
  end;
end;

procedure TOriRadioCheckBox.DoDrawText(var Rect: TRect; Flags: Longint);
var
  Text: TOriString;
  PaintDefault: Boolean;
begin
  Text := Caption;

  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or (Text[1] = '&') and (Text[2] = #0))
    then Text := Text + ' ';
  Flags := DrawTextBiDiModeFlags(Flags);

  // it is true painting, not measuring
  if Flags and DT_CALCRECT = 0 then
  begin
    // reserve place for focus frame, during measurement this
    // additional place are taken into account in SetBounds
    if Flags and DT_VCENTER = 0 then Rect.Top := Rect.Top + 1;

    PaintDefault := True;
    if Assigned(FOnPaintText) then
      FOnPaintText(Self, FCanvas, Text, Rect, Flags, PaintDefault);
    if not PaintDefault then Exit;
  end;

  with FCanvas do
  begin
    Font.Assign(Self.Font);
    Brush.Style := bsClear;
    case Flook of
      rclDefault:
        if not Enabled then
        begin
          OffsetRect(Rect, 1, 1);
          Font.Color := clBtnHighlight;
          OriGraphics.DrawText(Handle, POriChar(Text), Length(Text), Rect, Flags);
          OffsetRect(Rect, -1, -1);
          Font.Color := clBtnShadow;
          OriGraphics.DrawText(Handle, POriChar(Text), Length(Text), Rect, Flags);
        end
        else
          OriGraphics.DrawText(Handle, POriChar(Text), Length(Text), Rect, Flags);

      rclFlat:
      begin
        if not Enabled then Font.Color := clBtnShadow;
        OriGraphics.DrawText(Handle, POriChar(Text), Length(Text), Rect, Flags);
      end;
    end;
  end;
end;

procedure TOriRadioCheckBox.DrawItem(const DrawItemStruct: TDrawItemStruct);
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  VAlignments: array[Boolean] of Word = (DT_VCENTER, DT_TOP);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  IsDown, IsFocused: Boolean;
  PaintDefault: Boolean;
  R, CheckRect, TxtRect: TRect;
  Flags: Longint;
  Details: TThemedElementDetails;
  Button: TThemedButton;
  DrawStyle: Longint;
{$ifdef DXE2_UP}
  DrawTextFmt: TTextFormat;
{$endif}
begin
  R := ClientRect;

  with DrawItemStruct do
  begin
    FCanvas.Handle := hDC;
    FCanvas.Font.Assign(Self.Font);
    IsDown := itemState and ODS_SELECTED <> 0;
    IsFocused := itemState and ODS_FOCUS <> 0;
  end;

  if CheckPosition = taLeftJustify then
  begin
    case Layout of
      tlTop: CheckRect := Classes.Rect(R.Left, R.Top, R.Left+CheckSize, R.Top+CheckSize);
      tlBottom: CheckRect := Classes.Rect(R.Left, R.Bottom, R.Left+CheckSize, R.Bottom-CheckSize);
      tlCenter: CheckRect := Classes.Rect(R.Left, (R.Bottom+R.Top-CheckSize) div 2, R.Left+CheckSize, (R.Bottom+R.Top+CheckSize) div 2);
    end;
    Inc(R.Left, CheckSize + CheckIndent);
  end
  else
  begin
    case Layout of
      tlTop: CheckRect := Classes.Rect(R.Right-CheckSize, R.Top, R.Right, R.Top+CheckSize);
      tlBottom: CheckRect := Classes.Rect(R.Right-CheckSize, R.Bottom, R.Right, R.Bottom-CheckSize);
      tlCenter: CheckRect := Classes.Rect(R.Right-CheckSize, (R.Bottom+R.Top-CheckSize) div 2, R.Right, (R.Bottom+R.Top+CheckSize) div 2);
    end;
    Dec(R.Right, CheckSize + CheckIndent);
  end;

  // DoDrawText takes care of BiDi alignments
  DrawStyle := DT_EXPANDTABS or WordWraps[WordWrap] or
    Alignments[FAlignment] or VAlignments[WordWrap];
  TxtRect := R;
  DoDrawText(TxtRect, DrawStyle or DT_CALCRECT);
  OffsetRect(TxtRect, 0, (Height-TxtRect.Bottom) div 2);

  case FLook of
    rclDefault:
    if IsThemesEnabled then
    begin
      if (rcoRadioButton in Options) and not (rcoRadioAsCheck in Options) then
        if not Enabled then
          case State of
            cbChecked: Button := tbRadioButtonCheckedDisabled;
            else Button := tbRadioButtonUncheckedDisabled;
          end
        else if IsDown then
          case State of
            cbChecked: Button := tbRadioButtonCheckedPressed;
            else Button := tbRadioButtonUncheckedPressed;
          end
        else if _MouseOver then
          case State of
            cbChecked: Button := tbRadioButtonCheckedHot;
            else Button := tbRadioButtonUncheckedHot;
          end
        else
          case State of
            cbChecked: Button := tbRadioButtonCheckedNormal;
            else Button := tbRadioButtonUncheckedNormal;
          end
      else
      begin
        if not Enabled then
          case State of
            cbChecked: Button := tbCheckBoxCheckedDisabled;
            cbUnchecked: Button := tbCheckBoxUncheckedDisabled;
            else Button := tbCheckBoxMixedDisabled;
          end
        else if IsDown then
          case State of
            cbChecked: Button := tbCheckBoxCheckedPressed;
            cbUnchecked: Button := tbCheckBoxUncheckedPressed;
            else Button := tbCheckBoxMixedPressed;
          end
        else if _MouseOver then
          case State of
            cbChecked: Button := tbCheckBoxCheckedHot;
            cbUnchecked: Button := tbCheckBoxUncheckedHot;
            else Button := tbCheckBoxMixedHot;
          end
        else
          case State of
            cbChecked: Button := tbCheckBoxCheckedNormal;
            cbUnchecked: Button := tbCheckBoxUncheckedNormal;
            else Button := tbCheckBoxMixedNormal;
          end;
      end;
      Details := OriGraphics.ThemeServices.GetElementDetails(Button);
      OriGraphics.ThemeServices.DrawParentBackground(Parent.Handle, DrawItemStruct.hDC, @Details, True);
      OriGraphics.ThemeServices.DrawElement(FCanvas.Handle, Details, CheckRect);

      PaintDefault := True;
      if Assigned(FOnPaintText) then
        FOnPaintText(Self, FCanvas, Caption,
          TxtRect, DrawTextBiDiModeFlags(DrawStyle), PaintDefault);
      if PaintDefault then
      begin
      {$ifdef DXE2_UP}
        DrawTextFmt := [tfExpandTabs];
        if WordWrap then
          DrawTextFmt := DrawTextFmt + [tfWordBreak, tfTop]
        else
          Include(DrawTextFmt, tfVerticalCenter);
        case FAlignment of
          taLeftJustify: Include(DrawTextFmt, tfLeft);
          taRightJustify: Include(DrawTextFmt, tfRight);
          taCenter: Include(DrawTextFmt, tfCenter);
        end;
        if UseRightToLeftAlignment then
          Include(DrawTextFmt, tfRtlReading);
        OriGraphics.ThemeServices.DrawText(DrawItemStruct.hDC, Details, Caption,
          TxtRect, DrawTextFmt, 0);
      {$else}
        OriGraphics.ThemeServices.DrawText(DrawItemStruct.hDC, Details, Caption,
          TxtRect, DrawTextBiDiModeFlags(DrawStyle), 0);
      {$endif}
      end;
    end
    else
    begin
      if (rcoRadioButton in Options) and not (rcoRadioAsCheck in Options)
        then Flags := DFCS_BUTTONRADIO
        else Flags := DFCS_BUTTONCHECK;
      if Checked then Flags := Flags or DFCS_CHECKED;
      if IsDown then Flags := Flags or DFCS_PUSHED;
      if DrawItemStruct.itemState and ODS_DISABLED <> 0 then
        Flags := Flags or DFCS_INACTIVE;
      DrawFrameControl(DrawItemStruct.hDC, CheckRect, DFC_BUTTON, Flags);
      DoDrawText(TxtRect, DrawStyle);
      if IsFocused then
        with FCanvas do
        begin
          Pen.Color := clWindowFrame;
          Brush.Color := clWindow;
          Font.Color := clWindowText;
          InflateRect(TxtRect, 2, 2);
          case FCheckPosition of
            // если текст слева, то он начинается с нуля
            // и focus-rect вылезет за пределы компонента
            taRightJustify: TxtRect.Left := 0;
          end;
          Windows.DrawFocusRect(FCanvas.Handle, TxtRect);
        end;
    end;

    rclFlat:
      with FCanvas do
      begin
        if (rcoRadioButton in Options) and not (rcoRadioAsCheck in Options) then
        begin
          DrawRadioCircle(FCanvas.Handle, CheckRect, IsDown, Checked, Enabled);
        end
        else
        begin
          Pen.Color := clBtnShadow;
          if IsDown or not Enabled or (State = cbGrayed)
            then Brush.Color := clBtnFace
            else Brush.Color := clBtnHighlight;
          Rectangle(CheckRect); // check rect
          if Checked then
            DrawMonoGlyph(Handle, CheckRect.Left+2, CheckRect.Top+1,
              bgCheck, IfThen(Enabled, clWindowText, clBtnShadow));
        end;
        DoDrawText(TxtRect, DrawStyle);
        if IsFocused then
        begin
          Pen.Color := clWindowFrame;
          Brush.Color := clWindow;
          Font.Color := clWindowText;
          InflateRect(TxtRect, 2, 2);
          case FCheckPosition of
            // если текст слева, то он начинается с нуля
            // и focus-rect вылезет за пределы компонента
            taRightJustify: TxtRect.Left := 0;
          end;
          Windows.DrawFocusRect(FCanvas.Handle, TxtRect);
        end;
      end;
  end;
  FCanvas.Handle := 0;
end;

function TOriRadioCheckBox.GetChecked: Boolean;
begin
  Result := State = cbChecked;
end;

procedure TOriRadioCheckBox.SetChecked(Value: Boolean);
begin
  if Value then State := cbChecked else State := cbUnchecked;
end;

procedure TOriRadioCheckBox.SetState(Value: TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    Invalidate;
    if (rcoRadioButton in FOptions) and (FState = cbChecked) then
      UncheckSiblings;
    if not ClicksDisabled then Click;
  end;
end;

procedure TOriRadioCheckBox.Toggle;
begin
  case State of
    cbUnchecked:
      if FAllowGrayed and not (rcoRadioButton in FOptions)
        then State := cbGrayed
        else State := cbChecked;
    cbChecked:
      if rcoRadioButton in FOptions
        then Exit // user can't switch radio button to off
        else State := cbUnchecked;
    cbGrayed: State := cbChecked;
  end;
end;

procedure TOriRadioCheckBox.UncheckSiblings;
var i: Integer;
begin
  for i := 0 to Parent.ControlCount-1 do
    if (Parent.Controls[i] <> Self)
    and (Parent.Controls[i] is TOriRadioCheckBox) then
      with TOriRadioCheckBox(Parent.Controls[i]) do
        if Checked and (rcoRadioButton in Options)
          and (GroupIndex = Self.GroupIndex)
          then Checked := False;
end;

procedure TOriRadioCheckBox.Click;
begin
  inherited Changed;
  inherited Click;
end;

//------------------------------------------------------------------------------
{ TOriDialogPanel }

constructor TOriDialogPanel.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csAcceptsControls, csParentbackGround] - [csOpaque];

  FButtons := [dbutOk, dbutCancel];
  FHorzMargin := 6;
  FVertMargin := 6;
  FButtonWidth := 75;
  FButtonHeight := 25;
  FButtonSpace := 6;

  Align := alBottom;
  Height := FButtonHeight + 2*FVertMargin;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  ParentBackground := True;

  // if default value then create buttons,
  // else buttons will be created in SetButtons during loading
  if FButtons = [dbutOk, dbutCancel] then RecreateButtons;
end;

destructor TOriDialogPanel.Destroy;
begin
  RemoveButtons;
  inherited;
end;

{
var
  HookHandle: HHOOK;

function GetMsgHookProc(code: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  msg: PMsg;
begin
  msg := PMsg(Pointer(lParam));
  case msg.message of
    // This undocumented message is a precursor to WM_HELP and catching
    $4D: if wParam = PM_REMOVE then MsgBox('F1');
  end;
  Result := CallNextHookEx(HookHandle, code, wParam, lParam);
end;
}

procedure TOriDialogPanel.SetParent(AParent: TWinControl);
begin
  inherited;
//  HookHandle := SetWindowsHookEx(WH_GETMESSAGE, GetMsgHookProc, 0, GetCurrentThreadId);
end;

function TOriDialogPanel.GetButtonCaption(Value: TDialogButton): TOriString;

{$ifdef USE_DKLANG}
  function GetItemName(const AKey: String; const ADefault: TOriString): TOriString;
  begin
    try
      Result := LangManager.ConstantValue[AKey]
    except
      Result := ADefault;
    end;
  end;
{$endif}

const
  SCaptionOK = 'OK';
  SCaptionCancel = 'Cancel';
  SCaptionApply = 'Apply';
  SCaptionHelp = 'Help';
begin
{$ifdef USE_DKLANG}
try
  case Value of
    dbutOk:     Result := GetItemName('OriDialog_OK', SCaptionOK);
    dbutCancel: Result := GetItemName('OriDialog_Cancel', SCaptionCancel);
    dbutApply:  Result := GetItemName('OriDialog_Apply', SCaptionApply);
    dbutHelp:   Result := GetItemName('OriDialog_Help', SCaptionHelp);
	end;
except
	// TODO: убрать, когда старые имена констант (SDialog...)
	// будут исправлены во всех проектах на новые
  case Value of
    dbutOk:     Result := GetItemName('SDialogPanelOK', SCaptionOK);
    dbutCancel: Result := GetItemName('SDialogPanelCancel', SCaptionCancel);
    dbutApply:  Result := GetItemName('SDialogPanelApply', SCaptionApply);
    dbutHelp:   Result := GetItemName('SDialogPanelHelp', SCaptionHelp);
  end;
end;
{$else}
  case Value of
    dbutOk:     Result := SCaptionOK;
    dbutCancel: Result := SCaptionCancel;
    dbutApply:  Result := SCaptionApply;
    dbutHelp:   Result := SCaptionHelp;
  end;
{$endif}
end;

procedure TOriDialogPanel.RemoveButtons;
begin
  FreeAndNil(FButtonOk);
  FreeAndNil(FButtonCancel);
  FreeAndNil(FButtonApply);
  FreeAndNil(FButtonHelp);
end;

procedure TOriDialogPanel.RecreateButtons;
begin
  RemoveButtons;
  if dbutOk in FButtons then
  begin
    FButtonOk := TOriDlgButton.Create(Self);
    with FButtonOk do
    begin
      Parent := Self;
      ModalResult := mrOk;
      Default := True;
      Caption := GetButtonCaption(dbutOk);
      OnClick := FOnClickOK;
    end;
  end;
  if dbutCancel in FButtons then
  begin
    FButtonCancel := TOriDlgButton.Create(Self);
    with FButtonCancel do
    begin
      Parent := Self;
      ModalResult := mrCancel;
      Cancel := True;
      Caption := GetButtonCaption(dbutCancel);
      OnClick := FOnClickCancel;
    end;
  end;
  if dbutApply in FButtons then
  begin
    FButtonApply := TOriDlgButton.Create(Self);
    with FButtonApply do
    begin
      Parent := Self;
      Caption := GetButtonCaption(dbutApply);
      OnClick := FOnClickApply;
    end;
  end;
  if dbutHelp in FButtons then
  begin
    FButtonHelp := TOriDlgButton.Create(Self);
    with FButtonHelp do
    begin
      Parent := Self;
      Caption := GetButtonCaption(dbutHelp);
      OnClick := InternalHelpClick;
    end;
  end;
end;

procedure TOriDialogPanel.RecreateHelpLabel;
begin
  FreeAndNil(FHelpLabel);
  if dpoHelpLabel in FOptions then
  begin
    FHelpLabel := TLabel.Create(Self);
    with FHelpLabel do
    begin
      Parent := Self;
      Caption := GetButtonCaption(dbutHelp);
      Font.Color := clBlue;
      Font.Style := Font.Style + [fsUnderline];
      Cursor := crHandPoint;
      OnClick := InternalHelpClick;
    end;
  end;
end;

procedure TOriDialogPanel.ArrangeButtons;
var w: Integer;
begin
  // TODO: remaining aligns will be coded as needed
  //case Align of
    //alBottom:
      begin
        if Assigned(FButtonHelp) then
        begin
          FButtonHelp.Height := FButtonHeight;
          FButtonHelp.Width := FButtonWidth;
          FButtonHelp.Top := Height - FButtonHeight - FVertMargin;
          FButtonHelp.Left := FHorzMargin;
        end;
        w := 0;
        if Assigned(FButtonApply) then
        begin
          Inc(w, FButtonWidth);
          FButtonApply.Height := FButtonHeight;
          FButtonApply.Width := FButtonWidth;
          FButtonApply.Top := Height - FVertMargin - FButtonHeight;
          FButtonApply.Left := Width - FHorzMargin - w;
          Inc(w, FButtonSpace);
        end;
        if Assigned(FButtonCancel) then
        begin
          Inc(w, FButtonWidth);
          FButtonCancel.Height := FButtonHeight;
          FButtonCancel.Width := FButtonWidth;
          FButtonCancel.Top := Height - FVertMargin - FButtonHeight;
          FButtonCancel.Left := Width - FHorzMargin - w;
          Inc(w, FButtonSpace);
        end;
        if Assigned(FButtonOK) then
        begin
          Inc(w, FButtonWidth);
          FButtonOK.Height := FButtonHeight;
          FButtonOK.Width := FButtonWidth;
          FButtonOK.Top := Height - FVertMargin - FButtonHeight;
          FButtonOK.Left := Width - FHorzMargin - w;
        end;
        if Assigned(FHelpLabel) then
        begin
          FHelpLabel.Left := FHorzMargin;
          if dpoHelpLabelOpposite in FOptions then
            FHelpLabel.Top := Height - FVertMargin - FHelpLabel.Height
          else
            FHelpLabel.Top := 0;
        end;
      end;
  //end;
end;

procedure TOriDialogPanel.SetSize(Index: Integer; Value: Integer);
begin
  case Index of
    0:
      if Value <> FHorzMargin then
      begin
        FHorzMargin := Value;
        ArrangeButtons;
      end;
    1:
      if Value <> FVertMargin then
      begin
        FVertMargin := Value;
        ArrangeButtons;
      end;
    2:
      if Value <> FButtonWidth then
      begin
        FButtonWidth := Value;
        ArrangeButtons;
      end;
    3:
      if Value <> FButtonHeight then
      begin
        FButtonHeight := Value;
        ArrangeButtons;
      end;
    4:
      if Value <> FButtonSpace then
      begin
        FButtonSpace := Value;
        ArrangeButtons;
      end;
  end;
end;

procedure TOriDialogPanel.SetButtons(Value: TDialogButtons);
begin
  if Value <> FButtons then
  begin
    FButtons := Value;
    RecreateButtons;
    ArrangeButtons;
  end;
end;

procedure TOriDialogPanel.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

procedure TOriDialogPanel.SetEvent(Index: Integer; Value: TNotifyEvent);
begin
  case Index of
    0:
      begin
        FOnClickOK := Value;
        if Assigned(FButtonOK) then FButtonOK.OnClick := Value;
      end;
    1:
      begin
        FOnClickCancel := Value;
        if Assigned(FButtonCancel) then FButtonCancel.OnClick := Value;
      end;
    2:
      begin
        FOnClickApply := Value;
        if Assigned(FButtonApply) then FButtonApply.OnClick := Value;
      end;
    3:
      begin
        FOnClickHelp := Value;
      end;
  end;
end;

procedure TOriDialogPanel.InternalHelpClick(Sender: TObject);
begin
  if FHelpTopic <> '' then ShowHelp(FHelpTopic);
  if Assigned(FOnClickHelp) then FOnClickHelp(Self);
end;

procedure TOriDialogPanel.SetOptions(const Value: TDialogPanelOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    RecreateHelpLabel;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TOriDialogPanel.PaintDesignTime;
var
  R: TRect;
  DC: HDC;
begin
  DC := Canvas.Handle;
  R := ClientRect;
  SaveDC(DC);
  ExcludeClipRect(DC, R.Left+1, R.Top+1, R.Right-1, R.Bottom-1);
  DitherRect(DC, R, clBtnFace, clBtnShadow);
  RestoreDC(DC, -1);
end;

procedure TOriDialogPanel.PaintSizeGrip;
var
  D, Sz, I: Integer;
  DC: HDC;
  R: TRect;

  procedure DiagLine(C: TColor);
  var
    Pen, OldPen: HPEN;
  begin
    if C < 0 then C := GetSysColor(C and $000000FF);
    Pen := CreatePen(PS_SOLID, 1, C);
    OldPen := SelectObject(DC, Pen);
    with R do
    begin
      Windows.MoveToEx(DC, Right - 1 - D, Bottom - 1, nil);
      Windows.LineTo(DC, Right, Bottom - D - 2);
    end;
    SelectObject(DC, OldPen);
    DeleteObject(Pen);
    Inc(D);
  end;

begin
  D := 0;
  R := ClientRect;
  DC := Canvas.Handle;
  Sz := Min(R.Right - R.Left, R.Bottom - R.Top);
  // code borrwed from TTBXOfficeXPTheme.PaintStatusBar
  for I := 1 to 3 do
    case Sz of
      0..8:
        begin
          DiagLine(clBtnShadow);
          DiagLine(clBtnHighlight);
        end;
      9..11:
        begin
          DiagLine(clBtnFace);
          DiagLine(clBtnShadow);
          DiagLine(clBtnHighlight);
        end;
      12..14:
        begin
          DiagLine(clBtnShadow);
          DiagLine(clBtnShadow);
          DiagLine(clBtnHighlight);
        end;
      else
        DiagLine(clBtnFace);
        DiagLine(clBtnShadow);
        DiagLine(clBtnShadow);
        DiagLine(clBtnHighlight);
      end;
end;

procedure TOriDialogPanel.PaintSeparator;
begin
  Canvas.Pen.Color := clBtnShadow;
  case Align of
    alBottom:
    begin
      Canvas.MoveTo(0, 0);
      Canvas.LineTo(Width, 0);
    end;
    // TODO: остальные по необходимости
  end;
end;

procedure TOriDialogPanel.Paint;
begin
  if not IsThemesEnabled or not ParentBackground then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
  end;
  if csDesigning in ComponentState then PaintDesignTime;
  if dpoSizeGrip in FOptions then PaintSizeGrip;
  if dpoSeparator in FOptions then PaintSeparator;
end;

//------------------------------------------------------------------------------
{$ifdef USE_TBX}
{ TOriColorButton }

constructor TOriColorButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Include(FOptions, boDropDownArrow);

  FItemRoot := TTBXPopupMenu.Create(Self);
  FItemRoot.OnPopup := DoPopup;
  FItemRoot.Options := FItemRoot.Options + [tboToolbarStyle];
  DropDownMenu := FItemRoot;

  FColorPosition := taLeftJustify;
  FColorSelector := TTBXOriColorSelector.Create(Self);
end;

destructor TOriColorButton.Destroy;
begin
  FItemRoot.Free;
  FColorSelector.Free;
  inherited;
end;

procedure TOriColorButton.DoPopup(Sender: TObject);
begin
  FColorSelector.InitItems(FItemRoot.Items);
end;

procedure TOriColorButton.SetShowCaption(Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    Invalidate;
  end;
end;

procedure TOriColorButton.SetColorPosition(Value: TLeftRight);
begin
  if FColorPosition <> Value then
  begin
    FColorPosition := Value;
    Invalidate;
  end;
end;

procedure TOriColorButton.SetSelectedColor(Value: TColor);
begin
  FColorSelector.SelectedColor := Value;
end;

function TOriColorButton.GetSelectedColor: TColor;
begin
  Result := FColorSelector.SelectedColor;
end;

procedure TOriColorButton.SetOnColorChange(Value: TNotifyEvent);
begin
  FColorSelector.OnColorChange := Value;
end;

function TOriColorButton.GetOnColorChange: TNotifyEvent;
begin
  Result := FColorSelector.OnColorChange;
end;

procedure TOriColorButton.PaintText;
var
  Rect: TRect;
begin
  if FShowCaption then
  begin
    Rect := FPaintInfo.Rect;
    if FColorPosition = taLeftJustify then
    begin
      Rect.Right := Rect.Left + Rect.Bottom - Rect.Top;
      FPaintInfo.Rect.Left := Rect.Right;
    end
    else
    begin
      Rect.Left := Rect.Right - Rect.Bottom + Rect.Top;
      FPaintInfo.Rect.Right := Rect.Left;
    end;
  end
  else Rect := FPaintInfo.Rect;
  if FPaintInfo.Pressed and (FLook <> blFlat) then
    OffsetRect(Rect, 1, 1);
  InflateRect(Rect, -4, -4);
  // paint rect with selected color
  FCanvas.Pen.Color := clGray;
  FCanvas.Brush.Color := FColorSelector.SelectedColor;
  FCanvas.Rectangle(Rect);
  if FShowCaption then inherited;
end;

//------------------------------------------------------------------------------
{ TOriBrushButton }

constructor TOriBrushButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Include(FOptions, boDropDownArrow);

  FButtonStyle := bstBrush;
  FPenStyle := psSolid;
  FBrushStyle := bsSolid;
  FSmallDots := False;
  FUseSmallDots := False;
  FPopupWidth := 80;

  FItemRoot := TTBXPopupMenu.Create(Self);
  FItemRoot.OnPopup := DoPopup;
  FItemRoot.Options := FItemRoot.Options + [tboToolbarStyle];
  DropDownMenu := FItemRoot;
end;

destructor TOriBrushButton.Destroy;
begin
  FItemRoot.Free;
  inherited;
end;

procedure TOriBrushButton.SetPenStyle(Value: TPenStyle);
begin
  if Value <> FPenStyle then
  begin
    FPenStyle := Value;
    Invalidate;
  end;
end;

procedure TOriBrushButton.SetBrushStyle(Value: TBrushStyle);
begin
  if Value <> FBrushStyle then
  begin
    FBrushStyle := Value;
    Invalidate;
  end;
end;

procedure TOriBrushButton.SetSmallDots(Value: Boolean);
begin
  if Value <> FSmallDots then
  begin
    FSmallDots := Value;
    Invalidate;
  end;
end;

procedure TOriBrushButton.ItemPenClick(Sender: TObject);
begin
  FSmallDots := TPenStyle((Sender as TTBXItem).Tag) = psInsideFrame;
  PenStyle := TPenStyle((Sender as TTBXItem).Tag);
  if Assigned(OnStyleChanged) then OnStyleChanged(Self);
end;

procedure TOriBrushButton.ItemBrushClick(Sender: TObject);
begin
  BrushStyle := TBrushStyle((Sender as TTBXItem).Tag);
  if Assigned(OnStyleChanged) then OnStyleChanged(Self);
end;

procedure TOriBrushButton.DrawPenImage(Item: TTBCustomItem;
  Viewer: TTBItemViewer; Canvas: TCanvas; ImageRect: TRect;
  ImageOffset: TPoint; StateFlags: Integer);
var
  X1, X2, Y: Integer;
  R: TRect;
begin
  R := ImageRect;
  with Canvas do
  begin
    Brush.Color := clWhite;
    Pen.Color := clDkGray;
    Pen.Style := psSolid;
    Rectangle(R);
    Y := (R.Top + R.Bottom) div 2;
    X1 := R.Left + 1;
    X2 := R.Right - 1;
    if TPenStyle(Item.Tag) = psInsideFrame then begin
      while X1 < X2 do begin
        Pixels[X1, Y] := clDkGray;
        Pixels[X1+1, Y] := clDkGray;
        Pixels[X1, Y+1] := clDkGray;
        Pixels[X1+1, Y+1] := clDkGray;
        Inc(X1, 4);
      end;
    end
    else begin
      Pen.Style := TPenStyle(Item.Tag);
      MoveTo(X1, Y);
      LineTo(X2, Y);
      MoveTo(X1, Y-1);
      LineTo(X2, Y-1);
      MoveTo(X1, Y+1);
      LineTo(X2, Y+1);
    end;
  end;
end;

procedure TOriBrushButton.DrawBrushImage(Item: TTBCustomItem;
  Viewer: TTBItemViewer; Canvas: TCanvas; ImageRect: TRect;
  ImageOffset: TPoint; StateFlags: Integer);
begin
  with Canvas do
  begin
    Pen.Color := clDkGray;
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Rectangle(ImageRect);
    if TBrushStyle(Item.Tag) <> bsClear then
    begin
      Brush.Style := TBrushStyle(Item.Tag);
      Brush.Color := clDkGray;
      Rectangle(ImageRect);
    end;
  end;
end;

procedure TOriBrushButton.DoPopup(Sender: TObject);
var
  i: Integer;
  it: TTBXItem;
  bmp: TBitmap;
begin
  if FItemRoot.Items.Count = 0 then
  begin
    bmp := TBitmap.Create;
    bmp.Height := 16;
    bmp.Width := FPopupWidth;

    FImageList := TCustomImageList.Create(Self);
    FImageList.Height := 16;
    FImageList.Width := FPopupWidth;
    FImageList.Add(bmp, bmp);
    FItemRoot.Images := FImageList;

    bmp.Free;

    if FButtonStyle = bstPen then
    begin
      if UseSmallDots then i := 6 else i := 5;
      for i := 0 to i do
      begin
        it := TTBXItem.Create(FItemRoot);
        it.Tag := i;
        it.OnClick := ItemPenClick;
        it.OnDrawImage := DrawPenImage;
        it.ImageIndex := 0;
        FItemRoot.Items.Add(it);
      end;
    end
    else
      for i := 0 to 7 do
      begin
        it := TTBXItem.Create(FItemRoot);
        it.Tag := i;
        it.OnClick := ItemBrushClick;
        it.OnDrawImage := DrawBrushImage;
        it.ImageIndex := 0;
        FItemRoot.Items.Add(it);
      end;
  end;
end;

procedure TOriBrushButton.PaintText;
var
  Y, X1, X2: Integer;
  C: TColor;
  Rect: TRect;
begin
  Rect := FPaintInfo.Rect;
  if Enabled
    then C := clBtnText
    else C := clBtnShadow;
  if FPaintInfo.Pressed and (FLook <> blFlat) then
    OffsetRect(Rect, 1, 1);
  InflateRect(Rect, -4, -4);
  if ButtonStyle = bstPen then
  begin
    // draw thick line with selected pen
    Y := (Rect.Top + Rect.Bottom) div 2;
    X1 := Rect.Left + 1;
    X2 := Rect.Right - 1;
    with FCanvas do
    begin
      Pen.Color := clBtnShadow;
      Pen.Style := psSolid;
      Brush.Color := clWhite;
      Rectangle(Rect);
      if SmallDots then
        while X1 < X2 do
        begin
          Pixels[X1, Y] := C;
          Pixels[X1+1, Y] := C;
          Pixels[X1, Y+1] := C;
          Pixels[X1+1, Y+1] := C;
          Inc(X1, 4);
        end
      else
      begin
        Pen.Color := C;
        Pen.Style := FPenStyle;
        MoveTo(X1, Y);
        LineTo(X2, Y);
        MoveTo(X1, Y-1);
        LineTo(X2, Y-1);
        MoveTo(X1, Y+1);
        LineTo(X2, Y+1);
        Pen.Style := psSolid;
      end;
    end;
  end
  else
    // draw rect with selected brush
    with FCanvas do
    begin
      Pen.Color := clDkGray;
      Brush.Style := bsSolid;
      Brush.Color := clWhite;
      Rectangle(Rect);
      if FBrushStyle <> bsClear then
      begin
        Brush.Style := FBrushStyle;
        Brush.Color := clDkGray;
        Rectangle(Rect);
      end;
    end;
end;

//------------------------------------------------------------------------------
{ TOriComboButton }

function TTBXComboItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result := TTBXComboItemViewer;
end;

procedure TTBXComboItemViewer.CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
begin
  inherited CalcSize(Canvas, AWidth, AHeight);

  with TOriComboButton(Item.Owner) do
    if AWidth < Width then AWidth := Width;
end;

constructor TOriComboButton.Create(AOwner: TComponent);
begin
  inherited;

  Include(FOptions, boDropDownArrow);

  FItemIndex := -1;

  DropDownMenu := TTBXPopupMenu.Create(Self);
end;

procedure TOriComboButton.AddItem(const Value: TOriString; Tag: Integer);
var
  it: TTBXItem;
begin
  it := TTBXComboItem.Create(Self);
  it.Caption := Value;
  it.OnClick := ItemClick;
  it.Tag := Tag;
  TTBXPopupMenu(DropDownMenu).Items.Add(it);
end;

procedure TOriComboButton.ItemClick(Sender: TObject);
begin
  FItemIndex := TTBXPopupMenu(DropDownMenu).Items.IndexOf(TTBCustomItem(Sender));
  Caption := TTBXItem(Sender).Caption;
  DoChange;
end;

procedure TOriComboButton.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TOriComboButton.SetItemIndex(Value: Integer);
begin
  if Value <> FItemIndex then
  begin
    if (Value < -1) or (Value >= TTBXPopupMenu(DropDownMenu).Items.Count)
      then FItemIndex := -1
      else FItemIndex := Value;
    if FItemIndex = -1
      then Caption := ''
      else Caption := TTBXPopupMenu(DropDownMenu).Items[FItemIndex].Caption;
    DoChange;
  end;
end;

function TOriComboButton.GetItemsCount: Integer;
begin
  Result := TTBXPopupMenu(DropDownMenu).Items.Count;
end;
{$endif}

end.
