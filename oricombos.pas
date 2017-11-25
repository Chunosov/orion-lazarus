{***************************************************************************
 *
 * Orion-project.org Lazarus Helper Library
 * Copyright (C) 2016-2017 by Nikolay Chunosov
 * 
 * This file is part of the Orion-project.org Lazarus Helper Library
 * https://github.com/Chunosov/orion-lazarus
 *
 * This Library is free software: you can redistribute it and/or modify it 
 * under the terms of the MIT License. See enclosed LICENSE.txt for details.
 *
 ***************************************************************************}

unit OriCombos;

interface

uses
  Messages, SysUtils, Types, Classes, Controls, Graphics, StdCtrls;

type
  TOriComboLook = (oclDefault, oclFlat);

  TOriComboBox = class(TComboBox)
  private
    FDroppedWidth: Integer;
    FLook: TOriComboLook;
    procedure SetLook(Value: TOriComboLook);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
  public
    procedure CalcDroppedWidth;
  published
    property DroppedWidth: Integer read FDroppedWidth write FDroppedWidth default 0;
    property Look: TOriComboLook read FLook write SetLook default oclDefault;
    property Align;
  end;
(*
  { TCustomOriComboMRU }

  // this class implements a stack for keeping track of
  // most recently used items in the ComboBox
  TComboBoxMRUList = class
  protected
    FMaxItems : Integer;           // maximum items to keep
    FList     : TOriStrings;      // the items themselves
    procedure SetMaxItems(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Shrink;
    procedure NewItem(const Item: TOriString; Obj: TObject);
    function RemoveItem(const Item: TOriString) : Boolean;

    property Items: TOriStrings read FList;
    property MaxItems: Integer read FMaxItems write SetMaxItems;
  end;

  TOriComboStyle = (ocsDropDown, ocsDropDownList);

  TOriComboBoxMRU = class(TOriComboBoxBase)
  protected
    FDrawingEdit  : Boolean;
    FDroppedWidth : Integer;
    FItemHeight   : Integer;          // hides inherited property
    FMRUListColor : TColor;
    FStyle        : TOriComboStyle;
    FLook         : TOriComboLook;

    // internal variables
    FMRUList      : TComboBoxMRUList;
    FList         : TOriStrings;  // Items sans MRU Items, Fill in GetList

  protected
    // property methods
    procedure SetDroppedWidth(Value: Integer);
    procedure SetItemHeight(Value: Integer); reintroduce;
    function  GetListIndex: Integer;
    procedure SetListIndex(Value: Integer);
    function  GetList: TOriStrings;
    function  GetMRUList: TOriStrings;
    function  GetMRUListCount: Integer;
    procedure SetMRUListCount(Value: Integer);
    procedure SetComboStyle(Value: TOriComboStyle);
    procedure SetLook(Value: TOriComboLook);

    // internal methods
    procedure UpdateMRUList;
    procedure UpdateMRUListModified;
    procedure MRUListUpdate(Count : Integer);

    procedure CNCommand(var Message: TWmCommand); message CN_COMMAND;
    procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
    procedure WMMeasureItem(var Message: TMessage); message WM_MEASUREITEM;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

    procedure CreateParams(var Params : TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DoExit; override;
    procedure DrawItem(Index: Integer; ItemRect: TRect; State: TOwnerDrawState); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  AddItem(const Item: TOriString; AObject: TObject): Integer; reintroduce;
    procedure AssignItems(Source: TPersistent);
    procedure ClearItems;
    procedure InsertItem(Index: Integer; const Item: TOriString; AObject: TObject);
    procedure RemoveItem(const Item: TOriString);
    procedure AddItemToMRUList(Index: Integer);
    procedure ClearMRUList;

    property DrawingEdit: Boolean read FDrawingEdit;
    property List: TOriStrings read GetList;
    property ListIndex: Integer read GetListIndex write SetListIndex;
    property MRUList: TOriStrings read GetMRUList;

  published
    property Style: TOriComboStyle read FStyle write SetComboStyle default ocsDropDown;
    property DroppedWidth: Integer read FDroppedWidth write SetDroppedWidth default 0;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 18;
    property MRUListColor: TColor read FMRUListColor write FMRUListColor default clWindow;
    property MRUListCount: Integer read GetMRUListCount write SetMRUListCount default 3;
    property Look: TOriComboLook read FLook write SetLook default oclDefault;
    property Align;
  end;

  procedure LoadComboMRUList(ACombo: TOriComboBoxMRU; const MRUString: String);

type
  { TOriFontComboBox }
  TFontCategories   = (fcAll, fcTrueType, fcDevice);
  TFontPitches      = (fpAll, fpFixed, fpVariable);

  TOriFontComboBox = class(TOriComboBoxMRU)
  protected
    // property variables
    FPreviewFont    : Boolean;         // If True, fonts are previewed on list
    FCategories     : TFontCategories; // Categories to list
    FPitchStyles    : TFontPitches;    // Pitches to list
    FPreviewControl : TControl;        // Control used to preview font
    FListFontSize   : Integer;

    // internal variables
    fcTTBitmap      : TBitmap;         // TrueType bitmap
    fcDevBitmap     : TBitmap;         // Device font bitmap

    // property methods
    function GetFontName: TOriString;
    procedure SetFontName(const FontName: TOriString);
    procedure SetPreviewControl(Value: TControl);

    // vcl message methods
    procedure CMFontChange(var Message: TMessage); message CM_FONTCHANGE;

  protected
    procedure Change; override;
    procedure DrawItem(Index: Integer; ItemRect: TRect; State: TOwnerDrawState); override;
    procedure Loaded; override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure DoPreview;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Populate;

  published
    property FontName: TOriString read GetFontName write SetFontName;
    property FontCategories: TFontCategories read FCategories write FCategories default fcAll;
    property FontPitchStyles: TFontPitches read FPitchStyles write FPitchStyles default fpAll;
    property PreviewControl: TControl read FPreviewControl write SetPreviewControl;
    property PreviewFont: Boolean read FPreviewFont write FPreviewFont default False;
    property ListFontSize: Integer read FListFontSize write FListFontSize;
  end;
*)
implementation

//{$R *.res}

//uses
  //Printers,
  //OriGraphics;

const
  ButtonWidth = 18;

procedure TOriComboBox.CreateWnd;
begin
  inherited;
  CalcDroppedWidth;
end;

procedure TOriComboBox.Loaded;
begin
  inherited;
  CalcDroppedWidth;
end;

procedure TOriComboBox.CalcDroppedWidth;
var
  I, MaxW, W: Integer;
  C: TControlCanvas;
begin
  if FDroppedWidth = 0 then
  begin
    MaxW := 0;
    C := TControlCanvas.Create;
    try
      C.Control := Self;
      C.Font.Assign(Font);
      for I := 0 to Items.Count - 1 do
      begin
        W := C.GetTextWidth(Items[I]);
        if W > MaxW then MaxW := W;
      end;
    finally
      C.Free;
    end;
    Inc(MaxW, 8);
    if Items.Count > DropDownCount then
      Inc(MaxW, GetSystemMetrics(SM_CXVSCROLL));
  end
  else
    MaxW := FDroppedWidth;

  if MaxW > Width then
    SendMessage(Handle, CB_SETDROPPEDWIDTH, MaxW, 0);
end;

procedure TOriComboBox.SetLook(Value: TOriComboLook);
begin
  if Value <> FLook then
  begin
    FLook := Value;
    Invalidate;
  end;
end;

procedure TOriComboBox.WMPaint(var Message: TWMPaint);
var R: TRect;
begin
  inherited;
  case FLook of
    // TODO: в стиле csDropDownList рисовать как кнопку со стрелкой
    oclFlat:
      with TControlCanvas.Create do
      try
        Control := Self;
        Lock;

        R := ClientRect;
        ExcludeClipRect(Handle, 2, 2, R.Right - 2 - ButtonWidth, R.Bottom - 2);

        if Enabled
          then Brush.Color := clWindow
          else Brush.Color := clBtnFace;
        Brush.Style := bsSolid;
        Pen.Color := clBtnShadow;
        Pen.Style := psSolid;
        Rectangle(R);

        R.Left := R.Right - ButtonWidth;
        R.Top := R.Top + 2;
        R.Bottom := R.Bottom - 2;
        R.Right := R.Right - 2;
        if Enabled
          then DrawFrameControl(Handle, R, DFC_SCROLL, DFCS_FLAT or DFCS_SCROLLCOMBOBOX)
          else DrawFrameControl(Handle, R, DFC_SCROLL, DFCS_FLAT or DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE);
      finally
        Unlock;
        Free;
      end;
  end;
end;
(*
//------------------------------------------------------------------------------
{ TComboBoxMRUList }

constructor TComboBoxMRUList.Create;
begin
  FList := TOriStringList.Create;
  FMaxItems := 3;
end;

destructor TComboBoxMRUList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TComboBoxMRUList.NewItem(const Item: TOriString; Obj: TObject);
var Index: Integer;
begin
  Index := FList.IndexOf(Item);
  if Index > -1 then
  begin
    // If the item is already in the list, just bring it to the top
    FList.Delete(Index);
    FList.InsertObject(0, Item, Obj);
  end
  else
  begin
    FList.InsertObject(0, Item, Obj);
    // this may result in more items in the list than are allowed,
    // but a call to Shrink will remove the excess items
    Shrink;
  end;
end;

function TComboBoxMRUList.RemoveItem(const Item: TOriString) : Boolean;
var Index : Integer;
begin
  Index := FList.IndexOf(Item);
  Result := Index > -1;
  if Result then FList.Delete(Index);
end;

procedure TComboBoxMRUList.Shrink;
begin
  while FList.Count > FMaxItems do
    FList.Delete(FList.Count - 1);
end;

procedure TComboBoxMRUList.Clear;
begin
  FList.Clear;
end;

procedure TComboBoxMRUList.SetMaxItems(Value: Integer);
begin
  FMaxItems := Value;
  Shrink;
end;

//------------------------------------------------------------------------------
{ TCustomOriComboMRU }

procedure LoadComboMRUList(ACombo: TOriComboBoxMRU; const MRUString: String);
var
  strs: TStrings;
  i, index: Integer;
begin
  strs := TStringList.Create;
  try
    strs.DelimitedText := MRUString;
    for i := strs.Count-1 downto 0 do
    begin
      index := ACombo.Items.IndexOf(strs[i]);
      if index <> -1 then ACombo.AddItemToMRUList(index);
    end;
  finally
    strs.Free;
  end;
end;


const
  cbxSeparatorHeight = 3;

constructor TOriComboBoxMRU.Create(AOwner : TComponent);
begin
  inherited;

  FItemHeight := 18;

  FList := TOriStringList.Create;
  FMRUList := TComboBoxMRUList.Create;
  FMRUList.MaxItems := 3;

  FDroppedWidth := 0;
  FMRUListColor := clWindow;
end;

procedure TOriComboBoxMRU.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  DroppedWidth := FDroppedWidth;
end;

procedure TOriComboBoxMRU.CreateParams(var Params: TCreateParams);
begin
  inherited;
  case FStyle of
    ocsDropDown:  Params.Style := Params.Style or CBS_DROPDOWN or CBS_OWNERDRAWVARIABLE;
    ocsDropDownList: Params.Style := Params.Style or CBS_DROPDOWNLIST or CBS_OWNERDRAWVARIABLE;
  end;
end;

destructor TOriComboBoxMRU.Destroy;
begin
  FreeAndNil(FMRUList);
  FreeAndNil(FList);
  inherited;
end;

procedure TOriComboBoxMRU.ClearItems;
begin
  ClearMRUList;
  if HandleAllocated then Clear;
end;

procedure TOriComboBoxMRU.ClearMRUList;
var I: Integer;
begin
  if (FMRUList.Items.Count > 0) then begin
    for I := 1 to FMRUList.Items.Count do
      if (I <= Items.Count) then
        Items.Delete(0);
    FMRUList.Clear;
  end;
end;

// возвращает список строк без MRU-листа
function TOriComboBoxMRU.GetList: TOriStrings;
var
  I : Integer;
begin
  FList.Clear;
  FList.Assign(Items);
  if FMRUList.Items.Count > 0 then
    for I := 0 to Pred(FMRUList.Items.Count) do
      FList.Delete(0);
  Result := FList;
end;

// возвращает спиок строк MRU-листа
function TOriComboBoxMRU.GetMRUList: TOriStrings;
begin
  Result := FMRUList.FList;
end;

// Value is the index into the list sans MRU items
procedure TOriComboBoxMRU.SetListIndex(Value: Integer);
var I: Integer;
begin
  I := FMRUList.Items.Count;
  if (((Value + I) < Items.Count) and (Value >= 0))
    then ItemIndex := Value + I
    else ItemIndex := -1;
end;

// Translates ItemIndex into index sans MRU Items
function TOriComboBoxMRU.GetListIndex;
begin
  Result := ItemIndex - FMRUList.Items.Count;
end;

procedure TOriComboBoxMRU.AssignItems(Source: TPersistent);
begin
  Clear;
  Items.Assign(Source);
end;

function TOriComboBoxMRU.AddItem(const Item: TOriString; AObject: TObject): Integer;
begin
  Result := -1;
  if (Items.IndexOf(Item) < 0) then
  begin
    Result := Items.AddObject(Item, AObject) - FMRUList.Items.Count;
    UpdateMRUList;
  end;
end;

procedure TOriComboBoxMRU.InsertItem(Index: Integer; const Item: TOriString; AObject: TObject);
var I: Integer;
begin
  I := FMRUList.Items.Count;
  if (Index> -1) and (Index < (Items.Count - I)) then
  begin
    Items.InsertObject(Index + I, Item, AObject);
    UpdateMRUList;
  end;
end;

procedure TOriComboBoxMRU.RemoveItem(const Item: TOriString);
var I: Integer;
begin
  if FMRUList.RemoveItem(Item) then UpdateMRUListModified;

  I := Items.IndexOf(Item);
  if (I > -1) then
  begin
    if ItemIndex = I then Text := '';
    Items.Delete(I);
    UpdateMRUList;
  end;
end;

procedure TOriComboBoxMRU.AddItemToMRUList(Index: Integer);
var
  I : Integer;
begin
  I := FMRUList.Items.Count;
  if (I > -1) and (Index > -1) then
  begin
    FMRUList.NewItem(Items[Index], Items.Objects[Index]);
    if FMRUList.Items.Count > I then
      Items.InsertObject(0, Items[Index], Items.Objects[Index]);
  end;
end;

procedure TOriComboBoxMRU.UpdateMRUList;
begin
  MRUListUpdate(FMRUList.Items.Count);
end;

// Use this to update MRUList after removing item from MRUList
procedure TOriComboBoxMRU.UpdateMRUListModified;
begin
  MRUListUpdate(FMRUList.Items.Count + 1);
end;

// удаляет из начала спика строки в количестве Count и вставляет туда MRU-лист
procedure TOriComboBoxMRU.MRUListUpdate(Count: Integer);
var
  I: Integer;
  Txt: TOriString;
begin
  Txt := Text;

  // the first items are part of the MRU list
  if Count > 0 then for I := 1 to Count do Items.Delete(0);

  // make sure the MRU list is limited to its maximum size
  FMRUList.Shrink;

  if FMRUList.Items.Count > 0 then
  begin
    // add the MRU list items to the beginning of the combo list
    for I := Pred(FMRUList.Items.Count) downto 0 do
      Items.InsertObject(0, FMRUList.Items[I], FMRUList.Items.Objects[I]);

    // this is necessary because we are always inserting item 0 and Windows
    // thinks that it knows the height of all other items, so it only sends
    // a WM_MEASUREITEM for item 0. We need the last item of the MRU list
    // to be taller so we can draw a separator under it

    // При выставлении высоты одного элемента отличной от высоты остальных
    // возникает странный эффект: если подвести курсор к последнему видимому
    // элементу списка, то список проматывается на несколько элементов вниз.
    // Иногда это происходит один раз, а иногда, дергая мышь вниз-вверх над
    // последними видимыми элементами можно мотать до самого конца списка.
    // При ItemHeight = 18 и стандартном 8 шрифте разделитель смотрится
    // нормально и без дополнительной высоты элемента.
    //SendMessage(Handle, CB_SETITEMHEIGHT, wParam(FMRUList.Items.Count-1), lParam(FItemHeight+cbxSeparatorHeight));
  end;

  ItemIndex :=
    {$IFDEF USE_UNICODE}SendMessageW{$ELSE}SendMessage{$ENDIF}
      (Handle, CB_FINDSTRINGEXACT, FMRUList.Items.Count-1, LongInt(POriChar(Txt)));
end;

procedure TOriComboBoxMRU.CNDrawItem(var Msg: TWMDrawItem);
begin
  // gather flag information that Borland left out
  FDrawingEdit := (ODS_COMBOBOXEDIT and Msg.DrawItemStruct.itemState) <> 0;
  inherited;
end;

procedure TOriComboBoxMRU.CNCommand(var Message: TWmCommand);
begin
  if Message.NotifyCode = CBN_CLOSEUP then
    if ItemIndex > -1 then
    begin
      Text := Items[ItemIndex];
      AddItemToMRUList(ItemIndex);
      UpdateMRUList;
      Click;
    end;
  inherited;
end;

procedure TOriComboBoxMRU.DoExit;
begin
  AddItemToMRUList(ItemIndex);
  inherited DoExit;
end;

procedure TOriComboBoxMRU.DrawItem(Index: Integer; ItemRect: TRect; State: TOwnerDrawState);
var
  SepRect    : TRect;
  TxtRect    : TRect;
  BkMode     : Integer;
begin
  with Canvas do
  begin

    if not (odSelected in State) then
      Brush.Color := clHighlight
    else
      if (FMRUList.Items.Count > 0) and (Index < FMRUList.Items.Count)
        then Brush.Color := FMRUListColor
        else Brush.Color := Color;

    FillRect(ItemRect);

    with ItemRect do TxtRect := Rect(Left + 2, Top, Right, Bottom);

    BkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
    {$IFDEF USE_UNICODE}DrawTextW{$ELSE}DrawText{$ENDIF}
      (Canvas.Handle, POriChar(Items[Index]), Length(Items[Index]), TxtRect, DT_VCENTER or DT_LEFT);

    SetBkMode(Canvas.Handle, BkMode);

    if (FMRUList.Items.Count > 0) // draw separator between MRU and items
    and (Index = FMRUList.Items.Count - 1) and DroppedDown then
    begin
      SepRect := ItemRect;
      SepRect.Top := SepRect.Bottom - cbxSeparatorHeight;
      Pen.Color := clGrayText;

      if not DrawingEdit then
        with SepRect do Rectangle(Left-1, Top, Right+1, Bottom);
    end;
  end;
end;

procedure TOriComboBoxMRU.SetDroppedWidth(Value : Integer);
begin
  if HandleAllocated then
    SendMessage(Handle, CB_SETDROPPEDWIDTH, Value, 0);
  FDroppedWidth := Value;
end;

function TOriComboBoxMRU.GetMRUListCount: Integer;
begin
  Result := FMRUList.MaxItems;
end;

procedure TOriComboBoxMRU.SetMRUListCount(Value: Integer);
begin
  if ([csDesigning, csLoading] * ComponentState) = [] then ClearMRUList;
  FMRUList.MaxItems := Value;
end;

procedure TOriComboBoxMRU.SetComboStyle(Value: TOriComboStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    RecreateWnd;
  end;
end;

procedure TOriComboBoxMRU.SetItemHeight(Value : Integer);
begin
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    RecreateWnd;
  end;
end;

procedure TOriComboBoxMRU.WMMeasureItem(var Message : TMessage);
begin
  with (PMeasureItemStruct(Message.lParam))^ do
  begin
    ItemWidth := ClientWidth;
    ItemHeight := FItemHeight;
  end;
end;

procedure TOriComboBoxMRU.WMPaint(var Message: TWMPaint);
var R: TRect;
begin
  inherited;
  // плоская отрисовка иммеет смысл тольк когда BevelKind = bvNone, тогда
  // в inherited выполняется только Paint от TWinControl'а. Если BevelKind <> bvNone
  // тогда в в inherited сначала отрисуется соответствующий bevel, и потом внутри
  // него плоская отрисовка (почему внутри (D10) непонятно - то ли при установке
  // bevel'а client-rect сужается)
  case FLook of
    oclFlat:
      with TControlCanvas.Create do
      try
        Control := Self;
        Lock;

        R := ClientRect;
        ExcludeClipRect(Handle, 2, 2, R.Right - 2 - ButtonWidth, R.Bottom - 2);

        if Enabled
          then Brush.Color := clWindow
          else Brush.Color := clBtnFace;
        Brush.Style := bsSolid;
        Pen.Color := clBtnShadow;
        Pen.Style := psSolid;
        Rectangle(R);

        R.Left := R.Right - ButtonWidth;
        R.Top := R.Top + 2;
        R.Bottom := R.Bottom - 2;
        R.Right := R.Right - 2;
        if Enabled
          then DrawFrameControl(Handle, R, DFC_SCROLL, DFCS_FLAT or DFCS_SCROLLCOMBOBOX)
          else DrawFrameControl(Handle, R, DFC_SCROLL, DFCS_FLAT or DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE);
      finally
        Unlock;
        Free;
      end;
  end;
end;

procedure TOriComboBoxMRU.SetLook(Value: TOriComboLook);
begin
  if Value <> FLook then
  begin
    FLook := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
{ TOriFontComboBox }

// callback used by FontIsSymbol()
{function GetFontCharSet(lpLF : PLogFont; lpTM : PTextMetric;
         FontType : Integer; lParam : LongInt): Integer; far; stdcall;
begin
  PByte(lParam)^ := lpLF^.lfCharSet;
  Result := 0;
end;}

// font family enumeration callbacks
function EnumFontFamProc(lpLF: PEnumLogFont; lpTM: PNewTextMetric;
         FontType: Integer; lParam: LongInt) : Integer; far; stdcall;
var
  FontCombo : TOriFontComboBox;
  Bitmap    : TBitmap;
begin
  FontCombo := TOriFontComboBox(lParam);
  with FontCombo do
  begin
    // Filter fonts according to properties
    if (FontType and TRUETYPE_FONTTYPE = TRUETYPE_FONTTYPE) then
    begin
      Bitmap := fcTTBitmap;
      if (FontCategories in [fcAll, fcTrueType]) then
      begin
        if ((lpLF^.elfLogFont.lfPitchAndFamily and VARIABLE_PITCH = VARIABLE_PITCH)
          and (FontPitchStyles in [fpAll, fpVariable]))
          or ((lpLF^.elfLogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH)
          and (FontPitchStyles in [fpAll, fpFixed])) then
          if Items.IndexOf(lpLF^.elfLogFont.lfFaceName) = -1 then
            Items.AddObject(lpLF^.elfLogFont.lfFaceName, Bitmap)
      end;
    end
    else
    begin
      Bitmap := fcDevBitmap;
      if (FontCategories in [fcAll, fcDevice]) then
      begin
        if ((lpLF^.elfLogFont.lfPitchAndFamily and VARIABLE_PITCH = VARIABLE_PITCH)
          and (FontPitchStyles in [fpAll, fpVariable]))
          or ((lpLF^.elfLogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH)
          and (FontPitchStyles in [fpAll, fpFixed])) then
          if Items.IndexOf(lpLF^.elfLogFont.lfFaceName) = -1 then
            Items.AddObject(lpLF^.elfLogFont.lfFaceName, Bitmap)
      end;
    end;
  end;
  Result := 1;
end;

function EnumPrinterFontFamProc(lpLF: PEnumLogFont; lpTM: PNewTextMetric;
         FontType: Integer; lParam: LongInt): Integer; far; stdcall;
var
  FontCombo : TOriFontComboBox;
  Bitmap    : TBitmap;
  FaceName  : TOriString;
begin
  FontCombo := TOriFontComboBox(lParam);
  FaceName  := lpLF^.elfFullName;
  with FontCombo do
  begin
    if Items.IndexOf(FaceName) > -1 then
    begin
      Result := 1;
      Exit;
    end;
    // Filter fonts according to properties
    if (FontType and TRUETYPE_FONTTYPE = TRUETYPE_FONTTYPE) then
    begin
      Bitmap := fcTTBitmap;
      if (FontCategories in [fcAll, fcTrueType]) then
        if ((lpLF^.elfLogFont.lfPitchAndFamily and VARIABLE_PITCH = VARIABLE_PITCH)
          and (FontPitchStyles in [fpAll, fpVariable]))
          or ((lpLF^.elfLogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH)
          and (FontPitchStyles in [fpAll, fpFixed])) then
          if Items.IndexOf(lpLF^.elfLogFont.lfFaceName) = -1 then
            Items.AddObject(lpLF^.elfLogFont.lfFaceName, Bitmap)
    end
    else
    begin
      Bitmap := fcDevBitmap;
      if (FontCategories in [fcAll, fcDevice]) then
        if ((lpLF^.elfLogFont.lfPitchAndFamily and VARIABLE_PITCH = VARIABLE_PITCH)
          and (FontPitchStyles in [fpAll, fpVariable]))
          or ((lpLF^.elfLogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH)
          and (FontPitchStyles in [fpAll, fpFixed])) then
          if Items.IndexOf(lpLF^.elfLogFont.lfFaceName) = -1 then
            Items.AddObject(lpLF^.elfLogFont.lfFaceName, Bitmap)
    end;
  end;
  Result := 1;
end;

{ TOriFontComboBox }

constructor TOriFontComboBox.Create(AOwner: TComponent);
begin
  inherited;

  FPreviewFont := False;
  FCategories := fcAll;
  FPitchStyles := fpAll;
  FListFontSize := Font.Size;

  fcTTBitmap   := TBitmap.Create;
  fcDevBitmap  := TBitmap.Create;

  //FPrinter := Printer();
end;

destructor TOriFontComboBox.Destroy;
begin
  fcTTBitmap.Free;
  fcDevBitmap.Free;

  inherited;
end;


procedure TOriFontComboBox.CMFontChange(var Message : TMessage);
var
  Item: string;
begin
  if ItemIndex > -1 then
  begin
    Item := Items[ItemIndex];
    Populate;
    ItemIndex := Items.IndexOf(Item);
    // the selected font is no longer valid
    if ItemIndex < 0 then Change;
  end
  else
  begin
    Populate;
    Change;
  end;
end;

type TControlAccess = class(TControl);

procedure TOriFontComboBox.DoPreview;
begin
  if (Assigned(FPreviewControl)) and (ItemIndex > - 1) then
    TControlAccess(FPreviewControl).Font.Name := Items[ItemIndex];
end;

procedure TOriFontComboBox.DrawItem(Index: Integer; ItemRect: TRect; State: TOwnerDrawState);
var
  Bitmap     : TBitmap;
  SepRect    : TRect;
  TxtRect    : TRect;
  BmpRect    : TRect;
  BkMode     : Integer;
  aTop       : Integer;
begin
  with Canvas do
  begin

    if odSelected in State then
      Brush.Color := clHighlight
    else
      if (FMRUList.Items.Count > 0) and (Index < FMRUList.Items.Count)
        then Brush.Color := FMRUListColor
        else Brush.Color := Color;

    FillRect(ItemRect);

    Bitmap := TBitmap(Items.Objects[Index]);
    if Assigned(Bitmap) then
    begin
      with ItemRect do
      begin
        aTop := ((Top+Bottom) div 2) - (Bitmap.Height div 2);
        BmpRect := Rect(Left+2, aTop, Left+Bitmap.Width+2, aTop+Bitmap.Height);
      end;
      BrushCopy(BmpRect, Bitmap, Rect(0, 0, Bitmap.Width, Bitmap.Height), clYellow);
    end;

    with ItemRect do
      TxtRect := Rect(Left+Bitmap.Width+6, Top+1, Right, Bottom);

    BkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
    if FPreviewFont then
      {if Assigned(FPrinter) and (FPrinter.Printers.Count > 0) then
      begin
        // Do not draw symbol font names with the actual font - use the default
        if not FontIsSymbol(Items[Index])
        then} Font.Name := Items[Index]
        {else Font.Name := Self.Font.Name;
      end};
    Font.Size := FListFontSize;
    {$IFDEF USE_UNICODE}DrawTextW{$ELSE}DrawText{$ENDIF}
      (Canvas.Handle, POriChar(Items[Index]), Length(Items[Index]), TxtRect, DT_VCENTER or DT_LEFT);
    SetBkMode(Canvas.Handle, BkMode);

    if (FMRUList.Items.Count > 0) and (Index = FMRUList.Items.Count - 1) then
    begin
      SepRect := ItemRect;
      SepRect.Top := SepRect.Bottom - cbxSeparatorHeight;
      Pen.Color := clGrayText;
      if not DrawingEdit then
        with SepRect do Rectangle(Left-1, Top, Right+1, Bottom);
    end;
  end;
end;

{function TFontComboBoxAdv.FontIsSymbol(const FontName: TFlatString): Boolean;
var
  CharSet: Byte;
  DC: hDC;
  FntStr: array [0..63] of char;
begin
  DC := GetDC(0);
  CharSet := 0;
  StrPCopy(FntStr, FontName);
  try
    EnumFonts(DC, FntStr, @GetFontCharSet, PChar(@CharSet));
    if CharSet = 0 then // It's a printer font
      EnumFonts(Printer.Handle, FntStr, @GetFontCharSet, PChar(@CharSet));
  finally
    ReleaseDC(0, DC);
  end;
  Result := (CharSet = SYMBOL_CHARSET);
end;}

function TOriFontComboBox.GetFontName: TOriString;
begin
  if ItemIndex > -1
    then Result := Items[ItemIndex]
    else Result := '';
end;

procedure TOriFontComboBox.Loaded;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) then
  begin
    fcTTBitmap.LoadFromResourceName(HInstance, 'TRUETYPEFONT');
    fcDevBitmap.LoadFromResourceName(HInstance, 'DEVICEFONT');
    Populate;
  end;
end;

procedure TOriFontComboBox.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited Notification(Component, Operation);

  if (Component is TControl) and (Operation = opRemove) then
    if (Component as TControl) = FPreviewControl then
      PreviewControl := nil;
end;

procedure TOriFontComboBox.Populate;
var
  DC : HDC;
  TempList : TOriStringList;
begin
  Clear;
  DC := GetDC(0);
  TempList := TOriStringList.Create;
  try
    EnumFontFamilies(DC, nil, @EnumFontFamProc, Integer(Self));
    try
      if (Printer.Printers.Count > 0) and (Printer.Handle > 0) then
        EnumFontFamilies(Printer.Handle, nil, @EnumPrinterFontFamProc, Integer(Self));
    except
      // при недоступном сетевом принтере возникает ошибка:
      // EPrinter: Printer selected is not valid
    end;
    TempList.Assign(Items);
    TempList.Sort;
    Items.Assign(TempList);
  finally
    ReleaseDC(0, DC);
    TempList.Free;
  end;
end;

procedure TOriFontComboBox.Change;
begin
  inherited;
  DoPreview;
end;

procedure TOriFontComboBox.SetPreviewControl(Value: TControl);
begin
  if Value <> Self then
  begin
    FPreviewControl := Value;
    DoPreview;
  end;
end;

procedure TOriFontComboBox.SetFontName(const FontName: TOriString);
var I: Integer;
begin
  I := Items.IndexOf(FontName);
  if I > -1 then ItemIndex := I;
end;
*)
end.
