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

unit OriControlsBase;

{$mode objfpc}{$H+}

interface

uses
  Controls, Graphics, LCLType;

{type
  TButtonPaintInfo = packed record
    Parent: TWinControl;
    Window: HWND;
    Canvas: TCanvas;
    Rect: TRect;
    Pressed: Boolean;
    Disabled: Boolean;
    Focused: Boolean;
    Default: Boolean;
    Hover: Boolean;
    Checked: Boolean;
  end;

  TOriMonoGlyph = class
  protected
    Width: Integer;
    Height: Integer;
    Bitmap, OldBitmap: HBitmap;
    BitmapDC: HDC;
  public
    constructor Create(AWidth, AHeight: Integer; const Bits);
    destructor Destroy; override;
    procedure Draw(DC: HDC; X, Y: Integer; Color: TColor);
  end;}

type
  TOriButtonMonoGlyph = (bgNone, bgClose, bgCloseSmall, bgCloseThin, bgMaximize,
    bgMinimize, bgCascade, bgTriangleDown, bgTriangleUp, bgTriangleRight,
    bgTriangleLeft, bgQuoteDown, bgQuoteUp, bgQuoteRight, bgQuoteLeft,
    bgClipOn, bgClipOff, bgArrowDown, bgArrowUp, bgArrowRight,
    bgArrowLeft, bgEllipsis, bgCheck, bgPlus, bgMinus, bgMultiply, bgDivide,
    bgTriLeftSmall, bgTriRightSmall, bgFontBold, bgFontItalic, bgFontUnderline);

implementation

const
  MonoGlyphW = 10;
  MonoGlyphH = 10;
  MonoGlyphSize = 20;

{$region MonoGlyphs}
const MonoGlyphs: array [0..MonoGlyphSize*Ord(High(TOriButtonMonoGlyph))-1] of Byte = (
    {bgClose}         $C1, $80, $E3, $80, $77, $00, $3E, $00, $1C, $00, $3E, $00, $77, $00, $E3, $80, $C1, $80, $00, $00,
    {bgCloseSmall}    $00, $00, $00, $00, $61, $80, $33, $00, $1E, $00, $0C, $00, $1E, $00, $33, $00, $61, $80, $00, $00,
    {bgCloseThin}     $00, $00, $00, $00, $21, $00, $12, $00, $0C, $00, $0C, $00, $12, $00, $21, $00, $00, $00, $00, $00,
    {bgMaximize}      $FF, $C0, $FF, $C0, $80, $40, $80, $40, $80, $40, $80, $40, $80, $40, $80, $40, $80, $40, $FF, $C0,
    {bgMinimize}      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $7F, $00, $7F, $00, $00, $00,
    {bgCascade}       $3F, $C0, $3F, $C0, $20, $40, $FF, $40, $FF, $40, $81, $C0, $81, $00, $81, $00, $FF, $00, $00, $00,
    {bgTriangleDown}  $00, $00, $00, $00, $00, $00, $FF, $C0, $7F, $80, $3F, $00, $1E, $00, $0C, $00, $00, $00, $00, $00,
    {bgTriangleUp}    $00, $00, $00, $00, $00, $00, $0C, $00, $1E, $00, $3F, $00, $7F, $80, $FF, $C0, $00, $00, $00, $00,
    {bgTriangleRight} $20, $00, $30, $00, $38, $00, $3C, $00, $3E, $00, $3E, $00, $3C, $00, $38, $00, $30, $00, $20, $00,
    {bgTriangleLeft}  $02, $00, $06, $00, $0E, $00, $1E, $00, $3E, $00, $3E, $00, $1E, $00, $0E, $00, $06, $00, $02, $00,
    {bgQuoteDown}     $C0, $C0, $61, $80, $33, $00, $1E, $00, $CC, $C0, $61, $80, $33, $00, $1E, $00, $0C, $00, $00, $00,
    {bgQuoteUp}       $0C, $00, $1E, $00, $33, $00, $61, $80, $CC, $C0, $1E, $00, $33, $00, $61, $80, $C0, $C0, $00, $00,
    {bgQuoteRight}    $88, $00, $CC, $00, $66, $00, $33, $00, $19, $80, $19, $80, $33, $00, $66, $00, $CC, $00, $88, $00,
    {bgQuoteLeft}     $08, $80, $19, $80, $33, $00, $66, $00, $CC, $00, $CC, $00, $66, $00, $33, $00, $19, $80, $08, $80,
    {bgClipOn}        $3E, $00, $26, $00, $26, $00, $26, $00, $26, $00, $7F, $00, $08, $00, $08, $00, $08, $00, $08, $00,
    {bgClipOff}       $00, $00, $08, $00, $0F, $C0, $08, $40, $F8, $40, $0F, $C0, $0F, $C0, $08, $00, $00, $00, $00, $00,
    {bgArrowDown}     $1E, $00, $1E, $00, $1E, $00, $1E, $00, $1E, $00, $FF, $C0, $7F, $80, $3F, $00, $1E, $00, $0C, $00,
    {bgArrowUp}       $0C, $00, $1E, $00, $3F, $00, $7F, $80, $FF, $C0, $1E, $00, $1E, $00, $1E, $00, $1E, $00, $1E, $00,
    {bgArrowRight}    $04, $00, $06, $00, $07, $00, $FF, $80, $FF, $C0, $FF, $C0, $FF, $80, $07, $00, $06, $00, $04, $00,
    {bgArrowLeft}     $08, $00, $18, $00, $38, $00, $7F, $C0, $FF, $C0, $FF, $C0, $7F, $C0, $38, $00, $18, $00, $08, $00,
    {bgEllipsis}      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $CC, $C0, $CC, $C0, $00, $00, $00, $00, $00, $00,
    {bgCheck}         $00, $00, $00, $00, $01, $00, $03, $00, $47, $00, $6E, $00, $7C, $00, $38, $00, $10, $00, $00, $00,
    {bgPlus}          $00, $00, $0C, $00, $0C, $00, $0C, $00, $7F, $80, $7F, $80, $0C, $00, $0C, $00, $0C, $00, $00, $00,
    {bgMinus}         $00, $00, $00, $00, $00, $00, $00, $00, $7F, $80, $7F, $80, $00, $00, $00, $00, $00, $00, $00, $00,
    {bgMultiply}      $00, $00, $63, $00, $77, $00, $3E, $00, $1C, $00, $3E, $00, $77, $00, $63, $00, $00, $00, $00, $00,
    {bgDivide}        $00, $00, $0C, $00, $0C, $00, $00, $00, $7F, $80, $7F, $80, $00, $00, $0C, $00, $0C, $00, $00, $00,
    {bgTriLeftSmall}  $00, $00, $00, $00, $02, $00, $06, $00, $0E, $00, $1E, $00, $0E, $00, $06, $00, $02, $00, $00, $00,
    {bgTriRightSmall} $00, $00, $00, $00, $10, $00, $18, $00, $1C, $00, $1E, $00, $1C, $00, $18, $00, $10, $00, $00, $00,
    {bgFontBold}      $FF, $00, $73, $80, $73, $80, $73, $80, $7F, $00, $73, $80, $73, $80, $73, $80, $73, $80, $FF, $00,
    {bgFontItalic}    $1F, $C0, $07, $00, $07, $00, $0E, $00, $0E, $00, $1C, $00, $1C, $00, $38, $00, $38, $00, $FE, $00,
    {bgFontUnderline} $F7, $80, $63, $00, $63, $00, $63, $00, $63, $00, $63, $00, $63, $00, $3E, $00, $00, $00, $FF, $80
  );
{$endregion}

{$region TOriMonoGlyph}
{constructor TOriMonoGlyph.Create(AWidth, AHeight: Integer; const Bits);
begin
  Width := AWidth;
  Height := AHeight;
  Bitmap := CreateBitmap(Width, Height, 1, 1, @Bits);
  BitmapDC := CreateCompatibleDC(0);
  OldBitmap := SelectObject(BitmapDC, Bitmap);
end;

destructor TOriMonoGlyph.Destroy;
begin
  SelectObject(BitmapDC, OldBitmap);
  DeleteDC(BitmapDC);
  DeleteObject(Bitmap);
  inherited;
end;

procedure TOriMonoGlyph.Draw(DC: HDC; X, Y: Integer; Color: TColor);
var
  OldTextColor, OldBkColor: TColorRef;
  OldBrush, Brush: HBRUSH;
begin
  if Color = clNone then Exit;
  OldTextColor := SetTextColor(DC, $FFFFFF);
  OldBkColor := SetBkColor(DC, $000000);
  if Color < 0 then Brush := GetSysColorBrush(Color and $000000FF)
  else Brush := CreateSolidBrush(Color);
  OldBrush := SelectObject(DC, Brush);
  BitBlt(DC, X, Y, Width, Height, BitmapDC, 0, 0, $B8074A);
  SelectObject(DC, OldBrush);
  DeleteObject(Brush);
  SetBkColor(DC, OldBkColor);
  SetTextColor(DC, OldTextColor);
end;}
{$endregion}

end.

