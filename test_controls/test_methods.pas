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

unit Test_Methods;

{$mode objfpc}{$H+}

interface

uses
  FpcUnit, TestRegistry;

type
  TTest_Methods = class(TTestCase)
  private
    procedure TextRectPaint(Sender: TObject);
  published
    procedure TextRect;
  end;

implementation

uses
  Classes, Controls, ExtCtrls, Forms, Graphics;

procedure TTest_Methods.TextRect;
var
  W: TForm;
  pb: TPaintBox;
begin
  W := TForm.CreateNew(Application.MainForm);
  W.Width := 400;
  W.Height := 400;
  pb := TPaintBox.Create(W);
  pb.Parent := W;
  pb.SetBounds(100, 100, 200, 200);
  pb.Anchors := [akTop, akLeft, akRight, akBottom];
  pb.OnPaint := @TextRectPaint;
  W.Show;
end;

procedure TTest_Methods.TextRectPaint(Sender: TObject);
var
  pb: TPaintBox;
  Style: TTextStyle;
  TxtRect: TRect;
begin
  pb := TPaintBox(Sender);
  TxtRect.Left := pb.Width div 4;
  TxtRect.Top := pb.Height div 4;
  TxtRect.Right := TxtRect.Left * 3;
  TxtRect.Bottom := TxtRect.Top * 3;
  pb.Canvas.Pen.Style := psSolid;
  pb.Canvas.Pen.Color := clRed;
  pb.Canvas.Rectangle(0, 0, pb.Width, pb.Height);
  pb.Canvas.Pen.Color := clBlue;
  pb.Canvas.Rectangle(TxtRect);

  Style.Opaque := True; // warning suppress
  FillChar(Style, SizeOf(TTextStyle), 0);
  Style.SingleLine := True;

  {%region 'Horizontal'}
  Style.Alignment := taCenter;
  Style.Layout := tlCenter;
  pb.Canvas.Font.Color := clBlack;
  pb.Canvas.TextRect(TxtRect, 0, 0, 'taCenter,tlCenter', Style);

  Style.Alignment := taLeftJustify;
  Style.Layout := tlCenter;
  pb.Canvas.Font.Color := clRed;
  pb.Canvas.TextRect(TxtRect, TxtRect.Left, 0, 'taLeft,tlCenter', Style);

  Style.Alignment := taRightJustify;
  Style.Layout := tlCenter;
  pb.Canvas.Font.Color := clBlue;
  pb.Canvas.TextRect(TxtRect, 0, 0, 'taRight,tlCenter', Style);


  Style.Alignment := taCenter;
  Style.Layout := tlTop;
  pb.Canvas.Font.Color := clFuchsia;
  pb.Canvas.TextRect(TxtRect, 0, TxtRect.Top, 'taCenter,tlTop', Style);

  Style.Alignment := taLeftJustify;
  Style.Layout := tlTop;
  pb.Canvas.Font.Color := clMaroon;
  pb.Canvas.TextRect(TxtRect, TxtRect.Left, TxtRect.Top, 'taLeft,tlTop', Style);

  Style.Alignment := taRightJustify;
  Style.Layout := tlTop;
  pb.Canvas.Font.Color := clTeal;
  pb.Canvas.TextRect(TxtRect, 0, TxtRect.Top, 'taRight,tlTop', Style);


  Style.Alignment := taCenter;
  Style.Layout := tlBottom;
  pb.Canvas.Font.Color := clGreen;
  pb.Canvas.TextRect(TxtRect, 0, 0, 'taCenter,tlBottom', Style);

  Style.Alignment := taLeftJustify;
  Style.Layout := tlBottom;
  pb.Canvas.Font.Color := clNavy;
  pb.Canvas.TextRect(TxtRect, TxtRect.Left, 0, 'taLeft,tlBottom', Style);

  Style.Alignment := taRightJustify;
  Style.Layout := tlBottom;
  pb.Canvas.Font.Color := clOlive;
  pb.Canvas.TextRect(TxtRect, 0, 0, 'taRight,tlBottom', Style);
  {%endregion}

  {%region 'Bottom to top'}
  // TODO: сделать процедуру для вывода вертикального текста
  // или найти готовую. Поведение Canvas.TextRect неожиданное
  // при повернутом шрифте и отличается от то того что делает
  // виндовая процедура с аналогичными настройками (проверить)
  pb.Canvas.Font.Orientation := 900;

  Style.Alignment := taCenter;
  Style.Layout := tlCenter;
  pb.Canvas.Font.Color := clBlack;
  pb.Canvas.TextRect(TxtRect, 0, 0, 'taCenter,tlCenter', Style);

  Style.Alignment := taLeftJustify;
  Style.Layout := tlCenter;
  pb.Canvas.Font.Color := clRed;
  pb.Canvas.TextRect(TxtRect, TxtRect.Left, 0, 'taLeft,tlCenter', Style);

  Style.Alignment := taRightJustify;
  Style.Layout := tlCenter;
  pb.Canvas.Font.Color := clBlue;
  pb.Canvas.TextRect(TxtRect, 0, 0, 'taRight,tlCenter', Style);


  Style.Alignment := taCenter;
  Style.Layout := tlTop;
  pb.Canvas.Font.Color := clFuchsia;
  pb.Canvas.TextRect(TxtRect, 0, TxtRect.Top, 'taCenter,tlTop', Style);

  Style.Alignment := taLeftJustify;
  Style.Layout := tlTop;
  pb.Canvas.Font.Color := clMaroon;
  pb.Canvas.TextRect(TxtRect, TxtRect.Left, TxtRect.Top, 'taLeft,tlTop', Style);

  Style.Alignment := taRightJustify;
  Style.Layout := tlTop;
  pb.Canvas.Font.Color := clTeal;
  pb.Canvas.TextRect(TxtRect, 0, TxtRect.Top, 'taRight,tlTop', Style);


  Style.Alignment := taCenter;
  Style.Layout := tlBottom;
  pb.Canvas.Font.Color := clGreen;
  pb.Canvas.TextRect(TxtRect, 0, 0, 'taCenter,tlBottom', Style);

  Style.Alignment := taLeftJustify;
  Style.Layout := tlBottom;
  pb.Canvas.Font.Color := clNavy;
  pb.Canvas.TextRect(TxtRect, TxtRect.Left, 0, 'taLeft,tlBottom', Style);

  Style.Alignment := taRightJustify;
  Style.Layout := tlBottom;
  pb.Canvas.Font.Color := clOlive;
  pb.Canvas.TextRect(TxtRect, 0, 0, 'taRight,tlBottom', Style);
  {%endregion}
end;

initialization
  RegisterTest(TTest_Methods);
end.

