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

unit OriUtils_TChart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, TAGraph, TASeries, TACustomSeries,
  OriIniFile;

type
  TChartHelper = class helper for TChart
  public
    function HasSeries(ASeries: TBasicChartSeries): Boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  TChartSeriesHelper = class helper for TChartSeries
  public
    procedure SetXY(Index: Integer; const X, Y: Double); inline;
  end;

  TCopyImageFormat = (cifWmf, cifEmf, cifBmp);

  TChartPenSettings = record
    Visible: Boolean;
    Color: TColor;
    Style: TPenStyle;
    SmallDots: Boolean;
    Width: Integer;
  end;

procedure ChooseSeriesColor(Series: TLineSeries);
function GetLineSeriesColor(Chart: TChart): TColor;
function GetRandomLineSeriesColor(Chart: TChart): TColor;
function GetRandomSeriesColor: TColor;

procedure SetChartPen(var Pen: TChartPenSettings; Visible: Boolean; Color: TColor;
  Style: TPenStyle; SmallDots: Boolean; Width: Integer); inline; overload;

procedure WriteChartPen(Ini: TOriIniFile; const Key: String; var Pen: TChartPenSettings);
procedure ReadChartPen(Ini: TOriIniFile; const Key: String; var Pen: TChartPenSettings);

implementation

uses
  OriGraphics, OriStrings;

{%region TChartHelper}
function TChartHelper.HasSeries(ASeries: TBasicChartSeries): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Self.Series.Count-1 do
    if Self.Series[I] = ASeries then
    begin
      Result := True;
      exit;
    end;
end;

procedure TChartHelper.BeginUpdate;
var I: Integer;
begin
  for I := 0 to Series.Count-1 do
    if Series[I] is TLineSeries then
      TLineSeries(Series[I]).BeginUpdate;
end;

procedure TChartHelper.EndUpdate;
var I: Integer;
begin
  for I := 0 to Series.Count-1 do
    if Series[I] is TLineSeries then
      TLineSeries(Series[I]).EndUpdate;
end;
{%endregion}

{%region TChartSeriesHelper}
procedure TChartSeriesHelper.SetXY(Index: Integer; const X, Y: Double);
begin
  SetXValue(Index, X);
  SetYValue(Index, Y);
end;
{%endregion}

{%region Series Colors}

const
  SeriesColors: array[0..50] of TColor = (clRed, clGreen, clBlue, clBlack,
    clMaroon, clNavy, clOlive, clPurple, clTeal, clGray, clLime, clFuchsia,
    clAqua, clMediumVioletRed, clDarkRed, clBrown, clDarkGreen, clDarkCyan,
    clMidnightBlue, clDarkSlateGray, clDarkSlateBlue, clDarkOrange, clFireBrick,
    clDarkKhaki, clSienna, clPaleVioletRed, clSeaGreen, clCadetBlue, clRoyalBlue,
    clSlateBlue, clSlateGray, clDeepPink, clCrimson, clSaddleBrown, clDarkOliveGreen,
    clLightSeaGreen, clSteelBlue, clOrangeRed, clIndigo, clDimGray, clIndianRed,
    clDarkGoldenrod, clDarkSeaGreen, clTurquoise, clDodgerBlue, clDarkViolet,
    clDarkSalmon, clRosyBrown, clMediumSpringGreen, clMediumAquamarine, clViolet);

procedure ChooseSeriesColor(Series: TLineSeries);
begin
  if Assigned(Series.Owner) and (Series.Owner is TChart)
    then Series.LinePen.Color := GetLineSeriesColor(TChart(Series.Owner))
    else Series.LinePen.Color := GetRandomSeriesColor;
end;

function LineSeriesColorExists(Chart: TChart; Color: TColor): Boolean;
var
  I: Integer;
begin
  for I := 0 to Chart.Series.Count-1 do
    if Chart.Series[I] is TLineSeries then
      if TLineSeries(Chart.Series[I]).SeriesColor = Color then
      begin
        Result := True;
        exit;
      end;
  Result := False;
end;

function GetLineSeriesColor(Chart: TChart): TColor;
var
  I: Integer;
begin
  for I := Low(SeriesColors) to High(SeriesColors) do
    if not LineSeriesColorExists(Chart, SeriesColors[I]) then
    begin
      Result := SeriesColors[I];
      exit;
    end;
  Result := SeriesColors[Random(Length(SeriesColors))];
end;

function GetRandomLineSeriesColor(Chart: TChart): TColor;
var
  I, Start: Integer;
begin
  Start := Random(Length(SeriesColors));
  for I := Start to High(SeriesColors) do
    if not LineSeriesColorExists(Chart, SeriesColors[I]) then
    begin
      Result := SeriesColors[I];
      exit;
    end;
  for I := Low(SeriesColors) to Start-1 do
    if not LineSeriesColorExists(Chart, SeriesColors[I]) then
    begin
      Result := SeriesColors[I];
      exit;
    end;
  Result := SeriesColors[Start];
end;

function GetRandomSeriesColor: TColor;
begin
  Result := SeriesColors[Random(Length(SeriesColors))];
end;
{%endregion}

{%region Storage}
procedure SetChartPen(var Pen: TChartPenSettings; Visible: Boolean;
  Color: TColor; Style: TPenStyle; SmallDots: Boolean; Width: Integer);
begin
  Pen.Visible := Visible;
  Pen.Color := Color;
  Pen.Style := Style;
  Pen.SmallDots := SmallDots;
  Pen.Width := Width;
end;

procedure WriteChartPen(Ini: TOriIniFile; const Key: String; var Pen: TChartPenSettings);
begin
  with Pen do Ini.WriteString(Key, Format('%d;%d;%d;$%s;%d', [Ord(Visible), Ord(Style), Width, IntToHex(Color, 8), Ord(SmallDots)]));
end;

procedure ReadChartPen(Ini: TOriIniFile; const Key: String; var Pen: TChartPenSettings);
var Parts: TStringArray;
begin
  SplitStr(Ini.ReadString(Key, ''), ';', Parts);
  if Length(Parts) > 4 then
    with Pen do
    begin
      Visible   := StrToBoolDef(Parts[0], Visible);
      Style     := TPenStyle(StrToIntDef(Parts[1], Ord(Style)));
      Width     := StrToIntDef(Parts[2], Width);
      Color     := StrToIntDef(Parts[3], Color);
      SmallDots := StrToBoolDef(Parts[4], Visible);
    end;
end;
{%endregion}

end.

