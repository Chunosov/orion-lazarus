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

unit Tests_OriMath;

{$mode objfpc}{$H+}

interface

uses
  FpcUnit, TestRegistry;

type
  TTest_OriMath = class(TTestCase)
  published
    procedure StringFromFloat;
  end;

implementation

uses
  SysUtils,
  OriStrings, OriMath;

procedure TTest_OriMath.StringFromFloat;
var
  V: Double;
  S: String;
  D: Char;
begin
  D := DefaultFormatSettings.DecimalSeparator;

  V := 12456.7556;

  S := OriMath.StringFromFloat(V, 3, 3, False);
  AssertEquals(ReplaceChar('1,246E+4', ',', D), S);

  S := OriMath.StringFromFloat(V, 4, 3, False);
  AssertEquals(ReplaceChar('1,246E+4', ',', D), S);

  S := OriMath.StringFromFloat(V, 5, 3, False);
  AssertEquals(ReplaceChar('12456,756', ',', D), S);

  V := 0.0012;

  S := OriMath.StringFromFloat(V, 4, 3, False);
  AssertEquals(ReplaceChar('0,001', ',', D), S);

  S := OriMath.StringFromFloat(V, 3, 2, True);
  AssertEquals(ReplaceChar('1,20E-3', ',', D), S);

  S := OriMath.StringFromFloat(V, 3, 2, False);
  AssertEquals(ReplaceChar('1,2E-3', ',', D), S);
end;

initialization
  RegisterTest(TTest_OriMath);
end.

