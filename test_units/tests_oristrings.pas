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

unit Tests_OriStrings;

{$mode objfpc}{$H+}

interface

uses
  FpcUnit, TestRegistry;

type
  TTest_OriStrings = class(TTestCase)
  published
    procedure SplitStr;
  end;

implementation

uses
  OriStrings;

procedure TTest_OriStrings.SplitStr;
var
  S: String;
  R: TStringArray;
begin
  S := 'C1 λ_min,рус; ЉϢȠЂӔӜ   ڝڶڥڰڇکگ';
  R := OriStrings.SplitStr(S, ';, ');
  AssertEquals(5, Length(R));
  AssertEquals('C1', R[0]);
  AssertEquals('λ_min', R[1]);
  AssertEquals('рус', R[2]);
  AssertEquals('ЉϢȠЂӔӜ', R[3]);
  AssertEquals('ڝڶڥڰڇکگ', R[4]);
end;

initialization
  RegisterTest(TTest_OriStrings);
end.

