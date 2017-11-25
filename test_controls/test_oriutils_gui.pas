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

unit Test_OriUtils_Gui;

{$mode objfpc}{$H+}

interface

uses
  FpcUnit, TestRegistry;

type
  TTest_OriUtils_Gui = class(TTestCase)
  published
    procedure ErrorDlg;
  end;

implementation

uses
  OriUtils_Gui;

procedure TTest_OriUtils_Gui.ErrorDlg;
begin
  OriUtils_Gui.ErrorDlg('Test error message');
end;

initialization
  RegisterTest(TTest_OriUtils_Gui);
end.

