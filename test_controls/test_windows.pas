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

unit Test_Windows;

{$mode objfpc}{$H+}

interface

uses
  FpcUnit, TestRegistry;

type
  TTest_Windows = class(TTestCase)
  published
    procedure ColorMixer;
  end;

implementation

uses
  WinColorMixer;

procedure TTest_Windows.ColorMixer;
begin
  with TWndTestColors.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

initialization
  RegisterTest(TTest_Windows);
end.

