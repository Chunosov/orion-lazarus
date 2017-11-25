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

unit DesignTime;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources,
  OriTabs, OriEditors;

procedure Register;

implementation

const
  ORION_COMPONENTS_PAGE = 'Orion';

procedure Register;
begin
  RegisterComponents(ORION_COMPONENTS_PAGE, [TOriTabSet, TOriFloatEdit]);
end;

initialization
  {$I icons.lrs}
end.

