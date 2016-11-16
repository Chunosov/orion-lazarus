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

