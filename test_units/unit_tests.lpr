program Unit_Tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  Tests_OriMath, Tests_OriStrings;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

