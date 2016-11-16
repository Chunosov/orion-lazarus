{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Orion;

interface

uses
  OriTabs, DesignTime, OriEditors, OriDebugConsole, OriGraphics, OriIniFile, 
  OriMath, OriUndo, OriUtils, OriUtils_Gui, VistaDialogs, OriHelp, 
  ObjectFilter, OriStrings, OriControlsBase, OriDialogs, OriXmlFile, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DesignTime', @DesignTime.Register);
end;

initialization
  RegisterPackage('Orion', @Register);
end.
