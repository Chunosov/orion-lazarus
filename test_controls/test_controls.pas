unit Test_Controls;

{$mode objfpc}{$H+}

interface

uses
  FpcUnit, TestRegistry, Controls;

type
  TTest_Controls = class(TTestCase)
  private
    procedure ShowControlSimple(AControl: TControl);
  published
    procedure OriTabSet;
    procedure OriFloatEdit;
    //procedure OriComboBox;
  end;

implementation

uses
  WinPropEditor, OriTabs, OriEditors;

procedure TTest_Controls.OriTabSet;
var
  TabSet: TOriTabSet;
  W: TWndPropEditor;
begin
  W := ShowPropEditor;
  TabSet := TOriTabSet.Create(nil);
  TabSet.Parent := W;
  TabSet.Align := alClient;
  TabSet.Tabs.Add.Caption := 'Tab 1';
  TabSet.Tabs.Add.Caption := 'Tab 2';
  TabSet.Tabs.Add.Caption := 'Tab 3';
  W.SetSelection(TabSet);
end;

procedure TTest_Controls.ShowControlSimple(AControl: TControl);
var
  W: TWndPropEditor;
begin
  W := ShowPropEditor;
  AControl.Parent := W;
  AControl.Left := 20;
  AControl.Top := 20;
  AControl.Width := 100;
  W.SetSelection(AControl);
end;

procedure TTest_Controls.OriFloatEdit;
begin
  ShowControlSimple(TOriFloatEdit.Create(nil));
end;

//procedure TTest_Controls.OriComboBox;
//begin
//  ShowControlSimple(TOriComboBox.Create(nil));
//end;

initialization
  RegisterTest(TTest_Controls);
end.

