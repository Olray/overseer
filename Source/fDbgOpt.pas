{==============================================================================|
| Project : Delphree - Nexus - Overseer                          | 001.002.000 |
|==============================================================================|
| Content: Overseer Options dialog.                                            |
|==============================================================================|
| The contents of this file are subject to the Mozilla Public License Ver. 1.0 |
| (the "License"); you may not use this file except in compliance with the     |
| License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ |
|                                                                              |
| Software distributed under the License is distributed on an "AS IS" basis,   |
| WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for |
| the specific language governing rights and limitations under the License.    |
|==============================================================================|
| The Original Code is Overseer for Nexus Delphi Library.                      |
|==============================================================================|
| The Initial Developer of the Original Code is Pavel Cisar (Czech Republic).  |
| Portions created by Pavel Cisar are Copyright (C) 1998, 1999.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: 1.2.1999 Version 001.000.000 (Pavel Cisar)                          |
|          First public release.                                               |
|          7.2.1999 Version 001.001.000 (Pavel Cisar)                          |
|          CodeSite(tm) segregation.                                           |
|          14.2.1999 Version 001.002.000 (Pavel Cisar)                         |
|          Added stay on top option.                                           |
|==============================================================================}

unit fDbgOpt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, RXCtrls, StdCtrls, ExtCtrls, Mask;

type
  TfrmOptions = class(TForm)
    pgOptions: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    chlbSections: TRxCheckListBox;
    pnButtons: TPanel;
    btnCancel: TButton;
    btnApply: TButton;
    btnOK: TButton;
    lblIndent: TLabel;
    edIndent: TMaskEdit;
    chkInsertTop: TCheckBox;
    chkStayOnTop: TCheckBox;
    procedure edIndentChange(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private
    { Private declarations }
    FChanged : boolean ;
  public
    { Public declarations }
    procedure LoadSettings ;
    procedure SaveSettings ;
  end;

var
  frmOptions: TfrmOptions;

implementation

uses fDebug, Registry;

{$R *.DFM}

procedure TfrmOptions.LoadSettings ;
var I : integer ;
begin
  edIndent.Text := IntToStr (frmDebugger.IndentLevel) ;
  for I := 0 to frmDebugger.Sections.Count-1 do begin
    chlbSections.Items.Add ('('+frmDebugger.Sections.Names[I]+') '+frmDebugger.Sections.Values[frmDebugger.Sections.Names[I]]) ;
    chlbSections.Checked[I] := longbool (frmDebugger.SectionsEnabled.Items[I]) ;
  end ;
  FChanged := false ;
  chkInsertTop.Checked := frmDebugger.InsertTop ;
  chkStayOnTop.Checked := frmDebugger.StayOnTop ;
  btnApply.Enabled := false ;
end;

procedure TfrmOptions.SaveSettings ;
var I : integer ;
begin
  frmDebugger.IndentLevel := StrToInt (Trim (edIndent.Text)) ;
  for I := 0 to frmDebugger.Sections.Count-1 do begin
    frmDebugger.SectionsEnabled.Items[I] := pointer (longbool (chlbSections.Checked[I])) ;
  end ;
  FChanged := false ;
  btnApply.Enabled := false ;
  frmDebugger.SendEnabledSections ;
  frmDebugger.InsertTop := chkInsertTop.Checked ;
  frmDebugger.StayOnTop := chkStayOnTop.Checked ;
  frmDebugger.SaveSettings ;
end;

procedure TfrmOptions.edIndentChange(Sender: TObject);
begin
  FChanged := true ;
  btnApply.Enabled := true ;
end;

procedure TfrmOptions.btnApplyClick(Sender: TObject);
begin
  SaveSettings ;
end;

end.
