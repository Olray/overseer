{==============================================================================|
| Project : Delphree - Nexus - Overseer                          | 001.000.001 |
|==============================================================================|
| Content: Overseer Demo application.                                          |
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
|          7.2.1999 Version 001.000.001 (Pavel Cisar)                          |
|          CodeSite(tm) segregation.                                           |
|==============================================================================}

program DbgDemo;

uses
  Forms,
  fMain in 'fMain.pas' {frmMain},
  uDbg in '..\Lib\uDbg.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'CodeOverseer Demo Application';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
