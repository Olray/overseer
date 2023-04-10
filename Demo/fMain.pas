{==============================================================================|
| Project : Delphree - Nexus - Overseer                          | 001.000.001 |
|==============================================================================|
| Content: Overseer Demo main form.                                            |
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

unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfrmMain = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    btnFuncTest: TButton;
    btnComponentTest: TButton;
    btnData: TButton;
    btnScratchpad: TButton;
    btnSections: TButton;
    btnCustom: TButton;
    memoHelp: TMemo;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnFuncTestClick(Sender: TObject);
    procedure btnComponentTestClick(Sender: TObject);
    procedure btnDataClick(Sender: TObject);
    procedure btnScratchpadClick(Sender: TObject);
    procedure btnSectionsClick(Sender: TObject);
    procedure btnCustomClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ProcDemo1 ;
    procedure ProcDemo2 ;
    procedure ProcDemo3 ;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses uDbg, uDbgIntf ;

var
  DbgMainFrm : TNxDebugger ;

procedure TfrmMain.ProcDemo1 ;
begin
  Debugger.EnterProc ('ProcDemo1') ;
  ProcDemo2 ;
  Debugger.LeaveProc ('ProcDemo1') ;
end ;

procedure TfrmMain.ProcDemo2 ;
begin
  Debugger.EnterProc ('ProcDemo2') ;
  ProcDemo3 ;
  Debugger.LeaveProc ('ProcDemo2') ;
end ;

procedure TfrmMain.ProcDemo3 ;
begin
  Debugger.EnterProc ('ProcDemo3') ;
  Debugger.LogNote ('ProcDemo3 stuff') ;
  Debugger.LeaveProc ('ProcDemo3') ;
end ;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  memoHelp.Lines.Clear ;
  memoHelp.Lines.Text := 'This function establish connection to Overseer. If'+
                         ' viewer don''t running, it''s started (via registry settings).'+
                         ' Note traced application name in viewer caption.' ;
  NxStartDebug ;
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  memoHelp.Lines.Clear ;
  memoHelp.Lines.Text := 'This function close connection to Overseer. '+
                         'Viewer still run, but note [NO SESSION] in his caption.' ;
  Debugger.AddCheckPoint ('Debug sesion closed') ;
  NxEndDebug ;
end;

procedure TfrmMain.btnFuncTestClick(Sender: TObject);
begin
  // Procedure entry/exit demo
  memoHelp.Lines.Clear ;
  memoHelp.Lines.Text := 'Nested calls with EnterMethod/ExitMethod. '+
                         'Select proc entry message and push "Quick '+
                         'mark procedure boundary" button on pallete to '+
                         'mark entry/exit message pair. Note elapsed time '+
                         'on statusbar. You can also set some viewer options '+
                         'like indent, pictures etc.' ;
  Debugger.AddSeparator ;
  ProcDemo1 ;
end;

procedure TfrmMain.btnComponentTestClick(Sender: TObject);
begin
  // Component stream demo
  memoHelp.Lines.Clear ;
  memoHelp.Lines.Text := 'Now you can see streamed component properties '+
                         'in viewer attachment pane.' ;
  Debugger.AddSeparator ;
  Debugger.LogComponent ('btnComponentTest',btnComponentTest) ;
end;

procedure TfrmMain.btnDataClick(Sender: TObject);
begin
  // Data demo
  memoHelp.Lines.Clear ;
  memoHelp.Lines.Text := 'Here are common Overseer trace messages. '+
                         'Select each message to view her attachment.' ;
  Debugger.AddSeparator ;
  Debugger.LogMsg ('Simple message') ;
  Debugger.LogFmtMsg ('Formatted message [%s]',['<filename>']) ;
  Debugger.LogNote ('Note message') ;
  Debugger.LogFmtNote ('Formatted note message [%s]',['<filename>']) ;
  Debugger.LogError ('Error message') ;
  Debugger.LogFmtError ('Formatted error message [%s]',['<filename>']) ;
  Debugger.LogWarning ('Warning message') ;
  Debugger.LogFmtWarning ('Formatted warning message [%s]',['<filename>']) ;
  Debugger.LogAssigned ('Assigned test message (frmMain)',frmMain) ;
  Debugger.LogBoolean ('Boolean expression message',memoHelp.Enabled) ;
  Debugger.LogColor ('Color message',clOlive) ;
  Debugger.LogInteger ('Integer message',frmMain.Top) ;
  Debugger.LogPoint ('Point message',frmMain.Canvas.PenPos) ;
  Debugger.LogRect ('Rectangle message',frmMain.GetClientRect) ;
  Debugger.LogString ('String message','Some text') ;
  Debugger.LogProperty ('Property message (not implemented in viewer)',frmMain,'PopupMenu') ;
  Debugger.LogObject ('Object message (not implemented in viewer)',frmMain) ;
  Debugger.LogComponent ('Component message',frmMain) ;
  Debugger.LogStringList ('Stringlist message',memoHelp.Lines) ;
end;

procedure TfrmMain.btnScratchpadClick(Sender: TObject);
begin
  // Scratchpad demo
  memoHelp.Lines.Clear ;
  memoHelp.Lines.Text := 'Scratchpad is 100 persistent string slots accessible by index. '+
                         'Push scratchpad test button again to see slot change.' ;
  Debugger.ScratchPad (0,'Very stable slot') ;
  Debugger.ScratchPad (1,'Current Date&Time '+DateTimeToStr (Now)) ;
end;

procedure TfrmMain.btnSectionsClick(Sender: TObject);
begin
  // Sections demo
  memoHelp.Lines.Clear ;
  memoHelp.Lines.Text := 'You can create additional TNxDebugger instances to trace '+
                         'different sections of code. You can enable/disable sections '+
                         'for trace in viewer options dialog (setting is persistent).'+
                         'You can see section names along message text (push "show '+
                         'message origin" button on toolbar. uDbg unit define Debugger'+
                         'variable for common messages.' ;
  Debugger.AddSeparator ;
  Debugger.LogMsg ('General message') ;
  DbgMainFrm.LogMsg ('Main form message') ;
end;

procedure TfrmMain.btnCustomClick(Sender: TObject);
begin
  // Custom messages
  memoHelp.Lines.Clear ;
  memoHelp.Lines.Text := 'You can create custom messages with xxxEx methods. ' ;
  Debugger.AddSeparator ;
  Debugger.LogAssignedEx (dbgEnterMethod,'Enter btnCustomClick, sender is ',Sender) ;
  Debugger.LogAssignedEx (dbgExitMethod,'Enter btnCustomClick, sender is ',Sender) ;
end;

initialization
  DbgMainFrm := TNxDebugger.Create ('frmMain','Demo Application Main Form')

finalization
  DbgMainFrm.Free ;

end.
