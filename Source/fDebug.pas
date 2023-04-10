{==============================================================================|
| Project : Delphree - Nexus - Overseer                          | 001.003.000 |
|==============================================================================|
| Content: Overseer main window.                                               |
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
|          CodeSite(tm) segregation, minor improvements.                       |
|          14.2.1999 Version 001.002.000 (Pavel Cisar)                         |
|          Added delete selected, search text and stay on top feature.         |
|          21.2.1999 Version 001.003.000 (Pavel Cisar)                         |
|          Load/Save log, minor improvements & bugfixes.                       |
|==============================================================================}

unit fDebug;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, RxSplit, RxSpeedBar, ToolWin, TB97,
  Grids, uDbgIntf, uDbgLB, uDbgMsg, Menus, RxShell, RxAppEvent, RxTimerLst,
  System.ImageList, Vcl.ImgList;

type
  TfrmDebugger = class(TForm)
    imglSmall: TImageList;
    StatusBar: TStatusBar;
    pmnuSelectNavigation: TPopupMenu;
    mniMessage: TMenuItem;
    mniWarning: TMenuItem;
    mniError: TMenuItem;
    mniCheckPoint: TMenuItem;
    mniNote: TMenuItem;
    mniEnterMethod: TMenuItem;
    mniLeaveMethod: TMenuItem;
    mniSeparator: TMenuItem;
    mniInactive: TMenuItem;
    ImgNavigate: TImageList;
    pmnuMessages: TPopupMenu;
    mniEdit: TMenuItem;
    mniClearAll: TMenuItem;
    mniAddNote: TMenuItem;
    mniAddSeparator: TMenuItem;
    mniAddCheckPoint: TMenuItem;
    mniS1: TMenuItem;
    mniS2: TMenuItem;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuEdit: TMenuItem;
    mnuSearch: TMenuItem;
    mnuView: TMenuItem;
    mnuHelp: TMenuItem;
    Addnote1: TMenuItem;
    Addseparator1: TMenuItem;
    Addcheckpoint1: TMenuItem;
    mniPause: TMenuItem;
    mniClearlog: TMenuItem;
    mniEditmessagetext: TMenuItem;
    N1: TMenuItem;
    mniFirstmessage: TMenuItem;
    mniLastmessage: TMenuItem;
    N2: TMenuItem;
    mniFirst: TMenuItem;
    mniPrevious: TMenuItem;
    mniNext: TMenuItem;
    mniLast: TMenuItem;
    N3: TMenuItem;
    mniMarkProcedure: TMenuItem;
    mniImages: TMenuItem;
    mniIndent: TMenuItem;
    mniTime: TMenuItem;
    mniSections: TMenuItem;
    N4: TMenuItem;
    mniDetail: TMenuItem;
    mniSratchpad: TMenuItem;
    Panel1: TPanel;
    HorizSplit: TRxSplitter;
    pnMessages: TPanel;
    pnDetail: TPanel;
    VertSplit: TRxSplitter;
    lbMessages: TNxDbgDrawList;
    N5: TMenuItem;
    mniOptions: TMenuItem;
    trayViewer: TRxTrayIcon;
    pmnuTray: TPopupMenu;
    mnui_TrayClose: TMenuItem;
    mnui_TrayActivate: TMenuItem;
    mnui_: TMenuItem;
    mnui_Hide: TMenuItem;
    mnui_SelectNavigation: TMenuItem;
    Message1: TMenuItem;
    Warning1: TMenuItem;
    Error1: TMenuItem;
    CheckPoint1: TMenuItem;
    Note1: TMenuItem;
    EnterMethod1: TMenuItem;
    LeaveMethod1: TMenuItem;
    Separator1: TMenuItem;
    Inactive1: TMenuItem;
    imglTray: TImageList;
    appMain: TAppEvents;
    timerPing: TRxTimerList;
    teventPing: TRxTimerEvent;
    pnToolbar: TPanel;
    tbtnHide: TSpeedButton;
    tbtnPause: TSpeedButton;
    tbtnClear: TSpeedButton;
    tbtnAddNote: TSpeedButton;
    tbtnAddSeparator: TSpeedButton;
    tbtnAddCheckpoint: TSpeedButton;
    tbtnFirstMessage: TSpeedButton;
    tbtnFirst: TSpeedButton;
    tbtnPrionr: TSpeedButton;
    tbtnSelectNavigation: TSpeedButton;
    tbtnNext: TSpeedButton;
    tbtnLast: TSpeedButton;
    tbtnLastMessage: TSpeedButton;
    tbtnMarkProcedure: TSpeedButton;
    tbtnImages: TSpeedButton;
    tbtnIndent: TSpeedButton;
    tbtnTime: TSpeedButton;
    tbtnSections: TSpeedButton;
    tbtnDetail: TSpeedButton;
    tbtnBlackboard: TSpeedButton;
    tbtnOptions: TSpeedButton;
    pnBlackboard: TPanel;
    sgrdBlackboard: TStringGrid;
    Panel2: TPanel;
    mniFind: TMenuItem;
    mniFindNext: TMenuItem;
    mniClearSelected: TMenuItem;
    N6: TMenuItem;
    mniSaveLog: TMenuItem;
    mniLoadLog: TMenuItem;
    logOpenDialog: TOpenDialog;
    logSaveDialog: TSaveDialog;
    procedure sbtnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lbMessagesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure tbtnPauseClick(Sender: TObject);
    procedure lbMessagesClick(Sender: TObject);
    procedure tbtnMarkProcedureClick(Sender: TObject);
    procedure tbtnClearClick(Sender: TObject);
    procedure tbtnDetailClick(Sender: TObject);
    procedure tbtnBlackBoardClick(Sender: TObject);
    procedure tbtnTimeClick(Sender: TObject);
    procedure tbtnIndentClick(Sender: TObject);
    procedure tbtnImagesClick(Sender: TObject);
    procedure mniMessageClick(Sender: TObject);
    procedure mniWarningClick(Sender: TObject);
    procedure mniErrorClick(Sender: TObject);
    procedure mniCheckPointClick(Sender: TObject);
    procedure mniNoteClick(Sender: TObject);
    procedure mniEnterMethodClick(Sender: TObject);
    procedure mniLeaveMethodClick(Sender: TObject);
    procedure mniSeparatorClick(Sender: TObject);
    procedure mniInactiveClick(Sender: TObject);
    procedure tbtnOptionsClick(Sender: TObject);
    procedure tbtnFirstClick(Sender: TObject);
    procedure tbtnPriorClick(Sender: TObject);
    procedure tbtnNextClick(Sender: TObject);
    procedure tbtnLastClick(Sender: TObject);
    procedure tbtnAddSeparatorClick(Sender: TObject);
    procedure tbtnAddNoteClick(Sender: TObject);
    procedure tbtnAddCheckpointClick(Sender: TObject);
    procedure lbMessagesDblClick(Sender: TObject);
    procedure VertSplitResize(Sender: TObject);
    procedure HorizSplitResize(Sender: TObject);
    procedure tbtnSectionsClick(Sender: TObject);
    procedure tbtnFirstMessageClick(Sender: TObject);
    procedure tbtnLastMessageClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnui_TrayCloseClick(Sender: TObject);
    procedure teventPingTimer(Sender: TObject);
    procedure trayViewerClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbtnSelectNavigationClick(Sender: TObject);
    procedure tbtnHideClick(Sender: TObject);
    procedure mniFindClick(Sender: TObject);
    procedure mniFindNextClick(Sender: TObject);
    procedure mniClearSelectedClick(Sender: TObject);
    procedure lbMessagesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mniSaveLogClick(Sender: TObject);
    procedure mniLoadLogClick(Sender: TObject);
  private
    { Private declarations }
    FQuit : boolean;
    FUnseenMessages : boolean;
    FViewerListenMessage : cardinal;
    FClientListenMessage : cardinal;
    FViewers : TStringList;
    FClients : TList;
    FDebugClient : hWnd;
    FMsgIndent : integer;
    FMsgList : TList;
    FCheckPoints : TStrings;
    FSections : TStrings;
    FSectionsEnabled : TList;
    // CFG data
    FPaused : boolean;
    FShowTime : boolean;
    FIndent : boolean;
    FNavigateType : integer;
    FMarkOnNavigate : boolean;
    FShowSections : boolean;
    FIndentLevel : integer;
    FActiveViewer : TNxDataViewer;
    FStayOnTop,
    FInsertTop : boolean;
    FSearchText : string;
    FSearchPos : integer;
    procedure DoProcessDebugMessage (CDS : TCopyDataStruct);
    procedure ClearLog;
    procedure UpdateScratchPad (AMsg : TNxViewerMessage);
    procedure ClearMsgList;
    procedure UpdateNavigation;
    procedure UpdateDataView;
    procedure UpdateNavigateMenu (ATxt : string);
    procedure UpdateMenu;
    function GetMessage (Index : integer) : TNxViewerMessage;
    procedure AddMessageToList (AMsg : TNxViewerMessage);
//    procedure UpdateTrayIcon (Active,Messages : boolean);
    procedure UpdateTrayIcon;
  protected
    procedure WMCOPYDATA (var Message :TMessage); message WM_COPYDATA;
    procedure WMNewMsg (var Message : TMessage); message WM_NewDbgMsg;
    procedure WndProc(var Message: TMessage); override;
    procedure SetStayOnTop (Value : boolean);
    procedure HandleInternalMessage (var Message : TMessage);
    procedure UpdateStatusBar;
    procedure ComputeElapsed;
    procedure SendInternal (AMsg : TNxDebugMessageDesc);
    procedure CheckEnabledSections;
    function GetDataViewer (DataID : TGUID) : TNxDataViewer;
    procedure CenterView;
    procedure SearchText;
  public
    { Public declarations }
    procedure SendEnabledSections;
    procedure LoadSettings;
    procedure SaveSettings;
    property IndentLevel : integer read FIndentLevel write FIndentLevel;
    property Sections : TStrings read FSections;
    property SectionsEnabled : TList read FSectionsEnabled;
    property Indented : boolean read FIndent write FIndent;
    property ShowTime : boolean read FShowTime write FShowTime;
    property NavigateType : integer read FNavigateType write FNavigateType;
    property InsertTop : boolean read FInsertTop write FInsertTop;
    property StayOnTop : boolean read FStayOnTop write SetStayOnTop;
  end;

var
  frmDebugger: TfrmDebugger;

const
  lTrue : longbool = true;
  lFalse : longbool = false;

var
  nxNullGuid : TGUID = (D1:0;D2:0;D3:0;D4:(0,0,0,0,0,0,0,0));

function NxCompareGUIDs (A, B : TGUID) : Boolean;
function NxGetDLLName (AHInstance: longint): string;
function NxReplicate(Ch: Char; Len: Integer): string;
function NxPadL(const Str: string; Len: Integer; Ch: Char): string;
function NxTimeStampToString (Timer : TTimeStamp) : string;

implementation

{$R *.DFM}

uses ComObj, fDbgOpt, Registry;

resourcestring
  NxDelphreeRootKey = '\Software\Delphree';
  NxNexusKey = '\Nexus';
  NxDebugKey = '\Debug';
  NxLogSign = 'Overseer Log'^Z;

//------------------------------------------------------------------------------
// Temporary (taken from NxCore)                                               |
//------------------------------------------------------------------------------

function NxCompareGUIDs (A, B : TGUID) : Boolean;
begin
  Result := CompareMem(@A, @B, SizeOf(TGUID));
end;

function NxGetDLLName (AHInstance: longint): string;
var Buff : array[0..261] of char;
begin
  SetString(Result, Buff, Windows.GetModuleFileName(AHInstance, Buff, SizeOf(Buff)));
  Result := LowerCase(Result);
end;

function NxReplicate(Ch: Char; Len: Integer): string;
begin
  Result := '';
  while Len > 0 do begin
    Result := Result + Ch;
    Dec(Len);
  end;
end;

function NxPadL(const Str: string; Len: Integer; Ch: Char): string;
begin
  if Len <= Length(Str)
    then Result := Copy(Str, 1 + Length(Str) - Len, Len)
    else Result := NxReplicate(Ch, Len - Length(Str)) + Str;
end;

function NxTimeStampToString (Timer : TTimeStamp) : string;
var H,M,S,MS : integer;
begin
  MS := Timer.Time;
  S := MS div 1000;
  MS := MS mod 1000;
  M := S div 60;
  S := S mod 60;
  H := M div 60;
  M := M mod 60;
  Result := NxPadL (IntToStr (H),2,'0')+':'+NxPadL (IntToStr(M),2,'0')+':' +
            NxPadL (IntToStr(S),2,'0')+'.'+NxPadL (IntToStr(MS),3,'0');
end;

//------------------------------------------------------------------------------

procedure TfrmDebugger.SendInternal (AMsg : TNxDebugMessageDesc);
var M : PNxDebugMessage;
    I : integer;
    Target,
    Source : PChar;
begin
  if not Active then exit;
  M := AllocMem (sizeof (TNxDebugMessage) + (length (AMsg.MsgText) + 1) * SizeOf(Char) + AMsg.AttachmentSize);
  M^.Section := -1;
  M^.MsgType := AMsg.MsgType;
  M^.MsgSize := length (AMsg.MsgText) * SizeOf(Char);
  M^.MsgTime := Now;
  M^.AttachmentType := AMsg.AttachmentType;
  M^.AttachmentSize := AMsg.AttachmentSize;
  Target := PChar (@M^.Data);
  Source := PChar (AMsg.Attachment);
  for I := 1 to M.MsgSize do begin
    Target[0] := AMsg.MsgText[I];
    inc (Target);
  end;
  for I := 1 to M.AttachmentSize do begin
    Target[0] := Source[0];
    inc (Target); inc (Source);
  end;
  try
    SendMessage (Handle,WM_NewDbgMsg,0,FMsgList.Add (TNxViewerMessage.CreateFrom (M)));
  finally
    FreeMem (M);
  end;
end;

procedure TfrmDebugger.WMNewMsg (var Message : TMessage);
var AMsg : TNxViewerMessage;
    I : integer;
    S : string;
begin
  if not Visible then FUnseenMessages := true;
  UpdateTrayIcon;
  AMsg := TNxViewerMessage (FMsgList.Items[Message.lParam]);
  if (AMsg.MsgType = dbgExitMethod) and (FMsgIndent > 0) then dec (FMsgIndent);
  AMsg.Indent := FMsgIndent;
  case AMsg.MsgType of
  dbgCheckPoint:    begin
                      S := FCheckPoints.Values[AMsg.MsgText];
                      if S = '' then begin
                        I := 1;
                        FCheckPoints.Values[AMsg.MsgText] := '1';
                      end
                      else begin
                        I := StrToInt (S);
                        inc (I);
                        FCheckPoints.Values[AMsg.MsgText] := IntToStr (I);
                      end;
                      AMsg.MsgText := 'Check Point '+AMsg.MsgText+' #'+IntToStr (I);
                      AddMessageToList (AMsg);
                    end;
  dbgInfo,
  dbgWarning,
  dbgError,
  dbgObject,
  dbgStream,
  dbgStringList,
  dbgProperty,
  dbgNote,
  dbgEnterMethod,
  dbgExitMethod:    AddMessageToList (AMsg);
  dbgSeparator:     AddMessageToList (AMsg);
  dbgInactiveSeparator:AddMessageToList (AMsg);
  dbgClear:         ClearLog;
  dbgBlackboard:    UpdateScratchPad (AMsg);
  end;
  if AMsg.MsgType = dbgEnterMethod then inc (FMsgIndent);
  UpdateStatusBar;
  if FInsertTop then lbMessages.TopIndex := 0
  else lbMessages.TopIndex := lbMessages.Items.Count-1;
end;

procedure TfrmDebugger.WMCOPYDATA (var Message : TMessage);
begin
  if FPaused then exit;
  DoProcessDebugMessage (TCopyDataStruct(Pointer (Message.LParam)^));
  inherited;
end;

procedure TfrmDebugger.CheckEnabledSections;
var R : TRegistry;
    I : integer;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKey (NxDelphreeRootKey+NxNexusKey+NxDebugKey+'\Sections',true);
    for I := 0 to FSections.Count-1 do begin
      if R.ValueExists (FSections.Names[I]) then
       FSectionsEnabled.Items[I] := pointer (longbool (R.ReadBool (FSections.Names[I])))
      else FSectionsEnabled.Items[I] := pointer (lTrue);
    end;
  finally
    R.Free;
  end;
end;

function TfrmDebugger.GetDataViewer (DataID : TGUID) : TNxDataViewer;
var I : integer;
    S : string;
begin
  S := GUIDToString (DataID);
  for I := 0 to FViewers.Count-1 do begin
    if S = FViewers.Strings[I] then begin
      Result := TNxDataViewer (FViewers.Objects[I]);
      exit;
    end;
  end;
  Result := TNxDataViewer.Create (DataID);
  FViewers.AddObject (S,Result);
end;

type
  PBooleanArray = ^TBooleanArray;
  TBooleanArray = array [1..1000] of boolean;

procedure TfrmDebugger.SendEnabledSections;
var I : integer;
begin
  for I := 0 to FSections.Count-1 do begin
    if longbool (FSectionsEnabled.Items[I]) then
     SendMessage (FDebugClient,FClientListenMessage,dmsgEnableSection,I)
    else
     SendMessage (FDebugClient,FClientListenMessage,dmsgDisableSection,I);
  end;
end;

procedure TfrmDebugger.DoProcessDebugMessage (CDS : TCopyDataStruct);
var M : TNxViewerMessage;
    I : integer;
begin
  M := TNxViewerMessage.CreateFrom (CDS.lpData);
  if M.MsgType < dbgInternals then PostMessage (Handle,WM_NewDbgMsg,0,FMsgList.Add (M))
  else begin
    case M.MsgType of
    dbgClientData: begin
                     Caption := 'Debugging ' + ExtractFileName (M.MsgText);
                   end;
    dbgSections:   begin
                     FSections.Text := M.MsgText;
                     FSectionsEnabled.Clear;
                     for I := 0 to FSections.Count-1 do
                      FSectionsEnabled.Add (pointer (lTrue));
                     CheckEnabledSections;
                     SendEnabledSections;
                   end;
    end;
    M.Free;
  end;
end;

procedure TfrmDebugger.SetStayOnTop (Value : boolean);
begin
  if FStayOnTop <> Value then begin
    FStayOnTop := Value;
    if FStayOnTop then FormStyle := fsStayOnTop
    else FormStyle := fsNormal;
  end;
end;

procedure TfrmDebugger.HandleInternalMessage (var Message : TMessage);
begin
  case Message.WParam of
  dmsgStartSession:   if FDebugClient = 0 then begin
                        FDebugClient := Message.LParam;
                        SendMessage (FDebugClient,FClientListenMessage,dmsgStartSession,Handle);
                        UpdateTrayIcon;
                        teventPing.Enabled := true;
                      end;
  dmsgEndSession:     begin
                        SendMessage (FDebugClient,FClientListenMessage,dmsgEndSession,Handle);
                        FDebugClient := 0;
                        Caption := 'Overseer [NO SESSION]';
                        UpdateTrayIcon;
                        teventPing.Enabled := false;
                      end;
  end;
end;

procedure TfrmDebugger.WndProc (var Message : TMessage);
begin
  if Message.Msg = FViewerListenMessage then HandleInternalMessage (Message)
  else inherited;
end;

procedure TfrmDebugger.UpdateNavigation;
var B : TBitmap;
begin
  B := TBitmap.Create;
  try
    ImgNavigate.GetBitmap (FNavigateType-1,B);
    tbtnSelectNavigation.Glyph.Assign (B);
    tbtnSelectNavigation.Refresh;
  finally
    B.Free;
  end;
  UpdateMenu;
end;

procedure TfrmDebugger.UpdateDataView;
var I : integer;
    M : TNxViewerMessage;
begin
  if assigned (FActiveViewer) then FActiveViewer.Deactivate;
  FActiveViewer := nil;
  I := lbMessages.ItemIndex;
  if pnDetail.Visible and (I >= 0) and (lbMessages.Items.Count > I) then begin
    M := GetMessage (I);
    if assigned (M) then begin
      if not NxCompareGUIDs (M.AttachmentType, NxNullGuid) then begin
        FActiveViewer := GetDataViewer (M.AttachmentType);
        if assigned (FActiveViewer) then FActiveViewer.Activate (M.AttachmentSize, M.Attachment);
      end;
    end;
  end;
end;

procedure TfrmDebugger.UpdateNavigateMenu (ATxt : string);
begin
  mniFirst.Caption := 'First '+ATxt;
  mniPrevious.Caption := 'Previous '+ATxt;
  mniNext.Caption := 'Next '+ATxt;
  mniLast.Caption := 'Last '+ATxt;
end;

procedure TfrmDebugger.UpdateMenu;
var M : TNxViewerMessage;
begin
  case FNavigateType of
  dbgInfo:           UpdateNavigateMenu ('simple message');
  dbgWarning:        UpdateNavigateMenu ('warning');
  dbgError:          UpdateNavigateMenu ('error');
  dbgCheckPoint:     UpdateNavigateMenu ('checkpoint');
  dbgNote:           UpdateNavigateMenu ('note');
  dbgEnterMethod:    UpdateNavigateMenu ('enter method');
  dbgExitMethod:     UpdateNavigateMenu ('exit method');
  dbgSeparator:      UpdateNavigateMenu ('separator');
  dbgInactiveSeparator:UpdateNavigateMenu ('inactive separator');
  end;
  mniImages.Checked := lbMessages.ShowImages;
  mniSections.Checked := FShowSections;
  mniIndent.Checked := FIndent;
  mniTime.Checked := FShowTime;
  mniDetail.Checked := tbtnDetail.Down;
  mniSratchpad.Checked := tbtnBlackBoard.Down;
  M := GetMessage (lbMessages.ItemIndex);
  if assigned (M) then begin
    tbtnMarkProcedure.Enabled := M.MsgType in [dbgEnterMethod,dbgExitMethod];
    mniMarkProcedure.Enabled := tbtnMarkProcedure.Enabled;
  end;
end;

function TfrmDebugger.GetMessage (Index : integer) : TNxViewerMessage;
begin
  if (Index >= 0) and (Index < lbMessages.Items.Count) then
   Result := TNxViewerMessage (FMsgList.Items[integer(lbMessages.Items.Objects[Index])-1])
  else Result := nil;
end;

procedure TfrmDebugger.AddMessageToList (AMsg : TNxViewerMessage);
begin
  if InsertTop then
   lbMessages.Items.InsertObject (0,AMsg.MsgText,pointer(FMsgList.IndexOf (AMsg)+1))
  else
   lbMessages.Items.AddObject (AMsg.MsgText,pointer(FMsgList.IndexOf (AMsg)+1));
end;

//procedure TfrmDebugger.UpdateTrayIcon (Active,Messages : boolean);
procedure TfrmDebugger.UpdateTrayIcon;
var Icon : TIcon;
    I : integer;
begin
  if FDebugClient <> 0 then
   if FUnseenMessages then I := 3
   else I := 2
  else
   if FUnseenMessages then I := 1
   else I := 0;
  Icon := TIcon.Create;
  try
    imglTray.GetIcon (I,Icon);
    trayViewer.Icon.Assign (Icon);
  finally
    Icon.Free;
  end;
end;

procedure TfrmDebugger.LoadSettings;
var R : TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKey (NxDelphreeRootKey+NxNexusKey+NxDebugKey,true);
    R.WriteString ('Viewer',NxGetDLLName (HInstance));
    if R.ValueExists ('ShowTime') then FShowTime := R.ReadBool ('ShowTime');
    if R.ValueExists ('Indent') then
     FIndent := R.ReadBool ('Indent');
    if R.ValueExists ('IndentWidth') then FIndentLevel := R.ReadInteger ('IndentWidth');
    if R.ValueExists ('Navigate') then FNavigateType := R.ReadInteger ('Navigate');
//    if R.ValueExists ('MarkNavigation') then FMarkOnNavigate := R.ReadBool ('MarkNavigation');
    if R.ValueExists ('InsertTop') then FInsertTop := R.ReadBool ('InsertTop');
    if R.ValueExists ('StayOnTop') then StayOnTop := R.ReadBool ('StayOnTop');

    if R.ValueExists ('Left') then Left := R.ReadInteger ('Left');
    if R.ValueExists ('Top') then Top := R.ReadInteger ('Top');
    if R.ValueExists ('Width') then Width := R.ReadInteger ('Width');
    if R.ValueExists ('Height') then Height := R.ReadInteger ('Height');

    if R.ValueExists ('Images') then lbMessages.ShowImages := R.ReadBool ('Images');
    if R.ValueExists ('Detail') then tbtnDetail.Down := R.ReadBool ('Detail');
    if R.ValueExists ('ScratchPad') then tbtnBlackBoard.Down := R.ReadBool ('ScratchPad');
    if R.ValueExists ('ShowSections') then FShowSections := R.ReadBool ('ShowSections');

    tbtnSections.Down := FShowSections;
    tbtnImages.Down := lbMessages.ShowImages;
    tbtnIndent.Down := FIndent;
    tbtnTime.Down := FShowTime;
//    tbtnSelectOnMove.Down := FMarkOnNavigate;
    VertSplit.Visible := tbtnDetail.Down;
    pnDetail.Visible := tbtnDetail.Down;
    HorizSplit.Visible := tbtnBlackBoard.Down;
    pnBlackBoard.Visible := tbtnBlackBoard.Down;
    UpdateNavigation;
  finally
    R.Free;
  end;
end;

procedure TfrmDebugger.SaveSettings;
var R : TRegistry;
    I : integer;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKey (NxDelphreeRootKey+NxNexusKey+NxDebugKey,true);
    R.WriteBool ('ShowTime',FShowTime);
    R.WriteBool ('Indent',FIndent);
    R.WriteInteger ('IndentWidth',FIndentLevel);
    R.WriteInteger ('Navigate',FNavigateType);
    R.WriteBool ('InsertTop',FInsertTop);
    R.WriteBool ('StayOnTop',FStayOnTop);
//    R.WriteBool ('MarkNavigation',FMarkOnNavigate);
    R.WriteBool ('Images',lbMessages.ShowImages);
    R.WriteBool ('Detail',pnDetail.Visible);
    R.WriteBool ('ScratchPad',pnBlackBoard.Visible);
    R.WriteBool ('ShowSections',tbtnSections.Down);
    R.WriteInteger ('Top',Top);
    R.WriteInteger ('Left',Left);
    R.WriteInteger ('Width',Width);
    R.WriteInteger ('Height',Height);

    R.OpenKey (NxDelphreeRootKey+NxNexusKey+NxDebugKey+'\Sections',true);
    for I := 0 to FSections.Count-1 do begin
      R.WriteBool (FSections.Names[I],longbool(FSectionsEnabled.Items[I]));
    end;
  finally
    R.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmDebugger.UpdateStatusBar;
var S : string;
    M : TNxViewerMessage;
    I : integer;
begin
  if lbMessages.SelCount > 1 then ComputeElapsed
  else StatusBar.Panels.Items[0].Text := 'Elapsed 00:00:00.000';
  StatusBar.Panels.Items[1].Text := IntToStr (lbMessages.Items.Count) + ' messages';
  StatusBar.Panels.Items[2].Text := IntToStr (lbMessages.SelCount) + ' selected';
  I := lbMessages.ItemIndex;
  if (I >= 0) and (lbMessages.Items.Count > I) then begin
    M := GetMessage (I);
    if assigned (M) then begin
      if NxCompareGUIDs (M.AttachmentType,NxNullGuid) then S := 'No attachement'
      else begin
        // Finding the name of the attachment type
      end;
    end;
  end
  else S := '';
  StatusBar.Panels.Items[3].Text := S;
end;

procedure TfrmDebugger.ClearMsgList;
begin
  while FMsgList.Count > 0 do begin
    TObject (FMsgList.First).Free;
    FMsgList.Delete(0);
  end;
end;

procedure TfrmDebugger.ClearLog;
begin
  lbMessages.Items.BeginUpdate;
  ClearMsgList;
  lbMessages.Items.Clear;
  lbMessages.Items.EndUpdate;
  FMsgIndent := 0;
  UpdateStatusBar;
  UpdateDataView;
  CenterView;
end;

procedure TfrmDebugger.UpdateScratchPad (AMsg : TNxViewerMessage);
var I : integer;
begin
  I := integer (AMsg.Attachment^);
  sgrdBlackboard.Cells [1,I] := AMsg.MsgText;
end;

procedure TfrmDebugger.ComputeElapsed;
var I,LowIndex,HighIndex : integer;
    ML,MH : TNxViewerMessage;
begin
  LowIndex := -1;
  HighIndex := -1;
  for I := 0 to lbMessages.Items.Count-1 do begin
    if lbMessages.Selected[I] then begin
      LowIndex := I;
      break;
    end;
  end;
  for I := lbMessages.Items.Count-1 downto 0 do begin
    if lbMessages.Selected[I] then begin
      HighIndex := I;
      break;
    end;
  end;
  if (LowIndex >= 0) and (HighIndex >= 0) and (LowIndex < HighIndex) then begin
    ML := GetMessage (LowIndex);
    MH := GetMessage (HighIndex);
    StatusBar.Panels.Items[0].Text := 'Elapsed ' + NxTimeStampToString (DateTimeToTimeStamp(MH.MsgTime-ML.MsgTime));
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmDebugger.sbtnClearClick(Sender: TObject);
begin
  ClearLog;
end;

procedure TfrmDebugger.FormCreate(Sender: TObject);
var I : integer;
begin
  FClients := TList.Create;
  FSections := TStringList.Create;
  FSectionsEnabled := TList.Create;
  FViewers := TStringList.Create;
  FIndentLevel := 15;
  FNavigateType := dbgError;
  FShowTime := true;
  FIndent := true;
  FMsgList := TList.Create;
  FCheckPoints := TStringList.Create;
  FViewerListenMessage := RegisterWindowMessage ('NxDbgViewer');
  FClientListenMessage := RegisterWindowMessage ('NxDbgClient');
  for I := 0 to sgrdBlackboard.RowCount-1 do begin
    sgrdBlackboard.Rows[I].Add (NxPadL (IntToStr (I),3,' '));
  end;
  lbMessages.MultiSelect := true;
  sgrdBlackboard.ColWidths[0] := 20;
  sgrdBlackboard.ColWidths[1] := sgrdBlackboard.Width - sgrdBlackboard.ColWidths[0];
  LoadSettings;
  UpdateStatusBar;
  UpdateMenu;
end;

procedure TfrmDebugger.FormResize(Sender: TObject);
begin
  sgrdBlackboard.ColWidths[1] := sgrdBlackboard.Width - sgrdBlackboard.ColWidths[0];
  lbMessages.Refresh;
end;

procedure TfrmDebugger.lbMessagesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var AMsg : TNxViewerMessage;
begin
  AMsg := GetMessage (Index);
  if assigned (AMsg) then
    with lbMessages do begin
      ImageIndex := AMsg.MsgType-1;
      if FIndent then Indent := AMsg.Indent * FIndentLevel
      else Indent := 0;
      LeftText := AMsg.MsgText;
      RightText := '';
      if FShowTime and not (AMsg.MsgType in [dbgSeparator,dbgInactiveSeparator]) then
       RightText := NxTimeStampToString (DateTimeToTimeStamp (AMsg.MsgTime));
       if FShowSections and (FSections.Count > AMsg.Section) and (AMsg.Section >= 0)
         and not (AMsg.MsgType in [dbgSeparator,dbgInactiveSeparator]) then
       RightText := FSections.Names[AMsg.Section] + ' ' + RightText;
    end;
end;

procedure TfrmDebugger.FormDestroy(Sender: TObject);
begin
  PostMessage (FDebugClient,FClientListenMessage,dmsgEndSession,Handle);
  SaveSettings;
  ClearMsgList;
  FViewers.Free;
  FMsgList.Free;
  FCheckPoints.Free;
  FSections.Free;
  FSectionsEnabled.Free;
  FClients.Free;
end;

procedure TfrmDebugger.tbtnPauseClick(Sender: TObject);
var AMsg : TNxDebugMessageDesc;
begin
  // Pause
  FPaused := not FPaused;
  if FPaused then begin
    with AMsg do begin
      MsgType := dbgInactiveSeparator;
      MsgText := '\-';
      MsgTime := Now;
      AttachmentType := NxNullGuid;
      AttachmentSize := 0;
      Attachment := nil;
    end;
    SendInternal (AMsg);
  end;
end;

procedure TfrmDebugger.lbMessagesClick(Sender: TObject);
begin
  // Message selected
  UpdateStatusBar;
  UpdateDataView;
  UpdateMenu;
end;

procedure TfrmDebugger.tbtnMarkProcedureClick(Sender: TObject);
var I, Level : integer;
    M : TNxViewerMessage;
begin
  // Select procedure boundaries
  I := lbMessages.ItemIndex;
  M := GetMessage (I);
  Level := M.Indent;
  if InsertTop then begin
    if M.MsgType = dbgEnterMethod then
     while I >= 0 do begin
       M := GetMessage (I);
       if (M.MsgType = dbgExitMethod) and (M.Indent = Level) then begin
         lbMessages.Selected[I] := true;
         break;
       end;
       dec (I);
     end
    else
     while I < lbMessages.Items.Count do begin
       M := GetMessage (I);
       if (M.MsgType = dbgEnterMethod) and (M.Indent = Level) then begin
         lbMessages.Selected[I] := true;
         break;
       end;
       inc (I);
     end;
  end
  else begin
    if M.MsgType = dbgExitMethod then
     while I >= 0 do begin
       M := GetMessage (I);
       if (M.MsgType = dbgEnterMethod) and (M.Indent = Level) then begin
         lbMessages.Selected[I] := true;
         break;
       end;
       dec (I);
     end
    else
     while I < lbMessages.Items.Count do begin
       M := GetMessage (I);
       if (M.MsgType = dbgExitMethod) and (M.Indent = Level) then begin
         lbMessages.Selected[I] := true;
         break;
       end;
       inc (I);
     end;
  end;
  UpdateStatusBar;
  UpdateMenu;
end;

procedure TfrmDebugger.tbtnClearClick(Sender: TObject);
begin
  ClearLog;
end;

procedure TfrmDebugger.tbtnDetailClick(Sender: TObject);
begin
  // Switch Detail visibility
  if Sender is TMenuItem then tbtnDetail.Down := not tbtnDetail.Down;
  VertSplit.Visible := tbtnDetail.Down;
  pnDetail.Visible := tbtnDetail.Down;
  UpdateDataView;
  UpdateMenu;
end;

procedure TfrmDebugger.tbtnBlackBoardClick(Sender: TObject);
begin
  // Switch ScratchPad visibility
  if Sender is TMenuItem then tbtnBlackBoard.Down := not tbtnBlackBoard.Down;
  HorizSplit.Visible := tbtnBlackBoard.Down;
  pnBlackBoard.Visible := tbtnBlackBoard.Down;
  UpdateMenu;
end;

procedure TfrmDebugger.tbtnTimeClick(Sender: TObject);
begin
  if Sender is TMenuItem then tbtnTime.Down := not tbtnTime.Down;
  FShowTime := not FShowTime;
  lbMessages.Refresh;
  UpdateMenu;
end;

procedure TfrmDebugger.tbtnIndentClick(Sender: TObject);
begin
  if Sender is TMenuItem then tbtnIndent.Down := not tbtnIndent.Down;
  FIndent := not FIndent;
  lbMessages.Refresh;
  UpdateMenu;
end;

procedure TfrmDebugger.tbtnImagesClick(Sender: TObject);
begin
  if Sender is TMenuItem then tbtnImages.Down := not tbtnImages.Down;
  lbMessages.ShowImages := not lbMessages.ShowImages;
  lbMessages.Refresh;
  UpdateMenu;
end;

procedure TfrmDebugger.mniMessageClick(Sender: TObject);
begin
  // Naviagte trought messages
  FNavigateType := dbgInfo;
  UpdateNavigation;
end;

procedure TfrmDebugger.mniWarningClick(Sender: TObject);
begin
  // Naviagte trought warnings
  FNavigateType := dbgWarning;
  UpdateNavigation;
end;

procedure TfrmDebugger.mniErrorClick(Sender: TObject);
begin
  // Naviagte trought errors
  FNavigateType := dbgError;
  UpdateNavigation;
end;

procedure TfrmDebugger.mniCheckPointClick(Sender: TObject);
begin
  // Naviagte trought checkpoints
  FNavigateType := dbgCheckPoint;
  UpdateNavigation;
end;

procedure TfrmDebugger.mniNoteClick(Sender: TObject);
begin
  // Naviagte trought notes
  FNavigateType := dbgNote;
  UpdateNavigation;
end;

procedure TfrmDebugger.mniEnterMethodClick(Sender: TObject);
begin
  // Naviagte trought method enters
  FNavigateType := dbgEnterMethod;
  UpdateNavigation;
end;

procedure TfrmDebugger.mniLeaveMethodClick(Sender: TObject);
begin
  // Naviagte trought method leavings
  FNavigateType := dbgExitMethod;
  UpdateNavigation;
end;

procedure TfrmDebugger.mniSeparatorClick(Sender: TObject);
begin
  // Naviagte trought separators
  FNavigateType := dbgSeparator;
  UpdateNavigation;
end;

procedure TfrmDebugger.mniInactiveClick(Sender: TObject);
begin
  // Naviagte trought inactivity separators
  FNavigateType := dbgInactiveSeparator;
  UpdateNavigation;
end;

procedure TfrmDebugger.tbtnOptionsClick(Sender: TObject);
begin
  // Options
  with TfrmOptions.Create (self) do
   try
     LoadSettings;
     if ShowModal = mrOk then SaveSettings;
   finally
     Free;
   end;
  lbMessages.Refresh;
end;

procedure TfrmDebugger.tbtnFirstClick(Sender: TObject);
var I : integer;
    Found : boolean;
begin
  // First message
  I := 0;
  Found := false;
  while I < lbMessages.Items.Count do begin
    if GetMessage(I).MsgType = FNavigateType then begin
      Found := true;
      break;
    end;
    inc (I);
  end;
  if Found then begin
    lbMessages.Selected[lbMessages.ItemIndex] := FMarkOnNavigate;
    lbMessages.ItemIndex := I;
    lbMessages.Selected[I] := true;
  end;
  UpdateStatusBar;
  UpdateDataView;
  CenterView;
end;

procedure TfrmDebugger.tbtnPriorClick(Sender: TObject);
var I : integer;
    Found : boolean;
begin
  // Prior message
  I := lbMessages.ItemIndex;
  Found := false;
  if (I < 0) or (I >= lbMessages.Items.Count) then exit;
  if assigned (GetMessage(I)) then begin
    dec (I);
    while I >= 0 do begin
      if GetMessage(I).MsgType = FNavigateType then begin
        Found := true;
        break;
      end;
      dec (I);
    end;
    if Found then begin
      lbMessages.Selected[lbMessages.ItemIndex] := FMarkOnNavigate;
      lbMessages.ItemIndex := I;
      lbMessages.Selected[I] := true;
    end;
    UpdateStatusBar;
    UpdateDataView;
    CenterView;
  end;
end;

procedure TfrmDebugger.tbtnNextClick(Sender: TObject);
var I : integer;
    Found : boolean;
begin
  // Next message
  I := lbMessages.ItemIndex;
  Found := false;
  if (I < 0) or (I >= lbMessages.Items.Count) then exit;
  if assigned (GetMessage(I)) then begin
    inc (I);
    while I < lbMessages.Items.Count do begin
      if GetMessage(I).MsgType = FNavigateType then begin
        Found := true;
        break;
      end;
      inc (I);
    end;
    if Found then begin
      lbMessages.Selected[lbMessages.ItemIndex] := FMarkOnNavigate;
      lbMessages.ItemIndex := I;
      lbMessages.Selected[I] := true;
    end;
    UpdateStatusBar;
    UpdateDataView;
    CenterView;
  end;
end;

procedure TfrmDebugger.tbtnLastClick(Sender: TObject);
var I : integer;
    Found : boolean;
begin
  // LastMessage
  I := lbMessages.Items.Count-1;
  Found := false;
  while I >= 0 do begin
    if GetMessage(I).MsgType = FNavigateType then begin
      Found := true;
      break;
    end;
    dec (I);
  end;
  if Found then begin
    lbMessages.Selected[lbMessages.ItemIndex] := FMarkOnNavigate;
    lbMessages.ItemIndex := I;
    lbMessages.Selected[I] := true;
  end;
  UpdateStatusBar;
  UpdateDataView;
  CenterView;
end;

procedure TfrmDebugger.tbtnAddSeparatorClick(Sender: TObject);
var AMsg : TNxDebugMessageDesc;
begin
  // Add separator
  with AMsg do begin
    MsgType := dbgSeparator;
    MsgText := '-';
    MsgTime := Now;
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
    Attachment := nil;
  end;
  SendInternal (AMsg);
end;

procedure TfrmDebugger.tbtnAddNoteClick(Sender: TObject);
var AMsg : TNxDebugMessageDesc;
    S : string;
begin
  // Add Note
  if InputQuery ('Message text','',S) then begin
    with AMsg do begin
      MsgType := dbgInfo;
      MsgText := S;
      MsgTime := Now;
      AttachmentType := NxNullGuid;
      AttachmentSize := 0;
      Attachment := nil;
    end;
    SendInternal (AMsg);
  end;
end;

procedure TfrmDebugger.tbtnAddCheckpointClick(Sender: TObject);
var AMsg : TNxDebugMessageDesc;
begin
  // Add checkpoint
  with AMsg do begin
    MsgType := dbgCheckPoint;
    MsgText := 'Viewer';
    MsgTime := Now;
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
    Attachment := nil;
  end;
  SendInternal (AMsg);
end;

procedure TfrmDebugger.lbMessagesDblClick(Sender: TObject);
var I : integer;
    M : TNxViewerMessage;
begin
  // Edit message text
  I := lbMessages.ItemIndex;
  if (I >= 0) and (lbMessages.Items.Count > I) then begin
    M := GetMessage (I);
    if assigned (M) then begin
      lbMessages.Selected[I] := false;
      M.MsgText := InputBox ('Message text','',M.MsgText);
      lbMessages.Selected[I] := true;
    end;
  end;
end;

procedure TfrmDebugger.VertSplitResize(Sender: TObject);
begin
  lbMessages.Invalidate;
end;

procedure TfrmDebugger.HorizSplitResize(Sender: TObject);
begin
  lbMessages.Invalidate;
end;

procedure TfrmDebugger.tbtnSectionsClick(Sender: TObject);
begin
  // Show sections
  if Sender is TMenuItem then tbtnSections.Down := not tbtnSections.Down;
  FShowSections := tbtnSections.Down;
  lbMessages.Refresh;
  UpdateMenu;
end;

procedure TfrmDebugger.tbtnFirstMessageClick(Sender: TObject);
begin
  if lbMessages.Items.Count > 0 then begin
    lbMessages.Selected[lbMessages.ItemIndex] := FMarkOnNavigate;
    lbMessages.ItemIndex := 0;
    lbMessages.Selected[0] := true;
    UpdateStatusBar;
    UpdateDataView;
  end;
end;

procedure TfrmDebugger.tbtnLastMessageClick(Sender: TObject);
var I : integer;
begin
  if lbMessages.Items.Count > 0 then begin
    I := lbMessages.Items.Count - 1;
    lbMessages.Selected[lbMessages.ItemIndex] := FMarkOnNavigate;
    if ((lbMessages.Items.Count * lbMessages.ItemHeight) > lbMessages.Height) then begin
     lbMessages.TopIndex := I;
    end;
    lbMessages.ItemIndex := I;
    lbMessages.Selected[I] := true;
    UpdateStatusBar;
    UpdateDataView;
  end;
end;

procedure TfrmDebugger.FormHide(Sender: TObject);
begin
  trayViewer.Active := true;
  UpdateTrayIcon;
end;

procedure TfrmDebugger.FormShow(Sender: TObject);
begin
  trayViewer.Active := false;
  FUnseenMessages := false;
  UpdateTrayIcon;
end;

procedure TfrmDebugger.mnui_TrayCloseClick(Sender: TObject);
begin
  FQuit := true;
  Close;
end;

procedure TfrmDebugger.teventPingTimer(Sender: TObject);
begin
  if GetWindowThreadProcessId (FDebugClient,nil) = 0 then begin
    FDebugClient := 0;
    Caption := 'Overseer [NO SESSION]';
    UpdateTrayIcon;
    teventPing.Enabled := false;
  end
end;

procedure TfrmDebugger.trayViewerClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Show;
end;

procedure TfrmDebugger.tbtnSelectNavigationClick(Sender: TObject);
var P : TPoint;
begin
  P.x := tbtnSelectNavigation.Left;
  P.y := tbtnSelectNavigation.Top+tbtnSelectNavigation.Height;
  P := ClientToScreen (P);
  tbtnSelectNavigation.Down := true;
  pmnuSelectNavigation.Popup (P.x,P.y);
  tbtnSelectNavigation.Down := false;
end;

procedure TfrmDebugger.tbtnHideClick(Sender: TObject);
begin
  Hide;
end;

procedure TfrmDebugger.mniFindClick(Sender: TObject);
begin
  // Find text
  if InputQuery ('Search text in messages','Enter text',FSearchText) then begin
    FSearchText := AnsiUpperCase (FSearchText);
    if FInsertTop then FSearchPos := lbMessages.Items.Count-1
    else FSearchPos := 0;
    SearchText;
  end;
end;

procedure TfrmDebugger.CenterView;
var I : integer;
    R : TRect;
begin
  R := lbMessages.ClientRect;
  I := ((R.Bottom-R.Top) div lbMessages.ItemHeight) div 2;
  if lbMessages.ItemIndex > I then
   lbMessages.TopIndex := lbMessages.ItemIndex - I;
end;

procedure TfrmDebugger.SearchText;
var Found : boolean;
begin
  Found := false;
  if FInsertTop then begin
    while (FSearchPos > 0) and not Found do begin
      if pos (FSearchText,AnsiUpperCase (lbMessages.Items.Strings[FSearchPos])) > 0 then Found := true
      else dec (FSearchPos);
    end;
  end
  else begin
    while (FSearchPos < lbMessages.Items.Count-1) and not Found do begin
      if pos (FSearchText,AnsiUpperCase (lbMessages.Items.Strings[FSearchPos])) > 0 then Found := true
      else inc (FSearchPos);
    end;
  end;
  if Found then begin
    lbMessages.Selected[lbMessages.ItemIndex] := false;
    lbMessages.ItemIndex := FSearchPos;
    lbMessages.Selected[FSearchPos] := true;
    UpdateStatusBar;
    UpdateDataView;
    CenterView;
  end
  else ShowMessage('Not found');
end;

procedure TfrmDebugger.mniFindNextClick(Sender: TObject);
begin
  // Find next
  if FInsertTop then FSearchPos := lbMessages.ItemIndex-1
  else FSearchPos := lbMessages.ItemIndex+1;
  SearchText;
end;

procedure TfrmDebugger.mniClearSelectedClick(Sender: TObject);
var I,J,P : integer;
begin
  // Clear selected
  lbMessages.Items.BeginUpdate;
  I := 0;
  P := 0;
  while I < lbMessages.Items.Count do begin
    if lbMessages.Selected[I] then begin
      if FInsertTop then begin
        if P = 0 then P := I;
      end
      else P := I;
      J := integer(lbMessages.Items.Objects[I])-1;
      TObject (FMsgList.Items[J]).Free;
      FMsgList.Items[J] := nil;
      lbMessages.Items.Delete (I);
    end
    else inc (I);
  end;
  lbMessages.Items.EndUpdate;
  if P >= lbMessages.Items.Count then begin
    lbMessages.ItemIndex := lbMessages.Items.Count-1;
    lbMessages.Selected[lbMessages.Items.Count-1] := true;
  end
  else begin
    lbMessages.ItemIndex := P;
    lbMessages.Selected[P] := true;
  end;
  UpdateStatusBar;
  UpdateDataView;
  CenterView;
end;

procedure TfrmDebugger.lbMessagesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then begin
    if GetKeyState(VK_CONTROL) < 0 then tbtnClearClick (nil) 
    else mniClearSelectedClick (nil);
    Key := 0;
  end;
end;

procedure TfrmDebugger.mniSaveLogClick(Sender: TObject);
var I : integer;
    FLog : TFileStream;
    FW : TWriter;
begin
  // Save log
  if logSaveDialog.Execute then begin
    FLog := TFileStream.Create (logSaveDialog.FileName,fmCreate);
    FW := TWriter.Create (FLog,1024);
    try
      FW.WriteString (NxLogSign);
      FW.WriteInteger (FMsgList.Count);
      for I := 0 to FMsgList.Count-1 do begin
        TNxViewerMessage(FMsgList.Items[I]).SaveToStream (FW);
      end;
      FW.FlushBuffer;
    finally
      FW.Free;
      FLog.Free;
    end;
  end;
end;

procedure TfrmDebugger.mniLoadLogClick(Sender: TObject);
var I,Cnt : integer;
    FLog : TFileStream;
    FR : TReader;
    S : string;
    Msg : TNxViewerMessage;
begin
  // Load log
  if logOpenDialog.Execute then begin
    ClearLog;
    FLog := TFileStream.Create (logSaveDialog.FileName,fmOpenRead);
    FR := TReader.Create (FLog,1024);
    try
      S := FR.ReadString;
      if S = NxLogSign then begin
        Cnt := FR.ReadInteger;
        for I := 1 to Cnt do begin
          Msg := TNxViewerMessage.Create;
          Msg.LoadFromStream (FR);
          SendMessage (Handle,WM_NewDbgMsg,0,FMsgList.Add (Msg));
        end;
      end
      else ShowMessage('Isn''t an Overseer log !');
    finally
      FR.Free;
      FLog.Free;
    end;
  end;
end;

end.
