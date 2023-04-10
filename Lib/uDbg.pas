{==============================================================================|
| Project : Delphree - Nexus - Overseer                          | 001.002.000 |
|==============================================================================|
| Content: Overseer client.                                                    |
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
|          20.2.1999 Version 001.002.000 (Pavel Cisar)                         |
|          Make debugger channel ready for custom channels. Minor improvements.|
|==============================================================================}

unit uDbg;

interface

uses Windows,
     SysUtils,
     Messages,
     Vcl.Forms,
     Registry,
     Vcl.Dialogs,
     Classes,
     Vcl.Graphics,
     System.Types,
     REST.Json,
     uDbgIntf;

//------------------------------------------------------------------------------
// Debugging Engine                                                            |
//------------------------------------------------------------------------------

type
  TNxDebugger = class;

  TNxCustomDebuggerChannel = class (TObject)
  private
    FDbgList : TList;
  protected
    function GetActive : boolean; virtual; abstract;
    procedure SetActive (Value : boolean); virtual; abstract;
    procedure RegisterDebugger (Dbg : TNxDebugger);
    procedure UnregisterDebugger (Dbg : TNxDebugger);
    procedure SendSections;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Send (ADbg : TNxDebugger; AMsg : TNxDebugMessageDesc); virtual; abstract;
    procedure StartDebugSession; virtual; abstract;
    procedure EndDebugSession; virtual; abstract;
    property Active : boolean read GetActive write SetActive;
  end;

  TNxDebuggerChannel = class (TNxCustomDebuggerChannel)
  private
    FViewerWnd : hWnd;
    FDebuggerWnd : hWnd;
    FInterceptMessage : cardinal;
    function AllocateWindow: HWND;
  protected
    function GetActive : boolean; override;
    procedure SetActive (Value : boolean); override;
    function WndProc(Window: HWND; Message, wParam, lParam: Longint): Longint; stdcall;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Send (ADbg : TNxDebugger; AMsg : TNxDebugMessageDesc); override;
    procedure StartDebugSession; override;
    procedure EndDebugSession; override;
    property ViewerWindow : hWnd read FViewerWnd write FViewerWnd;
  end;

  TNxDebugger = class (TObject)
  private
    FID,
    FName : string;
    FAutoStart,
    FEnabled : boolean;
  protected
    procedure Send (AMsg : TNxDebugMessageDesc);
    function PointToString (APoint : TPoint) : string;
    function RectToString (ARect : TRect) : string;
    function CheckEnabled : boolean;
  public
    constructor Create (ID, Name : string);
    destructor Destroy; override;
    procedure LogMsg (AMsg : string);
    procedure LogMsgEx (AMsgType : integer; AMsg : string);
    procedure LogFmtMsg (Fmt : string; Args : array of const);
    procedure LogFmtMsgEx (AMsgType : integer; Fmt : string; Args : array of const);
    procedure LogNote (AMsg : string);
    procedure LogFmtNote (Fmt : string; Args : array of const);
    procedure LogError (AMsg : string);
    procedure LogFmtError (Fmt : string; Args : array of const);
    procedure LogWarning (AMsg : string);
    procedure LogFmtWarning (Fmt : string; Args : array of const);
    procedure LogAssigned (AMsg : string; Value : pointer);
    procedure LogAssignedEx (AMsgType : integer; AMsg : string; Value : pointer);
    procedure LogBoolean (AMsg : string; Expresion : boolean);
    procedure LogBooleanEx (AMsgType : integer; AMsg : string; Expresion : boolean);
    procedure LogColor (AMsg : string; Value : TColor);
    procedure LogColorEx (AMsgType : integer; AMsg : string; Value : TColor);
    procedure LogInteger (AMsg : string; Value : integer);
    procedure LogIntegerEx (AMsgType : integer; AMsg : string; Value : integer);
    procedure LogPoint (AMsg : string; Value : TPoint);
    procedure LogPointEx (AMsgType : integer; AMsg : string; Value : TPoint);
    procedure LogRect (AMsg : string; Value : TRect);
    procedure LogRectEx (AMsgType : integer; AMsg : string; Value : TRect);
    procedure LogString (AMsg : string; Value : string);
    procedure LogStringEx (AMsgType : integer; AMsg : string; Value : string);
    procedure LogProperty (AMsg : string; Obj : TObject; PropName : string);
    procedure LogObject (AMsg : string; Obj : TObject);
    procedure LogComponent (AMsg : string; Obj : TComponent);
    procedure LogStringList (AMsg : string; List : TStrings);
    procedure AddCheckPoint (CheckName : string);
    procedure AddSeparator;
    procedure Clear;
    procedure ScratchPad (LineIndex : integer; Msg : string);
    procedure FmtScratchPad (LineIndex : integer; Fmt : string; Args : array of const);
    procedure EnterProc (AMsg : string);
    procedure LeaveProc (AMsg : string);
    property Enabled : boolean read FEnabled;
  end;

procedure NxStartDebug;
procedure NxEndDebug;
function NxDebugging : boolean;

var
  NxDebuggerChannel : TNxDebuggerChannel;
  Debugger : TNxDebugger;

implementation

uses ComObj;

resourcestring
  NxDelphreeRootKey = '\Software\Delphree';
  NxNexusKey = '\Nexus';
  NxDebugKey = '\Debug';

var
  DebuggerClosing : Boolean = False;
  DebuggerListenMessage : cardinal;
  nxNullGuid : TGUID = (D1:0;D2:0;D3:0;D4:(0,0,0,0,0,0,0,0));

//------------------------------------------------------------------------------
// Debugging Engine                                                            |
//------------------------------------------------------------------------------

procedure NxStartDebug;
begin
  NxDebuggerChannel.Active := true;
end;

procedure NxEndDebug;
begin
  NxDebuggerChannel.Active := false;
end;

function NxDebugging : boolean;
begin
  Result := NxDebuggerChannel.Active;
end;

function DebuggerWndProc(Window: HWND; Message, wParam, lParam: Longint): Longint; stdcall;
begin
  if Message = DebuggerListenMessage then Result := NxDebuggerChannel.WndProc (Window,Message,wParam,lParam)
  else Result := DefWindowProc(Window, Message, wParam, lParam);
end;

var
  DebuggerWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DebuggerWndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TDebuggerClientWindow');

//------------------------------------------------------------------------------
// TNxCustomDebuggerChannel                                                    |
//------------------------------------------------------------------------------

constructor TNxCustomDebuggerChannel.Create;
begin
  inherited;
  FDbgList := TList.Create;
end;

destructor TNxCustomDebuggerChannel.Destroy;
var I : integer;
begin
  for I := 0 to FDbgList.Count-1 do begin
    TNxDebugger(FDbgList.Items[I]).FEnabled := false;
  end;
  FDbgList.Free;
  if Active then EndDebugSession;
  inherited;
end;

procedure TNxCustomDebuggerChannel.RegisterDebugger (Dbg : TNxDebugger);
begin
  FDbgList.Add (Dbg);
  Dbg.FEnabled := Active;
end;

procedure TNxCustomDebuggerChannel.UnregisterDebugger (Dbg : TNxDebugger);
begin
  Dbg.FEnabled := false;
  FDbgList.Remove(Dbg);
end;

procedure TNxCustomDebuggerChannel.SendSections;
var M : TNxDebugMessageDesc;
    S : TStringList;
    I : integer;
begin
  S := TStringList.Create;
  try
    for I := 0 to FDbgList.Count-1 do begin
      S.Values [TNxDebugger(FDbgList.Items[I]).FID] := TNxDebugger(FDbgList.Items[I]).FName;
    end;
    with M do begin
      MsgType := dbgSections;
      MsgText := S.Text;
      AttachmentType := NxNullGUID;
      AttachmentSize := 0;
    end;
  finally
    S.Free;
  end;
  Send (nil,M);
end;

//------------------------------------------------------------------------------
// TNxDebuggerChannel                                                          |
//------------------------------------------------------------------------------

constructor TNxDebuggerChannel.Create;
begin
  inherited;
  FDebuggerWnd := AllocateWindow;
  FInterceptMessage := RegisterWindowMessage ('NxDbgViewer');
  DebuggerListenMessage := RegisterWindowMessage ('NxDbgClient');
end;

destructor TNxDebuggerChannel.Destroy;
begin
  DestroyWindow(FDebuggerWnd);
  inherited;
end;

function TNxDebuggerChannel.AllocateWindow : HWND;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  DebuggerWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance, DebuggerWindowClass.lpszClassName,
    TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @DebuggerWndProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(DebuggerWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(DebuggerWindowClass);
  end;
  Result := CreateWindow(DebuggerWindowClass.lpszClassName, '', 0,
    0, 0, 0, 0, 0, 0, HInstance, nil);
end;

//------------------------------------------------------------------------------

type
  PBooleanArray = ^TBooleanArray;
  TBooleanArray = array [1..1000] of boolean;

function TNxDebuggerChannel.WndProc (Window: HWND; Message, wParam, lParam: Longint): Longint;
begin
  Result := 0;
  if Message = DebuggerListenMessage then
   case wParam of
   dmsgStartSession:    begin
                          FViewerWnd := lParam;
                        end;
   dmsgEndSession:      begin
                          FViewerWnd := 0;
                        end;
   dmsgEnableSection:   TNxDebugger(FDbgList.Items[lParam]).FEnabled := true;
   dmsgDisableSection:  TNxDebugger(FDbgList.Items[lParam]).FEnabled := false;
   else                 Result := 1;
   end;
end;

function TNxDebuggerChannel.GetActive : boolean;
begin
  Result := FViewerWnd <> 0;
end;

Function  LaunchOverseer(FileName : String) : DWORD;
VAR info    : _STARTUPINFOW;
VAR pi      : _PROCESS_INFORMATION;
VAR Retcode : DWORD;
VAR CurDir  : String;
VAR Msg     : tagMsg;
Begin
  GetDir(0, CurDir);
  FillChar (info, SizeOf (info), 0);
  info.cb := SizeOf (info);
  FillChar (pi, SizeOf (pi), 0);
//   Call CreateProcess
  If CreateProcess (PWideChar(FileName),
                    NIL,
                    NIL,
                    NIL,
                    True,
                    NORMAL_PRIORITY_CLASS,
                    NIL,
                    PWideChar(CurDir),
                    info,
                    pi) Then
  Begin
    WaitForInputIdle (pi.hProcess, INFINITE);
  End
  Else
    Result := GetLastError;
  Result := 32;
End;

procedure TNxDebuggerChannel.SetActive (Value : boolean);
var R : TRegistry;
    S : String;
    H : HWND;
    I : integer;
begin
  if Value <> Active then
   if Value then begin
     StartDebugSession;
     if not Active then begin
       // Start debugger
       R := TRegistry.Create;
       try
         R.OpenKey (NxDelphreeRootKey+NxNexusKey+NxDebugKey,true);
         if R.ValueExists ('Viewer') then begin
           S := R.ReadString ('Viewer');
           if S <> '' then begin
             I := 0;  H := 0;
             if LaunchOverseer(S) > 31 then
              while (H = 0) and (I < 1000) do begin
                Application.ProcessMessages;
                H := FindWindow ('TfrmDebugger', nil);
                inc(I);
              end;
           end;
           StartDebugSession;
         end;
       finally
         R.Free;
       end;
     end;
   end
   else EndDebugSession;
end;

//------------------------------------------------------------------------------

procedure TNxDebuggerChannel.StartDebugSession;
var M : TNxDebugMessageDesc;
begin
  SendMessage (HWND_BROADCAST,FInterceptMessage,dmsgStartSession,FDebuggerWnd);
  with M do begin
    MsgType := dbgClientData;
    MsgText := Application.ExeName;
    AttachmentType := NxNullGUID;
    AttachmentSize := 0;
  end;
  Send (nil,M);
  SendSections;
end;

procedure TNxDebuggerChannel.EndDebugSession;
var I : integer;
begin
  SendMessage (FViewerWnd,FInterceptMessage,dmsgEndSession,FDebuggerWnd);
  for I := 0 to FDbgList.Count-1 do begin
    TNxDebugger(FDbgList.Items[I]).FEnabled := false;
  end;
end;

procedure TNxDebuggerChannel.Send (ADbg : TNxDebugger; AMsg : TNxDebugMessageDesc);
var M : PNxDebugMessage;
    I : integer;
    Target,
    Source : PChar;
    CDS : TCopyDataStruct;
begin
  if not Active then exit;
  CDS.cbData := sizeof (TNxDebugMessage) + (length (AMsg.MsgText) + 1) * SizeOf(Char) + AMsg.AttachmentSize;
  M := AllocMem (CDS.cbData);
  M^.Section := FDbgList.IndexOf (ADbg);
  M^.MsgType := AMsg.MsgType;
  M^.MsgSize := length (AMsg.MsgText);
  M^.MsgTime := Now;
  M^.AttachmentType := AMsg.AttachmentType;
  M^.AttachmentSize := AMsg.AttachmentSize;
  Target := PChar (@M^.Data);
  Source := PChar (AMsg.Attachment);
  if M.MsgSize > 0 then
   for I := 1 to M.MsgSize do begin
     Target[0] := AMsg.MsgText[I];
     inc (Target);
   end;
  for I := 1 to M.AttachmentSize do begin
    Target[0] := Source[0];
    inc (Target); inc (Source);
  end;
  try
    CDS.lpData := M;
    SendMessage (FViewerWnd, WM_COPYDATA, 0, LParam (@CDS));
  finally
    FreeMem (M);
  end;
end;

//------------------------------------------------------------------------------
// TNxDebugger                                                                 |
//------------------------------------------------------------------------------

constructor TNxDebugger.Create (ID, Name : string);
begin
  inherited Create;
  FID := ID;
  FName := Name;
  FAutoStart := true;
  NxDebuggerChannel.RegisterDebugger (self);
end;

destructor TNxDebugger.Destroy;
begin
  if (Self = Debugger) and (not DebuggerClosing) then
    LogError('Debugger destroyed too early.');
  NxDebuggerChannel.UnregisterDebugger (self);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TNxDebugger.Send (AMsg : TNxDebugMessageDesc);
begin
  if CheckEnabled then NxDebuggerChannel.Send (self,AMsg);
end;

type
  ColorTableRec = record
    Name        : string;
    Color       : TColor;
  end;

const
  MaxColor   = 45;

function TNxDebugger.PointToString (APoint : TPoint) : string;
begin
  Result := '(' + IntToStr (APoint.x) + ',' + IntToStr (APoint.y) + ')';
end;

function TNxDebugger.RectToString (ARect : TRect) : string;
begin
  Result := '(' + PointToString (ARect.TopLeft) + ',' + PointToString (ARect.BottomRight) + ')';
end;

function TNxDebugger.CheckEnabled : boolean;
begin
  if (not NxDebuggerChannel.Active) and FAutoStart then NxDebuggerChannel.StartDebugSession;
  FAutoStart := false;
  Result := Enabled;
end;

//------------------------------------------------------------------------------

procedure TNxDebugger.LogMsg (AMsg : string);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgInfo;
    MsgText := AMsg;
    AttachmentType := NxNullGUID;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogMsgEx (AMsgType : integer; AMsg : string);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := AMsgType;
    MsgText := AMsg;
    AttachmentType := NxNullGUID;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogFmtMsg (Fmt : string; Args : array of const);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgInfo;
    MsgText := Format (Fmt,Args);
    AttachmentType := NxNullGUID;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogFmtMsgEx (AMsgType : integer; Fmt : string; Args : array of const);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := AMsgType;
    MsgText := Format (Fmt,Args);
    AttachmentType := NxNullGUID;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogNote (AMsg : string);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgNote;
    MsgText := AMsg;
    AttachmentType := NxNullGUID;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogFmtNote (Fmt : string; Args : array of const);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgNote;
    MsgText := Format (Fmt,Args);
    AttachmentType := NxNullGUID;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogError (AMsg : string);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgError;
    MsgText := AMsg;
    AttachmentType := NxNullGUID;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogFmtError (Fmt : string; Args : array of const);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgError;
    MsgText := Format (Fmt,Args);
    AttachmentType := NxNullGUID;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogWarning (AMsg : string);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgWarning;
    MsgText := AMsg;
    AttachmentType := NxNullGUID;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogFmtWarning (Fmt : string; Args : array of const);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgWarning;
    MsgText := Format (Fmt,Args);
    AttachmentType := NxNullGUID;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogAssigned (AMsg : string; Value : pointer);
var M : TNxDebugMessageDesc;
    S : string;
begin
  if not CheckEnabled then exit;
  if assigned (Value) then S := ' (Assigned)'
  else S := ' (Unassigned)';
  with M do begin
    MsgType := dbgInfo;
    MsgText := AMsg + S;
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogAssignedEx (AMsgType : integer; AMsg : string; Value : pointer);
var M : TNxDebugMessageDesc;
    S : string;
begin
  if not CheckEnabled then exit;
  if assigned (Value) then S := ' (Assigned)'
  else S := ' (Unassigned)';
  with M do begin
    MsgType := AMsgType;
    MsgText := AMsg + S;
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogBoolean (AMsg : string; Expresion : boolean);
var M : TNxDebugMessageDesc;
    S : string;
begin
  if not CheckEnabled then exit;
  if Expresion then S := ' (TRUE)'
  else S := ' (FALSE)';
  with M do begin
    MsgType := dbgInfo;
    MsgText := AMsg + S;
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogBooleanEx (AMsgType : integer; AMsg : string; Expresion : boolean);
var M : TNxDebugMessageDesc;
    S : string;
begin
  if not CheckEnabled then exit;
  if Expresion then S := ' (TRUE)'
  else S := ' (FALSE)';
  with M do begin
    MsgType := AMsgType;
    MsgText := AMsg + S;
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogColor (AMsg : string; Value : TColor);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgInfo;
    MsgText := AMsg + ' '+ ColorToString (Value);
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogColorEx (AMsgType : integer; AMsg : string; Value : TColor);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := AMsgType;
    MsgText := AMsg;
    MsgText := AMsg + ' '+ ColorToString (Value);
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogInteger (AMsg : string; Value : integer);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgInfo;
    MsgText := AMsg + ' (' + IntToStr (Value) + ')';
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogIntegerEx (AMsgType : integer; AMsg : string; Value : integer);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := AMsgType;
    MsgText := AMsg + ' (' + IntToStr (Value) + ')';
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogPoint (AMsg : string; Value : TPoint);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgInfo;
    MsgText := AMsg +' ' + PointToString (Value);
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogPointEx (AMsgType : integer; AMsg : string; Value : TPoint);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := AMsgType;
    MsgText := AMsg +' ' + PointToString (Value);
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogRect (AMsg : string; Value : TRect);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgInfo;
    MsgText := AMsg + ' ' + RectToString (Value);
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogRectEx (AMsgType : integer; AMsg : string; Value : TRect);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := AMsgType;
    MsgText := AMsg + ' ' + RectToString (Value);
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogString (AMsg : string; Value : string);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgInfo;
    MsgText := AMsg;
    AttachmentType := attPChar;
    AttachmentSize := length (Value) + 1;
    Attachment := pointer(Value);
  end;
  Send (M);
end;

procedure TNxDebugger.LogStringEx (AMsgType : integer; AMsg : string; Value : string);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := AMsgType;
    MsgText := AMsg;
    AttachmentType := attPChar;
    AttachmentSize := length (Value) + 1;
    Attachment := Pointer(Value);
  end;
  Send (M);
end;

procedure TNxDebugger.LogProperty (AMsg : string; Obj : TObject; PropName : string);
var M : TNxDebugMessageDesc;
begin
  raise Exception.Create('uDbg: LogProperty() not implemented and purpose unknown');
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgProperty;
    MsgText := AMsg;
//    AttachmentType := attComponentStream;
//    AttachmentSize := S.Size;
//    Attachment := S.Memory;
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LogObject (AMsg : string; Obj : TObject);
var M : TNxDebugMessageDesc;
    S : TStringStream;
    JSONString : String;
begin
  if not CheckEnabled then exit;
  S := TStringStream.Create;
  try
    JSONString := TJson.ObjectToJsonString(Obj, [joDateIsUTC, joDateFormatISO8601]);
    S.WriteString(JSONString);
    with M do begin
      MsgType := dbgObject;
      MsgText := AMsg;
      AttachmentType := StringToGUID(attObjectPropertiesStr);
      AttachmentSize := S.Size;
      Attachment     := S.Memory;
    end;
    Send (M);
  finally
    S.Free;
  end;
end;

procedure TNxDebugger.LogComponent (AMsg : string; Obj : TComponent);
var M : TNxDebugMessageDesc;
    S : TMemoryStream;
begin
  raise Exception.Create('uDbg: LogComponent() does not work. Use LogObject instead');
  if not CheckEnabled then exit;
  S := TMemoryStream.Create;
  try
    S.WriteComponent (Obj);
    with M do begin
      MsgType := dbgStream;
      MsgText := AMsg;
      AttachmentType := attComponentStream;
      AttachmentSize := S.Size;
      Attachment := S.Memory;
    end;
    Send (M);
  finally
    S.Free;
  end;
end;

procedure TNxDebugger.LogStringList (AMsg : string; List : TStrings);
var M : TNxDebugMessageDesc;
    S : TMemoryStream;
begin
  raise Exception.Create('uDbg: LogStringList() does not work. Use LogObject instead');
  if not CheckEnabled then exit;
  S := TMemoryStream.Create;
  try
    List.SaveToStream (S);
    with M do begin
      MsgType := dbgStringList;
      MsgText := AMsg;
      AttachmentType := attStringList;
      AttachmentSize := S.Size;
      Attachment := S.Memory;
    end;
    Send (M);
  finally
    S.Free;
  end;
end;

procedure TNxDebugger.AddCheckPoint (CheckName : string);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgCheckPoint;
    MsgText := CheckName;
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.AddSeparator;
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgSeparator;
    MsgText := '-';
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.Clear;
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgClear;
    MsgText := '';
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.ScratchPad (LineIndex : integer; Msg : string);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgBlackboard;
    MsgText := Msg;
    AttachmentType := NxNullGuid;
    AttachmentSize := sizeof (integer);
    Attachment := @LineIndex;
  end;
  Send (M);
end;

procedure TNxDebugger.FmtScratchPad (LineIndex : integer; Fmt : string; Args : array of const);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgBlackboard;
    MsgText := Format (Fmt,Args);
    AttachmentType := NxNullGuid;
    AttachmentSize := sizeof (integer);
    Attachment := @LineIndex;
  end;
  Send (M);
end;

procedure TNxDebugger.EnterProc (AMsg : string);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgEnterMethod;
    MsgText := AMsg;
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

procedure TNxDebugger.LeaveProc (AMsg : string);
var M : TNxDebugMessageDesc;
begin
  if not CheckEnabled then exit;
  with M do begin
    MsgType := dbgExitMethod;
    MsgText := AMsg;
    AttachmentType := NxNullGuid;
    AttachmentSize := 0;
  end;
  Send (M);
end;

//------------------------------------------------------------------------------

initialization
  NxDebuggerChannel := TNxDebuggerChannel.Create;
  Debugger := TNxDebugger.Create ('General','All nonspecific messages');

finalization
  DebuggerClosing := True;
  Debugger.Free;
  NxDebuggerChannel.Free;

end.
