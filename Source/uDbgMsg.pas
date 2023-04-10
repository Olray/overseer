{==============================================================================|
| Project : Delphree - Nexus - Overseer                          | 001.003.000 |
|==============================================================================|
| Content: Overseer messages.                                                  |
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
|          21.2.1999 Version 001.003.000 (Pavel Cisar)                         |
|          Streaming messages + minor changes.                                 |
|          9.4.2023 Version 001.003.000 (Markus Stenzel)                       |
|          Updated to Unicode for Delphi 10.4 "Sydney"                         |
|          Implemented class logging through JSON string                       |
|          Added object viewer with code by pawel.glowacki@embarcadero.com     |
|==============================================================================}

unit uDbgMsg;

interface

uses ComObj, Activex, Classes, Controls, extctrls, Windows, Messages, SysUtils,
     System.Json, JSONDoc, JSONTreeView, Forms, uDbgIntf, stdctrls, graphics,
     VCL.ComCtrls;

const
  WM_NewDbgMsg = WM_USER + 1;

type
  TNxViewerMessage = class (TObject)
  private
    FSection        : integer;
    FMsgType        : integer;
    FMsgText        : string;
    FMsgTime        : TDateTime;
    FIndent         : integer;
    FAttachmentType : TGUID;
    FAttachmentSize : integer;
    FAttachment     : pointer;
  protected
  public
    constructor Create;
    constructor CreateFrom (AMsg : PNxDebugMessage);
    destructor Destroy; override;
    procedure SaveToStream (AWriter : TWriter);
    procedure LoadFromStream (AReader : TReader);
    property MsgType        : integer read FMsgType write FMsgType;
    property MsgText        : string read FMsgText write FMsgText;
    property MsgTime        : TDateTime read FMsgTime write FMsgTime;
    property AttachmentType : TGUID read FAttachmentType write FAttachmentType;
    property AttachmentSize : integer read FAttachmentSize write FAttachmentSize;
    property Attachment     : pointer read FAttachment write FAttachment;
    property Indent         : integer read FIndent write FIndent;
    property Section        : integer read FSection write FSection;
  end;

  TNxDataViewer = class (TObject)
  private
    FViewer : INxDataViewer;
  protected
  public
    constructor Create (AViewerID : TGUID); virtual;
    destructor Destroy; override;
    procedure Activate (ASize : integer; AData : pointer);
    procedure Deactivate;
  end;

  TNxCustomViewer = class (TInterfacedObject, INxDataViewer)
  private
    FViewerControl : TControl;
  protected
    procedure ShowData (ASize : integer; AData : pointer); virtual; abstract;
  public
    constructor Create; virtual;
    procedure Activate (APanel : TPanel; ASize : integer; AData : pointer); virtual;
    procedure Deactivate (APanel : TPanel); virtual;
  end;

  TNxObjectPropertiesViewer = class (TNxCustomViewer)
  private
    FTreeView : TJSONTreeView;
  protected
    procedure ShowData (ASize : integer; AData : pointer); override;
    function MemoryToJSONString(ASize : integer; AData : pointer) : String;
    procedure AddJsonItemToTreeView(Node : TTreeNodes; JsonItem : TJsonValue);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNxComponentStreamViewer = class (TNxCustomViewer)
  private
    FList : TMemo;
  protected
    procedure ShowData (ASize : integer; AData : pointer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNxPCharViewer = class (TNxCustomViewer)
  private
    FList : TMemo;
  protected
    procedure ShowData (ASize : integer; AData : pointer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TNxStringListViewer = class (TNxCustomViewer)
  private
    FList : TMemo;
  protected
    procedure ShowData (ASize : integer; AData : pointer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses fDebug;

//------------------------------------------------------------------------------
// TNxViewerMessage                                                            |
//------------------------------------------------------------------------------

constructor TNxViewerMessage.Create;
begin
  inherited;
end;

constructor TNxViewerMessage.CreateFrom (AMsg : PNxDebugMessage);
begin
  inherited Create;
  FSection := AMsg^.Section;
  FMsgType := AMsg^.MsgType;
  SetString (FMsgText,PChar (@AMsg^.Data),AMsg^.MsgSize);
  FMsgTime := AMsg^.MsgTime;
  FAttachmentType := AMsg^.AttachmentType;
  FAttachmentSize := AMsg^.AttachmentSize;
  if AttachmentSize > 0 then begin
    FAttachment := AllocMem (AttachmentSize);
    move (PChar (@AMsg^.Data)[AMsg^.MsgSize],FAttachment^,AttachmentSize);
  end;
  if not NxCompareGUIDs (NxNullGuid,FAttachmentType) then begin

  end;
end;

destructor TNxViewerMessage.Destroy;
begin
  if AttachmentSize > 0 then begin
    FreeMem (FAttachment);
  end;
  inherited;
end;

procedure TNxViewerMessage.SaveToStream (AWriter : TWriter);
begin
  AWriter.WriteInteger (FSection);
  AWriter.WriteInteger (FMsgType);
  AWriter.WriteInteger (FIndent);
  AWriter.WriteString (FMsgText);
  AWriter.Write (FMsgTime,sizeof (TDateTime));
  AWriter.Write (FAttachmentType,sizeof (TGUID));
  AWriter.WriteInteger (FAttachmentSize);
  if AttachmentSize > 0 then AWriter.Write (FAttachment^,FAttachmentSize);
end;

procedure TNxViewerMessage.LoadFromStream (AReader : TReader);
begin
  FSection := AReader.ReadInteger;
  FMsgType := AReader.ReadInteger;
  FIndent := AReader.ReadInteger;
  FMsgText := AReader.ReadString;
  AReader.Read (FMsgTime,sizeof (TDateTime));
  AReader.Read (FAttachmentType,sizeof (TGUID));
  FAttachmentSize := AReader.ReadInteger;
  if AttachmentSize > 0 then begin
    FAttachment := AllocMem (AttachmentSize);
    AReader.Read (FAttachment^,FAttachmentSize);
  end;
end;

//------------------------------------------------------------------------------
// TNxDataViewer                                                               |
//------------------------------------------------------------------------------

const
  MaxDefaultViewer = 4;
  DefaultViewers : array [1..MaxDefaultViewer] of TGUID =
                   (attObjectPropertiesStr,attComponentStreamStr,
                    attPCharStr,attStringListStr);

constructor TNxDataViewer.Create (AViewerID : TGUID);
var I,VI : integer;
begin
  inherited Create;
  VI := 0;
  for I := 1 to MaxDefaultViewer do
   if NxCompareGUIDs (AViewerID,DefaultViewers[I]) then begin
     VI := I;
     break;
   end;
  case VI of
  1:      begin
            FViewer := TNxObjectPropertiesViewer.Create;
          end;  //attObjectPropertiesStr
  2:      begin
            FViewer := TNxComponentStreamViewer.Create;
          end; //attComponentStreamStr
  3:      begin
            FViewer := TNxPCharViewer.Create;
          end; //attPCharStr
  4:      begin
            FViewer := TNxStringListViewer.Create;
          end; //attStringListStr
  else    begin
            CoCreateInstance (AViewerID,nil,CLSCTX_INPROC_SERVER and CLSCTX_INPROC_HANDLER,INxDataViewer,FViewer);
          end;
  end;
end;

destructor TNxDataViewer.Destroy;
begin
  FViewer := nil;
  inherited;
end;

procedure TNxDataViewer.Activate (ASize : integer; AData : pointer);
begin
  FViewer.Activate (frmDebugger.pnDetail,ASize, AData);
end;

procedure TNxDataViewer.Deactivate;
begin
  FViewer.Deactivate (frmDebugger.pnDetail);
end;

//------------------------------------------------------------------------------
// TNxCustomViewer                                                             |
//------------------------------------------------------------------------------

constructor TNxCustomViewer.Create;
begin
  inherited;
end;

procedure TNxCustomViewer.Activate (APanel : TPanel; ASize : integer; AData : pointer);
begin
  FViewerControl.Parent := APanel;
  ShowData (ASize, AData);
end;

procedure TNxCustomViewer.Deactivate (APanel : TPanel);
begin
  FViewerControl.Parent := nil;
end;

//------------------------------------------------------------------------------
// TNxObjectPropertiesViewer                                                   |
//------------------------------------------------------------------------------

constructor TNxObjectPropertiesViewer.Create;
begin
  inherited;
  FViewerControl := TPanel.Create (nil);
  TPanel (FViewerControl).BevelOuter := bvNone;
  FViewerControl.Align := alClient;
  with TPanel.Create (FViewerControl) do begin
    Parent := TWinControl (Owner);
    Align := alTop;
    BorderStyle := bsSingle;
    Height := 20;
    Caption := 'Object fields';
    Font.Style := [fsBold];
  end;
  FTreeView := TJsonTreeView.Create (FViewerControl);
  with FTreeView do begin
    Parent := TWinControl (Owner);
    Align := alClient;
    ReadOnly := true;
  end;
end;

destructor TNxObjectPropertiesViewer.Destroy;
begin
  FreeAndNil(FTreeView);
  inherited;
end;

procedure TNxObjectPropertiesViewer.ShowData (ASize : integer; AData : pointer);
var JSONString : String;
var JsonRoot   : TJsonValue;
var JsonDocument : TJSONDocument;
begin
  JSONString := MemoryToJSONString(ASize, AData);
  JsonDocument := TJSONDocument.Create(nil);
  JsonDocument.JsonText := JSONString;
  FTreeView.ClearAll;
  FTreeView.JSONDocument := JsonDocument;
end;

function TNxObjectPropertiesViewer.MemoryToJSONString(ASize : integer; AData : pointer) : string;
var I : TMemoryStream;
var O : TStringStream;
begin
  I := TMemoryStream.Create;
  O := TStringStream.Create;
  try
    I.Write (AData^,ASize);
    I.Position := 0;
    O.LoadFromStream(I);
    O.Position := 0;
    Result := O.ReadString(ASize);
  finally
    I.Free;
    O.Free;
  end;
end;

procedure TNxObjectPropertiesViewer.AddJsonItemToTreeView(Node : TTreeNodes; JsonItem : TJsonValue);
begin
  // Simple Json value
  if(JsonItem is TJSONNumber) or
    (JsonItem is TJSONString) or
    (JsonItem is TJSONTrue) or
    (JsonItem is TJSONFalse) or
    (JsonItem is TJSONNull) then
  begin
    Node.AddChild(nil, JSonItem.Value)
  end
  else if JsonItem is TJSONObject then
  begin

  end;

end;

//------------------------------------------------------------------------------
// TNxComponentStreamViewer                                                    |
//------------------------------------------------------------------------------

constructor TNxComponentStreamViewer.Create;
begin
  inherited;
  FViewerControl := TPanel.Create (nil);
  TPanel (FViewerControl).BevelOuter := bvNone;
  FViewerControl.Align := alClient;
  with TPanel.Create (FViewerControl) do begin
    Parent := TWinControl (Owner);
    Align := alTop;
    BorderStyle := bsSingle;
    Height := 20;
    Caption := 'Stream output';
    Font.Style := [fsBold];
  end;
  FList := TMemo.Create (FViewerControl);
  with FList do begin
    Parent := TWinControl (Owner);
    Align := alClient;
    ReadOnly := true;
    ScrollBars := ssBoth;
  end;
end;

destructor TNxComponentStreamViewer.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TNxComponentStreamViewer.ShowData (ASize : integer; AData : pointer);
var I, O : TMemoryStream;
begin
  I := TMemoryStream.Create;
  O := TMemoryStream.Create;
  try
    I.Write (AData^,ASize);
    I.Position := 0;
    ObjectBinaryToText (I,O);
    FList.Lines.Text := strpas(PChar (O.Memory));
  finally
    I.Free;
    O.Free;
  end;
end;

//------------------------------------------------------------------------------
// TNxPCharViewer                                                              |
//------------------------------------------------------------------------------

constructor TNxPCharViewer.Create;
begin
  inherited;
  FViewerControl := TPanel.Create (nil);
  TPanel (FViewerControl).BevelOuter := bvNone;
  FViewerControl.Align := alClient;
  with TPanel.Create (FViewerControl) do begin
    Parent := TWinControl (Owner);
    Align := alTop;
    BorderStyle := bsSingle;
    Height := 20;
    Caption := 'String content';
    Font.Style := [fsBold];
  end;
  FList := TMemo.Create (FViewerControl);
  with FList do begin
    Parent := TWinControl (Owner);
    Align := alClient;
    ReadOnly := true;
    WordWrap := true;
    ScrollBars := ssBoth;
  end;
end;

destructor TNxPCharViewer.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TNxPCharViewer.ShowData (ASize : integer; AData : pointer);
var S : string;
begin
  S := strpas(PChar (AData));
  SetLength (S,ASize);
  FList.Lines.Text := S;
end;

//------------------------------------------------------------------------------
// TNxStringListViewer                                                         |
//------------------------------------------------------------------------------

constructor TNxStringListViewer.Create;
begin
  inherited;
  FViewerControl := TPanel.Create (nil);
  TPanel (FViewerControl).BevelOuter := bvNone;
  FViewerControl.Align := alClient;
  with TPanel.Create (FViewerControl) do begin
    Parent := TWinControl (Owner);
    Align := alTop;
    BorderStyle := bsSingle;
    Height := 20;
    Caption := 'String list';
    Font.Style := [fsBold];
  end;
  FList := TMemo.Create (FViewerControl);
  with FList do begin
    Parent := TWinControl (Owner);
    Align := alClient;
    ReadOnly := true;
    ScrollBars := ssBoth;
  end;
end;

destructor TNxStringListViewer.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TNxStringListViewer.ShowData (ASize : integer; AData : pointer);
var S : string;
begin
  S := strpas(PChar (AData));
  SetLength (S,ASize);
  FList.Lines.Text := S;
end;

end.
