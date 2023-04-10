{==============================================================================|
| Project : Delphree - Nexus - CodeOverseer                      | 001.000.000 |
|==============================================================================|
| Content: CodeOverseer listbox.                                               |
|==============================================================================|
| The contents of this file are subject to the Mozilla Public License Ver. 1.0 |
| (the "License"); you may not use this file except in compliance with the     |
| License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ |
|                                                                              |
| Software distributed under the License is distributed on an "AS IS" basis,   |
| WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for |
| the specific language governing rights and limitations under the License.    |
|==============================================================================|
| The Original Code is CodeOverseer for Nexus Delphi Library.                  |
|==============================================================================|
| The Initial Developer of the Original Code is Pavel Cisar (Czech Republic).  |
| Portions created by Pavel Cisar are Copyright (C) 1998, 1999.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: 1.2.1999 Version 001.000.000                                        |
|          First public release.                                               |
|==============================================================================}

unit uDbgLB;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     ComCtrls, StdCtrls;

type
  TNxDbgDrawList = class(TListBox)
  private
    FImageIndex : Integer;
    FIndent : integer ;
    FImageList : TImageList;
    FLeftText : string ;
    FRightText : string ;
    FShowImages : boolean ;
    Function GetImageList : TImageList;
    Procedure SetImageList(Value : TImageList);
    Function GetImageIndex : Integer;
    Procedure DrawImage(Index, NewIndex : Integer; Rect: TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);  override;
    property ImageIndex : Integer read FImageIndex write FImageIndex ;
    property Indent : integer read FIndent write FIndent ;
    property RightText : string read FRightText write FRightText ;
    property LeftText : string read FLeftText write FLeftText ;
  published
    property ImageList : TImageList read GetImageList write SetImageList ;
    property ShowImages : boolean read fShowImages write FShowImages ;
  end;

procedure Register;

implementation

// temporary (excerpt from NxCore)
function NxLeft(const Str: string; Len: Integer): string;
begin
  Result := Copy(Str, 1, Len);
end;

procedure TNxDbgDrawList.DrawImage(Index, NewIndex : Integer; Rect: TRect);
var MsgSize,MsgLeft,
    Msg2Left : integer ;
    Msg : string ;
begin
  Msg := FLeftText ;
  Canvas.FillRect(Rect);
  if length (RightText) > 0 then begin
    Msg2Left := Rect.Right - Canvas.TextWidth (RightText) - 5 ;
    Rect.Right := Msg2Left - 1 ;
  end
  else Msg2Left := Rect.Right ;
  if Msg = '-' then begin
    with Canvas do begin
      Pen.Color := Font.Color ;
      MoveTo (Rect.Left - 1,Rect.Top - ((Rect.Top - Rect.Bottom) div 2)) ;
      LineTo (Rect.Right,Rect.Top - ((Rect.Top - Rect.Bottom) div 2)) ;
    end ;
  end
  else
   if Msg = '\-' then begin
     with Canvas do begin
       Pen.Color := Font.Color ;
       Brush.Color := Font.Color ;
       Brush.Style := bsBDiagonal ;
       Rectangle (Rect.Left,Rect.Top,Rect.Right,Rect.Bottom) ;
     end ;
   end
   else begin
     MsgLeft := Rect.Left + Indent ;
     if ShowImages then begin
       FImageList.Draw (Canvas,MsgLeft, Rect.Top, GetImageIndex) ;
       MsgSize := Msg2Left - (MsgLeft + 2 + FImageList.Width) ;
       MsgLeft := MsgLeft + 2 + FImageList.Width ;
     end
     else begin
       MsgSize := Msg2Left - (Rect.Left + 2) ;
       MsgLeft := MsgLeft + 2 ;
     end ;
     while (length (Msg) > 0) and (Canvas.TextWidth (Msg) > (MsgSize)) do
      Msg := NxLeft (Msg,length (Msg)-1) ;
     if length (Msg) < length (Items.Strings[Index]) then begin
       if length (Msg) > 3 then Msg := NxLeft (Msg,length (Msg)-3) + '...' ;
     end ;
     Canvas.TextOut(MsgLeft, Rect.Top, Msg);
   end ;
  with Canvas do begin
    if length (RightText) > 0 then begin
      TextOut(Msg2Left, Rect.Top , RightText);
    end ;
  end ;
end;

Procedure TNxDbgDrawList.SetImageList(Value : TImageList);
Begin
  If Value <> FImageList Then
    FImageList := Value;
  If Not Assigned(FImageList) Then
    FImageList := Nil;
End;

Function TNxDbgDrawList.GetImageList : TImageList;
Begin
 Result := FImageList;
End;

Function TNxDbgDrawList.GetImageIndex : Integer;
Begin
 Result := FImageIndex;
End;

procedure TNxDbgDrawList.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  inherited;
  DrawImage(Index, FImageIndex, Rect);
end;

constructor TNxDbgDrawList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
  ControlStyle:= ControlStyle + [csAcceptsControls];
end;

Destructor TNxDbgDrawList.Destroy;
Begin
 inherited Destroy;
End;

procedure Register;
begin
  RegisterComponents('Nexus', [TNxDbgDrawList]);
end;

end.
