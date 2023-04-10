{==============================================================================|
| Project : Delphree - Nexus - Overseer                          | 001.001.000 |
|==============================================================================|
| Content: Overseer Client handler.                                            |
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
|==============================================================================}

unit uDbgHnd;

interface

uses Classes, Windows, Messages, uDbgIntf ;

type

  TNxDebuggerClientHandler = class (TObject)
  private
//    FDebugClient : hWnd ;
//    FMsgIndent : integer ;
//    FMsgList : TList ;
//    FCheckPoints : TStrings ;
    FSections : TStrings ;
    FSectionsEnabled : TList ;
    // Config
//    FPaused : boolean ;
    FShowTime : boolean ;
    FIndent : boolean ;
    FNavigateType : integer ;
//    FMarkOnNavigate : boolean ;
//    FShowSections : boolean ;
  protected
  public
    property Sections : TStrings read FSections ;
    property SectionsEnabled : TList read FSectionsEnabled ;
    property Indented : boolean read FIndent write FIndent ;
    property ShowTime : boolean read FShowTime write FShowTime ;
    property NavigateType : integer read FNavigateType write FNavigateType ;
  end ;

implementation

uses SysUtils;

end.
