{==============================================================================|
| Project : Delphree - Nexus - Overseer                          | 001.001.000 |
|==============================================================================|
| Content: Overseer interface.                                                 |
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
|==============================================================================}

unit uDbgIntf;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Vcl.Controls,
  Vcl.extctrls,
  Vcl.Graphics ;

const
  dmsgStartSession       = 1 ;
  dmsgEndSession         = 2 ;
  dmsgBlockSections      = 3 ;
  dmsgEnableSection      = 4 ;
  dmsgDisableSection     = 5 ;

  dbgInfo                = 1 ;
  dbgWarning             = 2 ;
  dbgError               = 3 ;
  dbgCheckPoint          = 4 ;
  dbgNote                = 5 ;
  dbgEnterMethod         = 6 ;
  dbgExitMethod          = 7 ;
  dbgSeparator           = 8 ;
  dbgInactiveSeparator   = 9 ;
  dbgObject              = 10 ;
  dbgStream              = 11 ;
  dbgStringList          = 12 ;
  dbgProperty            = 13 ;
  dbgClear               = 14 ;
  dbgBlackboard          = 15 ;
  dbgInternals           = 100 ;
  dbgClientData          = dbgInternals ;
  dbgSections            = dbgInternals + 1 ;

  attObjectPropertiesStr    = '{2106C780-E528-11D1-A311-BEC8F308CF9B}' ;
  attComponentStreamStr     = '{2106C781-E528-11D1-A311-BEC8F308CF9B}' ;
  attPCharStr               = '{2106C787-E528-11D1-A311-BEC8F308CF9B}' ;
  attStringListStr          = '{2106C788-E528-11D1-A311-BEC8F308CF9B}' ;

  attObjectProperties    : TGUID = attObjectPropertiesStr ;
  attComponentStream     : TGUID = attComponentStreamStr ;
  attPChar               : TGUID = attPCharStr ;
  attStringList          : TGUID = attStringListStr ;

type
  TNxDebugMessageDesc = record
    MsgType           : integer ;
    MsgText           : string ;
    MsgTime           : TDateTime ;
    AttachmentType    : TGUID ;
    AttachmentSize    : integer ;
    Attachment        : pointer ;
  end ;

  PNxDebugMessage     = ^TNxDebugMessage ;
  TNxDebugMessage     = record
    Client            : integer ;
    Section           : integer ;
    MsgType           : integer ;
    MsgSize           : integer ;
    MsgTime           : TDateTime ;
    AttachmentType    : TGUID ;
    AttachmentSize    : integer ;
    Data              : byte ;
  end ;

  INxDataViewer = interface (IUnknown)
  ['{9305F020-E82C-11D1-A311-E26DA411FC59}']
    procedure Activate (APanel : TPanel; ASize : integer; AData : pointer) ;
    procedure Deactivate (APanel : TPanel) ;
  end ;

implementation

end.
