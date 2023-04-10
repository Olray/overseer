program OverseerConsoleTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  Windows,
  uDbg,
  uDbgIntf;

type
  TMyTestClass = class(TObject)
    private
    aPrivateMember : String;
    public
    memberVariable: String;
    anotherMemberVariable: String;
    oneInteger : Integer;
    oneStringList : TStringList;
    constructor Create;
  end;

  { TMyTestClass }

constructor TMyTestClass.Create;
begin
  memberVariable := 'TestString';
  anotherMemberVariable := 'Another TestString';
  oneInteger := 4711;
  oneStringList := TStringList.Create;
  oneStringList.Add('First String');
  oneStringList.Add('Second String');
  oneStringList.Add('Third String');
  aPrivateMember := 'PrivateMember';
end;

  { main }

VAR TestClass : TMyTestClass;
begin
  try
    Debugger.Clear;
    Debugger.EnterProc('main');
    Debugger.LogMsg('Simple String Message');
    Debugger.LogMsgEx(dbgInfo, 'Info Message');
    Debugger.LogMsgEx(dbgError, 'Error Message');
    Debugger.LogMsgEx(dbgWarning, 'Warning Message');
    Debugger.LogFmtMsg('Formatted message with int(%d) float(%3.2f) string(%s) values', [4711, 123.45, 'teststring']);
    Debugger.LogFmtMsgEx(dbgError, 'Same as above as Error with int(%d) float(%3.2f) string(%s) values', [4711, 123.45, 'teststring']);
    Debugger.LogFmtMsgEx(dbgWarning, 'Same as above as Warning with int(%d) float(%3.2f) string(%s) values', [4711, 123.45, 'teststring']);
    Debugger.LogNote('This is a note message with LogNote');
    Debugger.LogFmtNote('This is a formatted note message with int(%d) float(%3.2f) string(%s) values', [4711, 123.45, 'teststring']);
    Debugger.LogAssigned('Output of LogAssigned with uninitialized object', TestClass);
    TestClass := TMyTestClass.Create;
    Debugger.LogAssigned('Output of LogAssigned with initialized object', TestClass);
    Debugger.LogColor('Colour White (defined by RGB(255,255,255))', RGB(255,255,255));
    Debugger.LogColor('Colour Black (defined by RGB(0,0,0))', RGB(0,0,0));
    Debugger.LogInteger('An integer value', 4711);
    Debugger.LogPoint('A TPoint value', TPoint.Create(1,2));
    Debugger.LogRect('A TRect value', TRect.Create(1,2,3,4));
    Debugger.AddCheckPoint('A check point');
    Debugger.LogObject('Instance of TMyTestClass', TestClass);
  //  Debugger.LogProperty('Property of TMyTestClass', TestClass, 'memberVariable');
  //  Debugger.LogComponent('OverseerTest''s button', btnTestEnterLeave);
  //  Debugger.LogStringList('String list of TestClass:', TestClass.oneStringList);
    for var i := 0 to 4 do
      Debugger.FmtScratchPad(i, 'Line %d', [i]);

    Debugger.LeaveProc('main');
    Debugger.AddSeparator;
    FreeAndNil(TestClass);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
