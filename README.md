# overseer
Delphi 10.4 "Sydney" port of Nexus Overseer

## reason
Starting in the year 2000 I used this little tool for debugging of my personal small projects using Borland Delphi. Back then Overseer was a nice piece of code for simple debugging. Small overhead and very limited functionality, but still useful. After implementation of Unicode in Delphi 2009 the tool stopped working.

Embarcadero offers a free Community Edition of Delphi for non-profit projects, but they deliberately chose to exclude the Light version of CodeSight for their non-paying users.
To restore a very basic functionality of "live debugging" I have ported Overseer to Delphi 10.4.

## original author
This code is based on "Delphree Nexus Overseer" Version 001.001.000 by Pavel Cisar (Czech Republic) released on 7th February 1999.
The original archive is still available at Torry's: https://torry.net/pages.php?id=1521

## changes
Added Unicode (simply recalculated the message size in bytes to match Unicode string length)
Added logging of objects utilizing REST.Json.ObjectToJsonString() with all it's drawbacks

## thanks
The following tools have been used:

- Toolbar97 by Jordan Russell
- RxLib Unofficial version Rx library for Delphi Sydney
- JSON TreeView Component by Pawel Glowacki https://github.com/pglowack/DelphiJSONComponents

Because I'm unsure of the license, the "JSON TreeView Component" files are not included. 
You have to download the files "JSONDoc.pas" and "JSONTreeView.pas" using the link provided and place them into the ./Source folder.
For your convenience you can download a precompiled copy of Overseer.exe here: https://owncloud.hostingundservice.de/index.php/s/Rnn8CpbZKa7RExC
Please contact me at olray(at)allanime(dot)org if this link is broken.

## how to use in your projects
- put the "Lib" directory into your path
- put "uDbg" in your list of Uses
- call Debugger.<function> from your Delphi code while Overseer.exe is running.
- watch output live in the Overseer window
