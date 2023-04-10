# overseer
Delphi 10.4 "Sydney" port of Nexus Overseer

## reason
Starting in the year 2000 I used this little tool for debugging of my personal small projects using Borland Delphi. Back then Overseer was a nice piece of code for simple debugging. Small overhead and very limited functionality, but still useful. After implementation of Unicode in Delphi 2009 the tool stopped working.

Embarcadero offers a free Community Edition of Delphi for non-profit projects, but they deliberately chose to exclude the Light verion of CodeSight for their non-paying users. To restore a very basic functionality of "live debugging" I have ported Overseer to Delphi 10.4.

## original author
This code is based on "Delphree Nexus Overseer" Version 001.001.000 by Pavel Cisar (Czech Republic) released on 7th February 1999.
The original archive is still available at Torry's: https://torry.net/pages.php?id=1521

## changes
Added Unicode (simply recalculated the message size in bytes to match Unicode string length)
Added logging of objects utilizing REST.Json.ObjectToJsonString() with all it's drawbacks

## thanks
The following tools have been used:

- JSON TreeView Component by Pawel Glowacki
