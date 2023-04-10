Overseer
The Open Source message-based debugging tool

File content:

1.) About Overseer
2.) Distribution package
3.) Installation instructions
4.) Advanced Overseer features

WARNING!
To successfuly open Overseer source, read carefully installation instructions !!!

Overseer homesite is http://delphree.clexpert.com under project Nexus
Contact: pcisar@atlas.cz

1.) About Overseer

Overseer is DebugWindow-like tool with rich set of message formats and viewer
features (like basic code profiling). I make this one in compliance with
CodeSite(tm) by Raize Software Solutions (Http://www.raize.com) early in 1998.
Before CodeSite(tm) has gave me a first impulse to make Overseer, I'm using
similar tool from GExperts, but CodeSite(tm) open my eyes about what can be
made. I take very short API specification published by Raize Software Solutions
on their website and some screenshots as starting point for my thoughts. I'm
never take my hands over CodeSite(tm) or saw him in action, so I must reinvent
the wheel (and learn so much, because until I take this challenge, I wrote
another kind of software <g>). I make the Overseer for my own needs (and for my
friends and colleagues at work), over weekend (yes, in two days <g>) with all
major features what has now. I'm spent another week (yes, seven days <g>) to
make him look like CodeSite(tm), only for fun to do it that way. Over time
Overseer take great role in my work and his client-side parts are burned to deep
core of my application framework.

Because all those facts, the early Overseer distributions has very similar
look&feel as CodeSite(tm). This fact upset some peoples, so I did some changes
in Overseer client API and UI, to make segregation between these applications
(see HISTORY.TXT for details).

Of course, Overseer hasn't all CodeSite(tm) cool features, because I don't know
them all, I didn't enough time to implement all known ones, or I dislike them.
On other side, Overseer has some features that CodeSize(tm) hasn't, like multiple
debugging sections and some sort of plug-in framework for custom data message
attachments.

So, what are major Overseer features ?

1.) Different message types, like basic message, warning, error, checkpoint,
    note, enter/leave procedure.
2.) Data attachments to messages with common data support (integer, string,
    boolean, point, rectangle, pointer assigment, color, component published
    properties (via stream), stringlist)
3.) Blackboard for last-value/state string messages.
4.) Basic code profiling capabilities.
5.) Multiple sections of code. You can independently enable/disable each active
    section.
6.) Automatic Overseer startup on debug session start.
7.) Search messages containing specified string

Overseer is part of Nexus Framework (see http://delphree.clexpert.com
project NEXUS) developed, maintained and distributed under Delphree - The Delphi
Open Source Development Initiative. Could be used as standalone tool or as
part of Nexus Logging Framework (see "LogHUB" and "Overseer integration" in
Nexus documentation).

Enjoy it, improve it, do whatever you want (in compliance with licence <g>).

2.) Distribution package

Package must be unpacked with subdirectories.
There are these derectories:

\Bin		- Compiled Overseer ready for use (run once for registration).
\Images	        - Images used in Overseer.
\Lib    	- Designtime component and shared units (for use in client app.).
\Source         - Overseer source code.
\Demo		- Overseer demo application source code.

3.) Installation instructions

a) Overseer user installation

It's simple:
- Run Overseer executable once (this store his location to windows registry).
- Put uDbg and uDbgIntf units along your search or library path.
- Include uDbg unit in your project and use Debugger variable or spawn more
  TNxDebugger instances for partitioned debugging.
- If you want custom debug messages, you must include uDbgIntf unit too.
- Look at Demo application source, compile it and run it.

b) Overseer source code installation

It's not so hard:
- Register unit uDbgLB in any your designtime package (contains modified listbox
  used by Overseer).
- Overseer also use some RxLib components, so you must have them installed in IDE.
  He's builded along 2.4 version of RxLib, but should work with latter versions
  (not tested).
- After that, you can load Overseer source into Delphi IDE.

NOTE: Overseer is D3 application, but may be successfuly compiled under D2 and D4.

4.) Advanced Overseer features

There're some advanced features that Overseer has over CodeSite. One of them are
multiple debugging sections. That is, Overseer debugger client (TNxDebugger class)
is a section. Aside implicitly created TNxDebugger instance named Debugger, you
can spawn more additional instances for fine-grained inspecting. Whenever in
active debug session, you can go to Overseer Options dialog, select Sections tab
and inspect all currently active sections. You can also enable/disable each
section as well. Disabled sections are truly disabled at client side, so call to
them has minimal impact to code executing time (and code profiling). Enable state
is persisted into registry between sessions. Unfortunately, you can't see ALL
section DECLARED in your code, but only currently active (created) ones. So you
should create all sections in unit's initialization section and destroy them in
finalization. Otherwise you couldn't see them in Sections tab.

Last update 21.2.1999
