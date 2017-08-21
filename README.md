SMS Z80 instruction exerciser
=============================

This is a port of ZEXALL, the Z80 instruction exerciser, to the Sega
Master System. It was written by Frank Cringle to go with his YAZE
emulator. It was ported to CP/M and Spectrum by J.G.Harston, and
subsequently to the SMS by Brett K. Unfortunately, his port didn't work
on a real system due to some mistakes in the SMS-specific parts and
(for some reason) produces different CRCs than were stored in the
Spectrum version, although the SMS does have a regular Z80 CPU.

So, I converted Brett K's source to compile with WLA DX (the best macro
assembler) and fixed the errors. The console emulation code he wrote is
excellent, by the way. I also masked out the undocumented flags from the
f register since no SMS games seem to depend on them and if an emulator
doesn't implement them it would otherwise fail almost every test. Then I
ran it on a real SMS and noted the results.

I also changed the ordering of the tests so that the quickest are
completed first. A few of the tests are a LOT longer than others. The
shortest complete in a fraction of a second and the longest takes (I
think) well over an hour. You have been warned.

Since ZEXALL is covered by the GNU, I am including the source and
licence.

Maxim

=============================

June 12, 2003

Eric R. Quinn

Added conditional assembly direction enabling test output to go to
SDSC Debug Console instead of SMS VDP.  The motivation for doing this
was to eliminate the need for having a functioning VDP in a SMS emulator
before running ZEXALL.  (Hopefully, the SDSC Debug Console is easier to
implement than a VDP.)

The binary zexall.sms uses the SMS VDP.

The binary zexall_sdsc.sms uses the SDSC Debug Console.

