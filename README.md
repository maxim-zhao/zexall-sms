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

=============================

2021/10/16

Version 0.17

This version supports various options for output: it will emit to the SMS 
VDP in mode 4 if detected, else it will try a TMS9918 VDP in mode 2 (text 
mode), so this should work on an SG-1000 or SC-3000. It will also emit 
simultaneously to the SDSC Debug Console and also write ASCII text to SRAM.

Press Up to force mode 4, and Down to force text mode.

zexall.sms includes the undocumented bits in the flags register, zexdoc.sms
ignores them.

You can build your own version if you want some different combination of
settings.

Fonts used are https://damieng.com/typography/zx-origins/envious for mode 2
and https://damieng.com/typography/zx-origins/zx-eurostile for mode 4.

=============================

2025/06/16

Version 0.20

This version solves a couple of long-standing issues caused by inconsistency 
between Z80 CPUs, specifically the undocumented flags' state after the `scf`
and `ccf` opcodes. All tests should now pass on "normal" Z80s seen in Sega
8-bit and 16-bit systems.

Because this was achieved by ignoring these flags in this scenario, an 
additional pre-test was added which checks the flags after each of `scf` and 
`ccf`, with different preconditions regarding the contents of register `a`
and the last ALU operation's flags result. Each is run $1fff times, and the 
screen shows the count of how many times each flag was set; it is expected 
that:

1. The numbers are usually all either 0 or 1fff - where any other value is seen, it
   indicates that the flag value is not stable. This has been observed on
   Sega Mark III (with a Zilog CPU) and Sega Genesis 2 (with a Sega 315-5676 
   Z80 clone, probably made by NEC).
2. The pattern of which are 0 and which are 1fff is stable for a given CPU
   type, but varies by CPU type. At least four patterns are known for Sega
   systems.

Finally, the CRC computing algorithm was improved so the tests run a bit faster.

=============================

2025/06/16

Version 0.21

This version adds support for the Emulicious/BGB debug console, which is 
enabled by default as it is harmless when not implemented.
