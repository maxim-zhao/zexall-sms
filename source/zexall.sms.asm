.define FastTestsFirst 1 ; if 1 then tests are ordered with the fastest to complete first
.define UseSDSCDebugConsole 0 ; if 1 then output is printed to SDSC Debug console instead of SMS VDP.
.define DocumentedOnly 1 ; if 0 then undocumented flags get checked too

; zexall.asm - Z80 instruction set exerciser
; Copyright (C) 1994  Frank D. Cringle
;
; 
; 
; 13-November-2010: (FluBBa)
; + Fixed compilation when UseSDSCDebugConsole is set to 1
; + Added correct CRCs for testing ALL flags (including the undocumented ones).
;
; 03-May-2007: (Rene S. Dare)
; + Added two moving slashes to animate the screen when Zexall is under internal processing. :)
;
; 09-February-2006: (Eric R. Quinn)
; + Fixed tests alu8x, cpd1, ld8ix1, id8ix2, ld8ix3, st8ix1, st8ix2, and st8ix3, that
; all accessed addresses outside of the MachineStateBeforeTest region.  This caused
; a state leak across the load and store tests.  Also, it should be noted that
; the test coverage of these tests is lacking.  These tests do not test that
; negative displacements work correctly.  There doesn't seem to be an easy way
; to test negative displacements (given the count/shift method used) without
; creating separate tests.
; + Fixed VDP initialization to be compatible with SMS1 VDP (315-5124)
; + Changed USE_SDSC_DEBUG_CONSOLE to UseSDSCDebugConsole to better match existing
; code style
;
; 11-June-2003: (Eric R. Quinn)
; + Modified to use SDSC Debug Console (instead of SMS VDP) for output
; + Define USE_SDSC_DEBUG_CONSOLE to 1 to use that console, otherwise uses VDP
;
; 18-Mar-2003: Tweaked a bit (Maxim)
; + Moved screen right by 8 to make it show on a real TV better
; + Added proper Pause button handler
; + Source clean-ups
;
; 13-Mar-2003: Modified to assemble with WLA DX (Maxim)
; + Banking system setup
; + 0nnh and 0nnnnh to $nn and $nnnn
; + db to .db
; + xxx equ yyy to .define xxx yyy
; + Macros redefined
; + Fixed some things that were wrong for the SMS side
;
; 28-Dec-2002: Ported to the Sega Master System (Brett K):
; + moved test code into RAM
; + moved various data locations into RAM
; + added SMS initialization routines, a character set, etc.
; + wrote print character routine to emulate console.
;
; 03-Nov-2002: Modified to assemble with ZMAC and MAXAM
; Copyright (C) 2002 J.G.Harston
; + labels on equates mustn't have trailing colon
; + macros don't understand <...> sequence, so parameters are passed
;   explicitly
; + ds n, c not supported, so strings are set to full explicity length
; + nonstandard 'cp a, ' and 'and a, ' changed to 'cp ' and 'and '
;
; 03-Nov-2002: Modified to run on Spectrum
; Copyright (C) 2002 J.G.Harston
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful, 
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;==============================================================
; WLA-DX banking setup
;==============================================================
.memorymap
defaultslot 0
slotsize $4000
slot 0 $0000  ; ROM
slot 1 $4000  ; ROM (not used)
slot 2 $8000  ; ROM (not used)
slot 3 $c000  ; RAM
.endme

.rombankmap
bankstotal 1
banksize $4000
banks 1
.endro

; Structs

.struct MachineState
  memop dw ; This is always the target of any memory operation
  iy    dw
  ix    dw
  hl    dw
  de    dw
  bc    dw
  f     db
  a     db
  sp    dw
.endst

.struct TestCase
  Opcode1       db
  Opcode2       db
  Opcode3       db
  Opcode4       db
  MachineState  instanceof MachineState
.endst

.struct Permuter
  Bit     db
  Byte    dw
  Buffer  dsw 20
.endst

.ramsection "Variables" slot 3
  Counter                 instanceof Permuter
  Shifter                 instanceof Permuter
  StackPointerSaved       dw      ; Saved sp
  CRCValue                dsb 4   ; CRC value
  padding                 dsb 4
  MachineStateAfterTest   instanceof MachineState
  ; CRCs are dependent on the location of this so it needs to stay at $c070.
  ; If moved, some tests require the low bit to be 0 to avoid state leaks.
  MachineStateBeforeTest  instanceof MachineState
  PauseFlag               db
  Test                    dsb 100+1 ; WLA DX doesn't (?) have a way to make this auto-sized
;  CaseCounter             dsb 4
.ends

.if DocumentedOnly == 1
.define FlagMask %11010111  ; Mask for flag register
.else
.define FlagMask %11111111  ; Mask for flag register
.endif
;                 SZXHXPNC
;                 |||||||`- Carry
;                 ||||||`-- Add/subtract
;                 |||||`--- Parity/overflow
;                 ||||`---- Undocumented
;                 |||`----- Half carry
;                 ||`------ Undocumented
;                 |`------- Zero
;                 `-------- Sign
; TODO: many test cases permute the flags, we should use this for them
; (but it will affect the test counts and CRCs)

.bank 0 Slot 0

.org $0000
.section "Boot section" Force
  di
  im 1
  ld sp, $dff0
  jp Start
  
.ends

.org $0066
.section "Pause handler" Force
  ; Note that the code uses stack operations to save and restore macine state, so there is a chance
  ; that pausing will corrupt the test state, causing a failure.
  push af
    ld a, (PauseFlag)
    xor 1             ; toggle flag
    ld (PauseFlag), a
  -:ld a, (PauseFlag)
    cp 0            ; Loop if non-zero
    jr nz, -
  pop af
  retn
.ends

/*
For the purposes of this test program, the machine state consists of:
* a 2 byte memory operand, followed by
* the registers iy, ix, hl, de, bc, af, sp
for a total of 16 bytes.

The program tests instructions (or groups of similar instructions)
by cycling through a sequence of machine states, executing the test
instruction for each one and running a 32-bit crc over the resulting
machine states.  At the end of the sequence the crc is compared to
an expected value that was found empirically on a real Z80.

A test case is defined by a descriptor which consists of:
* the base case, 
* the increment vector, 
* the shift vector, 
* the expected crc, 
* a short descriptive message.

The first three parts of the descriptor are vectors corresponding to 
a 4 byte instruction (padded with nops) and a multi-byte machine state.
* The first part is the base case, which is the first test case of
  the sequence.  This base is then modified according to the next two
  vectors.
* Each 1 bit in the increment vector specifies a bit to be
  cycled in the form of a binary counter.  For instance, if the byte
  corresponding to the accumulator is set to $ff in the increment
  vector, the test will be repeated for all 256 values of the
  accumulator.  Note that 1 bits don't have to be contiguous.  The
  number of test cases 'caused' by the increment vector is equal to
  2^(number of 1 bits).
* The shift vector is similar, but specifies a set of bits in the test 
  case that are to be successively inverted. Thus the shift vector 
  'causes' a number of test cases equal to the number of 1 bits in it,
  plus 1 (with all bits at their initial values).

The total number of test cases is the product of those caused by the
counter and shift vectors and can easily become unweildy.  Each
individual test case can take a few milliseconds to execute, due to
the overhead of test setup and crc calculation, so test design is a
compromise between coverage and execution time.

This program is designed to detect differences between
implementations and is not ideal for diagnosing the causes of any
discrepancies.  However, provided a reference implementation (or
real system) is available, a failing test case can be isolated by
hand using a binary search of the test space.
*/

.section "Start" free
Start:
  ; Initialisation
  call SMSInitialise
  ld de, Message_Title
  call OutputText

  ; Run tests, stop when first word of test data is 0000
  ld hl, Tests
-:ld a, (hl)
  inc hl
  or (hl)
  jp z, +  ; finish if 0000
  dec hl
  call StartTest   ; otherwise do test
  jp -

+:ld de, Message_Done
  call OutputText

-:jp -  ; Infinite loop to stop program
.ends

.section "Test table" free
; Lookup table of test data
Tests:
.if FastTestsFirst == 1/*
.dw ld162, ld163, ld166, ld167, ld161, ld164, ld16ix, ld8imx, ld8bd, lda, ldd1
.dw ldd2, ldi1, ldi2, ld165, ld168, ld16im, ld8im, stabd, st8ix3, ld8ix3, rotxy
.dw srzx, ld8ix2, st8ix2, ld8ixy, ld8ix1, incbc, incde, inchl, incix, inciy, incsp
.dw st8ix1, bitx, ld8rr, inca, incb, incc, incd, ince, inch, incl, incm, */.dw incxh
.dw incxl, incyh, incyl, rotz80, ld8rrx, srz80, incx, rot8080, rldop, cpd1, cpi1
.dw negop, add16, add16x, add16y, bitz80, daaop, adc16, alu8i, alu8x, alu8rx, alu8r
.else
.dw adc16, add16, add16x, add16y, alu8i, alu8r, alu8rx, alu8x, bitx, bitz80, cpd1
.dw cpi1, daaop, inca, incb, incc, incbc, incd, incde, ince, inch, inchl, incix
.dw inciy, incl, incm, incsp, incx, incxh, incxl, incyh, incyl, ld161, ld162
.dw ld163, ld164, ld165, ld166, ld167, ld168, ld16im, ld16ix, ld8bd, ld8im
.dw ld8imx, ld8ix1, ld8ix2, ld8ix3, ld8ixy, ld8rr, ld8rrx, lda, ldd1, ldd2, ldi1
.dw ldi2, negop, rldop, rot8080, rotxy, rotz80, srz80, srzx, st8ix1, st8ix2
.dw st8ix3, stabd
.endif
.dw 0
.ends

.section "Test cases" free
; Macros:
; TestData for defining test data
.macro TestData
.if NARGS != 13
  .printt "missing parameter\n"
  .fail
.endif
.dstruct Test\@ instanceof TestCase data \1, \2, \3, \4&$ff, \5, \6, \7, \8, \9, \10, \11, \12&$ff, \13
/*
.db \1, \2, \3, \4          ; Instruction opcodes
.dw \5, \6, \7, \8, \9, \10 ; memop, iy, ix, hl, de, bc
.db \11                     ; flags
.db \12                     ; a
.dw \13                     ; sp
*/
.endm
.macro TestData3
; 3-byte opcode
.if NARGS != 12
  .printt "missing parameter\n"
  .fail
.endif
  TestData \1, \2, \3, 0, \4, \5, \6, \7, \8, \9, \10, \11, \12
.endm
.macro TestData2
; 2-byte opcode
.if NARGS != 11
  .printt "missing parameter\n"
  .fail
.endif
  TestData \1, \2, 0, 0, \3, \4, \5, \6, \7, \8, \9, \10, \11
.endm
.macro TestData1
; 1-byte opcode
.if NARGS != 10
  .printt "missing parameter\n"
  .fail
.endif
  TestData \1, 0, 0, 0, \2, \3, \4, \5, \6, \7, \8, \9, \10
.endm
.macro TestData3_16
; 3-byte opcode with 16-bit arg in last two bytes
.if NARGS != 11
  .printt "missing parameter\n"
  .fail
.endif
  TestData \1, <\2, >\2, 0, \3, \4, \5, \6, \7, \8, \9, \10, \11
.endm
.macro TestData4_16
; 4-byte opcode with 16-bit arg in last two bytes
.if NARGS != 12
  .printt "missing parameter\n"
  .fail
.endif
  TestData \1, \2, <\3, >\3, \4, \5, \6, \7, \8, \9, \10, \11, \12
.endm
; Strings with control characters
.define STREND 0
.define NEWLINE 13
.asciitable
  map ' ' to '~' = 32 ; Our font mostly covers 7-bit "ASCII"
.enda
.macro MessageString
  .asc \1, STREND
  ; No message length checking :P
.endm

.macro CRC
  .db (\1>>24)&$ff, (\1>>16)&$ff, (\1>>8)&$ff, \1&$ff
.endm

.macro CRCs
.if DocumentedOnly == 1
  CRC \1
.else
  CRC \2
.endif
.endm

; <adc|sbc> hl, <bc|de|hl|sp> (72704 cases)
; Opcode: $ed %01sso010
; o = 0 for sbc, 1 for adc
; ss = bc|de|hl|sp
adc16:
  ;        <-- opcode ---> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $ed, %01000010, $832c, $4f88, $f22b, $b339, $7e1f, $1563, $d3, $89, $465e
  TestData2   0, %00111000,     0,     0,     0, $f821,     0,     0,   0,   0,     0 ; 10 bits -> 1024 permutations
  TestData2   0,         0,     0,     0,     0, $ffff, $ffff, $ffff, $d7,   0, $ffff ; 70 bits ->   71 permutations
  CRCs $f39089a0 $d48ad519
  MessageString "<adc|sbc> hl, <bc|de|hl|sp>.."

; add hl, <bc|de|hl|sp> (36352 cases)
; Opcode: $00ss1001
; ss = bc|de|hl|sp
add16:
  ;        <-opcode -> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 %00001001, $c4a5, $c4c7, $d226, $a050, $58ea, $8566, $c6, $de, $9bc9
  TestData1 %00110000,     0,     0,     0, $f821,     0,     0,   0,   0,     0 ;  9 bits -> 512 permutations
  TestData1         0,     0,     0,     0, $ffff, $ffff, $ffff, $d7,   0, $ffff ; 70 bits ->  71 permutations
  CRCs $1165fc90 $d9a4ca05
  MessageString "add hl, <bc|de|hl|sp>........"

; add ix, <bc|de|ix|sp> (36352 cases)
; Opcode: $dd $00ss1001
; ss = bc|de|hl|sp
add16x:
  ;        <-- opcode ---> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $dd, %00001001, $ddac, $c294, $635b, $33d3, $6a76, $fa20, $94, $68, $36f5
  TestData2   0, %00110000,     0,     0, $f821,     0,     0,     0,   0,   0,     0 ;  9 bits -> 512 permutations
  TestData2   0,         0,     0,     0, $ffff,     0, $ffff, $ffff, $d7,   0, $ffff ; 70 bits ->  71 permutations
  CRCs $c359f7a2 $b1df8ec0
  MessageString "add ix, <bc,de,ix,sp>........"

; add iy, <bc|de|iy|sp> (36352 cases)
; Opcode: $fd $00ss1001
; ss = bc|de|hl|sp
add16y:
  ;        <-- opcode ---> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $fd, %00001001, $c7c2, $f407, $51c1, $3e96, $0bf4, $510f, $92, $1e, $71ea
  TestData2   0, %00110000,     0, $f821,     0,     0,     0,     0,   0,   0,     0 ;  9 bits -> 512 permutations
  TestData2   0,         0,     0, $ffff,     0,     0, $ffff, $ffff, $d7,   0, $ffff ; 70 bits ->  71 permutations
  CRCs $5fc828e9 $39c8589b
  MessageString "add iy, <bc,de,iy,sp>........"

; aluop a, nn (196608 cases)
; Opcodes:
; %11[000]110 $nn add a, n
; %11[001]110 $nn adc a, n
; %11[010]110 $nn sub n
; %11[011]110 $nn sbc a, n
; %11[100]110 $nn and n
; %11[101]110 $nn xor n
; %11[110]110 $nn or n
; %11[111]110 $nn cp n
alu8i:
  ;         <---opcode---> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 %11000110,   0, $9140, $7e3c, $7a67, $df6d, $5b61, $0b29, $10, $66, $85b2
  TestData2         0, $ff,     0,     0,     0,     0,     0,     0, $d7,   0,     0  ; 14 bits -> 16384 permutations
  TestData2 %00111000,   0,     0,     0,     0,     0,     0,     0,   0, $ff,     0  ; 11 bits ->    12 permutations
  CRCs $48799360 $51c19c2e
  MessageString "aluop a, nn.................."

; aluop a, <b|c|d|e|h|l|(hl)|a> (884736 cases)
; Opcodes:
; %10[000]sss add a, sss
; %10[001]sss adc a, sss
; %10[010]sss sub sss
; %10[011]sss sbc sss
; %10[100]sss and sss
; %10[101]sss xor sss
; %10[110]sss or sss
; %10[111]sss cp sss
; sss = b|c|d|e|h|l|(hl)|a
alu8r:
  ;         <-opcode-> <memop>  <iy>   <ix>                         <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 %10000000, $c53e, $573a, $4c4d, MachineStateBeforeTest.memop, $e309, $a666, $d0, $3b, $adbb
  TestData1 %00111111,     0,     0,     0,                            0,     0,     0,   0, $ff,     0 ; 14 bits -> 16384 permutations
  TestData1         0, $ffff,     0,     0,                            0, $ffff, $ffff, $d7,   0,     0 ; 11 bits ->    11 permutations
  CRCs $5ddf949b $1ec10a46
  MessageString "aluop a,<b|c|d|e|h|l|(hl)|a>."

; aluop a, <ixh|ixl|iyh|iyl> (385024 cases)
; Opcodes:
; <$dd|$fd> %10[000]10[x] add a, i[xy][hl]
; <$dd|$fd> %10[001]10[x] adc a, i[xy][hl]
; <$dd|$fd> %10[010]10[x] sub i[xy][hl]
; <$dd|$fd> %10[011]10[x] sbc i[xy][hl]
; <$dd|$fd> %10[100]10[x] and i[xy][hl]
; <$dd|$fd> %10[101]10[x] xor i[xy][hl]
; <$dd|$fd> %10[110]10[x] or i[xy][hl]
; <$dd|$fd> %10[111]10[x] cp i[xy][hl]
alu8rx:
  ;         <---opcode---> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $dd, %10000100, $d6f7, $c76e, $accf, $2847, $22dd, $c035, $c5, $38, $234b
  TestData2 $20, %00111001,     0,     0,     0,     0,     0,     0,   0, $ff,     0 ; 13 bits -> 8192 permutations
  TestData2   0,         0, $00ff,     0,     0,     0, $ffff, $ffff, $d7,   0,     0 ; 46 bits ->   47 permutations
  CRCs $a4026d5a $a886cc44   ; ^^------------------------^^^^---^^^^- why bother? Why not cycle bits in iy/ix?
  MessageString "aluop a, <ixh|ixl|iyh|iyl>..."

; aluop a, (<ix|iy>+1) (245760 cases)
; Opcodes:
; <$dd|$fd> %10[000]110 +1 add a, (i[xy]+1
; <$dd|$fd> %10[001]110 +1 adc a, (i[xy]+1
; <$dd|$fd> %10[010]110 +1 sub i[xy][hl]
; <$dd|$fd> %10[011]110 +1 sbc i[xy][hl]
; <$dd|$fd> %10[100]110 +1 and i[xy][hl]
; <$dd|$fd> %10[101]110 +1 xor i[xy][hl]
; <$dd|$fd> %10[110]110 +1 or i[xy][hl]
; <$dd|$fd> %10[111]110 +1 cp i[xy][hl]
; ix and iy point at either byte of memop
; but the test only flips bits in the low byte
alu8x:
  ;         <-----opcode-----> <memop>                         <iy>                          <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3 $dd, %10000110, +1, $90b7, MachineStateBeforeTest.memop, MachineStateBeforeTest.memop, $32fd, $406e, $c1dc, $45, $6e, $e5fa
  TestData3 $20, %00111000,  0,     0,                            1,                            1,     0,     0,     0,   0, $ff,     0 ; 14 bits -> 16384 permutations
  TestData3   0,         0,  0, $00ff,                            0,                            0,     0,     0,     0, $d7,   0,     0 ; 14 bits ->    15 permutations
  CRCs $2bc2d52d $e334341a
  MessageString "aluop a, (<ix|iy>+1)........."

; bit n, (<ix|iy>+1) (2304 cases)
; Opcode:
; <$dd|$fd> $cb $oo %01nnn110
; ix and iy point to one byte before memop, and we cycle the bits in the following byte
; broken? TODO: compare
bitx:
  ;        <-------opcode-------> <memop>                           <iy>                            <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData $dd, $cb, 1, %01000110, $2075, MachineStateBeforeTest.memop-1, MachineStateBeforeTest.memop-1, $3cfc, $a79a, $3d74, $51, $27, $ca14
  TestData $20,   0, 0, %00111000,     0,                              0,                              0,     0,     0,     0, $53,   0,     0 ; 8 bits -> 256 permutations
  TestData   0,   0, 0,         0, $00ff,                              0,                              0,     0,     0,     0,   0,   0,     0 ; 8 bits ->   9 permutations
  CRCs $55c9ea76 $55c9ea76
  MessageString "bit n, (<ix|iy>+1)..........."

; bit n, <b|c|d|e|h|l|(hl)|a> (50176 cases)
; Opcode:
; $cb %010nnnss
; nnn = bit number
; ss = b|c|d|e|h|l|(hl)|a
bitz80:
  ;         <---opcode---> <memop>  <iy>   <ix>                          <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $cb, %01000000, $3ef1, $9dfc, $7acc, MachineStateBeforeTest.memop, $be61, $7a86, $50, $24, $1998
  TestData2   0, %00111111,     0,     0,     0,                            0,     0,     0, $53,   0,     0 ; 10 bits -> 1024 permutations
  TestData2   0,         0, $00ff,     0,     0,                            0, $ffff, $ffff,   0, $ff,     0 ; 48 bits ->   49 permutations
  CRCs $4b37451d $a937a161
  MessageString "bit n, <b|c|d|e|h|l|(hl)|a>.."

; cpd<r> (1) (14336 cases)
; Opcodes:
; $ed $a9 cpd   = cp (hl); dec hl; dec bc
; $ed $b9 cpdr  = cpd until bc = 0
; hl points to the end of the machine state
; bc is set to 1, 3, 9, 11 - originally due to a porting error, originally only 1 and 9 were tested
; a and f are permuted
cpd1:
  ;         <opcode> <memop>  <iy>   <ix>                       <hl>   <de> <bc> <f>  <a>   <sp>
  TestData2 $ed, $a9, $c7b6, $72b4, $18f6, MachineStateBeforeTest.sp, $8dbd,  1, $c0, $30, $94a3
  TestData2   0, $10,     0,     0,     0,                         0,     0, 10,   0, $ff,     0 ; 11 bits -> 2048 permutations
  TestData2   0,   0,     0,     0,     0,                         0,     0,  0, $d7,   0,     0 ;  6 bits ->    7 permutations
  CRCs $6b7eb6bf $4366d8a5
  MessageString "cpd<r>......................."

; cpi<r> (1) (14336 cases)
; Opcodes:
; $ed $a1 cpi   = cp (hl); inc hl; dec bc
; $ed $b1 cpir  = cpd until bc = 0
; hl points to the end of the machine state
; bc is set to 1, 3, 9, 11 - due to a porting error, originally only 1 and 9 were tested
; a and f are permuted
cpi1:
  ;         <opcode> <memop>  <iy>   <ix>                          <hl>   <de> <bc> <f>  <a>   <sp>
  TestData2 $ed, $a1, $4d48, $af4a, $906b, MachineStateBeforeTest.memop, $4e71,  1, $93, $6a, $907c
  TestData2 0,   $10,     0,     0,     0,                            0,     0, 10,   0, $ff,     0 ; 11 bits -> 2048 permutations
  TestData2 0,     0,     0,     0,     0,                            0,     0,  0, $d7,   0,     0 ;  6 bits ->    7 permutations
  CRCs $74baf310 $f52c5c23
  MessageString "cpi<r>......................."

; <daa|cpl|scf|ccf> (65536 cases)
; Opcodes:
; $27 daa
; $2f cpl
; $37 scf
; $3f ccf
; Test coverage is a bit weird: daa depends on af, cpl only on a, scf/ccf only on carry bit.
; But the count is pretty low so who cares?
daaop:
  ;     <opcode> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $27, $2141, $09fa, $1d60, $a559, $8d5b, $9079, $04, $8e, $299d
  TestData1 $18,     0,     0,     0,     0,     0,     0, $d7, $ff,     0 ; 16 bits -> 65536 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  0 bits ->     1 permutation
  CRCs $9b4ba675 $6d2dd213
  MessageString "<daa|cpl|scf|ccf>............"

; <inc|dec> a (3584 cases)
; Opcodes:
; $3c inc a
; $3d dec a
; All values in a are tested, with each flag bit tested in isolation.
inca: 
  ;     <opcode> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $3c, $4adf, $d5d8, $e598, $8a2b, $a7b0, $431b, $44, $5a, $d030
  TestData1 $01,     0,     0,     0,     0,     0,     0,   0, $ff,     0 ; 9 bits -> 512 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $d18815a4 $81fa8100 
  MessageString "<inc|dec> a.................."

; <inc|dec> b (3584 cases)
; Opcodes:
; $04 inc b
; $05 dec b
; All values in b are tested, with each flag bit tested in isolation.
incb:
  ;     <opcode> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $04, $d623, $432d, $7a61, $8180, $5a86, $1e85, $86, $58, $9bbb
  TestData1 $01,     0,     0,     0,     0,     0, $ff00,   0,   0,     0 ; 9 bits -> 512 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $5f682264 $77f35a73 
  MessageString "<inc|dec> b.................."

; <inc|dec> bc (1792 cases)
; Opcodes:
; $03 inc bc
; $0b dec bc
; Only 256 values in bc are tested, with each flag bit tested in isolation.
incbc:
  ;     <opcode> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $03, $cd97, $44ab, $8dc9, $e3e3, $11cc, $e8a4, $02, $49, $2a4d
  TestData1 $08,     0,     0,     0,     0,     0, $f821,   0,   0,     0 ; 8 bits -> 256 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $d2ae3bec $d2ae3bec 
  MessageString "<inc|dec> bc................."

; <inc|dec> c (3584 cases)
; Opcodes:
; $0c inc c
; $0d dec c
; All values in c are tested, with each flag bit tested in isolation.
incc:
  ;     <opcode> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $0c, $d789, $0935, $055b, $9f85, $8b27, $d208, $95, $05, $0660
  TestData1 $01,     0,     0,     0,     0,     0, $00ff,   0,   0,     0 ; 9 bits -> 512 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $c284554c $1af612a7 
  MessageString "<inc|dec> c.................."

; <inc|dec> d (3584 cases)
; Opcodes:
; $14 inc d
; $15 dec d
; All values in d are tested, with each flag bit tested in isolation.
incd: 
  ;     <opcode> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $14, $a0ea, $5fba, $65fb, $981c, $38cc, $debc, $43, $5c, $03bd
  TestData1 $01,     0,     0,     0,     0, $ff00,     0,   0,   0,     0 ; 9 bits -> 512 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $4523de10 $d146bf51 
  MessageString "<inc|dec> d.................."

; <inc|dec> de (1792 cases)
; Opcodes:
; $13 inc de
; $1b dec de
; Only 256 values in de are tested, with each flag bit tested in isolation.
incde:
  TestData1 $13, $342e, $131d, $28c9, $0aca, $9967, $3a2e, $92, $f6, $9d54
  TestData1 $08,     0,     0,     0,     0, $f821,     0,   0,   0,     0 ; 8 bits -> 256 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $aec6d42c $aec6d42c
  MessageString "<inc|dec> de................."

; <inc|dec> e (3584 cases)
; Opcodes:
; $1c inc e
; $1d dec e
; All values in e are tested, with each flag bit tested in isolation.
ince:
  ;     <opcode> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $1c, $602f, $4c0d, $2402, $e2f5, $a0f4, $a10a, $13, $32, $5925
  TestData1 $01,     0,     0,     0,     0, $00ff,     0,   0,   0,     0 ; 9 bits -> 512 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $e175afcc $ca8c6ac2 
  MessageString "<inc|dec> e.................."

; <inc|dec> h (3584 cases)
; Opcodes:
; $24 inc h
; $25 dec h
; All values in h are tested, with each flag bit tested in isolation.
inch:
  ;     <opcode> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $24, $1506, $f2eb, $e8dd, $262b, $11a6, $bc1a, $17, $06, $2818
  TestData1 $01,     0,     0,     0, $ff00,     0,     0,   0,   0,     0 ; 9 bits -> 512 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $1ced847d $560f955e 
  MessageString "<inc|dec> h.................."

; <inc|dec> hl (1792 cases)
; Opcodes:
; $23 inc hl
; $2b dec hl
; Only 256 values in hl are tested, with each flag bit tested in isolation.
inchl:
  TestData1 $23, $c3f4, $07a5, $1b6d, $4f04, $e2c2, $822a, $57, $e0, $c3e1
  TestData1 $08,     0,     0,     0, $f821,     0,     0,   0,   0,     0 ; 8 bits -> 256 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $fc0d6d4a $fc0d6d4a
  MessageString "<inc|dec> hl................."

; <inc|dec> ix (1792 cases)
; Opcodes:
; $dd $23 inc ix
; $dd $2b dec ix
; Only 256 values in ix are tested, with each flag bit tested in isolation.
incix:
  ;         <opcode> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $dd, $23, $bc3c, $0d9b, $e081, $adfd, $9a7f, $96e5, $13, $85, $0be2
  TestData2   0, $08,     0,     0, $f821,     0,     0,     0,   0,   0,     0 ; 8 bits -> 256 permutations
  TestData2   0,   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $a54dbe31 $a54dbe31 
  MessageString "<inc|dec> ix................."

; <inc|dec> iy (1792 cases)
; Opcodes:
; $fd $23 inc iy
; $fd $2b dec iy
; Only 256 values in iy are tested, with each flag bit tested in isolation.
inciy: 
  ;         <opcode> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $fd, $23, $9402, $637a, $3182, $c65a, $b2e9, $abb4, $16, $f2, $6d05
  TestData2   0, $08,     0, $f821,     0,     0,     0,     0,   0,   0,     0 ; 8 bits -> 256 permutations
  TestData2   0,   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $505d51a3 $505d51a3 
  MessageString "<inc|dec> iy................."

; <inc|dec> l (3584 cases)
; Opcodes:
; $2c inc l
; $2d dec l
; All values in l are tested, with each flag bit tested in isolation.
incl:
  ;     <opcode> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $2c, $8031, $a520, $4356, $b409, $f4c1, $dfa2, $d1, $3c, $3ea2
  TestData1 $01,     0,     0,     0, $00ff,     0,     0,   0,   0,     0 ; 9 bits -> 512 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $56cd06f3 $a0a1b49f 
  MessageString "<inc|dec> l.................."

; <inc|dec> (hl) (3584 cases)
; Opcodes:
; $34 inc (hl)
; $35 dec (hl)
; All values in (hl) are tested, with each flag bit tested in isolation.
incm:
  ;     <opcode> <memop> <iy>   <ix>                          <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $34, $b856, $0c7c, $e53e, MachineStateBeforeTest.memop, $877e, $da58, $15, $5c, $1f37
  TestData1 $01, $00ff,     0,     0,                            0,     0,     0,   0,   0,     0 ; 9 bits -> 512 permutations
  TestData1   0,     0,     0,     0,                            0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $46761d6b $d6659f4a 
  MessageString "<inc|dec> (hl)..............."

; <inc|dec> sp (1792 cases)
; Opcodes:
; $33 inc sp
; $3b dec sp
; Only 256 values in sp are tested, with each flag bit tested in isolation.
incsp: 
  ;     <opcode> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $33, $346f, $d482, $d169, $deb6, $a494, $f476, $53, $02, $855b
  TestData1 $08,     0,     0,     0,     0,     0,     0,   0,   0, $f821 ; 8 bits -> 256 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $5dacd527 $5dacd527 
  MessageString "<inc|dec> sp................."

; <inc|dec> (<ix|iy>+1) (7138 cases)
incx: 
; Opcodes:
; <$dd|$fd> $34 1 inc (i[xy]+1)
; <$dd|$fd> $35 1 dec (i[xy]+1)
; ix+1 and iy+1 point at the low byte of memop
  ;         <--opcode--> <memop>                          <iy>                            <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3 $dd, $34, 1, $fa6e, MachineStateBeforeTest.memop-1, MachineStateBeforeTest.memop-1, $2c28, $8894, $5057, $16, $33, $286f
  TestData3 $20, $01, 0, $00ff,                              0,                              0,     0,     0,     0,   0,   0,     0 ; 10 bits -> 1024 permutations
  TestData3   0,   0, 0,     0,                              0,                              0,     0,     0,     0, $d7,   0,     0 ;  6 bits ->    7 permutations
  CRCs $8897c715 $a35a7b8f 
  MessageString "<inc|dec> (<ix|iy>+1)........"

; <inc|dec> ixh (3584 cases)
; Opcodes:
; $dd $24 inc ixh (undocumented)
; $dd $25 dec ixh (undocumented)
; All values in ixh are tested, with each flag bit tested in isolation.
incxh: 
  ;         <opcode> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $dd, $24, $b838, $316c, $c6d4, $3e01, $8358, $15b4, $81, $de, $4259
  TestData2   0, $01,     0,     0, $ff00,     0,     0,     0,   0,   0,     0 ; 9 bits -> 512 permutations
  TestData2   0,   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $cfc8b622 $cfc8b622 
  MessageString "<inc|dec> ixh................"

; <inc|dec> ixl (3584 cases)
; Opcodes:
; $dd $2c inc ixl (undocumented)
; $dd $2d dec ixl (undocumented)
; All values in ixl are tested, with each flag bit tested in isolation.
incxl:
  ;         <opcode> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $dd, $2c, $4d14, $7460, $76d4, $06e7, $32a2, $213c, $d6, $d7, $99a5
  TestData2   0, $01,     0,     0, $00ff,     0,     0,     0,   0,   0,     0 ; 9 bits -> 512 permutations
  TestData2   0,   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $bb96e4c1 $bb96e4c1 
  MessageString "<inc|dec> ixl................"

; <inc|dec> iyh (3584 cases)
; Opcodes:
; $fd $24 inc iyh (undocumented)
; $fd $25 dec iyh (undocumented)
; All values in iyh are tested, with each flag bit tested in isolation.
incyh: 
  ;         <opcode> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $fd, $24, $2836, $9f6f, $9116, $61b9, $82cb, $e219, $92, $73, $a98c
  TestData2   0, $01,     0, $ff00,     0,     0,     0,     0,   0,   0,     0 ; 9 bits -> 512 permutations
  TestData2   0,   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $580724ce $580724ce 
  MessageString "<inc|dec> iyh................"

; <inc|dec> iyl (3584 cases)
; Opcodes:
; $fd $2c inc iyl (undocumented)
; $fd $2d dec iyl (undocumented)
; All values in iyl are tested, with each flag bit tested in isolation.
incyl: 
  ;         <opcode> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $fd, $2c, $d7c6, $62d5, $a09e, $7039, $3e7e, $9f12, $90, $d9, $220f
  TestData2   0, $01,     0, $00ff,     0,     0,     0,     0,   0,   0,     0 ; 9 bits -> 512 permutations
  TestData2   0,   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ; 6 bits ->   7 permutations
  CRCs $29b50d35 $29b50d35 
  MessageString "<inc|dec> iyl................"

; ld <bc|de>, (nnnn) (34 cases)
; Opcodes:
; $ed $4b $nnnn ld bc, (nnnn)
; $ed $5b $nnnn ld hl, (nnnn)
; TODO: could exercise undocumented $ed $6b ld hl, (nnnn)
ld161: 
  ;            <-------------- opcode --------------> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData4_16 $ed, $4b, MachineStateBeforeTest.memop, $f9a8, $f559, $93a4, $f5ed, $6f96, $d968, $86, $e6, $4bd8
  TestData4_16   0, $10,                            0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData4_16   0,   0,                            0, $ffff,     0,     0,     0,     0,     0,   0,   0,     0 ; 16 bits -> 17 permutations
  CRCs $4d45a9ac $4d45a9ac
  MessageString "ld <bc|de>, (nnnn)..........."

; ld hl, (nnnn) (17 cases)
; Opcode: $2a $nnnn
ld162: 
  ;            <------------ opcode -----------> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3_16 $2a, MachineStateBeforeTest.memop, $9863, $7830, $2077, $b1fe, $b9fa, $abb8, $04, $06, $6015
  TestData3_16   0,                            0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  0 bits ->  1 permutation
  TestData3_16   0,                            0, $ffff,     0,     0,     0,     0,     0,   0,   0,     0 ; 16 bits -> 17 permutations
  CRCs $5f972487 $5f972487
  MessageString "ld hl, (nnnn)................"

; ld sp, (nnnn) (17 cases)
; Opcode: $ed $7b $nnnn
ld163: 
  ;            <-------------- opcode --------------> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData4_16 $ed, $7b, MachineStateBeforeTest.memop, $8dfc, $57d7, $2161, $ca18, $c185, $27da, $83, $1e, $f460
  TestData4_16   0,   0,                            0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  0 bits ->  1 permutation
  TestData4_16   0,   0,                            0, $ffff,     0,     0,     0,     0,     0,   0,   0,     0 ; 16 bits -> 17 permutations
  CRCs $7acea11b $7acea11b 
  MessageString "ld sp, (nnnn)................"

; ld <ix|iy>, (nnnn) (34 cases)
; Opcode: <$dd|$fd> $2a $nnnn
ld164: 
  ;            <-------------- opcode --------------> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData4_16 $dd, $2a, MachineStateBeforeTest.memop, $ded7, $a6fa, $f780, $244c, $87de, $bcc2, $16, $63, $4c96
  TestData4_16 $20,   0,                            0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData4_16   0,   0,                            0, $ffff,     0,     0,     0,     0,     0,   0,   0,     0 ; 16 bits -> 17 permutations
  CRCs $858bf16d $858bf16d 
  MessageString "ld <ix|iy>, (nnnn)..........."

; ld (nnnn), <bc|de> (66 cases)
; Opcodes:
; $ed $43 $nnnn ld (nnnn), bc
; $ed $53 $nnnn ld (nnnn), de
ld165:
  ;            <-------------- opcode --------------> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData4_16 $ed, $43, MachineStateBeforeTest.memop, $1f98, $844d, $e8ac, $c9ed, $c95d, $8f61, $80, $3f, $c7bf
  TestData4_16   0, $10,                            0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData4_16   0,   0,                            0,     0,     0,     0,     0, $ffff, $ffff,   0,   0,     0 ; 32 bits -> 33 permutations
  CRCs $641e8715 $641e8715 
  MessageString "ld (nnnn), <bc|de>..........."

; ld (nnnn), hl (17 cases)
; Opcode: $22 $nnnn
ld166: 
  ;            <------------ opcode -----------> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3_16 $22, MachineStateBeforeTest.memop, $d003, $7772, $7f53, $3f72, $64ea, $e180, $10, $2d, $35e9
  TestData3_16   0,                            0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  0 bits ->  1 permutation
  TestData3_16   0,                            0,     0,     0,     0, $ffff,     0,     0,   0,   0,     0 ; 16 bits -> 17 permutations
  CRCs $a3608b47 $a3608b47 
  MessageString "ld (nnnn), hl................"

; ld (nnnn), sp (17 cases)
; Opcode: $ed $73 $nnnn
ld167: 
  ;            <-------------- opcode --------------> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData4_16 $ed, $73, MachineStateBeforeTest.memop, $c0dc, $d1d6, $ed5a, $f356, $afda, $6ca7, $44, $9f, $3f0a
  TestData4_16   0,   0,                            0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  0 bits ->  1 permutation
  TestData4_16   0,   0,                            0,     0,     0,     0,     0,     0,     0,   0,   0, $ffff ; 16 bits -> 17 permutations
  CRCs $16585fd7 $16585fd7 
  MessageString "ld (nnnn), sp................"

; ld (nnnn), <ix|iy> (66 cases)
; Opcode: <$dd|$fd> $22 $nnnn
ld168: 
  ;            <-------------- opcode --------------> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData4_16 $dd, $22, MachineStateBeforeTest.memop, $6cc3, $0d91, $6900, $8ef8, $e3d6, $c3f7, $c6, $d9, $c2df
  TestData4_16 $20,   0,                            0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData4_16   0,   0,                            0,     0, $ffff, $ffff,     0,     0,     0,   0,   0,     0 ; 32 bits -> 33 permutations
  CRCs $ba102a6b $ba102a6b 
  MessageString "ld (nnnn), <ix|iy>..........."

; ld <bc|de|hl|sp>, nnnn (68 cases)
; Opcode: %00ss0001 $nnnn ld ss, nnnn
; ss = bc|de|hl|sp
ld16im: 
  ;            <--- opcode ---> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3_16 %00000001,     0, $5c1c, $2d46, $8eb9, $6078, $74b1, $b30e, $46, $d1, $30cc
  TestData3_16 %00110000,     0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  2 bits ->  4 permutations
  TestData3_16         0, $ffff,     0,     0,     0,     0,     0,     0,   0,   0,     0 ; 16 bits -> 17 permutations
  CRCs $de391969 $de391969 
  MessageString "ld <bc|de|hl|sp>, nnnn......."

; ld <ix|iy>, nnnn (34 cases)
; Opcode: <$dd|$fd> $21 $nnnn
ld16ix: 
  ;            <--- opcode --> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData4_16 $dd, $21,     0, $87e8, $2006, $bd12, $b69b, $7253, $a1e5, $51, $13, $f1bd
  TestData4_16 $20,   0,     0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData4_16   0,   0, $ffff,     0,     0,     0,     0,     0,     0,   0,   0,     0 ; 16 bits -> 17 permutations
  CRCs $227dd525 $227dd525 
  MessageString "ld <ix|iy>, nnnn............."

; ld a, <(bc)|(de)> (46 cases)
; Opcodes:
; $0a ld a, (bc)
; $1a ld a, (de)
ld8bd: 
  ;     <opcode> <memop> <iy>   <ix>   <hl>                          <de>                          <bc>  <f>  <a>   <sp>
  TestData1 $0a, $b3a8, $1d2a, $7f8e, $42ac, MachineStateBeforeTest.memop, MachineStateBeforeTest.memop, $c6, $b1, $ef8e
  TestData1 $10,     0,     0,     0,     0,                            0,                            0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData1   0, $00ff,     0,     0,     0,                            0,                            0, $d7, $ff,     0 ; 22 bits -> 23 permutations
  CRCs $2439f60d $2439f60d 
  MessageString "ld a, <(bc)|(de)>............"

; ld <b|c|d|e|h|l|(hl)|a>, nn (72 cases)
; Opcode: %00sss110 $nn
; sss = b|c|d|e|h|l|(hl)|a
ld8im:
  ;         <opcode> <memop>  <iy>   <ix>                          <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $06,   0, $c407, $f49d, $d13d, MachineStateBeforeTest.memop, $de89, $7455, $53, $c0, $5509
  TestData2 $38,   0,     0,     0,     0,                            0,     0,     0,   0,   0,     0 ; 3 bits -> 8 permutations
  TestData2   0, $ff,     0,     0,     0,                            0,     0,     0,   0,   0,     0 ; 8 bits -> 9 permutations
  CRCs $df535f2a $df535f2a
  MessageString "ld <b|c|d|e|h|l|(hl)|a>, nn.."

; ld (<ix|iy>+1), nn (34 cases)
; Opcode: <$dd|$fd> $36 $oo $nn
; TODO: remove cases changing a
ld8imx: 
  ;        <----opcode----> <memop>                           <iy>                            <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData $dd, $36, 1, $00, $1b45, MachineStateBeforeTest.memop-1, MachineStateBeforeTest.memop-1, $d5c1, $61c7, $bdc4, $c0, $85, $cd16
  TestData $20,   0, 0,   0,     0,                              0,                              0,     0,     0,     0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData   0,   0, 0, $ff,     0,                              0,                              0,     0,     0,     0,   0, $ff,     0 ; 16 bits -> 17 permutations
  CRCs $0b6fd95c $0b6fd95c 
  MessageString "ld (<ix|iy>+1), nn..........."

; ld <b|c|d|e>, (<ix|iy>+1) (1088 cases)
; Opcode: <$dd|$fd> %010ss110 $oo
; ss = b|c|d|e
; Tests offsets 0 and 1 and also moves ix and iy on by 1 so it reads from iyl sometimes
ld8ix1: 
  ;         <-----opcode----> <memop>                         <iy>                          <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3 $dd, %01000110, 0, $d016, MachineStateBeforeTest.memop, MachineStateBeforeTest.memop, $4260, $7f39, $0404, $97, $4a, $d085
  TestData3 $20, %00011000, 1,     0,                            1,                            1,     0,     0,     0,   0,   0,     0 ;  6 bits -> 64 permutations
  TestData3   0,         0, 0, $ffff,                            0,                            0,     0,     0,     0,   0,   0,     0 ; 16 bits -> 17 permutations
  CRCs $6db05c44 $6db05c44
  MessageString "ld <b|c|d|e>, (<ix|iy>+1)...."

; ld <h|l>, (<ix|iy>+1) (544 cases)
; Opcode: <$dd|$fd> %011s110 $oo
; s = h|l
; Tests offsets 0 and 1 and also moves ix and iy on by 1 so it reads from iyl sometimes
ld8ix2: 
  ;         <--opcode--> <memop>                        <iy>                          <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3 $dd, $66, 0, $84e0, MachineStateBeforeTest.memop, MachineStateBeforeTest.memop, $9c52, $a799, $49b6, $93, $00, $eead
  TestData3 $20, $08, 1,     0,                            1,                            1,     0,     0,     0,   0,   0,     0 ;  5 bits -> 32 permutations
  TestData3   0,   0, 0, $ffff,                            0,                            0,     0,     0,     0,   0,   0,     0 ; 16 bits -> 17 permutations
  CRCs $3e094165 $3e094165
  MessageString "ld <h|l>, (<ix|iy>+1)........"

; ld a, (<ix|iy>+1) (272 cases)
; Opcode: <$dd|$fd> $7e $oo
; Tests offsets 0 and 1 and also moves ix and iy on by 1 so it reads from iyl sometimes
ld8ix3:
  ;         <--opcode--> <memop>                        <iy>                          <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3 $dd, $7e, 0, $d8b6, MachineStateBeforeTest.memop, MachineStateBeforeTest.memop, $c612, $df07, $9cd0, $43, $a6, $a0e5
  TestData3 $20,   0, 1,     0,                            1,                            1,     0,     0,     0,   0,   0,     0 ;  4 bits -> 16 permutations
  TestData3   0,   0, 0, $ffff,                            0,                            0,     0,     0,     0,   0,   0,     0 ; 16 bits -> 17 permutations
  CRCs $5407eb38 $5407eb38
  MessageString "ld a, (<ix|iy>+1)............"

; ld <ixh|ixl|iyh|iyl>, nn (1024 cases)
; Opcodes:
; <$dd|$fd> $26 $nn ld i[xy]h, n
; <$dd|$fd> $2e $nn ld i[xy]l, n
ld8ixy:
  ;        <---opcode---> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3 $dd, $26, $00, $3c53, $4640, $e179, $7711, $c107, $1afa, $81, $ad, $5d9b
  TestData3 $20, $08, $ff,     0,     0,     0,     0,     0,     0,   0,   0,     0  ; 10 bits -> 1024 permutations
  TestData3   0,   0,   0,     0,     0,     0,     0,     0,     0,   0,   0,     0  ;  0 bits ->    1 permutation
  CRCs $567e302a $567e302a 
  MessageString "ld <ixh|ixl|iyh|iyl>, nn....."

; ld <b|c|d|e|h|l|a>, <b|c|d|e|h|l|a> (3520 cases)
; Opcode: %01sssttt ld s, t
; sss = b|c|d|e|h|l|(hl)|a
; ttt = b|c|d|e|h|l|(hl)|a
ld8rr: 
  ;         <-opcode> <memop>  <iy>   <ix>                          <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 %01000000, $72a4, $a024, $61ac, MachineStateBeforeTest.memop, $82c7, $718f, $97, $8f, $ef8e
  TestData1 %00111111,     0,     0,     0,                            0,     0,     0,   0,   0,     0 ;  6 bits -> 64 permutations
  TestData1         0, $00ff,     0,     0,                            0, $ffff, $ffff, $d7, $ff,     0 ; 54 bits -> 55 permutations
  CRCs $5d1e1c64 $5d1e1c64 
  MessageString "ld <bcdehla>, <bcdehla>......"

; ld <b|c|d|e|ixy|a>, <b|c|d|e|ixy|a> (7040 cases)
; Opcode:
; <$dd|$fd> %01sssttt ($oo) ld s, t
; sss = b|c|d|e|i[xy]h|i[xy]l|(i[xy]+o)|a
; ttt = b|c|d|e|i[xy]h|i[xy]l|(i[xy]+o)|a
; Index offset parameter is used by some opcodes, so it's left at 0
; so it's a nop for the rest
; Non-offsetting ones are undocumented
ld8rrx: 
  ;         <--opcode--> <memop>                        <iy>                          <ix>                          <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3 $dd, $40, 0, $bcc5, MachineStateBeforeTest.memop, MachineStateBeforeTest.memop, MachineStateBeforeTest.memop, $2fc2, $98c0, $83, $1f, $3bcd
  TestData3 $20, $3f, 0,     0,                            0,                            0,                            0,     0,     0,   0,   0,     0 ;  7 bits -> 128 permutations
  TestData3   0,   0, 0, $00ff,                            0,                            0,                            0, $ffff, $ffff, $d7, $ff,     0 ; 54 bits ->  55 permutations
  CRCs $4c9e4b7b $4c9e4b7b 
  MessageString "ld <bcdexya>, <bcdexya>......"

; ld a, (nnnn) / ld (nnnn), a (46 cases)
; Opcodes:
; $32 $nnnn ld a, (nnnn)
; $3a $nnnn ld (nnnn), a
lda: 
  ;            <------------ opcode -----------> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3_16 $32, MachineStateBeforeTest.memop, $fd68, $f4ec, $44a0, $b543, $0653, $cdba, $d2, $4f, $1fd8
  TestData3_16 $08,                            0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData3_16   0,                            0, $00ff,     0,     0,     0,     0,     0, $d7, $ff,     0 ; 22 bits -> 23 permutations
  CRCs $c9262de5 $c9262de5 
  MessageString "ld a, (nnnn) / ld (nnnn), a.."

; ldd<r> (1) (46 cases)
; Opcodes:
; $ed $a8 ldd
; $ed $b8 lddr
; Copies a byte within the machine state from iyh to memop high, in both cases (bc = 1)
; TODO: permutes unaccessed memop?
ldd1: 
  ;         <opcode> <memop>  <iy>   <ix>                      <hl>                      <de> <bc> <f> <a>   <sp>
  TestData2 $ed, $a8, $9852, $68fa, $66a1, MachineStateBeforeTest+3, MachineStateBeforeTest+1, 1, $c1, $68, $20b7
  TestData2   0, $10,     0,     0,     0,                        0,                        0, 0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData2   0,   0, $ffff,     0,     0,                        0,                        0, 0, $d7,   0,     0 ; 22 bits -> 23 permutations
  CRCs $f82148b7 $f82148b7 
  MessageString "ldd<r> (1)..................."

; ldd<r> (2) (46 cases)
; Opcodes:
; $ed $a8 ldd
; $ed $b8 lddr
; Copies a byte or two within the machine state from iyh to memop high (bc = 2)
; TODO: permutes unaccessed memop?
ldd2: 
  ;         <opcode> <memop>  <iy>   <ix>                      <hl>                      <de> <bc> <f> <a>   <sp>
  TestData2 $ed, $a8, $f12e, $eb2a, $d5ba, MachineStateBeforeTest+3, MachineStateBeforeTest+1, 2, $47, $ff, $fbe4
  TestData2   0, $10,     0,     0,     0,                        0,                        0, 0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData2   0,   0, $ffff,     0,     0,                        0,                        0, 0, $d7,   0,     0 ; 22 bits -> 23 permutations
  CRCs $e22ab30f $8167f03a 
  MessageString "ldd<r> (2)..................."

; ldi<r> (1) (46 cases)
; Opcodes:
; $ed $a0 ldi
; $ed $b0 ldir
; Copies a byte within the machine state from iyl to memop low (bc = 1)
; TODO: permutes unaccessed memop?
ldi1: 
  ;         <opcode> <memop>  <iy>   <ix>                       <hl>                          <de> <bc> <f> <a>   <sp>
  TestData2 $ed, $a0, $fe30, $03cd, $0006, MachineStateBeforeTest.iy, MachineStateBeforeTest.memop, 1, $04, $60, $2688
  TestData2   0, $10,     0,     0,     0,                         0,                            0, 0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData2   0,   0, $ffff,     0,     0,                         0,                            0, 0, $d7,   0,     0 ; 22 bits -> 23 permutations
  CRCs $470098d4 $2a3fdeb0 
  MessageString "ldi<r> (1)..................."

; ldi<r> (2) (46 cases)
; Opcodes:
; $ed $a0 ldi
; $ed $b0 ldir
; Copies a byte or two within the machine state from iyl to memop low (bc = 2)
; TODO: permutes unaccessed memop?
ldi2:
  ;         <opcode> <memop>  <iy>   <ix>                       <hl>                          <de> <bc> <f> <a>   <sp>
  TestData2 $ed, $a0, $4ace, $c26e, $b188, MachineStateBeforeTest.iy, MachineStateBeforeTest.memop, 2, $14, $2d, $a39f
  TestData2   0, $10,     0,     0,     0,                         0,                            0, 0,   0,   0,     0 ;  1 bit  ->  2 permutations
  TestData2   0,   0, $ffff,     0,     0,                         0,                            0, 0, $d7,   0,     0 ; 22 bits -> 23 permutations
  CRCs $382fa523 $3a9cfc96 
  MessageString "ldi<r> (2)..................."

; neg (16384 cases)
; Opcode: $ed $44
; Tests all values of a and flags
negop:
  ;         <opcode> <memop>  <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $ed, $44, $38a2, $5f6b, $d934, $57e4, $d2d6, $4642, $43, $5a, $09cc
  TestData2   0,   0,     0,     0,     0,     0,     0,     0, $d7, $ff,     0 ; 14 bits -> 16384 permutations
  TestData2   0,   0,     0,     0,     0,     0,     0,     0,   0,   0,     0 ;  0 bits ->     1 permutations
  CRCs $6a3c3bbd $d638dd6a 
  MessageString "neg.........................."

; <rld|rrd> (7680 cases)
; Opcodes:
; $ed $67 rrd
; $ed $6f rld
rldop:
  ;         <opcode> <memop>  <iy>   <ix>                          <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $ed, $67, $91cb, $c48b, $fa62, MachineStateBeforeTest.memop, $e720, $b479, $40, $06, $8ae2
  TestData2   0, $08, $00ff,     0,     0,                            0,     0,     0,   0,   0,     0 ;  9 bits -> 512 permutations
  TestData2   0,   0,     0,     0,     0,                            0,     0,     0, $d7, $ff,     0 ; 14 bits ->  15 permutations
  CRCs $f7da9257 $9d030f06 
  MessageString "<rrd|rld>...................."

; <rlca|rrca|rla|rra> (7168 cases)
; Opcodes:
; $07 rlca
; $0f rrcs
; $17 rla
; $1f rra
rot8080:
  ;     <opcode> <memop> <iy>   <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData1 $07, $cb92, $6d43, $0a90, $c284, $0c53, $f50e, $91, $eb, $40fc
  TestData1 $18,     0,     0,     0,     0,     0,     0,   0, $ff,     0 ; 10 bits -> 1024 permutations
  TestData1   0,     0,     0,     0,     0,     0,     0, $d7,   0,     0 ;  6 bits ->    7 permutations
  CRCs $251330ae $9ba3807c 
  MessageString "<rlca|rrca|rla|rra>.........."

; shift/rotate (<ix|iy>+1) (448 cases)
; Opcodes:
; <$dd|$fd> $cb $oo $06 rlc (i[xy]+o)
; <$dd|$fd> $cb $oo $0e rrc (i[xy]+o)
; <$dd|$fd> $cb $oo $16 rl (i[xy]+o)
; <$dd|$fd> $cb $oo $1e rr (i[xy]+o)
; <$dd|$fd> $cb $oo $26 sla (i[xy]+o)
; <$dd|$fd> $cb $oo $2e sra (i[xy]+o)
; <$dd|$fd> $cb $oo $36 sll (i[xy]+o) (undocumented)
; <$dd|$fd> $cb $oo $3e srl (i[xy]+o)
; ix+1 and iy+1 point to memop low
rotxy:
  ;        <----opcode----> <memop>                           <iy>                            <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData $dd, $cb, 1, $06, $ddaf, MachineStateBeforeTest.memop-1, MachineStateBeforeTest.memop-1, $ff3c, $dbf6, $94f4, $82, $80, $61d9
  TestData $20,   0, 0, $38,     0,                              0,                              0,     0,     0,     0, $80,   0,     0 ;  5 bits -> 32 permutations
  TestData   0,   0, 0,   0, $00ff,                              0,                              0,     0,     0,     0, $57,   0,     0 ; 13 bits -> 14 permutations
  CRCs $b40e85cb $b4347c81 
  MessageString "shf/rot (<ix|iy>+1).........."

; shift/rotate <b|c|d|e|h|l|(hl)|a> (6912 cases)
; Opcodes:
; $cb %00000sss rlc sss
; $cb %00001sss rrc sss
; $cb %00010sss rl sss
; $cb %00011sss rr sss
; $cb %00100sss sla sss
; $cb %00101sss sra sss
; $cb %00110sss sll sss
; $cb %00111sss srl sss
; sss = b|c|d|e|h|l|(hl)|a
rotz80:
  ;         <---opcode---> <memop>  <iy>   <ix>                          <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $cb, %00000000, $cceb, $5d4a, $e007, MachineStateBeforeTest.memop, $1395, $30ee, $43, $78, $3dad
  TestData2  0,  %00111111,     0,     0,     0,                            0,     0,     0, $80,   0,     0 ;  7 bits -> 128 permutations
  TestData2  0,          0, $00ff,     0,     0,                            0, $ffff, $ffff, $57, $ff,     0 ; 53 bits ->  54 permutations
  CRCs $ee0c828b $150c42ed 
  MessageString "shf/rot <b|c|d|e|h|l|(hl)|a>."

; <set|res> n, <b|c|d|e|h|l|(hl)|a> (7040 cases)
; Opcodes:
; $cb %10sssnnn res n, sss
; $cb %11sssnnn set n, sss
; sss = b|c|d|e|h|l|(hl)|a
srz80:
  ;         <---opcode---> <memop>  <iy>   <ix>                          <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData2 $cb, %10000000, $2cd5, $97ab, $39ff, MachineStateBeforeTest.memop, $d14b, $6ab2, $53, $27, $b538
  TestData2   0, %01111111,     0,     0,     0,                            0,     0,     0,   0,   0,     0 ;  7 bits -> 128 permutations
  TestData2   0,         0, $00ff,     0,     0,                            0, $ffff, $ffff, $d7, $ff,     0 ; 54 bits ->  55 permutations
  CRCs $90aa19cd $90aa19cd 
  MessageString "<set|res> n, <bcdehl(hl)a>..."

; <set|res> n, (<ix|iy>+1) (480 cases)
; Opcodes:
; <$dd|$fd> $cb $oo %10nnn110 res n, (i[xy]+o)
; <$dd|$fd> $cb $oo %11nnn110 set n, (i[xy]+o)
srzx:
  ;        <--------opcode-------> <memop>                           <iy>                            <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData $dd, $cb, +1, %10000110, $fb44, MachineStateBeforeTest.memop-1, MachineStateBeforeTest.memop-1, $ba09, $68be, $32d8, $10, $5e, $a867
  TestData $20,   0,  0, %01111000,     0,                              0,                              0,     0,     0,     0,   0,   0,     0 ;  5 bits -> 32 permutations
  TestData   0,   0,  0,         0, $00ff,                              0,                              0,     0,     0,     0, $d7,   0,     0 ; 14 bits -> 15 permutations
  CRCs $177e3cb8 $177e3cb8 
  MessageString "<set|res> n, (<ix|iy>+1)....."

; ld (<ix|iy>+1), <b|c|d|e> (2112 cases)
; Opcode: <$dd|$fd> %011100ss $oo
; ss = b|c|d|e
; i[xy] point to memop or memop ^ 1, offsets 0 and 1 are exercised
; TODO: there is a state leak if MachineStateBeforeTest is odd
st8ix1:
  ;         <------opcode----> <memop>                         <iy>                          <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3 $dd, %01110000,  0, $270d, MachineStateBeforeTest.memop, MachineStateBeforeTest.memop, $b73a, $887b, $99ee, $86, $70, $ca07
  TestData3 $20, %00000011, +1,     0,                            1,                            1,     0,     0,     0,   0,   0,     0 ;  6 bits -> 64 permutations
  TestData3   0,         0,  0,     0,                            0,                            0,     0, $ffff, $ffff,   0,   0,     0 ; 32 bits -> 33 permutations
  CRCs $24c66b95 $24c66b95
  MessageString "ld (<ix|iy>+1), <b|c|d|e>...."

; ld (<ix|iy>+1), <h|l> (544 cases)
; Opcode: <$dd|$fd> %0111010s $oo
; s = h|l
; i[xy] point to memop or memop ^ 1, offsets 0 and 1 are exercised
; TODO: there is a state leak if MachineStateBeforeTest is odd
st8ix2: 
  ;         <------opcode----> <memop>                         <iy>                          <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3 $dd, %01110100,  0, $b664, MachineStateBeforeTest.memop, MachineStateBeforeTest.memop, $e8ac, $b5f5, $aafe, $12, $10, $9566
  TestData3 $20, %00000001, +1,     0,                            1,                            1,     0,     0,     0,   0,   0,     0 ;  5 bits -> 32 permutations
  TestData3   0,         0,  0,     0,                            0,                            0, $ffff,     0,     0,   0,   0,     0 ; 16 bits -> 17 permutations
  CRCs $b9b5243c $b9b5243c
  MessageString "ld (<ix|iy>+1), <h|l>........"

; ld (<ix|iy>+1), a (144 cases)
; Opcode: <$dd|$fd> %01110111 $oo
; i[xy] point to memop or memop ^ 1, offsets 0 and 1 are exercised
; TODO: there is a state leak if MachineStateBeforeTest is odd
st8ix3:
  ;         <--opcode-->  <memop>                        <iy>                          <ix>   <hl>   <de>   <bc>  <f>  <a>   <sp>
  TestData3 $dd, $77,  0, $67af, MachineStateBeforeTest.memop, MachineStateBeforeTest.memop, $4f13, $0644, $bcd7, $50, $ac, $5faf
  TestData3 $20,   0, +1,     0,                            1,                            1,     0,     0,     0,   0,   0,     0 ; 4 bits -> 16 permutations
  TestData3   0,   0,  0,     0,                            0,                            0,     0,     0,     0,   0, $ff,     0 ; 8 bits ->  9 permutations
  CRCs $51c0f862 $51c0f862
  MessageString "ld (<ix|iy>+1), a............"

; ld (<bc|de>), a (100 cases)
; Opcodes:
; $02 ld (bc), a
; $0a ld a, (bc)
; $12 ld (de), a
; $1a ld a, (de)
stabd:
  ;     <opcode> <memop> <iy>   <ix>   <hl>                          <de>                            <bc>  <f>  <a>   <sp>
  TestData1 $02, $0c3b, $b592, $6cff, $959e, MachineStateBeforeTest.memop, MachineStateBeforeTest.memop+1, $c1, $21, $bde7
  TestData1 $18,     0,     0,     0,     0,                            0,                              0,   0,   0,     0 ;  4 bits ->  4 permutations
  TestData1   0, $ffff,     0,     0,     0,                            0,                              0,   0, $ff,     0 ; 25 bits -> 25 permutations
  CRCs $257d7a11 $257d7a11 
  MessageString "ld (<bc|de>), a.............."
.ends

.section "Test runner" free
; StartTest
; Starts test pointed to by (hl)
StartTest:
  push hl
    xor a
    ld (SlashCounter), a
    ld a, (hl)  ; get pointer to test
    inc hl
    ld h, (hl)
    ld l, a
    push hl
      ld de, 20
      add hl, de  ; point to incmask
      ld de, Counter.Buffer
      call InitMask
    pop hl
    push hl
      ld de, 20+20
      add hl, de  ; point to scanmask
      ld de, Shifter.Buffer
      call InitMask
      ld hl, Shifter.Buffer
      ld (hl), 1  ; first bit
    pop hl
    push hl
      ld de, Test+OffsetOfInstructionUnderTest  ; self-modify instruction to Test
      ld bc, 4
      ldir
      ld de, MachineStateBeforeTest  ; copy initial machine state
      ld bc, _sizeof_MachineState
      ldir
      ld de, 20+20+4 ; skip incmask, scanmask and expcrc
      add hl, de
      ex de, hl
      call OutputText  ; show Test name
      call InitialiseCRC
;      ld hl, 0
;      ld (CaseCounter), hl
;      ld (CaseCounter+2), hl

.define HALT_OPCODE $76      
      
TestLoop:
      ld a, (Test+OffsetOfInstructionUnderTest)
      cp HALT_OPCODE  ; pragmatically avoid halt intructions
      jp z, ++
      and $df ; check for prefix bytes
      cp $dd
      jp nz, +
      ld a, (Test+OffsetOfInstructionUnderTest+1)
      cp HALT_OPCODE
   +: call nz, Test    ; execute the test instruction
.if UseSDSCDebugConsole == 0
      call UpdateProgressIndicator
.endif ; UseSDSCDebugConsole == 0
  ++: call count      ; increment the Counter
      call nz, shift   ; shift the scan bit
    pop hl  ; pointer to test case
    jp z, tlp3  ; done if shift returned NZ
    ld de, 20+20+20
    add hl, de  ; point to expected crc
    call CompareCRC
    ld de, Message_OK
    jp z, _OK
    push hl  ; save pointer to crc
      ld hl, CRCValue
      ld de, Message_ActualCRC
      call OutputText
      call PrintHex32
      ld de, Message_ExpectedCRC
      call OutputText
    pop hl  ; get pointer to crc back
    call PrintHex32
    ld de, Message_NewLine
_OK:call OutputText
  pop hl
  inc hl
  inc hl
  ret   ; end of test

tlp3:
  push hl
    ld a, 1  ; initialise count and shift scanners
    ld (Counter.Bit), a
    ld (Shifter.Bit), a
    ld hl, Counter.Buffer
    ld (Counter.Byte), hl
    ld hl, Shifter.Buffer
    ld (Shifter.Byte), hl

    ld b, 4  ; bytes in iut field
  pop hl  ; pointer to Test case
  push hl
    ld de, Test + OffsetOfInstructionUnderTest
    call setup  ; setup iut
    ld b, _sizeof_MachineState
    ld de, MachineStateBeforeTest
    call setup  ; setup machine state
    jp TestLoop

; set up a field of the Test case
; b  = number of bytes
; hl = pointer to base case
; de = destination
setup:
  call subyte
  inc hl
  djnz setup
  ret

subyte:
  push bc
  push de
  push hl
    ld c, (hl)  ; get base byte
    ld de, 20
    add hl, de  ; point to incmask
    ld a, (hl)
    cp 0
    jp z, subshf
    ld b, 8  ; 8 bits
subclp:
    rrca
    push af
      ld a, 0
      call c, GetNextCounterBit ; get next counter bit if mask bit was set
      xor c  ; flip bit if counter bit was set
      rrca
      ld c, a
    pop af
    djnz subclp
    ld b, 8
subshf:
    ld de, 20
    add hl, de  ; point to shift mask
    ld a, (hl)
    cp 0
    jp z, +
    ld b, 8  ; 8 bits
  -:rrca
    push af
      ld a, 0
      call c, GetNextShifterBit ; get next shifter bit if mask bit was set
      xor c  ; flip bit if shifter bit was set
      rrca
      ld c, a
    pop af
    djnz -
+:pop hl
  pop de
    ld a, c
    ld (de), a  ; mangled byte to destination
    inc de
  pop bc
  ret

; get next counter bit in low bit of a
GetNextCounterBit:
  push bc
  push hl
    ld hl, (Counter.Byte)
    ld b, (hl)
    ld hl, Counter.Bit
    ld a, (hl)
    ld c, a
    rlca
    ld (hl), a
    cp 1
    jp nz, +
    ld hl, (Counter.Byte)
    inc hl
    ld (Counter.Byte), hl
+:  ld a, b
    and c
 pop hl
 pop bc
 ret z
 ld a, 1
 ret

; get next shifter bit in low bit of a
GetNextShifterBit:
  push bc
  push hl
    ld hl, (Shifter.Byte)
    ld b, (hl)
    ld hl, Shifter.Bit
    ld a, (hl)
    ld c, a
    rlca
    ld (hl), a
    cp 1
    jp nz, +
    ld hl, (Shifter.Byte)
    inc hl
    ld (Shifter.Byte), hl
  +:ld a, b
    and c
    pop hl
    pop bc
  ret z
  ld a, 1
  ret

; clear memory at hl, bc bytes
clrmem:

; initialise Counter or Shifter
; de = pointer to work area for Counter or Shifter
; hl = pointer to mask
InitMask:
  push de
    ex de, hl
      ld bc, 20+20
      ; clear work area
      push de
      push hl
        ld (hl), 0
        ld d, h
        ld e, l
        inc de
        dec bc
        ldir
      pop hl
      pop de
    ex de, hl
    ld b, 20  ; byte counter
    ld c, 1  ; first bit
    ld d, 0  ; bit Counter
 --:ld e, (hl)
  -:ld a, e
    and c
    jp z, +
    inc d
  +:ld a, c
    rlca
    ld c, a
    cp 1
    jp nz, -
    inc hl
    djnz --
; got number of 1-bits in mask in reg d
    ld a, d
    and $f8
    rrca
    rrca
    rrca   ; divide by 8 (get byte offset)
    ld l, a
    ld h, 0
    ld a, d
    and 7  ; bit offset
    inc a
    ld b, a
    ld a, $80
  -:rlca
    djnz -
 pop de
 add hl, de
 ld de, 20
 add hl, de
 ld (hl), a
 ret

; multi-byte counter
count:
  push bc
  push de
  push hl
    ld hl, Counter.Buffer ; 20 byte counter starts here
    ld de, 20  ; somewhere in here is the stop bit
    ex de, hl
    add hl, de
    ex de, hl
  -:inc (hl)
    ld a, (hl)
    cp 0
    jp z, ++ ; overflow to next byte
    ld b, a
    ld a, (de)
    and b  ; Test for terminal value
    jp z, +
    ld (hl), 0  ; reset to zero
+:pop bc
  pop de
  pop hl
  ret

++: inc hl
    inc de
    jp -

; multi-byte shifter
shift:
  push bc
  push de
  push hl
    ld hl, Shifter.Buffer ; 20 byte shift register starts here
    ld de, 20  ; somewhere in here is the stop bit
    ex de, hl
    add hl, de
    ex de, hl
  -:ld a, (hl)
    or a
    jp z, ++
    ld b, a
    ld a, (de)
    and b
    jp nz, +
    ld a, b
    rlca
    cp 1
    jp nz, +++
    ld (hl), 0
    inc hl
    inc de
+++:ld (hl), a
    xor a  ; set Z
+:pop hl
  pop de
  pop bc
  ret
 
++: inc hl
    inc de
    jp -
.ends

.section "Test harness" free
; Self-modifying so it should be in RAM
TestCode:
  push af
  push bc
  push de
  push hl
    ld (StackPointerSaved), sp ; save stack pointer
    ld sp, MachineStateBeforeTest.iy ; point to test-case machine state
      pop iy  ; and load all regs
      pop ix
      pop hl
      pop de
      pop bc
      pop af
    ld sp, (MachineStateBeforeTest.sp)

_InstructionUnderTest:
    .dsb 4, 0  ; max 4 byte instruction under Test, modified at runtime

    ld (MachineStateAfterTest.sp), sp ; save stack pointer
    ld sp, MachineStateAfterTest.sp
      push af  ; save other registers
      push bc
      push de
      push hl
      push ix
      push iy
    ld sp, (StackPointerSaved) ; restore stack pointer
    ld hl, (MachineStateBeforeTest.memop) ; overwrite memop with initial value
    ld (MachineStateAfterTest.memop), hl
    ld hl, MachineStateAfterTest.f ; flags after Test
    ld a, (hl)
    and FlagMask  ; mask-out irrelevant bits
    ld (hl), a
    ld b, _sizeof_MachineState
    ld de, MachineStateAfterTest
    ld hl, CRCValue
-:  ld a, (de)
    inc de
    call UpdateChecksum  ; accumulate crc of this test case
    djnz -
  pop hl
  pop de
  pop bc
  pop af
  ret
_TestCodeEnd:
.define TestCodeSize _TestCodeEnd - TestCode
.define OffsetOfInstructionUnderTest _InstructionUnderTest - TestCode
.export TestCodeSize, OffsetOfInstructionUnderTest
.ends

.section "Text display" free
; display hex
; display the big-endian 32-bit value pointed to by hl
PrintHex32:
  push af
  push bc
  push hl
    ;call WaitForVBlank
    ld b, 4
  -:push bc
      ld a, (hl)
      call +
      inc hl
    pop bc
    djnz -
  pop hl
  pop bc
  pop af
  ret

; display byte in a
+:push af
    .rept 4
    rrca
    .endr
    call +
  pop af
; fall through

; display low nibble in a
+:push bc
  push hl
    and $0f
    cp 10
    jp c, +
    add a, 'a'-'9'-1
  +:add a, '0'
    call PrintChar
  pop hl
  pop bc
  ret

OutputText:
  push af
  push bc
  push de
  push hl
    call WaitForVBlank
-:  ld a, (de)
    cp STREND
    jr z, +
    call PrintChar
    inc de
    jr -
+:pop hl
  pop de
  pop bc
  pop af
  ret

; Messages
Message_Title:
  .asc "Z80 instruction exerciser", NEWLINE
.if DocumentedOnly == 1
  .asc "Documented flags version", NEWLINE, NEWLINE, STREND
.else
  .asc "Undocumented flags version", NEWLINE, NEWLINE, STREND
.endif
Message_Done:
  .asc "Tests complete", STREND
Message_OK:
  .asc "OK", NEWLINE, STREND
Message_ActualCRC:
  .asc NEWLINE, "CRC ", STREND
Message_ExpectedCRC:
  .asc " expected ", STREND
Message_NewLine:
  .asc NEWLINE, STREND
.ends

.section "CRC code" free
; compare crc
; hl points to value to compare to CRCValue
CompareCRC:
  push bc
  push de
  push hl
    ld de, CRCValue
    ld b, 4
  -:ld a, (de)
    cp (hl)
    jp nz, +
    inc hl
    inc de
    djnz -
+:pop hl
  pop de
  pop bc
  ret

; 32-bit crc routine
; entry: a contains next byte, hl points to crc
; exit:  crc updated
UpdateChecksum:
  push af
  push bc
  push de
  push hl
  push hl
    ld de, 3
    add hl, de ; point to low byte of old crc
    xor (hl) ; xor with new byte
    ld l, a
    ld h, 0
    add hl, hl ; use result as index into table of 4 byte entries
    add hl, hl
    ex de, hl
    ld hl, CRCLookupTable
    add hl, de ; point to selected entry in CRCLookupTable
    ex de, hl
    pop hl
    ld bc, 4 ; c = byte count, b = accumulator
  -:ld a, (de)
    xor b
    ld b, (hl)
    ld (hl), a
    inc de
    inc hl
    dec c
    jp nz, -
  pop hl
  pop de
  pop bc
  pop af
  ret

InitialiseCRC:
  push af
  push bc
  push hl
    ld hl, CRCValue
    ld a, $ff
    ld b, 4
  -:ld (hl), a
    inc hl
    djnz -
  pop hl
  pop bc
  pop af
  ret

; CRC lookup table
CRCLookupTable:
  CRC $00000000
  CRC $77073096
  CRC $ee0e612c
  CRC $990951ba
  CRC $076dc419
  CRC $706af48f
  CRC $e963a535
  CRC $9e6495a3
  CRC $0edb8832
  CRC $79dcb8a4
  CRC $e0d5e91e
  CRC $97d2d988
  CRC $09b64c2b
  CRC $7eb17cbd
  CRC $e7b82d07
  CRC $90bf1d91
  CRC $1db71064
  CRC $6ab020f2
  CRC $f3b97148
  CRC $84be41de
  CRC $1adad47d
  CRC $6ddde4eb
  CRC $f4d4b551
  CRC $83d385c7
  CRC $136c9856
  CRC $646ba8c0
  CRC $fd62f97a
  CRC $8a65c9ec
  CRC $14015c4f
  CRC $63066cd9
  CRC $fa0f3d63
  CRC $8d080df5
  CRC $3b6e20c8
  CRC $4c69105e
  CRC $d56041e4
  CRC $a2677172
  CRC $3c03e4d1
  CRC $4b04d447
  CRC $d20d85fd
  CRC $a50ab56b
  CRC $35b5a8fa
  CRC $42b2986c
  CRC $dbbbc9d6
  CRC $acbcf940
  CRC $32d86ce3
  CRC $45df5c75
  CRC $dcd60dcf
  CRC $abd13d59
  CRC $26d930ac
  CRC $51de003a
  CRC $c8d75180
  CRC $bfd06116
  CRC $21b4f4b5
  CRC $56b3c423
  CRC $cfba9599
  CRC $b8bda50f
  CRC $2802b89e
  CRC $5f058808
  CRC $c60cd9b2
  CRC $b10be924
  CRC $2f6f7c87
  CRC $58684c11
  CRC $c1611dab
  CRC $b6662d3d
  CRC $76dc4190
  CRC $01db7106
  CRC $98d220bc
  CRC $efd5102a
  CRC $71b18589
  CRC $06b6b51f
  CRC $9fbfe4a5
  CRC $e8b8d433
  CRC $7807c9a2
  CRC $0f00f934
  CRC $9609a88e
  CRC $e10e9818
  CRC $7f6a0dbb
  CRC $086d3d2d
  CRC $91646c97
  CRC $e6635c01
  CRC $6b6b51f4
  CRC $1c6c6162
  CRC $856530d8
  CRC $f262004e
  CRC $6c0695ed
  CRC $1b01a57b
  CRC $8208f4c1
  CRC $f50fc457
  CRC $65b0d9c6
  CRC $12b7e950
  CRC $8bbeb8ea
  CRC $fcb9887c
  CRC $62dd1ddf
  CRC $15da2d49
  CRC $8cd37cf3
  CRC $fbd44c65
  CRC $4db26158
  CRC $3ab551ce
  CRC $a3bc0074
  CRC $d4bb30e2
  CRC $4adfa541
  CRC $3dd895d7
  CRC $a4d1c46d
  CRC $d3d6f4fb
  CRC $4369e96a
  CRC $346ed9fc
  CRC $ad678846
  CRC $da60b8d0
  CRC $44042d73
  CRC $33031de5
  CRC $aa0a4c5f
  CRC $dd0d7cc9
  CRC $5005713c
  CRC $270241aa
  CRC $be0b1010
  CRC $c90c2086
  CRC $5768b525
  CRC $206f85b3
  CRC $b966d409
  CRC $ce61e49f
  CRC $5edef90e
  CRC $29d9c998
  CRC $b0d09822
  CRC $c7d7a8b4
  CRC $59b33d17
  CRC $2eb40d81
  CRC $b7bd5c3b
  CRC $c0ba6cad
  CRC $edb88320
  CRC $9abfb3b6
  CRC $03b6e20c
  CRC $74b1d29a
  CRC $ead54739
  CRC $9dd277af
  CRC $04db2615
  CRC $73dc1683
  CRC $e3630b12
  CRC $94643b84
  CRC $0d6d6a3e
  CRC $7a6a5aa8
  CRC $e40ecf0b
  CRC $9309ff9d
  CRC $0a00ae27
  CRC $7d079eb1
  CRC $f00f9344
  CRC $8708a3d2
  CRC $1e01f268
  CRC $6906c2fe
  CRC $f762575d
  CRC $806567cb
  CRC $196c3671
  CRC $6e6b06e7
  CRC $fed41b76
  CRC $89d32be0
  CRC $10da7a5a
  CRC $67dd4acc
  CRC $f9b9df6f
  CRC $8ebeeff9
  CRC $17b7be43
  CRC $60b08ed5
  CRC $d6d6a3e8
  CRC $a1d1937e
  CRC $38d8c2c4
  CRC $4fdff252
  CRC $d1bb67f1
  CRC $a6bc5767
  CRC $3fb506dd
  CRC $48b2364b
  CRC $d80d2bda
  CRC $af0a1b4c
  CRC $36034af6
  CRC $41047a60
  CRC $df60efc3
  CRC $a867df55
  CRC $316e8eef
  CRC $4669be79
  CRC $cb61b38c
  CRC $bc66831a
  CRC $256fd2a0
  CRC $5268e236
  CRC $cc0c7795
  CRC $bb0b4703
  CRC $220216b9
  CRC $5505262f
  CRC $c5ba3bbe
  CRC $b2bd0b28
  CRC $2bb45a92
  CRC $5cb36a04
  CRC $c2d7ffa7
  CRC $b5d0cf31
  CRC $2cd99e8b
  CRC $5bdeae1d
  CRC $9b64c2b0
  CRC $ec63f226
  CRC $756aa39c
  CRC $026d930a
  CRC $9c0906a9
  CRC $eb0e363f
  CRC $72076785
  CRC $05005713
  CRC $95bf4a82
  CRC $e2b87a14
  CRC $7bb12bae
  CRC $0cb61b38
  CRC $92d28e9b
  CRC $e5d5be0d
  CRC $7cdcefb7
  CRC $0bdbdf21
  CRC $86d3d2d4
  CRC $f1d4e242
  CRC $68ddb3f8
  CRC $1fda836e
  CRC $81be16cd
  CRC $f6b9265b
  CRC $6fb077e1
  CRC $18b74777
  CRC $88085ae6
  CRC $ff0f6a70
  CRC $66063bca
  CRC $11010b5c
  CRC $8f659eff
  CRC $f862ae69
  CRC $616bffd3
  CRC $166ccf45
  CRC $a00ae278
  CRC $d70dd2ee
  CRC $4e048354
  CRC $3903b3c2
  CRC $a7672661
  CRC $d06016f7
  CRC $4969474d
  CRC $3e6e77db
  CRC $aed16a4a
  CRC $d9d65adc
  CRC $40df0b66
  CRC $37d83bf0
  CRC $a9bcae53
  CRC $debb9ec5
  CRC $47b2cf7f
  CRC $30b5ffe9
  CRC $bdbdf21c
  CRC $cabac28a
  CRC $53b39330
  CRC $24b4a3a6
  CRC $bad03605
  CRC $cdd70693
  CRC $54de5729
  CRC $23d967bf
  CRC $b3667a2e
  CRC $c4614ab8
  CRC $5d681b02
  CRC $2a6f2b94
  CRC $b40bbe37
  CRC $c30c8ea1
  CRC $5a05df1b
  CRC $2d02ef8d
.ends

.section "SMS-specific stuff"
; Copies the Test code into modifiable memory.
; This is because the code is self-modifying.
Install:
  push bc
  push de
  push hl
    ld de, Test
    ld hl, TestCode
    ld bc, TestCodeSize
    ldir
  pop hl
  pop de
  pop bc

  ret
.ends

.if UseSDSCDebugConsole == 1
.section "SDSC console" free
.include "sdsc.inc"

; (erq) Re-write SMSInitialise to remove VDP-specific actions, and
; (erq) replace with debug console initialization code.

SMSInitialise:
  ; Disable joystick ports.  This enables ports in region $C0 through $FF
  ; allowing Debug Console ports at $FC and $FD to be visible.
  ; WARNING: The following assumes ZEXALL is being run from the SMS
  ; cartridge port.
  ;
  ld a, $af
  out ($3e), a

  ; Clear Debug Console screen
  ld a, SDSC_DEBUGCONSOLE_COMMAND_CLEARSCREEN
  out (SDSC_OUTPORT_DEBUGCONSOLE_COMMAND), a

  call Install ; first, install test code into RAM

  ret


PrintChar:
  cp $0d
  jp nz, sdscprint_out
  ld a, $0a

sdscprint_out:
  out (SDSC_OUTPORT_DEBUGCONSOLE_DATA), a
  ret
.ends

.else

.ramsection "SMS drawing routines variables" slot 3
  CursorX      db ; X coordinate of cursor
  VRAMAddress  dw ; VRAM address, pre-masked as a write command
  Scroll       db ; Current scrolling offset
  ScrollFlag   db ; Zero before we start scrolling, then 1
  SlashCounter db ; Counter for rawing slashes animation
.ends

.section "Font" free
font:
.include "BBC Micro font.inc"
fontend:
.ends

.section "VDP initialisation" free

.include "VDP data.inc"

SMSInitialise:
  push af
  push bc
  push de
  push hl
    xor a
    ld (PauseFlag), a
    call SetUpVDP
    call LoadFont
    call LoadPalette
    call InitCursor
    call Install ; first, install test code into RAM

    ; Turn screen on
    SET_VDP_REGISTER 1, %11000000
;                        ||||| |`- Zoomed sprites -> 16x16 pixels
;                        ||||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;                        ||||`---- 30 row/240 line mode
;                        |||`----- 28 row/224 line mode
;                        ||`------ VBlank interrupts
;                        |`------- Enable display
;                        `-------- Must be set (VRAM size bit)

  pop hl
  pop de
  pop bc
  pop af
  ret

; Set up our VDP for display. Set the correct video mode, but the display is turned off.
SetUpVDP:
  ; Set VDP registers
  ld hl, VDPRegisterInitialisation
  ld b, VDPRegisterInitialisationEnd - VDPRegisterInitialisation
  ld c, VDP_REGISTER
  otir

  ; Clear VRAM
  SET_VRAM_ADDRESS 0
  ld hl, $8000    
  ld bc, 1
  ld a, 0
-:out (VDP_DATA), a
  sbc hl, bc
  jr nz, -

  ret

; Install our palette into the VDP
LoadPalette:
  SET_CRAM_ADDRESS 0
  ld hl, Palette
  ld b, PaletteEnd - Palette
  ld c, VDP_DATA
  otir
  ret

; Load the font
LoadFont:
  SET_VRAM_ADDRESS 0
  ld hl, font
  ld bc, fontend-font
  ld de, 1
-:ld a, (hl)
  out (VDP_DATA), a
  out (VDP_DATA), a
  out (VDP_DATA), a
  out (VDP_DATA), a
  inc hl
  dec bc
  ld a, b
  or c
  jr nz, -
  ret
.ends

.section "Console emulation" free

.define NAME_TABLE_START NameTableAddress | VRAM_WRITE_MASK
.define SCREEN_END NAME_TABLE_START + 32*23*2
.define NAME_TABLE_END NAME_TABLE_START + 32*28*2

; Initialize the cursor variables
InitCursor:
  ld hl, NAME_TABLE_START
  ld a, 0
  ld (CursorX), a
  ld (VRAMAddress), hl
  ld (Scroll), a
  ld (ScrollFlag), a
  ret

; Code to wait for start of blanking period
WaitForVBlank:
  push af
    ; Wait for status bit to be set
-:  in a, (VDP_STATUS)
    and %10000000
    jr z, -
  pop af
  ret


; Code to print slash between executions
UpdateProgressIndicator:
/*
  push hl
  push bc
  push af
    ld hl, CaseCounter+3
  -:inc (hl)
    dec hl
    jp z, -

    ld hl,CaseCounter
    call PrintHex32
  pop af
  pop bc
  pop hl
  ret
*/
  ; Increment the counter
  ld a, (SlashCounter)
  inc a
  ld (SlashCounter), a
  cp '/' ; When it's a slash, print it
  jr z, +
  cp '\'
  jr z, +
  ret

+:push af
  push bc
  push hl
    call WaitForVBlank
    ld c, VDP_ADDRESS
    ld hl, (VRAMAddress)
    out (c), l
    out (c), h

    ; Print the char twice (but don't move the cursor on)
    sub ' '       ; Shift into our font range
    ld b, 0
    ld c, VDP_DATA
    out (c), a     ; Output the character
    out (c), b
    out (c), a     ; Output the character
    out (c), b
  pop hl
  pop bc
  pop af
  ret

; Code to print a character on the screen. Does stuff like handle
; line feeds, scrolling, etc.

PrintChar:
  push de
    ; If it's a carriage return, skip it.
    cp NEWLINE
    jp z, _NextLine
    ; Write the character (in a) to the screen
    sub ' '       ; Shift into our font range
    ld b, a       ; save value for later
    ld c, VDP_ADDRESS
    ld hl, (VRAMAddress)
    out (c), l    ; Output lower-order bits
    out (c), h    ; Output upper bits + control

    ld c, VDP_DATA
    out (c), a     ; Output the character
    ld b, 0
    out (c), b

    ; Update VRAM pointer
    inc hl
    inc hl
    ld (VRAMAddress), hl

    ; Move cursor forward
    ld hl,CursorX
    inc (hl)

    ; Check if we're at the end of the line
    ld a, (CursorX)
    cp 32
    jp z, _NextLine
  pop de
  ret

  ; Here we do the job of scrolling the display, computing the
  ; new VRAM address, and all that fun stuff.
_NextLine:
    ld hl, (VRAMAddress)     ; Increase the VRAM position
    ; Get the cursor position and find out how far it was to the
    ; end of the line.
    ld a, (CursorX)
    ld b, a
    ld a, 32
    sub b
    call _WriteBlanks   ; Fill the rest of the line
    sla a               ; Now, double this and add it to HL. This is the new address.
    ld c, a
    ld b, 0
    add hl, bc          ; Now create new address.
    ccf
    push hl
      ld bc, SCREEN_END ; Check if we got to the bottom of the screen
      sbc hl, bc
      jp c, +
      ld a, 1           ; If we are, set the scroll flag on... we scroll from now on.
      ld (ScrollFlag), a
  +:pop hl
    push hl
      ld bc, NAME_TABLE_END ; Next, check if we're at the end of VRAM
      sbc hl, bc
    pop hl
    jp c, +
    ld hl, NAME_TABLE_START ; If we are, return to the top of VRAM.
  +:ld (VRAMAddress), hl ; Now, save our VRAM address.
    ld a, 32             ; Clear the new line
    call _WriteBlanks
    ld a, (ScrollFlag)      ; Load the Scroll flag and check if it's set.
    cp 0
    jp z, noScroll
    ld a, (Scroll)       ; If it is, increase the Scroll value, and wrap at 28.
    inc a
    cp 28
    jp nz, doScroll
    ld a, 0
doScroll:
    ld (Scroll), a       ; Now, write out Scroll value out to the VDP.
    sla a
    sla a
    sla a
    ld c, $bf
    out (c), a
    ld a, $89
    out (c), a
noScroll:
    ld a, 0              ; Reset the cursor X position to 0
    ld (CursorX), a
  pop de
  ret

; Fill the nametable from HL with A bytes.
_WriteBlanks:
  push af
  push bc
    ; Do nothing for count = 0
    or a
    jr z, +
    ld c, VDP_ADDRESS
    out (c), l ; Output lower-order bits
    out (c), h ; Output upper bits + control
    ; Now, zero out the region
    ; Double so we write n*2 bytes
    add a, a
    ld b, a
    xor a
    ld c, VDP_DATA
  -:out (c), a ; Output the character
    djnz -
+:pop bc
  pop af
  ret
.ends
.endif ; UseSDSCDebugConsole == 1

;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 0.16, "Z80 Intruction Exerciser", SDSCNotes, "FluBBa, Maxim, Eric R. Quinn"

.section "SDSC notes"
SDSCNotes:
.db "Based on ZEXALL by Frank Cringle, "
.db "with credit to J.G.Harston and Brett K"
.db 0
.ends
