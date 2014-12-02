.define FastTestsFirst 1    ; if 1 then tests are ordered with the fastest to complete first
.define FlagMask %11010111  ; Mask for flag register
;                 SZXHXPNC
;                 |||||||`- Carry
;                 ||||||`-- Add/subtract
;                 |||||`--- Parity/overflow
;                 ||||`---- Undocumented
;                 |||`----- Half carry
;                 ||`------ Undocumented
;                 |`------- Zero
;                 `-------- Sign



; zexall.asm - Z80 instruction set exerciser
; Copyright (C) 1994  Frank D. Cringle
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
; + ds n,c not supported, so strings are set to full explicity length
; + nonstandard 'cp a,' and 'and a,' changed to 'cp ' and 'and '
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
.MemoryMap
DEFAULTSLOT 0
SLOTSIZE $4000
SLOT 0 $0000  ; ROM
SLOT 1 $4000
SLOT 2 $8000  ; ROM (not used)
SLOT 3 $c000  ; RAM
.ENDME

.RomBankMap
BANKSTOTAL 2
BANKSIZE $4000
BANKS 2
.ENDRO

.bank 0 slot 0

.org $0000
.section "Boot section" force
  di
  im 1
  ld sp,$dff0
  jp Start
.ends

.org $0066
.section "Pause handler" force
push af
  ld a,(PauseFlag)
  xor 1             ; toggle flag
  ld (PauseFlag),a
-:ld a,(PauseFlag)
  cp 0            ; Loop if non-zero
  jr nz,-
pop af
retn
.ends

/*
.ramsection "Variables" slot 1
  CntBit   db      ; Counter and Shifter variables...
  CntByt   dw
  ShfBit   db
  ShfByt   dw
  Counter  dsw 20
  Shifter  dsw 20
  MachineStateBeforeTest     dsb 14  ; Machine state before test
  StackPointerBeforeTest     dw      ; sp before test
  MachineStateAfterTest     dsb 14  ; Machine state after test
  StackPointerAfterTest     dw      ; sp after test
  StackPointerSaved    dw      ; Saved sp
  CRCValue   dsb 4   ; CRC value
.ends
.define MachineStateBeforeTestHi  MachineStateBeforeTest >> 8 ;/ $100
.define MachineStateBeforeTestLo  MachineStateBeforeTest & $ff*/
.define CntBit  $C000
.define CntByt  $C001
.define ShfBit  $C003
.define ShfByt  $C004
.define Counter $C010
.define Shifter $C038
.define MachineStateBeforeTest    $C070
.define StackPointerBeforeTest    $C07E
.define MachineStateBeforeTestHi  MachineStateBeforeTest / $100
.define MachineStateBeforeTestLo  MachineStateBeforeTest & $ff
.define MachineStateAfterTest    $C080  ; Machine state after test
.define StackPointerAfterTest    $C08E  ; Stack pointer after test
.define StackPointerSaved   $C090  ; Saved stack pointer
.define CRCValue  $C092  ; CRC value

; SMS drawing routines variables
.define CursorX $C300
.define VRAMAdr $C304
.define Scroll  $C306
.define ScrollF $C307
.define Test    $C400

.define PauseFlag $d000

; For the purposes of this test program, the machine state consists of:
; a 2 byte memory operand, followed by
; the registers iy,ix,hl,de,bc,af,sp
; for a total of 16 bytes.

; The program tests instructions (or groups of similar instructions)
; by cycling through a sequence of machine states, executing the test
; instruction for each one and running a 32-bit crc over the resulting
; machine states.  At the end of the sequence the crc is compared to
; an expected value that was found empirically on a real Z80.

; A test case is defined by a descriptor which consists of:
; a flag mask byte,
; the base case,
; the increment vector,
; the shift vector,
; the expected crc,
; a short descriptive message.
;
; The flag mask byte is used to prevent undefined flag bits from
; influencing the results.  Documented flags are as per Mostek Z80
; Technical Manual.
;
; The next three parts of the descriptor are 20 byte vectors
; corresponding to a 4 byte instruction and a 16 byte machine state.
; The first part is the base case, which is the first test case of
; the sequence.  This base is then modified according to the next 2
; vectors.  Each 1 bit in the increment vector specifies a bit to be
; cycled in the form of a binary Counter.  For instance, if the byte
; corresponding to the accumulator is set to $ff in the increment
; vector, the test will be repeated for all 256 values of the
; accumulator.  Note that 1 bits don't have to be contiguous.  The
; number of test cases 'caused' by the increment vector is equal to
; 2^(number of 1 bits).  The shift vector is similar, but specifies a
; set of bits in the test case that are to be successively inverted.
; Thus the shift vector 'causes' a number of test cases equal to the
; number of 1 bits in it.

; The total number of test cases is the product of those caused by the
; Counter and shift vectors and can easily become unweildy.  Each
; individual test case can take a few milliseconds to execute, due to
; the overhead of test setup and crc calculation, so test design is a
; compromise between coverage and execution time.

; This program is designed to detect differences between
; implementations and is not ideal for diagnosing the causes of any
; discrepancies.  However, provided a reference implementation (or
; real system) is available, a failing test case can be isolated by
; hand using a binary search of the test space.

Start:
  ; Initialisation
  call SMSInitialise
  ld de,msg1
  ld c,9
  call OutputText

  ; Run tests, stop when first word of test data is 0000
  ld hl,Tests
-:ld a,(hl)
  inc hl
  or (hl)
  jp z,+  ; finish if 0000
  dec hl
  call StartTest   ; otherwise do test
  jp -

+:ld de,msg2   ; output end result
  ld c,9
  call OutputText

-:jp -  ; Infinite loop to stop program

; Lookup table of test data
Tests:
.if FastTestsFirst == 1
.dw ld162,ld163,ld166,ld167,ld161,ld164,ld16ix,ld8imx,ld8ixy,lda,ldd1
.dw ldd2,ldi1,ldi2,ld8bd,ld168,ld16im,ld8im,ld165,st8ix3,stabd,ld8ix3
.dw ld8ix2,st8ix2,rotxy,ld8ix1,st8ix1,incc,incde,inchl,incix,inciy
.dw incsp,srzx,bitx,inca,incb,incbc,incd,ince,inch,incl,incm,incxh
.dw incxl,incyh,incyl,ld8rr,cpd1,cpi1,incx,rot8080,rotz80,ld8rrx,rldop
.dw srz80,negop,add16,add16x,add16y,alu8i,adc16,bitz80,daaop,alu8x
.dw alu8rx,alu8r
.else
.dw adc16,add16,add16x,add16y,alu8i,alu8r,alu8rx,alu8x,bitx,bitz80,cpd1
.dw cpi1,daaop,inca,incb,incc,incbc,incd,incde,ince,inch,inchl,incix
.dw inciy,incl,incm,incsp,incx,incxh,incxl,incyh,incyl,ld161,ld162
.dw ld163,ld164,ld165,ld166,ld167,ld168,ld16im,ld16ix,ld8bd,ld8im
.dw ld8imx,ld8ix1,ld8ix2,ld8ix3,ld8ixy,ld8rr,ld8rrx,lda,ldd1,ldd2,ldi1
.dw ldi2,negop,rldop,rot8080,rotxy,rotz80,srz80,srzx,st8ix1,st8ix2
.dw st8ix3,stabd
.endif
.dw 0

; Macros:
; TestData for defining test data
.macro TestData
.if NARGS != 13
  .printt "missing parameter"
  .fail
.endif
.db \1,\2,\3,\4            ; insn1,insn2,insn3,insn4
.dw \5,\6,\7,\8,\9,\10     ; memop,riy,rix,rhl,rde,rbc
.db \11                    ; flags
.db \12                    ; acc
.dw \13                    ; rsp
.endm

; MessageString for defining string constants
.macro MessageString
  .db \1,'$'
  ; No message length checking :P
.endm

.macro CRC
  .db (\1>>24)&$ff,(\1>>16)&$ff,(\1>>8)&$ff,\1&$ff
.endm

; <adc,sbc> hl,<bc,de,hl,sp> (38,912 cycles)
adc16:
  .db FlagMask
  TestData $ed,$42,0,0,$832c,$4f88,$f22b,$b339,$7e1f,$1563,$d3,$89,$465e
  TestData 0,$38,0,0,0,0,0,$f821,0,0,0,0,0 ; (1024 cycles)
  TestData 0,0,0,0,0,0,0,-1,-1,-1,$d7,0,-1 ; (38 cycles)
  CRC $f39089a0
  MessageString "<adc,sbc> hl,<bc,de,hl,sp>..."

; add hl,<bc,de,hl,sp> (19,456 cycles)
add16:
  .db FlagMask
  TestData 9,0,0,0,$c4a5,$c4c7,$d226,$a050,$58ea,$8566,$c6,$de,$9bc9
  TestData $30,0,0,0,0,0,0,$f821,0,0,0,0,0 ; (512 cycles)
  TestData 0,0,0,0,0,0,0,-1,-1,-1,$d7,0,-1 ; (38 cycles)
  CRC $1165fc90
  MessageString "add hl,<bc,de,hl,sp>........."

; add ix,<bc,de,ix,sp> (19,456 cycles)
add16x:
  .db FlagMask
  TestData $dd,9,0,0,$ddac,$c294,$635b,$33d3,$6a76,$fa20,$94,$68,$36f5
  TestData 0,$30,0,0,0,0,$f821,0,0,0,0,0,0 ; (512 cycles)
  TestData 0,0,0,0,0,0,-1,0,-1,-1,$d7,0,-1 ; (38 cycles)
  CRC $c359f7a2
  MessageString "add ix,<bc,de,ix,sp>........."

; add iy,<bc,de,iy,sp> (19,456 cycles)
add16y:
  .db FlagMask
  TestData $fd,9,0,0,$c7c2,$f407,$51c1,$3e96,$0bf4,$510f,$92,$1e,$71ea
  TestData 0,$30,0,0,0,$f821,0,0,0,0,0,0,0 ; (512 cycles)
  TestData 0,0,0,0,0,-1,0,0,-1,-1,$d7,0,-1 ; (38 cycles)
  CRC $5fc828e9
  MessageString "add iy,<bc,de,iy,sp>........."

; aluop a,nn (28,672 cycles)
alu8i:
  .db FlagMask
  TestData $c6,0,0,0,$9140,$7e3c,$7a67,$df6d,$5b61,$0b29,$10,$66,$85b2
  TestData $38,0,0,0,0,0,0,0,0,0,0,-1,0  ; (2048 cycles)
  TestData 0,-1,0,0,0,0,0,0,0,0,$d7,0,0  ; (14 cycles)
  CRC $48799360
  MessageString "aluop a,nn..................."

; aluop a,<b,c,d,e,h,l,(hl),a> (753,664 cycles)
alu8r:
  .db FlagMask
  TestData $80,0,0,0,$c53e,$573a,$4c4d,MachineStateBeforeTest,$e309,$a666,$d0,$3b,$adbb
  TestData $3f,0,0,0,0,0,0,0,0,0,0,-1,0  ; (16,384 cycles)
  TestData 0,0,0,0,$ff,0,0,0,-1,-1,$d7,0,0 ; (46 cycles)
  CRC $5ddf949b
  MessageString "aluop a,<b,c,d,e,h,l,(hl),a>."

; aluop a,<ixh,ixl,iyh,iyl> (376,832 cycles)
alu8rx:
  .db FlagMask
  TestData $dd,$84,0,0,$d6f7,$c76e,$accf,$2847,$22dd,$c035,$c5,$38,$234b
  TestData $20,$39,0,0,0,0,0,0,0,0,0,-1,0 ; (8,192 cycles)
  TestData 0,0,0,0,$ff,0,0,0,-1,-1,$d7,0,0 ; (46 cycles)
  CRC $a4026d5a
  MessageString "aluop a,<ixh,ixl,iyh,iyl>...."

; aluop a,(<ix,iy>+1) (229,376 cycles)
alu8x:
  .db FlagMask
  TestData $dd,$86,1,0,$90b7,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$32fd,$406e,$c1dc,$45,$6e,$e5fa
  TestData $20,$38,0,0,0,1,1,0,0,0,0,-1,0 ; (16,384 cycles)
  TestData 0,0,0,0,$ff,0,0,0,0,0,$d7,0,0  ; (14 cycles)
  CRC $b823fbc7
  MessageString "aluop a,(<ix,iy>+1).........."

; bit n,(<ix,iy>+1) (2048 cycles)
bitx: 
  .db FlagMask
  TestData $dd,$cb,1,$46,$2075,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$3cfc,$a79a,$3d74,$51,$27,$ca14
  TestData $20,0,0,$38,0,0,0,0,0,0,$53,0,0 ; (256 cycles)
  TestData 0,0,0,0,$ff,0,0,0,0,0,0,0,0  ; (8 cycles)
  CRC $55c9ea76
  MessageString "bit n,(<ix,iy>+1)............"

; bit n,<b,c,d,e,h,l,(hl),a> (49,152 cycles)
bitz80:
  .db FlagMask
  TestData $cb,$40,0,0,$3ef1,$9dfc,$7acc,MachineStateBeforeTest,$be61,$7a86,$50,$24,$1998
  TestData 0,$3f,0,0,0,0,0,0,0,0,$53,0,0  ; (1024 cycles)
  TestData 0,0,0,0,$ff,0,0,0,-1,-1,0,-1,0  ; (48 cycles)
  CRC $4b37451d
  MessageString "bit n,<b,c,d,e,h,l,(hl),a>..."

; cpd<r> (1) (6144 cycles)
cpd1:
  .db FlagMask
  TestData $ed,$a9,0,0,$c7b6,$72b4,$18f6,MachineStateBeforeTest+17,$8dbd,1,$c0,$30,$94a3
  TestData 0,$10,0,0,0,0,0,0,0,010,0,-1,0  ; (1024 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $437db7ff
  MessageString "cpd<r>......................."

; cpi<r> (1) (6144 cycles)
cpi1:
  .db FlagMask
  TestData $ed,$a1,0,0,$4d48,$af4a,$906b,MachineStateBeforeTest,$4e71,1,$93,$6a,$907c
  TestData 0,$10,0,0,0,0,0,0,0,010,0,-1,0  ; (1024 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $74baf310
  MessageString "cpi<r>......................."

; <daa,cpl,scf,ccf> (65,536 cycles)
daaop:
  .db FlagMask
  TestData $27,0,0,0,$2141,$09fa,$1d60,$a559,$8d5b,$9079,$04,$8e,$299d
  TestData $18,0,0,0,0,0,0,0,0,0,$d7,-1,0 ; (65,536 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,0,0,0  ; (1 cycle)
  CRC $9b4ba675
  MessageString "<daa,cpl,scf,ccf>............"

; <inc,dec> a (3072 cycles)
inca: 
  .db FlagMask
  TestData $3c,0,0,0,$4adf,$d5d8,$e598,$8a2b,$a7b0,$431b,$44,$5a,$d030
  TestData $01,0,0,0,0,0,0,0,0,0,0,-1,0  ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $d18815a4 
  MessageString "<inc,dec> a.................."

; <inc,dec> b (3072 cycles)
incb:
  .db FlagMask
  TestData $04,0,0,0,$d623,$432d,$7a61,$8180,$5a86,$1e85,$86,$58,$9bbb
  TestData $01,0,0,0,0,0,0,0,0,$ff00,0,0,0 ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $5f682264 
  MessageString "<inc,dec> b.................."

; <inc,dec> bc (1536 cycles)
incbc:
  .db FlagMask
  TestData $03,0,0,0,$cd97,$44ab,$8dc9,$e3e3,$11cc,$e8a4,$02,$49,$2a4d
  TestData $08,0,0,0,0,0,0,0,0,$f821,0,0,0 ; (256 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $d2ae3bec 
  MessageString "<inc,dec> bc................."

; <inc,dec> c (3072 cycles)
incc:
  .db FlagMask
  TestData $0c,0,0,0,$d789,$0935,$055b,$9f85,$8b27,$d208,$95,$05,$0660
  TestData $01,0,0,0,0,0,0,0,0,$ff,0,0,0  ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $c284554c 
  MessageString "<inc,dec> c.................."

; <inc,dec> d (3072 cycles)
incd: 
  .db FlagMask
  TestData $14,0,0,0,$a0ea,$5fba,$65fb,$981c,$38cc,$debc,$43,$5c,$03bd
  TestData $01,0,0,0,0,0,0,0,$ff00,0,0,0,0 ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $4523de10 
  MessageString "<inc,dec> d.................."

; <inc,dec> de (1536 cycles)
incde:
  .db FlagMask
  TestData $13,0,0,0,$342e,$131d,$28c9,$0aca,$9967,$3a2e,$92,$f6,$9d54
  TestData $08,0,0,0,0,0,0,0,$f821,0,0,0,0 ; (256 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $aec6d42c 
  MessageString "<inc,dec> de................."

; <inc,dec> e (3072 cycles)
ince:
  .db FlagMask
  TestData $1c,0,0,0,$602f,$4c0d,$2402,$e2f5,$a0f4,$a10a,$13,$32,$5925
  TestData $01,0,0,0,0,0,0,0,$ff,0,0,0,0  ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $e175afcc 
  MessageString "<inc,dec> e.................."

; <inc,dec> h (3072 cycles)
inch:
  .db FlagMask
  TestData $24,0,0,0,$1506,$f2eb,$e8dd,$262b,$11a6,$bc1a,$17,$06,$2818
  TestData $01,0,0,0,0,0,0,$ff00,0,0,0,0,0 ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $1ced847d 
  MessageString "<inc,dec> h.................."

; <inc,dec> hl (1536 cycles)
inchl:
  .db FlagMask
  TestData $23,0,0,0,$c3f4,$07a5,$1b6d,$4f04,$e2c2,$822a,$57,$e0,$c3e1
  TestData $08,0,0,0,0,0,0,$f821,0,0,0,0,0 ; (256 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $fc0d6d4a 
  MessageString "<inc,dec> hl................."

; <inc,dec> ix (1536 cycles)
incix:
  .db FlagMask
  TestData $dd,$23,0,0,$bc3c,$0d9b,$e081,$adfd,$9a7f,$96e5,$13,$85,$0be2
  TestData 0,8,0,0,0,0,$f821,0,0,0,0,0,0  ; (256 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $a54dbe31 
  MessageString "<inc,dec> ix................."

; <inc,dec> iy (1536 cycles)
inciy: 
  .db FlagMask
  TestData $fd,$23,0,0,$9402,$637a,$3182,$c65a,$b2e9,$abb4,$16,$f2,$6d05
  TestData 0,8,0,0,0,$f821,0,0,0,0,0,0,0  ; (256 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $505d51a3 
  MessageString "<inc,dec> iy................."

; <inc,dec> l (3072 cycles)
incl:
  .db FlagMask
  TestData $2c,0,0,0,$8031,$a520,$4356,$b409,$f4c1,$dfa2,$d1,$3c,$3ea2
  TestData $01,0,0,0,0,0,0,$ff,0,0,0,0,0  ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $56cd06f3 
  MessageString "<inc,dec> l.................."

; <inc,dec> (hl) (3072 cycles)
incm:
  .db FlagMask
  TestData $34,0,0,0,$b856,$0c7c,$e53e,MachineStateBeforeTest,$877e,$da58,$15,$5c,$1f37
  TestData $01,0,0,0,$ff,0,0,0,0,0,0,0,0  ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $46761d6b 
  MessageString "<inc,dec> (hl)..............."

; <inc,dec> sp (1536 cycles)
incsp: 
  .db FlagMask
  TestData $33,0,0,0,$346f,$d482,$d169,$deb6,$a494,$f476,$53,$02,$855b
  TestData $08,0,0,0,0,0,0,0,0,0,0,0,$f821 ; (256 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $5dacd527 
  MessageString "<inc,dec> sp................."

; <inc,dec> (<ix,iy>+1) (6144 cycles)
incx: 
  .db FlagMask
  TestData $dd,$34,1,0,$fa6e,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$2c28,$8894,$5057,$16,$33,$286f
  TestData $20,1,0,0,$ff,0,0,0,0,0,0,0,0  ; (1024 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $8897c715 
  MessageString "<inc,dec> (<ix,iy>+1)........"

; <inc,dec> ixh (3072 cycles)
incxh: 
  .db FlagMask
  TestData $dd,$24,0,0,$b838,$316c,$c6d4,$3e01,$8358,$15b4,$81,$de,$4259
  TestData 0,1,0,0,0,$ff00,0,0,0,0,0,0,0  ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $6f463662 
  MessageString "<inc,dec> ixh................"

; <inc,dec> ixl (3072 cycles)
incxl:
  .db FlagMask
  TestData $dd,$2c,0,0,$4d14,$7460,$76d4,$06e7,$32a2,$213c,$d6,$d7,$99a5
  TestData 0,1,0,0,0,$ff,0,0,0,0,0,0,0  ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $027bef2c 
  MessageString "<inc,dec> ixl................"

; <inc,dec> iyh (3072 cycles)
incyh: 
  .db FlagMask
  TestData $dd,$24,0,0,$2836,$9f6f,$9116,$61b9,$82cb,$e219,$92,$73,$a98c
  TestData 0,1,0,0,$ff00,0,0,0,0,0,0,0,0  ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $2d966cf3 
  MessageString "<inc,dec> iyh................"

; <inc,dec> iyl (3072 cycles)
incyl: 
  .db FlagMask
  TestData $dd,$2c,0,0,$d7c6,$62d5,$a09e,$7039,$3e7e,$9f12,$90,$d9,$220f
  TestData 0,1,0,0,$ff,0,0,0,0,0,0,0,0  ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $fbcbba95 
  MessageString "<inc,dec> iyl................"

; ld <bc,de>,(nnnn) (32 cycles)
ld161: 
  .db FlagMask
  TestData $ed,$4b,MachineStateBeforeTestLo,MachineStateBeforeTestHi,$f9a8,$f559,$93a4,$f5ed,$6f96,$d968,$86,$e6,$4bd8
  TestData 0,$10,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycles)
  TestData 0,0,0,0,-1,0,0,0,0,0,0,0,0  ; (16 cycles)
  CRC $4d45a9ac
  MessageString "ld <bc,de>,(nnnn)............"

; ld hl,(nnnn) (16 cycles)
ld162: 
  .db FlagMask
  TestData $2a,MachineStateBeforeTestLo,MachineStateBeforeTestHi,0,$9863,$7830,$2077,$b1fe,$b9fa,$abb8,$04,$06,$6015
  TestData 0,0,0,0,0,0,0,0,0,0,0,0,0  ; (1 cycle)
  TestData 0,0,0,0,-1,0,0,0,0,0,0,0,0  ; (16 cycles)
  CRC $5f972487
  MessageString "ld hl,(nnnn)................."

; ld sp,(nnnn) (16 cycles)
ld163: 
  .db FlagMask
  TestData $ed,$7b,MachineStateBeforeTestLo,MachineStateBeforeTestHi,$8dfc,$57d7,$2161,$ca18,$c185,$27da,$83,$1e,$f460
  TestData 0,0,0,0,0,0,0,0,0,0,0,0,0  ; (1 cycles)
  TestData 0,0,0,0,-1,0,0,0,0,0,0,0,0  ; (16 cycles)
  CRC $7acea11b 
  MessageString "ld sp,(nnnn)................."

; ld <ix,iy>,(nnnn) (32 cycles)
ld164: 
  .db FlagMask
  TestData $dd,$2a,MachineStateBeforeTestLo,MachineStateBeforeTestHi,$ded7,$a6fa,$f780,$244c,$87de,$bcc2,$16,$63,$4c96
  TestData $20,0,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycles)
  TestData 0,0,0,0,-1,0,0,0,0,0,0,0,0  ; (16 cycles)
  CRC $858bf16d 
  MessageString "ld <ix,iy>,(nnnn)............"

; ld (nnnn),<bc,de> (64 cycles)
ld165: 
  .db FlagMask
  TestData $ed,$43,MachineStateBeforeTestLo,MachineStateBeforeTestHi,$1f98,$844d,$e8ac,$c9ed,$c95d,$8f61,$80,$3f,$c7bf
  TestData 0,$10,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycles)
  TestData 0,0,0,0,0,0,0,0,-1,-1,0,0,0  ; (32 cycles)
  CRC $641e8715 
  MessageString "ld (nnnn),<bc,de>............"

; ld (nnnn),hl (16 cycles)
ld166: 
  .db FlagMask
  TestData $22,MachineStateBeforeTestLo,MachineStateBeforeTestHi,0,$d003,$7772,$7f53,$3f72,$64ea,$e180,$10,$2d,$35e9
  TestData 0,0,0,0,0,0,0,0,0,0,0,0,0  ; (1 cycle)
  TestData 0,0,0,0,0,0,0,-1,0,0,0,0,0  ; (16 cycles)
  CRC $a3608b47 
  MessageString "ld (nnnn),hl................."

; ld (nnnn),sp (16 cycles)
ld167: 
  .db FlagMask
  TestData $ed,$73,MachineStateBeforeTestLo,MachineStateBeforeTestHi,$c0dc,$d1d6,$ed5a,$f356,$afda,$6ca7,$44,$9f,$3f0a
  TestData 0,0,0,0,0,0,0,0,0,0,0,0,0  ; (1 cycle)
  TestData 0,0,0,0,0,0,0,0,0,0,0,0,-1  ; (16 cycles)
  CRC $16585fd7 
  MessageString "ld (nnnn),sp................."

; ld (nnnn),<ix,iy> (64 cycles)
ld168: 
  .db FlagMask
  TestData $dd,$22,MachineStateBeforeTestLo,MachineStateBeforeTestHi,$6cc3,$0d91,$6900,$8ef8,$e3d6,$c3f7,$c6,$d9,$c2df
  TestData $20,0,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycles)
  TestData 0,0,0,0,0,-1,-1,0,0,0,0,0,0  ; (32 cycles)
  CRC $ba102a6b 
  MessageString "ld (nnnn),<ix,iy>............"

; ld <bc,de,hl,sp>,nnnn (64 cycles)
ld16im: 
  .db FlagMask
  TestData 1,0,0,0,$5c1c,$2d46,$8eb9,$6078,$74b1,$b30e,$46,$d1,$30cc
  TestData $30,0,0,0,0,0,0,0,0,0,0,0,0  ; (4 cycles)
  TestData 0,$ff,$ff,0,0,0,0,0,0,0,0,0,0  ; (16 cycles)
  CRC $de391969 
  MessageString "ld <bc,de,hl,sp>,nnnn........"

; ld <ix,iy>,nnnn (32 cycles)
ld16ix: 
  .db FlagMask
  TestData $dd,$21,0,0,$87e8,$2006,$bd12,$b69b,$7253,$a1e5,$51,$13,$f1bd
  TestData $20,0,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycles)
  TestData 0,0,$ff,$ff,0,0,0,0,0,0,0,0,0  ; (16 cycles)
  CRC $227dd525 
  MessageString "ld <ix,iy>,nnnn.............."

; ld a,<(bc),(de)> (44 cycles)
ld8bd: 
  .db FlagMask
  TestData $0a,0,0,0,$b3a8,$1d2a,$7f8e,$42ac,MachineStateBeforeTest,MachineStateBeforeTest,$c6,$b1,$ef8e
  TestData $10,0,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycles)
  TestData 0,0,0,0,$ff,0,0,0,0,0,$d7,-1,0 ; (22 cycles)
  CRC $2439f60d 
  MessageString "ld a,<(bc),(de)>............."

; ld <b,c,d,e,h,l,(hl),a>,nn (64 cycles)
ld8im:
  .db FlagMask
  TestData 6,0,0,0,$c407,$f49d,$d13d,$0339,$de89,$7455,$53,$c0,$5509
  TestData $38,0,0,0,0,0,0,0,0,0,0,0,0  ; (8 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,0,-1,0  ; (8 cycles)
  CRC $f1dab556 
  MessageString "ld <b,c,d,e,h,l,(hl),a>,nn..."

; ld (<ix,iy>+1),nn (32 cycles)
ld8imx: 
  .db FlagMask
  TestData $dd,$36,1,0,$1b45,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$d5c1,$61c7,$bdc4,$c0,$85,$cd16
  TestData $20,0,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycles)
  TestData 0,0,0,-1,0,0,0,0,0,0,0,-1,0  ; (16 cycles)
  CRC $0b6fd95c 
  MessageString "ld (<ix,iy>+1),nn............"

; ld <b,c,d,e>,(<ix,iy>+1) (512 cycles)
ld8ix1: 
  .db FlagMask
  TestData $dd,$46,1,0,$d016,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$4260,$7f39,$0404,$97,$4a,$d085
  TestData $20,$18,0,0,0,1,1,0,0,0,0,0,0  ; (32 cycles)
  TestData 0,0,0,0,-1,0,0,0,0,0,0,0,0  ; (16 cycles)
  CRC $18df759e 
  MessageString "ld <b,c,d,e>,(<ix,iy>+1)....."

; ld <h,l>,(<ix,iy>+1) (256 cycles)
ld8ix2: 
  .db FlagMask
  TestData $dd,$66,1,0,$84e0,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$9c52,$a799,$49b6,$93,$00,$eead
  TestData $20,$08,0,0,0,1,1,0,0,0,0,0,0  ; (16 cycles)
  TestData 0,0,0,0,-1,0,0,0,0,0,0,0,0  ; (16 cycles)
  CRC $0275d849 
  MessageString "ld <h,l>,(<ix,iy>+1)........."

; ld a,(<ix,iy>+1) (128 cycles)
ld8ix3:
  .db FlagMask
  TestData $dd,$7e,1,0,$d8b6,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$c612,$df07,$9cd0,$43,$a6,$a0e5
  TestData $20,0,0,0,0,1,1,0,0,0,0,0,0  ; (8 cycles)
  TestData 0,0,0,0,-1,0,0,0,0,0,0,0,0  ; (16 cycles)
  CRC $c691fb20 
  MessageString "ld a,(<ix,iy>+1)............."

; ld <ixh,ixl,iyh,iyl>,nn (32 cycles)
ld8ixy:
  .db FlagMask
  TestData $dd,$26,0,0,$3c53,$4640,$e179,$7711,$c107,$1afa,$81,$ad,$5d9b
  TestData $20,8,0,0,0,0,0,0,0,0,0,0,0  ; (4 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,0,-1,0  ; (8 cycles)
  CRC $24e8828b 
  MessageString "ld <ixh,ixl,iyh,iyl>,nn......"

; ld <b,c,d,e,h,l,a>,<b,c,d,e,h,l,a> (3456 cycles)
ld8rr: 
  .db FlagMask
  TestData $40,0,0,0,$72a4,$a024,$61ac,MachineStateBeforeTest,$82c7,$718f,$97,$8f,$ef8e
  TestData $3f,0,0,0,0,0,0,0,0,0,0,0,0  ; (64 cycles)
  TestData 0,0,0,0,$ff,0,0,0,-1,-1,$d7,-1,0 ; (54 cycles)
  CRC $5d1e1c64 
  MessageString "ld <bcdehla>,<bcdehla>......."

; ld <b,c,d,e,ixy,a>,<b,c,d,e,ixy,a> (6912 cycles)
ld8rrx: 
  .db FlagMask
  TestData $dd,$40,0,0,$bcc5,MachineStateBeforeTest,MachineStateBeforeTest,MachineStateBeforeTest,$2fc2,$98c0,$83,$1f,$3bcd
  TestData $20,$3f,0,0,0,0,0,0,0,0,0,0,0  ; (128 cycles)
  TestData 0,0,0,0,$ff,0,0,0,-1,-1,$d7,-1,0 ; (54 cycles)
  CRC $4c9e4b7b 
  MessageString "ld <bcdexya>,<bcdexya>......."

; ld a,(nnnn) / ld (nnnn),a (44 cycles)
lda: 
  .db FlagMask
  TestData $32,MachineStateBeforeTestLo,MachineStateBeforeTestHi,0,$fd68,$f4ec,$44a0,$b543,$0653,$cdba,$d2,$4f,$1fd8
  TestData $08,0,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycle)
  TestData 0,0,0,0,$ff,0,0,0,0,0,$d7,-1,0 ; (22 cycles)
  CRC $c9262de5 
  MessageString "ld a,(nnnn) / ld (nnnn),a...."

; ldd<r> (1) (44 cycles)
ldd1: 
  .db FlagMask
  TestData $ed,$a8,0,0,$9852,$68fa,$66a1,MachineStateBeforeTest+3,MachineStateBeforeTest+1,1,$c1,$68,$20b7
  TestData 0,$10,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycles)
  TestData 0,0,0,0,-1,0,0,0,0,0,$d7,0,0  ; (22 cycles)
  CRC $f82148b7 
  MessageString "ldd<r> (1)..................."

; ldd<r> (2) (44 cycles)
ldd2: 
  .db FlagMask
  TestData $ed,$a8,0,0,$f12e,$eb2a,$d5ba,MachineStateBeforeTest+3,MachineStateBeforeTest+1,2,$47,$ff,$fbe4
  TestData 0,$10,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycles)
  TestData 0,0,0,0,-1,0,0,0,0,0,$d7,0,0  ; (22 cycles)
  CRC $e22ab30f 
  MessageString "ldd<r> (2)..................."

; ldi<r> (1) (44 cycles)
ldi1: 
  .db FlagMask
  TestData $ed,$a0,0,0,$fe30,$03cd,06$58,MachineStateBeforeTest+2,MachineStateBeforeTest,1,$04,$60,$2688
  TestData 0,$10,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycles)
  TestData 0,0,0,0,-1,0,0,0,0,0,$d7,0,0  ; (22 cycles)
  CRC $470098d4 
  MessageString "ldi<r> (1)..................."

; ldi<r> (2) (44 cycles)
ldi2:
  .db FlagMask
  TestData $ed,$a0,0,0,$4ace,$c26e,$b188,MachineStateBeforeTest+2,MachineStateBeforeTest,2,$14,$2d,$a39f
  TestData 0,$10,0,0,0,0,0,0,0,0,0,0,0  ; (2 cycles)
  TestData 0,0,0,0,-1,0,0,0,0,0,$d7,0,0  ; (22 cycles)
  CRC $382fa523 
  MessageString "ldi<r> (2)..................."

; neg (16,384 cycles)
negop:
  .db FlagMask
  TestData $ed,$44,0,0,$38a2,$5f6b,$d934,$57e4,$d2d6,$4642,$43,$5a,$09cc
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,-1,0  ; (16,384 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,0,0,0  ; (1 cycle)
  CRC $6a3c3bbd 
  MessageString "neg.........................."

; <rld,rrd> (7168 cycles)
rldop:
  .db FlagMask
  TestData $ed,$67,0,0,$91cb,$c48b,$fa62,MachineStateBeforeTest,$e720,$b479,$40,$06,$8ae2
  TestData 0,8,0,0,$ff,0,0,0,0,0,0,0,0  ; (512 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,-1,0  ; (14 cycles)
  CRC $f7da9257 
  MessageString "<rrd,rld>...................."

; <rlca,rrca,rla,rra> (6144 cycles)
rot8080:
  .db FlagMask
  TestData 7,0,0,0,$cb92,$6d43,$0a90,$c284,$0c53,$f50e,$91,$eb,$40fc
  TestData $18,0,0,0,0,0,0,0,0,0,0,-1,0  ; (1024 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,$d7,0,0  ; (6 cycles)
  CRC $251330ae 
  MessageString "<rlca,rrca,rla,rra>.........."

; shift/rotate (<ix,iy>+1) (416 cycles)
rotxy:
  .db FlagMask
  TestData $dd,$cb,1,6,$ddaf,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$ff3c,$dbf6,$94f4,$82,$80,$61d9
  TestData $20,0,0,$38,0,0,0,0,0,0,$80,0,0 ; (32 cycles)
  TestData 0,0,0,0,$ff,0,0,0,0,0,$57,0,0  ; (13 cycles)
  CRC $b40e85cb 
  MessageString "shf/rot (<ix,iy>+1).........."

; shift/rotate <b,c,d,e,h,l,(hl),a> (6784 cycles)
rotz80:
  .db FlagMask
  TestData $cb,0,0,0,$cceb,$5d4a,$e007,MachineStateBeforeTest,$1395,$30ee,$43,$78,$3dad
  TestData 0,$3f,0,0,0,0,0,0,0,0,$80,0,0  ; (128 cycles)
  TestData 0,0,0,0,$ff,0,0,0,-1,-1,$57,-1,0 ; (53 cycles)
  CRC $ee0c828b 
  MessageString "shf/rot <b,c,d,e,h,l,(hl),a>."

; <set,res> n,<b,c,d,e,h,l,(hl),a> (7936 cycles)
srz80:
  .db FlagMask
  TestData $cb,$80,0,0,$2cd5,$97ab,$39ff,MachineStateBeforeTest,$d14b,$6ab2,$53,$27,$b538
  TestData 0,$7f,0,0,0,0,0,0,0,0,0,0,0  ; (128 cycles)
  TestData 0,0,0,0,$ff,0,0,0,-1,-1,$d7,-1,0 ; (62 cycles)
  CRC $90aa19cd 
  MessageString "<set,res> n,<bcdehl(hl)a>...."

; <set,res> n,(<ix,iy>+1) (1792 cycles)
srzx:
  .db FlagMask
  TestData $dd,$cb,1,$86,$fb44,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$ba09,$68be,$32d8,$10,$5e,$a867
  TestData $20,0,0,$78,0,0,0,0,0,0,0,0,0 ; (128 cycles)
  TestData 0,0,0,0,$ff,0,0,0,0,0,$d7,0,0  ;(14 cycles)
  CRC $177e3cb8 
  MessageString "<set,res> n,(<ix,iy>+1)......"

; ld (<ix,iy>+1),<b,c,d,e> (1024 cycles)
st8ix1:
  .db FlagMask
  TestData $dd,$70,1,0,$270d,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$b73a,$887b,$99ee,$86,$70,$ca07
  TestData $20,$03,0,0,0,1,1,0,0,0,0,0,0  ; (32 cycles)
  TestData 0,0,0,0,0,0,0,0,-1,-1,0,0,0  ; (32 cycles)
  CRC $0b6eeefd 
  MessageString "ld (<ix,iy>+1),<b,c,d,e>....."

; ld (<ix,iy>+1),<h,l> (256 cycles)
st8ix2: 
  .db FlagMask
  TestData $dd,$74,1,0,$b664,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$e8ac,$b5f5,$aafe,$12,$10,$9566
  TestData $20,$01,0,0,0,1,1,0,0,0,0,0,0  ; (16 cycles)
  TestData 0,0,0,0,0,0,0,-1,0,0,0,0,0  ; (32 cycles)
  CRC $58d9048b 
  MessageString "ld (<ix,iy>+1),<h,l>........."

; ld (<ix,iy>+1),a (64 cycles)
st8ix3:
  .db FlagMask
  TestData $dd,$77,1,0,$67af,MachineStateBeforeTest-1,MachineStateBeforeTest-1,$4f13,$0644,$bcd7,$50,$ac,$5faf
  TestData $20,0,0,0,0,1,1,0,0,0,0,0,0  ; (8 cycles)
  TestData 0,0,0,0,0,0,0,0,0,0,0,-1,0  ; (8 cycles)
  CRC $b6657c5e 
  MessageString "ld (<ix,iy>+1),a............."

; ld (<bc,de>),a (96 cycles)
stabd:
  .db FlagMask
  TestData 2,0,0,0,$0c3b,$b592,$6cff,$959e,MachineStateBeforeTest,MachineStateBeforeTest+1,$c1,$21,$bde7
  TestData $18,0,0,0,0,0,0,0,0,0,0,0,0  ; (4 cycles)
  TestData 0,0,0,0,-1,0,0,0,0,0,0,-1,0  ; (24 cycles)
  CRC $257d7a11 
  MessageString "ld (<bc,de>),a..............."

; StartTest
; Starts test pointed to by (hl)
StartTest:
  push hl
    ld a,(hl)  ; get pointer to test
    inc hl
    ld h,(hl)
    ld l,a
    ld a,(hl)  ; flag mask
    ld ((Test+FlgMskCode-TestCode)+1),a   ; self-modify code for flag mask
    inc hl
    push hl
      ld de,20
      add hl,de  ; point to incmask
      ld de,Counter
      call InitMask
    pop hl
    push hl
      ld de,20+20
      add hl,de  ; point to scanmask
      ld de,Shifter
      call InitMask
      ld hl,Shifter
      ld (hl),1  ; first bit
    pop hl
    push hl
      ld de,Test+IUTCode-TestCode  ; self-modify instruction to Test
      ld bc,4
      ldir
      ld de,MachineStateBeforeTest  ; copy initial machine state
      ld bc,16
      ldir
      ld de,20+20+4 ; skip incmask, scanmask and expcrc
      add hl,de
      ex de,hl
      ld c,9
      call OutputText  ; show Test name
      call InitialiseCRC

      ; Test loop
 tlp: ld a,(Test+IUTCode-TestCode)
      cp $76  ; pragmatically avoid halt intructions
      jp z,++
      and $df
      cp $dd
      jp nz,+
      ld a,(Test+IUTCode-TestCode+1)
      cp $76
   +: call nz,Test    ; execute the Test instruction
  ++: call count      ; increment the Counter
      call nz,shift   ; shift the scan bit
    pop hl  ; pointer to Test case
    jp z,tlp3  ; done if shift returned NZ
    ld de,20+20+20
    add hl,de  ; point to expected crc
    call cmpcrc
    ld de,okmsg
    jp z,tlpok
    push hl  ; save pointer to crc
      ld hl,CRCValue
      ld de,ermsg1 ; jgh: swap crc= and expected= messages
      ld c,9
      call OutputText
      call phex8
      ld de,ermsg2
      ld c,9
      call OutputText
    pop hl  ; get pointer to crc back
    call phex8
    ld de,crlf
tlpok:
    ld c,9
    call OutputText
  pop hl
  inc hl
  inc hl
  ret   ; end of Test

tlp3:
  push hl
    ld a,1  ; initialise count and shift scanners
    ld (CntBit),a
    ld (ShfBit),a
    ld hl,Counter
    ld (CntByt),hl
    ld hl,Shifter
    ld (ShfByt),hl

    ld b,4  ; bytes in iut field
  pop hl  ; pointer to Test case
  push hl
    ld de,Test+IUTCode-TestCode
    call setup  ; setup iut
    ld b,16     ; bytes in machine state
    ld de,MachineStateBeforeTest
    call setup  ; setup machine state
    jp tlp

; set up a field of the Test case
; b  = number of bytes
; hl = pointer to base case
; de = destination
setup:
  call subyte
  inc hl
  dec b
  jp nz,setup
  ret

subyte:
  push bc
  push de
  push hl
    ld c,(hl)  ; get base byte
    ld de,20
    add hl,de  ; point to incmask
    ld a,(hl)
    cp 0
    jp z,subshf
    ld b,8  ; 8 bits
subclp:
    rrca
    push af
      ld a,0
      call c,nxtcbit ; get next Counter bit if mask bit was set
      xor c  ; flip bit if Counter bit was set
      rrca
      ld c,a
    pop af
    dec b
    jp nz,subclp
    ld b,8
subshf:
    ld de,20
    add hl,de  ; point to shift mask
    ld a,(hl)
    cp 0
    jp z,+
    ld b,8  ; 8 bits
  -:rrca
    push af
      ld a,0
      call c,nxtsbit ; get next Shifter bit if mask bit was set
      xor c  ; flip bit if Shifter bit was set
      rrca
      ld c,a
    pop af
    dec b
    jp nz,-
+:pop hl
  pop de
    ld a,c
    ld (de),a  ; mangled byte to destination
    inc de
  pop bc
  ret

; get next Counter bit in low bit of a
nxtcbit:
  push bc
  push hl
    ld hl,(CntByt)
    ld b,(hl)
    ld hl,CntBit
    ld a,(hl)
    ld c,a
    rlca
    ld (hl),a
    cp 1
    jp nz,+
    ld hl,(CntByt)
    inc hl
    ld (CntByt),hl
  +:ld a,b
    and c
 pop hl
 pop bc
 ret z
 ld a,1
 ret

; get next Shifter bit in low bit of a
nxtsbit:
  push bc
  push hl
    ld hl,(ShfByt)
    ld b,(hl)
    ld hl,ShfBit
    ld a,(hl)
    ld c,a
    rlca
    ld (hl),a
    cp 1
    jp nz,+
    ld hl,(ShfByt)
    inc hl
    ld (ShfByt),hl
  +:ld a,b
    and c
    pop hl
    pop bc
  ret z
  ld a,1
  ret

; clear memory at hl, bc bytes
clrmem:
  push af
  push bc
  push de
  push hl
    ld (hl),0
    ld d,h
    ld e,l
    inc de
    dec bc
    ldir
 pop hl
 pop de
 pop bc
 pop af
 ret

; initialise Counter or Shifter
; de = pointer to work area for Counter or Shifter
; hl = pointer to mask
InitMask:
  push de
    ex de,hl
    ld bc,20+20
    call clrmem  ; clear work area
    ex de,hl
    ld b,20  ; byte Counter
    ld c,1  ; first bit
    ld d,0  ; bit Counter
 --:ld e,(hl)
  -:ld a,e
    and c
    jp z,+
    inc d
  +:ld a,c
    rlca
    ld c,a
    cp 1
    jp nz,-
    inc hl
    dec b
    jp nz,--
; got number of 1-bits in mask in reg d
    ld a,d
    and $f8
    rrca
    rrca
    rrca   ; divide by 8 (get byte offset)
    ld l,a
    ld h,0
    ld a,d
    and 7  ; bit offset
    inc a
    ld b,a
    ld a,$80
  -:rlca
    dec b
    jp nz,-
 pop de
 add hl,de
 ld de,20
 add hl,de
 ld (hl),a
 ret

; multi-byte Counter
count:
  push bc
  push de
  push hl
    ld hl,Counter ; 20 byte Counter Starts here
    ld de,20  ; somewhere in here is the stop bit
    ex de,hl
    add hl,de
    ex de,hl
  -:inc (hl)
    ld a,(hl)
    cp 0
    jp z,++ ; overflow to next byte
    ld b,a
    ld a,(de)
    and b  ; Test for terminal value
    jp z,+
    ld (hl),0  ; reset to zero
+:pop bc
  pop de
  pop hl
  ret

 ++:inc hl
    inc de
    jp -

; multi-byte Shifter
shift:
  push bc
  push de
  push hl
    ld hl,Shifter ; 20 byte shift register Starts here
    ld de,20  ; somewhere in here is the stop bit
    ex de,hl
    add hl,de
    ex de,hl
  -:ld a,(hl)
    or a
    jp z,++
    ld b,a
    ld a,(de)
    and b
    jp nz,+
    ld a,b
    rlca
    cp 1
    jp nz,+++
    ld (hl),0
    inc hl
    inc de
+++:ld (hl),a
    xor a  ; set Z
+:pop hl
  pop de
  pop bc
  ret
 
 ++:inc hl
    inc de
    jp -

; Test harness
; Self-modifying so it should be in RAM
TestCode:
  push af
  push bc
  push de
  push hl
    di   ; disable interrupts
    ld (StackPointerSaved),sp ; save stack pointer
    ld sp,MachineStateBeforeTest+2 ; point to Test-case machine state
      pop iy  ; and load all regs
      pop ix
      pop hl
      pop de
      pop bc
      pop af
    ld sp,(StackPointerBeforeTest)

IUTCode:
    .dsb 4,0  ; max 4 byte instruction under Test, modified at runtime

    ld (StackPointerAfterTest),sp ; save stack pointer
    ld sp,StackPointerAfterTest
      push af  ; save other registers
      push bc
      push de
      push hl
      push ix
      push iy
    ld sp,(StackPointerSaved) ; restore stack pointer
;    ei   ; enable interrupts - not needed on SMS
    ld hl,(MachineStateBeforeTest) ; copy memory operand
    ld (MachineStateAfterTest),hl
    ld hl,flgsat ; flags after Test
    ld a,(hl)
FlgMskCode:
    and $d7  ; mask-out irrelevant bits, modified at runtime
    ld (hl),a
    ld b,16  ; total of 16 bytes of state
    ld de,MachineStateAfterTest
    ld hl,CRCValue
tcrc: ld a,(de)
    inc de
    call updcrc  ; accumulate crc of this Test case
    dec b
    jp nz,tcrc
  pop hl
  pop de
  pop bc
  pop af
  ret

TestCodeend:

; machine state after Test
; MachineStateAfterTest: ds 14 ; memop,iy,ix,hl,de,bc,af
; StackPointerAfterTest: ds 2 ; stack pointer after Test
.define flgsat StackPointerAfterTest-2 ; flags
; StackPointerSaved: ds 2 ; saved stack pointer

; display hex string (pointer in hl, byte count in b)
hexstr:
  ld a,(hl)
  call phex2
  inc hl
  dec b
  jp nz,hexstr
  ret

; display hex
; display the big-endian 32-bit value pointed to by hl
phex8:
  push af
  push bc
  push hl
    ld b,4
  -:ld a,(hl)
    call phex2
    inc hl
    dec b
    jp nz,-
  pop hl
  pop bc
  pop af
  ret

; display byte in a
phex2:
  push af
    rrca
    rrca
    rrca
    rrca
    call phex1
  pop af
; fall through

; display low nibble in a
phex1:
  push af
  push bc
  push de
  push hl
    and $0f
    cp 10
    jp c,+
    add a,'a'-'9'-1
  +:add a,'0'
    ld e,a
    ld c,2
    call OutputText
  pop hl
  pop de
  pop bc
  pop af
  ret

OutputText:
  push af
  push bc
  push de
  push hl
; Accept call 2 (print char) and call 9 (print string)
; Ignore all others
    ld b,a  ; save char (destroys B)
    ld a,c
    cp 2
    jr z,PrintChar
    cp 9
    jr z,PrintString
OutputTextdone:
  pop hl
  pop de
  pop bc
  pop af
  ret

; Pass OutputText calls to display code
PrintChar:
  ld a,b  ; get char back
  cp 10  ; ignore LF, CR auto-LFs
  jr z,OutputTextdone
  call smsprint
  jr OutputTextdone

PrintString:
  ld a,(de)
  cp '$'
  jr z,OutputTextdone
  cp 10
  call nz,$10
  inc de
  jr PrintString

; Messages
msg1:   .db "Z80 instruction exerciser",10,13,10,13,"$"
msg2:   .db "Tests complete$"
okmsg:  .db "OK",10,13,"$"
ermsg1: .db "   CRC:$" ; was ERROR:
ermsg2: .db " expected:$"
crlf:   .db 10,13,"$"

; compare crc
; hl points to value to compare to CRCValue
cmpcrc:
  push bc
  push de
  push hl
    ld de,CRCValue
    ld b,4
  -:ld a,(de)
    cp (hl)
    jp nz,+
    inc hl
    inc de
    dec b
    jp nz,-
+:pop hl
  pop de
  pop bc
  ret

; 32-bit crc routine
; entry: a contains next byte, hl points to crc
; exit:  crc updated
updcrc:
  push af
  push bc
  push de
  push hl
  push hl
    ld de,3
    add hl,de ; point to low byte of old crc
    xor (hl) ; xor with new byte
    ld l,a
    ld h,0
    add hl,hl ; use result as index into table of 4 byte entries
    add hl,hl
    ex de,hl
    ld hl,CRCLookupTable
    add hl,de ; point to selected entry in CRCLookupTable
    ex de,hl
    pop hl
    ld bc,4 ; c = byte count, b = accumulator
  -:ld a,(de)
    xor b
    ld b,(hl)
    ld (hl),a
    inc de
    inc hl
    dec c
    jp nz,-
  pop hl
  pop de
  pop bc
  pop af
  ret

InitialiseCRC:
  push af
  push bc
  push hl
    ld hl,CRCValue
    ld a,$ff
    ld b,4
  -:ld (hl),a
    inc hl
    dec b
    jp nz,-
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

; SMS-specific stuff
font:
.include "BBC Micro font.inc"
fontend:
.include "VDP data.inc"

SMSInitialise:
  push af
  push bc
  push de
  push hl
    ld a,0
    ld (PauseFlag),a
    call SetUpVDP
    call LoadFont
    call LoadPalette
    call InitCursor
    call Install ; first, install Test code into RAM

    ; Turn screen on
    ld a,%11000000
;         ||||| |`- Zoomed sprites -> 16x16 pixels
;         ||||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;         ||||`---- 30 row/240 line mode
;         |||`----- 28 row/224 line mode
;         ||`------ VBlank interrupts
;         |`------- Enable display
;         `-------- Must be set (VRAM size bit)
    out ($bf),a
    ld a,$81
    out ($bf),a

  pop hl
  pop de
  pop bc
  pop af
  ret

; Set up our VDP for display. Set the correct video mode, and enable display
SetUpVDP:
  ld hl,vdpregs
  ld b,18
  ld c,$bf
  otir
  ret

; Install our palette into the VDP
LoadPalette:
  ; Set VRAM address
  ld a,$00
  out ($bf),a
  ld a,$c0
  out ($bf),a
  ld hl,palette
  ld b,$20
  ld c,$be
  otir
  ret

; Load the font. Also clears the VRAM first.
LoadFont:
  ld hl,$4000    ; Clear VRAM
  ld bc,1
  ld a,0
-:out ($be),a
  sbc hl,bc
  jr nz,-

  ld a,$00       ; Set VRAM write address
  out ($bf),a
  ld a,$60
  out ($bf),a

  ld hl,font
  ld bc,fontend-font
  ld de,1
-:ld a,(hl)
  out ($be),a
  out ($be),a
  out ($be),a
  out ($be),a
  inc hl
  dec c
  jr nz,-
  djnz -
  ret

; Initialize the cursor variables
InitCursor:
  ld hl,0
  ld a,0
  ld (CursorX),a
  ld (VRAMAdr),hl
  ld (Scroll),a
  ld (ScrollF), a
  ret

; Copies the Test code into modifiable memory.
; This is because the code is self-modifying.
Install:
  push bc
  push de
  push hl
    ld de,Test
    ld hl,TestCode
    ld bc,TestCodeend - TestCode
    ldir
  pop hl
  pop de
  pop bc

  ret

; Code to wait for Start of blanking period
WaitForVBlank:
  push af
  push bc
 --:in a,($7e)  ; get VCount value
  -:ld b,a      ; store it
    in a,($7e)  ; and again
    cp b        ; Is it the same?
    jr nz,-     ; If not, repeat
    cp $c0      ; Is is $c0?
    jp nz,--    ; Repeat if not
  pop bc
  pop af
  ret

; Code to print a character on the screen. Does stuff like handle
; line feeds, scrolling, etc.

smsprint:
  push af
  push bc
  push de
  push hl
    call WaitForVBlank

    ; First, write the character (in a) to the screen
    ; If it's a carriage return, skip it.
    cp 13
    jp z,doneprint
    ; Otherwise, we write.
    ld b,a        ; Save the output value
    ld c,$bf      ; Control port
    ld hl,(VRAMAdr)
    ld a,$78      ; Set nametable base bits
    or h          ; Get remaining bits

    out (c),l     ; Output lower-order bits
    out (c),a     ; Output upper bits + control

    ld a,b        ; Put value back in A
    sub $20       ; Shift into our font range

    ld c,$be
    out (c),a     ; Output the character

    ld a,b        ; Put value back in A... again

    ld b,1
    out (c),b

    ; Move cursor forward
    push af
      ld a,(CursorX)
      inc a
      ld (CursorX),a
    pop af

    ; Update VRAM pointer
    inc hl
    inc hl
    ld (VRAMAdr),hl

    ; Now the fun computation
doneprint:
    ; Check if this was a carriage return.
    cp 13
    jp z,nextline
    ; Check if we're at the end of the line
    ld a,(CursorX)
    cp 32
    jp z,nextline
  pop hl
  pop de
  pop bc
  pop af
  ret

  ; Here we do the job of scrolling the display, computing the
  ; new VRAM address, and all that fun stuff.
nextline:
    ld hl,(VRAMAdr)     ; Increase the VRAM position
    ; Get the cursor position and find out how far it was to the
    ; end of the line.
    ld a,(CursorX)
    ld b,a
    ld a,32
    sub b
    sla a               ; Now, double this and add it to HL. This is the new address.
    ld c,a
    ld b,0
    call namefill       ; Fill the rest of the line
    add hl,bc           ; Now create new address.
    ccf                 ; Next, check if we're past the end of the screen.
    push hl
      ld bc,$05C0
      sbc hl,bc
      jp m,+
      ld a,1            ; If we are, set the scroll flag on... we scroll from now on.
      ld (ScrollF),a
  +:pop hl
    push hl
      ld bc,$0700       ; Next, check if we're at the end of VRAM
      sbc hl,bc
    pop hl
    jp m,+
    ld hl,0             ; If we are, return to the top of VRAM.
  +:ld (VRAMAdr),hl     ; Now, save our VRAM address.
    ld bc,$40           ; Clear the new line
    call namefill
    ld a,(ScrollF)      ; Load the Scroll flag and check if it's set.
    cp 0
    jp z,noScroll
    ld a,(Scroll)       ; If it is, increase the Scroll value, and wrap at 28.
    inc a
    cp 28
    jp nz,doScroll
    ld a,0
doScroll:
    ld (Scroll),a       ; Now, write out Scroll value out to the VDP.
    sla a
    sla a
    sla a
    ld c,$bf
    out (c),a
    ld a,$89
    out (c),a
noScroll:
    ld a,0              ; Reset the cursor X position to 0
    ld (CursorX),a
  pop hl
  pop de
  pop bc
  pop af
  ret

; Fill the nametable from HL with BC bytes.
namefill:
  push af
  push bc
  push de
  push hl
    ld a,b      ; Wait for blanking period
    or c
    jp z,+
    push bc     ; Save the Counter for later.
      ld c,$bf  ; Control port
      ld a,$78  ; Set nametable base bits
      or h      ; Get remaining bits
      out (c),l ; Output lower-order bits
      out (c),a ; Output upper bits + control
      ; Now, zero out the region
      pop hl    ; Get our Counter
      ld a,0
      ld de,1
      ld c,$be
      ccf
    -:out (c),a ; Output the character
      sbc hl,de
      jp nz,-
+:pop hl
  pop de
  pop bc
  pop af
  ret

;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 0.11,"Z80 Intruction Exerciser",SDSCNotes,"Maxim"

SDSCNotes:
.db "Based on ZEXALL by Frank Cringle, "
.db "with credit to J.G.Harston and Brett K."
.db 0