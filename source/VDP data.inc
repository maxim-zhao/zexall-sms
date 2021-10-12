.define SpriteSet           0       ; 0 for sprites to use tiles 0-255, 1 for 256+
.define NameTableAddress    $3800   ; must be a multiple of $800; usually $3800; fills $700 bytes (unstretched)
.define SpriteTableAddress  $3f00   ; must be a multiple of $100; usually $3f00; fills $100 bytes
VDPRegisterInitialisation:
.db %00100100,$80
;    |||||||`- Disable sync
;    ||||||`-- Enable extra height modes
;    |||||`--- SMS mode instead of SG
;    ||||`---- Shift sprites left 8 pixels
;    |||`----- Enable line interrupts
;    ||`------ Blank leftmost column for scrolling
;    |`------- Fix top 2 rows during scrolling
;    `-------- Fix right 8 columns during scrolling
.db %10000000,$81
;    |||||||`- Zoomed sprites -> 16x16 pixels
;    ||||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;    |||||`--- Always 0 for Mega Drive compatibility
;    ||||`---- 30 row/240 line mode
;    |||`----- 28 row/224 line mode
;    ||`------ VBlank interrupts
;    |`------- Enable display
;    `-------- VRAM size bit - always 1
.db (NameTableAddress>>10)|%11110001,$82	; Mask bits 0, 4-7 to 1 for compatibility
.db %11111111,$83 ; Unused, set all bits for compatibility
.db %11111111,$84 ; Unused, set all bits for compatibility
.db (SpriteTableAddress>>7)|%10000001,$85 ; Mask bits 0, 7 to 1 for compatibility
.db (SpriteSet<<2)|%10000011,$86 ; Mask bits 0-1, 7 for compatibility
.db $0,$87
;    `-------- Border palette colour (sprite palette)
.db $08,$88
;    ``------- Horizontal scroll
.db $00,$89
;    ``------- Vertical scroll
.db $ff,$8a
;    ``------- Line interrupt spacing

VDPRegisterInitialisation_TMS:
.db %00000000,$80
;    |||||||`- EXTVID - Enables external video input
;    ||||||`-- M2 - Select screen mode
;    ``````--- Unused
.db %11010000,$81
;    |||||||`- MAG - Sprites enlarged if set (sprite pixels are 2x2)
;    ||||||`-- SI - 16x16 sprites if set; 8x8 if reset
;    |||||`--- Unused
;    ||||`---- M3 - Select screen mode
;    |||`----- M1 - Select screen mode
;    ||`------ GINT - Generate interrupts if set
;    |`------- BL - Blank screen if reset; just backdrop. Sprite system inactive
;    `-------- 4/16K - Selects 16kB RAM if set.(must be 1)
.db (NameTableAddress>>10)|%11110000,$82
.db 0,$84 ; Patterns start at address 0 like in SMS mode
.db %11110001,$87
;    ||||````- BAckground colour (black)
;    ````----- Text colour (white)

;          0  1  2  3  4  5  6  7  8  9  a
; Me:     00 c0 fe ff 00 ff fb f1 00 00 ff
; BASIC:  00 f0 0f ff 03 76 03 13 00 00 ff
;            |   |     | || || ``- Colours
;            |   |     | || ``- Sprite indirect, unused
;            |   |     | ``- Sprite table, unused
;            |   |     `- Pattern location higher
;            |   `- Could have name table higher as it's smaller
;            `- Just no interrupts

Palette:
.db $00,$30,$0c,$03,$3c,$33,$0f,$16,$19,$06,$35,$21,$0d,$37,$23,$3f
.db $00,$30,$0c,$03,$3c,$33,$0f,$16,$19,$06,$35,$21,$0d,$37,$23,$07
PaletteEnd:

.define VDP_ADDRESS $bf
.define VDP_REGISTER $bf
.define VDP_STATUS $bf
.define VDP_DATA $be
.define VRAM_WRITE_MASK $4000

.macro SET_VDP_REGISTER args index, value
    ld a, value
    out (VDP_REGISTER), a
    ld a, $80 | index
    out (VDP_REGISTER), a
.endm

.macro SET_VRAM_ADDRESS args value
    ld a, <value
    out (VDP_ADDRESS), a
    ld a, $40 | (>value)
    out (VDP_ADDRESS), a
.endm

.macro SET_CRAM_ADDRESS args value
    ld a, <value
    out (VDP_ADDRESS), a
    ld a, $c0 | (>value)
    out (VDP_ADDRESS), a
.endm