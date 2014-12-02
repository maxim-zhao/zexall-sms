vdpregs:
.define SpriteSet           0       ; 0 for sprites to use tiles 0-255, 1 for 256+
.define NameTableAddress    $3800   ; must be a multiple of $800; usually $3800; fills $700 bytes (unstretched)
.define SpriteTableAddress  $3f00   ; must be a multiple of $100; usually $3f00; fills $100 bytes
.db %00100100,$80
;    |||||||`- Disable synch
;    ||||||`-- Enable extra height modes
;    |||||`--- SMS mode instead of SG
;    ||||`---- Shift sprites left 8 pixels
;    |||`----- Enable line interrupts
;    ||`------ Blank leftmost column for scrolling
;    |`------- Fix top 2 rows during scrolling
;    `-------- Fix right 8 columns during scrolling
.db %10000000,$81
;    ||||| |`- Zoomed sprites -> 16x16 pixels
;    ||||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;    ||||`---- 30 row/240 line mode
;    |||`----- 28 row/224 line mode
;    ||`------ VBlank interrupts
;    |`------- Enable display
;    `-------- Must be set (VRAM size bit)
.db (NameTableAddress>>10)|$01,$82	; (erq) OR data with $01 for 315-5124 compatibility
.db SpriteTableAddress>>7,$85
.db SpriteSet<<2,$86
.db $0,$87
;    `-------- Border palette colour (sprite palette)
.db $08,$88
;    ``------- Horizontal scroll
.db $00,$89
;    ``------- Vertical scroll
.db $ff,$8a
;    ``------- Line interrupt spacing

palette:

.db $00,$30,$0c,$03,$3c,$33,$0f,$16,$19,$06,$35,$21,$0d,$37,$23,$3f
.db $00,$30,$0c,$03,$3c,$33,$0f,$16,$19,$06,$35,$21,$0d,$37,$23,$07

