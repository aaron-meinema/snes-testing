.include "header.inc"
.include "InitSNES.asm"

.bank 0 slot 0
.org 0
.section "Vblank"
;--------------------------------------
VBlank:
rti
;--------------------------------------
.ends


.bank 0 slot 0
.org 0
.section "Main"
;--------------------------------------
Start:
  InitSNES

  ;first load color, usually id do dma but this is copied
  rep #%00010000  ;16 bit xy
  sep #%00100000  ;8 bit ab

  ;See this? We take every byte from the palette, and put it to CGRAM
  ldx #$0000
- lda UntitledPalette.l,x
  sta $2122
  inx
  cpx #8
  bne -

  ;I'll explain this later
  ;We'll have two palettes, only one color is needed for the second:
  lda #33		;The color we need is the 33rd
  sta $2121
  lda.l Palette2
  sta $2122
  lda.l Palette2+1
  sta $2122

  ;now load the tiles in vram by dma
  ldx #UntitledData   ; Address
  lda #:UntitledData  ; of UntitledData
  ldy #(15*16*2)      ; length of data
  stx $4302           ; write
  sta $4304           ; address
  sty $4305           ; and length
  lda #%00000001      ; set this mode (transferring words)
  sta $4300
  lda #$18            ; $211[89]: VRAM data write
  sta $4301           ; set destination

  ldy #$0000          ; Write to VRAM from $0000
  sty $2116

  lda #%00000001      ; start DMA, channel 0
  sta $420B

  lda #%10000000	; VRAM writing mode
  sta $2115
  ldx #$4000	    ; write to vram
  stx $2116       ; from $4000

  ;ugly code starts here - it writes the # shape I mentioned before.
  .rept 2
     ;X|X|X
     .rept 2
       ldx #$0000 ; tile 0 ( )
       stx $2118
       ldx #$0002 ; tile 2 (|)
       stx $2118
     .endr
     ldx #$0000
     stx $2118
     ;first line finished, add BG's
     .rept 27
       stx $2118  ; X=0
     .endr
     ;beginning of 2nd line
     ;-+-+-
     .rept 2
       ldx #$0004 ; tile 4 (-)
       stx $2118
       ldx #$0006 ; tile 6 (+)
       stx $2118
     .endr
     ldx #$0004   ; tile 4 (-)
     stx $2118
     ldx #$0004
     .rept 27
       stx $2118
     .endr
  .endr
  .rept 2
    ldx #$0000    ; tile 0 ( )
    stx $2118
    ldx #$0002    ; tile 2 (|)
    stx $2118
  .endr

  ldx #$6000  ; BG2 will start here
  stx $2116
  ldx #$000C  ; And will contain 1 tile
  stx $2118

  ;set up the screen
  lda #%00110000  ; 16x16 tiles, mode 0
  sta $2105       ; screen mode register
  lda #%01000000  ; data starts from $4000
  sta $2107       ; for BG1
  lda #%01100000  ; and $6000
  sta $2108       ; for BG2

  stz $210B	    ; BG1 and BG2 use the $0000 tiles

  lda #%00000001  ; enable bg1(bit0) and not yet 2 (bit 1)
  sta $212C


  lda #%00001111  ; enable screen, set brightness to 15
  sta $2100


forever:
wai
jmp forever
;--------------------------------------
.ends
.bank 1 slot 0       ; We'll use bank 1
.org 0
.section "Tiledata"
.include "tiles.inc" ; If you are using your own tiles, replace this
.ends
