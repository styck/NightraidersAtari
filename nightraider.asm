;--------------------------------
; NIGHTRAIDERS
; WRITTEN BY : PETER FILIBERTI
; COPYRIGHT 1982
;--------------------------------
;       ORG $6B0               ; mads assembler
;       .OR  $6B0              ; original
        * = $6b0              ; ATasm
;       .TF  OBJ               ;Stores object file directly to disk
;--------------------------------
; ZERO PAGE VARIABLES
;--------------------------------
HISCORE1   = $80             ;These are the three locations where
HISCORE2   = HISCORE1+1      ;the high score is saved in BCD
HISCORE3   = HISCORE2+1      ;for later use.
OLSCORE1   = HISCORE3+1      ;These are the three locations where
OLSCORE2   = OLSCORE1+1      ;the last score achieved is placed
OLSCORE3   = OLSCORE2+1      ;for use in menu screen also BCD.
BSCOR0     = OLSCORE3+1      ;This is a binary score used as a
BSCOR1     = BSCOR0+1        ;temporary score holder by the game.
SCORE1     = BSCOR1+1        ;This is the actual game score
SCORE2     = SCORE1+1        ;stored in BCD it is also the same
SCORE3     = SCORE2+1        ;score put on game screen while playing.
TEMP1      = SCORE3+1        ;TEMP1 through TEMP8 are temporary
TEMP2      = TEMP1+1         ;locations used by main program and
TEMP3      = TEMP2+1         ;subroutines. The INTERRUPT routines
TEMP4      = TEMP3+1         ;must not touch these locatons!
TEMP5      = TEMP4+1         ;""
TEMP6      = TEMP5+1         ;""
TEMP7      = TEMP6+1         ;""
TEMP8      = TEMP7+1         ;""
CLRVAR     = TEMP8+1
LEVEL      = CLRVAR+1        ;Games level of play 0-6
SCRCNT     = LEVEL+1         ;Screen fine scroll counter
HPOS1      = SCRCNT+1        ;Horizontal position player1
HPOS2      = HPOS1+1         ;Horizontal position player2
HPOS3      = HPOS2+1         ;Horizontal position player3
HPOS4      = HPOS3+1         ;Horizontal position plyer4
MOVFLG     = HPOS4+1         ;Flag for screen movement
FUEL       = MOVFLG+1        ;Fuel in planes tank 0-$50
CROSSX     = FUEL+1          ;Plane horizontal axis position
MISSLEX    = CROSSX+1        ;Missle horizontal position
SHIPS      = MISSLEX+1       ;Number of ships left. start=3
IRQVAR1    = SHIPS+1         ;IRQVAR1 through IRQVAR2 are
IRQVAR2    = IRQVAR1+1       ;temporary locations to be
IRQVAR3    = IRQVAR2+2       ;used by interrupt routines only.
IRQVAR4    = IRQVAR3+3       ;Main program should not use these!
TPOINT     = IRQVAR4+1       ;Target point positioner.
HIT1       = TPOINT+1        ;HIT1-HIT4 are copies of the
HIT2       = HIT1+1          ;colision register updated every
HIT3       = HIT2+1          ;60 hz. It is used by main program
HIT4       = HIT3+1          ;interrupts logicaly or data in these.
COLLAD     = HIT4+1          ;Screen collision address (TWO BYTES)
ACTFLG     = COLLAD+2        ;Plane action flag.
;--------------------------------
; ATARI LOCATIONS
;--------------------------------
SCREEN     = $4000           ;Location in Memory of our Menu Screen Data
PMBASE     = $D407           ;Player missle base address
DISPLA     = $3F00           ;Another menu scree location
VDLST      = $200            ;Display list interrupt vector
VBLK       = $224            ;Vertical blank interrupt vector
DLISTP     = $230            ;Display list pointer
CLB        = $2C8            ;Color register background
CLP0       = $2C4            ;Color register playfield 1
CLP1       = $2C5            ;Color register playfield 2
CLP2       = $2C6            ;Color register playfield 3
CLP3       = $2C7            ;Color register Playfield 4
CPLAY0     = $2C0            ;Color player 1
CPLAY1     = $2C1            ;Color player 2
CPLAY2     = $2C2            ;Color player 3
CPLAY3     = $2C3            ;Color player 4
CONSOL     = $D01F           ;Console switch address
GRACTL     = $D01D           ;Graphic control address
CHBASE     = $2F4            ;Character set base address
TRIG0      = $284            ;Joystick trigger
DMACTL     = $22F            ;Dma control register
NMIEN      = $D40E           ;NMI control register
;--------------------------------
; GAME COLD START
;--------------------------------
COLDSTART  LDX #HISCORE1       ;Clear all of zero page variables
           JSR INIT            
           JMP INTRO           ;Goto menu routines
WARMSTART  LDX #BSCOR0         ;Erase all of zero page variables
           JSR INIT            ;except high scores!
           JMP INTRO           ;Goto menu routines
;--------------------------------
; WORDS USED BY INTRODUCTION
;--------------------------------
 WORDS .BYTE 00
       .BYTE "BY PETER FILIBERTI    DATAMOST INC  8@?:"
       .BYTE 00
       .BYTE "HIGH SCORE      YOUR SCORE      "
       .BYTE 00
       .BYTE "CURRENT RANK"
       .BYTE 00
LEVNAM .BYTE "NOVICE"
       .BYTE 00
       .BYTE "CADET"
       .BYTE 00
       .BYTE "ENSIGN"
       .BYTE 00
       .BYTE "CAPTAIN"
       .BYTE 00
       .BYTE "COMMANDER"
       .BYTE 00
       .BYTE "NIGHTRAIDER" 
       .BYTE 00
       .BYTE "PRESS SELECT TO CHANGE STARTING RANK"
       .BYTE 00
       .BYTE "PRESS START OR FIRE BUTTON TO PLAY"
       .BYTE 00
       .BYTE "TODAYS TOP TEN HIGH SCORES"
       .BYTE 00
       .BYTE " CONGRAULATIONS YOU HAVE THE HIGH SCORE "
       .BYTE 00
       .BYTE "    YOUR SCORE IS ONE OF THE TOP TEN    "
       .BYTE 00
       .BYTE "           ENTER YOUR INITIALS          "
       .BYTE 00
;--------------------------------
TBL1   .BYTE $12,$19,$17,$0B
COLORT .BYTE $8D,$94,$CA,$45,$00 
P1     .BYTE $00,$18,$24,$24,$7E,$FF,$FF,$FF     ;P1-P4 are plane shapes 
       .BYTE $FF,$FF,$81,$FF             ;I just felt like putting
P2     .BYTE $00,$00,$18,$18,$00,$00,$42,$42     ;them here!??
       .BYTE $5A,$FF,$FF,$FF
P3     .BYTE $00,$00,$00,$00,$00,$01,$03,$0F
       .BYTE $1F,$7F,$00,$00
P4     .BYTE $00,$00,$00,$00,$00,$80,$C0,$F0    
       .BYTE $F8,$FE,$00,$00
;--------------------------------
; INTRODUCTION ROUTINE
;--------------------------------
INTRO  LDA #$2C                 ;Setup character base address
       STA CHBASE
       LDA #LIST1&255            ;Setup our display list pointers
       STA DLISTP               ;to point to our display list
       LDA #LIST1/255
       STA DLISTP+1
       LDA #$00
       STA $D405                ;Put a zero in horizontal scroll reg
       LDA #$3A
       STA DMACTL               ;Enable player DMA
       LDA #$03
       STA GRACTL               ;Enable player graphics
       JSR CLRMEN               ;Clear menu page
       LDA #MIRQ1&255               ;Setup the irq vectors to
       STA VDLST                ;point to our Irq routines
       LDA #MIRQ1/255
       STA VDLST+1
VSYNC  LDA $D40B                ;VCOUNT - Is scan line at the top of the screen?
       CMP #$80                 ;For an NTSC machine, VCOUNT counts from $00 to $82; for PAL, it counts to $9B.
       BCC VSYNC                ;If not then loop
       LDA #$C0                 ;NMIEN_DLI($80) | NMIEN_VBI($40) - activate display list interrupt and vertical blank interrupt
       STA NMIEN                ;Enable Display list interrupts
       LDA #$00
       STA TEMP1                ;Setup menu screen messages
       STA TEMP7
       LDX #$01     
       LDY #$00     
       JSR PRINT                ;By peter filiberti etc....
       INC TEMP1
       LDX #$02
       LDY #$07     
       JSR PRINT                ;High score    Your score
       
SCORER LDX #$02                 ;Put high score and last score
       LDY #$00                 ;on screen
FDS9   LDA HISCORE1,X           ;Get a BCD byte
       LSR                      ; 
       LSR                      ;
       LSR                      ;Only want left digit
       LSR                      ;
       CLC                      ;Add 1 to convert to
       ADC #$01                 ;our weird ascii
       STA $4059,Y              ;Put on screen
       LDA OLSCORE1,X
       LSR  
       LSR
       LSR
       LSR
       CLC
       ADC #$01
       STA $4069,Y
       INY
       LDA HISCORE1,X           ;Get a BCD byte
       AND #$0F                 ;Only want right digit
       CLC                      ;Add 1 to convert to
       ADC #$01                 ;our weird ascii
       STA $4059,Y              ;Put on screen
       LDA OLSCORE1,X
       AND #$0F
       CLC
       ADC #$01
       STA $4069,Y
       INY
       DEX
       BPL FDS9
       LDA #$80
       STA TEMP7
       LDA #$98     
       STA CLP3
       LDA #$06
       STA TEMP1
       LDX #$0A     
       LDY #$02     
       JSR PRINT
       INC TEMP1
       LDX #$0B
       LDY #$03
       JSR PRINT
       LDA #$0A
       STA TEMP1
       LDA #$00
       STA TEMP7    
       LDX #$03
       LDY #$04     
       JSR PRINT
       LDA #$00
       STA LEVEL
SELECT LDX #$14
       LDA #$00
FDS10  STA $41A4,X
       DEX
       BNE FDS10
       LDA #$0A
       STA TEMP1
       LDA #$80
       STA TEMP7
       LDA LEVEL 
       CLC
       ADC #$04
       TAX
       LDY #$17
       JSR PRINT
       JSR BEEPS
       LDA #$00
       STA TEMP6
       STA TEMP7
       STA TEMP8
CKEY   LDA CONSOL
       ROR
       BCS PF1
       JMP GAME
PF1    ROR
       BCS PF2
PF3    LDA CONSOL
       AND #$02
       BEQ PF3
       INC LEVEL
       LDA LEVEL
       CMP #$06
       BNE PF4
       LDA #$00
       STA LEVEL
PF4    JMP SELECT
PF2    LDA $D010
       BNE CKEY
       JMP GAME
;--------------------------------
CLRMEN LDA #$40                 ;Erase our menu screen
       STA TEMP2                ;$4000-$5000
       LDY #$00
       STY TEMP1
FDS12  TYA
FDS11  STA (TEMP1),Y
       INY
       BNE FDS11
       INC TEMP2
       LDA TEMP2
       CMP #$50
       BNE FDS12
       RTS
;--------------------------------
; MIRQ1 IRQ FOR MENU!
;--------------------------------
MIRQ1  PHA
       TXA
       PHA
       LDA $D40B        ;VCOUNT
       CMP #$30         ;For an NTSC machine, VCOUNT counts from $00 to $82; for PAL, it counts to $9B.
       BCS MIRQ2
       LDA $2C6
       LDX #$11     
FDS13  STA $D40A        ;WSYNC - Wait for Sync 0 A write to WSYNC causes the CPU to halt execution until the start of horizontal blank.
       STA $D018
       CLC
       ADC #$02     
       DEX
       BNE FDS13 
       INC CCNT
       LDA CCNT
       CMP #$08     
       BNE FDS14
       LDA #$00
       STA CCNT
       INC $2C6
FDS14  LDA #$28     
       STA $D018
       PLA
       TAX
       PLA
       RTI    
CCNT   .BYTE 00
;--------------------------------
MIRQ2  TYA
       PHA
       LDA #$C8     
       STA $D018
       LDA #$0F
       STA $D019    
       PLA
       TAY
       PLA
       TAX
       PLA
       RTI
;--------------------------------
GAME   LDA #$00
       STA NMIEN   
       STA DMACTL 
       LDX #$05
COLFIL LDA COLORT-1,X
       STA CLP0-1,X
       DEX
       BNE COLFIL
       LDA #IRQ1&255
       STA VDLST
       LDA #IRQ1/255
       STA VDLST+1
       LDA #VBLANK&255
       STA VBLK
       LDA #VBLANK/255
       STA VBLK+1
       LDA #LIST2&255
       STA DLISTP
       LDA #LIST2/255
       STA DLISTP+1
       LDA #$58
       STA LIST2+3
       LDA #$4C
       STA LIST2+4
       LDA #$07
       STA SCRCNT
       STA $D405
       JSR CLRMIS
       JSR SETSCREEN
       LDA #$0F
       STA $D404
       JSR PMAKER
       LDA #$70
       STA $2F4
       LDA #$50
       JSR MAPFIL
       LDA #$31
       STA $26F
CS     STA $D40A
       LDA $D40B         ;VCOUNT
       CMP #$78          ;For an NTSC machine, VCOUNT counts from $00 to $82; for PAL, it counts to $9B.
       BNE CS
       STA $D40A
       LDA #$C0          ;NMIEN_DLI($80) | NMIEN_VBI($40) - activate display list interrupt and vertical blank interrupt
       STA NMIEN
       LDA #$3E  
       STA DMACTL 
       LDA #$00
       STA BSCOR0
       STA BSCOR1
       STA SCORE1
       STA SCORE2
       STA SCORE3
       LDA #$50
       STA FUEL
       LDA #$04
       STA SHIPS

;--------------------------------
; NIGHTRAIDER GAME LOOPS
;--------------------------------
           LDA #$2C
           STA $2F4
           LDX #$0C
CURRAN     LDA BMES1-1,X 
           STA $4CFF,X
           DEX
           BNE CURRAN 
           JSR BOOP
           LDY LEVEL
           INY
FINDLEV    DEY
           BEQ GOGO
LOPFIND    INX
           LDA LEVNAM,X
           BNE LOPFIND
           INX
           BNE FINDLEV
GOGO       LDA LEVNAM,X
           BEQ TERR
           CMP #$20
           BNE SIRS
           LDA #$36
SIRS       SEC
           SBC #$36
           ORA #$80
           STA $4D0F,Y
           INY
           INX
           JMP GOGO
TERR       JSR BEEPS
           LDX #17
MIO        LDA BMES2-1,X
           STA $4D72,X
           DEX
           BNE MIO
           JSR BOOP
           LDX #$0D
MIO2       LDA BMES3-1,X
           STA $4D86,X
           DEX
           BNE MIO2
           JSR BEEPS
           LDX #$6
MIO3       LDA BMES4-1,X
           STA $4DF5,X
           DEX
           BNE MIO3
           JSR BOOP
           LDX #$6
MIO4       LDA BMES5-1,X
           STA $4DFE,X 
           DEX
           BNE MIO4
           JSR BEEPS
           LDX #$5
MIO5       LDA #$A8
           STA $D203
           STA $D205
           LDA #85
           STA $D202
           LDA #86 
           STA $D204
           TXA
           PHA
           CLC
           ADC #$01
           STA $4E25
           LDX #$04 
           JSR DLONG
           LDA #$00
           STA $D203
           STA $D205
           LDX #$10 
           JSR DLONG
           PLA
           TAX
           DEX
           BNE MIO5
           LDA #$01  
           STA $4E25  
           LDA #$A8
           STA $D203
           STA $D205
           LDA #50
           STA $D202
           LDA #51
           STA $D204
           LDA #$88
           STA $D201
           LDX #$05
MYOMY      STX $D200 
           TXA
           PHA
           LDX #$3
           JSR DLONG
           PLA
           TAX
           INX
           CPX #25  
           BNE MYOMY 
           LDA #$00
           STA $D201
           STA $D205
           LDA #$84
           STA $D203
           LDA #$50 
           STA $D202
           JSR DELAY
           LDX #$00
           LDA #$50
           JSR MAPFIL
           LDA #$70
           STA $2F4
           INC MOVFLG
           INC ACTFLG
;--------------------------------
; BEGINING GAME INTRO SHOWN
; NOW CLEAR THE BULSHIT AND
; LETS GET ON WITH SOME ACTION!
;--------------------------------
           JMP GM1
BOOP       LDA #$AF
           STA $D201
           LDA #$70
           STA $D200
           LDX #$4 
           JSR DLONG   
           LDA #$00  
           STA $D201   
           LDX #$5 
           JSR DLONG  
           RTS
BEEPS      LDA #$03
BEEPS2     PHA  
           LDA #$AF
           STA $D201
           LDA #$10
           STA $D200
           LDX #$02
           JSR DLONG   
           LDA #$00  
           STA $D201   
           LDX #$02
           JSR DLONG
           PLA
           SEC
           SBC #$01
           BNE BEEPS2
           LDX #$5
           JSR DLONG
           RTS
BMES1      .BYTE $0D,$1F,$1C,$1C,$0F,$18,$1E,$00
           .BYTE $1C,$0B,$18,$15
BMES2      .BYTE $17,$13,$1D,$1D,$13,$19,$18,$00
           .BYTE $19,$0C,$14,$0F,$0D,$1E,$13,$20,$0F
BMES3      .BYTE $8E,$8F,$9D,$9E,$9C,$99,$A3,$00,$8F
           .BYTE $98,$8F,$97,$A3
BMES4      .BYTE $1D,$1E,$0B,$1E,$1F,$1D
BMES5      .BYTE $96,$8B,$9F,$98,$8D,$92
;--------------------------------
; MAIN GAME LOOP #1
;--------------------------------
GM1        LDA #COLRUT&255   ;SETUP
           STA COLLAD       ;COLLISION
           LDA #COLRUT/255   ;ROUTINE
           STA COLLAD+1     ;VECTOR
GMLOOP     JSR PAUSER
           LDA BASER  
           BEQ PF5
           JSR EXPLOB
PF5        JSR TRAINER 
           JSR BRIDGER
           JSR MX
           JSR SOUND
           JSR MISSLES
           JSR SPCATK
           JSR UFODIE
           JSR ATTACK 
           JSR FIREPOWER
           LDA SPACFLG 
           BEQ PF6
           JSR KILLER2
           JMP GMLOOP
PF6        JSR KILLER  
           JMP GMLOOP
;--------------------------------
; SUBROUTINE KILLER
; EXPLODES KILLED OBJECTS
; AND HANDLES DEATH!
;--------------------------------
KILLER     JSR CONTROL
           INC KILCNT 
           LDA KILCNT
           CMP #$40
           BNE KILEND
           LDA #$00
           STA KILCNT
           LDX #$00 
KIL2       LDA HITABLE,X
           CMP #$FF
           BEQ KILEND
           CMP #$00
           BNE KIL4
KIL3       INX  
           INX
           INX
           INX
           INX
           BNE KIL2
KIL4       STX TEMP3 
           DEC HITABLE,X
           PHA
           LDA HITABLE+1,X 
           STA TEMP1 
           LDA HITABLE+2,X
           STA TEMP2
           PLA
           CMP #$06
           BNE KIL10
           LDA HITABLE+3,X
           LDX #$00
KIL5       CMP CHATBL,X 
           BEQ KIL6
           CMP #$FF
           BEQ KIL8
           INX
           BNE KIL5
KIL6       LDA #$00
           TAY
KIL7       STA (TEMP1),Y
           INX
           LDY CHTBL2,X
           BNE KIL7
KIL8       LDX TEMP3
           JMP KIL3
KILEND     RTS
KIL10      EOR #$FF
           CLC
           ADC #$06
           ASL
           STA TEMP4
           CMP #$08
           BNE TI
           JMP KIL12  
TI         LDA HITABLE+3,X
           LDX #$03
TJ         CMP EXPTBL,X  
           BNE TK  
           JMP KIL11  
TK         DEX
           BPL TJ  
           LDX TEMP3 
           LDA #$00
           STA HITABLE,X
           LDA HITABLE+3,X
           CMP #$15
           BCC KBRIDGE
           LDA TEMP1
           SEC
           SBC #$20
           STA TEMP1
           BCS PF22
           DEC TEMP2
PF22       LDY #$20
PF23       LDA (TEMP1),Y
           BEQ PF24
           CMP #$19   
           BEQ PF24
           CMP #$9E
           BEQ PF24
           DEY
           BNE PF23
           JMP KIL8
PF24       INY
           LDA #$9B
           STA (TEMP1),Y
PF25       LDA (TEMP1),Y
           CMP #$11
           BEQ PF26
           CMP #$19
           BEQ PF26
           CMP #$9A
           BEQ PF26
           LDA #$00
           STA (TEMP1),Y
           INY
           BNE PF25
PF26       LDA #$00
           STA TRNFLG
           LDA LEVEL
           ASL
           CLC
           ADC BSCOR1
           STA BSCOR1
           JMP KIL8

KBRIDGE    LDA TEMP1
           SEC
           SBC #$20
           STA TEMP1
           BCS FDS15
           DEC TEMP2
FDS15      LDY #$20
FDS16      LDA (TEMP1),Y
           CMP #$11
           BEQ FDS17
           DEY
           BNE FDS16
FDS17      LDA #$15
           STA (TEMP1),Y
           INY
           LDA #$16
           STA (TEMP1),Y
           INY
           LDA #$17
           STA (TEMP1),Y
           INY
           LDA #$18
           STA (TEMP1),Y
           LDA BSCOR0
           CLC
           ADC #$C8
           STA BSCOR0
           BCC FDS18
           INC BSCOR1
FDS18      INC BRDFLG
           JMP KIL8
KIL11      LDY EXPTBL2,X
           LDX TEMP4  
           LDA EXPTBL3,X
           STA (TEMP1),Y
           INY
           LDA EXPTBL3+1,X
           STA (TEMP1),Y
           JMP KIL8

KIL12      LDA HITABLE+3,X 
           LDX #$00
FDS19      CMP PNTBL,X
           BEQ FDS20
           INX
           BNE FDS19
FDS20      PHA
           LDA PNTBL+1,X
           CLC
           ADC BSCOR0
           STA BSCOR0
           LDA PNTBL+2,X
           ADC BSCOR1
           STA BSCOR1
           PLA
           LDX #$00
FDS21      CMP EXPTBL,X   
           BEQ KIL13
           INX
           BNE FDS21

KIL13      LDY EXPTBL2,X
           LDA TEMP1 
           SEC
           SBC #$01
           STA TEMP1
           BCS KIL14
           DEC TEMP2
KIL14      LDA #$2C   
           STA (TEMP1),Y
           INY
           LDA #$2D
           STA (TEMP1),Y
           INY
           LDA #$2E
           STA (TEMP1),Y
           INY
           LDA #$2F
           STA (TEMP1),Y
           JMP KIL8
;--------------------------------
; CONTROL SUBROUTINE
; SETS FLAGS FOR OTHER ROUTINES
; DEPENDING ON GAME PLAY
;--------------------------------

CONTROL    LDA BASER
           BEQ FDS22
           JMP CON4
FDS22      LDA SPACFLG
           BNE CONEND
           LDA LIST2+3
           BNE CONEND
           LDA LIST2+4  
           CMP #$40  
           BNE CONEND
           LDA BASFLG
           BNE CON2
           LDA MOVFLG
           BEQ CON3
           LDA #$00
           STA MOVFLG 
           INC SPACFLG
CONEND     RTS 
CON3       LDA #$58 
           STA LIST2+3
           LDA #$4C
           STA LIST2+4
           LDA #$60
           JSR MAPFIL
           INC MOVFLG
           INC BASFLG
           RTS

CON2       LDA #$40
           STA TEMP2
           LDA #$00
           STA TEMP1
           LDY #00
PF11       TYA
PF12       STA (TEMP1),Y
           DEY
           BNE PF12
           INC TEMP2
           LDA TEMP2
           CMP #$50
           BNE PF11
           LDX #$00
PF13       LDA $6000,X
           STA $48C0,X
           LDA $6100,X
           STA $49C0,X
           LDA $6200,X
           STA $4AC0,X
           DEX
           BNE PF13
           LDX #$6F
PF14       LDA $6300,X
           STA $4BC0,X
           DEX
           BNE PF14
           LDA #$58
           STA LIST2+3    
           LDA #$4C
           STA LIST2+4
           LDA #$FF
           STA BASER  
           LDA BASDEAD
           BEQ PF15
           ASL
           TAX
PF16       LDA BASOLD-2,X 
           STA TEMP1
           LDA BASOLD-1,X
           STA TEMP2
           LDY #$00
           LDA #$B0
           STA (TEMP1),Y
           INY
           STA (TEMP1),Y
           DEX
           DEX
           BNE PF16
PF15       RTS

CON4       LDA LIST2+3
           CMP #$78
           BNE FDS23
           LDA LIST2+4
           CMP #$45
           BNE FDS23
           LDA #$58
           STA LIST2+3
           LDA #$4C
           STA LIST2+4
           LDA #$50
           JSR MAPFIL
           LDA #$00
           STA SPACFLG
           STA BASER
           STA BASFLG
           LDX #$08
FDS24      STA MUSCNT,X
           DEX
           BPL FDS24
FDS23      RTS
;--------------------------------
; SUBROUTINE TRAINER
; CHECKS FOR BRIDGES ON THE SCREN
; AND ROLLS TRAINS ACROSS THEM
;--------------------------------
TRAINER    JSR CONTROL
           INC TRNCNT  
           LDA TRNCNT
           CMP #$30 
           BEQ FDS25
           RTS
FDS25      LDA #$00
           STA TRNCNT
           LDA TRNFLG  
           BNE TRAIN5
           LDA LIST2+3
           STA TEMP1
           LDA LIST2+4
           STA TEMP2
           LDY #$00
           LDA (TEMP1),Y
           CMP #$11
           BNE TRAIN3
           INY
           LDA (TEMP1),Y
           CMP #$12
           BEQ TRAIN2
TRAIN3     RTS  
TRAIN2     LDA TEMP1
           SEC
           SBC #$28
           STA TRNVAR1
           BCS TRAIN4
           DEC TEMP2
TRAIN4     LDA TEMP2  
           STA TRNVAR2
           INC TRNFLG
           LDA #$0
           STA TRNPNT1
           LDA #$21
           STA TRNPNT2
           RTS

TRAIN5     LDA TRNVAR1
           STA TEMP1
           LDA TRNVAR2
           STA TEMP2
           LDA TRNPNT1
           CMP #$27
           BEQ TRAIN6
           LDY TRNPNT1
           LDX TRNPNT2   
FDS26      CPX #$FF
           BEQ FDS27
           LDA TRNSTR,X
           DEX
           JMP FDS28
FDS27      LDA #$19
FDS28      STA (TEMP1),Y
           DEY
           BPL FDS26
           INC TRNPNT1
           RTS

TRAIN6     LDY #$27
           LDX TRNPNT2
           BMI TRAIN7
FDS29      CPX #$FF
           BEQ FDS30
           LDA TRNSTR,X
           DEX
           JMP FDS31
FDS30      LDA #$19
FDS31      STA (TEMP1),Y
           DEY
           BPL FDS29
           DEC TRNPNT2 
           RTS

TRAIN7     LDY #$27
           LDA #$19
FDS32      STA (TEMP1),Y
           DEY
           BPL FDS32
           DEC TRNFLG
           RTS

TRNSTR     .BYTE $19,$9A,$9B,$9B,$9B,$9B,$9E,$9A
           .BYTE $9B,$9B,$9B,$9B,$9E,$9A,$9B,$9B
           .BYTE $9B,$9B,$9E,$9A,$9B,$9B,$9B,$9B
           .BYTE $9E,$9A,$9B,$9B,$9B,$9B,$9B,$9B
           .BYTE $9C,$9D
;--------------------------------
; ATTACK ROUTINES ATTACKS 
; PLAYER DEPENDING ON
; WHAT FLAGS ARE SET
;--------------------------------
ATTACK     JSR CONTROL
           DEC FLCNT2
           BNE PF33
           LDA #$1C
           STA FLCNT2
           DEC FLCNT 
           BNE PF33
           LDA CRUD
           BNE PF33
PF32       LDA LEVEL
           ASL
           ASL
           ASL
           STA TEMP1
           LDA #59
           SEC
           SBC TEMP1
           STA FLCNT
           DEC FUEL 
           BNE PF33
           JSR PLANEGON   
PF33       LDA SPACFLG  
           BEQ PF36
           LDA WRNCNT
           BNE PF36
           LDA SAUCFLG
           BNE PF35
PF36       LDA SAUCFLG
           BNE PF35
           LDA SPS1  
           INC LEVEL 
           INC LEVEL 
           SEC
           SBC LEVEL
           DEC LEVEL 
           DEC LEVEL
           STA SPS1
           BCS PF31
           DEC SPS2
           BNE PF31
           LDA #$A0
           STA SPS2
           LDA #$FF
           STA SPS1
           INC SAUCFLG  
           LDY #$00
           LDX #$00
           LDA $D20A
           BMI PF34
           INX
           LDY #$FF
PF34       STX SAUCDIR   
           STY HPOS1
           STY HPOS2
           LDA #$35
           STA SAUCY
           LDA #$20
           STA WRNCNT  
           LDX #$60
           LDA #$00
PF37       STA $3400,X
           STA $3500,X
           DEX
           BNE PF37
PF31       RTS


PF35       LDA WRNCNT
           BEQ MOVSAUC 
           INC FLYCNT
           LDA FLYCNT
           CMP #$50
           BNE PF31
           LDA #$00
           STA FLYCNT
           LDA WRNCNT
           AND #$01
           BEQ TONE2
           LDA #25
           BNE TONE1
TONE2      LDA #100
TONE1      STA $D206  
           LDA #$AF
           STA $D207
           LDX #$09
FDS34      LDA WRNMES-1,X
           STA $3E99,X
           DEX
           BNE FDS34
           DEC WRNCNT
           BNE FDS33
           LDA #$00
           STA $D206
           STA $D207
           LDX #$09
FDS35      STA $3E99,X
           DEX
           BNE FDS35
FDS33      RTS
;

MOVSAUC    INC SAUCNT
           LDA SAUCNT
           CMP #$10 
           BEQ PF46
           RTS
PF46       LDA #$00   
           STA SAUCNT
           LDA SAUCDIR  
           BEQ PF41
           DEC HPOS1
           DEC HPOS2
           JMP PF42
PF41       INC HPOS1
           INC HPOS2
PF42       LDA SAUCT
           BEQ PF43
           BMI PF44
           INC SAUCY 
           LDA SAUCY
           CMP #$60
           BNE PF43
           LDA #$00
           STA SAUCT
           JMP PF43
PF44       DEC SAUCY
           LDA SAUCY
           CMP #$20
           BNE PF43
           LDA #$00
           STA SAUCT
PF43       LDX SAUCY  
           INX
           LDY #$0A
           LDA #$00
PF45       STA $3400,X
           STA $3500,X
           DEX  
           DEY    
           BPL PF45
           LDA #$44
           STA $2C0
           LDA #$84
           STA $2C1
           LDA SAUCPNT
           CMP #$04
           BNE PLOTSAUC
           LDA #$00
           STA SAUCPNT

PLOTSAUC   ASL
           TAX
           LDA SAUCTBL,X
           STA TEMP1
           LDA SAUCTBL+1,X
           STA TEMP2
           LDX SAUCY
           LDY #$00
FDS36      LDA (TEMP1),Y
           STA $3400,X  
           TYA
           CLC  
           ADC #$08
           TAY
           LDA (TEMP1),Y
           STA $3500,X
           TYA
           SEC
           SBC #$08
           TAY
           DEX
           INY
           CPY #$08
           BNE FDS36
           INC SAUCPNT2
           LDA SAUCPNT2
           CMP #$06
           BNE FDS37
           LDA #$00
           STA SAUCPNT2
           INC SAUCPNT
FDS37      LDA SAUCT 
           BNE MISCHK
           LDA $D20A
           BMI FDS38
           LDA SAUCY
           CMP #$20
           BEQ MISCHK
           DEC SAUCT 
           JMP MISCHK  
FDS38        LDA SAUCY

           CMP #$60
           BEQ MISCHK
           INC SAUCT
MISCHK     LDA SMISY 
           BNE NOMISL   
           LDA ACTFLG
           BEQ NOMISL
           LDA MISAUC
           SEC
           SBC LEVEL
           STA MISAUC
           BCS NOMISL  
           LDA #$50
           STA MISAUC
           LDA HPOS1
           STA HPOS3
           LDA SAUCY
           STA SMISY
           LDA #$00
           STA MISDIR
           LDA HPOS1
           SEC
           SBC CROSSX
           BCC FDS39
           CMP #$0D
           BCC NOMISL
           DEC MISDIR  
           BNE NOMISL
FDS39      EOR #$FF
           CLC
           ADC #$01
           CMP #$0D
           BCC NOMISL
           INC MISDIR
NOMISL     LDA HPOS1
           BNE FDS40
           LDA #$00
           STA SAUCFLG
           STA $D207
FDS40      RTS 
WRNMES     .BYTE $1C,$0F,$0E,$00,$0B,$16,$0F,$1C,$1E
;
SAUCTBL    .WORD SAUCER1
           .WORD SAUCER2
           .WORD SAUCER3
           .WORD SAUCER4
;--------------------------------
; SOUND GENERATOR
; GENERATES SOUNDS FOR GAME
;--------------------------------
SOUND      JSR CONTROL
           INC SNDCNT
           LDA SNDCNT
           CMP #$8 
           BNE SOUND3
           LDA #$00
           STA SNDCNT
           LDA EXPSND
           CMP #$FE
           BEQ SOUND2
           CMP #$FF
           BEQ SOUND3
           INC EXPSND
           LDA EXPSND
           STA $D204
           LDA #$68
           STA $D205
           BNE SOUND3
SOUND2     LDA #$00
           STA $D205
           INC EXPSND
SOUND3     LDA SMISY
           BNE FDS41
           LDA WRNCNT
           BNE FDS43
           LDA SAUCFLG
           BNE FDS42
           LDA #$00
           STA $D206
           STA $D207
FDS43      RTS
FDS41      LDA #$A8
           STA $D207
           LDA SMISY
           STA $D206
           RTS
FDS42      LDA VOLFLG    
           BNE FDS44
           LDA #$1
           STA $D206
           LDA #$60
           ORA VOLUM
           STA $D207
           INC VOLUM
           LDA VOLUM
           CMP #$0F
           BNE FDS43
           INC VOLFLG
           JMP FDS43
FDS44      LDA #$1
           STA $D206
           LDA #$60
           ORA VOLUM
           STA $D207
           DEC VOLUM
           BNE FDS43
           DEC VOLFLG
           JMP FDS43

;--------------------------------
; OBJECT COLLISION HANDLER
;--------------------------------
COLRUT     LDA LIST2+3
           STA IRQVAR1
           LDA LIST2+4
           STA IRQVAR2
           LDA GUNSY,X
           SEC
           SBC #$19
           CLC
           ADC SCRCNT
           LSR
           LSR
           LSR
           STA IRQVAR3
           ASL
           ASL
           CLC
           ADC IRQVAR3
           ASL
           ASL
           ASL
           BCC NOCAT
           INC IRQVAR2
NOCAT      CLC
           ADC IRQVAR1
           STA IRQVAR1
           LDA IRQVAR2
           ADC #$00
           STA IRQVAR2
           LDA GUNSX,X
           SEC
           SBC #$2F
           LSR
           LSR
           CLC
           ADC IRQVAR1
           STA IRQVAR1
           LDA IRQVAR2
           ADC #$00
           STA IRQVAR2
           LDY #$00
           LDA SPACFLG

           BEQ FDS45
           JMP SPACKIL
FDS45      LDA BASER
           BEQ RETRY
           JMP BASKILER
RETRY      LDA (IRQVAR1),Y 
           CMP #$06
           BNE COLRU2
           LDA #$8D
           STA (IRQVAR1),Y
           LDA BSCOR0
           CLC
           ADC #$01
           STA BSCOR0
           LDA BSCOR1
           ADC #$00
           STA BSCOR1
           JMP COLREND
COLRU2     LDX #$00
COLRU3     LDA CHATBL,X
           CMP (IRQVAR1),Y
           BEQ COLRU4
           CMP #$FF
           BEQ COLRU5
           INX
           BNE COLRU3
COLRU5     LDX #$00
FDS46      LDA CHTBL3,X
           CMP (IRQVAR1),Y
           BEQ COLRU8
           CMP #$FF
           BEQ COLREND
           INX
           BNE FDS46

COLRU4     CMP #$22
           BCC FDS48
           CMP #$2A
           BCS FDS48
           INC FUEL
           INC FUEL
           INC FUEL
           LDA LEVEL
           ASL
           CLC
           ADC FUEL
           CMP #$51
           BCC FDS47
           LDA #$50
FDS47      STA FUEL
FDS48      LDA IRQVAR1   
           SEC
           SBC CHTBL2,X
           STA IRQVAR1
           BCS FDS49
           DEC IRQVAR2  
FDS49      LDA #$06
           STA IRQVAR3
           JMP COLRU9
COLRU8     LDA #$05
           STA IRQVAR3
COLRU9     LDX #$00
COLRU6     LDA HITABLE,X
           BEQ COLRU7
           CMP #$FF
           BEQ COLREND
           INX
           INX
           INX
           INX
           INX
           BNE COLRU6
COLRU7     LDA IRQVAR3
           STA HITABLE,X
           LDA IRQVAR1
           STA HITABLE+1,X
           LDA IRQVAR2
           STA HITABLE+2,X
           LDA (IRQVAR1),Y
           STA HITABLE+3,X
           LDA #$00
           STA EXPSND
COLREND    STA $D01E
           JMP RTEND
;--------------------------------
; BRIDGE COLLAPSE ROUTINE
;--------------------------------
BRIDGER    JSR CONTROL
           LDA BRDFLG 
           BNE FDS51
FDS52      RTS  
FDS51      DEC BRDCNT
           BNE FDS52
           LDA BRDPNT
           CMP #$03
           BNE FDS53
           LDA #$00
           STA BRDFLG
           STA BRDCNT
           STA BRDPNT
           RTS
FDS53      ASL
           TAX
           LDA BRDTAB,X
           STA TEMP1
           LDA BRDTAB+1,X
           STA TEMP2
           LDY #$1F
FDS54      LDA (TEMP1),Y
           STA $70A8,Y
           DEY
           BPL FDS54
           INC BRDPNT
           RTS

BRDTAB     .WORD BRIDGE1
           .WORD BRIDGE2
           .WORD BRIDGE3
;--------------------------------
; EXPLODE PLANE ON SCREEN
;--------------------------------
PLANEGON   JSR CONTROL
           LDA #$00
           STA HPOS3
           DEC ACTFLG
           LDA #$20
           STA TEMP1
           LDA #$50
           STA CRUD
PF51       LDX #$0D
PF52       LDA $D20A  
           PHA
           EOR $3490,X
           AND P1-1,X
           STA $3490,X
           PLA
           ROR
           EOR $3590,X
           AND P2-1,X
           STA $3590,X
           DEX
           BNE PF52
           LDA $D20A
           AND #$3F
           STA $D202
           LDA #$8F
           STA $D203
           LDA TEMP1
           PHA
           LDX #$30
PF60       TXA
           PHA
           JSR TRAINER
           JSR KILLER
           JSR ATTACK
           JSR SOUND
           JSR UFODIE
           JSR SPCATK
           JSR KILLER2
           JSR MX
           PLA
           TAX
           DEX
           BNE PF60
           PLA
           STA TEMP1
           DEC TEMP1
           BNE PF51
           DEC TEMP1
PF53       LDA TEMP1
           STA $D202
           LDA TEMP1
           AND #$0F
           BNE PF54
           LDA TEMP1
           CMP #$90
           BCS PF55
           LDX #$10
PF58       LDA $3490,X
           LSR
           STA $3490,X
           LDA $3590,X
           ASL
           STA $3590,X
           DEX
           BNE PF58
           BEQ PF54
PF55       LDX #$0C
PF56       LDA $3690,X
           LSR
           STA $3690,X
           LDA $3790,X
           ASL
           STA $3790,X
           DEX
           BNE PF56
PF54       LDX #$6 
PF59       TXA  
           PHA
           LDA TEMP1
           PHA
           JSR TRAINER
           JSR KILLER
           JSR ATTACK
           JSR SOUND
           JSR UFODIE
           JSR SPCATK
           JSR KILLER2
           JSR MX
           PLA
           STA TEMP1
           PLA
           TAX
           DEX
           BNE PF59
           DEC TEMP1      
           BNE PF53
           LDA #$00
           STA $D203
           DEC SHIPS
           BNE PF57
           JMP ENDGAME
PF57       JSR PMAKER
           LDA #$35
           STA $D202
           LDA #$AF
           STA $D203
           LDY #$00
SUP        DEY
           BNE SUP
           LDA #$84
           STA $D203  
           LDA #$50 
           STA $D202
           INC ACTFLG  
           STA FUEL
           LDA #$00
           STA CRUD
           RTS

;--------------------------------
; MISSLE FIRE ROUTINE
;--------------------------------
MISSLES    INC MISCNT
           LDA MISCNT
           CMP #$10
           BNE ENDMIS
           LDA #$00
           STA MISCNT
           LDA SMISY  
           BEQ ENDMIS
           LDX SMISY
           LDA #$00
PF71       STA $3600,X
           DEX
           BNE PF71
           LDA MISDIR
           BEQ PF72
           BMI PF73
           INC HPOS3
           BEQ CKMISKL
           BNE PF72
PF73       DEC HPOS3 
           BEQ CKMISKL
PF72       INC SMISY
           INC SMISY
           INC SMISY
           INC SMISY
           LDA SMISY
           CMP #$84
           BCS CKMISKL
           LDX SMISY 
           LDY #$04
           LDA #$08
PF74       STA $3600,X 
           DEX 
           DEY
           BNE PF74
           LDA #$FF
           STA $2C2
ENDMIS     RTS 

CKMISKL    LDA HPOS3
           SEC
           SBC CROSSX    
           BEQ FDS72
           BCC FDS71 
           CMP #$0D
           BCS FDS73
           JMP FDS72
FDS71       EOR #$FF
           CMP #$0C
           BCS FDS73
FDS72      JSR PLANEGON  
FDS73      LDA #$00  
           STA HPOS3
           STA SMISY
           RTS
;--------------------------------
; SAUCDEATH!!!!
; CHECK FOR SAUCER HIT
;--------------------------------
UFODIE     JSR CONTROL
           JSR KILUFO
           LDX #$03  
FDS81      LDA HIT1,X
           AND #$3
           BNE FDS82
           DEX
           BPL FDS81
           RTS
FDS82       INC UFOEXP  
           LDA #$00
           STA EXPSND
           LDX #$03
FDS83      LDA HIT1,X  
           AND #$8
           STA HIT1,X  
           DEX
           BPL FDS83
           LDA LEVEL
           CLC
           ADC #$01
           ADC BSCOR1
           STA BSCOR1
           INC HPOS2
           INC HPOS2 
           INC HPOS2 
           INC HPOS2  
           LDA #$0F
           STA $2C0
           STA $2C1
           LDA #$28
           STA UFCNT
           RTS

KILUFO     LDA UFOEXP
           BNE FDS91
FDS92      RTS   
FDS91      LDA #$00
           STA SAUCFLG
           INC UFKFLG
           LDA UFKFLG
           CMP #$60
           BNE FDS96
           LDA #$00
           STA UFKFLG
           LDX #$60
FDS93      LDA $3400,X  
           ASL 
           STA $3400,X  
           LDA $3500,X 
           LSR
           STA $3500,X 
           DEX
           BNE FDS93
           DEC UFCNT   
           BNE FDS96
           LDA #$00
           STA UFOEXP
           LDX #$60
FDS94       STA $3400,X
           STA $3500,X
           DEX
           BNE FDS94
           LDX #$03
FDS95      LDA HIT1,X
           AND #$08
           STA HIT1,X
           DEX
           BPL FDS95
FDS96      PLA  
           PLA
           RTS

;--------------------------------
; STAR ROUTINE
;--------------------------------
STARS      INC STRCNT
           LDA STRCNT
           CMP #$02
           BEQ FDS101
           RTS
FDS101     LDA #$00
           STA STRCNT
           LDX STRFAS
           STA $73F8,X
           INC STRFAS
           LDA STRFAS
           CMP #$08
           BNE FDS102
           LDA #$00
           STA STRFAS
           JSR STARPLOT
FDS102     LDX STRFAS    
           LDA #$0C
           STA $73F8,X  
           RTS

;--------------------------------
; STARPLOT SUBROUTINES
;--------------------------------
STARPLOT   LDX #$0D
PF711        LDY STARY,X
           LDA YLOW,Y
           STA IRQVAR1
           LDA YHI,Y
           STA IRQVAR2
           LDY STARX,X
           LDA (IRQVAR1),Y
           BEQ PF721
           CMP #$7F
           BEQ PF721
           CMP #$FF
           BNE PF751
PF721      LDA #$00
           STA (IRQVAR1),Y
PF751      INC STARY,X
           LDA STARY,X
           CMP #$15
           BNE PF761
           LDA #$00
           STA STARY,X
PF761      TAY
           LDA YLOW,Y
           STA IRQVAR1
           LDA YHI,Y
           STA IRQVAR2
           LDY STARX,X
           LDA (IRQVAR1),Y
           BNE PF771
           LDA #$7F
           BIT $D20A
           BMI PF781
           LDA #$FF
PF781      STA (IRQVAR1),Y  
PF771      DEX   
           BPL PF711
           RTS

STARX      .BYTE $02,$05,$02,$0A,$0C,$0E,$13,$15,$19
           .BYTE $1D,$20,$24,$26,$28
STARY      .BYTE $01,$0A,$07,$00,$03,$11,$02,$05,$08
           .BYTE $13,$04,$00,$14,$04
YLOW       .BYTE $00,$28,$50,$78,$A0,$C8,$F0,$18  
           .BYTE $40,$68,$90,$B8,$E0,$08,$30,$58
           .BYTE $80,$A8,$D0,$F8,$20
YHI        .BYTE $40,$40,$40,$40,$40,$40,$40,$41
           .BYTE $41,$41,$41,$41,$41,$42,$42,$42
           .BYTE $42,$42,$42,$42,$43
;--------------------------------
; NIGHT 3
; SPACE ROUTINES
;--------------------------------
; SPACE ATTACKER ROUTINES
; ATTACKS PLAYER WITH ANDROID
; SHIPS
;--------------------------------
SPCATK     LDA SPACFLG
           BNE PF82
PF81       RTS
PF82       LDA MUSCNT
           CMP #$1A
           BEQ PF83
           CMP #$19
           BNE PF85
           LDA LEVEL
           CLC
           ADC #$01
           STA WAVES
PF85       LDA #$00   
           STA MUSDEL
           INC MUSCNT
           RTS
PF83       LDA #$2
           ADC MUSDEL
           STA MUSDEL
           CMP #$F0
           BCS PF84
           RTS
PF84       LDA #$00
           STA MUSDEL
           LDA WAVES
           BNE PF89
           INC MIKEY2
           LDA MIKEY2
           CMP #$30
           BNE PF87
           DEC SPACFLG
PF87       RTS
PF89       LDA FLYFLG  
           BNE ALLDEAD
;--------------------------------
; IF FLYFLAG NOT SET THEN WE
; INITIALIZE STRING PICK A
; RANDOM PATH AND INITIALIZE
; NUMBER OF PLANES TO KILL
;--------------------------------
           INC FLYFLG
           LDA $D20A
           AND #$07
           CMP #$05
           BCC PF86
           LSR
PF86       ASL
           STA PATHPNT
           LDA #$00
           STA PSTRING
           STA PSTATUS
           LDX #$5F
PF88       LDA SPACL1,X
           STA $7380,X
           DEX
           BPL PF88
;--------------------------------
; PLOT SHIPS
;--------------------------------
ALLDEAD    LDA PSTATUS
           CMP #$FF
           BNE PF91
           DEC WAVES
           BNE PF92
           RTS
PF92       DEC FLYFLG
           RTS
PF91       LDX PATHPNT
           LDA PATHX,X
           STA TEMP1
           LDA PATHX+1,X
           STA TEMP2
           LDA PATHY,X
           STA TEMP3
           LDA PATHY+1,X
           STA TEMP4
           LDA PSHIP,X
           STA TEMP5
           LDA PSHIP+1,X
           STA TEMP6
           JSR ERASEPLN   ; PUTPLN lable was not used
           LDY PSTRING
           LDA (TEMP1),Y
           CMP #$FF
           BEQ PF94
           LDA (TEMP3),Y
           TAX
           LDA (TEMP5),Y
           PHA
           LDA (TEMP1),Y
           TAY
           LDA YLOW,X
           STA TEMP7
           LDA YHI,X
           STA TEMP8
           PLA
           PHA
           CMP #$74
           BNE PF93
           LDA SMISY
           ORA WRNCNT 
           ORA SAUCFLG
           BNE PF93
           TXA
           ASL
           ASL
           ASL
           CLC
           ADC #$19
           STA SMISY
           TYA
           ASL
           ASL
           CLC
           ADC #$2F
           STA HPOS3
           LDA #$00
           STA MISDIR
PF93       PLA
           STA (TEMP7),Y
           INY
           CLC
           ADC #$01
           STA (TEMP7),Y
           ADC #$01
           PHA
           TYA
           CLC
           ADC #$27
           TAY
           PLA
           STA (TEMP7),Y
           INY
           CLC
           ADC #$01
           STA (TEMP7),Y
           INC PSTRING
           RTS  
PF94       DEC FLYFLG
           RTS
;--------------------------------
; ERASE PLANES
;--------------------------------
ERASEPLN   LDY PSTRING
           BEQ FDS111
           DEY
           LDA (TEMP1),Y
           CMP #$FF
           BEQ FDS111
           LDA (TEMP3),Y
           TAX
           LDA (TEMP1),Y
           TAY
           LDA YLOW,X
           STA TEMP7
           LDA YHI,X
           STA TEMP8
           LDA #$00
           STA (TEMP7),Y
           INY
           STA (TEMP7),Y
           TYA
           CLC
           ADC #$27
           TAY
           LDA #$00
           STA (TEMP7),Y
           INY
           STA (TEMP7),Y
FDS111     RTS
;--------------------------------
; SPACE COLLISION HANDLER
;--------------------------------
SPACKIL    LDA #$00  
           TAY
           TAX
PF101      LDA PLANCHR,X
           CMP #$FF
           BEQ PF106
           CMP (IRQVAR1),Y
           BEQ PF102
           INX
PF106      CMP #$FF 
           BNE PF101
           JMP COLREND
PF102      LDA IRQVAR1
           SEC
           SBC PLANDIF,X 
           STA IRQVAR1
           BCS PF103
           DEC IRQVAR2
PF103      LDX #$00
PF104      LDA PLANFIND,X 
           BEQ PF105
           INX
           CMP #$FF
           BNE PF104
           JMP COLREND
PF105      LDA #$5 
           STA PLANFIND,X
           TXA
           ASL
           TAX
           LDA IRQVAR1
           STA PLANTIME,X
           LDA IRQVAR2
           STA PLANTIME+1,X
           LDA #$00
           STA EXPSND
           LDA #$FF
           STA PSTATUS
           LDX LEVEL
           INX
PF107      LDA BSCOR0
           CLC 
           ADC #$64 
           STA BSCOR0
           LDA BSCOR1  
           ADC #$00
           STA BSCOR1
           DEX
           BNE PF107
           JMP COLREND


PLANCHR    .BYTE $70,$71,$72,$73,$74,$75,$76,$77
           .BYTE $78,$79,$7A,$7B,$FF
PLANDIF    .BYTE $00,$01,$28,$29,$00,$01,$28,$29
           .BYTE $00,$01,$28,$29,$FF
PLANFIND   .BYTE $00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $FF
PLANTIME   .BYTE $00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00
;--------------------------------
; KILLER2 KILLS SPACE CHARACTERS
;--------------------------------
KILLER2    LDA SPACFLG
           BNE STYX
           RTS
STYX       INC MIKEY
           LDA MIKEY
           CMP #$20
           BEQ RJS1
           RTS
RJS1       LDA #$00
           STA MIKEY
           TAX
RJS2       LDA PLANFIND,X
           CMP #$FF
           BNE RJS3
           RTS
RJS3       CMP #$00
           BNE RJS4
           INX
           BNE RJS2
RJS4       STX TEMP3
           DEC PLANFIND,X
           TXA
           ASL
           TAX
           LDA PLANTIME,X
           STA TEMP1
           LDA PLANTIME+1,X
           STA TEMP2
           LDX TEMP3
           LDA PLANFIND,X  
           BNE RJS5
           BEQ RJS6
RJS5       LDA #$30
RJS6       LDY #$00
           STA (TEMP1),Y
           INY
           STA (TEMP1),Y
           LDY #$28
           STA (TEMP1),Y
           INY
           STA (TEMP1),Y
           INX
           JMP RJS2

;--------------------------------
; MX SHOOTS MISSLES
;--------------------------------
MX         INC MXDELAY
           LDA MXDELAY
           CMP #$10
           BEQ RJS14
           RTS
RJS14      LDA #$00
           STA MXDELAY
           JSR MXKILL 
           LDA MXFLAG
           BNE MX2
           LDA LIST2+3
           CLC
           ADC #$40
           STA TEMP1
           LDA LIST2+4
           STA TEMP2
           BCC RJS11
           INC TEMP2
RJS11      INC TEMP2
           LDY #$27
RJS12      LDA (TEMP1),Y
           CMP #$E6
           BEQ RJS13
           DEY
           BPL RJS12
           RTS
RJS13      TYA
           ASL
           ASL
           ADC #$2E
           STA HPOS4
           INC MXFLAG
           LDA #$00
           STA MXSCRL
           RTS

MX2        LDA MXSCRL 
           CMP #$0F
           BCS MX3 
           LDA #$0E
           SEC 
           SBC MXSCRL
           TAY 
           LDX #$FF
FDS121         INX
           LDA MXDAT,X  
           STA $374B,Y
           INY
           CPX MXSCRL
           BNE FDS121
           INC MXSCRL
           RTS

MX3        INC $2C3
           LDX #$00
FDS131     LDA $3700,X  
           STA $36FF,X  
           INX
           CPX #$60
           BNE FDS131
           INC MXSCRL
           LDA MXSCRL
           CMP #$FF
           BEQ FDS132
           STA $D202
           LDA #$8F
           STA $D203
           RTS
FDS132     DEC MXFLAG
           LDA #$50
           STA $D202
           LDA #$88
           STA $D203
           RTS
;--------------------------------
; MXKILL CHECK FOR MX DEATH!
;--------------------------------
MXKILL     LDA MXDEATH
           BNE RJS24
           LDX #$03
RJS21      LDA HIT1,X
           AND #$08
           BNE RJS22
           DEX
           BPL RJS21
           RTS
RJS22      LDX #$03
RJS23      LDA HIT1,X
           AND #$07
           STA HIT1,X
           DEX
           BPL RJS23
           INC MXDEATH
           LDA #$00
           STA EXPSND 
           STA MXFLAG
           LDA #$28
           STA MCNT
           LDA LEVEL
           CLC
           ADC #$01
           ADC BSCOR1
           STA BSCOR1
           RTS
;--------------------------------
; KILL MISSLE!
;--------------------------------
RJS24      LDA #$50
           STA $D202
           LDA #$88
           STA $D203
           LDX #$60  
RJS25      LDA $3700,X
           BNE RJS29
           DEX
           BNE RJS25
RJS29      LDY #$0E
RJS28      DEX
           LDA $D20A
           AND MXDAT,Y
           STA $3700,X  
           DEY
           BPL RJS28
           DEC MCNT
           BNE RJS27
           LDA #$00
           STA MXDEATH
           LDX #$60
RJS26      STA $3700,X
           DEX
           BPL RJS26
RJS27      PLA
           PLA
           RTS

MXDAT      .BYTE $10,$10,$10,$38,$38,$38,$38,$38 
           .BYTE $38,$38,$38,$7C,$7C,$44,$44
;--------------------------------
; KIL BASER
;--------------------------------
EXPLOB     LDA BASDEAD
           CMP #$03
           BEQ RJS31
           RTS
RJS31      LDA #$00
           STA MOVFLG
           STA ACTFLG
           LDA #$54
           STA $2C8 
           LDA #$00
           STA HPOS1
           STA HPOS2
           STA HPOS3
           STA HPOS4
           STA $D205
           STA $D207
           LDA #$8F
           STA $D201
           LDA #$CF
           STA $D203
           LDX #$00
           TXA
RJS32      STA $48C0,X
           STA $49C0,X
           STA $4AC0,X
           INX
           INX
           BNE RJS32
           LDA #$CF 
           STA $D201
           LDA #$8F
           STA $D203
           LDX #$FF
RJS33      STX $D200
           TXA
           PHA
           EOR #$FF
           STA $D202
           TXA
           AND #$0F
           BNE RJS37
           LDX #$15
           LDA #$C0
           STA TEMP1
           LDA #$48
           STA TEMP2
RJS34      LDY #$00
RJS36      LDA (TEMP1),Y
           DEY
           STA (TEMP1),Y
           INY
           INY
           CPY #$15
           BCC RJS36
           LDY #$28 
RJS35      LDA (TEMP1),Y
           INY
           STA (TEMP1),Y
           DEY
           DEY
           CPY #$14
           BCS RJS35
           LDA TEMP1
           CLC
           ADC #$28
           STA TEMP1
           LDA TEMP2
           ADC #$00
           STA TEMP2
           DEX
           BNE RJS34
           LDX #$28
           LDY #$00
RJS38      LDA $49C0,Y
           STA $48C0,Y
           LDA $4AC0,Y
           STA $49C0,Y
           LDA $4BC0,Y
           STA $4AC0,Y
           LDA #$00
           STA $4BC0,Y
           INY
           BNE RJS38
RJS37      LDY #$00
           LDX #$05
RJS39      DEY
           BNE RJS39
           DEX
           BNE RJS39
           PLA
           TAX
           DEX
           BNE RJS33
           LDA #$00
           STA $D201
           STA $D203
           STA $2C8
           LDA #$2C
           STA $2F4

BONUS      LDX #$0E
RJS41      LDA BONSTR,X
           STA $4995,X
           DEX
           BPL RJS41
           LDX #$0F 
RJS42      LDY MUSDATA,X
           STY $D200
           DEY
           STY $D202
           LDA #$AF
           STA $D201
           STA $D203
           INY
           CPY #$00
           BNE RJS44
           STY $D201
           STY $D203
RJS44      LDA MUSDLY,X
           LDY #$00
RJS43      DEY
           BNE RJS43
           SEC
           SBC #$01
           BNE RJS43
           DEX
           BPL RJS42
           LDX #$10
           JSR DELAY
           LDA #$01
           STA $49EC
           STA $49ED
           STA $49EE
           LDY LEVEL
           INY
           LDX #$02 
RJS45      STX $49EB
           LDA BSCOR0
           CLC
           ADC #$E8
           STA BSCOR0
           BCC RJS48
           INC BSCOR1
RJS48      INC BSCOR1
           INC BSCOR1
           INC BSCOR1
           TXA
           PHA
           TYA
           PHA
           JSR BOOP
           PLA
           TAY
           PLA
           TAX
           INX
           DEY
           BNE RJS45
           LDX #$50
           JSR DLONG
           LDA LEVEL
           CMP #$04
           BNE RJS46
           INC SHIPS
RJS46      CMP #$05
           BEQ RJS47
           INC LEVEL
RJS47      LDA #$58
           STA LIST2+3
           LDA #$4C
           STA LIST2+4
           LDA #$50
           STA FUEL
           JSR MAPFIL
           JSR INITVAR
           LDX #$0C
           JMP CURRAN

BONSTR     .BYTE $0F,$18,$0F,$17,$23,$00,$0E,$0F,$1D,$1E,$1C,$19,$23,$0F,$0E
MUSDATA    .BYTE $00,$58,$00,$68,$00,$58,$00,$46,$00,$68,$00,$73,$00,$75,$00,$80
MUSDLY     .BYTE $0A,$64,$0A,$7D,$0A,$7D,$0A,$7D,$0A,$7D,$0A,$64,$0A,$AF,$0A,$64
;--------------------------------
; BASKILER BAS CHARACTER KILL ROUTINES!
;--------------------------------
BASKILER   LDA (IRQVAR1),Y
           CMP #$3F
           BEQ FDS142
           CMP #$40
           BEQ FDS141
           JMP RETRY
FDS141     LDA IRQVAR1
           SEC
           SBC #$01
           STA IRQVAR1
           BCS FDS142
           DEC IRQVAR2
FDS142     LDA #$B0
           STA (IRQVAR1),Y
           INY
           STA (IRQVAR1),Y
           LDA #$00 
           STA EXPSND
           INC BASDEAD
           LDA BASDEAD
           SEC
           SBC #$01
           ASL
           TAX
           LDA IRQVAR1
           STA BASOLD,X
           LDA IRQVAR2
           STA BASOLD+1,X
           JMP COLREND

BASOLD     .BYTE $00,$00
           .BYTE $00,$00
           .BYTE $00,$00
;--------------------------------
; ENDGAME
;--------------------------------
ENDGAME    LDA #$00
           LDA #$00
           STA $D201
           STA $D203
           STA $D205
           STA $D207
           LDA #$3D 
           STA HPOS1
           LDA #$5D
           STA HPOS2
           LDA #$85
           STA HPOS3
           LDA #$AD
           STA HPOS4
           LDX #$00
           TXA
FDS143     STA $3400,X
           STA $3500,X
           STA $3600,X
           STA $3700,X
           DEX
           BNE FDS143
           LDX #$5F
FDS144     LDA GDAT,X
           STA $3420,X
           LDA ADAT,X
           STA $3520,X 
           LDA MDAT,X  
           STA $3620,X
           LDA EDAT,X
           STA $3720,X
           DEX
           BPL FDS144
           LDA #$03
           STA $D008
           STA $D009
           STA $D00A
           STA $D00B
           LDA #$94
           STA $2C0
           STA $2C1
           STA $2C2
           STA $2C3
           LDA SCORE1
           STA OLSCORE1
           LDA SCORE2
           STA OLSCORE2
           LDA SCORE3
           STA OLSCORE3
HSER       LDA SCORE3
           CMP HISCORE3
           BCC NOHI
           BNE HIER
           LDA SCORE2
           CMP HISCORE2
           BCC NOHI 
           BNE HIER
           LDA SCORE1
           CMP HISCORE1
           BCC NOHI 
HIER       LDA SCORE1
           STA HISCORE1
           LDA SCORE2
           STA HISCORE2
           LDA SCORE3
           STA HISCORE3
NOHI       LDX #$30
           JSR DLONG
           LDY TEMP5
           JMP WARMSTART
;--------------------------------
; GAME OVER DATA
;--------------------------------
GDAT      .BYTE $FC,$FC,$FC,$FC,$FC,$80,$80,$80
          .BYTE $80,$80,$80,$80,$80,$80,$80,$80
          .BYTE $80,$80,$80,$80,$80,$80,$80,$80
          .BYTE $9C,$9C,$9C,$9C,$9C,$84,$84,$84
          .BYTE $84,$84,$84,$84,$84,$84,$FC,$FC
          .BYTE $FC,$FC,$FC,$00,$00,$00,$00,$00
          .BYTE $00,$00,$00,$00,$00,$FC,$FC,$FC
          .BYTE $FC,$84,$84,$84,$84,$84,$84,$84
          .BYTE $84,$84,$84,$84,$84,$84,$84,$84
          .BYTE $84,$84,$84,$84,$84,$84,$84,$84
          .BYTE $84,$84,$84,$84,$84,$84,$84,$84
          .BYTE $84,$84,$84,$84,$FC,$FC,$FC,$FC
ADAT      .BYTE $18,$18,$18,$18,$24,$24,$24,$24
          .BYTE $42,$42,$42,$42,$42,$42,$42,$42
          .BYTE $42,$42,$42,$42,$42,$42,$42,$7E
          .BYTE $7E,$7E,$7E,$42,$42,$42,$42,$42
          .BYTE $42,$42,$42,$42,$42,$42,$42,$42
          .BYTE $42,$42,$42,$00,$00,$00,$00,$00
          .BYTE $00,$00,$00,$00,$00,$81,$81,$81
          .BYTE $81,$81,$81,$81,$81,$81,$81,$81
          .BYTE $81,$81,$81,$81,$81,$42,$42,$42
          .BYTE $42,$42,$42,$42,$42,$42,$24,$24
          .BYTE $24,$24,$24,$24,$24,$24,$24,$24
          .BYTE $18,$18,$18,$18,$18,$18,$18,$00
MDAT      .BYTE $C3,$C3,$C3,$C3,$A5,$A5,$A5,$A5
          .BYTE $99,$99,$99,$99,$99,$99,$99,$99
          .BYTE $99,$99,$99,$99,$99,$99,$99,$99
          .BYTE $99,$99,$99,$99,$99,$99,$81,$81
          .BYTE $81,$81,$81,$81,$81,$81,$81,$81
          .BYTE $81,$81,$81,$00,$00,$00,$00,$00
          .BYTE $00,$00,$00,$00,$00,$FC,$FC,$FC
          .BYTE $FC,$80,$80,$80,$80,$80,$80,$80
          .BYTE $80,$80,$80,$80,$80,$80,$80,$F0
          .BYTE $F0,$F0,$F0,$80,$80,$80,$80,$80
          .BYTE $80,$80,$80,$80,$80,$80,$80,$80
          .BYTE $80,$80,$80,$80,$FC,$FC,$FC,$FC
EDAT      .BYTE $FC,$FC,$FC,$FC,$80,$80,$80,$80
          .BYTE $80,$80,$80,$80,$80,$80,$80,$80
          .BYTE $80,$80,$F0,$F0,$F0,$F0,$80,$80
          .BYTE $80,$80,$80,$80,$80,$80,$80,$80
          .BYTE $80,$80,$80,$80,$80,$80,$80,$FC
          .BYTE $FC,$FC,$FC,$00,$00,$00,$00,$00
          .BYTE $00,$00,$00,$00,$00,$F8,$F8,$F8
          .BYTE $F8,$F8,$88,$88,$88,$88,$88,$88
          .BYTE $88,$88,$88,$88,$88,$F8,$F8,$F8
          .BYTE $F8,$F8,$F8,$A0,$A0,$A0,$A0,$A0
          .BYTE $A0,$A0,$90,$90,$90,$90,$90,$90
          .BYTE $88,$88,$88,$88,$88,$84,$84,$84
          .BYTE $84,$00,$00,$00,$00,$00,$00,$00
;--------------------------------
; PAUSER ROUTINE CHECK FOR
; PAUSE AND GAME RESTART!
;--------------------------------
PAUSER     LDA CONSOL
           ROR
           BCS FDS151
           JMP WARMSTART
FDS151     ROR
           ROR
           BCC FDS152
           RTS
FDS152     LDA CONSOL
           AND #$04
           BEQ FDS152
           LDA MOVFLG
           PHA
           LDA ACTFLG
           PHA
           LDA #$00
           STA MOVFLG
           STA ACTFLG
           STA $D201
           STA $D203
           STA $D205
           STA $D207
           LDA #$FF
           STA $2FC
FDS153     LDA $2FC
           CMP #$FF
           BEQ FDS153
           PLA
           STA ACTFLG
           PLA
           STA MOVFLG
           LDA #50
           STA $D202
           LDA #$83
           STA $D203
           RTS
;--------------------------------
; FIREPOWER! FIRE FROM BASE AND
; FIRE FROM TANKS!
;--------------------------------
FIREPOWER  INC CNTFIRE
           BPL RJS55 
           LDA #$00
           STA CNTFIRE
           LDA SMISY
           ORA WRNCNT
           ORA SAUCFLG
           BEQ RJS51
RJS55      RTS 
RJS51      LDA LIST2+3
           CLC
           ADC #$C8
           STA TEMP1
           LDA LIST2+4
           ADC #$00
           STA TEMP2
           LDX #$08 
RJS52      LDY #$27
RJS53      LDA (TEMP1),Y
           CMP #$10
           BEQ RJS54
           CMP #$40
           BEQ RJS54
           DEY
           BPL RJS53
           LDA TEMP1
           CLC
           ADC #$28
           STA TEMP1
           LDA TEMP2
           ADC #$00
           STA TEMP2
           DEX
           BNE RJS53
           RTS
RJS54      DEY
           TYA
           ASL
           ASL
           CLC
           ADC #$2F
           STA HPOS3
           TXA
           EOR #$08 
           CLC
           ADC #$05
           ASL
           ASL
           ASL
           CLC
           ADC #$14 
           STA SMISY
           LDA #$00
           STA MISDIR
           RTS

;--------------------------------
; SUBROUTINES FOR NIGHTRAIDERS
;--------------------------------
MAPFIL     STA TEMP2
           CMP #$60
           BEQ MAPCOL2
           LDA #$8D
           STA $2C4
           LDA #$94
           STA $2C5
           LDA #$CA
           STA $2C6
           LDA #$48
           STA $2C7
           BNE MAPMOVER  
MAPCOL2    LDA #$28
           STA $2C4
           LDA #$CA
           STA $2C5
           LDA #$94
           STA $2C6
           LDA #$48
           STA $2C7
MAPMOVER   LDA #$40  
           STA TEMP4
           LDA #$00   
           STA TEMP1
           STA TEMP3
           TAY
MAPFIL2    LDA (TEMP1),Y
           STA (TEMP3),Y
           INC TEMP1
           INC TEMP3
           BNE MAPFIL2
           INC TEMP2
           INC TEMP4
           LDA TEMP4
           CMP #$50
           BNE MAPFIL2
           LDX #$00
           TXA
FDS161     STA $4000,X
           STA $4100,X
           STA $4200,X
           DEX
           BNE FDS161
           LDX #$6F
FDS162     STA $4300,X
           DEX
           BNE FDS162
           RTS
;--------------------------------
; SETUP GUAGE SCREEN
;--------------------------------
SETSCREEN  LDX #$4F 
SETS2      LDA BSCR-1,X
           STA $3E5F,X
           DEX
           BNE SETS2
           RTS

BSCR       .BYTE $00,$1D,$0D,$19,$1C,$0F,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$1D,$12,$13,$1A,$1D,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$90,$9F,$8F,$96,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00
;--------------------------------
; INITIALIZE GAME VARIABLES
;--------------------------------
INIT   LDA #$00
ILOOP  STA $0,X
       INX
       BNE ILOOP
       STA $D40E
       STA $D008
       STA $D009
       STA $D00A
       STA $D00B
       LDA #RTEND&255
       STA VBLK  
       STA COLLAD
       LDA #RTEND/255
       STA VBLK+1   
       STA COLLAD+1
       LDA #$40      ; NMIEN_VBI
       STA NMIEN     ; activate vertical blank interrupt
       JSR CLRMIS
       LDX #$14
ILOOP1 LDA #$00
       STA $3FBF,X  
       DEX
       BNE ILOOP1
       STA GRACTL
       STA DMACTL
       LDA #$30
       STA PMBASE
       LDX #$09
       LDA #$00
ILOOP2 STA CPLAY0-1,X
       DEX
       BNE ILOOP2
       LDX #$07
ILOOP3 STA $D200,X
       DEX
       DEX
       CPX #$FF
       BNE ILOOP3
       LDX #$05
CLRGUN STA GUNSX,X
       DEX
       BPL CLRGUN
       LDA #$40
       STA TPOINT
       LDA #$03
       STA $232
       STA $D20F
       LDA #$00
       STA $D208
       LDA #$03
       STA $D01E
       JSR INITVAR
       RTS
;--------------------------------
; MAKE PLANE
;--------------------------------
PMAKER LDX #$0C
PM1    LDA P1-1,X  
       STA $3490,X
       LDA P2-1,X
       STA $3590,X
       LDA P3-1,X
       STA $3690,X
       LDA P4-1,X
       STA $3790,X
       DEX
       BNE PM1
       LDA #$82
       STA CROSSX
       RTS
;--------------------------------
; RTEND RESTORE REGISTERS
; AFTER INTERRUPT
;--------------------------------
RTEND  PLA
       TAY
       PLA
       TAX
       PLA
NOINT  RTI
;--------------------------------
; CLEAR PLAYER MISSLE AREA
;--------------------------------
CLRMIS LDA #$00
       STA TEMP1
       LDA #$30
       STA TEMP2
       LDY #$00
CLROP  LDA #$00  
CLROP2 STA (TEMP1),Y
       INY
       BNE CLROP2
       INC TEMP2
       LDA TEMP2
       CMP #$38
       BNE CLROP
       RTS
;--------------------------------
; LONG DELAY ROUTINE
;--------------------------------
DLONG  STX TEMP2
       JSR DELAY
       LDX TEMP2
       DEX
       BNE DLONG
       RTS
;--------------------------------
; DELAY ROUTINE
;--------------------------------
DELAY  LDX #$10
DELAY1 LDY #$FF
DELAY2 DEY
       BNE DELAY2
       DEX
       BNE DELAY1
       RTS
;--------------------------------
; PRINT ROUTINE
;--------------------------------

PRINT  STY TEMP2
       LDA #WORDS&255
       STA TEMP3
       LDA #WORDS/255
       STA TEMP4
       LDY #$00
PRINT1 LDA (TEMP3),Y
       BEQ PRINT3
PRINT2 INY
       JMP PRINT1
PRINT3 DEX
       BNE PRINT2
       INY
       TYA
       CLC
       ADC TEMP3
       STA TEMP3
       BCC PRINT4
       INC TEMP4
PRINT4 LDA #SCREEN&255
       STA TEMP5
       LDA #SCREEN/255
       STA TEMP6
       LDX TEMP1
       BEQ LOOSE
PRINT5 LDA TEMP5
       CLC
       ADC #$28
       STA TEMP5
       BCC PRINT6
       INC TEMP6  
PRINT6 DEX 
       BNE PRINT5
LOOSE  LDY #$00 
PRINT7 LDA (TEMP3),Y
       BEQ PRINT9
       CMP #$20
       BNE PRINT8
       LDA #$36
PRINT8 SEC
       SBC #$36
       ORA TEMP7
       TAX
       TYA
       PHA
       LDY TEMP2
       TXA
       STA (TEMP5),Y
       INC TEMP2
       PLA
       TAY
       INY
       JMP PRINT7
PRINT9 RTS

;--------------------------------
; DISPLAY LISTS
; Note: Display list interrupts interrupt the main processor so it can make a change
; to a color register for example or a sprite location at the specific momemnt in time
; where he crt scan line is scanning. See https://www.atariarchives.org/mapping/appendix8.php
;--------------------------------

;--------------------------------
; Second Display list instructios for game
;--------------------------------
LIST2  .BYTE $70               ; 8 Blank Lines
       .BYTE $F0               ; Text Mode 0 40 pixels per line 40 bytes per line 8 scan lines + Horiz Scroll
                               ; + Vertical Scroll and Enable Display List Interrupt + Load Mem Scan
       .BYTE $64               ; Text Mode 40 pixels per line 40 bytes per line * 8 scan lines + Load Mem scan + Horiz Scroll                         
       .BYTE SCREEN&255        ; Low Address of Memory
       .BYTE SCREEN/255        ; High Address of Memory
       .BYTE $24,$24,$24,$24,$24       ; Text Mode 40 pixels per line 40 bytes per line * 8 scan lines + vertical scroll * 12
       .BYTE $24,$24,$24,$24,$24,$24,$24
       .BYTE $A4                       ; Same Text mode plus displa list interrupt + vertical scroll
       .BYTE $24,$24,$24,$24,$24       ; Text Mode 40 pixels per line 40 bytes per line * 8 scan lines + vertical scroll * 5
       .BYTE $04               ; Same text mode no scroll
       .BYTE $A0               ; Display list interrupt vertical scroll 1 blank line
       .BYTE $45               ; Text Mode 40 pixels per line 40 bytes per line 16 scan lines + Load Memory scan at 3E60 
       .BYTE $60               ; Low Byte of Memory address (3E60 is where he score line data is )
       .BYTE $3E               ; Hi Byte of Memory address
       .BYTE $05               ; Text Mode 40 pixels per line 40 bytes per line 16 scan lines
       .BYTE $20               ; 1 blank line + Vertical Scroll
       .BYTE $4A               ; Graphics mode 80 pixels per line 20 bytes per line 4 scan lines + load mem scan from 413f
       .BYTE $41               ; Low Byte of Memory address
       .BYTE $3F               ; Hi Byte of Memory address
       .BYTE $41               ; Jump and wait for vertical blank Tells ANTIC Processor where to fetch next instruction.
;       .DA #LIST2    ; .DA #expression (one byte, LSB of expression)
;       .DA /LIST2    ; .DA /expression (one byte, MSB of expression)
       .BYTE LIST2&255        ; Low byte of display list address
       .BYTE LIST2/255        ; High byte of display list address 
       ;.WORD LIST2    ; Stores words in memory at the current memory address in native format (LSB/MSB).

;--------------------------------
; First Display list instructios for intro screen 
;--------------------------------
LIST1  .BYTE $70               ; 8 Blank Lines
       .BYTE $60               ; 7 Blank Lines
       .BYTE $90               ; 1 Blank Line + Load Memory Scan + Horiz Scroll
       .BYTE $4F               ; Graphic Mode 8 320 pixels per line 40 bytes per line 1 scan line + Horiz Scroll
;       .DA #NIGHTDAT
;       .DA /NIGHTDAT
        .WORD NIGHTDAT    ; Stores words in memory at the current memory address in native format (LSB/MSB).
       .BYTE $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F ; (Graphic Mode 8 320 pixels per line 40 bytes per line 1 scan line ) * 14 lines
       .BYTE $0F,$0F,$0F,$0F,$0F,$0F
       .BYTE $30               ; 4 blank lines
       .BYTE $44               ; Graphics Mode 4 80 pixels per line 10 bytes per line 4 scan lines
                               ; + Load Memory Scan from memory location 4000H = SCREEN
       .BYTE SCREEN&255        ; Low Address of Memory
       .BYTE SCREEN/255        ; High Address of Memory
       .BYTE $D0               ; ????
       .BYTE $05,$05           ; (text mode 16 scan lines 40 pixels per line 40 bytes per line) * 2
       .BYTE $04,$04,$04,$04,$04,$04     ; (text mode 8 scan lines 20 pixels per line 20 bytes per line) * 6
       .BYTE $84               ; same text mode  + Display List Interrupt
       .BYTE $04               ; (text mode 8 scan lines 20 pixels per line 20 bytes per line) * 9
       .BYTE $04,$04,$04,$04,$04,$04,$04,$04 
       .BYTE $41               ; Jump and wait for vertical blank Tells ANTIC Processor where to fetch next instruction.
;       .DA #LIST1   ; .DA #expression (one byte, LSB of expression) 
;       .DA /LIST1   ; .DA /expression (one byte, MSB of expression)
       .BYTE LIST1&255
       .BYTE LIST1/255
       ;.WORD LIST1   ; Stores words in memory at the current memory address in native format (LSB/MSB).

;--------------------------------
; DATA TABLE FOR HI-RES NIGHTRAIDER!
;--------------------------------
NIGHTDAT   .BYTE $00,$00,$00,$00,$00,$00,$00,$00                
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$60,$60,$7E,$03,$FC,$18
           .BYTE $18,$7F,$E1,$FF,$00,$F0,$07,$E0,$7F,$C1,$FF,$87,$FC,$0F,$F0,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$C0,$C0,$30,$0E,$1C,$30,$30,$CC,$C3,$07,$03,$F0,$03,$00
           .BYTE $31,$C0,$C3,$0C,$1C,$38,$70,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$C1,$80,$60,$18,$18,$60
           .BYTE $61,$99,$86,$06,$0E,$70,$06,$00,$61,$81,$86,$18,$18,$60,$60,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$03,$C3,$00,$C0,$30,$00,$C0,$C0,$30,$0C,$0C,$38,$70,$0C,$00
           .BYTE $C3,$03,$00,$30,$30,$C0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$C6,$01,$80,$60,$01,$81
           .BYTE $80,$60,$18,$38,$60,$60,$18,$01,$86,$06,$00,$60,$E1,$C0,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$0D,$CC,$03,$00,$C0,$03,$FF,$00,$C0,$3F,$E0,$C0,$C0,$30,$03
           .BYTE $0C,$0F,$00,$FF,$81,$FC,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$19,$D8,$06,$01,$8F,$87,$FE
           .BYTE $01,$80,$7F,$81,$81,$80,$60,$06,$18,$1E,$01,$FE,$01,$FC,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$31,$F0,$0C,$03,$1F,$0C,$0C,$03,$00,$DC,$03,$FF,$00,$C0,$0C
           .BYTE $30,$30,$03,$70,$00,$1C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$61,$E0,$18,$06,$06,$18,$18
           .BYTE $06,$01,$9C,$07,$FE,$01,$80,$18,$60,$60,$06,$70,$00,$18,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$C1,$C0,$30,$0C,$0C,$30,$30,$0C,$03,$1C,$0C,$0C,$03,$00,$30
           .BYTE $C0,$C3,$0C,$70,$30,$30,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$01,$81,$80,$60,$1C,$18,$60,$60
           .BYTE $18,$06,$1C,$18,$18,$06,$00,$63,$81,$86,$18,$70,$70,$E0,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $03,$03,$03,$F0,$1F,$F0,$C0,$C0,$FC,$0C,$1C,$30,$30,$3F,$03,$FE
           .BYTE $0F,$FC,$30,$70,$7F,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$06,$06,$07,$E0,$1F,$E1,$81,$81
           .BYTE $F8,$18,$18,$60,$60,$7E,$07,$F8,$1F,$F8,$60,$60,$7E,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00,$00,$00,$00

;-------------------------------
; INTERRUPT ROUTINES FOR
; NIGHTRAIDER
;--------------------------------
IRQ1   PHA
       TXA
       PHA
       TYA
       PHA
       LDA PRESS
       BEQ NOPRES
       LDA $D010
       BEQ MOVEM
       LDA #$00 
       STA PRESS
       JMP MOVEM
NOPRES LDA ACTFLG
       BEQ MOVEM
       LDA $D010   
       BNE MOVEM
       INC PRESS
       LDX #$00
       LDA GUNSX,X
       BEQ BULLET
       INX
       LDA GUNSX,X
       BNE MOVEM
BULLET LDA #$98    
       STA GUNSY,X
       LDA CROSSX
       SEC
       SBC #$07
       STA GUNSX,X
       CLC
       ADC #$13
       STA GUNSX2,X
       LDA TPOINT
       STA TPL,X
       LDA #$FF
       STA HOLDER
       LDA #$00
       STA SNDFLG
       STA SNDFLG2
MOVEM  LDY #$9A
       LDA #$00
       TAX
FOPS   STA $3300,Y
       DEY
       BNE FOPS
       LDA #$05
       STA IRQVAR1  
       LDA GUNSX,X
       BNE RAYNOW
RAY    INX  
       LDA GUNSX,X 
       BEQ NOGUN   
       LDA #$50
       STA IRQVAR1
RAYNOW STX IRQVAR2
       LDY GUNSY,X  
       LDX #$04
RAYLOP LDA $3300,Y
       ORA IRQVAR1
       STA $3300,Y
       DEY
       DEX
       BNE RAYLOP
       LDX IRQVAR2
       BEQ RAY
NOGUN  LDA GUNSX
       STA $D004
       LDA GUNSX2
       STA $D005
       LDA GUNSX+1
       STA $D006
       LDA GUNSX2+1
       STA $D007
       LDA GUNSY  
       CMP TPL
       BCS NOFAR
       LDA #$00   
       STA GUNSX  
       JMP NOTWI      
NOFAR  DEC GUNSY   
       DEC GUNSY  
       DEC GUNSY  
       DEC GUNSY   
       LDA GUNSY    
       SEC
       SBC TPL
       CMP #$24
       BCS NOTWI   
       INC GUNSX
       DEC GUNSX2   
NOTWI  LDA GUNSY+1  
       CMP TPL+1 
       BCS NOFAR2  
       LDA #$00   
       STA GUNSX+1    
       JMP NOTWI2
NOFAR2 DEC GUNSY+1    
       DEC GUNSY+1
       DEC GUNSY+1 
       DEC GUNSY+1    
       LDA GUNSY+1  
       SEC
       SBC TPL+1
       CMP #$24
       BCS NOTWI2
       INC GUNSX+1  
       DEC GUNSX2+1
NOTWI2 LDA SNDFLG2
       BNE NOTWI3
       LDA SNDFLG   
       BNE SND2   
       LDA #$88
       STA $D201
       LDA HOLDER   
       STA $D200 
       LDA HOLDER 
       CMP #$FA
       BNE SSS
       LDA #$20
SSS    SEC     
       SBC #$01
       STA HOLDER
       BNE NOTWI3     
       INC SNDFLG    
       LDA #$90   
       STA HOLDER 
       JMP NOTWI3
SND2   DEC HOLDER  
       LDA HOLDER
       STA $D201
       CMP #$80
       BNE NOTWI3
       INC SNDFLG2
NOTWI3 LDX #$0C
FDS1   LDA $3490,X
       STA $34A0,X
       LDA $3590,X
       STA $35A0,X
       LDA $3690,X
       STA $36A0,X
       LDA $3790,X
       STA $37A0,X
       DEX
       BPL FDS1
       LDA #IRQ2&255
       STA VDLST
       LDA #IRQ2/255
       STA VDLST+1
       JMP RTEND
;--------------------------------
IRQ2   PHA
       TXA
       PHA
       TYA
       PHA
       STA $D40A
       STA $D40A
       LDA #$B4
       STA $D012
       LDA #$44
       STA $D013
       LDA #$92
       STA $D014
       STA $D015
       LDA ACTFLG  
       BEQ UPDATE  
       LDA $D20A
       ORA #$81      
       STA $359B
       LDA $D20A
       ORA #$81
       STA $349B
       LDA $D300
       ROR
       ROR
       ROR
       BCC LEFT
       ROR
       BCC RIGHT
       BCS UPDATE
LEFT   LDA CROSSX
       CMP #$30
       BEQ UPDATE
       DEC CROSSX
       BNE UPDATE
RIGHT  LDA CROSSX
       CMP #$C8
       BEQ UPDATE
       INC CROSSX
UPDATE LDA CROSSX
       STA $D000
       STA $D001
       SEC
       SBC #$08
       STA $D002
       CLC
       ADC #$10
       STA $D003
       LDA #$31
       STA $D01B
       LDA $D300 
       ROR  
       BCC UPSY
       ROR
       BCS NOTP
       LDA TPOINT
       CMP #$40
       BEQ NOTP
       DEC TPOINT   
       BNE NOTP
UPSY   LDA TPOINT
       CMP #$73
       BEQ NOTP
       INC TPOINT 
NOTP   LDA $D40B      ;VCOUNT
       CMP #$50       ;For an NTSC machine, VCOUNT counts from $00 to $82; for PAL, it counts to $9B.
       BCC NOTP
       LDA #$00
       STA $D012
       STA $D013
       STA $D014
       STA $D015
       LDA CROSSX
       SEC
       SBC #$10
       STA $D000  
       STA $D001
       SEC
       SBC #$08
       STA $D002
       CLC
       ADC #$10
       STA $D003
       LDA #IRQ3&255
       STA VDLST
       LDA #IRQ3/255
       STA VDLST+1
       LDA $D008
       BEQ TT1
       ORA HIT1  
       STA HIT1
TT1    LDA $D009
       BEQ TT2
       ORA HIT2
       STA HIT2
TT2    LDA $D00A
       BEQ TT3 
       ORA HIT3
       STA HIT3
TT3    LDA $D00B
       BEQ TT4
       ORA HIT4
       STA HIT4
       LDA SPACFLG
       BEQ TT4
       LDX #$03
FDS2   LDA $D000,X 
       AND #$0E
       BNE NOAH
       DEX
       BPL FDS2
TT4    LDX #$00
       LDA GUNSY,X
       SEC
       SBC TPL,X
       CMP #$04
       BCC GOTH
       INX 
       LDA GUNSY,X
       SEC
       SBC TPL,X
       CMP #$04
       BCS NOH
GOTH   TXA
       ASL
       TAY
       LDA $D000,Y
       AND #$0E 
       BNE NOAH
       LDA $D001,Y
       AND #$0E 
       BEQ NOH
NOAH   JMP (COLLAD)  
NOH    STA $D01E
       JMP RTEND   
;--------------------------------
IRQ3   PHA
       TXA
       PHA
       TYA
       PHA
       STA $D40A
       LDA #$2C
       STA $D409
       LDA #$92
       STA $D01A
       LDA #$00
       STA $D016
       LDA #$44
       STA $D017
       LDA #$FF
       STA $D018
       LDA #$C6
       STA $D019
       LDA BSCOR0
       BNE SCORESIT
       LDA BSCOR1
       BEQ SCORESIT2
       DEC BSCOR1
       LDA BSCOR0
       JMP TENNIS
SCORESIT 
       LDA BSCOR0
       CMP #$0A
       BCC JOUST
TENNIS SEC
       SBC #$0A
       STA BSCOR0
       LDA #$10
       JMP MINI  
JOUST  DEC BSCOR0 
       LDA #$01
MINI   SED
       CLC
       ADC SCORE1
       STA SCORE1
       LDA SCORE2
       ADC #$00
       STA SCORE2
       LDA SCORE3
       ADC #$00
       STA SCORE3
SCORESIT2 CLD
       LDY #$00
       LDX #$06  
SCRE   LDA SCORE1,Y  
       AND #$0F
       CLC
       ADC #$01
       STA $3E66,X
       DEX
       LDA SCORE1,Y
       LSR
       LSR
       LSR
       LSR
       CLC
       ADC #$01
       STA $3E66,X
       INY
       DEX
       BNE SCRE
       LDX #$0A  
       LDA #$00
GOBON  STA $3E78,X
       DEX
       BNE GOBON
       LDX SHIPS
       BEQ MAGA
       DEX
       BEQ MAGA
       LDY #$00
WASIT  LDA #$AB
       STA $3E79,Y
       INY
       LDA #$AC
       STA $3E79,Y
       INY
       DEX
       BNE WASIT
MAGA   LDY #$00
       LDX FUEL
LUAN   TYA
       PHA
       LDY #$04
       LDA #$00
       CPX #$00
       BEQ NOGAS
PUFFS  CLC
       ROR
       SEC
       ROR
       DEX
       BEQ NOGAS
       DEY
       BNE PUFFS
NOGAS  STA IRQVAR1
       PLA
       TAY
       LDA IRQVAR1
       STA $3F40,Y 
       INY
       CPY #20
       BNE LUAN
       LDA FUEL
       CMP #$20
       BCS FDS3
       INC SPARE
       LDA SPARE
       CMP #$05
       BNE FDS3
       LDA #$00
       STA SPARE
       LDX #$3
FDS4   LDA $3E89,X
       EOR #$80
       STA $3E89,X
       DEX
       BPL FDS4
FDS3   LDA #IRQ1&255
       STA VDLST
       LDA #IRQ1/255
       STA VDLST+1
       JMP RTEND
;--------------------------------
VBLANK LDA #$00
       STA $4D
       LDA MOVFLG
       BNE JIVE
SUDDY  JMP FLICK 
JIVE   INC VDCNT
       LDA VDCNT
       CMP #$03
       BEQ MILOS
       JMP FLICK
MILOS  LDA #$00
       STA VDCNT
       DEC SCRCNT
       LDA SCRCNT
       BPL SCROLM    
       LDA LIST2+3
       SEC
       SBC #$28
       STA LIST2+3
       BCS NOMINU 
       DEC LIST2+4
NOMINU LDA #$07
       STA SCRCNT
SCROLM STA $D405
FLICK  INC DELAYER    
       LDA DELAYER
       CMP #$0A
       BEQ FRANK
       JMP NOMOV
FRANK  LDA #$00
       STA DELAYER
       LDA WINDOWVAR ;THIS ROUTINE
       BEQ PLOTW     ;MAKES THE
       LDX #$FF      ;BURNING WINDOW
       STX WINDOWVAR ;CHARACTER
PLOTW  INC WINDOWVAR ;FLICKER!
       ASL
       TAX
       LDA WINDOWS,X
       STA IRQVAR1
       LDA WINDOWS+1,X
       STA IRQVAR2
       LDY #$07
FILWIN LDA (IRQVAR1),Y
       STA $7068,Y   
       DEY
       BPL FILWIN
       LDA RADARVAR
       CMP #$07
       BNE PLOTR
       LDX #$FF
       STX RADARVAR
PLOTR  INC RADARVAR
       ASL
       TAX
       LDA RADARS,X
       STA IRQVAR1
       LDA RADARS+1,X  
       STA IRQVAR2
       LDY #$0F
FILRAD LDA (IRQVAR1),Y
       STA $7150,Y 
       DEY
       BPL FILRAD
       LDA TANKFLAG
       BNE TANKUP   
       INC TANKVAR
       LDA TANKVAR
       CMP #$04
       BNE FILTAN
       INC TANKFLAG
       JMP NOMOV
TANKUP DEC TANKVAR
       LDA TANKVAR
       BPL FILTAN
       DEC TANKFLAG
       JMP NOMOV
FILTAN ASL
       TAX
       LDA TANKS,X
       STA IRQVAR1  
       LDA TANKS+1,X
       STA IRQVAR2
       LDY #$17
TANFIL LDA (IRQVAR1),Y
       STA $7070,Y
       DEY
       BPL TANFIL
NOMOV  DEC DELBAS
       BNE NOMOV2
       LDA #$0A     
       STA DELBAS
       DEC PNTBAS
       LDA PNTBAS
       BPL FDS5
       LDA #$06
       STA PNTBAS
FDS5   ASL
       TAX
       LDA BASLOK,X
       STA IRQVAR1
       LDA BASLOK+1,X
       STA IRQVAR2
       LDY #$0F
FDS6   LDA (IRQVAR1),Y
       STA $71F8,Y
       DEY
       BPL FDS6

NOMOV2 LDX #$10
RANLOP LDA $D20A
       EOR $D40B        ;VCOUNT - For an NTSC machine, VCOUNT counts from $00 to $82; for PAL, it counts to $9B.
       STA $717F,X 
       DEX
       BNE RANLOP
       LDA HPOS1 
       STA $D000
       LDA HPOS2
       STA $D001
       LDA HPOS3
       STA $D002
       LDA HPOS4
       STA $D003
       STA $D01E 
       LDA SPACFLG
       BEQ NOSTAR
       LDA BASER
       BNE NOSTAR
       JSR STARS
NOSTAR JMP RTEND  

;--------------------------------
; DATA STORAGE AND VARIABLES
;--------------------------------
DATA
;--------------------------------
VDCNT      .BYTE $00
SPARE      .BYTE $00
WINDOWVAR  .BYTE $00
RADARVAR   .BYTE $00
DELAYER    .BYTE $00
TANKFLAG   .BYTE $00
TANKVAR    .BYTE $00
DELBAS     .BYTE $00
GUNSX      .BYTE $00,$00
GUNSX2     .BYTE $00,$00
GUNSY      .BYTE $00,$00
PRESS      .BYTE $00
HOLDER     .BYTE $00
SNDFLG     .BYTE $00
KILCNT     .BYTE $00
SPACFLG    .BYTE $00
BASFLG     .BYTE $00
BASER      .BYTE $00
TRNCNT     .BYTE $00
TRNFLG     .BYTE $00
TRNPNT1    .BYTE $00
TRNPNT2    .BYTE $00
TRNVAR1    .BYTE $00
TRNVAR2    .BYTE $00
SMISY      .BYTE $00
SAUCT      .BYTE $00
SAUCPNT    .BYTE $00
SAUCPNT2   .BYTE $00
SAUCFLG    .BYTE $00
SAUCDIR    .BYTE $00
SAUCY      .BYTE $00
SAUCNT     .BYTE $00
WRNCNT     .BYTE $00
VOLUM      .BYTE $00
VOLFLG     .BYTE $00
SNDCNT     .BYTE $00
BRDFLG     .BYTE $00
BRDPNT     .BYTE $00
CRUD       .BYTE $00
MISDIR     .BYTE $00
MISCNT     .BYTE $00
UFOEXP     .BYTE $00
UFKFLG     .BYTE $00
UFCNT      .BYTE $00
STRCNT     .BYTE $00
STRFAS     .BYTE $00
MUSCNT     .BYTE $00
MUSDEL     .BYTE $00
PSTRING    .BYTE $00
PSTATUS    .BYTE $00
FLYFLG     .BYTE $00
PATHPNT    .BYTE $00
WAVES      .BYTE $00
MIKEY      .BYTE $00
MIKEY2     .BYTE $00
MXDELAY    .BYTE $00
MXFLAG     .BYTE $00
MXSCRL     .BYTE $00
MXDEATH    .BYTE $00
MCNT       .BYTE $00
BASDEAD    .BYTE $00
CNTFIRE    .BYTE $00
;--------------------------------
; CHARACTER HIT TABLES
; USED BY THE CHARATER DESTROY
; ROUTINES
; EACH OBJECT IT IS RESERVED
; FIVE BYTES IN MEMORY
; BYTE 0 = EXPLOSION STATUS
; BYTE 1 & 2 = ADRESS HIT AT
; BYTE 3 = CHARACTER HIT   
; THERE IS ENOUGH ROOM FOR 
; TWENTY EXPLOSIONS AT ONCE
; THAT SHOULD BE ENOUGH!
;--------------------------------
HITABLE    .BYTE $00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00
           .BYTE $00,$00,$00,$00,$00
ENDAT      .BYTE $FF
;--------------------------------
; NONZERO VARIABLES TO BE
; FILLED
;--------------------------------
NONDAT
;--------------------------------
TPL        .BYTE $40,$40
SNDFLG2    .BYTE $01
PNTBAS     .BYTE $07
FLYCNT     .BYTE $01
FLCNT      .BYTE $01
FLCNT2     .BYTE $1C
SPS1       .BYTE $FF
SPS2       .BYTE $A0
MISAUC     .BYTE $50
EXPSND     .BYTE $FF
BRDCNT     .BYTE $FF
MUSCOM     .BYTE $FF
NONEND     .BYTE $00
;--------------------------------
STRGFIL    .BYTE $40,$40,$01,$07,$01,$01,$1C,$FF,$A0,$50,$FF,$FF,$FF
;--------------------------------
INITVAR    LDX #ENDAT-DATA-1
           LDA #$00
FDS7       STA DATA,X
           DEX
           BPL FDS7
           LDX #$0C 
FDS8       LDA STRGFIL,X 
           STA NONDAT,X
           DEX
           BPL FDS8
           RTS
;--------------------------------
; SHAPETABLES
;--------------------------------
WINFIRE    .BYTE $AA,$B6,$AE,$92,$AA,$BA,$B2,$A6
WINFIRE2   .BYTE $AA,$A6,$B2,$BA,$AA,$B6,$AE,$92
;--------------------------------
; UPDATES CHARACER $0D FOR
; WINDOW FIRES!
;--------------------------------
TANK1      .BYTE $00,$05,$00,$00,$01,$07,$06,$06
           .BYTE $0A,$55,$0A,$6A,$A8,$A2,$95,$41
           .BYTE $A0,$A0,$66,$9A,$69,$90,$40,$40
TANK2      .BYTE $00,$00,$00,$00,$01,$07,$06,$06  
           .BYTE $0A,$55,$0A,$6A,$A8,$A2,$95,$41 
           .BYTE $A0,$A0,$66,$9A,$69,$90,$40,$40
TANK3      .BYTE $00,$00,$00,$00,$01,$07,$06,$06
           .BYTE $0A,$05,$0A,$6A,$A8,$A2,$95,$41 
           .BYTE $A0,$A0,$66,$9A,$69,$90,$40,$40
TANK4      .BYTE $00,$00,$00,$00,$01,$07,$06,$06 
           .BYTE $0A,$0A,$0A,$6A,$A8,$A2,$95,$41 
           .BYTE $A0,$55,$66,$9A,$69,$90,$40,$40

;--------------------------------
; TANKS FOR SHOW!
;--------------------------------
;--------------------------------
;--------------------------------
BRIDGE1    .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
           .BYTE $00,$FF,$FA,$AA,$AA,$AA,$AA,$AA
           .BYTE $00,$00,$F3,$00,$00,$00,$00,$00
           .BYTE $00,$FF,$0F,$00,$00,$00,$00,$00
BRIDGE2    .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
           .BYTE $00,$00,$F0,$AF,$AA,$AA,$AA,$AA
           .BYTE $00,$00,$00,$00,$F0,$00,$0F,$00
           .BYTE $00,$0F,$0F,$00,$F0,$00,$00,$00
BRIDGE3    .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
           .BYTE $00,$00,$00,$00,$00,$00,$45,$A9
           .BYTE $00,$00,$00,$00,$00,$48,$36,$00
           .BYTE $00,$0F,$0F,$00,$F0,$00,$00,$00
;-----------------------------------------------
RADAR1     .BYTE $0A,$15,$56,$15,$0A,$3F,$0F,$03
           .BYTE $A0,$54,$95,$54,$A0,$A8,$A0,$80
RADAR2     .BYTE $00,$01,$06,$15,$16,$3F,$0F,$03
           .BYTE $50,$58,$68,$A0,$A0,$A8,$A0,$80
RADAR3     .BYTE $00,$02,$0A,$2A,$0E,$3F,$0F,$03
           .BYTE $80,$A0,$A0,$80,$A0,$A8,$A0,$80
RADAR4     .BYTE $02,$2A,$AA,$2A,$0F,$3F,$0F,$03
           .BYTE $A0,$A8,$A0,$A0,$A0,$A8,$A0,$80
RADAR5     .BYTE $2A,$AA,$2A,$0A,$0F,$3F,$0F,$03
           .BYTE $A8,$AA,$A8,$A0,$A0,$A8,$A0,$80
RADAR6     .BYTE $0A,$2A,$0A,$0A,$0F,$3F,$0F,$03
           .BYTE $80,$A8,$AA,$A8,$A0,$A8,$A0,$80
RADAR7     .BYTE $02,$0A,$0A,$02,$0F,$3F,$0F,$03
           .BYTE $00,$80,$A0,$A8,$A0,$A8,$A0,$80
RADAR8     .BYTE $05,$25,$29,$0A,$0A,$3F,$0F,$03
           .BYTE $00,$40,$90,$54,$54,$A8,$A0,$80
;--------------------------------
; SHAPE VECTOR LOOKUP TABLES
;--------------------------------
WINDOWS    .WORD WINFIRE
           .WORD WINFIRE2
;--------------------------------
TANKS      .WORD TANK1
           .WORD TANK2
           .WORD TANK3
           .WORD TANK4
;--------------------------------
BRIDGES    .WORD BRIDGE1
           .WORD BRIDGE2
           .WORD BRIDGE3
;--------------------------------
RADARS     .WORD RADAR1
           .WORD RADAR2
           .WORD RADAR3
           .WORD RADAR4
           .WORD RADAR5
           .WORD RADAR6
           .WORD RADAR7
           .WORD RADAR8
;--------------------------------
; ATTACK CHARACTERS FOR 
; NONSPACE ATTACK!
;--------------------------------
SAUCER1    .BYTE $18,$00,$FF,$FF,$7E,$3C,$24,$42
           .BYTE $00,$3C,$00,$03,$00,$00,$24,$42
SAUCER2    .BYTE $18,$00,$FF,$FF,$7E,$3C,$24,$42
           .BYTE $18,$3C,$00,$0C,$00,$00,$24,$42
SAUCER3    .BYTE $18,$00,$FF,$FF,$7E,$3C,$24,$42
           .BYTE $00,$3C,$00,$30,$00,$00,$24,$42
SAUCER4    .BYTE $18,$00,$FF,$FF,$7E,$3C,$24,$42
           .BYTE $18,$3C,$00,$C0,$00,$00,$24,$42
SAUCER5    .BYTE $54,$4C,$B3,$B3,$32,$70,$68,$0E
           .BYTE $54,$70,$4C,$40,$4C,$4C,$68,$0E
SAUCERC    .BYTE $44,$84 ;COLORS
;--------------------------------
; CHARACTER LOOKUP TABLE
; TELLS HOW TO DRAW SAPE ETC.
;--------------------------------
CHATBL     .BYTE $2A,$2B,$22,$23,$24,$25,$26
           .BYTE $27,$28,$29,$0E,$0F,$10,$65
           .BYTE $E6,$67,$FF  
CHTBL2     .BYTE $00,$01,$00,$01,$02,$03,$28
           .BYTE $29,$2A,$2B,$00,$01,$02,$00
           .BYTE $01,$02,$00
CHTBL3     .BYTE $11,$12,$13,$14,$9A,$9B
           .BYTE $9C,$9D,$9E,$FF
EXPTBL     .BYTE $2A,$22,$0E,$65
EXPTBL2    .BYTE $00,$29,$00,$00
EXPTBL3    .BYTE $E8,$E9,$EA,$EB,$EC,$ED,$EE,$EF 
;--------------------------------
; POINT VALUE TABLE
;--------------------------------
PNTBL      .BYTE $2A,$C8,$00
           .BYTE $22,$64,$00  
           .BYTE $0E,$2C,$01
           .BYTE $65,$90,$01 
           .BYTE $FF
;--------------------------------
; SPACE ATTACK SHAPES
;--------------------------------
SPACL1     .BYTE $00,$00,$00,$00,$00,$08,$28,$A8
           .BYTE $00,$00,$00,$00,$00,$00,$00,$30
           .BYTE $2A,$FE,$3E,$0F,$00,$00,$00,$00
           .BYTE $F0,$B0,$A0,$A4,$A5,$25,$0A,$00
;-----------------------------------------------
SPACL2     .BYTE $00,$00,$00,$02,$02,$02,$FA,$35
           .BYTE $00,$00,$00,$80,$80,$80,$AF,$5C
           .BYTE $0A,$02,$00,$00,$00,$00,$00,$00
           .BYTE $A0,$80,$00,$00,$00,$00,$00,$00
;-----------------------------------------------
SPACL3     .BYTE $00,$00,$00,$00,$00,$00,$00,$0C
           .BYTE $00,$00,$00,$00,$00,$20,$28,$2A
           .BYTE $0F,$0E,$0A,$1A,$5A,$58,$A0,$00
           .BYTE $A8,$BF,$BC,$F0,$00,$00,$00,$00
;--------------------------------
; SPACE PATH TABLES 
; THERE ARE 14
; THERE IS AN XPATH A YPATH AND
; A SHPTABLES FOR EVERYONE
;--------------------------------
XPATH1     .BYTE $11,$10,$0F,$0E,$0D,$0D,$0D,$0E
           .BYTE $0F,$10,$11,$12,$13,$14,$14,$14
           .BYTE $15,$16,$17,$18,$19,$1A,$1B,$1C
           .BYTE $1D,$1E,$1F,$20,$21,$22,$23,$FF
YPATH1     .BYTE $00,$01,$02,$03,$04,$04,$04,$04
           .BYTE $04,$04,$04,$04,$04,$04,$04,$04
           .BYTE $05,$06,$07,$08,$09,$0A,$0B,$0C
           .BYTE $0D,$0E,$0F,$10,$11,$12,$13,$FF
SHPTAB1    .BYTE $78,$78,$78,$78,$74,$74,$74,$74
           .BYTE $74,$74,$74,$74,$74,$74,$74,$74
           .BYTE $70,$70,$70,$70,$70,$70,$70,$70
           .BYTE $70,$70,$70,$70,$70,$70,$70,$FF
;------------------------------------------------
XPATH2     .BYTE $07,$08,$09,$0A,$0B,$0B,$0B,$0B
           .BYTE $0A,$09,$08,$07,$06,$05,$05,$05
           .BYTE $04,$03,$02,$01,$00,$FF
YPATH2     .BYTE $00,$01,$02,$03,$04,$04,$04,$04
           .BYTE $04,$04,$04,$04,$04,$04,$04,$04
           .BYTE $05,$06,$07,$08,$09,$FF
SHPTAB2    .BYTE $70,$70,$70,$70,$70,$74,$74,$74
           .BYTE $74,$74,$74,$74,$74,$74,$74,$74
           .BYTE $78,$78,$78,$78,$78,$FF
;------------------------------------------------
XPATH3     .BYTE $03,$04,$05,$06,$07,$07,$07,$07
           .BYTE $08,$09,$0A,$0B,$0C,$0D,$0E,$0F
           .BYTE $10,$11,$12,$13,$FF
YPATH3     .BYTE $00,$01,$02,$03,$05,$05,$05,$05
           .BYTE $06,$07,$08,$09,$0A,$0B,$0C,$0D
           .BYTE $0E,$0F,$10,$11,$FF
SHPTAB3    .BYTE $70,$70,$70,$70,$70,$74,$74,$74
           .BYTE $70,$70,$70,$70,$70,$70,$70,$70
           .BYTE $70,$70,$70,$70,$FF
;------------------------------------------------
XPATH4     .BYTE $26,$26,$25,$24,$23,$22,$21,$20
           .BYTE $1F,$1E,$1D,$1C,$1B,$1A,$19,$18
           .BYTE $17,$16,$15,$14,$13,$12,$FF
YPATH4     .BYTE $04,$05,$05,$05,$05,$05,$05,$05
           .BYTE $06,$07,$08,$09,$0A,$0B,$0C,$0D
           .BYTE $0E,$0F,$10,$11,$12,$13,$FF
SHPTAB4    .BYTE $78,$78,$74,$74,$74,$74,$74,$78
           .BYTE $78,$78,$78,$78,$78,$78,$78,$78
           .BYTE $78,$78,$78,$78,$78,$78,$FF
;----------------------------------------------
XPATH5     .BYTE $0B,$0C,$0D,$0E,$0F,$0F,$0F,$0F
           .BYTE $0E,$0D,$0C,$0B,$0B,$0B,$0B,$0C
           .BYTE $0D,$0E,$0F,$10,$11,$12,$13,$14
           .BYTE $15,$16,$FF
YPATH5     .BYTE $00,$01,$02,$03,$04,$04,$04,$04
           .BYTE $05,$06,$07,$08,$08,$08,$08,$09
           .BYTE $0A,$0B,$0C,$0D,$0E,$0F,$10,$11
           .BYTE $12,$13,$FF
SHPTAB5    .BYTE $70,$70,$70,$70,$70,$74,$74,$74
           .BYTE $78,$78,$78,$78,$74,$74,$74,$70
           .BYTE $70,$70,$70,$70,$70,$70,$70,$70
           .BYTE $70,$70,$FF
;--------------------------------
; PATHPOINTERS
;--------------------------------
PATHX      .WORD XPATH1
           .WORD XPATH2
           .WORD XPATH3
           .WORD XPATH4
           .WORD XPATH5
;--------------------------------
PATHY      .WORD YPATH1
           .WORD YPATH2
           .WORD YPATH3
           .WORD YPATH4
           .WORD YPATH5
;--------------------------------
PSHIP      .WORD SHPTAB1
           .WORD SHPTAB2   
           .WORD SHPTAB3
           .WORD SHPTAB4
           .WORD SHPTAB5
;--------------------------------
; BASE STATION CANNON SHAPES
;--------------------------------
BASCAN1    .BYTE $00,$0F,$0F,$0F,$0F,$01,$01,$01,$00,$F0,$F0,$E0,$F8,$42,$40,$40
BASCAN2    .BYTE $00,$0F,$0F,$0E,$0F,$01,$01,$01,$00,$F0,$F0,$B0,$B0,$70,$40,$40
BASCAN3    .BYTE $00,$0F,$0F,$0E,$0E,$09,$01,$01,$00,$F0,$F0,$B0,$F0,$40,$40,$40
BASCAN4    .BYTE $00,$0F,$0F,$0B,$2F,$81,$01,$01,$00,$F0,$F0,$F0,$F0,$40,$40,$40

;--------------------------------
; SHAPE MOVEMENT LOOKUP TABLES
;--------------------------------
BASLOK     .WORD BASCAN1
           .WORD BASCAN2
           .WORD BASCAN3
           .WORD BASCAN4
           .WORD BASCAN3
           .WORD BASCAN2
           .WORD BASCAN1

