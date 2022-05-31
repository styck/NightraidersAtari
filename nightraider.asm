*--------------------------------
* NIGHTRAIDERS
* WRITTEN BY : PETER FILIBERTI
* COPYRIGHT 1982
*--------------------------------
       .OR  $6B0    
       .TF  OBJ
*--------------------------------
* ZERO PAGE VARIABLES
*--------------------------------
HISCORE1   .EQ $80             ;These are the three locations where
HISCORE2   .EQ HISCORE1+1      ;the high score is saved in BCD
HISCORE3   .EQ HISCORE2+1      ;for later use.
OLSCORE1   .EQ HISCORE3+1      ;These are the three locations where
OLSCORE2   .EQ OLSCORE1+1      ;the last score achieved is placed
OLSCORE3   .EQ OLSCORE2+1      ;for use in menu screen also BCD.
BSCOR0     .EQ OLSCORE3+1      ;This is a binary score used as a
BSCOR1     .EQ BSCOR0+1        ;temporary score holder by the game.
SCORE1     .EQ BSCOR1+1        ;This is the actual game score
SCORE2     .EQ SCORE1+1        ;stored in BCD it is also the same
SCORE3     .EQ SCORE2+1        ;score put on game screen while playing.
TEMP1      .EQ SCORE3+1        ;TEMP1 through TEMP8 are temporary
TEMP2      .EQ TEMP1+1         ;locations used by main program and
TEMP3      .EQ TEMP2+1         ;subroutines. The INTERRUPT routines
TEMP4      .EQ TEMP3+1         ;must not touch these locatons!
TEMP5      .EQ TEMP4+1         ;""
TEMP6      .EQ TEMP5+1         ;""
TEMP7      .EQ TEMP6+1         ;""
TEMP8      .EQ TEMP7+1         ;""
CLRVAR     .EQ TEMP8+1
LEVEL      .EQ CLRVAR+1        ;Games level of play 0-6
SCRCNT     .EQ LEVEL+1         ;Screen fine scroll counter
HPOS1      .EQ SCRCNT+1        ;Horizontal position player1
HPOS2      .EQ HPOS1+1         ;Horizontal position player2
HPOS3      .EQ HPOS2+1         ;Horizontal position player3
HPOS4      .EQ HPOS3+1         ;Horizontal position plyer4
MOVFLG     .EQ HPOS4+1         ;Flag for screen movement
FUEL       .EQ MOVFLG+1        ;Fuel in planes tank 0-$50
CROSSX     .EQ FUEL+1          ;Plane horizontal axis position
MISSLEX    .EQ CROSSX+1        ;Missle horizontal position
SHIPS      .EQ MISSLEX+1       ;Number of ships left. start=3
IRQVAR1    .EQ SHIPS+1         ;IRQVAR1 through IRQVAR2 are
IRQVAR2    .EQ IRQVAR1+1       ;temporary locations to be
IRQVAR3    .EQ IRQVAR2+2       ;used by interrupt routines only.
IRQVAR4    .EQ IRQVAR3+3       ;Main program should not use these!
TPOINT     .EQ IRQVAR4+1       ;Target point positioner.
HIT1       .EQ TPOINT+1        ;HIT1-HIT4 are copies of the
HIT2       .EQ HIT1+1          ;colision register updated every
HIT3       .EQ HIT2+1          ;60 hz. It is used by main program
HIT4       .EQ HIT3+1          ;interrupts logicaly or data in these.
COLLAD     .EQ HIT4+1          ;Screen collision address (TWO BYTES)
ACTFLG     .EQ COLLAD+2        ;Plane action flag.
*--------------------------------
* ATARI LOCATIONS
*--------------------------------
SCREEN     .EQ $4000           ;Our menu sreen
PMBASE     .EQ $D407           ;Player missle base address
DISPLA     .EQ $3F00           ;Another menu scree location
VDLST      .EQ $200            ;Display list interrupt vector
VBLK       .EQ $224            ;Vertical blank interrupt vector
DLISTP     .EQ $230            ;Display list pointer
CLB        .EQ $2C8            ;Color register background
CLP0       .EQ $2C4            ;Color register playfield 1
CLP1       .EQ $2C5            ;Color register playfield 2
CLP2       .EQ $2C6            ;Color register playfield 3
CLP3       .EQ $2C7            ;Color register Playfield 4
CPLAY0     .EQ $2C0            ;Color player 1
CPLAY1     .EQ $2C1            ;Color player 2
CPLAY2     .EQ $2C2            ;Color player 3
CPLAY3     .EQ $2C3            ;Color player 4
CONSOL     .EQ $D01F           ;Console switch address
GRACTL     .EQ $D01D           ;Graphic control address
CHBASE     .EQ $2F4            ;Character set base address
TRIG0      .EQ $284            ;Joystick trigger
DMACTL     .EQ $22F            ;Dma control register
NMIEN      .EQ $D40E           ;NMI control register
*--------------------------------
* GAME COLD START
*--------------------------------
COLDSTART  LDX #HISCORE1       ;Clear all of zero page variables
           JSR INIT            
           JMP INTRO           ;Goto menu routines
WARMSTART  LDX #BSCOR0         ;Erase all of zero page variables
           JSR INIT            ;except high scores!
           JMP INTRO           ;Goto menu routines
*--------------------------------
* WORDS USED BY INTRODUCTION
*--------------------------------
WORDS  .HS 00
       .AS "BY PETER FILIBERTI    DATAMOST INC  8@?:"  
       .HS 00
       .AS "HIGH SCORE      YOUR SCORE      "
       .HS 00
       .AS "CURRENT RANK" 
       .HS 00
LEVNAM .AS "NOVICE"  
       .HS 00
       .AS "CADET"
       .HS 00
       .AS "ENSIGN"
       .HS 00
       .AS "CAPTAIN"
       .HS 00
       .AS "COMMANDER"
       .HS 00
       .AS "NIGHTRAIDER" 
       .HS 00
       .AS "PRESS SELECT TO CHANGE STARTING RANK"
       .HS 00
       .AS "PRESS START OR FIRE BUTTON TO PLAY"
       .HS 00
       .AS "TODAYS TOP TEN HIGH SCORES"
       .HS 00
       .AS " CONGRAULATIONS YOU HAVE THE HIGH SCORE "
       .HS 00
       .AS "    YOUR SCORE IS ONE OF THE TOP TEN    "
       .HS 00
       .AS "           ENTER YOUR INITIALS          "
       .HS 00
*--------------------------------
TBL1   .HS 1219170B
COLORT .HS 8D94CA4500 
P1     .HS 001824247EFFFFFF     ;P1-P4 are plane shapes 
       .HS FFFF81FF             ;I just felt like putting
P2     .HS 0000181800004242     ;them here!??
       .HS 5AFFFFFF
P3     .HS 000000000001030F
       .HS 1F7F0000
P4     .HS 000000000080C0F0    
       .HS F8FE0000
*--------------------------------
* INTRODUCTION ROUTINE
*--------------------------------
INTRO  LDA #$2C                 ;Setup character base address
       STA CHBASE
       LDA #LIST1               ;Setup our display list pointers
       STA DLISTP               ;to point to our display list
       LDA /LIST1
       STA DLISTP+1
       LDA #$00
       STA $D405                ;Put a zero in horizontal scroll reg
       LDA #$3A
       STA DMACTL               ;Enable player DMA
       LDA #$03
       STA GRACTL               ;Enable player graphics
       JSR CLRMEN               ;Clear menu page
       LDA #MIRQ1               ;Setup the irq vectors to
       STA VDLST                ;point to our Irq routines
       LDA /MIRQ1
       STA VDLST+1
VSYNC  LDA $D40B                ;Is scan line at the top of the screen?
       CMP #$80
       BCC VSYNC                ;If not then loop
       LDA #$C0
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
.1     LDA HISCORE1,X           ;Get a BCD byte
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
       BPL .1
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
.1     STA $41A4,X
       DEX
       BNE .1
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
       BCS .1
       JMP GAME
.1     ROR
       BCS .2
.3     LDA CONSOL
       AND #$02
       BEQ .3
       INC LEVEL
       LDA LEVEL
       CMP #$06
       BNE .4
       LDA #$00
       STA LEVEL
.4     JMP SELECT
.2     LDA $D010
       BNE CKEY
       JMP GAME
*--------------------------------
CLRMEN LDA #$40                 ;Erase our menu screen
       STA TEMP2                ;$4000-$5000
       LDY #$00
       STY TEMP1
.2     TYA
.1     STA (TEMP1),Y
       INY
       BNE .1
       INC TEMP2
       LDA TEMP2
       CMP #$50
       BNE .2
       RTS
*--------------------------------
* MIRQ1 IRQ FOR MENU!
*--------------------------------
MIRQ1  PHA
       TXA
       PHA
       LDA $D40B
       CMP #$30     
       BCS MIRQ2
       LDA $2C6
       LDX #$11     
.1     STA $D40A
       STA $D018
       CLC
       ADC #$02     
       DEX
       BNE .1 
       INC CCNT
       LDA CCNT
       CMP #$08     
       BNE .2
       LDA #$00
       STA CCNT
       INC $2C6
.2     LDA #$28     
       STA $D018
       PLA
       TAX
       PLA
       RTI
CCNT   .HS 00
*--------------------------------
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
*--------------------------------
GAME   LDA #$00
       STA NMIEN   
       STA DMACTL 
       LDX #$05
COLFIL LDA COLORT-1,X
       STA CLP0-1,X
       DEX
       BNE COLFIL
       LDA #IRQ1
       STA VDLST
       LDA /IRQ1
       STA VDLST+1
       LDA #VBLANK
       STA VBLK
       LDA /VBLANK
       STA VBLK+1
       LDA #LIST2
       STA DLISTP
       LDA /LIST2
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
       LDA $D40B
       CMP #$78     
       BNE CS
       STA $D40A
       LDA #$C0
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

*--------------------------------
* NIGHTRAIDER GAME LOOPS
*--------------------------------
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
*--------------------------------
* BEGINING GAME INTRO SHOWN
* NOW CLEAR THE BULSHIT AND
* LETS GET ON WITH SOME ACTION!
*--------------------------------
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
BMES1      .HS 0D1F1C1C0F181E00
           .HS 1C0B1815
BMES2      .HS 17131D1D13191800
           .HS 190C140F0D1E13200F
BMES3      .HS 8E8F9D9E9C99A3008F
           .HS 988F97A3
BMES4      .HS 1D1E0B1E1F1D
BMES5      .HS 968B9F988D92
*--------------------------------
* MAIN GAME LOOP #1
*--------------------------------
GM1        LDA #COLRUT   ;SETUP
           STA COLLAD  ;COLLISION
           LDA /COLRUT  ;ROUTINE
           STA COLLAD+1 ;VECTOR
GMLOOP     JSR PAUSER
           LDA BASER  
           BEQ .99
           JSR EXPLOB
.99        JSR TRAINER 
           JSR BRIDGER
           JSR MX
           JSR SOUND
           JSR MISSLES
           JSR SPCATK
           JSR UFODIE
           JSR ATTACK 
           JSR FIREPOWER
           LDA SPACFLG 
           BEQ .1
           JSR KILLER2
           JMP GMLOOP
.1         JSR KILLER  
           JMP GMLOOP
*--------------------------------
* SUBROUTINE KILLER
* EXPLODES KILLED OBJECTS
* AND HANDLES DEATH!
*--------------------------------
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
           BCS .2
           DEC TEMP2
.2         LDY #$20
.3         LDA (TEMP1),Y
           BEQ .4
           CMP #$19   
           BEQ .4
           CMP #$9E
           BEQ .4
           DEY
           BNE .3
           JMP KIL8
.4         INY
           LDA #$9B
           STA (TEMP1),Y
.5         LDA (TEMP1),Y
           CMP #$11
           BEQ .6
           CMP #$19
           BEQ .6
           CMP #$9A
           BEQ .6
           LDA #$00
           STA (TEMP1),Y
           INY
           BNE .5  
.6         LDA #$00
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
           BCS .1
           DEC TEMP2
.1         LDY #$20
.2         LDA (TEMP1),Y
           CMP #$11
           BEQ .3
           DEY
           BNE .2
.3         LDA #$15
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
           BCC .4
           INC BSCOR1
.4         INC BRDFLG
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
.1         CMP PNTBL,X
           BEQ .2
           INX
           BNE .1
.2         PHA
           LDA PNTBL+1,X
           CLC
           ADC BSCOR0
           STA BSCOR0
           LDA PNTBL+2,X
           ADC BSCOR1
           STA BSCOR1
           PLA
           LDX #$00
.3         CMP EXPTBL,X   
           BEQ KIL13
           INX
           BNE .3
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
*--------------------------------
* CONTROL SUBROUTINE
* SETS FLAGS FOR OTHER ROUTINES
* DEPENDING ON GAME PLAY
*--------------------------------
CONTROL    LDA BASER
           BEQ .44 
           JMP CON4
.44        LDA SPACFLG
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
.1         TYA
.2         STA (TEMP1),Y
           DEY
           BNE .2
           INC TEMP2
           LDA TEMP2
           CMP #$50
           BNE .1
           LDX #$00
.3         LDA $6000,X
           STA $48C0,X
           LDA $6100,X
           STA $49C0,X
           LDA $6200,X
           STA $4AC0,X
           DEX
           BNE .3
           LDX #$6F
.4         LDA $6300,X
           STA $4BC0,X
           DEX
           BNE .4
           LDA #$58
           STA LIST2+3    
           LDA #$4C
           STA LIST2+4
           LDA #$FF
           STA BASER  
           LDA BASDEAD
           BEQ .5
           ASL
           TAX
.6         LDA BASOLD-2,X 
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
           BNE .6
.5         RTS
CON4       LDA LIST2+3
           CMP #$78
           BNE .1
           LDA LIST2+4
           CMP #$45
           BNE .1
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
.2         STA MUSCNT,X
           DEX
           BPL .2
.1         RTS
*--------------------------------
* SUBROUTINE TRAINER
* CHECKS FOR BRIDGES ON THE SCREN
* AND ROLLS TRAINS ACROSS THEM
*--------------------------------
TRAINER    JSR CONTROL
           INC TRNCNT  
           LDA TRNCNT
           CMP #$30 
           BEQ .1
           RTS
.1         LDA #$00
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
.1         CPX #$FF
           BEQ .2 
           LDA TRNSTR,X
           DEX
           JMP .3
.2         LDA #$19
.3         STA (TEMP1),Y
           DEY
           BPL .1
           INC TRNPNT1
           RTS
TRAIN6     LDY #$27
           LDX TRNPNT2
           BMI TRAIN7
.1         CPX #$FF
           BEQ .2
           LDA TRNSTR,X
           DEX
           JMP .3
.2         LDA #$19
.3         STA (TEMP1),Y
           DEY
           BPL .1
           DEC TRNPNT2 
           RTS
TRAIN7     LDY #$27
           LDA #$19
.1         STA (TEMP1),Y
           DEY
           BPL .1
           DEC TRNFLG
           RTS
TRNSTR     .HS 199A9B9B9B9B9E9A
           .HS 9B9B9B9B9E9A9B9B
           .HS 9B9B9E9A9B9B9B9B
           .HS 9E9A9B9B9B9B9B9B
           .HS 9C9D
*--------------------------------
* ATTACK ROUTINES ATTACKS 
* PLAYER DEPENDING ON
* WHAT FLAGS ARE SET
*--------------------------------
ATTACK     JSR CONTROL
           DEC FLCNT2
           BNE .3
           LDA #$1C
           STA FLCNT2
           DEC FLCNT 
           BNE .3
           LDA CRUD
           BNE .3
.2         LDA LEVEL
           ASL
           ASL
           ASL
           STA TEMP1
           LDA #59
           SEC
           SBC TEMP1
           STA FLCNT
           DEC FUEL 
           BNE .3
           JSR PLANEGON   
.3         LDA SPACFLG  
           BEQ .6
           LDA WRNCNT
           BNE .6
           LDA SAUCFLG
           BNE .5
.6         LDA SAUCFLG
           BNE .5
           LDA SPS1  
           INC LEVEL 
           INC LEVEL 
           SEC
           SBC LEVEL
           DEC LEVEL 
           DEC LEVEL
           STA SPS1
           BCS .1
           DEC SPS2
           BNE .1
           LDA #$A0
           STA SPS2
           LDA #$FF
           STA SPS1
           INC SAUCFLG  
           LDY #$00
           LDX #$00
           LDA $D20A
           BMI .4
           INX
           LDY #$FF
.4         STX SAUCDIR   
           STY HPOS1
           STY HPOS2
           LDA #$35
           STA SAUCY
           LDA #$20
           STA WRNCNT  
           LDX #$60
           LDA #$00
.20        STA $3400,X
           STA $3500,X
           DEX
           BNE .20
.1         RTS
.5         LDA WRNCNT
           BEQ MOVSAUC 
           INC FLYCNT
           LDA FLYCNT
           CMP #$50
           BNE .1
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
.2         LDA WRNMES-1,X
           STA $3E99,X
           DEX
           BNE .2
           DEC WRNCNT
           BNE .1
           LDA #$00
           STA $D206
           STA $D207
           LDX #$09
.3         STA $3E99,X
           DEX
           BNE .3
.1         RTS
MOVSAUC    INC SAUCNT
           LDA SAUCNT
           CMP #$10 
           BEQ .11
           RTS
.11        LDA #$00   
           STA SAUCNT
           LDA SAUCDIR  
           BEQ .1
           DEC HPOS1
           DEC HPOS2
           JMP .2
.1         INC HPOS1
           INC HPOS2
.2         LDA SAUCT
           BEQ .3
           BMI .4
           INC SAUCY 
           LDA SAUCY
           CMP #$60
           BNE .3
           LDA #$00
           STA SAUCT
           JMP .3
.4         DEC SAUCY
           LDA SAUCY
           CMP #$20
           BNE .3
           LDA #$00
           STA SAUCT
.3         LDX SAUCY  
           INX
           LDY #$0A
           LDA #$00
.5         STA $3400,X
           STA $3500,X
           DEX  
           DEY    
           BPL .5 
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
.1         LDA (TEMP1),Y
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
           BNE .1
           INC SAUCPNT2
           LDA SAUCPNT2
           CMP #$06
           BNE .66
           LDA #$00
           STA SAUCPNT2
           INC SAUCPNT
.66        LDA SAUCT 
           BNE MISCHK
           LDA $D20A
           BMI .10
           LDA SAUCY
           CMP #$20
           BEQ MISCHK
           DEC SAUCT 
           JMP MISCHK  
.10        LDA SAUCY
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
           BCC .1
           CMP #$0D
           BCC NOMISL
           DEC MISDIR  
           BNE NOMISL
.1         EOR #$FF
           CLC
           ADC #$01
           CMP #$0D
           BCC NOMISL
           INC MISDIR  
NOMISL     LDA HPOS1
           BNE .1
           LDA #$00
           STA SAUCFLG
           STA $D207
.1         RTS 
WRNMES     .HS 1C0F0E000B160F1C1E
SAUCTBL    .DA SAUCER1
           .DA SAUCER2
           .DA SAUCER3
           .DA SAUCER4
*--------------------------------
* SOUND GENERATOR
* GENERATES SOUNDS FOR GAME
*--------------------------------
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
           BNE .1
           LDA WRNCNT
           BNE .3
           LDA SAUCFLG
           BNE .2
           LDA #$00
           STA $D206
           STA $D207
.3         RTS
.1         LDA #$A8
           STA $D207
           LDA SMISY
           STA $D206
           RTS
.2         LDA VOLFLG    
           BNE .4 
           LDA #$1
           STA $D206
           LDA #$60
           ORA VOLUM
           STA $D207
           INC VOLUM
           LDA VOLUM
           CMP #$0F
           BNE .3
           INC VOLFLG
           JMP .3
.4         LDA #$1
           STA $D206
           LDA #$60
           ORA VOLUM
           STA $D207
           DEC VOLUM
           BNE .3
           DEC VOLFLG
           JMP .3
*--------------------------------
* OBJECT COLLISION HANDLER
*--------------------------------
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
           BEQ .1
           JMP SPACKIL
.1         LDA BASER
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
.1         LDA CHTBL3,X
           CMP (IRQVAR1),Y
           BEQ COLRU8
           CMP #$FF
           BEQ COLREND
           INX
           BNE .1
COLRU4     CMP #$22
           BCC .2
           CMP #$2A
           BCS .2
           INC FUEL
           INC FUEL
           INC FUEL
           LDA LEVEL
           ASL
           CLC
           ADC FUEL
           CMP #$51
           BCC .1
           LDA #$50
.1         STA FUEL
.2         LDA IRQVAR1   
           SEC
           SBC CHTBL2,X
           STA IRQVAR1
           BCS .3
           DEC IRQVAR2  
.3         LDA #$06  
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
*--------------------------------
* BRIDGE COLLAPSE ROUTINE
*--------------------------------
BRIDGER    JSR CONTROL
           LDA BRDFLG 
           BNE .1
.2         RTS  
.1         DEC BRDCNT
           BNE .2
           LDA BRDPNT
           CMP #$03
           BNE .3
           LDA #$00
           STA BRDFLG
           STA BRDCNT
           STA BRDPNT
           RTS
.3         ASL
           TAX
           LDA BRDTAB,X
           STA TEMP1
           LDA BRDTAB+1,X
           STA TEMP2
           LDY #$1F
.4         LDA (TEMP1),Y
           STA $70A8,Y
           DEY
           BPL .4
           INC BRDPNT
           RTS
BRDTAB     .DA BRIDGE1
           .DA BRIDGE2
           .DA BRIDGE3
*--------------------------------
* EXPLODE PLANE ON SCREEN
*--------------------------------
PLANEGON   JSR CONTROL
           LDA #$00
           STA HPOS3
           DEC ACTFLG
           LDA #$20
           STA TEMP1
           LDA #$50
           STA CRUD
.1         LDX #$0D
.2         LDA $D20A  
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
           BNE .2
           LDA $D20A
           AND #$3F
           STA $D202
           LDA #$8F
           STA $D203
           LDA TEMP1
           PHA
           LDX #$30
.66        TXA
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
           BNE .66
           PLA
           STA TEMP1
           DEC TEMP1
           BNE .1
           DEC TEMP1
.3         LDA TEMP1
           STA $D202
           LDA TEMP1
           AND #$0F
           BNE .4
           LDA TEMP1
           CMP #$90
           BCS .5
           LDX #$10
.8         LDA $3490,X
           LSR
           STA $3490,X
           LDA $3590,X
           ASL
           STA $3590,X
           DEX
           BNE .8
           BEQ .4
.5         LDX #$0C
.6         LDA $3690,X
           LSR
           STA $3690,X
           LDA $3790,X
           ASL
           STA $3790,X
           DEX
           BNE .6
.4         LDX #$6 
.10        TXA  
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
           BNE .10
           DEC TEMP1      
           BNE .3
           LDA #$00
           STA $D203
           DEC SHIPS
           BNE .11
           JMP ENDGAME
.11        JSR PMAKER
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
*--------------------------------
* MISSLE FIRE ROUTINE
*--------------------------------
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
.1         STA $3600,X
           DEX
           BNE .1
           LDA MISDIR
           BEQ .2
           BMI .3
           INC HPOS3
           BEQ CKMISKL
           BNE .2
.3         DEC HPOS3 
           BEQ CKMISKL
.2         INC SMISY
           INC SMISY
           INC SMISY
           INC SMISY
           LDA SMISY
           CMP #$84
           BCS CKMISKL
           LDX SMISY 
           LDY #$04
           LDA #$08
.5         STA $3600,X 
           DEX 
           DEY
           BNE .5
           LDA #$FF
           STA $2C2
ENDMIS     RTS  
CKMISKL    LDA HPOS3
           SEC
           SBC CROSSX    
           BEQ .2
           BCC .1 
           CMP #$0D
           BCS .3
           JMP .2
.1         EOR #$FF
           CMP #$0C
           BCS .3
.2         JSR PLANEGON  
.3         LDA #$00  
           STA HPOS3
           STA SMISY
           RTS
*--------------------------------
* SAUCDEATH!!!!
* CHECK FOR SAUCER HIT
*--------------------------------
UFODIE     JSR CONTROL
           JSR KILUFO
           LDX #$03  
.1         LDA HIT1,X
           AND #$3
           BNE .2
           DEX
           BPL .1 
           RTS
.2         INC UFOEXP  
           LDA #$00
           STA EXPSND
           LDX #$03
.3         LDA HIT1,X  
           AND #$8
           STA HIT1,X  
           DEX
           BPL .3
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
           BNE .1
.2         RTS   
.1         LDA #$00
           STA SAUCFLG
           INC UFKFLG
           LDA UFKFLG
           CMP #$60
           BNE .15
           LDA #$00
           STA UFKFLG
           LDX #$60
.3         LDA $3400,X  
           ASL 
           STA $3400,X  
           LDA $3500,X 
           LSR
           STA $3500,X 
           DEX
           BNE .3
           DEC UFCNT   
           BNE .15
           LDA #$00
           STA UFOEXP
           LDX #$60
.6         STA $3400,X
           STA $3500,X
           DEX
           BNE .6
           LDX #$03
.14        LDA HIT1,X
           AND #$08
           STA HIT1,X
           DEX
           BPL .14
.15        PLA  
           PLA
           RTS
*--------------------------------
* STAR ROUTINE
*--------------------------------
STARS      INC STRCNT
           LDA STRCNT
           CMP #$02
           BEQ .1
           RTS
.1         LDA #$00
           STA STRCNT
           LDX STRFAS
           STA $73F8,X
           INC STRFAS
           LDA STRFAS
           CMP #$08
           BNE .2
           LDA #$00
           STA STRFAS
           JSR STARPLOT
.2         LDX STRFAS    
           LDA #$0C
           STA $73F8,X  
           RTS
*--------------------------------
* STARPLOT SUBROUTINES
*--------------------------------
STARPLOT   LDX #$0D
.1         LDY STARY,X
           LDA YLOW,Y
           STA IRQVAR1
           LDA YHI,Y
           STA IRQVAR2
           LDY STARX,X
           LDA (IRQVAR1),Y
           BEQ .2
           CMP #$7F
           BEQ .2
           CMP #$FF
           BNE .5
.2         LDA #$00
           STA (IRQVAR1),Y
.5         INC STARY,X
           LDA STARY,X
           CMP #$15
           BNE .6
           LDA #$00
           STA STARY,X
.6         TAY
           LDA YLOW,Y
           STA IRQVAR1
           LDA YHI,Y
           STA IRQVAR2
           LDY STARX,X
           LDA (IRQVAR1),Y
           BNE .7
           LDA #$7F
           BIT $D20A
           BMI .8
           LDA #$FF
.8         STA (IRQVAR1),Y  
.7         DEX   
           BPL .1
           RTS
STARX      .HS 0205020A0C0E131519
           .HS 1D20242628
STARY      .HS 010A07000311020508
           .HS 1304001404
YLOW       .HS 00285078A0C8F018  
           .HS 406890B8E0083058
           .HS 80A8D0F820
YHI        .HS 4040404040404041
           .HS 4141414141424242
           .HS 4242424243

*--------------------------------
* NIGHT 3
* SPACE ROUTINES
*--------------------------------
* SPACE ATTACKER ROUTINES
* ATTACKS PLAYER WITH ANDROID
* SHIPS
*--------------------------------
SPCATK     LDA SPACFLG
           BNE .2
.1         RTS
.2         LDA MUSCNT
           CMP #$1A
           BEQ .3
           CMP #$19
           BNE .5
           LDA LEVEL
           CLC
           ADC #$01
           STA WAVES
.5         LDA #$00   
           STA MUSDEL
           INC MUSCNT
           RTS
.3         LDA #$2
           ADC MUSDEL
           STA MUSDEL
           CMP #$F0
           BCS .4
           RTS
.4         LDA #$00
           STA MUSDEL
           LDA WAVES
           BNE .9
           INC MIKEY2
           LDA MIKEY2
           CMP #$30
           BNE .7
           DEC SPACFLG
.7         RTS
.9         LDA FLYFLG  
           BNE ALLDEAD
*--------------------------------
* IF FLYFLAG NOT SET THEN WE
* INITIALIZE STRING PICK A
* RANDOM PATH AND INITIALIZE
* NUMBER OF PLANES TO KILL
*--------------------------------
           INC FLYFLG
           LDA $D20A
           AND #$07
           CMP #$05
           BCC .6
           LSR
.6         ASL
           STA PATHPNT
           LDA #$00
           STA PSTRING
           STA PSTATUS
           LDX #$5F
.8         LDA SPACL1,X
           STA $7380,X
           DEX
           BPL .8
*--------------------------------
* PLOT SHIPS
*--------------------------------
ALLDEAD    LDA PSTATUS
           CMP #$FF
           BNE .1
           DEC WAVES
           BNE .2
           RTS
.2         DEC FLYFLG
           RTS
.1         LDX PATHPNT
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
PUTPLN     JSR ERASEPLN   
           LDY PSTRING
           LDA (TEMP1),Y
           CMP #$FF
           BEQ .4
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
           BNE .3
           LDA SMISY
           ORA WRNCNT 
           ORA SAUCFLG
           BNE .3
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
.3         PLA
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
.4         DEC FLYFLG
           RTS
*--------------------------------
* ERASE PLANES
*--------------------------------
ERASEPLN   LDY PSTRING
           BEQ .1
           DEY
           LDA (TEMP1),Y
           CMP #$FF
           BEQ .1
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
.1         RTS
*--------------------------------
* SPACE COLLISION HANDLER
*--------------------------------
SPACKIL    LDA #$00  
           TAY
           TAX
.1         LDA PLANCHR,X
           CMP #$FF
           BEQ .6
           CMP (IRQVAR1),Y
           BEQ .2
           INX
.6         CMP #$FF 
           BNE .1
           JMP COLREND
.2         LDA IRQVAR1
           SEC
           SBC PLANDIF,X 
           STA IRQVAR1
           BCS .3
           DEC IRQVAR2
.3         LDX #$00
.4         LDA PLANFIND,X 
           BEQ .5
           INX
           CMP #$FF
           BNE .4
           JMP COLREND
.5         LDA #$5 
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
.99        LDA BSCOR0
           CLC 
           ADC #$64 
           STA BSCOR0
           LDA BSCOR1  
           ADC #$00
           STA BSCOR1
           DEX
           BNE .99
           JMP COLREND
PLANCHR    .HS 7071727374757677
           .HS 78797A7BFF
PLANDIF    .HS 0001282900012829
           .HS 00012829FF
PLANFIND   .HS 0000000000000000
           .HS FF
PLANTIME   .HS 0000000000000000
           .HS 0000000000000000
*--------------------------------
* KILLER2 KILLS SPACE CHARACTERS
*--------------------------------
KILLER2    LDA SPACFLG
           BNE STYX
           RTS
STYX       INC MIKEY
           LDA MIKEY
           CMP #$20
           BEQ .1
           RTS
.1         LDA #$00
           STA MIKEY
           TAX
.2         LDA PLANFIND,X
           CMP #$FF
           BNE .3
           RTS
.3         CMP #$00
           BNE .4
           INX
           BNE .2
.4         STX TEMP3
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
           BNE .5
           BEQ .6
.5         LDA #$30
.6         LDY #$00
           STA (TEMP1),Y
           INY
           STA (TEMP1),Y
           LDY #$28
           STA (TEMP1),Y
           INY
           STA (TEMP1),Y
           INX
           JMP .2
*--------------------------------
* MX SHOOTS MISSLES
*--------------------------------
MX         INC MXDELAY
           LDA MXDELAY
           CMP #$10
           BEQ .4
           RTS
.4         LDA #$00
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
           BCC .1
           INC TEMP2
.1         INC TEMP2
           LDY #$27
.2         LDA (TEMP1),Y
           CMP #$E6
           BEQ .3
           DEY
           BPL .2
           RTS
.3         TYA
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
.2         INX
           LDA MXDAT,X  
           STA $374B,Y
           INY
           CPX MXSCRL
           BNE .2
           INC MXSCRL
           RTS
MX3        INC $2C3
           LDX #$00
.1         LDA $3700,X  
           STA $36FF,X  
           INX
           CPX #$60
           BNE .1
           INC MXSCRL
           LDA MXSCRL
           CMP #$FF
           BEQ .2
           STA $D202
           LDA #$8F
           STA $D203
           RTS
.2         DEC MXFLAG
           LDA #$50
           STA $D202
           LDA #$88
           STA $D203
           RTS
*--------------------------------
* MXKILL CHECK FOR MX DEATH!
*--------------------------------
MXKILL     LDA MXDEATH
           BNE .4 
           LDX #$03
.1         LDA HIT1,X
           AND #$08
           BNE .2
           DEX
           BPL .1
           RTS
.2         LDX #$03
.3         LDA HIT1,X
           AND #$07
           STA HIT1,X
           DEX
           BPL .3
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
*--------------------------------
* KILL MISSLE!
*--------------------------------
.4         LDA #$50
           STA $D202
           LDA #$88
           STA $D203
           LDX #$60  
.5         LDA $3700,X
           BNE .55
           DEX
           BNE .5
.55        LDY #$0E
.56        DEX
           LDA $D20A
           AND MXDAT,Y
           STA $3700,X  
           DEY
           BPL .56 
           DEC MCNT
           BNE .15
           LDA #$00
           STA MXDEATH
           LDX #$60
.8         STA $3700,X
           DEX
           BPL .8
.15        PLA
           PLA
           RTS
MXDAT      .HS 1010103838383838 
           .HS 3838387C7C4444
*--------------------------------
* KIL BASER
*--------------------------------
EXPLOB     LDA BASDEAD
           CMP #$03
           BEQ .1
           RTS
.1         LDA #$00
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
.2         STA $48C0,X
           STA $49C0,X
           STA $4AC0,X
           INX
           INX
           BNE .2   
           LDA #$CF 
           STA $D201
           LDA #$8F
           STA $D203
           LDX #$FF
.3         STX $D200
           TXA
           PHA
           EOR #$FF
           STA $D202
           TXA
           AND #$0F
           BNE .7
           LDX #$15
           LDA #$C0
           STA TEMP1
           LDA #$48
           STA TEMP2
.4         LDY #$00
.6         LDA (TEMP1),Y
           DEY
           STA (TEMP1),Y
           INY
           INY
           CPY #$15
           BCC .6
           LDY #$28 
.5         LDA (TEMP1),Y
           INY
           STA (TEMP1),Y
           DEY
           DEY
           CPY #$14
           BCS .5
           LDA TEMP1
           CLC
           ADC #$28
           STA TEMP1
           LDA TEMP2
           ADC #$00
           STA TEMP2
           DEX
           BNE .4
           LDX #$28
           LDY #$00
.10        LDA $49C0,Y
           STA $48C0,Y
           LDA $4AC0,Y
           STA $49C0,Y
           LDA $4BC0,Y
           STA $4AC0,Y
           LDA #$00
           STA $4BC0,Y
           INY
           BNE .10
.7         LDY #$00
           LDX #$05
.20        DEY
           BNE .20
           DEX
           BNE .20 
           PLA
           TAX
           DEX
           BNE .3
           LDA #$00
           STA $D201
           STA $D203
           STA $2C8
           LDA #$2C
           STA $2F4
BONUS      LDX #$0E
.1         LDA BONSTR,X
           STA $4995,X
           DEX
           BPL .1
           LDX #$0F 
.2         LDY MUSDATA,X
           STY $D200
           DEY
           STY $D202
           LDA #$AF
           STA $D201
           STA $D203
           INY
           CPY #$00
           BNE .55
           STY $D201
           STY $D203
.55        LDA MUSDLY,X
           LDY #$00
.3         DEY
           BNE .3
           SEC
           SBC #$01
           BNE .3
           DEX
           BPL .2
           LDX #$10
           JSR DELAY
           LDA #$01
           STA $49EC
           STA $49ED
           STA $49EE
           LDY LEVEL
           INY
           LDX #$02 
.21        STX $49EB
           LDA BSCOR0
           CLC
           ADC #$E8
           STA BSCOR0
           BCC .22
           INC BSCOR1
.22        INC BSCOR1
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
           BNE .21
           LDX #$50
           JSR DLONG
           LDA LEVEL
           CMP #$04
           BNE .87
           INC SHIPS
.87        CMP #$05
           BEQ .9
           INC LEVEL
.9         LDA #$58
           STA LIST2+3
           LDA #$4C
           STA LIST2+4
           LDA #$50
           STA FUEL
           JSR MAPFIL
           JSR INITVAR
           LDX #$0C
           JMP CURRAN
BONSTR     .HS 0F180F1723000E0F1D1E1C19230F0E
MUSDATA    .HS 00580068005800460068007300750080
MUSDLY     .HS 0A640A7D0A7D0A7D0A7D0A640AAF0A64
*--------------------------------
* BASKILER BAS CHARACTER KILL ROUTINES!
*--------------------------------
BASKILER   LDA (IRQVAR1),Y
           CMP #$3F
           BEQ .2
           CMP #$40
           BEQ .1
           JMP RETRY
.1         LDA IRQVAR1
           SEC
           SBC #$01
           STA IRQVAR1
           BCS .2
           DEC IRQVAR2
.2         LDA #$B0
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
BASOLD     .HS 0000
           .HS 0000
           .HS 0000
*--------------------------------
* ENDGAME
*--------------------------------
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
.3         STA $3400,X
           STA $3500,X
           STA $3600,X
           STA $3700,X
           DEX
           BNE .3
           LDX #$5F
.1         LDA GDAT,X
           STA $3420,X
           LDA ADAT,X
           STA $3520,X 
           LDA MDAT,X  
           STA $3620,X
           LDA EDAT,X
           STA $3720,X
           DEX
           BPL .1
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
*--------------------------------
* GAME OVER DATA
*--------------------------------
GDAT       .HS FCFCFCFCFC808080
           .HS 8080808080808080
           .HS 8080808080808080
           .HS 9C9C9C9C9C848484
           .HS 848484848484FCFC
           .HS FCFCFC0000000000
           .HS 0000000000FCFCFC
           .HS FC84848484848484
           .HS 8484848484848484 
           .HS 8484848484848484 
           .HS 8484848484848484
           .HS 84848484FCFCFCFC
ADAT       .HS 1818181824242424
           .HS 4242424242424242
           .HS 424242424242427E
           .HS 7E7E7E4242424242
           .HS 4242424242424242
           .HS 4242420000000000
           .HS 0000000000818181
           .HS 8181818181818181
           .HS 8181818181424242
           .HS 4242424242422424
           .HS 2424242424242424
           .HS 1818181818181800
MDAT       .HS C3C3C3C3A5A5A5A5
           .HS 9999999999999999
           .HS 9999999999999999
           .HS 9999999999998181
           .HS 8181818181818181
           .HS 8181810000000000
           .HS 0000000000FCFCFC
           .HS FC80808080808080
           .HS 80808080808080F0
           .HS F0F0F08080808080
           .HS 8080808080808080
           .HS 80808080FCFCFCFC
EDAT       .HS FCFCFCFC80808080
           .HS 8080808080808080
           .HS 8080F0F0F0F08080
           .HS 8080808080808080
           .HS 80808080808080FC
           .HS FCFCFC0000000000
           .HS 0000000000F8F8F8
           .HS F8F8888888888888
           .HS 8888888888F8F8F8
           .HS F8F8F8A0A0A0A0A0
           .HS A0A0909090909090
           .HS 8888888888848484
           .HS 8400000000000000
*--------------------------------
* PAUSER ROUTINE CHECK FOR
* PAUSE AND GAME RESTART!
*--------------------------------
PAUSER     LDA CONSOL
           ROR
           BCS .1
           JMP WARMSTART
.1         ROR
           ROR
           BCC .2
           RTS
.2         LDA CONSOL
           AND #$04
           BEQ .2 
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
.3         LDA $2FC
           CMP #$FF
           BEQ .3
           PLA
           STA ACTFLG
           PLA
           STA MOVFLG
           LDA #50
           STA $D202
           LDA #$83
           STA $D203
           RTS
*--------------------------------
* FIREPOWER! FIRE FROM BASE AND
* FIRE FROM TANKS!
*--------------------------------
FIREPOWER  INC CNTFIRE
           BPL .9 
           LDA #$00
           STA CNTFIRE
           LDA SMISY
           ORA WRNCNT
           ORA SAUCFLG
           BEQ .1
.9         RTS 
.1         LDA LIST2+3
           CLC
           ADC #$C8
           STA TEMP1
           LDA LIST2+4
           ADC #$00
           STA TEMP2
           LDX #$08 
.2         LDY #$27
.3         LDA (TEMP1),Y
           CMP #$10
           BEQ .4
           CMP #$40
           BEQ .4
           DEY
           BPL .3
           LDA TEMP1
           CLC
           ADC #$28
           STA TEMP1
           LDA TEMP2
           ADC #$00
           STA TEMP2
           DEX
           BNE .3
           RTS
.4         DEY
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
*--------------------------------
* SUBROUTINES FOR NIGHTRAIDERS
*--------------------------------
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
.1         STA $4000,X
           STA $4100,X
           STA $4200,X
           DEX
           BNE .1
           LDX #$6F
.2         STA $4300,X
           DEX
           BNE .2
           RTS
*--------------------------------
* SETUP GUAGE SCREEN
*--------------------------------
SETSCREEN  LDX #$4F 
SETS2      LDA BSCR-1,X
           STA $3E5F,X
           DEX
           BNE SETS2
           RTS
BSCR       .HS 001D0D191C0F0000
           .HS 0000000000000000
           .HS 00001D12131A1D00
           .HS 0000000000000000
           .HS 0000000000000000
           .HS 00909F8F96000000
           .HS 0000000000000000
           .HS 0000000000000000
           .HS 0000000000000000
           .HS 0000000000000000
*--------------------------------
* INITIALIZE GAME VARIABLES
*--------------------------------
INIT   LDA #$00
ILOOP  STA $0,X
       INX
       BNE ILOOP
       STA $D40E
       STA $D008
       STA $D009
       STA $D00A
       STA $D00B
       LDA #RTEND
       STA VBLK  
       STA COLLAD
       LDA /RTEND
       STA VBLK+1   
       STA COLLAD+1
       LDA #$40
       STA NMIEN
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
*--------------------------------
* MAKE PLANE
*--------------------------------
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
*--------------------------------
* RTEND RESTORE REGISTERS
* AFTER INTERRUPT
*--------------------------------
RTEND  PLA
       TAY
       PLA
       TAX
       PLA
NOINT  RTI
*--------------------------------
* CLEAR PLAYER MISSLE AREA
*--------------------------------
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
*--------------------------------
* LONG DELAY ROUTINE
*--------------------------------
DLONG  STX TEMP2
       JSR DELAY
       LDX TEMP2
       DEX
       BNE DLONG
       RTS
*--------------------------------
* DELAY ROUTINE
*--------------------------------
DELAY  LDX #$10
DELAY1 LDY #$FF
DELAY2 DEY
       BNE DELAY2
       DEX
       BNE DELAY1
       RTS
*--------------------------------
* PRINT ROUTINE
*--------------------------------
PRINT  STY TEMP2
       LDA #WORDS
       STA TEMP3
       LDA /WORDS
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
PRINT4 LDA #SCREEN
       STA TEMP5
       LDA /SCREEN 
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
*--------------------------------
* DISPLAY LISTS
*--------------------------------
LIST2  .HS 70F06400402424242424
       .HS 24242424242424
       .HS A4242424242404 
       .HS A045603E05204A 
       .HS 403F41  
       .DA #LIST2
       .DA /LIST2
LIST1  .HS 7060904F
       .DA #NIGHTDAT
       .DA /NIGHTDAT
       .HS 0F0F0F0F0F0F0F0F
       .HS 0F0F0F0F0F0F30440040D0
       .HS 05050404040404048404
       .HS 040404040404040441
       .DA #LIST1   
       .DA /LIST1   
*--------------------------------
* DATA TABLE FOR HI-RES NIGHTRAIDER!
*--------------------------------
NIGHTDAT   .HS 0000000000000000                
           .HS 00000000000000000000000000000000
           .HS 00000000000000000000000000000000
           .HS 0000000000000000000060607E03FC18
           .HS 187FE1FF00F007E07FC1FF87FC0FF000
           .HS 00000000000000000000000000000000
           .HS 0000C0C0300E1C3030CCC30703F00300
           .HS 31C0C30C1C3870000000000000000000
           .HS 00000000000000000001C18060181860
           .HS 619986060E7006006181861818606000
           .HS 00000000000000000000000000000000
           .HS 0003C300C03000C0C0300C0C38700C00
           .HS C303003030C000000000000000000000
           .HS 00000000000000000007C60180600181
           .HS 806018386060180186060060E1C00000
           .HS 00000000000000000000000000000000
           .HS 000DCC0300C003FF00C03FE0C0C03003
           .HS 0C0F00FF81FC00000000000000000000
           .HS 00000000000000000019D806018F87FE
           .HS 01807F8181806006181E01FE01FC0000
           .HS 00000000000000000000000000000000
           .HS 0031F00C031F0C0C0300DC03FF00C00C
           .HS 30300370001C00000000000000000000
           .HS 00000000000000000061E01806061818
           .HS 06019C07FE0180186060067000180000
           .HS 00000000000000000000000000000000
           .HS 00C1C0300C0C30300C031C0C0C030030
           .HS C0C30C70303000000000000000000000
           .HS 0000000000000000018180601C186060
           .HS 18061C18180600638186187070E00000
           .HS 00000000000000000000000000000000
           .HS 030303F01FF0C0C0FC0C1C30303F03FE
           .HS 0FFC30707F8000000000000000000000
           .HS 0000000000000000060607E01FE18181
           .HS F8181860607E07F81FF860607E000000
           .HS 00000000000000000000000000000000
           .HS 00000000000000000000000000000000
           .HS 00000000000000000000000000000000
           .HS 00000000000000000000000000000000
           .HS 00000000000000000000000000000000
           .HS 0000000000000000

*--------------------------------
* INTERRUPT ROUTINES FOR
* NIGHTRAIDER
*--------------------------------
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
.1     LDA $3490,X
       STA $34A0,X
       LDA $3590,X
       STA $35A0,X
       LDA $3690,X
       STA $36A0,X
       LDA $3790,X
       STA $37A0,X
       DEX
       BPL .1
       LDA #IRQ2   
       STA VDLST
       LDA /IRQ2
       STA VDLST+1
       JMP RTEND
*--------------------------------
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
NOTP   LDA $D40B
       CMP #$50
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
       LDA #IRQ3
       STA VDLST
       LDA /IRQ3
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
.1     LDA $D000,X 
       AND #$0E
       BNE NOAH
       DEX
       BPL .1
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
*--------------------------------
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
       BCS .1
       INC SPARE
       LDA SPARE
       CMP #$05
       BNE .1
       LDA #$00
       STA SPARE
       LDX #$3
.2     LDA $3E89,X
       EOR #$80
       STA $3E89,X
       DEX
       BPL .2
.1     LDA #IRQ1  
       STA VDLST
       LDA /IRQ1
       STA VDLST+1
       JMP RTEND
*--------------------------------
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
       BPL .1
       LDA #$06
       STA PNTBAS
.1     ASL
       TAX
       LDA BASLOK,X
       STA IRQVAR1
       LDA BASLOK+1,X
       STA IRQVAR2
       LDY #$0F
.2     LDA (IRQVAR1),Y
       STA $71F8,Y
       DEY
       BPL .2
NOMOV2 LDX #$10
RANLOP LDA $D20A
       EOR $D40B
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

*--------------------------------
* DATA STORAGE AND VARIABLES
*--------------------------------
DATA
*--------------------------------
VDCNT      .HS 00
SPARE      .HS 00
WINDOWVAR  .HS 00
RADARVAR   .HS 00
DELAYER    .HS 00
TANKFLAG   .HS 00
TANKVAR    .HS 00
DELBAS     .HS 00
GUNSX      .HS 0000
GUNSX2     .HS 0000
GUNSY      .HS 0000
PRESS      .HS 00
HOLDER     .HS 00
SNDFLG     .HS 00
KILCNT     .HS 00
SPACFLG    .HS 00
BASFLG     .HS 00
BASER      .HS 00
TRNCNT     .HS 00
TRNFLG     .HS 00
TRNPNT1    .HS 00
TRNPNT2    .HS 00
TRNVAR1    .HS 00
TRNVAR2    .HS 00
SMISY      .HS 00
SAUCT      .HS 00
SAUCPNT    .HS 00
SAUCPNT2   .HS 00
SAUCFLG    .HS 00
SAUCDIR    .HS 00
SAUCY      .HS 00
SAUCNT     .HS 00
WRNCNT     .HS 00
VOLUM      .HS 00
VOLFLG     .HS 00
SNDCNT     .HS 00
BRDFLG     .HS 00
BRDPNT     .HS 00
CRUD       .HS 00
MISDIR     .HS 00
MISCNT     .HS 00
UFOEXP     .HS 00
UFKFLG     .HS 00
UFCNT      .HS 00
STRCNT     .HS 00
STRFAS     .HS 00
MUSCNT     .HS 00
MUSDEL     .HS 00
PSTRING    .HS 00
PSTATUS    .HS 00
FLYFLG     .HS 00
PATHPNT    .HS 00
WAVES      .HS 00
MIKEY      .HS 00
MIKEY2     .HS 00
MXDELAY    .HS 00
MXFLAG     .HS 00
MXSCRL     .HS 00
MXDEATH    .HS 00
MCNT       .HS 00
BASDEAD    .HS 00
CNTFIRE    .HS 00
*--------------------------------
* CHARACTER HIT TABLES
* USED BY THE CHARATER DESTROY
* ROUTINES
* EACH OBJECT IT IS RESERVED
* FIVE BYTES IN MEMORY
* BYTE 0 = EXPLOSION STATUS
* BYTE 1 & 2 = ADRESS HIT AT
* BYTE 3 = CHARACTER HIT   
* THERE IS ENOUGH ROOM FOR 
* TWENTY EXPLOSIONS AT ONCE
* THAT SHOULD BE ENOUGH!
*--------------------------------
HITABLE    .HS 0000000000
           .HS 0000000000
           .HS 0000000000
           .HS 0000000000
           .HS 0000000000
           .HS 0000000000
           .HS 0000000000
ENDAT      .HS FF
*--------------------------------
* NONZERO VARIABLES TO BE
* FILLED
*--------------------------------
NONDAT
*--------------------------------
TPL        .HS 4040
SNDFLG2    .HS 01
PNTBAS     .HS 07
FLYCNT     .HS 01
FLCNT      .HS 01
FLCNT2     .HS 1C
SPS1       .HS FF
SPS2       .HS A0
MISAUC     .HS 50
EXPSND     .HS FF
BRDCNT     .HS FF
MUSCOM     .HS FF
NONEND     .HS 00
*--------------------------------
STRGFIL    .HS 4040010701011CFFA050FFFFFF
*--------------------------------
INITVAR    LDX #ENDAT-DATA-1
           LDA #$00
.1         STA DATA,X
           DEX
           BPL .1
           LDX #$0C 
.2         LDA STRGFIL,X 
           STA NONDAT,X
           DEX
           BPL .2
           RTS

*--------------------------------
* SHAPETABLES
*--------------------------------
WINFIRE    .HS AAB6AE92AABAB2A6
WINFIRE2   .HS AAA6B2BAAAB6AE92
*--------------------------------
* UPDATES CHARACER $0D FOR
* WINDOW FIRES!
*--------------------------------
TANK1      .HS 0005000001070606
           .HS 0A550A6AA8A29541
           .HS A0A0669A69904040
TANK2      .HS 0000000001070606  
           .HS 0A550A6AA8A29541 
           .HS A0A0669A69904040
TANK3      .HS 0000000001070606
           .HS 0A050A6AA8A29541 
           .HS A0A0669A69904040
TANK4      .HS 0000000001070606 
           .HS 0A0A0A6AA8A29541 
           .HS A055669A69904040
*--------------------------------
* TANKS FOR SHOW!
*--------------------------------
*--------------------------------
*--------------------------------
BRIDGE1    .HS FFFFFFFFFFFFFFFF
           .HS 00FFFAAAAAAAAAAA
           .HS 0000F30000000000
           .HS 00FF0F0000000000
BRIDGE2    .HS FFFFFFFFFFFFFFFF
           .HS 0000F0AFAAAAAAAA
           .HS 00000000F0000F00
           .HS 000F0F00F0000000
BRIDGE3    .HS FFFFFFFFFFFFFFFF
           .HS 00000000000045A9
           .HS 0000000000483600
           .HS 000F0F00F0000000
*--------------------------------
RADAR1     .HS 0A1556150A3F0F03
           .HS A0549554A0A8A080
RADAR2     .HS 00010615163F0F03
           .HS 505868A0A0A8A080
RADAR3     .HS 00020A2A0E3F0F03
           .HS 80A0A080A0A8A080
RADAR4     .HS 022AAA2A0F3F0F03
           .HS A0A8A0A0A0A8A080
RADAR5     .HS 2AAA2A0A0F3F0F03
           .HS A8AAA8A0A0A8A080
RADAR6     .HS 0A2A0A0A0F3F0F03
           .HS 80A8AAA8A0A8A080
RADAR7     .HS 020A0A020F3F0F03
           .HS 0080A0A8A0A8A080
RADAR8     .HS 0525290A0A3F0F03
           .HS 0040905454A8A080
*--------------------------------
* SHAPE VECTOR LOOKUP TABLES
*--------------------------------
WINDOWS    .DA WINFIRE
           .DA WINFIRE2
*--------------------------------
TANKS      .DA TANK1
           .DA TANK2
           .DA TANK3
           .DA TANK4
*--------------------------------
BRIDGES    .DA BRIDGE1
           .DA BRIDGE2
           .DA BRIDGE3
*--------------------------------
RADARS     .DA RADAR1
           .DA RADAR2
           .DA RADAR3
           .DA RADAR4
           .DA RADAR5
           .DA RADAR6
           .DA RADAR7
           .DA RADAR8
*--------------------------------
* ATTACK CHARACTERS FOR 
* NONSPACE ATTACK!
*--------------------------------
SAUCER1    .HS 1800FFFF7E3C2442
           .HS 003C000300002442
SAUCER2    .HS 1800FFFF7E3C2442
           .HS 183C000C00002442
SAUCER3    .HS 1800FFFF7E3C2442
           .HS 003C003000002442
SAUCER4    .HS 1800FFFF7E3C2442
           .HS 183C00C000002442
SAUCER5    .HS 544CB3B33270680E
           .HS 54704C404C4C680E
SAUCERC    .HS 4484 ;COLORS
*--------------------------------
* CHARACTER LOOKUP TABLE
* TELLS HOW TO DRAW SAPE ETC.
*--------------------------------
CHATBL     .HS 2A2B2223242526
           .HS 2728290E0F1065
           .HS E667FF  
CHTBL2     .HS 00010001020328
           .HS 292A2B00010200
           .HS 010200
CHTBL3     .HS 111213149A9B
           .HS 9C9D9EFF
EXPTBL     .HS 2A220E65
EXPTBL2    .HS 00290000
EXPTBL3    .HS E8E9EAEBECEDEEEF 
*--------------------------------
* POINT VALUE TABLE
*--------------------------------
PNTBL      .HS 2AC800
           .HS 226400  
           .HS 0E2C01
           .HS 659001 
           .HS FF
*--------------------------------
* SPACE ATTACK SHAPES
*--------------------------------
SPACL1     .HS 00000000000828A8
           .HS 0000000000000030
           .HS 2AFE3E0F00000000
           .HS F0B0A0A4A5250A00
*--------------------------------
SPACL2     .HS 000000020202FA35
           .HS 000000808080AF5C
           .HS 0A02000000000000
           .HS A080000000000000
*--------------------------------
SPACL3     .HS 000000000000000C
           .HS 000000000020282A
           .HS 0F0E0A1A5A58A000
           .HS A8BFBCF000000000
*--------------------------------
* SPACE PATH TABLES 
* THERE ARE 14
* THERE IS AN XPATH A YPATH AND
* A SHPTABLES FOR EVERYONE
*--------------------------------
XPATH1     .HS 11100F0E0D0D0D0E
           .HS 0F10111213141414
           .HS 15161718191A1B1C
           .HS 1D1E1F20212223FF
YPATH1     .HS 0001020304040404
           .HS 0404040404040404
           .HS 05060708090A0B0C
           .HS 0D0E0F10111213FF
SHPTAB1    .HS 7878787874747474
           .HS 7474747474747474
           .HS 7070707070707070
           .HS 70707070707070FF
*--------------------------------
XPATH2     .HS 0708090A0B0B0B0B
           .HS 0A09080706050505
           .HS 0403020100FF
YPATH2     .HS 0001020304040404
           .HS 0404040404040404
           .HS 0506070809FF
SHPTAB2    .HS 7070707070747474
           .HS 7474747474747474
           .HS 7878787878FF
*--------------------------------
XPATH3     .HS 0304050607070707
           .HS 08090A0B0C0D0E0F
           .HS 10111213FF
YPATH3     .HS 0001020305050505
           .HS 060708090A0B0C0D
           .HS 0E0F1011FF
SHPTAB3    .HS 7070707070747474
           .HS 7070707070707070
           .HS 70707070FF
*--------------------------------
XPATH4     .HS 2626252423222120
           .HS 1F1E1D1C1B1A1918
           .HS 171615141312FF
YPATH4     .HS 0405050505050505
           .HS 060708090A0B0C0D
           .HS 0E0F10111213FF
SHPTAB4    .HS 7878747474747478
           .HS 7878787878787878
           .HS 787878787878FF
*--------------------------------
XPATH5     .HS 0B0C0D0E0F0F0F0F
           .HS 0E0D0C0B0B0B0B0C
           .HS 0D0E0F1011121314
           .HS 1516FF
YPATH5     .HS 0001020304040404
           .HS 0506070808080809
           .HS 0A0B0C0D0E0F1011
           .HS 1213FF
SHPTAB5    .HS 7070707070747474
           .HS 7878787874747470
           .HS 7070707070707070
           .HS 7070FF
*--------------------------------
* PATHPOINTERS
*--------------------------------
PATHX      .DA XPATH1
           .DA XPATH2
           .DA XPATH3
           .DA XPATH4
           .DA XPATH5
*--------------------------------
PATHY      .DA YPATH1
           .DA YPATH2
           .DA YPATH3
           .DA YPATH4
           .DA YPATH5
*--------------------------------
PSHIP      .DA SHPTAB1
           .DA SHPTAB2   
           .DA SHPTAB3
           .DA SHPTAB4
           .DA SHPTAB5
*--------------------------------
* BASE STATION CANNON SHAPES
*--------------------------------
BASCAN1    .HS 000F0F0F0F01010100F0F0E0F8424040
BASCAN2    .HS 000F0F0E0F01010100F0F0B0B0704040
BASCAN3    .HS 000F0F0E0E09010100F0F0B0F0404040
BASCAN4    .HS 000F0F0B2F81010100F0F0F0F0404040
*--------------------------------
* SHAPE MOVEMENT LOOKUP TABLES
*--------------------------------
BASLOK     .DA BASCAN1
           .DA BASCAN2
           .DA BASCAN3
           .DA BASCAN4
           .DA BASCAN3
           .DA BASCAN2
           .DA BASCAN1
