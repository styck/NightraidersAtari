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
