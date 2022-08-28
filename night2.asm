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
           .LOCAL
GM1        LDA COLRUT&255   ;SETUP
           STA COLLAD       ;COLLISION
           LDA COLRUT/255   ;ROUTINE
           STA COLLAD+1     ;VECTOR
GMLOOP     JSR PAUSER
           LDA BASER  
           BEQ ?99
           JSR EXPLOB
?99        JSR TRAINER 
           JSR BRIDGER
           JSR MX
           JSR SOUND
           JSR MISSLES
           JSR SPCATK
           JSR UFODIE
           JSR ATTACK 
           JSR FIREPOWER
           LDA SPACFLG 
           BEQ ?1
           JSR KILLER2
           JMP GMLOOP
?1         JSR KILLER  
           JMP GMLOOP
;--------------------------------
; SUBROUTINE KILLER
; EXPLODES KILLED OBJECTS
; AND HANDLES DEATH!
;--------------------------------
           .LOCAL
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
           BCS ?2
           DEC TEMP2
?2         LDY #$20
?3         LDA (TEMP1),Y
           BEQ ?4
           CMP #$19   
           BEQ ?4
           CMP #$9E
           BEQ ?4
           DEY
           BNE ?3
           JMP KIL8
?4         INY
           LDA #$9B
           STA (TEMP1),Y
?5         LDA (TEMP1),Y
           CMP #$11
           BEQ ?6
           CMP #$19
           BEQ ?6
           CMP #$9A
           BEQ ?6
           LDA #$00
           STA (TEMP1),Y
           INY
           BNE ?5  
?6         LDA #$00
           STA TRNFLG
           LDA LEVEL
           ASL
           CLC
           ADC BSCOR1
           STA BSCOR1
           JMP KIL8
           .LOCAL
KBRIDGE    LDA TEMP1
           SEC
           SBC #$20
           STA TEMP1
           BCS ?1
           DEC TEMP2
?1         LDY #$20
?2         LDA (TEMP1),Y
           CMP #$11
           BEQ ?3
           DEY
           BNE ?2
?3         LDA #$15
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
           BCC ?4
           INC BSCOR1
?4         INC BRDFLG
           JMP KIL8
KIL11      LDY EXPTBL2,X
           LDX TEMP4  
           LDA EXPTBL3,X
           STA (TEMP1),Y
           INY
           LDA EXPTBL3+1,X
           STA (TEMP1),Y
           JMP KIL8
           .LOCAL
KIL12      LDA HITABLE+3,X 
           LDX #$00
?1         CMP PNTBL,X
           BEQ ?2
           INX
           BNE ?1
?2         PHA
           LDA PNTBL+1,X
           CLC
           ADC BSCOR0
           STA BSCOR0
           LDA PNTBL+2,X
           ADC BSCOR1
           STA BSCOR1
           PLA
           LDX #$00
?3         CMP EXPTBL,X   
           BEQ KIL13
           INX
           BNE ?3
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
           .LOCAL
CONTROL    LDA BASER
           BEQ ?44 
           JMP CON4
?44        LDA SPACFLG
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
           .LOCAL
CON2       LDA #$40
           STA TEMP2
           LDA #$00
           STA TEMP1
           LDY #00
?1         TYA
?2         STA (TEMP1),Y
           DEY
           BNE ?2
           INC TEMP2
           LDA TEMP2
           CMP #$50
           BNE ?1
           LDX #$00
?3         LDA $6000,X
           STA $48C0,X
           LDA $6100,X
           STA $49C0,X
           LDA $6200,X
           STA $4AC0,X
           DEX
           BNE ?3
           LDX #$6F
?4         LDA $6300,X
           STA $4BC0,X
           DEX
           BNE ?4
           LDA #$58
           STA LIST2+3    
           LDA #$4C
           STA LIST2+4
           LDA #$FF
           STA BASER  
           LDA BASDEAD
           BEQ ?5
           ASL
           TAX
?6         LDA BASOLD-2,X 
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
           BNE ?6
?5         RTS
           .LOCAL
CON4       LDA LIST2+3
           CMP #$78
           BNE ?1
           LDA LIST2+4
           CMP #$45
           BNE ?1
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
?2         STA MUSCNT,X
           DEX
           BPL ?2
?1         RTS
;--------------------------------
; SUBROUTINE TRAINER
; CHECKS FOR BRIDGES ON THE SCREN
; AND ROLLS TRAINS ACROSS THEM
;--------------------------------
           .LOCAL
TRAINER    JSR CONTROL
           INC TRNCNT  
           LDA TRNCNT
           CMP #$30 
           BEQ ?1
           RTS
?1         LDA #$00
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
           .LOCAL
TRAIN5     LDA TRNVAR1
           STA TEMP1
           LDA TRNVAR2
           STA TEMP2
           LDA TRNPNT1
           CMP #$27
           BEQ TRAIN6
           LDY TRNPNT1
           LDX TRNPNT2   
?1         CPX #$FF
           BEQ ?2 
           LDA TRNSTR,X
           DEX
           JMP ?3
?2         LDA #$19
?3         STA (TEMP1),Y
           DEY
           BPL ?1
           INC TRNPNT1
           RTS
           .LOCAL
TRAIN6     LDY #$27
           LDX TRNPNT2
           BMI TRAIN7
?1         CPX #$FF
           BEQ ?2
           LDA TRNSTR,X
           DEX
           JMP ?3
?2         LDA #$19
?3         STA (TEMP1),Y
           DEY
           BPL ?1
           DEC TRNPNT2 
           RTS
           .LOCAL
TRAIN7     LDY #$27
           LDA #$19
?1         STA (TEMP1),Y
           DEY
           BPL ?1
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
           .LOCAL
ATTACK     JSR CONTROL
           DEC FLCNT2
           BNE ?3
           LDA #$1C
           STA FLCNT2
           DEC FLCNT 
           BNE ?3
           LDA CRUD
           BNE ?3
?2         LDA LEVEL
           ASL
           ASL
           ASL
           STA TEMP1
           LDA #59
           SEC
           SBC TEMP1
           STA FLCNT
           DEC FUEL 
           BNE ?3
           JSR PLANEGON   
?3         LDA SPACFLG  
           BEQ ?6
           LDA WRNCNT
           BNE ?6
           LDA SAUCFLG
           BNE ?5
?6         LDA SAUCFLG
           BNE ?5
           LDA SPS1  
           INC LEVEL 
           INC LEVEL 
           SEC
           SBC LEVEL
           DEC LEVEL 
           DEC LEVEL
           STA SPS1
           BCS ?1
           DEC SPS2
           BNE ?1
           LDA #$A0
           STA SPS2
           LDA #$FF
           STA SPS1
           INC SAUCFLG  
           LDY #$00
           LDX #$00
           LDA $D20A
           BMI ?4
           INX
           LDY #$FF
?4         STX SAUCDIR   
           STY HPOS1
           STY HPOS2
           LDA #$35
           STA SAUCY
           LDA #$20
           STA WRNCNT  
           LDX #$60
           LDA #$00
?20        STA $3400,X
           STA $3500,X
           DEX
           BNE ?20
?1         RTS

?5         LDA WRNCNT
           BEQ MOVSAUC 
           INC FLYCNT
           LDA FLYCNT
           CMP #$50
           BNE ?1
           LDA #$00
           STA FLYCNT
           LDA WRNCNT
           AND #$01
           BEQ TONE2
           LDA #25
           BNE TONE1
           .LOCAL
TONE2      LDA #100
TONE1      STA $D206  
           LDA #$AF
           STA $D207
           LDX #$09
?2         LDA WRNMES-1,X
           STA $3E99,X
           DEX
           BNE ?2
           DEC WRNCNT
           BNE ?1
           LDA #$00
           STA $D206
           STA $D207
           LDX #$09
?3         STA $3E99,X
           DEX
           BNE ?3
?1         RTS
;
          .LOCAL
;
MOVSAUC    INC SAUCNT
           LDA SAUCNT
           CMP #$10 
           BEQ ?11
           RTS
?11        LDA #$00   
           STA SAUCNT
           LDA SAUCDIR  
           BEQ ?1
           DEC HPOS1
           DEC HPOS2
           JMP ?2
?1         INC HPOS1
           INC HPOS2
?2         LDA SAUCT
           BEQ ?3
           BMI ?4
           INC SAUCY 
           LDA SAUCY
           CMP #$60
           BNE ?3
           LDA #$00
           STA SAUCT
           JMP ?3
?4         DEC SAUCY
           LDA SAUCY
           CMP #$20
           BNE ?3
           LDA #$00
           STA SAUCT
?3         LDX SAUCY  
           INX
           LDY #$0A
           LDA #$00
?5         STA $3400,X
           STA $3500,X
           DEX  
           DEY    
           BPL ?5 
           LDA #$44
           STA $2C0
           LDA #$84
           STA $2C1
           LDA SAUCPNT
           CMP #$04
           BNE PLOTSAUC
           LDA #$00
           STA SAUCPNT
          .LOCAL
PLOTSAUC   ASL
           TAX
           LDA SAUCTBL,X
           STA TEMP1
           LDA SAUCTBL+1,X
           STA TEMP2
           LDX SAUCY
           LDY #$00
?1         LDA (TEMP1),Y
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
           BNE ?1
           INC SAUCPNT2
           LDA SAUCPNT2
           CMP #$06
           BNE ?66
           LDA #$00
           STA SAUCPNT2
           INC SAUCPNT
?66        LDA SAUCT 
           BNE MISCHK
           LDA $D20A
           BMI ?10
           LDA SAUCY
           CMP #$20
           BEQ MISCHK
           DEC SAUCT 
           JMP MISCHK  
?10        LDA SAUCY
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
           .LOCAL
           SBC CROSSX
           BCC ?1
           CMP #$0D
           BCC NOMISL
           DEC MISDIR  
           BNE NOMISL
?1         EOR #$FF
           CLC
           ADC #$01
           CMP #$0D
           BCC NOMISL
           INC MISDIR
           .LOCAL
NOMISL     LDA HPOS1
           BNE ?1
           LDA #$00
           STA SAUCFLG
           STA $D207
?1         RTS 
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
           .LOCAL
SOUND3     LDA SMISY
           BNE ?1
           LDA WRNCNT
           BNE ?3
           LDA SAUCFLG
           BNE ?2
           LDA #$00
           STA $D206
           STA $D207
?3         RTS
?1         LDA #$A8
           STA $D207
           LDA SMISY
           STA $D206
           RTS
?2         LDA VOLFLG    
           BNE ?4 
           LDA #$1
           STA $D206
           LDA #$60
           ORA VOLUM
           STA $D207
           INC VOLUM
           LDA VOLUM
           CMP #$0F
           BNE ?3
           INC VOLFLG
           JMP ?3
?4         LDA #$1
           STA $D206
           LDA #$60
           ORA VOLUM
           STA $D207
           DEC VOLUM
           BNE ?3
           DEC VOLFLG
           JMP ?3
;--------------------------------
; OBJECT COLLISION HANDLER
;--------------------------------
           .LOCAL
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
           BEQ ?1
           JMP SPACKIL
?1         LDA BASER
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
           .LOCAL
COLRU5     LDX #$00
?1         LDA CHTBL3,X
           CMP (IRQVAR1),Y
           BEQ COLRU8
           CMP #$FF
           BEQ COLREND
           INX
           BNE ?1
           .LOCAL
COLRU4     CMP #$22
           BCC ?2
           CMP #$2A
           BCS ?2
           INC FUEL
           INC FUEL
           INC FUEL
           LDA LEVEL
           ASL
           CLC
           ADC FUEL
           CMP #$51
           BCC ?1
           LDA #$50
?1         STA FUEL
?2         LDA IRQVAR1   
           SEC
           SBC CHTBL2,X
           STA IRQVAR1
           BCS ?3
           DEC IRQVAR2  
?3         LDA #$06  
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
           .LOCAL
BRIDGER    JSR CONTROL
           LDA BRDFLG 
           BNE ?1
?2         RTS  
?1         DEC BRDCNT
           BNE ?2
           LDA BRDPNT
           CMP #$03
           BNE ?3
           LDA #$00
           STA BRDFLG
           STA BRDCNT
           STA BRDPNT
           RTS
?3         ASL
           TAX
           LDA BRDTAB,X
           STA TEMP1
           LDA BRDTAB+1,X
           STA TEMP2
           LDY #$1F
?4         LDA (TEMP1),Y
           STA $70A8,Y
           DEY
           BPL ?4
           INC BRDPNT
           RTS
BRDTAB     .WORD BRIDGE1
           .WORD BRIDGE2
           .WORD BRIDGE3
;--------------------------------
; EXPLODE PLANE ON SCREEN
;--------------------------------
           .LOCAL
PLANEGON   JSR CONTROL
           LDA #$00
           STA HPOS3
           DEC ACTFLG
           LDA #$20
           STA TEMP1
           LDA #$50
           STA CRUD
?1         LDX #$0D
?2         LDA $D20A  
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
           BNE ?2
           LDA $D20A
           AND #$3F
           STA $D202
           LDA #$8F
           STA $D203
           LDA TEMP1
           PHA
           LDX #$30
?66        TXA
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
           BNE ?66
           PLA
           STA TEMP1
           DEC TEMP1
           BNE ?1
           DEC TEMP1
?3         LDA TEMP1
           STA $D202
           LDA TEMP1
           AND #$0F
           BNE ?4
           LDA TEMP1
           CMP #$90
           BCS ?5
           LDX #$10
?8         LDA $3490,X
           LSR
           STA $3490,X
           LDA $3590,X
           ASL
           STA $3590,X
           DEX
           BNE ?8
           BEQ ?4
?5         LDX #$0C
?6         LDA $3690,X
           LSR
           STA $3690,X
           LDA $3790,X
           ASL
           STA $3790,X
           DEX
           BNE ?6
?4         LDX #$6 
?10        TXA  
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
           BNE ?10
           DEC TEMP1      
           BNE ?3
           LDA #$00
           STA $D203
           DEC SHIPS
           BNE ?11
           JMP ENDGAME
?11        JSR PMAKER
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
           .LOCAL
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
?1         STA $3600,X
           DEX
           BNE ?1
           LDA MISDIR
           BEQ ?2
           BMI ?3
           INC HPOS3
           BEQ CKMISKL
           BNE ?2
?3         DEC HPOS3 
           BEQ CKMISKL
?2         INC SMISY
           INC SMISY
           INC SMISY
           INC SMISY
           LDA SMISY
           CMP #$84
           BCS CKMISKL
           LDX SMISY 
           LDY #$04
           LDA #$08
?5         STA $3600,X 
           DEX 
           DEY
           BNE ?5
           LDA #$FF
           STA $2C2
ENDMIS     RTS  
           .LOCAL
CKMISKL    LDA HPOS3
           SEC
           SBC CROSSX    
           BEQ ?2
           BCC ?1 
           CMP #$0D
           BCS ?3
           JMP ?2
?1         EOR #$FF
           CMP #$0C
           BCS ?3
?2         JSR PLANEGON  
?3         LDA #$00  
           STA HPOS3
           STA SMISY
           RTS
;--------------------------------
; SAUCDEATH!!!!
; CHECK FOR SAUCER HIT
;--------------------------------
           .LOCAL
UFODIE     JSR CONTROL
           JSR KILUFO
           LDX #$03  
?1         LDA HIT1,X
           AND #$3
           BNE ?2
           DEX
           BPL ?1 
           RTS
?2         INC UFOEXP  
           LDA #$00
           STA EXPSND
           LDX #$03
?3         LDA HIT1,X  
           AND #$8
           STA HIT1,X  
           DEX
           BPL ?3
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
           .LOCAL
KILUFO     LDA UFOEXP
           BNE ?1
?2         RTS   
?1         LDA #$00
           STA SAUCFLG
           INC UFKFLG
           LDA UFKFLG
           CMP #$60
           BNE ?15
           LDA #$00
           STA UFKFLG
           LDX #$60
?3         LDA $3400,X  
           ASL 
           STA $3400,X  
           LDA $3500,X 
           LSR
           STA $3500,X 
           DEX
           BNE ?3
           DEC UFCNT   
           BNE ?15
           LDA #$00
           STA UFOEXP
           LDX #$60
?6         STA $3400,X
           STA $3500,X
           DEX
           BNE ?6
           LDX #$03
?14        LDA HIT1,X
           AND #$08
           STA HIT1,X
           DEX
           BPL ?14
?15        PLA  
           PLA
           RTS
;--------------------------------
; STAR ROUTINE
;--------------------------------
           .LOCAL
STARS      INC STRCNT
           LDA STRCNT
           CMP #$02
           BEQ ?1
           RTS
?1         LDA #$00
           STA STRCNT
           LDX STRFAS
           STA $73F8,X
           INC STRFAS
           LDA STRFAS
           CMP #$08
           BNE ?2
           LDA #$00
           STA STRFAS
           JSR STARPLOT
?2         LDX STRFAS    
           LDA #$0C
           STA $73F8,X  
           RTS
;--------------------------------
; STARPLOT SUBROUTINES
;--------------------------------
           .LOCAL
STARPLOT   LDX #$0D
?1         LDY STARY,X
           LDA YLOW,Y
           STA IRQVAR1
           LDA YHI,Y
           STA IRQVAR2
           LDY STARX,X
           LDA (IRQVAR1),Y
           BEQ ?2
           CMP #$7F
           BEQ ?2
           CMP #$FF
           BNE ?5
?2         LDA #$00
           STA (IRQVAR1),Y
?5         INC STARY,X
           LDA STARY,X
           CMP #$15
           BNE ?6
           LDA #$00
           STA STARY,X
?6         TAY
           LDA YLOW,Y
           STA IRQVAR1
           LDA YHI,Y
           STA IRQVAR2
           LDY STARX,X
           LDA (IRQVAR1),Y
           BNE ?7
           LDA #$7F
           BIT $D20A
           BMI ?8
           LDA #$FF
?8         STA (IRQVAR1),Y  
?7         DEX   
           BPL ?1
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
