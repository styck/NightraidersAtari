;--------------------------------
; NIGHT 3
; SPACE ROUTINES
;--------------------------------
; SPACE ATTACKER ROUTINES
; ATTACKS PLAYER WITH ANDROID
; SHIPS
;--------------------------------
           .LOCAL
SPCATK     LDA SPACFLG
           BNE ?2
?1         RTS
?2         LDA MUSCNT
           CMP #$1A
           BEQ ?3
           CMP #$19
           BNE ?5
           LDA LEVEL
           CLC
           ADC #$01
           STA WAVES
?5         LDA #$00   
           STA MUSDEL
           INC MUSCNT
           RTS
?3         LDA #$2
           ADC MUSDEL
           STA MUSDEL
           CMP #$F0
           BCS ?4
           RTS
?4         LDA #$00
           STA MUSDEL
           LDA WAVES
           BNE ?9
           INC MIKEY2
           LDA MIKEY2
           CMP #$30
           BNE ?7
           DEC SPACFLG
?7         RTS
?9         LDA FLYFLG  
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
           BCC ?6
           LSR
?6         ASL
           STA PATHPNT
           LDA #$00
           STA PSTRING
           STA PSTATUS
           LDX #$5F
?8         LDA SPACL1,X
           STA $7380,X
           DEX
           BPL ?8
;--------------------------------
; PLOT SHIPS
;--------------------------------
           .LOCAL
ALLDEAD    LDA PSTATUS
           CMP #$FF
           BNE ?1
           DEC WAVES
           BNE ?2
           RTS
?2         DEC FLYFLG
           RTS
?1         LDX PATHPNT
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
           BEQ ?4
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
           BNE ?3
           LDA SMISY
           ORA WRNCNT 
           ORA SAUCFLG
           BNE ?3
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
?3         PLA
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
?4         DEC FLYFLG
           RTS
;--------------------------------
; ERASE PLANES
;--------------------------------
           .LOCAL
ERASEPLN   LDY PSTRING
           BEQ ?1
           DEY
           LDA (TEMP1),Y
           CMP #$FF
           BEQ ?1
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
?1         RTS
;--------------------------------
; SPACE COLLISION HANDLER
;--------------------------------
.LOCAL
SPACKIL    LDA #$00  
           TAY
           TAX
?1         LDA PLANCHR,X
           CMP #$FF
           BEQ ?6
           CMP (IRQVAR1),Y
           BEQ ?2
           INX
?6         CMP #$FF 
           BNE ?1
           JMP COLREND
?2         LDA IRQVAR1
           SEC
           SBC PLANDIF,X 
           STA IRQVAR1
           BCS ?3
           DEC IRQVAR2
?3         LDX #$00
?4         LDA PLANFIND,X 
           BEQ ?5
           INX
           CMP #$FF
           BNE ?4
           JMP COLREND
?5         LDA #$5 
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
?99        LDA BSCOR0
           CLC 
           ADC #$64 
           STA BSCOR0
           LDA BSCOR1  
           ADC #$00
           STA BSCOR1
           DEX
           BNE ?99
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
.LOCAL
KILLER2    LDA SPACFLG
           BNE STYX
           RTS
STYX       INC MIKEY
           LDA MIKEY
           CMP #$20
           BEQ ?1
           RTS
?1         LDA #$00
           STA MIKEY
           TAX
?2         LDA PLANFIND,X
           CMP #$FF
           BNE ?3
           RTS
?3         CMP #$00
           BNE ?4
           INX
           BNE ?2
?4         STX TEMP3
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
           BNE ?5
           BEQ ?6
?5         LDA #$30
?6         LDY #$00
           STA (TEMP1),Y
           INY
           STA (TEMP1),Y
           LDY #$28
           STA (TEMP1),Y
           INY
           STA (TEMP1),Y
           INX
           JMP ?2
;--------------------------------
; MX SHOOTS MISSLES
;--------------------------------
.LOCAL
MX         INC MXDELAY
           LDA MXDELAY
           CMP #$10
           BEQ ?4
           RTS
?4         LDA #$00
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
           BCC ?1
           INC TEMP2
?1         INC TEMP2
           LDY #$27
?2         LDA (TEMP1),Y
           CMP #$E6
           BEQ ?3
           DEY
           BPL ?2
           RTS
?3         TYA
           ASL
           ASL
           ADC #$2E
           STA HPOS4
           INC MXFLAG
           LDA #$00
           STA MXSCRL
           RTS
.LOCAL
MX2        LDA MXSCRL 
           CMP #$0F
           BCS MX3 
           LDA #$0E
           SEC 
           SBC MXSCRL
           TAY 
           LDX #$FF
?2         INX
           LDA MXDAT,X  
           STA $374B,Y
           INY
           CPX MXSCRL
           BNE ?2
           INC MXSCRL
           RTS
.LOCAL           
MX3        INC $2C3
           LDX #$00
?1         LDA $3700,X  
           STA $36FF,X  
           INX
           CPX #$60
           BNE ?1
           INC MXSCRL
           LDA MXSCRL
           CMP #$FF
           BEQ ?2
           STA $D202
           LDA #$8F
           STA $D203
           RTS
?2         DEC MXFLAG
           LDA #$50
           STA $D202
           LDA #$88
           STA $D203
           RTS
;--------------------------------
; MXKILL CHECK FOR MX DEATH!
;--------------------------------
.LOCAL
MXKILL     LDA MXDEATH
           BNE ?4 
           LDX #$03
?1         LDA HIT1,X
           AND #$08
           BNE ?2
           DEX
           BPL ?1
           RTS
?2         LDX #$03
?3         LDA HIT1,X
           AND #$07
           STA HIT1,X
           DEX
           BPL ?3
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
?4         LDA #$50
           STA $D202
           LDA #$88
           STA $D203
           LDX #$60  
?5         LDA $3700,X
           BNE ?55
           DEX
           BNE ?5
?55        LDY #$0E
?56        DEX
           LDA $D20A
           AND MXDAT,Y
           STA $3700,X  
           DEY
           BPL ?56 
           DEC MCNT
           BNE ?15
           LDA #$00
           STA MXDEATH
           LDX #$60
?8         STA $3700,X
           DEX
           BPL ?8
?15        PLA
           PLA
           RTS
MXDAT      .BYTE $10,$10,$10,$38,$38,$38,$38,$38 
           .BYTE $38,$38,$38,$7C,$7C,$44,$44
;--------------------------------
; KIL BASER
;--------------------------------
           .LOCAL
EXPLOB     LDA BASDEAD
           CMP #$03
           BEQ ?1
           RTS
?1         LDA #$00
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
?2         STA $48C0,X
           STA $49C0,X
           STA $4AC0,X
           INX
           INX
           BNE ?2
           LDA #$CF 
           STA $D201
           LDA #$8F
           STA $D203
           LDX #$FF
?3         STX $D200
           TXA
           PHA
           EOR #$FF
           STA $D202
           TXA
           AND #$0F
           BNE ?7
           LDX #$15
           LDA #$C0
           STA TEMP1
           LDA #$48
           STA TEMP2
?4         LDY #$00
?6         LDA (TEMP1),Y
           DEY
           STA (TEMP1),Y
           INY
           INY
           CPY #$15
           BCC ?6
           LDY #$28 
?5         LDA (TEMP1),Y
           INY
           STA (TEMP1),Y
           DEY
           DEY
           CPY #$14
           BCS ?5
           LDA TEMP1
           CLC
           ADC #$28
           STA TEMP1
           LDA TEMP2
           ADC #$00
           STA TEMP2
           DEX
           BNE ?4
           LDX #$28
           LDY #$00
?10        LDA $49C0,Y
           STA $48C0,Y
           LDA $4AC0,Y
           STA $49C0,Y
           LDA $4BC0,Y
           STA $4AC0,Y
           LDA #$00
           STA $4BC0,Y
           INY
           BNE ?10
?7         LDY #$00
           LDX #$05
?20        DEY
           BNE ?20
           DEX
           BNE ?20 
           PLA
           TAX
           DEX
           BNE ?3
           LDA #$00
           STA $D201
           STA $D203
           STA $2C8
           LDA #$2C
           STA $2F4
           .LOCAL
BONUS      LDX #$0E
?1         LDA BONSTR,X
           STA $4995,X
           DEX
           BPL ?1
           LDX #$0F 
?2         LDY MUSDATA,X
           STY $D200
           DEY
           STY $D202
           LDA #$AF
           STA $D201
           STA $D203
           INY
           CPY #$00
           BNE ?55
           STY $D201
           STY $D203
?55        LDA MUSDLY,X
           LDY #$00
?3         DEY
           BNE ?3
           SEC
           SBC #$01
           BNE ?3
           DEX
           BPL ?2
           LDX #$10
           JSR DELAY
           LDA #$01
           STA $49EC
           STA $49ED
           STA $49EE
           LDY LEVEL
           INY
           LDX #$02 
?21        STX $49EB
           LDA BSCOR0
           CLC
           ADC #$E8
           STA BSCOR0
           BCC ?22
           INC BSCOR1
?22        INC BSCOR1
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
           BNE ?21
           LDX #$50
           JSR DLONG
           LDA LEVEL
           CMP #$04
           BNE ?87
           INC SHIPS
?87        CMP #$05
           BEQ ?9
           INC LEVEL
?9         LDA #$58
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
           .LOCAL
BASKILER   LDA (IRQVAR1),Y
           CMP #$3F
           BEQ ?2
           CMP #$40
           BEQ ?1
           JMP RETRY
?1         LDA IRQVAR1
           SEC
           SBC #$01
           STA IRQVAR1
           BCS ?2
           DEC IRQVAR2
?2         LDA #$B0
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
           .LOCAL
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
?3         STA $3400,X
           STA $3500,X
           STA $3600,X
           STA $3700,X
           DEX
           BNE ?3
           LDX #$5F
?1         LDA GDAT,X
           STA $3420,X
           LDA ADAT,X
           STA $3520,X 
           LDA MDAT,X  
           STA $3620,X
           LDA EDAT,X
           STA $3720,X
           DEX
           BPL ?1
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
           .LOCAL
PAUSER     LDA CONSOL
           ROR
           BCS ?1
           JMP WARMSTART
?1         ROR
           ROR
           BCC ?2
           RTS
?2         LDA CONSOL
           AND #$04
           BEQ ?2 
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
?3         LDA $2FC
           CMP #$FF
           BEQ ?3
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
           .LOCAL
FIREPOWER  INC CNTFIRE
           BPL ?9 
           LDA #$00
           STA CNTFIRE
           LDA SMISY
           ORA WRNCNT
           ORA SAUCFLG
           BEQ ?1
?9         RTS 
?1         LDA LIST2+3
           CLC
           ADC #$C8
           STA TEMP1
           LDA LIST2+4
           ADC #$00
           STA TEMP2
           LDX #$08 
?2         LDY #$27
?3         LDA (TEMP1),Y
           CMP #$10
           BEQ ?4
           CMP #$40
           BEQ ?4
           DEY
           BPL ?3
           LDA TEMP1
           CLC
           ADC #$28
           STA TEMP1
           LDA TEMP2
           ADC #$00
           STA TEMP2
           DEX
           BNE ?3
           RTS
?4         DEY
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
