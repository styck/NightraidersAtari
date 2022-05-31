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
                                                                                                                                                                                 