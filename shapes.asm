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
;--------------------------------
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
