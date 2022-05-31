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
                                      