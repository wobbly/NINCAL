PC       EQU       0
         INC       COMMON.inc
         INC       CONS.inc
         INCLUDE   NSTEDD.inc
.;Patch1.9
          include   compdd.inc
          include   cntdd.inc
.         INCLUDE   NMLRDD.inc
.;Patch1.9

Release   Init      "2.0"               DLH PL 
Reldate   Init      "18 February 2009"
.Release INIT "1.9" DMB 26MAY2004        Mailer COnversion
.RELEASE  INIT      "1.8"        ASH 02OCT2000 NEW SERVER ADDED
.RELEASE  INIT      "1.7"        DLH 23Aug99 NINadj nadjust Y2K, File Expansion
.RELEASE  INIT      "1.6"        DLH 26APR99 NININV Y2K, File Expansion
.RELEASE  INIT      "1.5"        ASH 20JAN99 NINORD Y2K, File Expansion
.RELEASE  INIT      "1.4"        JD30oct97 added dsinit vars.
.RELEASE  INIT      "1.3"        added date chk, break mlr/brker, write order sales#s
.RELEASE  INIT      "1.2"       DLH APR93 BRK GUAR, BROKER NUMBER
.RELEASE  INIT      "1.1"       DLH 23MAR92   NMLRXX, CONVERT JULIAN USAGE TO
.                              CVTJUL, NSTEXX
.RELEASE  INIT      "1.0"
.release  init      "0.9"      30MAY90. BREAK  BY TEAMS SA,EM,LIST MGMT
.release  init      "0.5"      13JUNE1988. BREAK SELECT CHANGED FROM OVER 60 TO 30-60+
STEOUT   FILE
STEOUT2  FILE
MO       DIM       2
DY       DIM       2
YR       DIM       2
BR       FORM      1
DATE     DIM       8
DATEMASK DIM       8
sysdate  dim       5
DIFF     FORM      5
SIXTY    FORM      "60"
THIRTY   FORM      "30"
MLR      DIM       8
HOLDMLR  DIM       8
holdbrk  dim       8
.PROGRAM ACCESS.
.NAME           TYPE          ACCESS
BRKMLR   DIM       7
FORM5    FORM      5
SALESBR  FORM      2            HOLDS SALESMAN NUMBER FOR BRANCH
SALESNUM DIM       1            USED TO BUILD SALESMAN NUMBER
TEAM1    INIT      "1"          *brokerage
TEAM2    INIT      "2"          *consultant
TEAM3    INIT      "3"          *list management
COUNT    FORM      5
SALES    DIM       1
holds10  dim       1
holds    dim       1
acctnum  dim       2
.MAIN
         MOVE      "NEOM0002" TO PROGRAM
         MOVE      "NIN" TO COMPNME
         MOVE      "STATEMENT PARSE" TO STITLE
         MATCH     B8 TO TODAY
         IF        EQUAL
         goto      clock
                   ELSE
         IF        EOS
         goto      clock
         ENDIF
         ENDIF
         goto      date1
CLOCK    CLOCK     DATE TO TODAY
date1    IFZ       PC
         UNPACK    TODAY INTO mm,STR1,dd,STR1,yy
         UNPACK    TODAY INTO mo,STR1,dy,STR1,yr
         XIF
         MOVE      "EXIT" TO PF5
         TRAP      done IF F5
         CALL      PAINT
         CALL      FUNCDISP
         KEYIN     *CL
         IFZ       PC
.START PATCH 1.8 REPLACED LOGIC
.         PREPARE   STEOUT,"g:\DATA\NINSTE60"
.         PREPARE   STEOUT2,"g:\DATA\NINSTE90"
         PACK      STR35,NTWKPATH1,"NINSTE60"
         PACK      STR45,NTWKPATH1,"NINSTE90"
         PREPARE   STEOUT,STR35
         PREPARE   STEOUT2,STR45
.END PATCH 1.8 REPLACED LOGIC
         XIF
.         DISPLAY   *P1:1,*ES
         DISPLAY   *P01:06,"Input File  : ":
                   *P01:08,"Input Count : ":
                   *P01:09,"Eom Date : "
.
INPGET   TRAP      FILENG IF IO
         OPEN      TESTFILE,INPNAME
         TRAPCLR   IO
         DISPLAY   *P15:06,INPNAME
         GOTO      chkdate
FILENG   NORETURN
         KEYIN     *P01:24,*EL,"The Input file is not on-line. ":
                   *P15:06,INPNAME
         GOTO      INPGET
DATE
         KEYIN     *P20:12,*EF,"DATE  : ",*DV,DATEMASK,",OK ? ",*RV,*T200,str1
         CMATCH    YES TO str1
         GOTO      datecvt IF EQUAL
         GOTO      DATE IF EOS
KEYDATe  KEYIN     *P20:14,*EL,"DATE  : ",*DE,*JR,*ZF,*RV,MM,"/":
                   *DE,*JR,*ZF,*RV,DD,"/",*DE,*JR,*ZF,*RV,YY
         PACK      DATE FROM mm,SLASH,dd,SLASH,yy
         MOVE      DATE TO DATEMASK
         GOTO      DATE
chkdate  match     mm to mo
         goto      badmonth if not equal
         match     yy to yr
         goto      badyr if not equal
datecvt  CALL      CVTJUL
         MOVE      JULDAYS TO SYSDATE
         move      inpname to nstename
READ     CALL      NSTESEQ
         GOTO      DONE IF OVER
         ADD       C1 TO COUNT
         DISPLAY   *P15:08,count:
                   *P15:09,today
.         DISPLAY   *P12:14,"RECORDS IN ",COUNT
         PACK      MLR FROM STEMLR,STEbrk
.         PACK      MLR FROM STEMLR,STECNT,STEBILTO
         REP       ZFILL IN MLR
         MATCH     MLR TO HOLDbrk
         CALL      BREAK IF NOT EQUAL
.begin patch 2.0
.         CLEAR     STECODE
.         APPEND    "F" TO STECODE
.         APPEND    SALESNUM TO STECODE
.         RESET     STECODE
.end patch 2.0
         BRANCH    BR OF WRITE,WRIT2
         shutdown  "cls"
         STOP
BREAK
.         PACK      MKEY FROM STEMLR,STECNT
.         REP       ZFILL IN MKEY
.begin patch 2.0                                  can get rid of the call                                        

         CALL      MLRIO 
.         MOVE      MKEY TO BRKMLR
.         MOVE      MLR TO HOLDMLR
         MOVE      MLR TO HOLDbrk
.         UNPACK    STEINVDT INTO MO,DY,YR
.Start Patch #1.5 - Var expanded from NINORD expansion
.         UNPACK    STEMLDDT INTO MM,DD,YY
         UNPACK    STEMLDDT INTO str2,YY,MM,DD
.End Patch #1.5 - Var expanded from NINORD expansion
         CALL      CVTJUL
         MOVE      SYSDATE TO DIFF
         MOVE      JULDAYS TO FORM5
         SUB       FORM5 FROM DIFF
         COMPARE   SIXTY TO DIFF
.         COMPARE   THIRTY TO DIFF
.         GOTO      OLD IF EQUAL
         GOTO      NEW IF EQUAL
         GOTO      NEW IF LESS
OLD      MOVE      C2 TO BR
         RETURN
NEW      MOVE      C1 TO BR
         RETURN
WRITE
         WRITE     STEOUT,SEQ;STECODE:
                   STEMLR:
                   STELR:
                   STEBILTO:
                   STEINVNO:
                   STEINVDT:
                   STEAR:
                   STECNT:
                   STEMLRPO:
                   STEAP1:
                   STEAP2:
                   STEMLDDT:
                   STEGRNTE:
                   STELNAME:
                   STEGUAR:   102-102   OUTSIDE GUAR.
                   STEFIL3:   103-105
                   STECNAME:   106-110   CLIENT NAME (FOR ALPHA SORTING)
                   STEBRK:
                   STEBRKCT:
                   STESLS10:
                   STESALES   
         GOTO      READ
.
WRIT2    WRITE     STEOUT2,SEQ;STECODE:
                   STEMLR:
                   STELR:
                   STEBILTO:
                   STEINVNO:
                   STEINVDT:
                   STEAR:
                   STECNT:
                   STEMLRPO:
                   STEAP1:
                   STEAP2:
                   STEMLDDT:
                   STEGRNTE:
                   STELNAME:
                   STEGUAR:   102-102   OUTSIDE GUAR.
                   STEFIL3:   103-105
                   STECNAME:   106-110   CLIENT NAME (FOR ALPHA SORTING)
                   STEBRK:
                   STEBRKCT:
                   stesls10:
                   stesales
         GOTO      READ
MLRIO
.         CALL      NMLRKEY
         MOVE      C0 TO SALESBR
         MOVE      C0 TO SALESNUM
.         MOVE      MSLSPER TO SALESBR
         clear      acctnum
         pack       acctnum from stesls10,stesales
         move       acctnum to salesbr
.LOADOK   LOAD      SALESNUM FROM SALESBR OF TEAM1,TEAM1,TEAM1,TEAM2:
.                   TEAM2,TEAM3,TEAM2,TEAM1:
.                   TEAM1,TEAM1,TEAM1,TEAM2:
.                   TEAM1,TEAM2,TEAM2,TEAM1:
.                   TEAM2,TEAM1,TEAM3,TEAM1,TEAM1
LOADOK   LOAD      SALESNUM FROM SALESBR OF TEAM1,TEAM3,TEAM1,TEAM2:
                   TEAM2,TEAM3,TEAM2,TEAM1:
                   TEAM1,TEAM1,TEAM1,TEAM2:
                   TEAM1,TEAM2,TEAM2,TEAM1:
                   TEAM2,TEAM1,TEAM3,TEAM1,TEAM1,team1,team1,team1:
                   team1,team1
.begin patch 2.0

.         CLEAR     STECODE
.         APPEND    "F" TO STECODE
.         APPEND    SALESNUM TO STECODE
.         RESET     STECODE
.end patch 2.0
.         DISPLAY   *P15:22,MSLSPER," ",SALESBR," ",SALESNUM," ",1STESTAT,*W2
         RETURN
badmonth
         display   *p15:22,"MONTH DOES NOT MATCH CURRENT MONTH!!",*B
         goto      keydate
badyr
         display   *p15:22,"YEAR DOES NOT MATCH CURRENT YEAR!!",*B
         goto      keydate
...............................................................................
DONE     NORETURN
         WEOF      STEOUT,SEQ
         CLOSE     STEOUT,EOFSIZE
         WEOF      STEOUT2,SEQ
         CLOSE     STEOUT2,EOFSIZE
         shutdown  "cls"
         STOP
..............................................................................
;Patch1.9
          include   compio.inc
          include   cntio.inc
.         INCLUDE   NMLRIO.inc
;Patch1.9
         INCLUDE   NSTEIO.inc
         INCLUDE   COMLOGIC.inc

