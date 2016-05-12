*goto dontrun
...............................................................................
.ONACCOUNTDEL - CREATED 89AUG.
.  
.PURPOSE - DELETE ACCOUNTS WITH BALANCES OF 0.00 AND NO ACTIVETY IN 
.          CURRENT MONTH/YEAR.
.
...............................................................................
PC       EQU       0
         INCLUDE   COMMON.inc
         INCLUDE   CONS.inc
         INCLUDE   NMOADD.inc
         INCLUDE   NMOBDD.inc
release  init      "2.01"          ASH 27OCT98 NINMOA Y2K,File expansion
.release  init      "2.00"          DLH 10JUL95 - make copy of purged records.
.release  init      "1.4"           DLH 17MAY94 REVISED
.RELEASE  INIT      "1.3"           DLH 23JUL93 CHANGE IN NMOAIO   NMOAKS.
.RELEASE  INIT      "1.2"          DLH 07MAY92   NMOAXX, NMOBXX.
.
.RELEASE  INIT      "1.1"         DLH  12MAR92
prgbal   file      fixed=21
.Start Patch #2.01 - remmed and replaced line
.prgdet   file      fixed=119
prgdet   file      fixed=128
.End Patch #2.01 - remmed and replaced line
ACCOUNTS FILE      FIXED=21
RECNUM   FILE
...............................................................................
DETCOUNT FORM      5
BALCOUNT FORM      5
DETsaved FORM      5
BALsaved FORM      5
DATE     DIM       8
NULL     DIM       7
CHKDATE  DIM       4
SYSDATE  form      5
SAVEKEY  DIM       8
SAMEMLR  DIM       4
SAMEBRK  DIM       4
SAME     DIM       8
DATEMASK DIM       8
CHANGE   FORM      7.2
inbal    form      4
repflag  form      1               .1=create file only 2=create file & delete
delflag  form      1         .only used if repflag = 1, 1=1st rec in 'del'
.
...............................................................................
.         OPEN      ACCOUNT1,"ONACCNTBALAN"
         IFNZ       PC
         OPEN      ACCOUNTS,"NINMOB"
         XIF
         IFZ        PC
         OPEN      ACCOUNTS,"NINMOB.eom"
         move      c2 to nmoapath
         call      nmoaopen
         XIF
...............................................................................
         CLOCK     DATE TO DATE
         MOVE      "NONA0005" TO PROGRAM
         MOVE      "Names in the News Ca Inc" TO compnme
         MOVE      "DELETE INACTIVE MOA" TO STITLE
         move      "abort" to pf5
DATE     
         MOVE      DATE TO TODAY
         MOVE      DATE TO DATEMASK
         UNPACK    DATE INTO MM,SLASH,DD,SLASH,YY
         move      "01" to dd
         CALL      PAINT
         call      cvtjul
         move      juldays to sysdate
.         PACK      SYSDATE FROM MM,YY
         KEYIN     *P10:10,"DATE OK ",*DV,DATEMASK," ",STR1
         CMATCH    YES TO STR1
         GOTO      DATEOK IF EQUAL
         KEYIN     *P18:10,*+,MM,"/",DD,"/",YY
         move      "01" to dd
         PACK      DATE FROM MM,SLASH,DD,SLASH,YY
         call      cvtjul
         move      juldays to sysdate
         GOTO      DATE
DATEOK   CLEAR     NULL
.
         MOVE      C2 TO NMOBPATH
         move      c2 to repflag
         prepare   prgdet,"c:\work\prgdet",exclusive
         prepare   prgbal,"c:\work\prgbal",exclusive
         CALL      PAINT
         call      funcdisp
         trap      done if f5
...............................................................................
MENU     CLEAR     NMOAFLD
         clear     nmoafld4
         clear     nmobbrk
         clear     mlr
         MOVE      "N" TO OVER
         READ      ACCOUNTS,SEQ;MLR,MCNT,BALANCE,nmobbrk
         GOTO      DONE IF OVER
         add       c1 to inbal
         display   *p10:12,"master records in ",inbal
         COMPARE   C0 TO BALANCE     *NO MONEY ON ACCOUNT?
         GOTO      MENU IF NOT EQUAL     *HAVE BALANCE
         PACK      NMOAFLD4 FROM NMOBBRK,MLR    *NO BALANCE, ACTIVE?
         REP       " 0" IN NMOAFLD4
         MOVE      NMOAFLD4 TO SAVEKEY
...............................................................................
         MOVE      C4 TO NMOAPATH
         CALL      NMOAKEY    *GET DETAIL REC.
         GOTO      DELETE IF OVER
         CALL      CHKDATE    *CHECK DATE
.
LOOP 
         CALL      NMOAKS      *GET NEXT DETAIL REC.
         GOTO      DELETE IF over     
         pack      same from NMOABRK,mlr
         rep       zfill in same
         match     same to nmoafld4
         goto      delete if not equal
         CALL      CHKDATE         *YES, CHECK DATE.
         GOTO      LOOP      *DATE IN DIFFERENT MONTH, CHECK NEXT REC,.
.
...............................................................................
.CHKDATE  - TRANSACTION FROM CURRENT MONTH?
CHKDATE  
.Start patch #2.01 - remmed and replaced line
.         UNPACK    trandate INTO MM,DD,YY
         UNPACK    trandate INTO STR2,YY,MM,DD
.End patch #2.01 - remmed and replaced line
         match     "  " to yy
         return    if equal
         move      c1 to dd
         call      cvtjul
.         PACK      CHKDATE FROM MM,YY
.         MATCH     CHKDATE TO SYSDATE
         compare   juldays to sysdate
         GOTO      NEXT IF EQUAL          *CURRENT MONTH DON'T DEL
.                                         *WHAT'S MORE SKIP TO NEXT MLR.
         goto      next if less           *into next months records.
.                                         *ie we are running job late (in beginning of next month)         
         RETURN
.
...............................................................................
.NEXT - RECORD FOUND FOR CURRENT MONTH. BREAK CYCLE AND START ON NEXT MLR.
NEXT     NORETURN
         GOTO      MENU
.
...............................................................................
.DELETE - NO CURRENT TRANSACTIONS AND BALANCE IS ZERO SO DELETE ALL RECORDS.,
.
DELETE   MOVE      SAVEKEY TO NMOAFLD4
.DELLOOP
         MOVE      C4 TO NMOAPATH
.         compare   c1 to delflag
.         if        equal
.         move      c2 to delflag
         rep       zfill in nmoafld4
         CALL      NMOAKEY
.         GOTO      DELBAL IF OVER
         if        over
         GOTO      DELBAL
         endif
         goto      delwrt
.         else
delloop
         MOVE      C4 TO NMOAPATH
         CALL      NMOAKS      *GET NEXT DETAIL REC.
.         GOTO      DELbal IF over     
         if        over
         GOTO      DELBAL
         endif
         clear     same
         pack      same from NMOABRK,mlr
         rep       zfill in same
         rep       zfill in nmoafld4
         match     same to nmoafld4
         goto      delbal if not equal
.         endif
delwrt   ADD       C1 TO detsaved
         write     prgdet,seq;moavars
         DISPLAY   *P1:14,*EL,*P15:14,"DETAIL RECORDS written : ",DETsaved
         MOVE      TRANSNUM TO NMOAFLD
         MOVE      C2 TO NMOAPATH
         REP       ZFILL IN NMOAFLD
         CALL      NMOAKEY
         display   *p1:24,*el,"getting ready to delete", nmoafld
         branch    repflag to deldetno,deldet
         goto      deldetno
deldet   FILEPI    5;NMOAFLE4,NMOAFLE2
         DELETEK   NMOAFLE4,NULL
         DELETE    NMOAFLE2,nmoafld
         ADD       C1 TO DETCOUNT
deldetno PACK       NMOAFLD  FROM MLR,MCNT
.dlh desperate 18nov96         PACK      NMOAFLD4 FROM NMOBBRK,MLR    *NO BALANCE, ACTIVE?
         REP       ZFILL IN NMOAFLD4
         REP       ZFILL IN NMOAFLD
         GOTO      DELLOOP
DELBAL   MOVE      SAVEKEY TO NMOAFLD4
         CALL      NMOBKEY
         write     prgbal,seq;nmobmlr,nmobmcnt,BALANCE,nmobbrk
         ADD       C1 TO BALsaved
         DISPLAY   *P1:14,*EL,*P15:14,"DETAIL RECORDS written : ",DETsaved:
                   *P1:16,*EF,*P15:16,"BALANCE RECORDS written: ",BALsaved
         move      c1 to delflag
         branch    repflag to menu,delbal1
delbal1  FILEPI    1;NMOBFle2
         DELETE    NMOBFLE2,NMOAFLD4
         ADD       C1 TO BALCOUNT
         DISPLAY   *P1:10,*EL,*P15:10,"DETAIL RECORDS DELETED : ",DETCOUNT:
                   *P1:12,*EF,*P15:12,"BALANCE RECORDS DELETED: ",BALCOUNT
         GOTO      MENU
...............................................................................
.
DONE    
         weof      prgdet,seqeof
         weof      prgbal,seqeof
         close     prgdet
         close     prgbal
         stop
         INCLUDE   NMOAIO.inc
         INCLUDE   NMOBIO.inc
         INCLUDE   COMLOGIC.inc

